const getDeviceList = async () => {
    try {
        const response = await fetch(`${BASE_URL}/devices`)
        return await response.json()
    } catch (e) {
        // do something useful here (notification)
        console.error(e)
    }
}

// const BASE_URL = "http://ninjatrace.harmless.ninja"
const BASE_URL = "http://localhost:8080"
// const WEBSOCKET_URL = "ws://ninjatrace.harmless.ninja"
const WEBSOCKET_URL = "ws://localhost:8080"

const hasGpsSensor = (sensors) => sensors.filter(sensor => sensor.name === "gps_sensor")
    .length > 0

const renderDeviceList = async () => {
    const container = document.getElementById("device-container")
    const devices = await getDeviceList()
    if (devices.length === 0) return;
    container.innerHTML =
        `
<div class="row">
${devices.map(device => `
  <div class="col-sm-6">
    <div class="card">
      <div class="card-body">
        <h5 class="card-title">${device.name}</h5>
        <p class="card-text">Sensors</p>
        <div class="row">
        ${device.sensors.map((sensor) =>
            `<div class="col-sm-3">
                <span class="badge bg-primary">${sensor.name}</span>
                </div>`
        )}
        </div>
        <div class="row" style="margin-top: 2%">
        <div class="col-sm-6">
        ${hasGpsSensor(device.sensors) ? `<a href="#" class="btn btn-primary" onclick="trace('${device.name}')">Trace</a>` : `<div/>`}
        </div>
        </div>
      </div>
    </div>
  </div>
`)}
</div>
`
}

const renderMap = () => {
    const map = L.map('map').setView([51.505, -0.09], 10);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);
    return map
}

const parse = (message) => {
    const sensorData = JSON.parse(message.data);
    return sensorData
        .reduce((acc, val) => Object.assign(val.data), {})
}

const mapHandler = (map, socket, pastPositions, currentPositionMarker) => (message) => gpsEventHandler(map,
    message,
    socket,
    pastPositions,
    currentPositionMarker);
const gpsEventHandler = (map, message, socket, pastPositions, currentPositionMarker) => {
    const parsed = parse(message);
    const currentPosition = [parsed.lat, parsed.lng]

    if (parsed.lat === undefined || parsed.lng === undefined) {
        console.warn("GPS lost connection")
        socket.onmessage = mapHandler(map, socket, pastPositions, currentPositionMarker)
        return
    }

    if (currentPositionMarker !== null) currentPositionMarker.remove()
    const newCurrentPositionMarker = L.marker(currentPosition).addTo(map)
    // draw old positionsnet_adm:ping('srv@ec2-3-65-38-243.eu-central-1.compute.amazonaws.com').
    L.polyline(pastPositions, {color: 'green'}).addTo(map);

    // sent a ping each 10 received messages
    pastPositions.length % 10 === 0 ? socket.send("ping") : undefined

    socket.onmessage = mapHandler(map, socket, [...pastPositions, currentPosition], newCurrentPositionMarker)
}

const trace = (deviceName) => {
    const map = renderMap();
    try {
        const socket = new WebSocket(`${WEBSOCKET_URL}/info/ws/${deviceName}`)
        new WinBox({
            title: "ninja live location",
            mount: document.getElementById("map"),
            background: "#181D22FF",
            border: 0.5,
            onfocus: () => {
                document.getElementById("map").style.display = "block";
                socket.onmessage = (message) => {
                    const initialPosition = parse(message);

                    if (initialPosition.lat === undefined || initialPosition.lng === undefined) {
                        console.warn("GPS track lost")
                        socket.onmessage = mapHandler(map, socket, [], null)
                        return;
                    }

                    const coords = [initialPosition.lat, initialPosition.lng]
                    // center the view on the initial position received
                    map.setView(coords, 13)
                    //set the new handler
                    socket.onmessage = mapHandler(map, socket, [coords], null)
                }
                map.invalidateSize(true);
            },
            onclose: () => {
                document.getElementById("map").style.display = "none";
                socket.close();
                map.remove();
            }
        });
    } catch (e) {
        map.remove();
        console.error(e);
        new WinBox({
            title: "Error",
            html: `<h1>Could not establish connection</h1>
                    <small>${e.message}</small>`
        });
    }
};

const main = async () => {
    await renderDeviceList();
};
main().then(() => console.log("ninjatrace initialized"), (err) => console.error(err));