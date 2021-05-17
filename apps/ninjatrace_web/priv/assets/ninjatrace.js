const getDeviceList = async () => {
    try {
        const response = await fetch(`${BASE_URL}/devices`)
        const devices = await response.json()
        return devices
    } catch (e) {
        // do something useful here (notification)
        console.error(e)
    }
}

const BASE_URL = "http://localhost:8080"

const renderDeviceList = async () => {
    const container = document.getElementById("device-container")
    const response = await fetch(`${BASE_URL}/devices`)
    const devices = await response.json()
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
        <ul class="list-group list-group-flush">
        ${device.sensors.map((sensor) =>
            `<li class="list-group-item">${sensor.name}</li>`
        )}
        </ul>
        <a href="#" class="btn btn-primary" onclick="trace('${device.name}')">Trace</a>
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

const trace = (deviceName) => {
        const map = renderMap();
        try {
            const socket = new WebSocket(`${BASE_URL}/info/ws/${deviceName}`)
            new WinBox({
                title: "ninja live location",
                mount: document.getElementById("map"),
                background: "#181D22FF",
                border: 0.5,
                onfocus: () => {
                    document.getElementById("map").style.display = "block";
                    socket.addEventListener("message", (message) => {
                        const json = JSON.parse(message.data);
                        const gpsData = json["info"][0]["gps_sensor"]
                        const tempData = json["info"][1]["temperature"]
                        const lat = gpsData["lat"];
                        const lng = gpsData["lng"];

                        map.setView([lat, lng], 13)
                        const marker = L.marker([lat, lng]);
                        marker.bindPopup(`CPU Temperature: ${tempData["temperature"]} °C`)
                        marker.addTo(map);
                    });
                    map.invalidateSize(true);
                },
                onclose: () => {
                    document.getElementById("map").style.display = "none";
                    socket.close();
                    map.remove();
                }
            });
        } catch (e) {
            new WinBox({
                title: "Error",
                html: "<h1>Could not establish connection</h1>"
            });
        }
    };

const main = async () => {
    await renderDeviceList();
};
main().then(() => console.log("rendered"), (err) => console.error(err));