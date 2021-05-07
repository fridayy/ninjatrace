const renderMap = () => {
    const map = L.map('map').setView([51.505, -0.09], 10);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);
    return map
}
const connect = (map, socket) => {
    socket.addEventListener("open", () => {});
    socket.addEventListener("message", (event) => {
        console.log(event.data);
        const json = JSON.parse(event.data);
        const gpsData = json["info"][0]["gps_sensor"]
        const tempData = json["info"][1]["temperature"]
        const lat = gpsData["lat"];
        const lng = gpsData["lng"];

        map.setView([lat, lng], 13)
        const marker = L.marker([lat, lng]);
        marker.bindPopup(`CPU Temperature: ${tempData["temperature"]} °C`)
        marker.addTo(map);
    });
}
const openWindow = () => {
    const map = renderMap();
    const socket = new WebSocket("ws://localhost:8080/info/ws/device1@archpi-1");
    new WinBox({
        title: "ninja live location",
        mount: document.getElementById("map"),
        background: "#181D22FF",
        border: 0.5,
        onfocus: () => {
            connect(map, socket)
            map.invalidateSize(true);
            document.getElementById("map").style.display = "block";
        },
        onclose: () => {
            document.getElementById("map").style.display = "none";
            socket.close();
            map.remove();
        }
    });
};
