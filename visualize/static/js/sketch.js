// JavaScript source code
var weather;
function preload() {
    var url = getURL() + "/testweather/";
    trajectories = loadJSON(url);
}

function setup() {
    createCanvas(640, 480);
}

function draw() {
    if (mouseIsPressed) {
        fill(0);
    } else {
        fill(255, 204, weather.main.humidity);
    }
    ellipse(mouseX, mouseY, 80, 80);
}
