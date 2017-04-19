// JavaScript source code
var weather;
function preload() {
    var url = getURL() + "/testweather/";
    weather = loadJSON(url);
}

function setup() {
    noLoop();
}

function draw() {
    background(200);
    // get the humidity value out of the loaded JSON
    var humidity = weather.main.humidity;
    fill(0, humidity); // use the humidity value to set the alpha
    ellipse(width / 2, height / 2, 50, 50);
}