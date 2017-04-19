var boids = [];
var idx = 0;
var len = 0;
var new_idx = 0;
var new_tr = 1;
var pos = [];
var num_trajectories = 10;
var new_trajectories = [];

function preload() {
    var url = getURL() + "/trajectorydata/"; // loads trajectory data from Django backend server
    trajectories = loadJSON(url); //adding trajectory data into dictionary 
    len = trajectories.length;
}

function setup() {
    var canvas = createCanvas(920, 520);
    canvas.parent('canvas-center');
    frameRate(10);
    for (var i = 0; i < num_trajectories; i++) {        
        boids[i] = new Boid(0, 0);
        pos[i] = 0;
    }
}

function draw() {    
    background('#BCCE98');
    for (var i = 0; i < boids.length; i++) {
        boids[i].run(boids, i);
    }

}

function reset() {
    for (var i = 0; i < num_trajectories; i++) {        
        boids[i] = new Boid(0, 0);
        pos[i] = 0;
        idx = 0;
    }
    loop();
}


// Boid class
function Boid(x, y) {    
    this.position = createVector(x, y);
    this.r = 3.0;
    this.maxspeed = 3;    // Maximum speed
    
}

Boid.prototype.run = function (boids, i) {
    if (pos[i] == 149) {        
        noLoop(); // if last time step, pause loop
       
    }    
    this.update(i);    
    this.render();    
}

function keyPressed() {
    reset();       
}

function mouseDragged() {
    //TODO: Right now, when the mouse is dragged the path is only displayed and not recorded. We need to 
    //save the values of mouseX and mouseY in intervals that correspond to the existing trajectory data
    //so that we can add it and display the added trajectory in the next iteration
    loop();    
    ellipse(mouseX, mouseY, 20, 20); // displays an elipse at the given coordinate everytime the mouse is dragged
    //noLoop();   
    
}
// Method to update location
Boid.prototype.update = function (i) {
    idx = pos[i];
    this.position = createVector(trajectories[i + 1][idx][0], trajectories[i + 1][idx][1]);
    pos[i] = pos[i]+1
}

// Draw boid as a circle
Boid.prototype.render = function () {
    fill('#222222');
    stroke(200);
    ellipse(this.position.x, this.position.y, 20, 20); //displays current position of trajectory data
}

// Wraparound
Boid.prototype.borders = function () {
    //I am not sure what this does
    if (this.position.x < -this.r) this.position.x = width + this.r;
    if (this.position.y < -this.r) this.position.y = height + this.r;
    if (this.position.x > width + this.r) this.position.x = -this.r;
    if (this.position.y > height + this.r) this.position.y = -this.r;
}

