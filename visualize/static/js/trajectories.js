var boids = [];
var idx = 0;
var len = 0;
var new_idx = 0;
var new_tr = 1;
var pos = [];
var num_trajectories = 10;
var new_trajectories = [];
function preload() {
    var url = getURL() + "/trajectorydata/";
    trajectories = loadJSON(url);
    len = trajectories.length;
}

function setup() {
    var canvas = createCanvas(920, 520);
    canvas.parent('canvas-center');
    frameRate(10);


    for (var i = 0; i < num_trajectories; i++) {
        //boids[i] = new Boid(trajectories[i + 1][0][0], trajectories[i + 1][0][1]);
        boids[i] = new Boid(0, 0);
        pos[i] = 0;
    }
}

function draw() {
    //document.write(idx);
    background('#BCCE98');
    for (var i = 0; i < boids.length; i++) {
        boids[i].run(boids, i);
    }
        //idx += 1;

}

function reset() {
    for (var i = 0; i < num_trajectories; i++) {
        //boids[i] = new Boid(trajectories[i + 1][0][0], trajectories[i + 1][0][1]);
        boids[i] = new Boid(0, 0);
        pos[i] = 0;
        idx = 0;
    }
    loop();
}


// Boid class
// Methods for Separation, Cohesion, Alignment added
function Boid(x, y) {
    //this.acceleration = createVector(0, 0);
    //this.velocity = p5.Vector.random2D();
    this.position = createVector(x, y);
    this.r = 3.0;
    this.maxspeed = 3;    // Maximum speed
    
}

Boid.prototype.run = function (boids, i) {
    if (pos[i] == 149) {        
        noLoop();
       
    }    
    this.update(i);    
    this.render();
    
}

function keyPressed() {
    reset();
    //noLoop();
    //new_trajectories.push({new_idx: [mouseX, mouseY]});
    //new_tr += 1;
    
}
function mouseDragged() {
    loop();
    //frameRate(10);
    //line(mouseX, mouseY, pmouseX, pmouseY);
    //new_idx += 1;
    //new_trajectories.push({ new_idx: [mouseX, mouseY] });
    //if statment to check time 
    ellipse(mouseX, mouseY, 20, 20);
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
    ellipse(this.position.x, this.position.y, 20, 20);
}

// Wraparound
Boid.prototype.borders = function () {
    if (this.position.x < -this.r) this.position.x = width + this.r;
    if (this.position.y < -this.r) this.position.y = height + this.r;
    if (this.position.x > width + this.r) this.position.x = -this.r;
    if (this.position.y > height + this.r) this.position.y = -this.r;
}

