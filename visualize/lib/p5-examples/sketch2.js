var bugs;

function setup() {
  createCanvas(710, 400);
  // Create objects
  bugs = new BugSys();
  for (var i=0; i<20; i++) {
    bugs.bugs.push(new Jitter);
  }
}

function draw() {
  background(50, 89, 100);
  bugs.run();
}

function mouseClicked() {
  bugs.bugs.pop();
}

function keyPressed() {
  bugs.addBug();
}

// Jitter class
function Jitter() {
  this.x = random(width);
  this.y = random(height);
  this.diameter = random(10, 30);
  this.speed = 2;

  this.move = function() {
    this.x += random(-this.speed, this.speed);
    this.y += random(-this.speed, this.speed);
  };

  this.display = function() {
    ellipse(this.x, this.y, this.diameter, this.diameter);
  };
}

//Bugs class
function BugSys() {
  this.bugs = []

  this.addBug = function() {
  	var b = new Jitter();
  	b.x = mouseX;
  	b.y = mouseY;
  	this.bugs.push(b);
  };

  this.run = function() {
    for (var i=0; i<this.bugs.length; i++) {
      this.bugs[i].move();
      this.bugs[i].display();
    }
  };
}