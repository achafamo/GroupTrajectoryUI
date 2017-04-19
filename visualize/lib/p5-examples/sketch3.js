var VectorSystem;

function setup() {
  createCanvas(1200,800);
  sys = new VectorSystem();
  test = new Particle();
  test.position.x = 500;
  test.position.y = 500;
  sys.particles.push(test);
}

function draw() {
  background(50, 89, 100);
  sys.run();
}

function mousePressed() {
  var p = new Particle();
  sys.particles.push(p);
}

function keyPressed() {
  sys.particles.pop();
}

// A simple Particle class
var Particle = function() {
  //this.acceleration = createVector(0, 0.05);
  this.velocity = createVector(random(-1.5, 1.5), random(-1.5, 1.5));
  this.position = createVector(mouseX,mouseY);
  this.color = [random(0,255),random(0,255),random(0,255)];

  this.run = function() {
    this.update();
    this.display();
  };

  // Method to update position
  this.update = function() {
    //this.velocity.add(this.acceleration);
    this.position.add(this.velocity);
    //if (this.position.x > 1200 || this.position.x < 0 || this.position.y > 800 || this.position.y < 0)

  };

  // Method to display
  this.display = function() {
  	fill(this.color[0],this.color[1],this.color[2]);
    ellipse(this.position.x, this.position.y, 12, 12);
  };
}


var VectorSystem = function() {
  this.particles = [];

/*  this.addParticle = function() {
  	var p = new Particle();
    this.particles.push(p);
  };*/

  this.run = function() {
    for (var i = this.particles.length-1; i >= 0; i--) {
      var p = this.particles[i];
      p.run();
    }
  };
}