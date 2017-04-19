var system;

function setup() {
  createCanvas(1200, 800);
  system = new TrajectorySystem();
  for (var n = 0; n < 20; n++) {
  	var t = new Trajectory();
  	for (var i = random(0,1000); i < 1000; i++) {
  	  t.xpath.push(i+random(-1,1));
  	  t.ypath.push(i+random(-1,1));
    }
  system.trajectories.push(t);
  }
}

function draw() {
  background(50, 89, 100);
  system.run();
}

function mousePressed() {
  //noLoop();
  ellipse(mouseX,mouseY,10,10);
  system.addTrajectory();
  //loop();
}

// A Trajectory class
var Trajectory = function() {
  this.xpath = [];
  this.ypath = [];

  this.display = function() {
	ellipse(this.xpath[0], this.ypath[0], 20, 20);
	this.xpath.shift();
	this.ypath.shift();
  };
}

// A TrajectorySystem class
var TrajectorySystem = function() {
  this.trajectories = [];

  this.addTrajectory = function() {
	var t = new Trajectory(); //create new object
	while (1) {
      console.log(i);
	  if (!mouseIsPressed){
		break;
	  }
	  if (millis() % 500 == 0) { //populate array
		t.xpath.push(mouseX);
        t.ypath.push(mouseY);
	  }
	}
	this.trajectories.push(t); //add to system
  };

  this.run = function() {
	for (var j = 0; j < this.trajectories.length; j++) {
	  this.trajectories[j].display();
	}
  };
}