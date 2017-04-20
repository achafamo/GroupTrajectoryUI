# GroupTrajectoryUI

## Setup
Install Python 3 if you DON'T have python or if you have python 2.x version 
    
    sudo apt-get install python3

Then install pip if you don't already have it and using pip, install virtualenv which is a tool that we can use to ensure we
are all working on the same environment and the same dependencies

    python3 -m pip install -U pip    
    pip install virtualenv

Create a Virtual environment to clone our project repo in. Change env_name to something related to our project 

    virtualenv env_name
    cd env_name/bin
    source ./activate #activates our virtual env. You should see (env_name) at the command line input
    cd ../ #move back to our env_name directory
    
Clone this repository.
    
    git clone https://github.com/achafamo/GroupTrajectoryUI.git

Install requirements:

    pip install -r requirements.txt


Migrate:

    python manage.py migrate


Create a superuser:

    python manage.py createsuperuser

Run the development server:

    python manage.py runserver

You should see trajectories moving on the screen at root url:

    http://localhost:8000/
