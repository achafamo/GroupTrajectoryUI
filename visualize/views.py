from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.template import Template, Context
from django.shortcuts import render_to_response
from django.template.loader import get_template
from django.conf import settings 
import os


import collections
def home(request):
    '''
    this function returns our main page which displays the visualization and grouping of trajectories
    '''
    t = get_template('index2.html')
    html = t.render()
    return HttpResponse(html)

def get_trajectories(request):
    '''
    returns the trajectory data in the text file in JSON format.
    '''
    trajectories = get_file("visualize/trajectories.txt")  
    return JsonResponse(trajectories)
    #TODO: Right now, we are only returning a hard coded trajectory data. We should expand this to allow users to choose
    #       from a list of trajectory data

def update_trajectories(request):
    '''
    takes newly posted trajectory data and runs grouping algorithm. Should return/update trajectories and new group_info
    '''
    #TODO: this is where the interface with the haskell code should happen 
    pass
def get_file(filename):
    '''
    returns a dictionary of each trajectories coordinates for every time stamp from the netlogo input format data 
    '''
    d = collections.defaultdict(dict)
    with open(os.path.join(settings.BASE_DIR, filename), 'r') as f:
        lines = [line for line in f]
        n = int(lines[0]) #number of trajectories
        tau = int(lines[1]) #number of time steps for the trajectories
        trajectory_id = 1
        t = 0
        idx = 2
        while True:
            if(t == tau):  #start new trajectory              
                t = 0
                trajectory_id +=1
            if (trajectory_id > n):
                    break                
            coordinate = lines[idx].replace('[', '').replace(']','').split() 
            d[trajectory_id][t] = coordinate            
            t+=1
            idx+=1
    return d

