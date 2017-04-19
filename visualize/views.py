from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.template import Template, Context
from django.shortcuts import render_to_response
from django.template.loader import get_template
from django.conf import settings 
import os


import collections
def home(request):
    t = get_template('index2.html')
    html = t.render()
    return HttpResponse(html)
def weather(request):
    trajectories = get_file("visualize/trajectories.txt")
    return JsonResponse(trajectories)
# Create your views here.
def get_file(filename):
    d = collections.defaultdict(dict)

    with open(os.path.join(settings.BASE_DIR, filename), 'r') as f:
        line_generator = [line for line in f]
        n = int(line_generator[0])
        tau = int(line_generator[1])
        trajectory_id = 1;
        t = 0;
        idx = 2;
        while True:
            #print (idx)
           
            if(t == tau):                
                t = 0
                trajectory_id +=1
            if (trajectory_id > n):
                    break                
            coordinate = line_generator[idx].replace('[', '').replace(']','').split()
            d[trajectory_id][t] = coordinate            
            t+=1
            idx+=1
    return d