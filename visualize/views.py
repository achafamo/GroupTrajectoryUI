from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.template import Template, Context
from django.shortcuts import render_to_response
from django.template.loader import get_template
from django.conf import settings 
import os
import random
import math 

import collections
from django.core.mail import EmailMessage
from django.shortcuts import redirect
from django.template import Context
from visualize.forms import ContactForm

TRAJECTORY_DATA_PATH = "visualize/input/"
GROUPING_DATA_PATH = "visualize/output/"
TRAJECTORY_DATA = "visualize/input/trajectory1.txt"
GROUPING_DATA = "visualize/output/grouping1.txt"
IO_DICT = collections.defaultdict(dict)
FILE_LOCATION = collections.defaultdict()
# our view
def home(request):
		form_class = ContactForm
		global TRAJECTORY_DATA
		global GROUPING_DATA
		if request.method == 'POST':
			initialize_options()			
			form = form_class(data=request.POST)
			data_file = request.POST.get('data_field', '')
			time_field = request.POST.get('time_field','')
			members_field = request.POST.get('members_field','')
			distance_field = request.POST.get('distance_field)' ,'')				
			TRAJECTORY_DATA = data_location(data_file)			
			print ("traj" +TRAJECTORY_DATA)
			GROUPING_DATA = group(TRAJECTORY_DATA, parameters = [distance_field, time_field, members_field])
			request.session['TRAJECTORY_DATA'] = TRAJECTORY_DATA
			request.session['GROUPING_DATA'] = GROUPING_DATA
			#print (request.session['TRAJECTORY_DATA'], request.session['GROUPING_DATA'])
			return render(request, 'contact1.html', {'form': form_class, })
			
		request.session['TRAJECTORY_DATA'] = TRAJECTORY_DATA
		request.session['GROUPING_DATA'] = GROUPING_DATA
		#print (request.session['TRAJECTORY_DATA'], request.session['GROUPING_DATA'])
		return render(request, 'contact.html', {'form': form_class, })
			
def home1(request):
	'''
	this function returns our main page which displays the visualization and grouping of trajectories
	'''
	t = get_template('index4.html')
	html = t.render()
	return HttpResponse(html)

def get_trajectories(request):
	'''
	returns the trajectory data in the text file in JSON format.
	'''
	#print (request.session['TRAJECTORY_DATA'], request.session['GROUPING_DATA'])
	#trajectories = get_file(TRAJECTORY_DATA)  
	trajectories = get_file(request.session['TRAJECTORY_DATA'])	
	#print (trajectories)
	return JsonResponse(trajectories)
	#TODO: Right now, we are only returning a hard coded trajectory data. We should expand this to allow users to choose
	#		 from a list of trajectory data// drop down list with all the trajectory data we have 

	#TODO: only gets trajectories from file and not user input, update trajectory should create a copy of the file and add the additional ilines 


def group(trajectory, parameters):	
	if IO_DICT[trajectory]:
		print ("IO_DICT " + IO_DICT[trajectory])
		return IO_DICT[trajectory]
	else:
		IO_DICT[trajectory] = 	execute_haskell(trajectory, parameters)
		return IO_DICT[trajectory]
		
def data_location(filename):
	if FILE_LOCATION[filename]:
		return FILE_LOCATION[filename]
	else:
		return 

def initialize_options():
	FILE_LOCATION["Trajectory1"] = TRAJECTORY_DATA_PATH + "trajectory1.txt"
	FILE_LOCATION["Trajectory2"] = TRAJECTORY_DATA_PATH + "trajectory2.txt"
	FILE_LOCATION["Trajectory3"] = TRAJECTORY_DATA_PATH + "trajectory3.txt"
	IO_DICT[FILE_LOCATION["Trajectory1"]] = GROUPING_DATA_PATH + "grouping1.txt"
	IO_DICT[FILE_LOCATION["Trajectory3"]] = GROUPING_DATA_PATH + "grouping3.txt"

def output_name(input_file):	
	input_file = input_file.replace("trajectory", "grouping")
	input_file = input_file.replace("input", "output")
	return input_file

def input_name(input_file):
	input_file = input_file.replace(".txt", ".netlogo")
	return input_file
def execute_haskell(input_file, parameters = [10, 3.5, 3]):	
	new_path = os.path.join(settings.BASE_DIR, "visualize/lib/hgrouping/src")	
	os.chdir(new_path)
	input_file = input_name(input_file)
	output_file = output_name(input_file)
	command = "./HGrouping -i " + os.path.join(settings.BASE_DIR, input_file) + " -o " + os.path.join(settings.BASE_DIR, output_file) + " -p 10,3.5,3 |>log.txt"
	print (command)
	#command = "./HGrouping -i ../../input/netlogo/trajectories_300_150.netlogo -o out2.netlogo -p 10,3.5,3"	''	
	res = os.system(command)	
	
	return process_grouping(output_file)
	
	
def get_groupings(request):
	'''
	
	'''
	#sort groupings file based on start and end time
	#pass modified file to read_groupings, change this to a script that can take in files
	
	#groupings = read_grouping(GROUPING_DATA)	
	groupings = read_grouping(request.session['GROUPING_DATA'])	
	return JsonResponse(groupings)
	
def update_trajectories(request):
	'''
	takes newly posted trajectory data and runs grouping algorithm. Should return/update trajectories and new group_info
	'''
	#TODO: this is where the interface with the haskell code should happen 
	#takes info about the added trajectory, writes it to the trajectory file 
	pass
def get_file(filename):
	'''
	returns a dictionary of each trajectories coordinates for every time stamp from the netlogo input format data 
	'''
	d = collections.defaultdict(dict)
	print("in get_File"  + os.path.join(settings.BASE_DIR, filename))
	with open(os.path.join(settings.BASE_DIR, filename), 'r') as f:
		print ("file opened?")
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
			#print (trajectory_id, n)
			if (trajectory_id > n):
				print ("breaking")
				break					 
			coordinate = lines[idx].replace('[', '').replace(']','').split() 
			d[trajectory_id][t] = coordinate				
			t+=1
			idx+=1
	print ("should be here")
	return d
def process_grouping(filename):
	output_name = filename.replace("netlogo", "txt")
	print ("ouput should be at " +output_name)
	with open(os.path.join(settings.BASE_DIR,filename), 'r') as f, open(os.path.join(settings.BASE_DIR, output_name), 'w') as f2:
		line_generator = [line for line in f]
		print (line_generator[0])
		n = int(line_generator[0].split(' ')[0]) * int(line_generator[0].split(' ')[1])
		n = n+1
		i = n
		print (n, len(line_generator))
		while i<len(line_generator) :
			curr_line = line_generator[i].split()
			curr_line = ','.join(curr_line)
			next_line = line_generator[i+1].split()
			curr_line+=",,"	
			curr_line+=','.join(next_line)	
			#print (curr_line)
			i+=2
			f2.write(curr_line + os.linesep)
	#with open(settings.BASE_DIR + output_name, 'r') as f3:
		#print(f3.read())
	return output_name

def read_grouping(sorted_file):
	colors = ['#F0F8FF','#FAEBD7','#00FFFF','#7FFFD4','#FFEBCD','#0000FF','#8A2BE2','#A52A2A']
	d = collections.defaultdict(dict)
	with open(os.path.join(settings.BASE_DIR, sorted_file), 'r') as f:
		lines = [line for line in f]
		n = len(lines) #number of groups
		for line in lines:
			#print (line.split(',,')[0])
			num, start, end = line.split(',,')[0].split(',')
			group = line.split(',,')[1].split(',')		  
			r = lambda: random.randint(0,255)
			clr = random.choice(colors) #('#%02X%02X%02X' % (r(),r(),r()))		  
			for t in range(math.floor(float(start)), math.floor(float(end))):
				for traj in group:
					d[int(traj)][t] = clr
	return d

def test(request):
	return JsonResponse(read_grouping("visualize/new.txt"))


	

