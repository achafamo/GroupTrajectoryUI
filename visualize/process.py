import collections
def get_file(filename):
    d = collections.defaultdict(dict)

    with open(filename, 'r') as f:
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
def process_grouping(filename, output):
	with open(filename, 'r') as f and open(output, 'w') as f2:
		line_generator = [line for line in f]
		n = int(line_generator[0].split(',')[0]) * int(line_generator[0].split(',')[1])
		n = n+1
		for i in range(n,2,len(line_generat0r)):
			curr_line = line[i].split()
			curr_line = ','.join(curr_line)
			next_line = line[i+1].split()
			cur_line+=",,"	
			curr_line+=','.join(next_line)
			f2.write(curr_line)

if __name__ == "__main__":
    print (get_file("static/text/trajectories.txt"))
