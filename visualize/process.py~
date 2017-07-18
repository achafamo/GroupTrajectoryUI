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

if __name__ == "__main__":
    print (get_file("static/text/trajectories.txt"))
