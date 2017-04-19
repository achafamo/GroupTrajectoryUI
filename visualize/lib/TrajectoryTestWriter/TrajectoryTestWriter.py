#test_file_writer.py

#trajectoryNum = 400
#trajectoryLength = 150
#lineNum = 1

def main():

	trajectoryNum = 400
	trajectoryLength = 45    # EDIT THIS (MAX 150)

	lineNum = 1
	fileList = []

	# invalidInput = True
	# while invalidInput:
	# 	ans = raw_input("What is the length of the trajectory? Maximum 150" + "\n")
	# 	if ans.isdigit() and int(ans) <= 150:
	# 		invalidInput = False
	# 		trajectoryLength = ans

	# Generates the files to fill
	for i in range(trajectoryNum-1):
		file = open("trajectories_" + str(i+1) + "_" + str(trajectoryLength) + ".netlogo", 'w+')
		fileList = fileList + [file]

	# Parses through the lines of the base netlogo file and selectively writes to the new files
	with open("trajectories_400_150.netlogo", 'r') as f:
		for line in f:

			for fileNum in range(len(fileList)):
				if lineNum == 1:
					file.write(str(fileNum)+"\n")

				if lineNum == 2:
					file.write(str(trajectoryLength) + "\n")

				if ((lineNum-2) % 150) > trajectoryLength:
					continue

				else:
					file = fileList[fileNum]
					if int((lineNum-2)/ 150) <= fileNum:
						file.write(line)

			lineNum+=1

	for file in fileList:
		file.close()

main()



