import sys, os, pandas, yaml

transformed_dir = sys.argv[1] + '/transformed'
student_dirs = [ transformed_dir + "/" + sdir for sdir in next(os.walk(transformed_dir))[1]]


anf_files = {1: "earthquake-1-anf.arr.out", 2: "earthquake-2-anf.arr.out"}

sdlist = []

for sdir in student_dirs:
	for file_index, afile in anf_files.items():
		json_out = sdir + "/final-submission/" + afile
		with open(json_out, 'r') as myfile:
			json_data = myfile.read().replace('\n', '').replace('The program didn\'t define any tests.', '')
			student_data = yaml.safe_load(json_data)
			for studID, tests in student_data.items():
				for test in tests:
					for testID, functions in test.items():
						for function in functions:
								if function['fid'] != '':
									sdlist.append({'studentID': int(studID), 'submissionID': int(file_index), 'testcaseID': int(testID), 'functionID': int(function['fid']), 'function_name':  function['fname'], 'function_input': function['fin'], 'funtion_output': function['fout']})

student_data = pandas.DataFrame(sdlist)
student_data = student_data[['studentID', 'submissionID', 'testcaseID', 'functionID', 'function_name', 'function_input', 'funtion_output']]

student_data.to_excel("student_data.xlsx", sheet_name='anf', index=False)