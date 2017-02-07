import file as F
import filelib as FL
include string-dict
include sets
import file('../data/anfdata.arr') as A



studprogs = A.dat

fun appordering():
0
end

distancefuns = 
[list:

# Call counts
lam (a, b):
0
end,

# Exactly equal
lam (a, b):
0
end,

# distance1
lam (a, b):
0
end,

# distance2
lam (a, b):
0
end,

# distanceiso
lam (a, b):
0
end]

# Groupby function names

# Need to take care of name conflicts later within a student program (for now 
# assume same function if function names are same, except lambda, and underscores)

fun collapse-indices(sp):
	unnamed = [set: "lambda", "___"]
	for fold(fundict from [mutable-string-dict: ], studsub from sp):
		{sid; subid; submission} = studsub
		for each(testcase from submission):
			for each(tuple from testcase):
				{fid; fname; fin; fout} = tuple
				name = num-to-string-digits(sid) + ":" + num-to-string-digits(subid) + ":" + fname
				when not(unnamed.member(name)): ## leaving out fns whose name cannot be known 
					if fundict.has-key(name):
						fundict.set-now(name, fundict.get-value-now(name).append([list: {fin; fout}]))
					else:
						fundict.set-now(name, [list: {fin; fout}])
					end
				end
			end
		end
	end
end

fun correls(studfuns):
	var i = 0
	var j = 0
	fnames = studfuns.keys().to-list()
	(for fold(matrices from [list: ], distfn from distancefuns):
		link(matrices, (for fold(rows from [list: ], fnA from fnames):
			i := i + 1
			link(rows, (for fold(columns from [list: ], fnB from fnames):
				j := j + 1
				if(j <= i):
					link(0, columns) 
				else:
					link(distfn(fnA, fnB), columns)
				end
			end).reverse())
		end).reverse())
	end).reverse()
end

var file-counter = 0
correls(collapse-indices(studprogs)).each(lam(mat): 
	F.output-file(num-to-string(file-counter := file-counter + 1) + ".csv", false).display(mat.map(lam(row): 
		row.join-str(", ") end).join-str("\n")) end)
