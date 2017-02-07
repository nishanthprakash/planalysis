import file as F
import filelib as FL
include string-dict
include sets
import file('../data/anfdata.arr') as A



studprogs = A.dat

fun appordering():

end

distancefuns = 
[list:

# Call counts
lam ():

end,

# Exactly equal
lam ():

end,

# distance1
lam ():

end,

# distance2
lam ():

end,

# distanceiso
lam ():

end]

# Groupby function names
# Need to take care of name conflicts (assume same function if function names are same, except lambda, and underscores)

fun groupfuns(sp):
	unnamed = [set: "lambda", "___"]
	for map(studsub from sp):
		{sid; subid; submission} = studsub
		for map(testcase from submission):
			for fold(fundict from [mutable-string-dict: ], tuple from testcase):
				{fid; fname; fin; fout} = tuple
				when not(unnamed.member(fname)): ## leaving out fns whose name cannot be known 
					if fundict.has-key(fname):
						fundict.set-now(fname, fundict.get-value-now(fname).append([list: {fin; fout}]))
					else:
						fundict.set-now(fname, [list: {fin; fout}])
					end
				end
			end
		end
	end
end

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

# grouped = groupfuns(studprogs)
studfuns = collapse-indices(studprogs)
var i = 0
var j = 0
fun correls(studfuns):
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
