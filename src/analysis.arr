import file as F
import filelib as FL
include string-dict
import file('../data/anfdata.arr') as A

distancesdir = '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/distances'

fun appordering():
0
end

distancefuns = 
[list:

# Call counts
lam(a, b):
	num-abs(a.length() - b.length())
end,

# Exactly equal
lam(a, b):
	if a == b:
		0
	else:
		1
	end
end,

# distance1
lam(a, b):
0
end,

# distance2
lam(a, b):
0
end,

# distanceiso
lam(a, b):
0
end]

# Groupby function names

# Need to take care of name conflicts later within a student program (for now 
# assume same function if function names are same, except lambda, and underscores)

fun collapse-indices(sp, names):
	unnamed = [set: "lambda", "___"]
	for fold(fundict from [mutable-string-dict: ], studsub from sp):
		block:
			{sid; subid; submission} = studsub
			for each(testcase from submission):
				for each(tuple from testcase):
					{fid; fname; fin; fout} = tuple
					name = if names:
						num-to-string(sid) + ":" + num-to-string(subid) + ":" + fname
					else:
						num-to-string(sid) + ":" + num-to-string(subid) + ":" + fid
					end
					when not(unnamed.member(fname)): ## leaving out fns whose name cannot be known 
						if fundict.has-key-now(name):
							fundict.set-now(name, fundict.get-value-now(name).append([list: {fin; fout}]))
						else:
							fundict.set-now(name, [list: {fin; fout}])
						end
					end
				end
			end
			fundict
		end
	end
end

fun correls(studfuns):
	var i = 0
	var j = 0
	fnames = studfuns.keys().to-list()
	(for fold(matrices from [list: ], distfn from distancefuns):
		link(link(link("", fnames), (for fold(rows from [list: ], fnA from fnames):
			block:
				i := i + 1
				j := 0
				link(link(fnA, (for fold(columns from [list: ], fnB from fnames):
					block:
						j := j + 1
						if (j <= i):
							link(0, columns) 
						else:
							link(distfn(studfuns.get(fnA).value, studfuns.get(fnB).value), columns)
						end
					end
				end).reverse()), rows)
			end
		end).reverse()), matrices)
	end).reverse()
end

when not(FL.exists(distancesdir)):
  FL.create-dir(distancesdir)
end

var file-counter = 0
(correls(collapse-indices(A.dat, true).freeze())).each(lam(mat): 
		block:
			file-counter := file-counter + 1
			F.output-file(distancesdir + "/" + num-to-string(file-counter) + ".csv", false).display((mat.map(lam(row): 
				row.join-str(", ") end)).join-str("\n"))
		end
	end)
