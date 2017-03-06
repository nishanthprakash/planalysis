import file as F
import filelib as FL
include string-dict
import file('../data/anfdata/STF1.arr') as A
import file('../data/anfdata/STF2.arr') as B

distancesdir = '/Users/np/Projects/Plan Composition/planalysis/data/distances'

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
end

# distance1

# distance2

# distanceiso
]

# Groupby function names

# Need to take care of name conflicts later within a student program (for now 
# assume same function if function names are same, except lambda, and underscores)

fun collapse-indices(sp, names) block:
	unnamed = [set: "lambda", "___"]
	{sid; subid; appdata} = sp
	fundict = [mutable-string-dict: ]
	for each(ftuple from appdata):
		{fstart; fstop; fid; fname; fin; fout; fobj} = ftuple
		name = if names:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + fname
		else:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + num-to-string(fid) + ":" + fname
		end
		when not(unnamed.member(fname)): ## leaving out fns whose name cannot be known 
			if fundict.has-key-now(name):
				fundict.set-now(name, fundict.get-value-now(name).append([list: {fin; fout; fobj}]))
			else:
				fundict.set-now(name, [list: {fin; fout; fobj}])
			end
		end
	end
	fundict
end

fun correls-across(studfuns1, studfuns2):
	var i = 0
	var j = 0
	fnamesA = studfuns1.keys().to-list()
	fnamesB = studfuns2.keys().to-list()
	(for fold(matrices from [list: ], distfn from distancefuns):
		link(link(link("", fnamesB), (for fold(rows from [list: ], fnA from fnamesA):
			link(link(fnA, (for fold(columns from [list: ], fnB from fnamesB):
				link(distfn(studfuns1.get(fnA).value, studfuns2.get(fnB).value), columns)
			end).reverse()), rows)
		end).reverse()), matrices)
	end).reverse()
end

when not(FL.exists(distancesdir)):
  FL.create-dir(distancesdir)
end

var file-counter = 0

for each(collapsee from [list: true, false]):
	(correls-across(collapse-indices(A.dat, collapsee).freeze(), collapse-indices(B.dat, collapsee).freeze())).each(lam(mat) block:
			file-counter := file-counter + 1
			F.output-file(distancesdir + "/" + num-to-string(file-counter) + "f" + "STF1xSTF2" + ".csv", false).display((mat.map(lam(row): 
				row.join-str(", ") end)).join-str("\n"))
		end)
end
