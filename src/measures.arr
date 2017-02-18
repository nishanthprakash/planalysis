import file as F
import filelib as FL
include string-dict
import file('../data/anfdata/STF.arr') as A

measuresdir = '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/measures'

fun appordering():
0
end

# Call counts
counts(studfuns):
	fnames = studfuns.keys().to-list()

	for fold(rows from [list: ], fn from fnames):
		rows + [list: link(fn, studfuns.get(fnA).length())]
	end
end

# Groupby function names

# Need to take care of name conflicts later within a student program (for now 
# assume same function if function names are same, except lambda, and underscores)

fun collapse-indices(sp, names) block:
	unnamed = [set: "lambda", "___"]
	{sid; subid; appdata} = sp
	fundict = [mutable-string-dict: ]
	for each(ftuple from appdata):
		{fid; fname; fin; fout} = ftuple
		name = if names:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + fname
		else:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + num-to-string(fid) + ":" + fname
		end
		when not(unnamed.member(fname)): ## leaving out fns whose name cannot be known 
			if fundict.has-key-now(name):
				fundict.set-now(name, fundict.get-value-now(name).append([list: {fin; fout}]))
			else:
				fundict.set-now(name, [list: {fin; fout}])
			end
		end
	end
	fundict
end

fun justlist(sp, names) block:
	unnamed = [set: "lambda", "___"]
	{sid; subid; appdata} = sp
	fnlist = [list: ]
	for fold(ftuple from appdata):
		{fid; fname; fin; fout} = ftuple
		name = if names:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + fname
		else:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + num-to-string(fid) + ":" + fname
		end
		when not(unnamed.member(fname)):
			fnlist.append([list: name])
		end
	end
	fnlist
end


when not(FL.exists(measuresdir)):
  FL.create-dir(measuresdir)
end

var file-counter = 0
for each(collapsee from [list: true, false])
	file-counter := file-counter + 1
	F.output-file(measuresdir + "/" + num-to-string(file-counter) + "f" + "STF" + ".csv", false).display((
		justlist(A.dat, collapsee).join-str("\n"))
end
