import file as F
import filelib as FL
include string-dict
import file('../data/anfdata/STF.arr') as A

measuresdir = '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/measures'

fun justlist(sp, names) block:
	unnamed = [set: "lambda", "___"]
	{sid; subid; appdata} = sp
	fnlist =
	for map(ftuple from appdata):
		{fid; fname; fin; fout} = ftuple
		if names:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + fname
		else:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + num-to-string(fid) + ":" + fname
		end
	end
	fnlist
end


when not(FL.exists(measuresdir)):
  FL.create-dir(measuresdir)
end

var file-counter = 0
for each(collapsee from [list: true, false]) block:
	file-counter := file-counter + 1
	F.output-file(measuresdir + "/" + num-to-string(file-counter) + "f" + "STF" + ".csv", false).display(
		justlist(A.dat, collapsee).join-str("\n"))
end
