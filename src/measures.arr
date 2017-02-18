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
		{fpos; fid; fname; fin; fout} = ftuple
		name = if names:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + fname
		else:
			num-to-string(sid) + ":" + num-to-string(subid) + ":" + num-to-string(fid) + ":" + fname
		end
		{fpos; name}
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
		((justlist(A.dat, collapsee)
			.sort-by(lam(x, y):
				 		{pos1; nam1} = x 
				 		{pos2; nam2} = y 
				 		pos1 < pos2 
				 	end, 
				 	lam(x, y): 
				 		{pos1; nam1} = x 
				 		{pos2; nam2} = y
				 		pos1 == pos2  
				 	end))
			.map(lam(x):
				 		{pos; nam} = x 
				 		nam
				 	end))
			.join-str("\n"))
end
