import file as F
import filelib as FL

anfdata-dir = '/Users/np/Projects/Plan Composition/pyret-starterplanalysis/data/anfdata/'

anfdata-files = FL.list-files(anfdata-dir)

for each(afile from anfdata-files):
	import file(anfdata-files + afile) as CL
end