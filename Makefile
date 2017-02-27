pyret-base := /Users/np/Projects/pyret-lang

data := data

studs := $(wildcard $(data)/transformed/*)

srcs := $(foreach dir, $(studs), $(wildcard $(dir)/final-submission/*.arr))
objs := $(foreach dir, $(studs), $(wildcard $(dir)/final-submission/*.arr.jarr))

rebuild:
	-rm -rf node_modules
	-rm -rf compiled
	-rm -rf undefined
	-rm .pyret-parley.1705.*
	npm install

pyret = cd $(pyret-base) && node build/phaseA/pyret.jarr \
    --build-runnable $(1)  \
    --outfile $(2) \
    --builtin-js-dir src/js/trove \
    --builtin-arr-dir src/arr/trove \
    --require-config src/scripts/standalone-configA.json


everything: transform execute analyze plots

transform:
	-rm '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/student-codes/.DS_Store'
	-rm -rf '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/transformed'
	$(call pyret, '../Plan Composition/pyret-starter/planalysis/src/anf-main.arr', '../Plan Composition/pyret-starter/planalysis/bin/anf-main.arr.jarr') 
	node ./bin/anf-main.arr.jarr '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data'

execute:
	-rm -rf '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/anfdata'
	for src in $(srcs) ; do \
		$(call pyret, "../Plan Composition/pyret-starter/planalysis/$$src", "../Plan Composition/pyret-starter/planalysis/$$src.jarr") ; \
		node "/Users/np/Projects/Plan Composition/pyret-starter/planalysis/$$src.jarr" ; \
	done

anafs := $(foreach file, $(notdir $(wildcard $(data)/anfdata/*.arr)), $(subst .arr, , $(file)))

anas := $(sort $(foreach fstr, $(anafs), $(word 1, $(subst _, , $(fstr)))))
tests := $(sort $(foreach fstr, $(anafs), $(word 2, $(subst _, , $(fstr)))))

analyze: distances measures

distances:
	-rm -rf '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/distances'
	for tes in $(tests) ; do \
		for ana1 in $(anas) ; do \
			for ana2 in $(anas) ; do \
				sed "s#STF1#$$ana1\_$$tes#g" '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/src/distances.arr' | sed "s#STF2#$$ana2\_$$tes#g" > '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/src/sdistances.arr' ; \
				$(call pyret, '../Plan Composition/pyret-starter/planalysis/src/sdistances.arr', '../Plan Composition/pyret-starter/planalysis/bin/sdistances.arr.jarr') ; \
			    node '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/bin/sdistances.arr.jarr' ; \
			done \
		done \
	done

measures:
	-rm -rf '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/measures'
	for tes in $(tests) ; do \
		for ana in $(anas) ; do \
				sed "s#STF#$$ana\_$$tes#g" '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/src/measures.arr' > '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/src/smeasures.arr' ; \
			    $(call pyret, '../Plan Composition/pyret-starter/planalysis/src/smeasures.arr', '../Plan Composition/pyret-starter/planalysis/bin/smeasures.arr.jarr') ; \
			    node '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/bin/smeasures.arr.jarr' ; \
		done \
	done

plots:
	Rscript ./src/visuals.R
	Rscript ./src/clusterplans.R

cleanall: cleansrc cleanobj	cleanout

cleansrc:
	rm $(srcs)

cleanobj:
	rm $(objs)

cleanout:
	rm $(outs)


#------------------------

dist:
	-rm -rf '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data/distances'
	node_modules/.bin/pyret ./planalysis/src/analysis.arr
	node ./planalysis/src/analysis.arr.jarr

tables:
	python planalysis/src/format-outs.py '/Users/np/Projects/Plan Composition/pyret-starter/planalysis/data'

