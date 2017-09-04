pyret-base := /Users/np/Projects/pyret-lang
data := data

rebuild:
	-rm -rf node_modules
	-rm -rf compiled
	-rm -rf undefined
	-rm .pyret-parley.1705.*
	npm install

pyret = cd $(pyret-base) && gtimeout 2500 node build/phaseA/pyret.jarr \
    --build-runnable $(1)  \
    --outfile $(2) \
    --builtin-js-dir src/js/trove \
    --builtin-arr-dir src/arr/trove \
    --require-config src/scripts/standalone-configA.json


everything: checkbads filterbads transform execute #analyze plots

clusterplots: transform execute measures

studs := $(wildcard $(data)/student-codes/*)
srcs := $(foreach dir, $(studs), $(wildcard $(dir)/final-submission/earthquake-*.arr))
objs := $(foreach dir, $(studs), $(wildcard $(dir)/final-submission/earthquake-*.arr.jarr))

checkbads:
	-rm '/Users/np/Projects/Plan Composition/planalysis/data/student-codes/.DS_Store'
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/cleaning'
	mkdir -p '/Users/np/Projects/Plan Composition/planalysis/data/cleaning'
	$(call pyret, '../Plan Composition/planalysis/src/cleantest.arr', '../Plan Composition/planalysis/bin/cleantest.arr.jarr') 
	for src in $(srcs) ; do \
		node "/Users/np/Projects/Plan Composition/planalysis/bin/cleantest.arr.jarr" "/Users/np/Projects/Plan Composition/planalysis/$$src" ; \
	done


cstuds := $(wildcard $(data)/cleaning/*)
csrcs := $(foreach dir, $(cstuds), $(wildcard $(dir)/final-submission/*.arr))
cobjs := $(foreach dir, $(cstuds), $(wildcard $(dir)/final-submission/*.arr.jarr))

filterbads:
	-rm '/Users/np/Projects/Plan Composition/planalysis/data/cleaning/.DS_Store'
	for src in $(csrcs) ; do \
		$(call pyret, "../Plan Composition/planalysis/$$src", "../Plan Composition/planalysis/$$src.jarr") || rm "/Users/np/Projects/Plan Composition/planalysis/$$src" ; \
		node "/Users/np/Projects/Plan Composition/planalysis/$$src.jarr" || rm "/Users/np/Projects/Plan Composition/planalysis/$$src" ; \
	done


ccstuds := $(wildcard $(data)/cleaning/*)
ccsrcs := $(foreach dir, $(ccstuds), $(wildcard $(dir)/final-submission/earthquake-*.arr))
ccobjs := $(foreach dir, $(ccstuds), $(wildcard $(dir)/final-submission/earthquake-*.arr.jarr))

transform:
	-rm '/Users/np/Projects/Plan Composition/planalysis/data/student-codes/.DS_Store'
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/transformed-json'
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/transformed-arr'
	$(call pyret, '../Plan Composition/planalysis/src/anf-main.arr', '../Plan Composition/planalysis/bin/anf-main.arr.jarr') 
	for src in $(ccsrcs) ; do \
		node "/Users/np/Projects/Plan Composition/planalysis/bin/anf-main.arr.jarr" --src "/Users/np/Projects/Plan Composition/planalysis/$$src" ; \
	done

tstuds := $(wildcard $(data)/transformed-json/*)
tsrcs := $(foreach dir, $(tstuds), $(wildcard $(dir)/final-submission/*.arr))
tobjs := $(foreach dir, $(tstuds), $(wildcard $(dir)/final-submission/*.arr.jarr))

execute:
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/json-anf'
	for src in $(tsrcs) ; do \
		$(call pyret, "../Plan Composition/planalysis/$$src", "../Plan Composition/planalysis/$$src.jarr") ; \
		node "/Users/np/Projects/Plan Composition/planalysis/$$src.jarr" ; \
	done

anafs := $(foreach file, $(notdir $(wildcard $(data)/anfdata/*.arr)), $(subst .arr, , $(file)))
anas := $(sort $(foreach fstr, $(anafs), $(word 1, $(subst _, , $(fstr)))))
tests := $(sort $(foreach fstr, $(anafs), $(word 2, $(subst _, , $(fstr)))))



#------------- older -----------

analyze: distances measures

distances:
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/distances'
	for tes in $(tests) ; do \
		for ana1 in $(anas) ; do \
			for ana2 in $(anas) ; do \
				sed "s#STF1#$$ana1\_$$tes#g" '/Users/np/Projects/Plan Composition/planalysis/src/distances.arr' | sed "s#STF2#$$ana2\_$$tes#g" > '/Users/np/Projects/Plan Composition/planalysis/src/sdistances.arr' ; \
				$(call pyret, '../Plan Composition/planalysis/src/sdistances.arr', '../Plan Composition/planalysis/bin/sdistances.arr.jarr') ; \
			    node '/Users/np/Projects/Plan Composition/planalysis/bin/sdistances.arr.jarr' ; \
			done \
		done \
	done

measures:
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/measures'
	for tes in $(tests) ; do \
		for ana in $(anas) ; do \
				sed "s#STF#$$ana\_$$tes#g" '/Users/np/Projects/Plan Composition/planalysis/src/measures.arr' > '/Users/np/Projects/Plan Composition/planalysis/src/smeasures.arr' ; \
			    $(call pyret, '../Plan Composition/planalysis/src/smeasures.arr', '../Plan Composition/planalysis/bin/smeasures.arr.jarr') ; \
			    node '/Users/np/Projects/Plan Composition/planalysis/bin/smeasures.arr.jarr' ; \
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
	-rm -rf '/Users/np/Projects/Plan Composition/planalysis/data/distances'
	node_modules/.bin/pyret ./planalysis/src/analysis.arr
	node ./planalysis/src/analysis.arr.jarr

tables:
	python planalysis/src/format-outs.py '/Users/np/Projects/Plan Composition/planalysis/data'

