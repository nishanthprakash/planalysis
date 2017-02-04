
buildpyret:
	npm install

buildmain:
	#./node_modules/.bin/pyret main.arr
	./node_modules/.bin/pyret ../planalysis/anf-main.arr

transform:
	#node main.arr.jarr
	node ../planalysis/anf-main.arr.jarr

clean:
	rm *.jarr

rmtran:
	rm ./data/transformed/*

