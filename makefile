
buildpyret:
	npm install

buildmain:
	./node_modules/.bin/pyret main.arr

transform:
	node main.arr.jarr

clean:
	rm *.jarr

rmtran:
	rm ./data/transformed/*

