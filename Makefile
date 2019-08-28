.PHONY : clean

app/acrostic.min.js : app/acrostic.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$@

app/acrostic.js : src/acrostic.elm src/*.elm
	elm make $< --output=$@

#deploy : app/wave.min.js
#	git subtree push --prefix app origin gh-pages

clean :
	rm app/acrostic.js app/acrostic.min.js
