.PHONY : all creator player deploy clean

all : creator player

creator : app/creator.min.js 

player : app/player.min.js

app/%.min.js : app/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $@

app/creator.js : src/Creator.elm src/*.elm
	elm make $< --output=$@

app/player.js : src/Player.elm src/*.elm
	elm make $< --output=$@

deploy : app/creator.min.js app/player.min.js
	git subtree push --prefix app origin gh-pages

clean :
	-rm app/creator.js app/creator.min.js
	-rm app/player.js app/player.min.js
