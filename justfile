alias b := all
alias s := server

OUTPUT:="dist-newstyle/build/js-ghcjs/ghcjs-8.10.7/holbert-0.6/x/app/build/app/app.jsexe"
OUTPUT_NEWSTYLE:="dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/holbert-0.6/x/app/build/app/app.jsexe/"
STATICS:="index.html favicon.PNG euler.woff typicons.* *.min.js cmunfonts *.holbert"
all: cleanup
	cabal build app && cp -R {{STATICS}} {{OUTPUT}}
newstyle: cleanup
	cabal build app && cp -R {{STATICS}} {{OUTPUT_NEWSTYLE}}
test: cleanup
	cabal run test
server: all
	cd {{OUTPUT}} && python3 -m http.server
server_newstyle: newstyle
	cd {{OUTPUT_NEWSTYLE}} && python3 -m http.server
launch: server_newstyle
info:
	happy Parse/Parser.y -i
debug: cleanup
	happy -a -d -i Parse/Parser.y
	cabal build app && cp -R {{STATICS}} {{OUTPUT}}
	cd {{OUTPUT}} && python3 -m http.server
cleanup:
	[ ! -e Parse/Parser.hs ] || rm Parse/Parser.hs
	[ ! -e Parse/Parser.info ] || rm Parse/Parser.info
