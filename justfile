alias b := all
alias s := server

OUTPUT:="dist-newstyle/build/js-ghcjs/ghcjs-8.10.7/holbert-0.6/x/app/build/app/app.jsexe"
OUTPUT_NEWSTYLE:="dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/holbert-0.6/x/app/build/app/app.jsexe/"
STATICS:="index.html favicon.PNG euler.woff typicons.* *.min.js cmunfonts *.holbert"
all:
	cabal build && cp -R {{STATICS}} {{OUTPUT}}
newstyle:
	cabal build && cp -R {{STATICS}} {{OUTPUT_NEWSTYLE}}
server: all
	cd {{OUTPUT}} && python3 -m http.server
server_newstyle: newstyle
	cd {{OUTPUT_NEWSTYLE}} && python3 -m http.server
launch: server_newstyle
