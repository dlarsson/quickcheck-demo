export PATH:=$(PATH):/usr/local/texlive/2016/bin/x86_64-darwin/

%.tex: %.lhs
	lhs2tex --poly -o $@ $<

%.pdf: %.tex
	echo $(PATH)
	echo $$PATH
	/usr/local/texlive/2016/bin/x86_64-darwin/xelatex $<

all: src/Data/Encoding/Weird.pdf
