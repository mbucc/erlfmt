erlfmt.beam: erlfmt.erl
	erlc erlfmt.erl

install: ${HOME}/bin/erlfmt.beam ${HOME}/bin/erlfmt

${HOME}/bin/erlfmt.beam: erlfmt.beam
	cp erlfmt.beam ${HOME}/bin/

${HOME}/bin/erlfmt: erlfmt
	cp erlfmt ${HOME}/bin/

clean::
	rm -f *.beam
	rm -f *.dump
