erlfmt.beam: erlfmt.erl
	erlc erlfmt.erl

install: erlfmt.beam
	cp erlfmt ${HOME}/bin/
	cp erlfmt.beam ${HOME}/bin/

clean::
	rm -f *.beam
	rm -f *.dump
