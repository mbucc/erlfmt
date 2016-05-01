erlfmt.beam: erlfmt.erl
	erlc erlfmt.erl

clean::
	rm -f *.beam
	rm -f *.dump
