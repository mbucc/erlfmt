install: ${HOME}/bin/erlfmt ${HOME}/bin/erlfmt.escript
${HOME}/bin/erlfmt: erlfmt
	cp -i -a erlfmt ${HOME}/bin/erlfmt

${HOME}/bin/erlfmt.escript: erlfmt.escript
	cp -i -a erlfmt.escript ${HOME}/bin/erlfmt.escript
