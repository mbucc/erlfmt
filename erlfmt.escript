#!/usr/bin/env escript
%%! -shutdown_time 1000
main([Name]) ->
	erl_tidy:file(Name, [{stdout, true}, {keep_unused, true}]);
main(_) ->
    usage().

usage() ->
    io:put_chars("usage: erlfmt.escript <file>\n"),
    halt(1).
