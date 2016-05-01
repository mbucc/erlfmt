-module(erlfmt).
-export([fmt/0]).

% erl_parse.form doesn't work with whitespace and comment tokens.
formify({white_space, _, _}) -> false;
formify({comment, _, _}) -> false;
formify(_) -> true.

% Only works if a dot is always at end of a line.
fmt([{dot, N}|ReversedTokens]) ->
	Tokens = lists:reverse(ReversedTokens) ++ [{dot, N}],
	FormOnlyTokens = lists:filter(fun formify/1, Tokens),
	{ok, Form} = erl_parse:parse_form(FormOnlyTokens),
	io:fwrite("~s", [erl_pp:form(Form)]),
	fmt([]);

fmt(ReversedTokens) ->
	case io:get_line('') of
		eof -> 
			halt();
		Data ->
			{ok, Tokens, _} = erl_scan:string(Data, 0, return),
			fmt(lists:reverse(Tokens) ++ ReversedTokens)
	end.

fmt() ->
	fmt([]).
