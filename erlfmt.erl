%    Copyright (c) 2016, Mark Bucciarelli <mkbucc@gmail.com>
%
%    Permission to use, copy, modify, and/or distribute this software
%    for any purpose with or without fee is hereby granted, provided
%    that the above copyright notice and this permission notice appear
%    in all copies.
%
%    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%    WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%    WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%    AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
%    DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
%    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
%    TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%    PERFORMANCE OF THIS SOFTWARE.

-module(erlfmt).
-export([fmt/0]).

% Filter out tokens that erl_parse.form doesn't support.
formify({white_space,_,_}) ->
    false;
formify({comment,_,_}) ->
    false;
formify(_) ->
    true.

% Give user a clue if nothing happens.
help(true) ->
    ok;
help(false) ->
    io:fwrite("erlfmt: no forms found, try removing any whitespace afte"
              "r the terminating \"dot\"'s in your code.~n",
              []).

% This module's logic assumes that a dot always appears at the end of a line.
% If we find a dot that doesn't follow this rule, let user know and abort.
assertNoDot([]) ->
    ok;
assertNoDot(_) ->
    io:put_chars(standard_error, "erlfmt: dot must be at end of line\n"),
    halt(1).

matches(_Text, []) ->
    false;
matches(Text, [H|T]) ->
    {ok,MP} = re:compile(H),
    case re:run(Text, MP) of
        nomatch ->
            matches(Text, T);
        _ ->
            true
    end.

% Return true if string contains a macro or define.
containsPreprocessorJuju(Text) when is_list(Text) ->
    Regexes =
        ["^ *-define[(]",
         "^ *-include[(]",
         "^ *-include_lib[(]",
         "[?][_[:alnum:]]+"],
    matches(Text, Regexes).


% Write leading whitespace and comments, then parse the remaining
% tokens into a form and pretty-print it.
%
% We drop non-leading whitespace and comment tokens because erl_parse:form
% doesn't work if we leave them in.
%
% Note: erl_parse does not deal with "pre-processor juju" [R. Carlsson].
% See: http://erlang.org/pipermail/erlang-questions/2009-March/042225.html
%
write([{X,_,TokenText}|Tokens], RawText)
    when X == white_space; X == comment ->
    case containsPreprocessorJuju(RawText) of
        true ->
            io:put_chars(RawText);
        false ->
            io:put_chars(TokenText),
            write(Tokens, RawText)
    end;
write(Tokens, RawText) ->
    case containsPreprocessorJuju(RawText) of
        true ->
            io:put_chars(RawText);
        false ->
            FormOnlyTokens = lists:filter(fun formify/1, Tokens),
            case erl_parse:parse_form(FormOnlyTokens) of
                {ok,Form} ->
                    io:fwrite("~s", [erl_pp:form(Form)]);
                {error,Error} ->
                    io:fwrite("erlfmt: ~p~nTokens=~p~n",
                              [Error,FormOnlyTokens])
            end
    end.

	
% Read stdin line-by-line, parsing each line into tokens.
% When we find a line that ends in a dot, print the form to
% stdout.
fmt([{dot,N}|Tokens], _, RawText) ->
    assertNoDot([ 
                 X ||
                     {dot,_} = X <- Tokens
                ]),
    T = lists:reverse(Tokens) ++ [{dot,N}],
    write(T, lists:reverse(RawText)),
    fmt([], true, "");
fmt(Tokens, FoundAForm, RawText) ->
    case io:get_line('') of
        eof ->
            help(FoundAForm),
            halt();
        Data ->
	    TrimmedData = re:replace(Data, "\\.[ \\t]+$", ".", [{return, list}]),
            {ok,T,_} = erl_scan:string(TrimmedData, 0, return),
            fmt(lists:reverse(T) ++ Tokens,
                FoundAForm,
                lists:reverse(TrimmedData) ++ RawText)
    end.

fmt() ->
    fmt([], false, "").
