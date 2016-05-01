%    Copyright (c) 2016d, Mark Bucciarelli <mkbucc@gmail.com>
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

% Write any leading whitespace and comment tokens that come before form.
write([{white_space,_,Text}|Tokens]) ->
    io:fwrite("~s", [Text]),
    write(Tokens);
write([{comment,_,Text}|Tokens]) ->
    io:fwrite("~s", [Text]),
    write(Tokens);
write(Tokens) ->
    FormOnlyTokens = lists:filter(fun formify/1, Tokens),
    {ok,Form} = erl_parse:parse_form(FormOnlyTokens),
    io:fwrite("~s", [erl_pp:form(Form)]).
	
% Read stdin line-by-line, parsing each line into tokens.
% When we find a line that ends in a dot, print the form to 
% stdout.
fmt([{dot,N}|ReversedTokens], _) ->
    assertNoDot([ 
                 X ||
                     {dot,_} = X <- ReversedTokens
                ]),
    Tokens = lists:reverse(ReversedTokens) ++ [{dot,N}],
    write(Tokens),
    fmt([], true);
fmt(ReversedTokens, FoundAForm) ->
    case io:get_line('') of
        eof ->
            help(FoundAForm),
            halt();
        Data ->
            {ok,Tokens,_} = erl_scan:string(Data, 0, return),
            fmt(lists:reverse(Tokens) ++ ReversedTokens, FoundAForm)
    end.

fmt() ->
    fmt([], false).
