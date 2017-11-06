-module(calc).
-include_lib("eunit/include/eunit.hrl").
-export([tokenize/1]).

%% @doc gets tokens from passed `String`.
%% Token represents by tuple `{TokenType, Lexeme}`,
%% where `TokenType` takes one of the following values:
%% - `lp` - left parenthesis (")")
%% - `rp` - right parenthesis ("(")
%% - `sign` - unary minus("~")
%% - `minus` - binary minus ("-")
%% - `plus` - binary plus ("+")
%% - `multi` - multiplication sign ("*")
%% - `wp` - white space (" ")
%% - `unkwn` for others
%% and `Lexeme` is value of `Token` in `String`
tokenize(String) -> lists:reverse(tokenize_p(String, {}, [])).
% return result with last Token (with reversed Lexeme) in the head.
tokenize_p([], {TokenType, Lexeme}, Result) ->
    [{TokenType, lists:reverse(Lexeme)} | Result];
% process digit from the input string
tokenize_p([Head | Tail], Token, Result)
  when Head >= $0 andalso Head =< $9 ->
    case Token of
        % add digit to the continuous sequence of digits being processed
        {num, Lexeme} -> tokenize_p(Tail, {num, [Head | Lexeme]}, Result);
        % move previous Token from buffer at the beggining of result
        % and start processing new (numeric) token (put it into buffer)
        {TokenType, Lexeme} -> tokenize_p(Tail
                                            , {num, [Head]}
                                            , [{TokenType, lists:reverse(Lexeme)} | Result]);
        % start processing numeric Token
        _ -> tokenize_p(Tail, {num, [Head]}, Result)
    end;
% process non digit char from input string
tokenize_p([Head | Tail], Token, Result) ->
    Type = case Head of
                $( -> lp;
                $) -> rp;
                $~ -> sign;
                $- -> minus;
                $+ -> plus;
        $* -> multi;
        $  -> wp;
                _ -> unkwn
          end,
    case Token of
        % move previous token from buffer at the beggining of result and put current token into buffer
        {TokenType, Lexeme} -> tokenize_p(Tail
                                            , {Type, [Head]}
                                            , [{TokenType, lists:reverse(Lexeme)} | Result]);
        % put new token into buffer
        _ -> tokenize_p(Tail, {}, [{Type, [Head]} | Result])
    end.
tokenize_test() -> [{lp, "("}, {lp, "("},
		    {num, "2"}, {plus, "+"}, {num, "3"},
		    {rp, ")"},
		    {minus, "-"}, {num, "4"}, {rp, ")"}
		   ] = tokenize("((2+3)-4)").    
tokenize_unkwn_test() -> 
	[{unkwn, "x"}, 
	 {wp, " "},
	 {multi, "*"}, 
	 {wp, " "}, 
	 {sign, "~"}, 
	 {num, "14"}]
	= tokenize("x * ~14").
        
        
