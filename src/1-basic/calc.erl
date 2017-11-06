-module(calc).
-include_lib("eunit/include/eunit.hrl").
-export([tokenize/1
        , parse/1]).

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
% --- tests ---
tokenize_single_number_test() -> [{num, "189"}] = tokenize("189").
tokenize_nested_expression_test() ->
    [{lp, "("}
        , {lp, "("}
            , {num, "2"}, {plus, "+"}, {num, "3"}
        , {rp, ")"}
        , {minus, "-"}, {num, "4"}
    , {rp, ")"}
    ] = tokenize("((2+3)-4)").
tokenize_unknown_token_test() ->
    [{unkwn, "x"},
     {wp, " "},
     {multi, "*"},
     {wp, " "},
     {num, "14"}]
    = tokenize("x * 14").


%% @doc transform token list `TokenList` (builded by tokenize function) to abstract syntax tree
parse(TokenList) -> parse_p(TokenList, [], []).
% terminate parsing if unknown token is reached
parse_p([{unkwn, Value} | _Tail], _Operation, _Operand) -> {error, {unsupported_token, {unkwn, Value}}};
% skip white space token
parse_p([{wp, _Value} | Tail], Operation, Operand) -> parse_p(Tail, Operation, Operand);
% convert string representation of numeric token value to integer representation
parse_p([{num, Value} | Tail], Operation, Operand) when is_list(Value) ->
    parse_p([{num, list_to_integer(Value)} | Tail], Operation, Operand);
% process nested expression to get new token list where
% first element is the syntax tree of nested expression
% and tail contains uprocessed tokens
parse_p([{lp, _Lexeme} | Tail], Operation, Operand) ->
    parse_p(parse_p(Tail, [], []), Operation, Operand);
% process end of nested expression (returns calculated its syntax tree as standalone first item and tail)
parse_p([{rp, _Lexeme} | Tail], _Op, Operand) -> [Operand | Tail];
% process reached operation token (save it into Operation variable)
parse_p([{Operation, _Lexeme} | Tail], [], Operand)
    when is_list(_Lexeme)
            andalso
            % minus, plus and multiplication is allowed only after non-empty suspended operand (value or expression)
            (Operand =/= [] andalso (Operation =:= minus orelse
                                    Operation =:= plus orelse
                                    Operation =:= multi )
            orelse
            % sign is allowed only if all previous operands had been processed
            Operand == [] andalso (Operation =:= sign)
            ) ->
    parse_p(Tail, Operation, Operand);
% save expression as first operand if operation had not been reached yet
parse_p([Expression | Tail], [], _Operand) -> parse_p(Tail, [], Expression);
% assembly and put new node of syntax tree (with unary operation) into head of input list
parse_p([Expression | Tail], Operation, []) -> parse_p([{Operation, Expression} | Tail], [], []);
% assembly and put new node of syntax tree (with binary operation) into head of input list
parse_p([Expression | Tail], Operation, Operand) -> parse_p([{Operation, Operand, Expression} | Tail], [], []);
% return Operand as result expression in empty list is empty
parse_p([], [], Expression) -> Expression.
% --- tests ---
parse_sinlge_number_test() -> {num, 4} = parse([{num, "4"}]).
parse_single_number_in_parentheses_test() ->
    {num, 7} =
    parse([{lp, "("}
            , {lp, "("}
                    , {num, "7"}
            , {rp, ")"}
        , {rp, ")"}
        ]).
parse_one_operation_test() ->
    {plus, {num, 2}, {num, 5}} = parse([{num, "2"}, {plus, "+"}, {num, "5"}]).
parse_two_operations_test() ->
    {minus
        , {plus
            , {num, 2}
            , {num, 5}}
        , {num, 9}
    } = parse([{num, "2"}, {plus, "+"}, {num, "5"}, {minus, "-"}, {num, "9"}]).
parse_nested_expression_at_the_beginning_test() ->
    {minus
        , {sign, {plus
                    , {num, 2}
                    , {num, 5}}}
        , {num, 9}
    } =
    parse([{sign, "~"}, {lp, "("}
                            , {num, "2"}, {plus, "+"}, {num, "5"}
                        , {rp, ")"}
            , {minus, "-"}, {num, "9"}]).
parse_nested_expression_at_the_end_test() ->
    {plus
        , {multi
            , {num, 3}
            , {num, 22}}
        , {minus
            , {num, 2}
            , {num, 5}}
    } =
    parse([ {num, "3"}, {multi, "*"}, {num, "22"}
            , {plus, "+"}, {lp, "("}
                                , {num, "2"}, {minus, "-"}, {num, "5"}
                            , {rp, ")"}]).