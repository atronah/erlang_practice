-module(calc).
-include_lib("eunit/include/eunit.hrl").
-export([tokenize/1
        , parse/1
        , evaluate/1
        , print_expression/1
        , compile_instructions/1
        , evaluate_instructions/1
        ]).

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
tokenize(String) -> lists:reverse(tokenizevaluate_p(String, {}, [])).
% return result with last Token (with reversed Lexeme) in the head.
tokenizevaluate_p([], {TokenType, Lexeme}, Result) ->
    [{TokenType, lists:reverse(Lexeme)} | Result];
% process digit from the input string
tokenizevaluate_p([Head | Tail], Token, Result)
  when Head >= $0 andalso Head =< $9 ->
    case Token of
        % add digit to the continuous sequence of digits being processed
        {num, Lexeme} -> tokenizevaluate_p(Tail, {num, [Head | Lexeme]}, Result);
        % move previous Token from buffer at the beggining of result
        % and start processing new (numeric) token (put it into buffer)
        {TokenType, Lexeme} -> tokenizevaluate_p(Tail
                                            , {num, [Head]}
                                            , [{TokenType, lists:reverse(Lexeme)} | Result]);
        % start processing numeric Token
        _ -> tokenizevaluate_p(Tail, {num, [Head]}, Result)
    end;
% process non digit char from input string
tokenizevaluate_p([Head | Tail], Token, Result) ->
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
        {TokenType, Lexeme} -> tokenizevaluate_p(Tail
                                            , {Type, [Head]}
                                            , [{TokenType, lists:reverse(Lexeme)} | Result]);
        % put new token into buffer
        _ -> tokenizevaluate_p(Tail, {}, [{Type, [Head]} | Result])
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
parse(TokenList) -> parsevaluate_p(TokenList, [], []).
% terminate parsing if unknown token is reached
parsevaluate_p([{unkwn, Value} | _Tail], _Operation, _Operand) -> {error, {unsupported_token, {unkwn, Value}}};
% skip white space token
parsevaluate_p([{wp, _Value} | Tail], Operation, Operand) -> parsevaluate_p(Tail, Operation, Operand);
% convert string representation of numeric token value to integer representation
parsevaluate_p([{num, Value} | Tail], Operation, Operand) when is_list(Value) ->
    parsevaluate_p([{num, list_to_integer(Value)} | Tail], Operation, Operand);
% process nested expression to get new token list where
% first element is the syntax tree of nested expression
% and tail contains uprocessed tokens
parsevaluate_p([{lp, _Lexeme} | Tail], Operation, Operand) ->
    parsevaluate_p(parsevaluate_p(Tail, [], []), Operation, Operand);
% process end of nested expression (returns calculated its syntax tree as standalone first item and tail)
parsevaluate_p([{rp, _Lexeme} | Tail], _Op, Operand) -> [Operand | Tail];
% process reached operation token (save it into Operation variable)
parsevaluate_p([{Operation, _Lexeme} | Tail], [], Operand)
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
    parsevaluate_p(Tail, Operation, Operand);
% save expression as first operand if operation had not been reached yet
parsevaluate_p([Expression | Tail], [], _Operand) -> parsevaluate_p(Tail, [], Expression);
% assembly and put new node of syntax tree (with unary operation) into head of input list
parsevaluate_p([Expression | Tail], Operation, []) -> parsevaluate_p([{Operation, Expression} | Tail], [], []);
% assembly and put new node of syntax tree (with binary operation) into head of input list
parsevaluate_p([Expression | Tail], Operation, Operand) -> parsevaluate_p([{Operation, Operand, Expression} | Tail], [], []);
% return Operand as result expression in empty list is empty
parsevaluate_p([], [], Expression) -> Expression.
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

%% @doc evaluates passed string expression
evaluate(StringExpression) -> evaluate_p([parse(tokenize(StringExpression))], []).
evaluate_p([{num, Value} | Tail], Stack) ->
    evaluate_p(Tail, [Value | Stack]);
evaluate_p([{UnaryOperation, Operand} | Tail], Stack) ->
    evaluate_p([Operand | [UnaryOperation | Tail]],  Stack);
evaluate_p([{Operation, FirstOperand, SecondOperand} | Tail], Stack) ->
    evaluate_p([FirstOperand | [SecondOperand | [Operation | Tail]]],  Stack);
evaluate_p([sign | Tail], [Operand | Stack]) ->
    evaluate_p(Tail, [- Operand | Stack]);
evaluate_p([Operation | Tail], [SecondOperand,  FirstOperand | Stack]) ->
    evaluate_p(Tail
        , [case Operation of
                minus -> FirstOperand - SecondOperand;
                plus -> FirstOperand + SecondOperand;
                multi -> FirstOperand * SecondOperand
            end
            | Stack]);
evaluate_p([], [Head | []]) -> Head.
% --- tests ---
evaluate_single_number_test() ->
    4 = evaluate("4").
evaluate_single_number_in_parentheses_test() ->
    12 = evaluate("(((12)))").
evaluate_minus_test() ->
    37 = evaluate("50 - 13").
evaluate_plus_test() ->
    18 = evaluate("5 + 13").
evaluate_multiplication_test() ->
    99 = evaluate("33 * 3").
evaluate_sign_test() -> -7 = evaluate("~7").
evaluete_complex_expression_test() ->
    -12 = evaluate("4 - (~8) + (3 * (~6)) - (2 + 4)").


%% @doc prints passed expression
print_expression(Expression) -> lists:reverse(print_expression_p(Expression, [], [])).
print_expression_p({}, [], Result) -> Result;
print_expression_p($), Buffer, Result) ->
    print_expression_p({}, Buffer, [$) | Result]);
% process numeric values (convert to string representation)
print_expression_p({num, Value}, Buffer, Result) when is_integer(Value) ->
    print_expression_p({num, integer_to_list(Value)}, Buffer, Result);
% move string representation of numeric numeric char-by-char into result
print_expression_p({num, [Head | Tail]}, Buffer, Result) ->
    print_expression_p({num, Tail}, Buffer, [Head | Result]);
% finish processing string representation of numeric values
print_expression_p({num, []}, Buffer, Result) ->
    print_expression_p({}, Buffer, Result);
% process unary operation of expression
print_expression_p({Operation, Value}, Buffer, Result) ->
    print_expression_p({Operation, {}, Value}, Buffer, Result);
% process binary operation of expression
print_expression_p({Operation, FirstOperand, SecondOperand}, Buffer, Result) ->
    print_expression_p(FirstOperand, [Operation | [SecondOperand | [$) | Buffer]]], [$( | Result]);
% process values from buffer
print_expression_p({}, [Head|Tail], Result) ->
    print_expression_p(Head, Tail, Result);
% process operation lexeme
print_expression_p(Operation, Buffer, Result) ->
    print_expression_p({}
                        , Buffer
                        , [case Operation of
                                sign -> $~;
                                minus -> $-;
                                plus -> $+;
                                multi -> $*
                            end
                            | Result]).
% --- tests ---
print_expression_test() ->
    "((3*22)+(2-(~5)))" =
    print_expression({plus
                        , {multi, {num, 3}, {num, 22}}
                        , {minus, {num, 2}, {sign, {num, 5}}}
                    }).


%% @doc compile instructions sequence to evaluate string expression on stack-machine
compile_instructions(StringExpression) -> compile_instructions_p([parse(tokenize(StringExpression))], []).
compile_instructions_p([{num, Value} | Tail], Result) ->
    compile_instructions_p(Tail, [{push, Value} | Result]);
compile_instructions_p([{UnaryOperation, Operand} | Tail], Result) ->
    compile_instructions_p([Operand | [UnaryOperation | Tail]],  Result);
compile_instructions_p([{Operation, FirstOperand, SecondOperand} | Tail], Result) ->
    compile_instructions_p([FirstOperand | [SecondOperand | [Operation | Tail]]],  Result);
compile_instructions_p([Operation | Tail], Stack) ->
    compile_instructions_p(Tail
        , [{case Operation of
                sign -> sign;
                minus -> substract;
                plus -> add;
                multi -> multiply
            end}
            | Stack]);
compile_instructions_p([], Stack) -> lists:reverse(Stack).
% --- tests ---
compile_instructions_test() ->
    [{push, 3}
    , {push, 5}
    , {add}
    , {push, 6}
    , {sign}
    , {push, 9}
    , {multiply}
    , {substract}
    ] = compile_instructions("3+5-((~6)*9)").


%% @doc evaluate expression represented by instructions sequence for stack-machine with logging each action
%% usage example:
%%      calc:evaluate_instructions(calc:compile_instructions("3+2*(8*2-3)+5")).
evaluate_instructions(Instructions) -> evaluate_instructions_p(Instructions, []).
evaluate_instructions_p([{push, Value} | Tail], Stack) ->
    io:format("Push `~B` into stack. Stack: ~w~n", [Value, [Value | Stack]]),
    evaluate_instructions_p(Tail, [Value | Stack]);
evaluate_instructions_p([{sign} | Tail], [Operand | Stack]) ->
    io:format("Sign~n"),
    io:format("    Pop `~B` from stack as Operand. Stack: ~w~n", [Operand, Stack]),
    io:format("    Change sign of `~B` and push result (`~B`) into stack. Stack: ~w~n"
                , [Operand, -Operand, [-Operand | Stack]]),
    evaluate_instructions_p(Tail, [-Operand | Stack]);
evaluate_instructions_p([{Operation} | Tail], [SecondOperand, FirstOperand | Stack]) ->
    {OperationName, OperationSign, OperationResult} =
        case Operation of
            add -> {"Add", "+", FirstOperand + SecondOperand};
            substract -> {"Substract", "-", FirstOperand - SecondOperand};
            multiply -> {"Multiply", "*", FirstOperand * SecondOperand}
        end,
    io:format("~s~n", [OperationName]),
    io:format("    Pop `~B` from stack as SecondOperand. Stack: ~w~n", [SecondOperand, [FirstOperand | Stack]]),
    io:format("    Pop `~B` from stack as FirstOperand. Stack: ~w~n", [FirstOperand, Stack]),
    io:format("    Evaluate `~B ~s ~B`~n", [FirstOperand, OperationSign, SecondOperand]),
    io:format("    Push result (`~B`) into stack. Stack: ~w~n" , [OperationResult, [OperationResult | Stack]]),
    evaluate_instructions_p(Tail, [OperationResult | Stack]);
evaluate_instructions_p([], [StackHead | []]) ->
    io:format("Finished. Result of evaluating is `~B`~n", [StackHead]),
    StackHead.