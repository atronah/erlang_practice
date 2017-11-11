-module(calc).
-include_lib("eunit/include/eunit.hrl").
-export([evaluate/1
        , evaluate/2
        , optimize/1
        , parse/1
        , tokenize/1
        , compile_instructions/1
        , print_expression/1
        ]).


%% @doc evaluate string expression using stack-machine (with logging each action is Is_Verbose passes as true)
%% @param StringExpression - expression for evaluating
%% @param Is_Verbose - (optional) if true, all actions will be displayed by io:format calls
%% usage example:
%%      calc:evaluate("3+2*(8*2-3)+5", true).
evaluate(StringExpression) -> evaluate(StringExpression, false).
evaluate(StringExpression, Is_Verbose) -> evaluate_p(compile_instructions(StringExpression), [], Is_Verbose).
% -- tests ---
evaluate_test_() ->
    [
        {"single number", ?_assert(evaluate("4") =:= 4)},
        {"single number in parentheses", ?_assert(evaluate("(((12)))") =:= 12)},
        {"minus operation", ?_assert(evaluate("50 - 13") =:= 37)},
        {"plus operation", ?_assert(evaluate("5 + 13") =:= 18)},
        {"multiplication operation", ?_assert(evaluate("33 * 3") =:= 99)},
        {"sign operation", ?_assert(evaluate("~7") =:= -7)},
        {"complex expression", ?_assert(evaluate("4 - (~8) + (3 * (~6)) - (2 + 4)") =:= -12)}
    ].
% --- implementation ---
evaluate_p([{push, Value} | Tail], Stack, Is_Verbose) ->
    if
        Is_Verbose -> io:format("Push `~B` into stack. Stack: ~w~n", [Value, [Value | Stack]]);
        true -> ok
    end,
    evaluate_p(Tail, [Value | Stack], Is_Verbose);
evaluate_p([{sign} | Tail], [Operand | Stack], Is_Verbose) ->
    if
        Is_Verbose ->
                    io:format("Sign~n"),
                    io:format("    Pop `~B` from stack as Operand. Stack: ~w~n", [Operand, Stack]),
                    io:format("    Change sign of `~B` and push result (`~B`) into stack. Stack: ~w~n"
                                , [Operand, -Operand, [-Operand | Stack]]);
        true -> ok
    end,
    evaluate_p(Tail, [-Operand | Stack], Is_Verbose);
evaluate_p([{Operation} | Tail], [SecondOperand, FirstOperand | Stack], Is_Verbose) ->
    {OperationName, OperationSign, OperationResult} =
        case Operation of
            add -> {"Add", "+", FirstOperand + SecondOperand};
            substract -> {"Substract", "-", FirstOperand - SecondOperand};
            multiply -> {"Multiply", "*", FirstOperand * SecondOperand}
        end,
    if
        Is_Verbose ->
                    io:format("~s~n", [OperationName]),
                    io:format("    Pop `~B` from stack as SecondOperand. Stack: ~w~n"
                                , [SecondOperand, [FirstOperand | Stack]]),
                    io:format("    Pop `~B` from stack as FirstOperand. Stack: ~w~n"
                                , [FirstOperand, Stack]),
                    io:format("    Evaluate `~B ~s ~B`~n"
                                , [FirstOperand, OperationSign, SecondOperand]),
                    io:format("    Push result (`~B`) into stack. Stack: ~w~n"
                                , [OperationResult, [OperationResult | Stack]]);
        true -> ok
    end,
    evaluate_p(Tail, [OperationResult | Stack], Is_Verbose);
evaluate_p([], [StackHead | []], Is_Verbose) ->
    if
        Is_Verbose -> io:format("Finished. Result of evaluating is `~B`~n", [StackHead]);
        true -> ok
    end,
    StackHead.
% -----------------------------------------------------------------------------


%% @doc optimize absract syntax tree.
%% supported optimisation:
%%      - `~0`    -> `0`
%%      - `X + 0` -> `X`
%%      - `X - 0` -> `X`
%%      - `X - X` -> `0`
%%      - `X * 0` -> `0`
%%      - `X * 1` -> `X`
optimize(Expression) -> optimize_p([Expression], []).
% --- tests ---
optimize_test_() ->
    {"wide"
        , ?_assert(optimize({multi
                            , {minus
                                , {multi
                                    , {num, 1}
                                    , {plus, {num, 3}, {num, 0}}}
                                , {num, 0}}
                            , {sign, {num, 0}}}
                            )
                    =:= {multi, {num, 3}, {num, 0}})}
        .
% --- implementation ---
% optimize `~0` -> `0`
optimize_p([{sign, {num, 0}} | Tail], Stack) ->
    optimize_p(Tail, [{num, 0} | Stack]);
% optimize `0 + X` -> `X`
optimize_p([{plus, {num, 0}, Operand} | Tail], Stack) ->
    optimize_p([Operand | Tail], Stack);
% optimize `X + 0` -> `X`
optimize_p([{plus, Operand, {num, 0}} | Tail], Stack) ->
    optimize_p([Operand | Tail], Stack);
% optimize `X - 0` -> `X`
optimize_p([{minus, Operand, {num, 0}} | Tail], Stack) ->
    optimize_p([Operand | Tail], Stack);
% optimize `X - X` -> `0`
optimize_p([{minus, Operand, Operand} | Tail], Stack) ->
    optimize_p([Tail], [{num, 0} | Stack]);
% optimize `X * 0` -> `0`
optimize_p([{multi, _Operand, {num, 0}} | Tail], Stack) ->
    optimize_p([Tail], [{num, 0} | Stack]);
% optimize `0 * X` -> `0`
optimize_p([{multi, {num, 0}, _Operand} | Tail], Stack) ->
    optimize_p([Tail], [{num, 0} | Stack]);
% optimize `X * 1` -> `X`
optimize_p([{multi, Operand, {num, 1}} | Tail], Stack) ->
    optimize_p([Operand | Tail], Stack);
% optimize `1 * X` -> `X`
optimize_p([{multi, {num, 1}, Operand} | Tail], Stack) ->
    optimize_p([Operand | Tail], Stack);
% skip optimization for simple number
optimize_p([{num, Value} | Tail], Stack) ->
    optimize_p(Tail, [{num, Value} | Stack]);
% skip optimisation for other unary operations
optimize_p([{Operation, Operand} | Tail], Stack) ->
    optimize_p([Operand | Tail], [Operation | Stack]);
% skip optimize for others binary operations
optimize_p([{Operation, FirstOperand, SecondOperand} | Tail], Stack) ->
    optimize_p([SecondOperand, FirstOperand | Tail], [Operation | Stack]);
% return optimized Expression if it is last item in Stack
optimize_p([], [Expression | []]) -> Expression;
% transform stack representation to expression back
%   for unary operations
optimize_p([], [Operand, Operation | Tail])
    when Operation =:= sign ->
    optimize_p([], [{Operation, Operand} | Tail]);
optimize_p([], [FirstOperand, SecondOperand, Operation | Tail])
    when Operation =:= plus
        orelse Operation =:= minus
        orelse Operation =:= multi
    ->
    optimize_p([], [{Operation, FirstOperand, SecondOperand} | Tail]).
% -----------------------------------------------------------------------------


%% @doc transform token list `TokenList` (builded by tokenize function) to abstract syntax tree
parse(TokenList) -> parse_p(TokenList, [], []).
% --- tests ---
parse_test_() ->
    [
        {"sinlge number: `4` = `4`"
            , ?_assert(parse([{num, "4"}]) =:= {num, 4})},
        {"single number in parentheses: `((7)) = 7`"
            , ?_assert(parse([{lp, "("}, {lp, "("}, {num, "7"}, {rp, ")"}, {rp, ")"}])
                        =:= {num, 7})},
        {"one operation: `2 + 5`"
            , ?_assert(parse([{num, "2"}, {plus, "+"}, {num, "5"}])
                        =:= {plus, {num, 2}, {num, 5}})},
        {"two operations: `2 + 5 - 9`"
            , ?_assert(parse([{num, "2"}, {plus, "+"}, {num, "5"}, {minus, "-"}, {num, "9"}])
                        =:= {minus
                                    , {plus
                                        , {num, 2}
                                        , {num, 5}}
                                    , {num, 9}
                                })},
        {"nested subexpression at the beginning: `~(2 + 5) - 9`"
            , ?_assert(parse([{sign, "~"}, {lp, "("}
                                                , {num, "2"}, {plus, "+"}, {num, "5"}
                                            , {rp, ")"}
                                , {minus, "-"}, {num, "9"}])
                        =:= {minus
                                , {sign, {plus
                                            , {num, 2}
                                            , {num, 5}}}
                                , {num, 9}
                            })},
        {"nested subexpression at the end: `3 * 22 + (2 - 5)`"
            , ?_assert(parse([ {num, "3"}, {multi, "*"}, {num, "22"}
                                , {plus, "+"}, {lp, "("}
                                                    , {num, "2"}, {minus, "-"}, {num, "5"}
                                                , {rp, ")"}])
                        =:= {plus
                                , {multi
                                    , {num, 3}
                                    , {num, 22}}
                                , {minus
                                    , {num, 2}
                                    , {num, 5}}
                            })}
    ].
% --- implementation ---
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
% -----------------------------------------------------------------------------


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
% --- tests ---
tokenize_test_() ->
    [
        {"single number"
            , ?_assert(tokenize("189") =:= [{num, "189"}])},
        {"nested expression"
            , ?_assert(tokenize("((2+3)-4)")
                        =:= [{lp, "("}
                                , {lp, "("}
                                    , {num, "2"}, {plus, "+"}, {num, "3"}
                                , {rp, ")"}
                                , {minus, "-"}, {num, "4"}
                            , {rp, ")"}
                            ]
                        )},
        {"unknown token"
            , ?_assert(tokenize("x * 14")
                        =:= [{unkwn, "x"},
                             {wp, " "},
                             {multi, "*"},
                             {wp, " "},
                             {num, "14"}]
                        )}
    ].
% --- implementation ---
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
% -----------------------------------------------------------------------------


%% @doc compile instructions sequence to evaluate string expression on stack-machine
compile_instructions(StringExpression) -> compile_instructions_p([parse(tokenize(StringExpression))], []).
% --- tests ---
compile_instructions_test_() ->
    {"wide"
        , ?_assert(compile_instructions("3+5-((~6)*9)")
                    =:=[
                            {push, 3},
                            {push, 5},
                            {add},
                            {push, 6},
                            {sign},
                            {push, 9},
                            {multiply},
                            {substract}
                        ])}.
% --- implementation ---
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
% -----------------------------------------------------------------------------


%% @doc prints passed expression
print_expression(Expression) -> lists:reverse(print_expression_p(Expression, [], [])).
% --- tests ---
print_expression_test_() ->
    {"wide"
        , ?_assert(print_expression({plus
                                    , {multi, {num, 3}, {num, 22}}
                                    , {minus, {num, 2}, {sign, {num, 5}}}
                                    })
                    =:= "((3*22)+(2-(~5)))"
                )}.
% --- implementation ---
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

