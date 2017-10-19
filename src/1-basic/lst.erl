-module(lst).
-include_lib("eunit/include/eunit.hrl").
-export([create/1
        , create_backward/1
        , print_range/1
        , print_range_odd/1
        , filter/2
        , reverse/1
        , concat/1
        ]).

create_test() -> [1, 2, 3, 4, 5] = create(5).
create(N) -> create_p(N, []).
create_p(0, L) -> L;
create_p(N, L) -> create_p(N-1, [N|L]).
        
create_backward_test() -> [4, 3, 2, 1] = create_backward(4).
create_backward(N) -> create_backward_p(N, []).
create_backward_p(0, L) -> L;
create_backward_p(N, L) -> create_backward_p(N-1, L ++ [N]).

print_range(1) -> io:format("~p~n", [1]);
print_range(N) ->
    print_range(N-1),
    io:format("~p~n", [N]).

print_range_odd(1) -> io:format("~p~n", [1]);
print_range_odd(N) when N rem 2 =/= 0 -> 
    print_range_odd(N-1),
    io:format("~p~n", [N]);
print_range_odd(N) -> print_range_odd(N-1).

filter_test() -> [1, 2, 3] = filter([1, 2, 3, 4, 5], 3).
filter(L, M) -> [X || X <- L, X =< M].

reverse_test() -> [3, 2, 1] = reverse([1, 2, 3]).
reverse(L) -> reverse_p(L, []).
reverse_p([], R) -> R;
reverse_p([H|T], R) -> reverse_p(T, [H|R]).

concat_test() -> [1, 2, 3, 4, 5, 6, ok, 7, 8, 9] = concat([[1, 2], [], [3, 4, 5, 6], ok, [7, 8, 9]]).
concat(L) -> reverse(concat_p(L, [])).
concat_p([[HS|TS]|T], R) -> concat_p([TS|T], [HS|R]); 
concat_p([[]|T], R) -> concat_p(T, R);
concat_p([H|T], R) -> concat_p(T, [H|R]);
concat_p([], R) -> R.

flatten_test() -> [1, 2, 3, 4, 5] = flatten([[1, [2, [3], []], [[[4]]], [5, 6]]]).
flatten(L) -> concat(L).

rdna_test() -> "UUACCGAA" = rdna("AATGGCTT").
rdna(DNA) -> rdna_p(DNA, []).
rdna_p([H|T], R) -> rdna_p(T, [nucl_compl(H)|R]);
rdna_p([], R) -> reverse(R).
nucl_compl('G') -> 'C'; nucl_compl(g) -> c;
nucl_compl('C') -> 'G'; nucl_compl(c) -> g;
nucl_compl('T') -> 'A'; nucl_compl(t) -> a;
nucl_compl('A') -> 'U'; nucl_compl(a) -> u.

