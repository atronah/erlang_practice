-module(lst).
-include_lib("eunit/include/eunit.hrl").
-export([create/1
        , create_backward/1
        , print_range/1
        , print_range_odd/1
        , filter/2
        , reverse/1
        , concat/1
        , flatten/1
	, dna_to_rna/1
        ]).

%% @doc makes list with numbers from `1` to `N` (inckuding).
create(N) -> create_p(N, []).
create_p(0, List) -> List;
create_p(N, List) -> create_p(N-1, [N|List]).
create_test() -> [1, 2, 3, 4, 5] = create(5).

%% @doc makes list with numbers from `N` to `1` (including).
create_backward(N) -> create_backward_p(N, []).
create_backward_p(0, Result) -> Result;
create_backward_p(N, Result) -> create_backward_p(N-1, Result ++ [N]).
create_backward_test() -> [4, 3, 2, 1] = create_backward(4).

%% @doc prints numbers from `1` to `N` (including), each with new line.
print_range(1) -> io:format("~p~n", [1]);
print_range(N) ->
    print_range(N-1),
    io:format("~p~n", [N]).

%% @doc prints odd numbers from `1` to `N` (including), each with new line.
print_range_odd(1) -> io:format("~p~n", [1]);
print_range_odd(N) when N rem 2 =/= 0 -> 
    print_range_odd(N-1),
    io:format("~p~n", [N]);
print_range_odd(N) -> print_range_odd(N-1).

%% @doc makes list containing numbers from `List` which less than `Max` or equal it
filter(List, Max) -> [Item || Item <- List, Item =< Max].
filter_test() -> [1, 2, 3] = filter([1, 2, 3, 4, 5], 3).

%% @doc makes list with reversed items of `List`
reverse(List) -> reverse_p(List, []).
reverse_p([], Result) -> Result;
reverse_p([Head|Tail], Result) -> reverse_p(Tail, [Head|Result]).
reverse_test() -> [3, 2, 1] = reverse([1, 2, 3]).

%% @doc concatinates items from nested list of `List`
concat(List) -> reverse(concat_p(List, [])).
concat_p([[SubHead|SubTail]|Tail], Result) -> concat_p([SubTail|Tail], [SubHead|Result]); 
concat_p([[]|Tail], Result) -> concat_p(Tail, Result);
concat_p([Head|Tail], Result) -> concat_p(Tail, [Head|Result]);
concat_p([], Result) -> Result.
concat_test() -> [1, 2, 3, 4, 5, 6, ok, 7, 8, 9] = concat([[1, 2], [], [3, 4, 5, 6], ok, [7, 8, 9]]).

%% @doc transforms `List` with nested lists to flat view 
flatten(List) -> flatten_p(List, []).
flatten_p([[Head|Tail]], Result) -> flatten_p([Head|Tail], Result);
flatten_p([[]|Tail], Result) -> flatten_p(Tail, Result);
flatten_p([Head|Tail], Result) -> flatten_p(concat(Tail), [Head|Result]);
flatten_p([], Result) -> reverse(Result).
flatten_test() -> [1, 2, 3, 4, 5, 6] = flatten([[1, [2, [3], []], [[[4]]], [5, 6]]]).

%% @doc transforms DNA to RNA
%% @param DNA sequence of nucleatides, represented either as a list of atoms (`[a, Tail, g, c]`) or as a uppercase string (`ATGC`)
%% @returns sequence of nucleatides complimentary to `DNA` nucleatides and represented as a list of atoms (`[a, u, g, c]`)
dna_to_rna(DNA) -> dna_to_rna_p(nucl_string_to_atoms(DNA), []).
dna_to_rna_p([Head|Tail], Result) -> dna_to_rna_p(Tail, [nucl_compliment(Head)|Result]);
dna_to_rna_p([], Result) -> reverse(Result).
dna_to_rna_test() -> [u, u, a, c, c, g, a, a] = dna_to_rna("AATGGCTT").

%% @doc tranform letter representaion of nucleatide to atom representation
nucl_atom($G) -> g; 
nucl_atom($C) -> c; 
nucl_atom($T) -> t;
nucl_atom($A) -> a;
nucl_atom(Other) -> Other.

%% @doc transaform string represenation of nucleatides to list of atoms
nucl_string_to_atoms(String) -> nucl_string_to_atoms_p(String, []).
nucl_string_to_atoms_p([], Result) -> reverse(Result);
nucl_string_to_atoms_p([Head|Tail], Result) -> 
	nucl_string_to_atoms_p(Tail, [nucl_atom(Head)|Result]).

%% @doc transform DNA nucleatide to complimentary RDNA nucleatide
nucl_compliment(c) -> g;
nucl_compliment(g) -> c;
nucl_compliment(t) -> a;
nucl_compliment(a) -> u.
