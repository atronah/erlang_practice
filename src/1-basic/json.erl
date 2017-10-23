-module(json).
-include_lib("eunit/include/eunit.hrl").
-export([new/1,
	read/2,
	write/3]).


%% @doc creates new JSON object from list `List` of pairs {`Key`, `Value`}
new(List) -> new_p(List, maps:new()).
new_p([], Map) -> Map;
new_p([{Key, Value}|Tail], Map) -> new_p(Tail, maps:put(Key, Value, Map));
new_p(_, _) -> {error, bad_input}.
new_empty_test() -> #{} = new([]).
new_non_empty_test() -> #{a := "A", b := "B"} = new([{a, "A"}, {b, "B"}]).
new_not_list_input_test() -> {error, bad_input} = new(1).
new_not_tuple_input_test() -> {error, bad_input} = new([1]).

%% @doc returns value from JSON with key `Key`.
read(Key, JSON) ->
	case JSON of
		#{Key := Value} -> {ok, Value};
		_Other ->  {error, not_found}
	end.
read_ok_test() -> {ok, [4, 5, 6]} = read(y, new([{x, [1, 2, 3]}, {y, [4, 5, 6]}])).
read_error_test() -> {error, not_found} = read(z, new([{x, [1, 2, 3]}, {y, [4, 5, 6]}])).


%% @doc puts new `Value` into `JSON` with `Key`
write(Key, Value, JSON) -> 
	case JSON of
		#{Key := OldValue} -> JSON#{Key := Value};
		_Other ->  {error, not_found}
	end.
write_ok_test() -> #{x := true, y := false} = write(y, false, new([{x, true}, {y, true}])). 
write_error_test() -> {error, not_found} = write(z, false, new([{x, true}, {y, true}])). 

