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
read(_Key, _JSON) -> not_implemented.

%% @doc puts new `Value` into `JSON` with `Key`
write(_Key, _Value, _JSON) -> not_implemented.


