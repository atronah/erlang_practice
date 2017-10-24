-module(json).
-include_lib("eunit/include/eunit.hrl").
-export([new/1,
	read/2,
	write/3]).


%% @doc creates new JSON object from list `List` of pairs {`Key`, `Value`}
new(List) -> 
	if is_ValueSpec_list(List, is_Key_ValueSpec_Pair) -> new_p(List, maps:new());
		true -> {error, bad_input}
	end.
new_p([], Map) -> Map;
new_p([{Key, Value}|Tail], Map) -> new_p(Tail, maps:put(Key, Value, Map));
new_p(_, _) -> {error, bad_input}.
new_empty_test() -> #{} = new([]).
new_non_empty_test() -> #{"a" := "A", "b" := "B"} = new([{"a", "A"}, {"b", "B"}]).
new_not_list_input_test() -> {error, bad_input} = new(1).
new_not_tuple_input_test() -> {error, bad_input} = new([1]).

%% @doc returns value from JSON with key `Key`.
read(Key, JSON) ->
	if is_KeySpec(Key) ->
		case JSON of
			#{Key := Value} -> {ok, Value};
			_Other ->  {error, not_found}
		end;
		true -> {error, bad_input}
	end.
read_ok_test() -> {ok, [4, 5, 6]} = read("y", new([{"x", [1, 2, 3]}, {"y", [4, 5, 6]}])).
read_error_test() -> {error, not_found} = read(z, new([{"x", [1, 2, 3]}, {"y", [4, 5, 6]}])).


%% @doc puts new `Value` into `JSON` with `Key`
write(Key, Value, JSON) -> 
	if is_KeySpec(Key) andalso is_ValueSpec(Value) ->
		case JSON of
			#{Key := _OldValue} -> JSON#{Key := Value};
			_Other ->  {error, not_found}
		end;
		true -> {error, bad_input}.
write_ok_test() -> #{"x" := true, "y" := false} = write("y", false, new([{"x", true}, {"y", true}])). 
write_error_test() -> {error, not_found} = write("z", false, new([{"x", true}, {"y", true}])). 

%% @doc checks that `Value` is string (list of integers)
is_string([Head|Tail]) when is_integer(Head) -> 
	is_string_p(Tail, true).
is_string_p([], true) -> true;
is_string_p(_Other, _Status) -> false.

%% @doc checks that `Value` matches the specification `Key = string()`
is_Key(Value) -> is_string(Value).

%% @doc checks that `Value` matches the specification `KeySpec = string()`
is_KeySpec(Value) -> is_string(Value).

%% @doc checks that `Value` matches the specification `BasicValue = string() | boolean() | integer() | float()`
is_BasicValue(Value) -> is_string(Value) 
                        orelse is_boolean(Value)
                        orelse is_integer(Value)
                        orelse is_float(Value).

%% @doc checks that passed argument matches the specification `{Key, ValueSpec}` 
is_Key_ValueSpec_Pair({Key, ValueSpec}) -> is_Key(Key) andalso is_ValueSpec(ValueSpec);
is_Key_ValueSpec_Pair(_Other) -> false.

%% @doc checks that `Value` matches the specification `ValueSpec = BasicValue | [BasicValue] | {Key, ValueSpec} | [{Key, ValueSpec}]`
is_ValueSpec([Head|Tail]) -> is_BasicValue(Head) andalso is_ValueSpec_list(Tail, is_BasicValue)
                            orelse is_Key_ValueSpec_Pair(Head) andalso is_ValueSpec_list(Tail, is_Key_ValueSpec_Pair);
is_ValueSpec(Value) -> is_BasicValue(Value) orelse is_Key_ValueSpec_Pair(Value).
is_ValueSpec_list([Head|Tail], Checker) -> Checker(Head) andalso is_ValueSpec_list(Tail, Checker);
is_ValueSpec_list([], _Checker) -> true;
is_ValueSpec_list(_Other, _Checker) -> false.
