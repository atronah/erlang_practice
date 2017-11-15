-module(db).
-export([new/0,
         destroy/1,
         write/3,
         read/2,
         delete/2,
         match/2]).

-include_lib("eunit/include/eunit.hrl").


%% @doc create new empty `Db`
new() -> {}.


%% @doc release resources used for `Db`
destroy(_Db) -> ok.


%% @doc Adds/rewrites item of `Db` asociated with passed `Key`
write(Key, Data, _Db = {}) ->
    {Key, Data, {}, {}};
write(Key, Data, {RootKey, RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, RootData, write(Key, Data, Smaller), Larger};
write(Key, Data, {RootKey, RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, RootData, Smaller, write(Key, Data, Larger)};
write(Key, Data, {Key, _RootData, Smaller, Larger}) ->
    {Key, Data, Smaller, Larger}.


%% @doc Read `Data` from item of `Db` associated with passed `Key`
read(Key, {RootKey, _RootData, Smaller, _Larger}) when Key < RootKey ->
    read(Key, Smaller);
read(Key, {RootKey, _RootData, _Smaller, Larger}) when Key > RootKey ->
    read(Key, Larger);
read(Key, {Key, Data, _Smaller, _Larger}) ->
    {ok, Data};
read(_Key, _Db) -> {error, not_found}.


%% @doc Deletes item of `Db` with passed `Key`
delete(Key, {RootKey, _RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, _RootData, delete(Key, Smaller), Larger};
delete(Key, {RootKey, _RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, _RootData, Smaller, delete(Key, Larger)};
delete(Key, {Key, _Data, {}, {}}) ->
    {};
delete(Key, {Key, _Data, {SmallerKey, SmallerData, Smaller, Larger}, {}}) ->
    {SmallerKey, SmallerData, Smaller, Larger};
delete(Key, {Key, _Data, {}, {LargerKey, LargerData, Smaller, Larger}}) ->
    {LargerKey, LargerData, Smaller, Larger};
delete(Key, {Key, Data, Smaller, Larger}) ->
    delete(Key, rotate_right(Key, {Key, Data, Smaller, Larger}));
delete(_Key, Db) -> Db.


%% @doc Returns list of keys, which associated with passed `Data`
match(Data, Db) -> match_p(Data, [Db], []).
match_p(_Data, [], Result) ->
    Result;
match_p(Data, [{} | Tail], Result) ->
    match_p(Data, Tail, Result);
match_p(Data, [{Key, Data, Smaller, Larger} | Tail], Result) ->
    match_p(Data, [Smaller, Larger | Tail], [Key | Result]);
match_p(Data, [{_Key, _OtherData, Smaller, Larger} | Tail], Result) ->
    match_p(Data, [Smaller, Larger | Tail], Result).


% accessory function to rotate tree (needs to delete nodes with both of children)
rotate_right_test_() ->
    [
        ?_assertEqual({error, not_found}, rotate_right(key, {})),
        {"Check ratating right around element '4' for following tree
                6
               /
              4
             / \
            2   5
           / \
          1   3
         to get following tree:
              6
             /
            2
           / \
          1   4
             / \
            3   5 "
            , ?_assertEqual({6, "6"
                            , {2, "2"
                                , {1, "1", {}, {}}
                                , {4, "4"
                                    , {3, "3", {}, {}}
                                    , {5, "5", {}, {}}}}
                            , {}}
                            , rotate_right(4
                                            , {6, "6"
                                                , {4, "4"
                                                    , {2, "2"
                                                        , {1, "1", {}, {}}
                                                        , {3, "3", {}, {}}}
                                                    , {5, "5", {}, {}}}
                                                , {}}))}
    ].
rotate_right(Key, {RootKey, _RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, _RootData, rotate_right(Key, Smaller), Larger};
rotate_right(Key, {RootKey, _RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, _RootData, Smaller, rotate_right(Key, Larger)};
rotate_right(Key, {Key, Data, {SKey, SData, SSmaller, SLarger}, Larger}) ->
    {SKey, SData, SSmaller, {Key, Data, SLarger, Larger}};
rotate_right(_Key, _Db) -> {error, not_found}.

