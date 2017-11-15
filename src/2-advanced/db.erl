-module(db).
-export([new/1,
         destroy/1,
         write/3,
         append/3,
         read/2,
         delete/2,
         match/2,
         batch_read/2,
         batch_delete/2
         ]).

-include_lib("eunit/include/eunit.hrl").


%% @doc create new empty `Db` with specified parameters `Params`
new(Params) -> {Params, {}}.


%% @doc release resources used for `Db`
destroy(_Db) -> ok.


%% @doc Adds/rewrites item of `Db` asociated with passed `Key`
write(Key, Data, {Params, Storage}) ->
    write(Key, Data, {Params, Storage}, Params).
% default value for `append` is `allow`
write(Key, Data, _Db, []) ->
    write(Key, Data, _Db, [{append, allow}]);
% if appending is allow, starts writings
write(Key, Data, {Params, Storage}, [{append, allow} | _ParamsTail]) ->
    {Params, write_p(Key, Data, Storage)};
% if appending is deny, returns error
write(_Key, _Data, _Db, [{append, deny} | _ParamsTail]) ->
    {error, access_denied};
% skip others params
write(Key, Data, Storage, [_Param | ParamsTail]) ->
    write(Key, Data, Storage, ParamsTail).
% --- implementation ---
write_p(Key, Data, _Storage = {}) ->
    {Key, Data, {}, {}};
write_p(Key, Data, {RootKey, RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, RootData, write_p(Key, Data, Smaller), Larger};
write_p(Key, Data, {RootKey, RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, RootData, Smaller, write_p(Key, Data, Larger)};
write_p(Key, Data, {Key, _RootData, Smaller, Larger}) ->
    {Key, Data, Smaller, Larger}.


%% @doc alias for write function
append(Key, Data, Db)
    -> write(Key, Data, Db).


%% @doc Read `Data` from item of `Db` associated with passed `Key`
read(Key, {_Params, Storage}) ->
    read_p(Key, Storage).
read_p(Key, {RootKey, _RootData, Smaller, _Larger}) when Key < RootKey ->
    read_p(Key, Smaller);
read_p(Key, {RootKey, _RootData, _Smaller, Larger}) when Key > RootKey ->
    read_p(Key, Larger);
read_p(Key, {Key, Data, _Smaller, _Larger}) ->
    {ok, Data};
read_p(_Key, _Storage) -> {error, not_found}.


%% @doc Deletes item of `Db` with passed `Key`
delete(Key, {Params, Storage}) ->
    {Params, delete_p(Key, Storage)}.
delete_p(Key, {RootKey, _RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, _RootData, delete_p(Key, Smaller), Larger};
delete_p(Key, {RootKey, _RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, _RootData, Smaller, delete_p(Key, Larger)};
delete_p(Key, {Key, _Data, {}, {}}) ->
    {};
delete_p(Key, {Key, _Data, {SmallerKey, SmallerData, Smaller, Larger}, {}}) ->
    {SmallerKey, SmallerData, Smaller, Larger};
delete_p(Key, {Key, _Data, {}, {LargerKey, LargerData, Smaller, Larger}}) ->
    {LargerKey, LargerData, Smaller, Larger};
delete_p(Key, {Key, Data, Smaller, Larger}) ->
    delete_p(Key, rotate_right(Key, {Key, Data, Smaller, Larger}));
delete_p(_Key, Storage) -> Storage.


%% @doc Returns list of keys, which associated with passed `Data`
match(Data, {_Params, Storage}) ->
    match_p(Data, [Storage], []).
match_p(_Data, [], Result) ->
    Result;
match_p(Data, [{} | Tail], Result) ->
    match_p(Data, Tail, Result);
match_p(Data, [{Key, Data, Smaller, Larger} | Tail], Result) ->
    match_p(Data, [Smaller, Larger | Tail], [Key | Result]);
match_p(Data, [{_Key, _OtherData, Smaller, Larger} | Tail], Result) ->
    match_p(Data, [Smaller, Larger | Tail], Result).


%% @doc Returns list of {Key, Data} for each matching keys from passed `KeyList`
batch_read(KeyList, {Params, Storage}) ->
    batch_read(KeyList, {Params, Storage}, Params).
%default batch limit is 0 (zero)
batch_read(KeyList, Db, []) ->
    batch_read(KeyList, Db, [{batch, 0}]);
batch_read(KeyList, Db, [{batch, Limit}]) ->
    batch_read_p(KeyList, Db, Limit, []);
batch_read(KeyList, Db, [_Param | ParamsTail]) ->
    batch_read(KeyList, Db, ParamsTail).
batch_read_p([], _Db, _Limit, Result) ->
    Result;
batch_read_p([_Key | _Tail], _Db, 0, _Result) ->
    {error, batch_limit};
batch_read_p([Key | Tail], Db, Limit, Result) ->
    case db:read(Key, Db) of
        {ok, Data} -> batch_read_p(Tail, Db, Limit - 1, [{Key, Data} | Result]);
        OtherResult -> OtherResult
    end.


%% @doc deletes data for each matching keys from passed `KeyList`
batch_delete(KeyList, {Params, Storage}) ->
    batch_delete(KeyList, {Params, Storage}, Params).
%default batch limit is 0 (zero)
batch_delete(KeyList, Db, []) ->
    batch_delete(KeyList, Db, [{batch, 0}]);
batch_delete(KeyList, Db, [{batch, Limit}]) ->
    batch_delete_p(KeyList, Db, Limit);
batch_delete(KeyList, Db, [_Param | ParamsTail]) ->
    batch_delete(KeyList, Db, ParamsTail).
batch_delete_p([], Db, _Limit) ->
    Db;
batch_delete_p([_Key | _Tail], _Db, 0) ->
    {error, batch_limit};
batch_delete_p([Key | Tail], Db, Limit) ->
    batch_delete_p(Tail, db:delete(Key, Db), Limit - 1).


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

