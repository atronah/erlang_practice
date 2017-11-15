-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

all_test_() ->
    [
        {"test reading data"
            , ?setup(fun read_data/1)},
        {"test deleting data"
            , ?setup(fun delete_data/1)},
        {"test matching data"
            , ?setup(fun match_data/1)},
        {"test appending access"
            , ?setup(fun append_access/1)},
        {"test batch read"
            , ?setup(fun batch_read/1)},
        {"test batch delete"
            , ?setup(fun batch_delete/1)}
    ].


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%


setup() ->
    setup(db:new([{append, allow}, {batch, 3}])
        , [ {list, [1, 2, 3]},
            {text, "text"},
            {int, 123},
            {1, "odd"},
            {2, "even"},
            {3, "odd"},
            {4, "even"},
            {5, "odd"},
            {6, "even"},
            {7, "odd"},
            {8, "even"}
            ]).
setup(Db, []) ->
    Db;
setup(Db, [{Key, Data} | Tail]) ->
    setup(db:write(Key, Data, Db), Tail).


cleanup(Db) ->
    db:destroy(Db).


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

read_data(Db) ->
    [
        ?_assertEqual({ok, "text"}, db:read(text, Db)),
        ?_assertEqual({ok, [1, 2, 3]}, db:read(list, Db)),
        ?_assertEqual({ok, 123}, db:read(int, Db))
    ].

delete_data(Db) ->
    [
        ?_assertEqual({ok, "text"}, db:read(text, Db)),
        ?_assertEqual({error, not_found}
                        , db:read(text, db:delete(text, Db))),
        ?_assertEqual({ok, [1, 2, 3]}
                        , db:read(list, db:delete(text, Db))),
        ?_assertEqual({ok, 123}
                        , db:read(int, db:delete(text, Db)))
    ].

match_data(Db) ->
    [
        ?_assertEqual([2, 4, 6, 8], lists:sort(db:match("even", Db)))
    ].

append_access(Db) ->
    [
        ?_assertEqual({error, access_denied}
                        , db:append(key, value, db:new([{append, deny}]))),
        ?_assertEqual({error, access_denied}
                        , db:append(key, value, db:new([{other, other}, {append, deny}, {other, other}]))),
        ?_assertNotEqual({error, access_denied}
                            , db:append(key, value, db:new([{append, allow}]))),
        ?_assertNotEqual({error, access_denied}
                        , db:append(key, value, db:new([{other, other}, {other, other}]))),
        ?_assertNotEqual({error, access_denied}
                        , db:append(key, value, Db))
    ].

batch_read(Db) ->
    [
        ?_assertEqual(lists:sort([{1, "odd"}, {4, "even"}, {text, "text"}])
                        , lists:sort(db:batch_read([text, 1, 4], Db))),
        ?_assertEqual({error,  batch_limit}
                        , db:batch_read([1, 2, 3, 4], Db)),
        ?_assertEqual({error,  not_found}
                        , db:batch_read([11, 2, 3], Db))
    ].

batch_delete(Db) ->
    [
        ?_assertEqual({error, not_found}
                        , db:read(1, db:batch_delete([text, 1, 4], Db))),
        ?_assertEqual({error,  batch_limit}
                        , db:batch_read([1, 2, 3, 4], Db))
    ].