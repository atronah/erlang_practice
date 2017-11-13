-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

write_test_() ->
    [
        {"test reading data"
            , ?setup(fun read_data/1)},
        {"test deleting data"
            , ?setup(fun delete_data/1)},
        {"test matching data"
            , ?setup(fun match_data/1)}
    ].


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%


setup() -> 
    setup(db:new()
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