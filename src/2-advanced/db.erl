-module(db).
-export([new/0,
         destroy/1,
         write/3,
         read/2,
         delete/2,
         match/2]).

-include_lib("eunit/include/eunit.hrl").


new() -> {}.


destroy(_Db) -> ok.


write(Key, Data, _Db = {}) ->
    {Key, Data, {}, {}};
write(Key, Data, {RootKey, RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, RootData, write(Key, Data, Smaller), Larger};
write(Key, Data, {RootKey, RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, RootData, Smaller, write(Key, Data, Larger)};
write(Key, Data, {Key, _RootData, Smaller, Larger}) ->
    {Key, Data, Smaller, Larger}.
    


read(Key, {RootKey, _RootData, Smaller, _Larger}) when Key < RootKey ->
    read(Key, Smaller);
read(Key, {RootKey, _RootData, _Smaller, Larger}) when Key > RootKey ->
    read(Key, Larger);
read(Key, {Key, Data, _Smaller, _Larger}) ->
    {ok, Data};
read(_Key, _Db) -> {error, not_found}.


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


match(Data, Db) -> match_p(Data, [Db], []).
match_p(_Data, [], Result) ->
    Result;
match_p(Data, [{} | Tail], Result) ->
    match_p(Data, Tail, Result);
match_p(Data, [{Key, Data, Smaller, Larger} | Tail], Result) ->
    match_p(Data, [Smaller, Larger | Tail], [Key | Result]);
match_p(Data, [{_Key, _OtherData, Smaller, Larger} | Tail], Result) ->
    match_p(Data, [Smaller, Larger | Tail], Result).
    



rotate_right_test_() ->
    [
        ?_assertEqual({error, not_found}, rotate_right(key, {})),
        ?_assertEqual({6, "6"
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
                                            , {}}))
    ].
rotate_right(Key, {RootKey, _RootData, Smaller, Larger}) when Key < RootKey ->
    {RootKey, _RootData, rotate_right(Key, Smaller), Larger};
rotate_right(Key, {RootKey, _RootData, Smaller, Larger}) when Key > RootKey ->
    {RootKey, _RootData, Smaller, rotate_right(Key, Larger)};
rotate_right(Key, {Key, Data, {SKey, SData, SSmaller, SLarger}, Larger}) ->
    {SKey, SData, SSmaller, {Key, Data, SLarger, Larger}};
rotate_right(_Key, _Db) -> {error, not_found}.
    
