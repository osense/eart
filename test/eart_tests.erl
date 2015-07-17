-module(eart_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


insert_test() ->
    T1 = eart:new(),
    T2 = eart:insert(<<"roman">>, 100, T1),
    ?assertEqual({<<"roman">>, 100, []}, T2),
    T3 = eart:insert(<<"romanus">>, val2, T2),
    ?assertEqual({<<"roman">>, 100,
        [{<<"us">>, val2, []}]},
    T3),
    T4 = eart:insert(<<"rome">>, val3, T3),
    ?assertEqual({<<"rom">>, none, [
        {<<"e">>, val3, []},
        {<<"an">>, 100,
            [{<<"us">>, val2, []}]}]},
    T4).
