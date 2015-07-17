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

lookup_test() ->
    T1 = eart:new(),
    T2 = eart:insert(<<"roman">>, 100, T1),
    T3 = eart:insert(<<"romanus">>, val2, T2),
    T4 = eart:insert(<<"rome">>, val3, T3),
    ?assertEqual(100, eart:lookup(<<"roman">>, T4)),
    ?assertEqual(val2, eart:lookup(<<"romanus">>, T4)),
    ?assertEqual(val3, eart:lookup(<<"rome">>, T4)).

combined_test() ->
    T1 = eart:new(),
    T2 = eart:insert(<<"aab">>, aab, T1),
    T3 = eart:insert(<<"aac">>, aac, T2),
    T4 = eart:insert(<<"aa">>, aa, T3),
    ?assertEqual(aa, eart:lookup(<<"aa">>, T4)),
    ?assertEqual(aab, eart:lookup(<<"aab">>, T4)),
    ?assertEqual(aac, eart:lookup(<<"aac">>, T4)).
