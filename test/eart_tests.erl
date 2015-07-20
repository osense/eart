-module(eart_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


insert_test() ->
    T1 = eart:new(),
    T2 = eart:insert(<<"roman">>, 100, T1),
    ?assertEqual({<<"roman">>, 100, empty, empty}, T2),
    T3 = eart:insert(<<"romanus">>, val2, T2),
    ?assertEqual({<<"roman">>, 100,
        {<<"us">>, val2, empty, empty},
        empty},
    T3),
    T4 = eart:insert(<<"rome">>, val3, T3),
    ?assertEqual({<<"rom">>, none,
        {<<"an">>, 100,
            {<<"us">>, val2, empty, empty},
            {<<"e">>, val3, empty, empty}},
        empty},
    T4),
    T5 = eart:insert(<<"aaa">>, val4, T4),
    ?assertEqual({<<"rom">>, none,
        {<<"an">>, 100,
            {<<"us">>, val2, empty, empty},
            {<<"e">>, val3, empty, empty}},
        {<<"aaa">>, val4, empty, empty}},
    T5).

lookup_test() ->
    T1 = eart:new(),
    T2 = eart:insert(<<"roman">>, 100, T1),
    T3 = eart:insert(<<"romanus">>, val2, T2),
    T4 = eart:insert(<<"rome">>, val3, T3),
    ?assertEqual(100, eart:lookup(<<"roman">>, T4)),
    ?assertEqual(val2, eart:lookup(<<"romanus">>, T4)),
    ?assertEqual(val3, eart:lookup(<<"rome">>, T4)),
    ?assertEqual(none, eart:lookup(<<"noo">>, T4)).

combined_test() ->
    T1 = eart:new(),
    T2 = eart:insert(<<"aab">>, aab, T1),
    T3 = eart:insert(<<"aac">>, aac, T2),
    T4 = eart:insert(<<"aa">>, aa, T3),
    ?assertEqual(aa, eart:lookup(<<"aa">>, T4)),
    ?assertEqual(aab, eart:lookup(<<"aab">>, T4)),
    ?assertEqual(aac, eart:lookup(<<"aac">>, T4)).
