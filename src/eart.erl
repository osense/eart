-module(eart).

-export([new/0, insert/3, lookup/2]).
-compile([export_all]).


%% API
new() ->
    {none, none, []}.


insert(Key, Value, {NodeKey, _, Children}) when NodeKey == none orelse Key == NodeKey ->
    {Key, Value, Children};
insert(<<KHead:1/binary, _/binary>>, _Value, {NodeKey = <<NHead:1/binary, _/binary>>, NodeVal, Children}) when KHead /= NHead ->
    {NodeKey, NodeVal, Children};
insert(Key, Value, {NodeKey, NodeVal, Children}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<Pref:PrefLen/binary, RestPref/binary>> = NodeKey,
    <<Pref:PrefLen/binary, RestKey/binary>> = Key,
    if
        RestPref == <<>> ->
            {NodeKey, NodeVal, insert_into_children(RestKey, Value, Children)};
        (RestPref /= <<>>) and (RestKey /= <<>>) ->
            {Pref, none, [insert(RestKey, Value, new()), {RestPref, NodeVal, Children}]};
        (RestPref /= <<>>) and (RestKey == <<>>) ->
            {Pref, Value, [{RestPref, NodeVal, Children}]}
    end.

insert_into_children(Key, Value, Children) ->
    insert_into_children(Key, Value, Children, []).

insert_into_children(Key, Value, [], AccOut) ->
    [insert(Key, Value, new()) | AccOut];
insert_into_children(Key, Value, [Child = {ChildKey, _, _} | Rest], AccOut) ->
    PrefLen = binary:longest_common_prefix([Key, ChildKey]),
    <<Pref:PrefLen/binary, _Rest/binary>> = ChildKey,
    if
        Pref == <<>> ->
            insert_into_children(Key, Value, Rest, [Child | AccOut]);
        Pref /= <<>> ->
            Rest ++ [insert(Key, Value, Child) | AccOut]
    end.


lookup(Key, {Key, Value, _}) ->
    Value;
lookup(<<KHead:1/binary, _/binary>>, {<<NHead:1/binary, _/binary>>, _NodeVal, _Children}) when KHead /= NHead ->
    none;
lookup(Key, {NodeKey, _NodeVal, Children}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<_Pref:PrefLen/binary, RestPref/binary>> = NodeKey,
    if
        RestPref == <<>> ->
            <<_Pref:PrefLen/binary, RestKey/binary>> = Key,
            lookup_in_children(RestKey, Children);
        RestPref /= <<>> ->
            none
    end.

lookup_in_children(_Key, []) ->
    none;
lookup_in_children(Key, [Child | Rest]) ->
    case lookup(Key, Child) of
        none ->
            lookup_in_children(Key, Rest);
        Val ->
            Val
    end.
