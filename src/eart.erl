-module(eart).

-export([new/0, insert/3, lookup/2]).


%% API
new() ->
    {none, none, empty, empty}.


insert(Key, Value, {NodeKey, _, Fit, NoFit}) when NodeKey == none orelse Key == NodeKey ->
    {Key, Value, Fit, NoFit};
insert(Key = <<KHead:1/binary, _/binary>>, Value, {NodeKey = <<NHead:1/binary, _/binary>>, NodeVal, Fit, NoFit}) when KHead /= NHead ->
    {NodeKey, NodeVal, Fit, insert(Key, Value, ensure_nonempty(NoFit))};
insert(Key, Value, {NodeKey, NodeVal, Fit, NoFit}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<Pref:PrefLen/binary, RestPref/binary>> = NodeKey,
    <<Pref:PrefLen/binary, RestKey/binary>> = Key,
    if
        RestPref == <<>> ->
            {NodeKey, NodeVal, insert(RestKey, Value, ensure_nonempty(Fit)), NoFit};
        RestPref /= <<>> ->
            if
                RestKey /= <<>> ->
                    SplitFit = {RestPref, NodeVal, Fit, NoFit},
                    {Pref, none, insert(RestKey, Value, SplitFit), empty};
                RestKey == <<>> ->
                    {Key, Value, {RestPref, NodeVal, Fit, empty}, NoFit}
            end
    end.

ensure_nonempty(empty) ->
    new();
ensure_nonempty(T) ->
    T.

lookup(Key, {Key, Value, _, _}) ->
    Value;
lookup(Key, {NodeKey, _NodeVal, Fit, NoFit}) ->
    PrefLen = binary:longest_common_prefix([Key, NodeKey]),
    <<Pref:PrefLen/binary, RestKey/binary>> = Key,
    case Pref of
        NodeKey -> lookup(RestKey, Fit);
        <<>> -> lookup(Key, NoFit);
        _ -> none
    end;
lookup(_Key, empty) ->
    none.
