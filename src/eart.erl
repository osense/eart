-module(eart).

-export([new/0, insert/3, lookup/2]).


-type tree() :: {key(), value(), tree(), tree()} | empty.

-type key() :: binary() | none.
-type value() :: term().

%% API
-spec new() -> tree().
new() ->
    {none, none, empty, empty}.


-spec insert(key(), value(), tree()) -> tree().
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

-spec ensure_nonempty(tree()) -> tree().
ensure_nonempty(empty) ->
    new();
ensure_nonempty(T) ->
    T.

-spec lookup(key(), tree()) -> value().
lookup(<<H:8, T/binary>>, Tree) ->
    lookup1(H, T, Tree).

lookup1(H, T, {<<NH:8, _/binary>>, _, _, NoFit}) when H /= NH ->
    lookup1(H, T, NoFit);
lookup1(H, T, {<<_:8, NT/binary>>, NodeVal, Fit, NoFit}) ->
    NTLen = byte_size(NT),
    case T of
        <<NT:NTLen/binary, RestKey/binary>> ->
            if
                RestKey == <<>> -> NodeVal;
                true -> lookup(RestKey, Fit)
            end;
        _ -> lookup1(H, T, NoFit)
    end;
lookup1(_, _, empty) ->
    none.
