-module(transee_util).
-export([ qs_numbers/1
        , extract_transport_ids/1
        ]).


qs_numbers(Numbers) ->
    string:join(lists:map(fun(S) ->
        ["\""|[S|["\""]]]
    end, Numbers), ",").

extract_transport_ids(Numbers) ->
    lists:map(fun({ID, Number}) -> ID end, Numbers).
