-module(razor_group_map).

-export(
   [ from_list/1,
     compact/1
   ]).

from_list(List) ->
    from_list(List, razor_range_list).

from_list(List, Set) ->
    lists:foldl(
      fun (Entry, Map) ->
              add_entry(Entry, Map, Set)
      end,
      [],
      List).

add_entry(M, [], _) ->
    [M];
add_entry({S1, V1}=M, [H={S2, V2}|T], Set) ->
    Intersection = Set:intersection(S1, S2),
    case Set:is_empty(Intersection) of
        true ->
            [H|add_entry(M,T,Set)];
        false ->
            S1Rest = Set:subtract(S1, Intersection),
            S2Rest = Set:subtract(S2, Intersection),

            T1 =
                case Set:is_empty(S1Rest) of
                    true ->
                        T;
                    false ->
                        add_entry({S1Rest, V1}, T, Set)
                end,

            H1 =
                {Intersection, ordsets:union(V1, V2)},

            case Set:is_empty(S2Rest) of
                true ->
                    [H1|T1];
                false ->
                    [{S2Rest, V2}, H1|T1]
            end
    end.


compact(List) ->
    compact(List, razor_range_list).

compact(List, Set) ->
    [ { lists:foldl(fun Set:union/2, Set:new(), Sets), Value }
      || {Value, Sets}
             <- dict:to_list(
                  lists:foldl(
                    fun({S, V}, D) ->
                            dict:append(V, S, D)
                    end,
                    dict:new(),
                    List))
    ].
