-module(razor_utils).

-export([traverse/4, dict_fetch/3]).


traverse(List, State, IsVisited, Visit) ->
    traverse_step(queue:from_list(List), State, IsVisited, Visit).


traverse_step(Queue, State, IsVisited, Visit) ->
    case queue:out(Queue) of
        {empty, _} ->
            State;
        {{value, Value}, Queue1} ->
            case IsVisited(Value, State) of
                true ->
                    traverse_step(Queue1, State, IsVisited, Visit);
                false ->
                    {Values, State1} = Visit(Value, State),
                    traverse_step(
                      queue:join(Queue1, queue:from_list(Values)),
                      State1, IsVisited, Visit)
            end
    end.


dict_fetch(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.
