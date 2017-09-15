-module(razor_ets_session).

-behaviour(gen_server).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export(
   [ new_session/1, new_session/2,
     get_session/1, get_session/2,
     save_session/2, save_session/3,
     delete_session/1, delete_session/2 ]).


new_session(ID) ->
    new_session(ID, ?MODULE).

new_session(ID, Name) ->
    ets:insert_new(Name, {ID, #{}}).


get_session(ID) ->
    get_session(ID, ?MODULE).

get_session(ID, Name) ->
    case ets:lookup(Name, ID) of
        [] ->
            none;
        [{ID, Session}] ->
            {ok, Session}
    end.


save_session(ID, Session) ->
    save_session(ID, Session, ?MODULE).

save_session(ID, Session, Name) ->
    ets:update_element(Name, ID, {2, Session}).


delete_session(ID) ->
    delete_session(ID, ?MODULE).

delete_session(ID, Name) ->
    ets:delete(Name, ID).


start() ->
    start(?MODULE).

start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
    Name =
        ets:new(
          Name,
          [ set,
            public,
            named_table,
            {read_concurrency, true},
            {write_concurrency, true}
          ]),

    {ok, Name}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
