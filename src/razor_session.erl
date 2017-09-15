-module(razor_session).

-export(
   [middleware/6,
    new_session/1,
    ensure_session/1
   ]).


middleware(Method, Endpoint, Args, {M,F,A}, Key, Backend) ->
    case proplists:lookup(Key, get(cookies)) of
        none ->
            SessionID1 = undefined,
            Session1 = undefined;
        {Key, SessionID1} ->
            put(session_id, SessionID1),
            case get_session(Backend, SessionID1) of
                {ok, Session1} ->
                    put(session, Session1);
                none ->
                    Session1 = undefined
            end
    end,

    Response = {http_response, Code, Headers, Body} =
        apply(M,F,[Method, Endpoint, Args|A]),

    SessionID2 = get(session_id),
    Session2 = get(session),

    if (SessionID1 =/= SessionID2) or (Session1 =/= Session2), SessionID2 =/= undefined ->
            case Session2 of
                undefined ->
                    delete_session(Backend, SessionID2);
                _ ->
                    save_session(Backend, SessionID2, Session2)
            end;
       true ->
            ok
    end,

    if SessionID2 =/= undefined, Session2 =:= undefined ->
            {http_response, Code,
             [<<"Set-Cookie: ">>, Key, <<"=">>, SessionID2, <<"; Path=/; Max-Age=1\r\n">>|Headers],
             Body};
       SessionID2 =/= undefined, SessionID2 =/= SessionID1 ->
            {http_response,
             Code,
             [<<"Set-Cookie: ">>, Key, <<"=">>, SessionID2, <<"; Path=/\r\n">>|Headers],
             Body};
       true ->
            Response
    end.


ensure_session(Backend) ->
    case get(session) of
        undefined ->
            ID = new_session(Backend),
            put(session_id, ID),
            put(session, #{}),
            #{};
        Session ->
            Session
    end.

get_session({M,A}, SessionID) ->
    apply(M, get_session, [SessionID|A]).

new_session({M,A}) ->
    <<X:160>> = crypto:strong_rand_bytes(20),
    ID = integer_to_binary(X, 36),
    case apply(M, new_session, [ID|A]) of
        true ->
            ID;
        false ->
            new_session({M,A})
    end.

save_session({M,A}, SessionID, Session) ->
    true = apply(M, save_session, [SessionID, Session|A]).


delete_session({M,A}, SessionID) ->
    true = apply(M, delete_session, [SessionID|A]).
