-module(razor_demo).

-compile({parse_transform, razor_url_dispatch}).
-compile({parse_transform, razor_db}).

-export([start/0]).

-export([url_dispatch/1, url_reverse/2]).

-export([middlewares/0, handle_request/3, not_found/0]).

-pattern(
   { integer,
     "[0-9]+",
     {erlang, binary_to_integer},
     {erlang, integer_to_binary}
   }).

-dispatch({root, "", {endpoint, home}}).
-dispatch({root, "accounts/", {dispatch, accounts}}).
-dispatch({root, "threads/", {dispatch, threads}}).
-dispatch({root, "posts/{id:integer}", {dispatch, post}}).

-dispatch({accounts, "register.php", {endpoint, register}}).
-dispatch({accounts, "login.php", {endpoint, login}}).
-dispatch({accounts, "logout.php", {endpoint, logout}}).

-dispatch({threads, "new.php", {endpoint, new_thread}}).
-dispatch({threads, "{id:integer}.html", {endpoint, thread}}).

-dispatch({post, ".html", {endpoint, post}}).
-dispatch({post, "/reply.php", {endpoint, reply}}).


start() ->
    start(8000).

start(Port) ->
    razor_ets_session:start(),
    razor_http_server:start(
      Port, [], ?MODULE,
      <<"x-powered-by: PHP/7.1.8\r\n">>).

session_backend() ->
    {razor_ets_session, []}.

middlewares() ->
    [{razor_cookie, middleware,
      []},
     {razor_session, middleware,
      [<<"PHPSESSID">>, session_backend()]}].

connect_db() ->
    epgsql:connect(
      "127.0.0.1",
      "razor", "",
      [{database, "razor"}]
     ).


handle_request('GET', home, Args) ->
    {ok, DB} = connect_db(),
    try
        {ok, _, _} = epgsql:squery(DB, "BEGIN"),
        Threads =
            razor_db:select(
              DB,
              [ #{id => ThreadID,
                  title => Title,
                  time => Time,
                  username => Name,
                  comments => Count - 1}
                || #{object_id := ThreadID,
                     post_count := Count,
                     title := Title}
                       <- from(razor_thread),
                   #{user_id := UserID,
                     post_time := Time}
                       <- join(razor_post,
                               c(post_subject_id) == ThreadID,
                               c(parent_id) =:= null),
                   #{name := Name}
                       <- join(razor_user,
                               c(user_id) == UserID),
                   order_by(desc(Time))
              ]
             ),

        html_response(
          200,
          page(
            <<"Home">>,
            [navbar(url_reverse(home, Args)),
             {main, [],
              [ {article, [],
                 [{'div', [], [{a, [{href, url_reverse(thread, #{id => ID})}], [Title]}]},
                  {'div', [{class, <<"meta">>}],
                   [{span, [], [io_lib:format("by ~s", [Name])]},
                    {span, [], [format_timestamp(Time)]},
                    {span, [], [io_lib:format("~w comments", [Comments])]}
                   ]
                  }
                 ]
                }
                || #{ id := ID,
                      title := Title,
                      time := Time,
                      username := Name,
                      comments := Comments} <- Threads
              ]
             }
            ]
           )
         )
    after
        epgsql:close(DB)
    end;

handle_request('GET', thread, Args = #{id := ThreadID}) ->
    {ok, DB} = connect_db(),
    try
        {ok, _, _} = epgsql:squery(DB, "BEGIN"),
        [Title] =
            razor_db:select(
              DB,
              [ Title
                || #{title := Title,
                     object_id := ObjectID}
                       <- from(razor_thread),
                   ObjectID == ThreadID ]),

        Posts =
            razor_db:select(
              DB,
              #{ post =>
                     [
                      [ #{ id => ID,
                           path => [ID],
                           depth => 0 }
                        || #{object_id := ID,
                             post_subject_id := PostSubjectID,
                             parent_id := ParentID }
                               <- from(razor_post),
                           PostSubjectID == ThreadID,
                           ParentID =:= null
                      ],
                      [ #{ id => ID,
                           path => Path ++ [ID],
                           depth => Depth + 1 }
                        || #{ object_id := ID,
                              parent_id := ParentID}
                               <- from(razor_post),
                           #{ id := PostID, path := Path, depth := Depth}
                               <- from(post),
                           PostID == ParentID
                      ]
                     ]
               },
              [ #{ id => ID,
                   time => Time,
                   depth => Depth,
                   username => Name,
                   body => Body}
                || #{id := ID,
                     path := Path,
                     depth := Depth} <- from(post),
                   #{user_id := UserID,
                     post_time := Time,
                     revision_id := RevID}
                       <- join(razor_post,
                               c(object_id) == ID),
                   #{revision_body := Body}
                       <- join(razor_revision,
                               c(revision_id) == RevID),
                   #{name := Name}
                       <- join(razor_user,
                               c(user_id) == UserID),

                   order_by(asc(Path))
              ]
             ),

        html_response(
          200,
          page(
            Title,
            [navbar(url_reverse(thread, Args)),
             {main, [],
              [{h1, [], [{a, [{href, url_reverse(thread, #{id => ThreadID})}], [Title]}]}
               | [ format_post(P) || P <- Posts ]
              ]
             }
            ]
           )
         )
    after
        epgsql:close(DB)
    end;

handle_request('GET', new_thread, Args) ->
    case get(session) of
        #{user_id := _} ->
            html_response(
              200,
              page(<<"New thread">>,
                   [navbar(url_reverse(new_thread, Args)),
                    {main, [], [new_thread_form()]}
                   ]
                  )
             );
        _ ->
            redirect_to([url_reverse(login, #{}), razor_query:encode([{next, url_reverse(new_thread, Args)}])])
    end;

handle_request('POST', new_thread, Args) ->
    case get(session) of
        #{user_id := UserID} ->
            case razor_http_server:read_form() of
                {error, length_required} ->
                    html_response(411, error_html(<<"411 - Length Required">>));
                {ok, Form} ->
                    Title = proplists:get_value(<<"title">>, Form, <<>>),
                    Body = proplists:get_value(<<"body">>, Form, <<>>),
                    {ok, DB} = connect_db(),
                    try
                        {ok, _, _} = epgsql:squery(DB, "BEGIN"),

                        {ok, _, _, [{ThreadID}]} =
                            epgsql:equery(
                              DB,
                              "INSERT INTO razor_thread(title) VALUES ($1) RETURNING object_id",
                              [Title]),

                        add_post(DB, ThreadID, UserID, null, Body),

                        {ok, _, _} = epgsql:squery(DB, "COMMIT"),
                        redirect_to(url_reverse(thread, #{id => ThreadID}))
                    after
                        epgsql:close(DB)
                    end
            end;
        _ ->
            redirect_to([url_reverse(login, #{}), razor_query:encode([{next, url_reverse(new_thread, Args)}])])
    end;

handle_request('GET', post, Args = #{id := PostID}) ->
    {ok, DB} = connect_db(),
    try
        {ok, _, _} = epgsql:squery(DB, "BEGIN"),

        [Post|Posts] =
            razor_db:select(
              DB,
              #{ post =>
                     [
                      [ #{ id => ID,
                           path => [ID],
                           depth => 0 }
                        || #{object_id := ID }
                               <- from(razor_post),
                           ID == PostID
                      ],
                      [ #{ id => ID,
                           path => Path ++ [ID],
                           depth => Depth + 1 }
                        || #{ object_id := ID,
                              parent_id := ParentID}
                               <- from(razor_post),
                           #{ id := PID, path := Path, depth := Depth}
                               <- from(post),
                           PID == ParentID
                      ]
                     ]
               },
              [ #{ id => ID,
                   time => Time,
                   depth => Depth,
                   username => Name,
                   body => Body}
                || #{id := ID,
                     path := Path,
                     depth := Depth} <- from(post),
                   #{user_id := UserID,
                     post_time := Time,
                     revision_id := RevID}
                       <- join(razor_post,
                               c(object_id) == ID),
                   #{revision_body := Body}
                       <- join(razor_revision,
                               c(revision_id) == RevID),
                   #{name := Name}
                       <- join(razor_user,
                               c(user_id) == UserID),

                   order_by(asc(Path))
              ]
             ),

        [{Title, ThreadID, ParentID}] =
            razor_db:select(
              DB,
              [ {Title, ThreadID, ParentID}
                || #{object_id := ObjectID,
                     parent_id := ParentID,
                     post_subject_id := PostSubjectID}
                       <- from(razor_post),
                   ObjectID == PostID,
                   #{title := Title,
                     object_id := ThreadID}
                       <- from(razor_thread),
                   PostSubjectID == ThreadID ]),

        html_response(
          200,
          page(
            Title,
            [navbar(url_reverse(post, Args)),
             {main, [],
              [{h1, [], [{a, [{href, url_reverse(thread, #{id => ThreadID})}], [Title]}]}
               | [ format_post(P) || P <- [Post#{parent_id=>ParentID}|Posts] ]
              ]
             }
            ]
           )
         )
    after
        epgsql:close(DB)
    end;

handle_request('GET', reply, Args = #{id := ID}) ->
    case get(session) of
        #{user_id := _} ->
            html_response(
              200,
              page(io_lib:format("Reply to ~w",[ID]),
                   [navbar(url_reverse(reply, Args)),
                    {main, [], [reply_form()]}
                   ]
                  )
             );
        _ ->
            redirect_to([url_reverse(login, #{}), razor_query:encode([{next, url_reverse(reply, Args)}])])
    end;

handle_request('POST', reply, Args = #{id := ID}) ->
    case get(session) of
        #{user_id := UserID} ->
            case razor_http_server:read_form() of
                {error, length_required} ->
                    html_response(411, error_html(<<"411 - Length Required">>));
                {ok, Form} ->
                    Body = proplists:get_value(<<"body">>, Form, <<>>),
                    {ok, DB} = connect_db(),
                    try
                        {ok, _, _} = epgsql:squery(DB, "BEGIN"),

                        [ThreadID] =
                            razor_db:select(
                              DB,
                              [ ThreadID
                                || #{object_id := ObjectID,
                                     post_subject_id := ThreadID}
                                       <- from(razor_post),
                                   ID == ObjectID
                              ]),

                        PostID = add_post(DB, ThreadID, UserID, ID, Body),

                        {ok, _, _} = epgsql:squery(DB, "COMMIT"),
                        redirect_to(url_reverse(post, #{id => PostID}))
                    after
                        epgsql:close(DB)
                    end
            end;
        _ ->
            redirect_to([url_reverse(login, #{}), razor_query:encode([{next, url_reverse(reply, Args)}])])
    end;


handle_request('GET', register, _Args) ->
    Next = proplists:get_value(<<"next">>, get(query), url_reverse(home, #{})),
    case get(session) of
        #{user_id := _} ->
            redirect_to(Next);
        _ ->
            html_response(
              200,
              page(<<"Register">>,
                   [register_form(<<>>, Next)]
                  )
             )
    end;
handle_request('POST', register, _Args) ->
    case razor_http_server:read_form() of
        {error, length_required} ->
            html_response(411, error_html(<<"411 - Length Required">>));
        {ok, Form} ->
            Next = proplists:get_value(<<"next">>, Form, url_reverse(home, #{})),
            Username = proplists:get_value(<<"username">>, Form, <<>>),
            {ok, DB} = connect_db(),
            try
                {ok, _, _} = epgsql:squery(DB, "BEGIN"),
                {ok, _, _, Rows} =
                    epgsql:equery(
                      DB,
                      "INSERT INTO razor_user(name) VALUES ($1) ON CONFLICT DO NOTHING RETURNING user_id",
                      [Username]),
                {ok, _, _} = epgsql:squery(DB, "COMMIT"),
                case Rows of
                    [] ->
                        html_response(
                          200,
                          page(<<"Register">>,
                               [register_form(Username, Next)]
                              )
                         );
                    [{ID}] ->
                        Session = razor_session:ensure_session(session_backend()),
                        put(session, Session#{user_id => ID}),
                        redirect_to(Next)
                end
            after
                epgsql:close(DB)
            end
    end;
handle_request('GET', login, _Args) ->
    Next = proplists:get_value(<<"next">>, get(query), url_reverse(home, #{})),
    case get(session) of
        #{user_id := _} ->
            redirect_to(Next);
        _ ->
            html_response(
              200,
              page(<<"Log In">>,
                   [login_form(<<>>, Next)]
                  )
             )
    end;
handle_request('POST', login, _Args) ->
    case razor_http_server:read_form() of
        {error, length_required} ->
            html_response(411, error_html(<<"411 - Length Required">>));
        {ok, Form} ->
            Next = proplists:get_value(<<"next">>, Form, url_reverse(home, #{})),
            Username = proplists:get_value(<<"username">>, Form, <<>>),
            {ok, DB} = connect_db(),
            try
                case razor_db:select(
                       DB,
                       [ UID
                         || #{user_id := UID, name := Name} <- from(razor_user),
                            Name == Username])
                of
                    [] ->
                        html_response(
                          200,
                          page(<<"Log In">>,
                               [login_form(Username, Next)]
                              )
                         );
                    [ID] ->
                        Session = razor_session:ensure_session(session_backend()),
                        put(session, Session#{user_id => ID}),
                        redirect_to(Next)
                end
            after
                epgsql:close(DB)
            end
    end;
handle_request('GET', logout, _Args) ->
    Next = proplists:get_value(<<"next">>, get(query), url_reverse(home, #{})),
    case get(session) of
        #{user_id := _} ->
            html_response(
              200,
              page(<<"Log Out">>,
                   [logout_form(Next)]
                  )
             );
        _ ->
            redirect_to(Next)
    end;
handle_request('POST', logout, _Args) ->
    case razor_http_server:read_form() of
        {error, length_required} ->
            html_response(411, error_html(<<"411 - Length Required">>));
        {ok, Form} ->
            erase(session),
            redirect_to(proplists:get_value(<<"next">>, Form, url_reverse(home, #{})))
    end;
handle_request(_Method, _Endpoint, _Args) ->
    html_response(405, error_html(<<"405 - Method Not Allowed">>)).


html_response(Code, Body) ->
    html_response(Code, [], Body).

html_response(Code, Headers, Body) ->
    { http_response,
      Code,
      [<<"Content-Type: text/html; charset=utf-8\r\n">>|Headers],
      Body
    }.


redirect_to(Next) ->
    html_response(
      302,
      [<<"Location: ">>, Next, <<"\r\n">>],
      []
     ).


not_found() ->
    html_response(404, error_html(<<"404 - Not Found">>)).


error_html(Bin) ->
    razor_html:format(
      {html, [],
       [{head, [], [{title, [], [Bin]}]},
        {body, [], [Bin]}
       ]
      }
     ).

format_timestamp({{Year,Month,Day}, {Hour,Minute,Second}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year,Month,Day,Hour,Minute,trunc(Second)]).


page(Title, Body) ->
    razor_html:format(
      {html, [],
       [{head, [],
         [{title, [], [Title]},
          {style, [{type, "text/css"}],
           ["html {margin: 0.5em; padding: 0;}"
            "body {min-width: 40em; width: 85%; margin: 0 auto; padding: 0}"
            "a {color: #000; text-decoration:none;}"
            "a:active {outline:none;}"
            "nav {background: #DFA; display: flex; flex-direction: row;}"
            "nav a {flex: 0 0 auto; margin: 0 0.25em;}"
            "nav a:first-child {flex: 1 0 auto;}"
            "main {background: #FFE;}"
            "main h1 {margin: 0; padding: 0;}"
            "article {padding: 0.5em;}"
            "article span {padding-right: 0.5em;}"
            "div.meta {color: #888; font-size: 80%;}"
           ]
          }
         ]
        },
        {body, [], Body}
       ]
      }
     ).


register_form(Username, Next) ->
    {form,
     [{method, <<"POST">>}, {action, <<"">>}],
     [{input, [{name, <<"username">>}, {value, Username}]},
      {input, [{type, <<"hidden">>}, {name, <<"next">>}, {value, Next}]},
      {button, [{type, <<"submit">>}], [<<"Register">>]}]
    }.

login_form(Username, Next) ->
    {form,
     [{method, <<"POST">>}, {action, <<"">>}],
     [{input, [{name, <<"username">>}, {value, Username}]},
      {input, [{type, <<"hidden">>}, {name, <<"next">>}, {value, Next}]},
      {button, [{type, <<"submit">>}], [<<"Log In">>]}]
    }.

logout_form(Next) ->
    {form,
     [{method, <<"POST">>}, {action, <<"">>}],
     [{input, [{type, <<"hidden">>}, {name, <<"next">>}, {value, Next}]},
      {button, [{type, <<"submit">>}], [<<"Log Out">>]}]
    }.

new_thread_form() ->
    {form,
     [{method, <<"POST">>}, {action, <<"">>}],
     [{'div', [],
       [{label, [{for, <<"title">>}], [<<"Title: ">>]}, {input, [{type, <<"text">>}, {name, <<"title">>}]}]
      },
      {'div', [],
       [{label, [{for, <<"body">>}], [<<"Body: ">>]}, {textarea, [{name, <<"body">>}], []}]
      },
      {button, [{type, <<"submit">>}], [<<"Submit new thread">>]}]
    }.

reply_form() ->
    {form,
     [{method, <<"POST">>}, {action, <<"">>}],
     [{'div', [],
       [{label, [{for, <<"body">>}], [<<"Body: ">>]}, {textarea, [{name, <<"body">>}], []}]
      },
      {button, [{type, <<"submit">>}], [<<"Submit reply">>]}]
    }.


navbar(Next) ->
    case get(session) of
        #{user_id := _} ->
            {nav, [],
             [{a, [{href, url_reverse(home, #{})}], [<<"Home">>]},
              {a, [{href, url_reverse(new_thread, #{})}], [<<"Submit">>]},
              {a, [{href, [url_reverse(logout, #{}), razor_query:encode([{next, Next}])]}], [<<"Log out">>]}
             ]
            };
        _ ->
            {nav, [],
             [{a, [{href, url_reverse(home, #{})}], [<<"Home">>]},
              {a, [{href, [url_reverse(register, #{}), razor_query:encode([{next, Next}])]}], [<<"Register">>]},
              {a, [{href, [url_reverse(login, #{}), razor_query:encode([{next, Next}])]}], [<<"Log in">>]}
             ]
            }
    end.

format_post(
  #{ id := ID,
     time := Time,
     depth := Depth,
     username := Name,
     body := Body
   } = Post) ->
    {article, [{style, io_lib:format("padding-left: ~wem;", [1+Depth*2])}],
     [{'div', [{class,<<"meta">>}],
       [{span, [], [Name]},
        {span, [], [format_timestamp(Time)]},
        {span, [], [{a, [{href, url_reverse(post, #{id => ID})}], [<<"Link">>]}]}
        | case Post of
              #{parent_id := ParentID} when is_integer(ParentID) ->
                  [{span, [], [{a, [{href, url_reverse(post, #{id => ParentID})}], [<<"Parent">>]}]}];
              _ ->
                  []
          end
       ]
      },
      {'div', [],
       [{span, [], [Body]}
       ]
      },
      {'div', [{class,<<"meta">>}],
       [{a, [{href, url_reverse(reply, #{id => ID})}], [<<"Reply">>]}]
      }
     ]
    }.



add_post(DB, ObjectID, UserID, ParentID, Body) ->
    Now = erlang:timestamp(),
    {ok, _, _} =
        epgsql:equery(
          DB,
          "SELECT post_count FROM razor_post_subject WHERE object_id=$1 FOR UPDATE",
          [ObjectID]),

    {ok, _} =
        epgsql:equery(
          DB,
          "UPDATE razor_post_subject SET post_count=post_count+1 WHERE object_id=$1",
          [ObjectID]),

    {ok, _, _, [{RevID}]} =
        epgsql:equery(
          DB,
          "INSERT INTO razor_revision(user_id,revision_body,revision_time) VALUES ($1,$2,$3) RETURNING revision_id",
          [UserID, Body, Now]),

    {ok, _, _, [{PostID}]} =
        epgsql:equery(
          DB,
          "INSERT INTO razor_post(post_subject_id,parent_id,revision_id,user_id,post_time) VALUES ($1,$2,$3,$4,$5) RETURNING object_id",
          [ObjectID, ParentID, RevID, UserID, Now]),

    PostID.
