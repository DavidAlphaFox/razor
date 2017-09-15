-module(razor_db_examples).

-compile({parse_transform, razor_db}).
-export([test/2]).


test(DB, product_list) ->
    razor_db:select(
      DB,
      [ Name
        || #{name := Name, price := Price}
               <- from(product_list),
           Price > 100
      ]);
test(DB, select_no_column) ->
    razor_db:select(
      DB,
      [ 1
        || #{} <- from(items)
      ]);
test(DB, select_one_column) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <- from(items)
      ]);
test(DB, {select_where, X}) ->
    razor_db:select(
      DB,
      [ Name
        || #{id := ID, name := Name} <- from(items),
           ID == X
      ]);
test(DB, {select_where_param, X}) ->
    razor_db:select(
      DB,
      [ Name
        || #{id := ID, name := Name} <- from(items),
           ID == param(X)
      ]);
test(DB, cross_join) ->
    razor_db:select(
      DB,
      [ I1
        || #{id := I1} <- from(i1),
           #{id := I2} <- from(i2),
           I1 == I2
      ]);
test(DB, left_outer_join) ->
    razor_db:select(
      DB,
      [ {I1, Name}
        || #{id := I1} <- from(i1),
           #{name := Name} <- join(i2, c(id) == I1)
      ]);
test(DB, group_by) ->
    razor_db:select(
      DB,
      [ {Name, Count}
        || #{id := ID, name := Name} <- from(items),
           group_by(Name),
           Count <- [count(ID)]
      ]);
test(DB, {group_by_having, X}) ->
    razor_db:select(
      DB,
      [ {Name, Count}
        || #{id := ID, name := Name} <- from(items),
           group_by(Name),
           Count <- [count(ID)],
           Count == X
      ]);
test(DB, order_by) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <- from(items),
           order_by(asc(ID))
      ]);
test(DB, distinct) ->
    razor_db:select(
      DB,
      [ Name
        || #{name := Name} <- from(items),
           distinct(Name)
      ]);
test(DB, offset_limit) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <- from(items),
           order_by(asc(ID)),
           offset(10),
           limit(10)
      ]);
test(DB, subquery) ->
    razor_db:select(
      DB,
      [ A
        || #{id := A} <- from(i1),
           exists(
             select(
               [ #{}
                 || #{id := B} <- from(i2),
                    A == B
               ]))
      ]);
test(DB, subquery_having) ->
    razor_db:select(
      DB,
      [ Name
        || #{id := ID, name := Name} <- from(i1),
           group_by(Name),
           Count <- [count(ID)],
           exists(
             select(
               [ #{count => C}
                 || #{count := C} <- from(i2),
                    C == Count
               ]))
      ]);
test(DB, select_subquery) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <-
               from(
                 select(
                   [ #{ id => ID}
                     || #{id := ID} <- from(items)
                   ]))
      ]);
test(DB, cte) ->
    razor_db:select(
      DB,
      #{i => [[#{id => ID} || #{id := ID} <- from(items)]]},
      [ ID
        || #{id := ID} <- from(i)
      ]);
test(DB, subquery_with_cte) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <-
               from(
                 select(
                   #{i => [[#{id => ID} || #{id := ID} <- from(items)]]},
                   [ #{id=> ID}
                     || #{id := ID} <- from(i)
                   ]
                  ))
      ]);
test(DB, unused_var) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID, name := Name} <- from(item)
      ]);
test(DB, unused_subquery_var) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <- from(i1),
           exists(
             select(
               [ #{}
                 || #{id := Unused} <- from(i2)
               ]))
      ]);
test(DB, unused_param) ->
    razor_db:select(
      DB,
      [ ID
        || #{id := ID} <- from(items),
           _ <- [param(1)]
      ]);
test(DB, group_by_map) ->
    razor_db:select(
      DB,
      [ #{name => Name, count => Count}
        || #{name := Name} <- from(item),
           group_by(Name),
           Count <- [count(1)],
           Count == 0
      ]);
test(DB, {vote, UserID}) ->
    razor_db:select(
      DB,
      [ #{post_id => PostID,
          content => Content,
          vote_id => VoteID,
          is_upvote => IsUpvote}
        || #{id := PostID, content := Content} <- from(post),
           #{id := VoteID, is_upvote := IsUpvote}
               <- join(vote, c(post_id) == PostID, c(user_id) == UserID)
      ]).
