-module(razor_url_regex).

-export([encode/1]).

-export([test/0]).

encode(RE) ->
    razor_regex:simplify([ [encode_elem(E) || E <- Seq] || Seq <- RE]).

encode_elem({repeat, X, Y, E}) ->
    {repeat, X, Y, encode_elem(E)};
encode_elem({group, E}) ->
    {group, encode(E)};
encode_elem({set, Set}) ->
    {group, encode_ranges(Set)};
encode_elem($/) ->
    $/;
encode_elem(C)
  when $a =< C, C =< $z;
       $A =< C, C =< $Z;
       $0 =< C, C =< $9;
       $& =< C, C < $/;
       C =:= $$; C =:= $!; C =:= $_ ; C =:= $~ ;
       C =:= $=; C =:= $:; C =:= $; ; C =:= $@ ->
    {group, [[C]|encode_char(C)]};
encode_elem(C)
  when is_integer(C) ->
    {group, encode_char(C)}.

encode_char(C) ->
    [[$%,H,L]
     || H <- hex_chars(C bsr 4),
        L <- hex_chars(C band 15)].

hex_chars(X) when 0 =< X, X =< 9 ->
    [$0 + X];
hex_chars(X) when 10 =< X, X =< 15 ->
    [$A + X - 10, $a + X - 10].


encode_ranges(Set) ->
    lists:append(
      [[[C] || C <- safe_chars(), X =< C, C =< Y] ++
           if X =< $/, $/ =< Y ->
                   if X < $/ ->
                           encode_range(X, $/-1);
                      true ->
                           []
                   end ++
                       if $/ < Y ->
                               encode_range($/+1, Y);
                          true ->
                               []
                       end;
             true ->
                   encode_range(X,Y)
           end
       || {X,Y} <- Set]).


safe_chars() ->
    lists:seq($a, $z)
        ++ lists:seq($A, $Z)
        ++ lists:seq($0, $9)
        ++ lists:seq($&, $/)
        ++ [$$, $!, $_, $~, $=, $:, $;, $@].


encode_range(X, Y) ->
    L = ((X - 1) bsr 4) + 1,
    H = (Y + 1) bsr 4,

    if L =< H ->
            if L < H ->
                    [[$%,
                      half_byte_group(L, H-1),
                      half_byte_group()]];
               true ->
                    []
            end ++
                if X < (L bsl 4) ->
                        [[$%,
                          half_byte_group(X bsr 4),
                          half_byte_group(X band 15, 15)]];
                   true ->
                        []
                end ++
                if Y >= (H bsl 4) ->
                        [[$%,
                          half_byte_group(Y bsr 4),
                          half_byte_group(0, Y band 15)]];
                   true ->
                        []
                end;
       true ->
            if X =< Y ->
                    [[$%,
                      half_byte_group(X bsr 4),
                      half_byte_group(X band 15, Y band 15)]];
               true ->
                    []
            end
    end.

half_byte_group(X, Y) ->
    { group,
      [ [C]
        || I <- lists:seq(X, Y),
           C <- hex_chars(I)
      ]}.

half_byte_group(X) ->
    half_byte_group(X, X).

half_byte_group() ->
    half_byte_group(0, 15).

test_encode_range(X, Y, Groups) ->
    Expected =
        [[$%|[half_byte_group(L,H) || {L,H} <- G]]
         || G <- Groups],

    Expected = encode_range(X, Y),
    ok.

test(half_byte_group) ->
    {group, []} = half_byte_group(1, 0),
    {group, [[$1]]} = half_byte_group(1, 1),
    {group, [[$1],[$2],[$3]]} = half_byte_group(1, 3),
    {group, [[$A],[$a],[$B],[$b]]} = half_byte_group(16#A, 16#B),
    ok;
test(encode_range) ->
    [] = encode_range(1,0),
    ok = test_encode_range(0, 0, [[{0,0},{0,0}]]),

    %% | X Y |
    ok = test_encode_range(16#11, 16#1E, [[{1,1},{1,16#E}]]),

    %% X | Y
    ok = test_encode_range(
           16#11, 16#2E,
           [[{1,1},{1,16#F}],
            [{2,2},{0,16#E}]]),

    %% X | | Y
    ok = test_encode_range(
           16#11, 16#4E,
           [[{2,3},{0,16#F}],
            [{1,1},{1,16#F}],
            [{4,4},{0,16#E}]]),

    %% |X  Y| 
    ok = test_encode_range(
           16#10, 16#4F,
           [[{1,4},{0,16#F}]]),

    %% |X  Y
    ok = test_encode_range(
           16#10, 16#1E,
           [[{1,1},{0,16#E}]]),

    %% |X  | Y
    ok = test_encode_range(
           16#10, 16#2E,
           [[{1,1},{0,16#F}],
            [{2,2},{0,16#E}]]),

    %% X  Y|
    ok = test_encode_range(
           16#11, 16#1F,
           [[{1,1},{1,16#F}]]),

    %% X | Y|
    ok = test_encode_range(
           16#11, 16#2F,
           [[{2,2},{0,16#F}],
            [{1,1},{1,16#F}]]),

    ok;
test(encode) ->
    [[$/]] = encode([[$/]]),
    [[$a], [$%, $6, $1]] = encode([[$a]]),
    [[$%, $8, $0]] = encode([[16#80]]),

    [[$/]] = encode([[{set, [{$/, $/}]}]]),
    [[$a], [$%, $6, $1]] = encode([[{set, [{$a, $a}]}]]),
    [[$%, $8, $0]] = encode([[{set, [{16#80, 16#80}]}]]),
    ok.

test() ->
    test(half_byte_group),
    test(encode_range),
    test(encode),
    ok.
