-module(razor_query).

-export([decode/1, encode/1]).


decode(Query) ->
    [ case binary:split(Part, <<"=">>) of
          [X] ->
              decode_s(X);
          [X,Y] ->
              { decode_s(X),
                decode_s(Y) }
      end
      || Part <- binary:split(Query, <<"&">>, [global, trim_all])
    ].


decode_s(<<>>) ->
    <<>>;
decode_s(<<$+, Rest/binary>>) ->
    Rest1 = decode_s(Rest),
    <<$ , Rest1/binary>>;
decode_s(<<X, Rest/binary>>)
  when $a =< X, X =< $z;
       $A =< X, X =< $Z;
       $0 =< X, X =< $9;
       $' =< X, X =< $*;
       $, =< X, X =< $/;
       X =:= $$; X =:= $!; X =:= $_ ; X =:= $~ ;
       X =:= $:; X =:= $; ; X =:= $@ ->
    Rest1 = decode_s(Rest),
    <<X, Rest1/binary>>;
decode_s(<<$%, H, L, Rest/binary>>) ->
    Z = binary_to_integer(<<H,L>>, 16),
    Rest1 = decode_s(Rest),
    <<Z, Rest1/binary>>.


join([E]) ->
    [E];
join([H|T]) ->
    [H, <<"&">>|join(T)].


encode([]) ->
    [];
encode(Query) ->
    [<<"?">>,
     join(
       [ case A of
             {K, V} ->
                 [encode_s(atom_to_binary(K, utf8)), <<"=">>, encode_s(iolist_to_binary(V))];
             _ ->
                 encode_s(atom_to_binary(A, utf8))
         end
         || A <- Query ])].

encode_s(<<>>) ->
    <<>>;
encode_s(<<$ , Rest/binary>>) ->
    Rest1 = encode_s(Rest),
    <<$+, Rest1/binary>>;
encode_s(<<X, Rest/binary>>)
  when $a =< X, X =< $z;
       $A =< X, X =< $Z;
       $0 =< X, X =< $9;
       $& =< X, X =< $*;
       $, =< X, X =< $.;
       X =:= $$; X =:= $!; X =:= $_;
       X =:= $:; X =:= $;; X =:= $@ ->
    Rest1 = encode_s(Rest),
    <<X, Rest1/binary>>;
encode_s(<<X, Rest/binary>>) ->
    Rest1 = encode_s(Rest),
    H = to_hex(X bsr 4),
    L = to_hex(X band 15),
    <<$%, H, L, Rest1/binary>>.


to_hex(X) when 0 =< X, X =< 9 ->
    $0 + X;
to_hex(X) when 10 =< X, X =< 15 ->
    $A + X - 10.
