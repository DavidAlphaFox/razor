-module(razor_url).

-export([decode/1, encode/1]).

decode(<<>>) ->
    <<>>;
decode(<<X, Rest/binary>>)
  when $a =< X, X =< $z;
       $A =< X, X =< $Z;
       $0 =< X, X =< $9;
       $& =< X, X =< $/;
       X =:= $$; X =:= $!; X =:= $_ ; X =:= $~ ;
       X =:= $=; X =:= $:; X =:= $; ; X =:= $@ ->
    Rest1 = decode(Rest),
    <<X, Rest1/binary>>;
decode(<<$%, H, L, Rest/binary>>) ->
    Z = binary_to_integer(<<H,L>>, 16),
    Rest1 = decode(Rest),
    <<Z, Rest1/binary>>.

encode(<<>>) ->
    <<>>;
encode(<<X, Rest/binary>>)
  when $a =< X, X =< $z;
       $A =< X, X =< $Z;
       $0 =< X, X =< $9;
       $& =< X, X =< $/;
       X =:= $$; X =:= $!; X =:= $_ ; X =:= $~ ;
       X =:= $=; X =:= $:; X =:= $; ; X =:= $@ ->
    Rest1 = encode(Rest),
    <<X, Rest1/binary>>;
encode(<<X, Rest/binary>>) ->
    Rest1 = encode(Rest),
    H = to_hex(X bsr 4),
    L = to_hex(X band 15),
    <<$%, H, L, Rest1/binary>>.

to_hex(X) when 0 =< X, X =< 9 ->
    $0 + X;
to_hex(X) when 10 =< X, X =< 15 ->
    $A + X - 10.
