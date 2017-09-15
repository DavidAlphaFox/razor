-module(razor_html).

-export([format/1]).

format(Doc) ->
    [<<"<!doctype html>">>,
     format_element(Doc)].

format_element({Tag, Attrs}) ->
    [<<"<">>,
     format_tag(Tag),
     [format_attr(A) || A <- Attrs],
     <<"/>">>];
format_element({Tag, Attrs, Body}) ->
    T = format_tag(Tag),
    [<<"<">>,
     T,
     [format_attr(A) || A <- Attrs],
     <<">">>,
     [ case E of
           _ when is_tuple(E) ->
               format_element(E);
           _ ->
               encode_text(iolist_to_binary(E))
       end
       || E <- Body],
     <<"</">>, T, <<">">>].

format_tag(Tag) when is_atom(Tag) ->
    atom_to_binary(Tag, utf8).

format_attr(A) when is_atom(A) ->
    [<<" ">>, atom_to_binary(A, utf8)];
format_attr({T, V}) ->
    [<<" ">>,
     atom_to_binary(T, utf8),
     <<"=\"">>,
     encode_text(iolist_to_binary(V)),
     <<"\"">>].

encode_text(T) ->
    <<case C of
          $" ->
              <<"&quot;">>;
          $& ->
              <<"&amp;">>;
          $' ->
              <<"&apos;">>;
          $< ->
              <<"&lt;">>;
          $> ->
              <<"&gt;">>;
          _ ->
              <<C>>
      end
      || <<C>> <= T>>.
