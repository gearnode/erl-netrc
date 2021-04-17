-module(netrc_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  Parse = fun netrc_parser:parse/1,
  [?_assertEqual({ok, []},
                 Parse(<<"">>)),
   ?_assertEqual({ok, [#{machine => default}]},
                 Parse(<<"default">>)),
   ?_assertEqual({ok, [#{machine => <<"example.com">>}]},
                 Parse(<<"machine example.com">>)),
   ?_assertEqual({ok, [#{machine => <<"example.com">>,
                         login => <<"foo">>,
                         password => <<"bar">>}]},
                 Parse(<<"machine\texample.com "
                         "login  foo\n\t"
                         "password \t bar\n  ">>)),
   ?_assertEqual({ok, [#{machine => default,
                         port => 42}]},
                 Parse(<<"default\t port \n42\n">>)),
   ?_assertEqual({ok, [#{machine => default,
                         port => <<"https">>}]},
                 Parse(<<"default port https">>)),
   ?_assertEqual({ok, [#{machine => <<"a.example.com">>,
                         login => <<"foo">>},
                       #{machine => default},
                       #{machine => <<"b.example.com">>,
                         password => <<"bar">>}]},
                 Parse(<<"machine a.example.com login foo"
                         " default",
                         " machine b.example.com password bar">>)),
   ?_assertEqual({error, {unknown_token, <<"foo">>}},
                 Parse(<<"default foo bar">>)),
   ?_assertEqual({error, {unexpected_token, login}},
                 Parse(<<"login foo">>)),
   ?_assertEqual({error, {truncated_token, <<"login">>}},
                 Parse(<<"default login">>)),
   ?_assertMatch({error, {invalid_token, <<"port">>, <<"123f">>, _}},
                 Parse(<<"default port 123f">>))].
