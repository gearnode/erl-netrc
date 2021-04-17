-module(netrc).

-export([path/0, parse/0, parse/1, parse_data/1,
         format_error_reason/1]).

-export_type([result/0, result/1, error_reason/0, entry/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() ::
        {read_file, term(), file:name_all()}
      | {syntax_error, unicode:chardata()}
      | {unknown_token, binary()}
      | {unexpected_token, atom()}
      | {truncated_token, binary()}
      | {invalid_token, binary(), binary(), unicode:chardata()}.

-type entry() ::
        #{machine := binary() | default,
          port => inet:port_number(),
          login => binary(),
          password => binary()}.

-spec path() -> file:name_all().
path() ->
  case os:getenv("NETRC") of
    false ->
      {ok, [[HomeDir]]} = init:get_argument(home),
      filename:join(HomeDir, ".netrc");
    Path ->
      Path
  end.

-spec parse() -> result([entry()]).
parse() ->
  parse(path()).

-spec parse(file:name_all()) -> result([entry()]).
parse(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      parse_data(Data);
    {error, Reason} ->
      {error, {read_file, Reason, Path}}
  end.

-spec parse_data(binary()) -> result([entry()]).
parse_data(Data) ->
  netrc_parser:parse(Data).

-spec format_error_reason(error_reason()) -> unicode:chardata().
format_error_reason({read_file, Reason, Path}) ->
  io_lib:format("cannot read file ~ts: ~tp", [Path, Reason]);
format_error_reason({syntax_error, Description}) ->
  io_lib:format("syntax error: ~ts", [Description]);
format_error_reason({unknown_token, Name}) ->
  io_lib:format("unkown token '~ts'", [Name]);
format_error_reason({unexpected_token, Name}) ->
  io_lib:format("unexpected token '~ts' out of a machine or default group",
                [Name]);
format_error_reason({truncated_token, Name}) ->
  io_lib:format("missing value for token '~ts'", [Name]);
format_error_reason({invalid_token, Name, Value, Description}) ->
  io_lib:format("invalid value '~ts' for token '~ts': ~ts",
                [Value, Name, Description]);
format_error_reason(Reason) ->
  io_lib:format("~0tp", [Reason]).
