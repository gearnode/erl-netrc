%% Copyright (c) 2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(netrc).

-export([search/1, search/2,
         path/0, load/0, load/1,
         format_error_reason/1]).

-export_type([result/0, result/1, entry/0, query/0, error_reason/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type entry() ::
        #{machine := binary() | default,
          port => binary() | inet:port_number(),
          login => binary(),
          password => binary(),
          account => binary()}.

-type query() ::
        #{machine := binary(),
          port => binary() | inet:port_number(),
          login => binary(),
          account => binary()}.

-type error_reason() ::
        {read_file, term(), file:name_all()}
      | {syntax_error, unicode:chardata()}
      | {unknown_token, binary()}
      | {unexpected_token, atom()}
      | {truncated_token, binary()}
      | {invalid_token, binary(), binary(), unicode:chardata()}.

-spec search(query()) -> [entry()].
search(Query) ->
  netrc_cache:search(Query).

-spec search(query(), [entry()]) -> [entry()].
search(Query, Entries) ->
  [Entry || Entry <- Entries, match(Query, Entry)].

-spec match(query(), entry()) -> boolean().
match(Query, Entry) ->
  maps:with(maps:keys(Query), Entry) =:= Query.

-spec path() -> file:name_all().
path() ->
  case os:getenv("NETRC") of
    false ->
      {ok, [[HomeDir]]} = init:get_argument(home),
      filename:join(HomeDir, ".netrc");
    Path ->
      Path
  end.

-spec load() -> result([entry()]).
load() ->
  load(path()).

-spec load(file:name_all()) -> result([entry()]).
load(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      netrc_parser:parse(Data);
    {error, Reason} ->
      {error, {read_file, Reason, Path}}
  end.

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
