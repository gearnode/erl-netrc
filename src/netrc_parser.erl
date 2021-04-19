%% Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(netrc_parser).

-export([parse/1]).

-type token() ::
        default
      | {machine, binary()}
      | {port, binary() | inet:port_number()}
      | {login, binary()}
      | {password, binary()}
      | {account, binary()}.

-spec parse(binary()) -> netrc:result([netrc:entry()]).
parse(Data) ->
  case parse_tokens(Data, []) of
    {ok, Tokens} ->
      parse_entries(Tokens, undefined, []);
    {error, Reason} ->
      {error, Reason}
  end.

-spec parse_entries([token()], undefined | netrc:entry(), [netrc:entry()]) ->
        netrc:result([netrc:entry()]).
parse_entries([], Entry, Entries) ->
  {ok, lists:reverse(add_entry(Entry, Entries))};
parse_entries([default | Tokens], Entry, Entries) ->
  parse_entries(Tokens, #{machine => default}, add_entry(Entry, Entries));
parse_entries([{machine, Machine} | Tokens], Entry, Entries) ->
  parse_entries(Tokens, #{machine => Machine}, add_entry(Entry, Entries));
parse_entries([{Name, _} | _Tokens], undefined, _Entries) ->
  {error, {unexpected_token, Name}};
parse_entries([{Name, Value} | Tokens], Entry, Entries) ->
  parse_entries(Tokens, Entry#{Name => Value}, Entries).

-spec add_entry(undefined | netrc:entry(), [netrc:entry()]) ->
        [netrc:entry()].
add_entry(undefined, Entries) ->
  Entries;
add_entry(Entry, Entries) ->
  [Entry | Entries].

-spec parse_tokens(binary(), [token()]) -> netrc:result([token()]).
parse_tokens(Data, Tokens) ->
  case read_word(Data) of
    {<<"default">>, Rest} ->
      parse_tokens(Rest, [default | Tokens]);
    {Name, Rest} when
        Name =:= <<"machine">>;
        Name =:= <<"login">>;
        Name =:= <<"password">>;
        Name =:= <<"account">> ->
      parse_value(Rest, Name, fun parse_binary/1, Tokens);
    {Name, Rest} when
        Name =:= <<"port">> ->
      parse_value(Rest, Name, fun parse_port_number/1, Tokens);
    {Name, _} ->
      {error, {unknown_token, Name}};
    eof ->
      {ok, lists:reverse(Tokens)}
  end.

-spec parse_value(Data :: binary(), Name :: binary(),
                  fun((binary()) ->
                         {ok, term()} | {error, unicode:chardata()}),
                  [token()]) ->
        netrc:result([token()]).
parse_value(Data, Name, ParseValue, Tokens) ->
  case read_word(Data) of
    {Value, Rest} ->
      case ParseValue(Value) of
        {ok, Term} ->
          parse_tokens(Rest, [{binary_to_atom(Name), Term} | Tokens]);
        {error, Description} ->
          {error, {invalid_token, Name, Value, Description}}
      end;
    eof ->
      {error, {truncated_token, Name}}
  end.

-spec parse_binary(binary()) ->
        {ok, binary()} | {error, unicode:chardata()}.
parse_binary(Value) ->
  {ok, Value}.

-spec parse_port_number(binary()) ->
        {ok, binary() | inet:port_number()} | {error, unicode:chardata()}.
parse_port_number(Value = <<Digit, _/binary>>) when
    Digit >= $0, Digit =< $9 ->
  try
    binary_to_integer(Value)
  of
    N when N > 0, N < 65536 ->
      {ok, N};
    _ ->
      {error, "invalid port number"}
  catch
    error:_ ->
      {error, "invalid integer"}
  end;
parse_port_number(Value) ->
  {ok, Value}.

-spec read_word(binary()) -> {binary(), binary()} | eof.
read_word(<<C, Data/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n ->
  read_word(Data);
read_word(Data) ->
  case binary:split(Data, [<<" ">>, <<"\t">>, <<"\n">>]) of
    [Word, Rest] ->
      {Word, Rest};
    [<<>>] ->
      eof;
    [Word] ->
      {Word, <<>>}
  end.
