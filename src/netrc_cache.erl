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

-module(netrc_cache).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, terminate/2,
         handle_continue/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([ref/0, options/0]).

-type ref() :: et_gen_server:ref().

-type options() :: #{}.

-type state() :: #{options := options(),
                   entries := [netrc:entry()]}.

-spec start_link(options()) -> et_gen_server:start_ret().
start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-spec stop(ref()) -> ok.
stop(Ref) ->
  gen_server:stop(Ref).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => [netrc, cache]}),
  State = #{options => Options,
            entries => []},
  {ok, State, {continue, reload}}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec handle_continue(term(), state()) ->
        et_gen_server:handle_continue_ret(state()).
handle_continue(reload, State) ->
  case reload(State) of
    {ok, State2} ->
      {noreply, State2};
    {error, Reason} ->
      ?LOG_ERROR("cannot load netrc file: ~tp", [Reason]),
      {stop, {error, Reason}, State}
  end.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec reload(state()) -> netrc:result(state()).
reload(State) ->
  Path = netrc:path(),
  ?LOG_INFO("loading ~ts", [Path]),
  case netrc:load(Path) of
    {ok, Entries} ->
      ?LOG_DEBUG("~b entries loaded", [length(Entries)]),
      {ok, State#{entries => Entries}};
    {error, Reason} ->
      {error, Reason}
  end.
