%%%-------------------------------------------------------------------
%%% @doc whoisd public API
%%% @end
%%%-------------------------------------------------------------------
-module(whoisd_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc start/2
%%--------------------------------------------------------------------
-spec start(Type, Args) -> Result when
      Type :: term(),
      Args :: term(),
      Result :: {ok, pid()}.
start(_Type, Args) ->
    pg2:create(whoisd_listener),
    pg2:create(whoisd_acceptor),
    whoisd_sup:start_link().

%%--------------------------------------------------------------------
%% @doc stop/1
%%--------------------------------------------------------------------
-spec stop(State) -> Result when
      State :: term(),
      Result :: ok.
stop(_State) ->
    pg2:delete(whoisd_acceptor),
    pg2:delete(whoisd_listener),
    ok.
