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
start(_Type, _Args) ->
    % TODO-001: add a way to create process group when the application
    %           start by using `pg`" module.

    % TODO-002: add a way to start the main application supervisor
    %           process named `whoisd_sup`.
    ok.

%%--------------------------------------------------------------------
%% @doc stop/1
%%--------------------------------------------------------------------
-spec stop(State) -> Result when
      State :: term(),
      Result :: ok.
stop(_State) ->
    % TODO-003: add a way to clean the application and remove
    %           different group from `pg2`.
    ok.
