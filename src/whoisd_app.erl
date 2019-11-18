%%%-------------------------------------------------------------------
%%% @doc whoisd public API
%%% @end
%%%-------------------------------------------------------------------
-module(whoisd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    whoisd_sup:start_link().

stop(_State) ->
    ok.
