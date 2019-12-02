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
    pg2:create(whoisd_listener),
    pg2:create(whoisd_acceptor),
    {ok, Pid} = whoisd_sup:start_link(),
    Children = application:get_env(whoisd, acceptor, 100),
    [ whoisd_acceptor_sup:start_acceptor() ||  _ <- lists:seq(1, Children) ],
    {ok, Pid}.

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
