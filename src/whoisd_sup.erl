%%%-------------------------------------------------------------------
%%% @doc whoisd top level application supervisor called
%%% whoisd_sup. This supervisor is the first one to be started. When
%%% whoisd application is started by using application:start(whoisd),
%%% whoisd_sup:start_link/1 start automatically three children:
%%% whoisd_listener_sup:start_link/1,
%%% whoisd_acceptor_sup:start_link/1, whoisd_manager_sup:start_link/1.
%%% @end
%%% -------------------------------------------------------------------
-module(whoisd_sup).
-behaviour(supervisor).
-export([start_link/0, start_link/1]).
-export([init/1]).

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @end
%% @see start_link/1.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> Result when
      Result :: {ok, pid()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1 start whoisd_sup as linked supervisor
%% @end
%% application.
%% --------------------------------------------------------------------
-spec start_link(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc flag/0 return default whoisd_sup supervisor flag.
%% @end
%%--------------------------------------------------------------------
-spec flags() -> Result when
      Result :: map().
flags() ->
    #{ strategy => one_for_one }.

%%--------------------------------------------------------------------
%% @doc child/0
%% @end
%% @see child/1.
%% @end
%%--------------------------------------------------------------------
-spec child(Type) -> Result when
      Type :: atom(),
      Result :: map().
child(Module) ->
    child(Module, []).

%%--------------------------------------------------------------------
%% @doc child/1 returns a standard supervisor child for whoisd_sup
%% supervisor.
%% @end
%% --------------------------------------------------------------------
-spec child(Module, Args) -> Result when
      Module :: atom(),
      Args :: list(),
      Result :: map().
child(Module, Args) ->
    #{ id => Module
     , start => {Module, start_link, [Args]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc init/1 returns supervisor flags and 3 children,
%% whoisd_manager_sup, whoisd_listener_sup and whoisd_acceptor_sup.
%% @end
%% --------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, {Flag, Childs}},
      Flag :: map(),
      Childs :: [Child, ...],
      Child :: map().
init(_Args) ->
    % TODO-004: find a way to start these children:
    %           `whoisd_listener_sup`, `whoisd_acceptor_sup`,
    %           `whoisd_storage_sup`, `whoisd_service_sup` and
    %           `whoisd_manager_sup`, all as supervisor. You can use
    %           the different helpers defined in this module.
    Children = []
    {ok, {flags(), Children}}.
