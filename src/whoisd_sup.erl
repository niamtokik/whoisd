%%%-------------------------------------------------------------------
%% @doc whoisd top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(whoisd_sup).
-behaviour(supervisor).
-export([start_link/0, start_link/1]).
-export([stop/0]).
-export([init/1]).

%%--------------------------------------------------------------------
%% @doc start_link/0
%%--------------------------------------------------------------------
-spec start_link() -> Result when
      Result :: {ok, pid()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%%--------------------------------------------------------------------
-spec start_link(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc stop/0
%%--------------------------------------------------------------------
-spec stop() -> Result when
      Result :: ok.
stop() ->
    supervisor:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc flag/0
%%--------------------------------------------------------------------
-spec flags() -> Result when
      Result :: map().
flags() ->
    #{ strategy => one_for_one }.

%%--------------------------------------------------------------------
%% @doc child/0
%%--------------------------------------------------------------------
-spec child(Type) -> Result when
      Type :: listener | acceptor,
      Result :: map().
child(listener) ->
    #{ id => whoisd_listener_sup
     , start => {whoisd_listener_sup, start_link, []}
     , type => supervisor
     };
child(acceptor) ->
    #{ id => whoisd_acceptor_sup
     , start => {whoisd_acceptor_sup, start_link, []}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, {Flag, Childs}},
      Flag :: map(),
      Childs :: [Child, ...],
      Child :: map().
init([]) ->
    Childs = [child(listener), child(acceptor)],
    {ok, {flags(), Childs}}.
