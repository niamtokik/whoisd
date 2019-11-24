%%%-------------------------------------------------------------------
%%% @doc whoisd_acceptor_sup module
%%%-------------------------------------------------------------------
-module(whoisd_acceptor_sup).
-export([start_link/0, start_link/1]).
-export([init/1]).
-export([start_acceptor/0, start_acceptor/1]).
-export([flags/0]).
-export([child/0]).
-behavior(supervisor).

%%-------------------------------------------------------------------
%% @doc start_link/0
%%-------------------------------------------------------------------
-spec start_link() -> Result when
      Result :: {ok, pid()}.
start_link() ->
    start_link([]).

%%-------------------------------------------------------------------
%% @doc start_link/1
%%-------------------------------------------------------------------
-spec start_link(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, {Flags, Childs}},
      Flags :: map(),
      Childs :: [Child, ...],
      Child :: map().
init(_Args) ->
    {ok, {flags(), [child()]}}.

%%--------------------------------------------------------------------
%% @doc start_acceptor/0
%%--------------------------------------------------------------------
-spec start_acceptor() -> Result when
      Result :: ok.
start_acceptor() ->
    start_acceptor([]).

%%--------------------------------------------------------------------
%% @doc start_acceptor/1
%%--------------------------------------------------------------------
-spec start_acceptor(Args) -> Result when
      Args :: list(),
      Result :: ok.
start_acceptor(_Args) ->
    supervisor:start_child(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc flags/0
%%--------------------------------------------------------------------
-spec flags() -> Result when
      Result :: map().
flags() ->
    #{ strategy => simple_one_for_one }.

%%--------------------------------------------------------------------
%% @doc child/0
%%--------------------------------------------------------------------
-spec child() -> Result when
      Result :: map().
child() ->
    child([]).

%%--------------------------------------------------------------------
%% @doc child/1
%%--------------------------------------------------------------------
-spec child(Args) -> Result when
      Args :: list(),
      Result :: map().
child(_Args) ->
    #{ id => whoisd_acceptor
     , start => {whoisd_acceptor, start_link, []}
     , restart => transient
     , type => worker
     }.
