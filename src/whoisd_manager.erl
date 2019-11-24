%%%-------------------------------------------------------------------
%%% @doc whoisd_manager module control whoisd application by giving
%%%      a simple interface to other whoisd processes, you can easily
%%%      start listener, acceptor or retrieve stats from different
%%%      running processes related to whoisd.
%%%-------------------------------------------------------------------
-module(whoisd_manager).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([listener/0, listener/1]).
-export([acceptor/0, acceptor/1, acceptor/2]).
-behaviour(gen_server).
-type state() :: map().

%%--------------------------------------------------------------------
%% @doc start/0
%% @see start/1
%%--------------------------------------------------------------------
-spec start() -> Result when
      Result :: {ok, pid()}.
start() ->
    start([]).

%%--------------------------------------------------------------------
%% @doc start/1
%% @see start/2
%%--------------------------------------------------------------------
-spec start(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start(Args) ->
    start(Args, []).

%%--------------------------------------------------------------------
%% @doc start/2
%%--------------------------------------------------------------------
-spec start(Args, Opts) -> Result when
      Args :: list(),
      Opts :: list(),
      Result :: {ok, pid()}.
start(Args, Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc start_link/0
%% @see start_link/1
%%--------------------------------------------------------------------
-spec start_link() -> Result when
      Result :: {ok, pid()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @see start_link/2
%%--------------------------------------------------------------------
-spec start_link(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%%--------------------------------------------------------------------
-spec start_link(Args, Opts) -> Result when
      Args :: list(),
      Opts :: list(),
      Result :: {ok, pid()}.
start_link(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, State},
      State :: state().
init(_Args) ->
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Result when
      Reason :: term(),
      State :: state(),
      Result :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc handle_call/3
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Result when
      Message :: listener | acceptor,
      From :: term(),
      State :: state(),
      Result :: {reply, term(), state()}.
handle_call(listener, _From, State) ->
    Response = pg2:get_members(whoisd_listener),
    {reply, Response, State};
handle_call(acceptor, _From, State) ->
    Response = pg2:get_members(whoisd_acceptor),
    {reply, Response, State}.

%%--------------------------------------------------------------------
%% @doc handle_cast/3
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Result when
      Message :: Acceptor,
      Acceptor :: acceptor | {acceptor, Args} | {acceptor, Args, Opts},
      Args :: list(),
      Opts :: list(),
      State :: state(),
      Result :: {noreply, state()}.
handle_cast(acceptor, State) ->
    {noreply, State};
handle_cast({acceptor, _Args}, State) ->
    {noreply, State};
handle_cast({acceptor, _Args, _Opts}, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc listener/0 api 
%%--------------------------------------------------------------------
listener() ->
    gen_server:call(?MODULE, listener).

%%--------------------------------------------------------------------
%% @doc listener/1 api
%%--------------------------------------------------------------------
listener(Args) ->
    gen_server:cast(?MODULE, {listener, Args}).

%%--------------------------------------------------------------------
%% @doc acceptor/0 api
%%--------------------------------------------------------------------
acceptor() ->
    gen_server:call(?MODULE, acceptor).

%%--------------------------------------------------------------------
%% @doc acceptor/1 api
%%--------------------------------------------------------------------
acceptor(Args) ->
    gen_server:cast(?MODULE, {acceptor, Args}).

%%--------------------------------------------------------------------
%% @doc acceptor/2 api
%%--------------------------------------------------------------------
acceptor(Args, Opts) ->
    gen_server:cast(?MODULE, {acceptor, Args, Opts}).
