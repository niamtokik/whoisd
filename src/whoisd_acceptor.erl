%%%-------------------------------------------------------------------
%%% @doc whoisd_acceptor module
%%%-------------------------------------------------------------------
-module(whoisd_acceptor).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([callback_mode/0, default_port/0, default_opts/0]).
-export([init/1, terminate/2]).
-export([active/3, reuse/3, close/3]).
-behavior(gen_statem).

%%--------------------------------------------------------------------
%% @doc start/0
%%--------------------------------------------------------------------
-spec start() -> Result when
      Result :: {ok, pid()}.
start() -> 
    start([]).

%%--------------------------------------------------------------------
%% @doc start/1
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
    gen_statem:start(?MODULE, Args, Opts).

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
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%%--------------------------------------------------------------------
-spec start_link(Args, Opts) -> Result when
      Args :: list(),
      Opts :: list(),
      Result :: {ok, pid()}.
start_link(Args, Opts) -> 
    gen_statem:start_link(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc default_opts/0
%%--------------------------------------------------------------------
-spec default_opts() -> Result when
      Result :: list().
default_opts() -> 
    [binary, {packet, 0}].

%%--------------------------------------------------------------------
%% @doc default_port
%%--------------------------------------------------------------------
-spec default_port() -> Result when
      Result :: integer().
default_port() ->
    4343.

%%--------------------------------------------------------------------
%% @doc callback_mode/0
%%--------------------------------------------------------------------
-spec callback_mode() -> Result when
      Result :: [atom(), ...].
callback_mode() ->
    [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(State) -> Result when
      State :: {port(), port()},
      Result :: {ok, atom(), State}.
init({ListenSocket, AcceptSocket}) ->
    pg2:join(whoisd_acceptor, self()),
    {ok, active, {ListenSocket, AcceptSocket}}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%%--------------------------------------------------------------------
-spec terminate(term(), {pid(), pid()|atom()}) -> ok.
terminate(_Reason, {ListenSocket, _AcceptSocket}) ->
    gen_tcp:close(ListenSocket).

%%--------------------------------------------------------------------
%% @doc active/3
%%--------------------------------------------------------------------
active(enter, _OldState, {ListenSocket, undefined}) ->
    AcceptSocket = accept(ListenSocket),
    {next_state, active, {ListenSocket, AcceptSocket}};
active(info, {tcp, AcceptSocket, Message}, {ListenSocket, AcceptSocket}) ->
    io:format("got message: ~p~n", [Message]),
    {keep_state, {ListenSocket, AcceptSocket}};
active(info, {tcp_closed, AcceptSocket}, {ListenSocket, AcceptSocket}) ->
    gen_tcp:close(AcceptSocket),
    {next_state, reuse, {ListenSocket, undefined}, [{next_event, internal, reuse}]};
active(_Type, _Message, {ListenSocket, ActiveSocket}) ->
    {keep_state, {ListenSocket, ActiveSocket}}.

%%--------------------------------------------------------------------
%% @doc reuse/3
%%--------------------------------------------------------------------
reuse(enter, _OldState, Data) ->
    {next_state, reuse, Data};
reuse(internal, reuse, Data) ->
    {next_state, active, Data}.

%%--------------------------------------------------------------------
%% @doc close/3
%%--------------------------------------------------------------------
close(enter, _OldState, Data) ->
    {stop, close, Data}.

%%--------------------------------------------------------------------
%% @doc accept/1
%%--------------------------------------------------------------------
accept(ListenSocket) ->
    {ok, AcceptSocket } = gen_tcp:accept(ListenSocket),
    AcceptSocket.
