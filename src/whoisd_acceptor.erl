%%%-------------------------------------------------------------------
%%% @doc whoisd_acceptor module create a new acceptor worker based
%%%      on a listen socket given directly when we start it.
%%%-------------------------------------------------------------------
-module(whoisd_acceptor).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([callback_mode/0]).
-export([init/1, terminate/3]).
-export([active/3]).
-behavior(gen_statem).
-record(data, { listen_socket = undefined 
              , accept_socket = undefined }).

%%--------------------------------------------------------------------
%% @doc start/0 
%% @see start/1.
%%--------------------------------------------------------------------
-spec start() -> Result when
      Result :: {ok, pid()}.
start() -> 
    start([]).

%%--------------------------------------------------------------------
%% @doc start/1
%% @see start/2.
%%--------------------------------------------------------------------
-spec start(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start(Args) -> 
    start(Args, []).

%%--------------------------------------------------------------------
%% @doc start/2 start a worker without link with acceptor arguments
%%      and OTP options based on gen_statem.
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
%% @doc callback_mode/0
%%--------------------------------------------------------------------
-spec callback_mode() -> Result when
      Result :: list().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok, atom(), Data},
      Data :: #data{}.
init(_Args) ->
    pg2:join(whoisd_acceptor, self()),
    ListenSocket = whoisd_listener:socket(),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    {ok, active, #data{ listen_socket = ListenSocket
                      , accept_socket = AcceptSocket }}.

%%--------------------------------------------------------------------
%% @doc terminate/3
%%--------------------------------------------------------------------
-spec terminate(Reason, State, Data) -> Result when
      Reason :: term(),
      State :: term(),
      Data :: #data{},
      Result :: ok.
terminate(_Reason, _State, #data{ accept_socket = AcceptSocket }) ->
    io:format("process ~p terminated", [self()]),
    ok = gen_tcp:close(AcceptSocket),
    {ok, _Pid} = whoisd_acceptor_sup:start_acceptor(),
    ok.

%%--------------------------------------------------------------------
%% @doc active/3
%%--------------------------------------------------------------------
-spec active(Type, Message, Data) -> Result when
      Type :: info,
      Message :: {tcp, _, TcpMessage} | {tcp_closed, _},
      TcpMessage :: bitstring(),
      Data :: #data{},
      Result :: {stop, normal, Data} |
                {keep_state, Data}.
active(info, {tcp, Socket, Message}, Data) ->
    io:format("got message: ~p on ~p(~p)~n", [Message, self(), Socket]),
    {ok, Answer} = whoisd_service:request(Message),
    gen_tcp:send(Socket, Answer),
    {stop, normal, Data};
active(info, {tcp_closed, _}, Data) ->
    {stop, normal, Data};
active(Type, Message, Data) ->
    io:format("got: ~p ~p ~p", [Type, Message, Data]),
    {keep_state, Data}.
