%%%-------------------------------------------------------------------
%%% @doc whoisd_acceptor module create a new acceptor worker based
%%%      on a listen socket given directly when we start it.
%%%-------------------------------------------------------------------
-module(whoisd_acceptor).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([callback_mode/0]).
-export([init/1, terminate/2]).
-export([active/3, reuse/3, close/3]).
-behavior(gen_statem).
-record(data, { listen_socket = undefined 
              , accept_socket = undefined }).

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
callback_mode() -> [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: Proplist | port(),
      Proplist :: [{atom(), term()}],
      Result :: {ok, atom(), Data},
      Data :: #data{}.
init(ListenSocket) 
  when is_port(ListenSocket) ->
    pg2:join(whoisd_acceptor, self()),
    {ok, active, #data{ listen_socket = ListenSocket }};
init(Args) 
  when is_list(Args) ->
    pg2:join(whoisd_acceptor, self()),
    ListenSocket = proplists:get_value(socket, Args, undefined),
    case ListenSocket of
        undefined -> 
            {ok, passive, #data{}};
        ListenSocket when is_port(ListenSocket) ->
            {ok, active, #data{ listen_socket = ListenSocket }};
        _ -> 
            {stop, badargs}
    end.

%%--------------------------------------------------------------------
%% @doc terminate/2
%%--------------------------------------------------------------------
-spec terminate(term(), {pid(), pid()|atom()}) -> ok.
terminate(_Reason, {ListenSocket, _AcceptSocket}) ->
    gen_tcp:close(ListenSocket).

%%--------------------------------------------------------------------
%% @doc active/3
%%--------------------------------------------------------------------
-spec active(Type, Message, Data) -> Result when
      Type :: enter | info,
      Message :: {tcp, _, TcpMessage} | {tcp_closed, _},
      TcpMessage :: bitstring(),
      Data :: #data{},
      Result :: {keep_state, Data} |
                {next_state, active, Data} |
                {next_state, reuse, Data}.
active(enter, _OldState, #data{ listen_socket = ListenSocket } = Data) ->
    AcceptSocket = accept(ListenSocket),
    {next_state, active, Data#data{ accept_socket = AcceptSocket } };
active(info, {tcp, _, Message}, #data{ accept_socket = AcceptSocket } = Data) ->
    io:format("got message: ~p on ~p(~p)~n", [Message, self(), AcceptSocket]),
    {keep_state, Data};
active(info, {tcp_closed, _}, #data{ accept_socket = AcceptSocket } = Data) ->
    gen_tcp:close(AcceptSocket),
    {next_state, reuse, Data#data{ accept_socket = undefined}, [{next_event, internal, reuse}]}.

%%--------------------------------------------------------------------
%% @doc reuse/3
%%--------------------------------------------------------------------
-spec reuse(Type, Message, Data) -> Result when
      Type :: enter | internal,
      Message :: reuse,
      Data :: #data{},
      Result :: {nxt_state, reuse | active, Data}.
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
