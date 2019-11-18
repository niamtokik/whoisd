%%%-------------------------------------------------------------------
%%% @doc whoisd_listener module
%%%-------------------------------------------------------------------
-module(whoisd_listener).
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
      Result :: {ok, pid()},
      Args :: list(),
      Opts :: list().
start(Args, Opts) -> 
    gen_start:start(?MODULE, Args, Opts).

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
      Result :: {ok, pid()},
      Args :: list().
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
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args :: list()) 
          -> {ok, active, {pid(), undefined}} |
             {error, term()}.
init(Args) ->
    Port = proplists:get_value(port, Args, default_port()),
    Opts = proplists:get_value(opts, Args, default_opts()),
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} -> {ok, active, {ListenSocket, undefined}};
        {error, Reason} -> {stop, Reason}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate(term(), {pid(), pid()|atom()}) -> ok.
terminate(_Reason, {ListenSocket, _AcceptSocket}) ->
    gen_tcp:close(ListenSocket).

listen(enter, _OldState, Data) ->
    {next_state, listen, Data};
listen(cast, {add, acceptor, Counter}, Data) ->
    [ whoisd_acceptor_sup:start_acceptor() || _ <- lists:seq(1, Counter) ],
    {keep_state, Data};
listen({call, From}, {get, acceptor}, Data) -> 
    {keep_state, Data, [{reply, From, ok}]}.

%%--------------------------------------------------------------------
%%
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
%%
%%--------------------------------------------------------------------
reuse(enter, _OldState, Data) -> {next_state, reuse, Data};
reuse(internal, reuse, Data) -> {next_state, active, Data}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
close(enter, _OldState, Data) -> {stop, close, Data}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
callback_mode() -> [state_functions, state_enter].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
default_port() -> 8333.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
accept(ListenSocket) ->
    {ok, AcceptSocket } = gen_tcp:accept(ListenSocket),
    AcceptSocket.
