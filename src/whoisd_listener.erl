%%%-------------------------------------------------------------------
%%% @doc whoisd_listener module
%%%-------------------------------------------------------------------
-module(whoisd_listener).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([callback_mode/0, default_port/0, default_opts/0]).
-export([init/1, terminate/3]).
-export([active/3, passive/3]).
-export([socket/0, activate/0, close/0]).
-behavior(gen_statem).
-record(data, { socket = undefined :: port() | undefined
              , args = { undefined, [] } :: tuple() }).

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
    gen_statem:start({local, ?MODULE}, ?MODULE, Args, Opts).

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
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc default_port/0
%%--------------------------------------------------------------------
-spec default_port() -> Result when
      Result :: integer().
default_port() -> 8333.

%%--------------------------------------------------------------------
%% @doc default_opts/0
%%--------------------------------------------------------------------
-spec default_opts() -> Result when 
      Result :: list().
default_opts() -> 
    [binary, {packet, 0}, {active, once}].

%%--------------------------------------------------------------------
%% @doc callback_mode/0
%%--------------------------------------------------------------------
-spec callback_mode() -> Result when
      Result :: state_functions.
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @doc init/1
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, active, Data} | {error, Reason},
      Data :: {pid(), undefined},
      Reason :: term().
init(Args) ->
    pg2:join(whoisd_listener, self()),
    Port = proplists:get_value(port, Args, default_port()),
    Opts = proplists:get_value(opts, Args, default_opts()),
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} -> 
            Data = #data{ socket = ListenSocket
                        , args = {Port, Opts} },
            {ok, active, Data};
        {error, Reason} -> {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc terminate/3
%%--------------------------------------------------------------------
-spec terminate(Reason, State, Data) -> Result when
      Reason :: term(),
      State :: active | listen,
      Data :: tuple(),
      Result :: ok.
terminate(_Reason, _State, #data{ socket = ListenSocket }) ->
    gen_tcp:close(ListenSocket).

%%--------------------------------------------------------------------
%% @doc active/3
%%--------------------------------------------------------------------
-spec active(Type, Message, Data) -> Result when
      Type :: cast | {call, From},
      From :: term(),
      Message :: close | {get, socket},
      Data :: #data{},
      Result :: {next_state, passive, Data} |
                {keep_state, Data, Reply},
      Reply :: [{reply, From, Socket}],
      From :: pid(),
      Socket :: port().
active(cast, close, #data{ socket = ListenSocket } = Data) ->
    gen_tcp:close(ListenSocket),
    {next_state, passive, Data#data{ socket = undefined }};
active({call, From}, socket, #data{ socket = ListenSocket } = Data) ->
    {keep_state, Data, [{reply, From, ListenSocket}]}.

%%--------------------------------------------------------------------
%% @doc passive/3
%%--------------------------------------------------------------------
-spec passive(Type, Message, Data) -> Result when
      Type :: cast,
      Message :: active,
      Data :: #data{},
      Result :: {next_state, active, Data}.
passive(cast, active, #data{ socket = undefined
                        , args = {Port, Opts}} = Data) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    {next_state, active, Data#data{ socket = ListenSocket }}.

%%--------------------------------------------------------------------
%% @doc get/0
%%--------------------------------------------------------------------
-spec socket() -> Result when
      Result :: port().
socket() ->
    gen_statem:call(?MODULE, socket).

%%--------------------------------------------------------------------
%% @doc activate/0
%%--------------------------------------------------------------------
-spec activate() -> Result when
      Result :: ok.
activate() ->
    gen_statem:cast(?MODULE, active).

%%--------------------------------------------------------------------
%% @doc close/0
%%--------------------------------------------------------------------
-spec close() -> Result when
      Result :: ok.
close() ->
    gen_statem:cast(?MODULE, close).
