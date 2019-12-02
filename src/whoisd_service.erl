%%%-------------------------------------------------------------------
%%% @doc whoisd_service module.
%%% @end
%%%-------------------------------------------------------------------
-module(whoisd_service).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([request/1]).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% @doc start/0
%% @end
%% @see start/1.
%% @end
%%--------------------------------------------------------------------
-spec start() -> Result when
      Result :: {ok, pid()}.
start() ->
    start([]).

%%--------------------------------------------------------------------
%% @doc start/1
%% @end
%% @see start/2.
%%--------------------------------------------------------------------
-spec start(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start(Args) ->
    start(Args, []).

%%--------------------------------------------------------------------
%% @doc start/2
%% @end
%%--------------------------------------------------------------------
-spec start(Args, Opts) -> Result when
      Args :: list(),
      Opts :: list(),
      Result :: {ok, pid()}.
start(Args, Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, Opts).

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
%% @doc start_link/1
%% @end
%% @see start_link/2.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Result when
      Args :: list(),
      Result :: {ok, pid()}.
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%% @end
%% @see gen_server:start_link/3
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args, Opts) -> Result when
      Args :: list(),
      Opts :: list(),
      Result :: {ok, pid()}.
start_link(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%% @see gen_server:init/1.
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%% @end
%% @see gen_server:terminate/2.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc handle_call/3
%% @end
%% @see gen_server:handle_call/3.
%% @end
%%--------------------------------------------------------------------
handle_call(Message, _From, State) ->
    {reply, Message, State}.

%%--------------------------------------------------------------------
%% @doc handle_cast/2
%% @end
%% @see gen_server:handle_cast/2.
%% @end
%%--------------------------------------------------------------------
handle_cast(_Message, State) ->
    {noreply, State}.

% TODO-014: this is your turn! create a full whoisd_service process,
%           this one will react to different message sent by the
%           clients. You will use `gen_server` behaviour.
