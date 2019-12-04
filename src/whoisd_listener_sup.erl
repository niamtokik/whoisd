%%%-------------------------------------------------------------------
%%% @doc whoisd_listener_sup module
%%%-------------------------------------------------------------------
-module(whoisd_listener_sup).
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
%% @doc flags/0
%%--------------------------------------------------------------------
-spec flags() -> Result when
      Result :: map().
flags() ->
    #{ strategy => one_for_one }.

%%--------------------------------------------------------------------
%% @doc child/0
%%--------------------------------------------------------------------
-spec child() -> Result when
      Result :: map().
child() -> 
    #{ id => whoisd_listener
     , start => { whoisd_listener, start_link, []}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc init/0
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, Flags, Childs},
      Flags :: map(),
      Childs :: [Child, ...],
      Child :: map().
init(_Args) ->
    % TODO-LS1: find a way to configure this supervisor with a
    %           one_for_one strategy. you can read `supervisor`
    %           documentation.
    Flags = #{},

    % TODO-LS2: find a way to start the main worker child named
    %           whoisd_listener. you can use the internal function in
    %           this module.
    Children = [],
    {ok, {Flags, Children}}.
