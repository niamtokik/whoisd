-module(whoisd_listener_sup).
-export([start_link/0, start_link/1]).
-export([stop/0]).
-export([init/1]).

start_link() ->
    start_link([]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

stop() ->
    supervisor:stop(?MODULE).

flags() ->
    #{ strategy => one_for_one }.

child() -> 
    #{ id => whoisd_listener
     , start => { whoisd_listener, start_link, []}
     }.

init(_Args) ->
    {ok, flags(), [child()]}.
