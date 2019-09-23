-module(whoisd_acceptor_sup).
-export([start_link/0, start_link/1]).
-export([init/1]).
-export([start_acceptor/0, start_acceptor/1]).
-export([supervisor_flags/0]).
-export([child_spec/0]).
-behavior(supervisor).

start_link() ->
    start_link([]).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    supervisor:start_link({local, ?MODULE}, Args, Opts).

init(_Args) ->
    {ok, {supervisor_flags, []}}.

start_acceptor() ->
    start_acceptor([]).

start_acceptor(Args) ->
    supervisor:start_child(?MODULE, child_spec(Args)).

supervisor_flags() ->
    #{ strategy => simple_one_for_one }.

child_spec() ->
    child_spec([]).

child_spec(Args) ->
    #{ id => whoisd_acceptor
     , start => {whoisd_acceptor, start_link, Args}
     , type => worker
     }.
    
