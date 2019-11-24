%%%-------------------------------------------------------------------
%%% @doc whois client CLI. This code is used when whois command is 
%%%      executed from the command line.
%%% @end
%%%-------------------------------------------------------------------
-module(whois).
-export([main/1]).

%%--------------------------------------------------------------------
%% @doc main/1
%% @end
%%--------------------------------------------------------------------
-spec main(Args) -> Result when
      Args :: [list(), ...],
      Result :: any().
main(Args) ->
    {ok, Args}.
