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
    % TODO-C01: this is the main function of the whois client. You can
    % pass your argument there. Arguments supported by whois command:
    %
    %   whois [-AadgIilmPQRr] [-c country-code | -h host] [-p port] name ...

    {ok, Args}.
