%%%--------------------------------------------------------------------
%%% @doc whois server CLI. This code executed when whoisd is called
%%%      from the command line.
%%% @end
%%%--------------------------------------------------------------------
-module(whoisd).
-export([main/1]).

%%--------------------------------------------------------------------
%% @doc main/1 get arguments from command line and parse them with the
%%      help of main/2 function.
%% @end
%% @see main/2.
%% @end
%%--------------------------------------------------------------------
-spec main(Args) -> Result when
      Args :: [list(), ...],
      Result :: any().
main(Args) ->
    Config = main(Args, #{}),
    loop(Config).

%%--------------------------------------------------------------------
%% @doc main/2 function take a list of argument from command line and
%%      create a structured map based on supported input.
%% @end
%%--------------------------------------------------------------------
-spec main(Args, Buffer) -> Result when
      Args :: [list(), ...],
      Buffer :: map(),
      Result :: any().
main([], Buffer) ->
    Buffer;
main(["-port", Port|Rest], Buffer) ->
    main(Rest, Buffer#{ port => Port });
main(["-address", Address|Rest], Buffer) ->
    main(Rest, Buffer#{ address => Address }).

%%--------------------------------------------------------------------
%% @doc loop/0
%% @end
%%--------------------------------------------------------------------
loop(Args) ->
    receive
        _ -> loop(Args)
    end.
             
