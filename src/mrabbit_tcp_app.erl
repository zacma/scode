-module(mrabbit_tcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
 io:format("11111111111~n"),
	{ok, SupPid} = mrabbit_tcp_sup:start_link(),
	rabbit_networking:boot_tcp(),
	rabbit_networking:start(), 
io:format("211111111111~n"),
	%% here start rabbit tcp		
	{ok, SupPid}.

stop(_State) ->
    ok.
