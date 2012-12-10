-module(mrabbit_tcp).


-export([start/0,
		stop/0]).


start() ->
	%application:start(start_sasl),
	application:start(mrabbit_tcp).



stop() ->
	application:stop(mrabbit_tcp).
