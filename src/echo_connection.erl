-module(echo_connection).

-export([start_link/0, init/1]).

-record(v1, {parent, sock, callback, buffer, connection_state}).

start_link() ->
  {ok, proc_lib:spawn_link(?MODULE, init, [self()])}. 

init(Parent) ->
  receive 
	{go, Sock, SockTransform} ->
io:format("receive go Parent=~p~n", [Parent]),
      start_connection(Parent, Sock,  SockTransform)
  end.       

switch_callback(State, Callback) ->
  State#v1{callback = Callback}.

start_connection(Parent, Sock, _SockTransform) ->
  process_flag(trap_exit, true),
  recv_loop(switch_callback(#v1{parent = Parent,
				sock = Sock,
				callback = uninitialized_callback,
				buffer = []},
			    handle_recv_echo_msg)).

read_line(Data) ->
  read_line(Data, "").
read_line("", _Line) ->
  noline;
read_line("\r\n" ++ Data, Line) ->
  {line, lists:reverse(Line), Data};
read_line([Char|Data], Line) ->
  read_line(Data, [Char | Line]).

recv_loop(State = #v1{buffer = Buffer})
  when Buffer =:= [] ->
    main_loop(State);
recv_loop(State = #v1{buffer = Buffer}) ->
	D = list_to_binary(Buffer),
  Data = binary_to_list(D),
io:format("receive msg: data:~p~n", [Data]),
  case read_line(Data) of
    {line, Line, NewData} ->
	  io:format("line Line=~p NewData=~p~n", [Line, NewData]),
      recv_loop(handle_input(State#v1.callback,
	  Line,
	  State#v1{buffer = [NewData]}));
    noline ->
      io:format("noline~n"),
		main_loop(State#v1{buffer = []})
  end.

handle_input(handle_recv_echo_msg, Msg, State) ->
  io:format("recv msg:~p~n", [Msg]),
  gen_tcp:send(State#v1.sock, Msg ++ "\r\n"),
  recv_loop(State).

main_loop(State = #v1{sock = Sock, buffer = Buffer}) ->
  inet:setopts(Sock,[{active,once}]),
  case tcp_server_util:recv(Sock) of
    {data, Data} ->
      recv_loop(State#v1{buffer = [Data | Buffer]});
    closed -> if State#v1.connection_state =:= closed ->
	  State;
	true ->
	  	io:format("connect closed~n")
		%throw(connection_closed_abruptly)
      end;
    {error, Reason} ->
      throw({inet_error, Reason});
    {other, Other} ->
      handle_other(Other, State)
  end.

handle_other(_Other, _State) ->
  io:format("handle_other~n").
