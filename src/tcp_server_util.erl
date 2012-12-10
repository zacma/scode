-module(tcp_server_util).

-export([tcp_name/3,
	 getaddr/2,
	 check_tcp_listener_address/2]).
-export([ntoa/1, ntoab/1]).
-export([recv/1]).
-export([throw_on_error/2, peername/1, socket_op/2]).

socket_op(Sock, Fun) ->
  case Fun(Sock) of
    {ok, Res}       -> Res;
    {error, Reason} ->
      io:format("error on TCP connection ~p:~p~n", [self(), Reason]),
      io:format("closing TCP connection ~p~n", [self()]),
      exit(normal)
  end.

peername(Sock) when is_port(Sock) ->
  inet:peername(Sock).

tcp_name(Prefix, IPAddress, Port)
  when is_atom(Prefix) andalso is_number(Port) ->
   list_to_atom(
     lists:flatten(
       io_lib:format("~w_~s:~w",
	 [Prefix, inet_parse:ntoa(IPAddress), Port]))).

%% inet_parse:address takes care of ip string, like "0.0.0.0"
%% inet:getaddr returns immediately for ip tuple {0,0,0,0},
%%  and runs 'inet_gethost' port process for dns lookups.
%% On Windows inet:getaddr runs dns resolver for ip string, which may fail.
getaddr(Host, Family) ->
  case inet_parse:address(Host) of
    {ok, IPAddress} ->
      [{IPAddress, resolve_family(IPAddress, Family)}];
    {error, _}      ->
      gethostaddr(Host, Family)
  end.

gethostaddr(Host, auto) -> 
  Lookups = [{Family, inet:getaddr(Host, Family)} || Family <- [inet, inet6]],
  case [{IP, Family} || {Family, {ok, IP}} <- Lookups] of
    []  -> host_lookup_error(Host, Lookups);
    IPs -> IPs
  end;

gethostaddr(Host, Family) ->
  case inet:getaddr(Host, Family) of
    {ok, IPAddress} ->
      [{IPAddress, Family}]; 
    {error, Reason} ->
      host_lookup_error(Host,
	Reason)
  end.  

host_lookup_error(Host, Reason) ->
  error_logger:error_msg("invalid host ~p - ~p~n", [Host, Reason]),
  throw({error, {invalid_host, Host, Reason}}).

resolve_family({_,_,_,_}, auto) -> inet;
resolve_family({_,_,_,_,_,_,_,_}, auto) -> inet6;
resolve_family(IP, auto) ->
  throw({error, {strange_family, IP}});
resolve_family(_, F)    -> F.

check_tcp_listener_address(NamePrefix, {Host, Port, Family0}) ->
  if is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) ->
      ok;
    true ->
      error_logger:error_msg("invalid port ~p - not 0..65535~n", [Port]),
      throw({error, {invalid_port, Port}})
 end,
 [{IPAddress, Port, Family, tcp_name(NamePrefix, IPAddress, Port)}
   || {IPAddress, Family} <- getaddr(Host, Family0)].

%% Format IPv4-mapped IPv6 addresses as IPv4, since they're what we see
%% when IPv6 is enabled but not used (i.e. 99% of the time).
ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
inet_parse:ntoa(IP).

ntoab(IP) ->
  Str = ntoa(IP),
    case string:str(Str, ":") of
    0 -> Str;
    _ -> "[" ++ Str ++ "]" 
    end.

throw_on_error(E, Thunk) ->
  case Thunk() of
    {error, Reason} -> throw({E, Reason});
    {ok, Res}       -> Res;
    Res             -> Res
  end.

recv(Sock) when is_port(Sock) ->
  recv(Sock, {tcp, tcp_closed, tcp_error}).

recv(S, {DataTag, ClosedTag, ErrorTag}) ->
  receive
    {DataTag, S, Data}    -> {data, Data};
    {ClosedTag, S}        -> closed;
    {ErrorTag, S, Reason} -> {error, Reason};
    Other                 -> {other,
	Other}
  end.
