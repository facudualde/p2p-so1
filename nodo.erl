-module(nodo).
-export([
  init/0,
  get_random_id/0,
  start_udp/0,
  send_name_request/2,
  wait_invalid_name/2,
  get_unique_id/1,
  start_node/0,
  send_hello/3,
  listen_udp/3
]).

get_random_id() ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
  end, [], lists:seq(1, 4)).

start_udp() ->
  {ok, Socket} = gen_udp:open(12346, [
    binary,
    {active, true},
    {reuseaddr, true},
    {broadcast, true},
    {ip, {192,168,1,8}}
  ]),
  Socket.

send_name_request(Socket, Id) ->
  Msg = <<"NAME_REQUEST ", Id/binary, "\n">>,
  gen_udp:send(Socket, {169,254,0,0}, 12346, Msg).

wait_invalid_name(Socket, Id) ->
  receive
    {udp, Socket, _Ip, _Port, Msg} ->
      Msg = binary_to_list(Msg),
      case string:tokens(Msg, " \n") of
        ["INVALID_NAME", ReceivedId] when ReceivedId =:= Id ->
          true;
        _Other ->
          wait_invalid_name(Socket, Id)
      end
  after 10000 ->
    false
  end.

get_unique_id(Socket) ->
  Id = list_to_binary(get_random_id()),
  send_name_request(Socket, Id),
  case wait_invalid_name(Socket, binary_to_list(Id)) of
    true ->
      timer:sleep(3000),
      get_unique_id(Socket);
    false ->
      Id
  end.

send_hello(Socket, Id, Port) ->
  PortBinary = integer_to_binary(Port),
  Message = <<"HELLO ", Id/binary, " ", PortBinary/binary, "\n">>,
  gen_udp:send(Socket, {169,254,0,0}, 12346, Message),
  io:format("Enviando HELLO...: ~s~n", [Message]),
  timer:sleep(15000 + rand:uniform(5000)), 
  send_hello(Socket, Id, Port).

listen_udp(Socket, MyId, KnownNodes) ->
  receive
    {udp, Socket, Ip, _Port, Msg} ->
      MsgStr = binary_to_list(Msg),
      case string:tokens(MsgStr, " \n") of
        ["HELLO", NodeId, PortStr] ->
          if NodeId =/= MyId ->
            Port = list_to_integer(PortStr),
            NewMap = maps:put(NodeId, #{ip => Ip, port => Port}, KnownNodes),
            io:format("Se recibio HELLO de ~s en el puerto ~p:~p~n", [NodeId, Ip, Port]),
            listen_udp(Socket, MyId, NewMap);
          true ->
            listen_udp(Socket, MyId, KnownNodes)
          end;

        ["NAME_REQUEST", ReqId] ->
          if ReqId =:= MyId ->
            gen_udp:send(Socket, Ip, 12346, <<"INVALID_NAME ", ReqId/binary, "\n">>),
            io:format("INVALID_NAME ~p. ID repetido: ~s~n", [Ip, ReqId]),
            listen_udp(Socket, MyId, KnownNodes);
          true ->
            io:format("NAME_REQUEST recibido de ~p con ID ~s ~n", [Ip, ReqId]),
            listen_udp(Socket, MyId, KnownNodes)
          end;

        _Other ->
          listen_udp(Socket, MyId, KnownNodes)
      end
  end.


start_node() ->
  rand:seed(exsplus, os:timestamp()),
  Socket = start_udp(),
  UniqueId = get_unique_id(Socket),
  spawn(fun() -> send_hello(Socket, UniqueId, 12345) end),
  spawn(fun() -> listen_udp(Socket, binary_to_list(UniqueId), #{}) end),
  io:format("Mi ID único: ~s~n", [UniqueId]),
  init().

init() ->
  case file:list_dir("compartida") of
    {ok, Filenames} ->
      io:fwrite("Archivos compartidos: ~p ~n", [Filenames]),
      Filenames;
    {error, Reason} ->
      io:fwrite("Error. No se pudo leer la carpeta compartida: ~p ~n", [Reason]),
      []
  end.

