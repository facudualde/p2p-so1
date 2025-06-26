-module(nodo).
-export([shared_files/0, get_random_id/0, start_node/0, send_hello/3, loop/4]).
-define(UDP_PORT, 12346).

get_random_id() ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
  end, [], lists:seq(1, 4)).

start_node() ->
  rand:seed(exsplus, os:timestamp()),
  case gen_udp:open(?UDP_PORT, [binary, {active, true}, {reuseaddr, true}, {broadcast, true}, {ip, {0,0,0,0}}]) of
    {ok, Socket} ->
      Id = get_valid_id(Socket, []),
      io:format("El ID del nodo es: ~s~n", [Id]),
      shared_files(),
      spawn(fun() -> send_hello(Socket, Id, ?UDP_PORT) end),
      loop(Socket, Id, [binary_to_list(Id)], #{});
    {error, Reason} ->
      io:format("Error al iniciar el servidor UDP: ~p~n", [Reason]),
      {error, Reason}
  end.

get_valid_id(Socket, TriedIds) ->
  Id = get_random_id(),
  case lists:member(Id, TriedIds) of
    true ->
      get_valid_id(Socket, TriedIds);  
    false ->
      send_name_request(Socket, Id),
      case wait_invalid_name(Socket, Id) of
        true ->
          RandSleep = 2000 + rand:uniform(8000),
          timer:sleep(RandSleep),
          get_valid_id(Socket, [Id | TriedIds]);
        false ->
          list_to_binary(Id)
      end
  end.

send_name_request(Socket, Id) ->
  Msg = <<"NAME_REQUEST ", (list_to_binary(Id))/binary, "\n">>,
  gen_udp:send(Socket, {255,255,255,255}, ?UDP_PORT, Msg).

wait_invalid_name(Socket, Id) ->
  receive
    {udp, Socket, _Ip, _Port, Msg} ->
      MsgStr = binary_to_list(Msg),
      case string:tokens(MsgStr, " \n") of
        ["INVALID_NAME", OtherId] when OtherId =:= Id ->
          true;
        _ ->
          wait_invalid_name(Socket, Id)
      end
  after 10000 ->
    false
  end.

send_hello(Socket, Id, Port) ->
  Mesg = <<"HELLO ", Id/binary, " ", (integer_to_binary(Port))/binary, "\n">>,
  gen_udp:send(Socket, {255,255,255,255}, ?UDP_PORT, Mesg),
  io:format("Enviando HELLO...: ~s~n", [Mesg]),
  timer:sleep(15000 + rand:uniform(5000)), 
  send_hello(Socket, Id, Port).

loop(Socket, MyId, MyRequestedIds, KnownNodes) ->
  receive
    {udp, Socket, Ip, _Port, Msg} ->
      inet:setopts(Socket, [{active, true}]),
      MsgStr = binary_to_list(Msg),
      case string:tokens(MsgStr, " \n") of
        ["HELLO", NodeId, PortStr] ->
          case NodeId =:= binary_to_list(MyId) of
            true ->
              loop(Socket, MyId, MyRequestedIds, KnownNodes);
            false ->
              Port = list_to_integer(PortStr),
              NewMap = maps:put(NodeId, #{ip => Ip, port => Port}, KnownNodes),
              io:format("Se recibió HELLO de ~s en ~p:~p~n", [NodeId, Ip, Port]),
              loop(Socket, MyId, MyRequestedIds, NewMap)
          end;

        ["NAME_REQUEST", ReqId] ->
          case ReqId =:= binary_to_list(MyId) orelse lists:member(ReqId, MyRequestedIds) of
            true ->
              gen_udp:send(Socket, Ip, ?UDP_PORT, <<"INVALID_NAME ", ReqId/binary, "\n">>),
              io:format("Enviado INVALID_NAME a ~p por ID repetido: ~s~n", [Ip, ReqId]),
              loop(Socket, MyId, MyRequestedIds, KnownNodes);
            false ->
              io:format("NAME_REQUEST recibido de ~p con ID ~s~n", [Ip, ReqId]),
              loop(Socket, MyId, MyRequestedIds, KnownNodes)
          end;

        _Other ->
          io:format("Mensaje no reconocido: ~s~n", [MsgStr]),
          loop(Socket, MyId, MyRequestedIds, KnownNodes)
      end
  end.

shared_files() ->
  case file:list_dir("compartida") of
    {ok, Filenames} ->
      io:fwrite("Archivos compartidos: ~p ~n", [Filenames]),
      Filenames;
    {error, Reason} ->
      io:fwrite("Error al leer carpeta compartida: ~p ~n", [Reason]),
      []
  end.

