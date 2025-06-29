-module(nodo).
-export([shared_files/0, get_random_id/0, start_node/0, send_hello/3, loop/4,remove_inactive_nodes/2, cli_loop/2]).
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
      spawn(fun() -> cli_loop(Id, self()) end),
      KnownNodesFromFile = nodes_registry:load(),
      CurrentTime = os:system_time(second),
      KnownNodesSaved = maps:map(fun(_Key, Val) ->
        Val#{last_seen => CurrentTime}
      end, KnownNodesFromFile),
      spawn(fun() -> send_hello(Socket, Id, ?UDP_PORT) end),
      loop(Socket, Id, [binary_to_list(Id)], KnownNodesSaved);
    {error, Reason} ->
      io:format("Error al iniciar el servidor UDP: ~p~n", [Reason]),
      {error, Reason}
  end.

cli_loop(NodeId, NodoPid) ->
  io:format("~nCLI << Elegir un comando:~n"),
  io:format("1. id_nodo~n"),
  io:format("2. listar_mis_archivos~n"),
  io:format("3. salir~n"),
  case io:get_line("") of
    "1\n" ->
      io:format("El ID del nodo es: ~s~n", [NodeId]),
      cli_loop(NodeId, NodoPid);
    "2\n" ->
      case file:list_dir("compartida") of
        {ok, Files} ->
          io:format("Archivos compartidos:~n"),
          lists:foreach(fun(F) -> io:format(" - ~s~n", [F]) end, Files);
        {error, Reason} ->
          io:format("Error al leer 'compartida': ~p~n", [Reason])
      end,
      cli_loop(NodeId, NodoPid);
    "3\n" ->
      io:format("Cerrando nodo...~n"),
      NodoPid ! stop,
      ok;
    _ ->
      io:format("Comando no reconocido. Ingrese 1, 2 o 3. ~n"),
      cli_loop(NodeId, NodoPid)
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
    stop ->
      io:format("Cerrando la CLI...~n"),
      gen_udp:close(Socket),
      exit(normal);
    {udp, Socket, Ip, _Port, Msg} ->
      MsgStr = binary_to_list(Msg),
      case string:tokens(MsgStr, " \n") of
        ["GET_ID"] ->
        Response = <<"ID ", MyId/binary, "\n">>,
        gen_udp:send(Socket, Ip,  _Port, Response),
        loop(Socket, MyId, MyRequestedIds, KnownNodes);
        ["HELLO", NodeId, PortStr] ->
          case NodeId =:= binary_to_list(MyId) of
            true ->
              loop(Socket, MyId, MyRequestedIds, KnownNodes);
            false ->
              Port = list_to_integer(PortStr),
              CurrentTime = os:system_time(second),
                NodeInfo = #{
                    ip => list_to_binary(inet:ntoa(Ip)),
                    port => Port,
                    last_seen => CurrentTime
                },
              ActiveNodes = maps:put(list_to_binary(NodeId), NodeInfo, KnownNodes),
              nodes_registry:save(ActiveNodes),
              io:format("Se recibió HELLO de ~s en ~p:~p~n", [NodeId, Ip, Port]),
              loop(Socket, MyId, MyRequestedIds, ActiveNodes)
          end;
         ["GET_FILES"] ->
              FileListBinary = list_to_binary(string:join(shared_files(), ",")),
              Response = << FileListBinary/binary>>,
              gen_udp:send(Socket, Ip, _Port, Response),
              loop(Socket, MyId, MyRequestedIds, KnownNodes);
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
    
      after 5000 ->
      ActiveNodes = remove_inactive_nodes(KnownNodes, 45),
      loop(Socket, MyId, MyRequestedIds, ActiveNodes)
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

remove_inactive_nodes(Nodes, Timeout) ->
  CurrentTime = os:system_time(second),
  maps:filter(fun(_NodeId, #{last_seen := LastSeen}) ->CurrentTime - LastSeen =< Timeout end,Nodes).
