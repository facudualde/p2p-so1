-module(nodo).
-export([start_node/0,
    tcp_loop/2,
    cli_loop/2,
    handle_cli_command/2,
    handle_client/1,
    get_random_id/0,
    get_valid_id/2,
    send_name_request/2,
    wait_invalid_name/2,
    send_hello/3,
    loop/4,
handle_error/3,
    shared_files/0,
    remove_inactive_nodes/2]).
  -define(UDP_PORT, 12346).
-define(TCP_PORT, 12345).
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_CHUNK_SIZE, 1048576). % 1MB
-define(FOUR_MB, 4 * 1024 * 1024).
-define(STATUS_OK, 101).
-define(STATUS_CHUNK, 111).
-define(STATUS_FILE_NOT_FOUND, 112).
-define(STATUS_OPEN_FAILED, 113).
-define(STATUS_READ_FAILED, 114).
-define(STATUS_BAD_REQUEST, 115).
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
            case gen_tcp:listen(?TCP_PORT, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]) of
                {ok, TcpSocket} ->
                  
                 Pid= spawn(?MODULE, tcp_loop , [ Id,TcpSocket]),
                  spawn(fun() -> start_cli(Id, Pid) end);
                {error, Reason} ->
                    io:format("Error al abrir el socket TCP: ~p~n", [Reason])
            end,
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

tcp_loop(NodeId, ServerSocket) ->
    receive
        {cli_command, Command, FromPid} ->
            io:format("Comando recibido desde la CLI: ~p~n", [Command]),
            handle_cli_command(Command, FromPid),
            tcp_loop(NodeId, ServerSocket)
    after 0 ->  
        case gen_tcp:accept(ServerSocket) of
            {ok, ClientSocket} ->
                io:format("Cliente conectado: ~p~n", [ClientSocket]),
                spawn(fun() -> handle_client(ClientSocket) end),
                tcp_loop(NodeId, ServerSocket);
            {error, Reason} ->
                io:format("Error en el socket TCP: ~p~n", [Reason]),
                gen_tcp:close(ServerSocket)
        end
    end.

handle_cli_command({list_connections}, FromPid) ->
    FromPid ! {response, "Conexiones activas: Ninguna (esto es un ejemplo)"};
handle_cli_command(_, FromPid) ->
    FromPid ! {error, "Comando no reconocido"}.



start_cli(NodeId, ServerPid) ->
    io:format("CLI iniciada para el nodo ~s. Ingrese un comando:~n", [NodeId]),
    cli_loop(NodeId, ServerPid).

cli_loop(NodeId, ServerPid) ->
    Command = io:get_line(">> "),
    case string:trim(Command) of
        "listar_mis_archivos" ->
            ServerPid ! {cli_command, {list_connections}, self()},
            receive
                {response, Msg} ->
                    io:format("Respuesta del servidor: ~s~n", [Msg]);
                {error, Msg} ->
                    io:format("Error del servidor: ~s~n", [Msg])
            after 5000 ->
                io:format("El servidor no respondió a tiempo.~n")
            end,
            cli_loop(NodeId, ServerPid);
        "salir" ->
            io:format("Cerrando nodo...~n"),
            halt();
        _ ->
            io:format("Comando no reconocido: ~s~n", [Command]),
            cli_loop(NodeId, ServerPid)
    end.

 handle_client(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, RawMsg} ->
            io:format("Raw message received: ~p~n", [RawMsg]),
            Msg = lists:delete($\n, binary_to_list(RawMsg)),
            Tokens = string:tokens(Msg, " "),
            case Tokens of
                ["DOWNLOAD_REQUEST", FileName] ->
                    io:format("File requested: ~p~n", [FileName]),
                    try
                        io:format("File requested: ~p~n", [FileName])
                    catch
                        error:{file_not_found, Reason} ->
                            handle_error(ClientSocket, ?STATUS_FILE_NOT_FOUND, Reason);
                        error:{open_failed, Reason} ->
                            handle_error(ClientSocket, ?STATUS_OPEN_FAILED, Reason);
                        error:{read_failed, Reason} ->
                            handle_error(ClientSocket, ?STATUS_READ_FAILED, Reason)
                    end;
                _ ->
                    handle_error(ClientSocket, ?STATUS_BAD_REQUEST, "Bad request")
            end;
        {error, closed} ->
            io:format("Client disconnected~n"),
            ok
    end.

       



handle_error(ClientSocket, StatusCode, Reason) ->
  io:format("Error: ~p~n", [Reason]),
  send_error_response(ClientSocket, StatusCode),
  handle_client(ClientSocket).

send_error_response(ClientSocket, StatusCode) ->
  gen_tcp:send(ClientSocket, <<StatusCode:8/big-unsigned-integer>>).



















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