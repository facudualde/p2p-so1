-module(main).
-export([shared_files/0,
    get_random_id/0,
    send_hello/3,
    udp_loop/4,
    tcp_loop/2,
    remove_inactive_nodes/2,
    start/0,
    get_valid_id/2,
    handle_get_files/4,
    handle_get_id/5,
    handle_hello/6,
    
    handle_name_request/7,
    client_loop/2
]).
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

udp_loop(Socket, MyId, MyRequestedIds, KnownNodes) ->
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
        udp_loop(Socket, MyId, MyRequestedIds, KnownNodes);
        ["HELLO", NodeId, PortStr] ->
          case NodeId =:= binary_to_list(MyId) of
            true ->
              udp_loop(Socket, MyId, MyRequestedIds, KnownNodes);
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
              udp_loop(Socket, MyId, MyRequestedIds, ActiveNodes)
          end;
         ["GET_FILES"] ->
              FileListBinary = list_to_binary(string:join(shared_files(), ",")),
              Response = << FileListBinary/binary>>,
              gen_udp:send(Socket, Ip, _Port, Response),
              udp_loop(Socket, MyId, MyRequestedIds, KnownNodes);
        ["NAME_REQUEST", ReqId] ->
          case ReqId =:= binary_to_list(MyId) orelse lists:member(ReqId, MyRequestedIds) of
            true ->
              gen_udp:send(Socket, Ip, ?UDP_PORT, <<"INVALID_NAME ", ReqId/binary, "\n">>),
              io:format("Enviado INVALID_NAME a ~p por ID repetido: ~s~n", [Ip, ReqId]),
              udp_loop(Socket, MyId, MyRequestedIds, KnownNodes);
            false ->
              io:format("NAME_REQUEST recibido de ~p con ID ~s~n", [Ip, ReqId]),
              udp_loop(Socket, MyId, MyRequestedIds, KnownNodes)
          end;

        _Other ->
          io:format("Mensaje no reconocido: ~s~n", [MsgStr]),
          udp_loop(Socket, MyId, MyRequestedIds, KnownNodes)
      end
    
      after 5000 ->
      ActiveNodes = remove_inactive_nodes(KnownNodes, 45),
      udp_loop(Socket, MyId, MyRequestedIds, ActiveNodes)
  end.
handle_get_id(Socket, Ip, Port, MyId, KnownNodes) ->
    Response = <<"ID ", MyId/binary, "\n">>,
    gen_udp:send(Socket, Ip, Port, Response),
    io:format("Respondido ID a ~p:~p~n", [Ip, Port]),
    KnownNodes.

handle_hello(_Socket, Ip, PortStr, NodeId, MyId, KnownNodes) ->
    io:format("Comparando NodeId ~p con MyId ~p~n", [NodeId, MyId]),
    case NodeId =:= binary_to_list(MyId) of
        true ->
            KnownNodes;
        false ->
            Port = list_to_integer(PortStr),
            CurrentTime = os:system_time(second),
            NodeInfo = #{
                ip => list_to_binary(inet:ntoa(Ip)),
                port => Port,
                last_seen => CurrentTime
            },
            ActiveNodes = maps:put(list_to_binary(NodeId), NodeInfo, KnownNodes),
            %% Commented out nodes_registry:save/1 to avoid undefined function error
            nodes_registry:save(ActiveNodes),
            io:format("Se recibió HELLO de ~s en ~p:~p~n", [NodeId, Ip, Port]),
            ActiveNodes
    end.


handle_get_files(Socket, Ip, Port, KnownNodes) ->
    FileListBinary = list_to_binary(string:join(shared_files(), ",")),
    gen_udp:send(Socket, Ip, Port, FileListBinary),
    io:format("Enviado GET_FILES a ~p:~p~n", [Ip, Port]),
    KnownNodes.
handle_name_request(Socket, Ip, Port, ReqId, MyId, MyRequestedIds, KnownNodes) ->
    case ReqId =:= binary_to_list(MyId) orelse lists:member(ReqId, MyRequestedIds) of
        true ->
            gen_udp:send(Socket, Ip, Port, <<"INVALID_NAME ", (list_to_binary(ReqId))/binary, "\n">>),
            io:format("Enviado INVALID_NAME a ~p por ID repetido: ~s~n", [Ip, ReqId]),
            KnownNodes;
        false ->
            io:format("NAME_REQUEST recibido de ~p con ID ~s~n", [Ip, ReqId]),
            KnownNodes
    end.


remove_inactive_nodes(KnownNodes, Timeout) ->
    CurrentTime = os:system_time(second),
    maps:filter(
        fun(_NodeId, #{last_seen := LastSeen}) ->
            CurrentTime - LastSeen =< Timeout
        end,
        KnownNodes
    ).

shared_files() ->
  case file:list_dir("compartida") of
    {ok, Filenames} ->
      io:fwrite("Archivos compartidos: ~p ~n", [Filenames]),
      Filenames;
    {error, Reason} ->
      io:fwrite("Error al leer carpeta compartida: ~p ~n", [Reason]),
      []
  end.





start() ->
    io:format("Iniciando nodo...~n"),
    
    case gen_udp:open(?UDP_PORT, [binary, {active, true}, {reuseaddr, true}, {broadcast, true}, {ip, {0, 0, 0, 0}}]) of
        {ok, UDPSocket} ->
            Id = get_valid_id(UDPSocket, []),
            io:format("Nodo iniciado con ID: ~s~n", [Id]),
            KnownNodesFromFile = nodes_registry:load(),
            spawn(?MODULE, send_hello, [UDPSocket, Id, ?TCP_PORT]),
            CurrentTime = os:system_time(second),
            KnownNodesSaved = maps:map(fun(_Key, Val) ->
                Val#{last_seen => CurrentTime}
            end, KnownNodesFromFile),
            udp_loop( UDPSocket, Id, [binary_to_list(Id)], KnownNodesSaved),
            case gen_tcp:listen(?TCP_PORT, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]) of
                {ok, TcpSocket} ->
                   spawn(?MODULE, tcp_loop, [TcpSocket, Id]) ;

                {error, Reason} ->
                    io:format("Error al abrir el socket TCP: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Error al abrir el socket UDP: ~p~n", [Reason])
    end.

client_loop(ClientSocket, MyId) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
            % Procesar el mensaje recibido
            Command = string:strip(binary_to_list(Data), right, $\n),
            io:format("Recibido del cliente: ~s~n", [Command]),
            % Manejar el comando recibido
            case Command of
                "id_nodo" ->
                    io:format("ID del nodo: ~s~n", [MyId]),
                    client_loop(ClientSocket, MyId);
                "listar_mis_archivos" ->
                    io:format("Archivos compartidos: ~p~n", [shared_files()]),
                    client_loop(ClientSocket, MyId);
                "salir" ->
                    io:format("Cliente solicitó desconexión.~n"),
                    gen_tcp:close(ClientSocket);
                _ ->
                    io:format("Comando no reconocido: ~s~n", [Command]),
                    client_loop(ClientSocket, MyId)
            end;
        {error, closed} ->
            io:format("Conexión cerrada por el cliente.~n"),
            ok;
        {error, Reason} ->
            io:format("Error al recibir datos del cliente: ~p~n", [Reason]),
            ok
    end.

















tcp_loop(ServerSocket, MyId) ->
  case gen_tcp:accept(ServerSocket) of
    {ok, ClientSocket} ->
      io:format("Cliente conectado~n"),
      spawn(?MODULE, client_loop, [ClientSocket, MyId]),
      tcp_loop(ServerSocket, MyId);
    {error, Reason} ->
      io:format("Error en el socket TCP: ~p~n", [Reason]),
      gen_tcp:close(ServerSocket)  % Cierra el socket si ocurre un error crítico
  end.
