-module(test).

-export([shared_files/0, get_random_id/0, tcp_loop/1, receive_all/2,receive_big_file/4,
         collect_tcp_responses/1, descargar_tcp/2, file_match/1, start_node/0, send_hello/3,
         handle_tcp_request/1, receive_file/2, process_line/1, loop/4, remove_inactive_nodes/2,
         cli_loop/2, buscar_tcp/3, receive_small_file/3, receive_chunks/5,
         handle_file_error/1]).

-define(UDP_PORT, 12346).
-define(TCP_PORT, 12345).
-define(DEFAULT_CHUNK_SIZE, 1048576). 
-define(FOUR_MB, 4 * 1024 * 1024).
-define(STATUS_OK, 101).
-define(STATUS_CHUNK, 111).
-define(STATUS_FILE_NOT_FOUND, 112).
-define(STATUS_OPEN_FAILED, 113).
-define(STATUS_READ_FAILED, 114).
-define(STATUS_BAD_REQUEST, 115).

-include_lib("kernel/include/file.hrl").

get_random_id() ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  lists:foldl(fun(_, Acc) ->
                 [lists:nth(
                    rand:uniform(length(AllowedChars)), AllowedChars)]
                 ++ Acc
              end,
              [],
              lists:seq(1, 4)).

start_node() ->
  rand:seed(exsplus, os:timestamp()),
  case gen_udp:open(?UDP_PORT,
                    [binary,
                     {active, true},
                     {reuseaddr, true},
                     {broadcast, true},
                     {ip, {0, 0, 0, 0}}])
  of
    {ok, UdpSocket} ->
      case gen_tcp:listen(?TCP_PORT, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, TcpSocket} ->
          Id = get_valid_id(UdpSocket, []),
          io:format("El ID del nodo es: ~p~n", [Id]),
          spawn(fun() -> cli_loop(Id, self()) end),
          spawn(fun() -> tcp_loop(TcpSocket) end),
          KnownNodesFromFile = nodes_registry:load(),
          CurrentTime = os:system_time(second),
          KnownNodesSaved =
            maps:map(fun(_Key, Val) -> Val#{last_seen => CurrentTime} end, KnownNodesFromFile),
          register(hello_sender, spawn(fun() -> send_hello(UdpSocket, Id, ?TCP_PORT) end)),
          loop(UdpSocket, Id, [binary_to_list(Id)], KnownNodesSaved);
        {error, Reason} ->
          io:format("Error al iniciar el servidor TCP: ~p~n", [Reason]),
          {error, Reason}
      end;
    {error, Reason} ->
      io:format("Error al iniciar el servidor UDP: ~p~n", [Reason]),
      {error, Reason}
  end.

file_match(Pattern) ->
  case file:list_dir("compartida") of
    {ok, Files} ->
      Tokens = string:tokens(Pattern, "."),
      case Tokens of
        [Name, Extension] ->
          io:format("Tokens: ~p | Name: ~s | Extension: ~s ~n", [Tokens, Name, Extension]),
          case Name of
            "*" ->
              Cons =
                lists:filter(fun(Filename) ->
                                Ext = filename:extension(Filename),
                                Ext =:= "." ++ Extension
                             end,
                             Files),
              io:format("Archivos compartidos: ~p ~n", [Cons]),
              Cons;
            _ ->
              lists:filter(fun(Filename) -> Filename == Pattern end, Files)
          end;
        _ ->
          io:format("Patrón inválido: ~s~n", [Pattern]),
          []
      end;
    {error, Reason} ->
      io:format("Error al listar directorio: ~p~n", [Reason]),
      []
  end.

cli_loop(NodeId, NodoPid) ->
  io:format("~nCLI << Elegir un comando:~n"),
  io:format("1. id_nodo~n"),
  io:format("2. listar_mis_archivos~n"),

  io:format("3. buscar_por_tcp~n"),
  io:format("4. Descargar archivo~n"),

  io:format("5. salir~n"),
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
      PatternLine = io:get_line("Buscar por TCP (nombre o patrón): "),
      Pattern = string:trim(PatternLine),
      buscar_tcp(NodeId, Pattern, NodoPid),
      cli_loop(NodeId, NodoPid);
    "4\n" ->
      FileNameLine = io:get_line("Ingrese el nombre del archivo a descargar: "),
      descargar_tcp(NodeId, string:trim(FileNameLine)),
      cli_loop(NodeId, NodoPid);
    "5\n" ->
      io:format("Cerrando CLI...~n"),
      NodoPid ! stop,
      hello_sender ! stop,
      timer:sleep(1),
      init:stop();
    _ ->
      io:format("Comando no reconocido. Ingrese 1, 2, 3, 4 o 5. ~n"),
      cli_loop(NodeId, NodoPid)
  end.

descargar_tcp(_NodeId, FileName) ->
  KnownNodes = nodes_registry:load(),
  maps:foreach(fun(_Id, #{<<"ip">> := IpBin}) ->
                  io:format("Solicitando archivo ~s al nodo ~s~n", [FileName, IpBin]),
                  spawn(fun() ->
                           case gen_tcp:connect(binary_to_list(IpBin),
                                                ?TCP_PORT,
                                                [binary, {active, false}])
                           of
                             {ok, Socket} ->
                               Msg =
                                 <<"DOWNLOAD_REQUEST ", (list_to_binary(FileName))/binary, "\n">>,
                               gen_tcp:send(Socket, Msg),
                               receive_file(Socket, FileName),
                               gen_tcp:close(Socket);
                             {error, Reason} ->
                               io:format("No se pudo conectar a ~s:~p - ~p~n",
                                         [IpBin, ?TCP_PORT, Reason])
                           end
                        end)
               end,
               KnownNodes).

tcp_loop(TcpSocket) ->
  case gen_tcp:accept(TcpSocket) of
    {ok, ClientSocket} ->
      io:format("Cliente conectado para búsqueda TCP~n"),
      spawn(fun() -> handle_tcp_request(ClientSocket) end),
      tcp_loop(TcpSocket);
    {error, Reason} ->
      io:format("Error aceptando conexión: ~p~n", [Reason])
  end.

handle_tcp_request(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Msg = lists:delete($\n, binary_to_list(Data)),
      Tokens = string:tokens(Msg, " "),
      case Tokens of
        ["SEARCH_REQUEST", NodoID, Pattern] ->
          io:format("Recibí SEARCH_REQUEST de ~s: patrón ~s~n", [NodoID, Pattern]),
          Files = file_match(Pattern),
          lists:foreach(fun(File) ->
                           case file:read_file_info(
                                  filename:join("compartida", File))
                           of
                             {ok, FileInfo} ->
                               Size = FileInfo#file_info.size,
                               Response =
                                 io_lib:format("SEARCH_RESPONSE ~s ~s ~p~n", [NodoID, File, Size]),
                               gen_tcp:send(Socket, list_to_binary(Response));
                             _ -> ok
                           end
                        end,
                        Files),
          gen_tcp:close(Socket);
        ["DOWNLOAD_REQUEST", FileName] ->
          try
            FileInfo = find_file(FileName),
            io:format("File requested: ~p~n", [FileName]),
            send_file(Socket, FileInfo)
          catch
            error:{file_not_found, Reason} ->
              handle_error(Socket, ?STATUS_FILE_NOT_FOUND, Reason);
            error:{open_failed, Reason} ->
              handle_error(Socket, ?STATUS_OPEN_FAILED, Reason);
            error:{read_failed, Reason} ->
              handle_error(Socket, ?STATUS_READ_FAILED, Reason)
          end;
        _ ->
          io:format("Solicitud no reconocida: ~s~n", [Msg]),
          handle_error(Socket, ?STATUS_BAD_REQUEST, "Bad request"),
          gen_tcp:close(Socket)
      end;
    {error, closed} ->
      io:format("Conexión cerrada por el cliente~n"),
      ok;
    {error, timeout} ->
      io:format("Tiempo de espera agotado~n"),
      ok
  end.

buscar_tcp(NodeId, Pattern, _NodoPid) ->
  KnownNodes = nodes_registry:load(),
  Self = self(),

  maps:foreach(fun(_Id, #{<<"ip">> := IpBin}) ->
                  io:format("Buscando en nodo ~s con patrón ~s~n", [IpBin, Pattern]),
                  spawn(fun() ->
                           case gen_tcp:connect(binary_to_list(IpBin),
                                                ?TCP_PORT,
                                                [binary, {active, false}])
                           of
                             {ok, Socket} ->
                               Msg =
                                 <<"SEARCH_REQUEST ",
                                   NodeId/binary,
                                   " ",
                                   (list_to_binary(Pattern))/binary,
                                   "\n">>,
                               gen_tcp:send(Socket, Msg),
                               receive_all(Socket, Self),
                               gen_tcp:close(Socket);
                             {error, Reason} ->
                               io:format("No se pudo conectar a ~s:~p - ~p~n",
                                         [IpBin, ?TCP_PORT, Reason])
                           end
                        end)
               end,
               KnownNodes),
  collect_tcp_responses(5000).

receive_all(Socket, Self) ->
  case gen_tcp:recv(Socket, 0, 3000) of
    {ok, Data} ->
      Self ! {search_response_tcp, Data},
      receive_all(Socket, Self);
    {error, timeout} ->
      ok;
    {error, closed} ->
      ok
  end.

collect_tcp_responses(Timeout) ->
  receive
    {search_response_tcp, Msg} ->
      MsgStr = binary_to_list(Msg),
      Lines = string:tokens(MsgStr, "\n"),
      lists:foreach(fun(Line) -> process_line(Line) end, Lines),
      collect_tcp_responses(Timeout)
  after Timeout ->
    io:format("Fin de recolección de respuestas por TCP.~n")
  end.

process_line(Line) ->
  case string:tokens(Line, " ") of
    ["SEARCH_RESPONSE", NodoID, File, SizeStr] ->
      io:format("Archivo ~s de nodo ~s, tamaño ~s bytes~n", [File, NodoID, SizeStr]);
    _ ->
      io:format("Respuesta no reconocida: ~s~n", [Line])
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
  gen_udp:send(Socket, {255, 255, 255, 255}, ?UDP_PORT, Msg).

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
  receive
    stop ->
      io:format("Deteniendo envío de HELLO~n"),
      exit(normal)
  after 0 ->
    Mesg = <<"HELLO ", Id/binary, " ", (integer_to_binary(Port))/binary, "\n">>,
    gen_udp:send(Socket, {255, 255, 255, 255}, ?UDP_PORT, Mesg),
    timer:sleep(15000 + rand:uniform(5000)),
    send_hello(Socket, Id, Port)
  end.

loop(Socket, MyId, MyRequestedIds, KnownNodes) ->
  receive
    stop ->
      io:format("Cerrando la CLI...~n"),
      gen_udp:close(Socket),
      exit(normal);
    {udp, _Socket, Ip, _Port, Msg} ->
      MsgStr = binary_to_list(Msg),
      case string:tokens(MsgStr, " \n") of
        ["HELLO", NodeId, PortStr] ->
          case NodeId =:= binary_to_list(MyId) of
            true ->
              loop(Socket, MyId, MyRequestedIds, KnownNodes);
            false ->
              Port = list_to_integer(PortStr),
              CurrentTime = os:system_time(second),
              NodeInfo =
                #{<<"ip">> => list_to_binary(inet:ntoa(Ip)),
                  <<"port">> => Port,
                  last_seen => CurrentTime},
              ActiveNodes = maps:put(list_to_binary(NodeId), NodeInfo, KnownNodes),
              nodes_registry:save(ActiveNodes),
              io:format("Se recibió HELLO de ~s en ~p:~p~n", [NodeId, Ip, Port]),
              loop(Socket, MyId, MyRequestedIds, ActiveNodes)
          end;
        ["NAME_REQUEST", ReqId] ->
          case ReqId =:= binary_to_list(MyId) orelse lists:member(ReqId, MyRequestedIds) of
            true ->
              InvalidMsg = <<"INVALID_NAME ", (list_to_binary(ReqId))/binary, "\n">>,
              gen_udp:send(Socket, Ip, ?UDP_PORT, InvalidMsg),
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
  maps:filter(fun(_NodeId, #{last_seen := LastSeen}) -> CurrentTime - LastSeen =< Timeout
              end,
              Nodes).

% Funciones para enviar archivos
big_file(ClientSocket, FD, ChunkIndex) ->
  case file:read(FD, ?DEFAULT_CHUNK_SIZE) of
    eof ->
      ok;
    {ok, FileContent} ->
      ContentSize = byte_size(FileContent),
      Payload =
        if ContentSize == ?DEFAULT_CHUNK_SIZE ->
             <<?STATUS_CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big,
               FileContent:?DEFAULT_CHUNK_SIZE/binary>>;
           true ->
             <<?STATUS_CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ContentSize:32/integer-unsigned-big,
               FileContent:ContentSize/binary>>
        end,
      gen_tcp:send(ClientSocket, Payload),
      big_file(ClientSocket, FD, ChunkIndex + 1);
    {error, Reason} ->
      error({read_failed, Reason})
  end.

small_file(ClientSocket, FD, FileSize) ->
  case file:read(FD, FileSize) of
    eof ->
      ok;
    {ok, FileContent} ->
      Payload =
        <<?STATUS_OK:8/integer-unsigned-big,
          FileSize:32/integer-unsigned-big,
          FileContent/binary>>,
      gen_tcp:send(ClientSocket, Payload);
    {error, Reason} ->
      error({read_failed, Reason})
  end.

send_file(ClientSocket, {FilePath, FileSize}) ->
  case file:open(FilePath, [read, binary]) of
    {ok, FD} ->
      try
        if FileSize =< ?FOUR_MB ->
             small_file(ClientSocket, FD, FileSize);
           true ->
             Payload =
               <<?STATUS_OK:8/integer-unsigned-big,
                 FileSize:32/integer-unsigned-big,
                 ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big>>,
             gen_tcp:send(ClientSocket, Payload),
             big_file(ClientSocket, FD, 0)
        end
      after
        file:close(FD),
        io:format("Cerrando socket después de enviar archivo~n"),
        gen_tcp:close(ClientSocket)
      end;
    {error, Reason} ->
      io:format("Error al abrir archivo: ~p~n", [Reason]),
      error({open_failed, Reason})
  end.

send_error_response(ClientSocket, StatusCode) ->
  gen_tcp:send(ClientSocket, <<StatusCode:8/big-unsigned-integer>>).

find_file(FileName) ->
  FilePath = filename:join("compartida", FileName),
  io:format("Buscando archivo en: ~s~n", [FilePath]),
  case file:read_file_info(FilePath) of
    {ok, FileInfo} ->
      {FilePath, FileInfo#file_info.size};
    {error, Reason} ->
      io:format("Error al buscar archivo: ~s, motivo: ~p~n", [FileName, Reason]),
      error({file_not_found, Reason})
  end.

handle_error(ClientSocket, StatusCode, Reason) ->
  io:format("Error: ~p~n", [Reason]),
  send_error_response(ClientSocket, StatusCode),
  gen_tcp:close(ClientSocket).

% Funciones para recibir archivos
receive_file(Socket, FileName) ->
  io:format("Recibiendo archivo: ~s~n", [FileName]),
  case gen_tcp:recv(Socket, 5, 10000) of
    {ok, <<?STATUS_OK:8, FileSize:32/big-unsigned-integer>>} ->
      io:format("Archivo encontrado. Tamaño: ~p bytes~n", [FileSize]),
      if FileSize =< ?FOUR_MB ->
           receive_small_file(Socket, FileName, FileSize);
         true ->
           case gen_tcp:recv(Socket, 4, 10000) of
             {ok, <<ChunkSize:32/big-unsigned-integer>>} ->
               io:format("Recibiendo archivo grande con chunks de ~p bytes~n", [ChunkSize]),
               receive_big_file(Socket, FileName, FileSize, ChunkSize);
             {error, Reason} ->
               io:format("Error al recibir tamaño de chunk: ~p~n", [Reason])
           end
      end;
    {ok, <<StatusCode:8>>} ->
      handle_file_error(StatusCode);
    {error, Reason} ->
      io:format("Error al recibir respuesta: ~p~n", [Reason])
  end.

receive_small_file(Socket, FileName, FileSize) ->
  case gen_tcp:recv(Socket, FileSize, 10000) of
    {ok, FileContent} ->
      FilePath = filename:join("compartida", FileName),
      case file:write_file(FilePath, FileContent) of
        ok ->
          io:format("Archivo ~s descargado exitosamente (~p bytes)~n", [FileName, FileSize]);
        {error, Reason} ->
          io:format("Error al guardar archivo: ~p~n", [Reason])
      end;
    {error, Reason} ->
      io:format("Error al recibir contenido del archivo: ~p~n", [Reason])
  end.

receive_big_file(Socket, FileName, TotalSize, ChunkSize) ->
  FilePath = filename:join("compartida", FileName),
  case file:open(FilePath, [write, binary]) of
    {ok, FD} ->
      try
        receive_chunks(Socket, FD, 0, TotalSize, ChunkSize),
        io:format("Archivo ~s descargado exitosamente (~p bytes)~n", [FileName, TotalSize])
      after
        file:close(FD)
      end;
    {error, Reason} ->
      io:format("Error al crear archivo: ~p~n", [Reason])
  end.

receive_chunks(Socket, FD, ReceivedBytes, TotalSize, _ChunkSize)
  when ReceivedBytes < TotalSize ->
  case gen_tcp:recv(Socket, 7, 10000) of
    {ok,
     <<?STATUS_CHUNK:8,
       ChunkIndex:16/big-unsigned-integer,
       ActualChunkSize:32/big-unsigned-integer>>} ->
      io:format("Recibiendo chunk ~p de ~p bytes~n", [ChunkIndex, ActualChunkSize]),
      case gen_tcp:recv(Socket, ActualChunkSize, 10000) of
        {ok, ChunkData} ->
          case file:write(FD, ChunkData) of
            ok ->
              NewReceivedBytes = ReceivedBytes + ActualChunkSize,
              io:format("Progreso: ~p/~p bytes (~.1f%)~n",
                        [NewReceivedBytes, TotalSize, NewReceivedBytes / TotalSize * 100]),
              receive_chunks(Socket, FD, NewReceivedBytes, TotalSize, ActualChunkSize);
            {error, Reason} ->
              io:format("Error al escribir chunk: ~p~n", [Reason])
          end;
        {error, Reason} ->
          io:format("Error al recibir datos del chunk: ~p~n", [Reason])
      end;
    {error, Reason} ->
      io:format("Error al recibir header del chunk: ~p~n", [Reason])
  end;
receive_chunks(_Socket, _FD, ReceivedBytes, TotalSize, _ChunkSize) ->
  io:format("Descarga completa: ~p bytes de ~p bytes~n", [ReceivedBytes, TotalSize]).

handle_file_error(StatusCode) ->
  case StatusCode of
    ?STATUS_FILE_NOT_FOUND ->
      io:format("Error: Archivo no encontrado~n");
    ?STATUS_OPEN_FAILED ->
      io:format("Error: No se pudo abrir el archivo~n");
    ?STATUS_READ_FAILED ->
      io:format("Error: Error al leer el archivo~n");
    ?STATUS_BAD_REQUEST ->
      io:format("Error: Solicitud inválida~n");
    _ ->
      io:format("Error desconocido: ~p~n", [StatusCode])
  end.
