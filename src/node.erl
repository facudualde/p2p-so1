
-module(node).

-export([ tcp_loop/1, receive_all/2, receive_big_file/4,
         collect_tcp_responses/1, descargar_tcp/2, file_match/1, run/0, send_hello/3,
         handle_tcp_request/1, receive_file/2, process_line/1, loop/4, remove_inactive_nodes/2,
         cli/1, buscar_tcp/2, receive_small_file/3, receive_chunks/5, handle_file_error/1]).

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
run() ->
  case gen_udp:open(?UDP_PORT, [binary, {active, true}, {reuseaddr, true}, {broadcast, true}, {ip, {0, 0, 0, 0}}]) of
    {ok, UdpSocket} ->
      case gen_tcp:listen(?TCP_PORT, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, TcpSocket} ->
          io:format("Obteniendo un ID válido...~n"),
          Id = get_valid_id(UdpSocket, []),
          register(node, self()),
          spawn(?MODULE, tcp_loop, [TcpSocket]),
          spawn(?MODULE, cli, [Id]),
          KnownNodesFromFile = utils:load_register(),
          CurrentTime = os:system_time(second),
          KnownNodesSaved = maps:map(fun(_Key, Val) -> Val#{last_seen => CurrentTime} end, KnownNodesFromFile),
          register(hello_sender, spawn(fun() -> send_hello(UdpSocket, Id, ?TCP_PORT) end)),
          loop(UdpSocket, Id, [Id], KnownNodesSaved);
        {error, Reason} ->
          io:format("Error al iniciar el servidor TCP: ~p~n", [Reason]),
          {error, Reason}
      end;
    {error, Reason} ->
      io:format("Error al iniciar el servidor UDP: ~p~n", [Reason]),
      {error, Reason}
  end.

%% Maneja errores enviando una respuesta y cerrando el socket.
handle_error(Socket, StatusCode, Reason) ->
  io:format("Error: ~p~n", [Reason]),
  send_error_response(Socket, StatusCode),
  gen_tcp:close(Socket).

%% Busca archivos que coincidan con un patron en el directorio compartido.
file_match(Pattern) ->
  case file:list_dir("compartida") of
    {ok, Files} ->
      Tokens = string:tokens(Pattern, "."),
      case Tokens of
        [Name, Extension] ->
          case Name of
            "*" ->
              Cons = lists:filter(fun(Filename) -> filename:extension(Filename) =:= "." ++ Extension end, Files),
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

%% Interfaz de Línea de Comandos (CLI) para interacción con el nodo.
%% - Proporciona un menú interactivo que permite a los usuarios realizar varias operaciones relacionadas con el nodo.
%%
%%
%% 2. Según el comando ingresado por el usuario:
%%    - **Comando "1"**: 
%%      - Muestra el identificador (`NodeId`) del nodo actual.
%%      - Llama recursivamente a `cli/1` para volver al menú.
%%    - **Comando "2"**: 
%%      - Llama a `utils:shared_files/0` para obtener una lista de los archivos compartidos por el nodo.
%%      - Itera sobre los archivos compartidos y los imprime.
%%      - Llama recursivamente a `cli/1` para volver al menú.
%%    - **Comando "3"**: 
%%      - Solicita al usuario un nombre o patrón de archivo mediante `io:get_line/1`.
%%      - Llama a `buscar_tcp/2` para buscar el patrón en los nodos conocidos.
%%      - Vuelve al menú tras completar la búsqueda.
%%    - **Comando "4"**: 
%%      - Solicita al usuario el nombre de un archivo mediante `io:get_line/1`.
%%      - Llama a `descargar_tcp/2` para intentar descargar el archivo desde nodos conocidos.
%%      - Vuelve al menú tras completar la operación.
%%    - **Comando "5"**: 
%%      - Cierra la CLI y el nodo actual de manera ordenada:
%%        - Guarda el estado de los nodos conocidos usando `utils:save_register/1`.
%%        - Detiene procesos auxiliares (`node`, `hello_sender`).
%%        - Llama a `init:stop/0` para finalizar el programa.
%%    - **Comando no reconocido**: 
%%      - Imprime un mensaje de error y muestra nuevamente el menú.
%%

cli(NodeId) ->
  io:format("~nLista de comandos:~n"),
  io:format("1: ver id~n"),
  io:format("2: listar archivos~n"),
  io:format("3: buscar archivos~n"),
  io:format("4: descargar archivo~n"),
  io:format("5: salir~n"),
  case io:get_line("Comando: ") of
    "1\n" ->
      io:format("El ID del nodo es: ~s~n", [NodeId]),
      cli(NodeId);
    "2\n" ->
      Files = utils:shared_files(),
      io:format("Archivos compartidos:~n"),
      lists:foreach(fun(F) -> io:format(" - ~s~n", [F]) end, Files),
      cli(NodeId);
    "3\n" ->
      PatternLine = io:get_line("Buscar por TCP (nombre o patrón): "),
      Pattern = string:trim(PatternLine),
      buscar_tcp(NodeId, Pattern),
      cli(NodeId);
    "4\n" ->
      FileNameLine = io:get_line("Ingrese el nombre del archivo a descargar: "),
      descargar_tcp(NodeId, string:trim(FileNameLine)),
      cli(NodeId);
    "5\n" ->
      io:format("Cerrando CLI...~n"),
      utils:save_register(#{}),
      node ! stop,
      hello_sender ! stop,
      timer:sleep(1),
      init:stop();
    _ ->
      io:format("Comando no reconocido. Ingrese 1, 2, 3, 4 o 5. ~n"),
      cli(NodeId)
  end.

%% Solicita un archivo por TCP desde nodos conocidos.
%% - Busca en una lista de nodos registrados y solicita el archivo especificado a cada nodo.
%% - Si la conexión TCP es exitosa, envía una solicitud de descarga y guarda el archivo recibido.
%% - En caso de error, registra la falla.
descargar_tcp(_NodeId, FileName) ->
  KnownNodes = utils:load_register(),
  maps:foreach(fun(_Id, #{<<"ip">> := IpBin}) ->
                  io:format("Solicitando archivo ~s al nodo ~s~n", [FileName, IpBin]),
                  spawn(fun() ->
                           case gen_tcp:connect(binary_to_list(IpBin), ?TCP_PORT, [binary, {active, false}]) of
                             {ok, Socket} ->
                               Msg = <<"DOWNLOAD_REQUEST ", (list_to_binary(FileName))/binary, "\n">>,
                               gen_tcp:send(Socket, Msg),
                               receive_file(Socket, FileName),
                               gen_tcp:close(Socket);
                             {error, Reason} ->
                               io:format("No se pudo conectar a ~s:~p - ~p~n", [IpBin, ?TCP_PORT, Reason])
                           end
                        end)
               end,
               KnownNodes).

%% Loop que maneja conexiones TCP entrantes.
%% - Acepta conexiones TCP entrantes en un socket dado.
%% - Para cada cliente conectado, inicia un proceso que maneja las solicitudes del cliente.
%% - Reintenta continuamente aceptar nuevas conexiones.
tcp_loop(TcpSocket) ->
  case gen_tcp:accept(TcpSocket) of
    {ok, ClientSocket} ->
      io:format("Cliente conectado para búsqueda TCP~n"),
      spawn(fun() -> handle_tcp_request(ClientSocket) end),
      tcp_loop(TcpSocket);
    {error, Reason} ->
      io:format("Error aceptando conexión: ~p~n", [Reason])
  end.

%% Maneja las solicitudes recibidas por un socket TCP.
%% - Procesa diferentes tipos de solicitudes:
%%   - "SEARCH_REQUEST": Busca archivos que coincidan con un patrón y responde con los resultados.
%%   - "DOWNLOAD_REQUEST": Envío del archivo solicitado al cliente.
%%   - Maneja errores como solicitudes no reconocidas o fallas en la lectura de archivos.
%% - Cierra el socket al finalizar el procesamiento.

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
                           case file:read_file_info(filename:join("compartida", File)) of
                             {ok, FileInfo} ->
                               Size = FileInfo#file_info.size,
                               Response = io_lib:format("SEARCH_RESPONSE ~s ~s ~p~n", [NodoID, File, Size]),
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
%%% Buscar archivos en nodos conocidos usando TCP.
%% - Esta función permite realizar una búsqueda distribuida de archivos entre los nodos registrados.
%% 
%% Detalles de implementación:
%% 1. Carga la lista de nodos conocidos utilizando `utils:load_register()`, donde cada nodo tiene asociado su dirección IP.
%% 2. Imprime un mensaje indicando el inicio de la búsqueda con el patrón especificado.
%% 3. Itera sobre la lista de nodos conocidos (`KnownNodes`) y para cada nodo:
%%    - Imprime el nodo objetivo y el patrón de búsqueda.
%%    - Inicia un proceso separado para manejar la conexión con ese nodo utilizando `spawn/1`:
%%      - Intenta establecer una conexión TCP con el nodo en el puerto definido por `?TCP_PORT`.
%%      - Si la conexión es exitosa:
%%        - Construye un mensaje `SEARCH_REQUEST` que incluye:
%%          - El identificador del nodo que realiza la solicitud (`NodeId`).
%%          - El patrón de búsqueda (`Pattern`).
%%        - Envía el mensaje al nodo remoto utilizando `gen_tcp:send/2`.
%%        - Invoca la función `receive_all/2` para recibir todas las respuestas del nodo remoto y enviarlas al proceso principal (`Self`).
%%        - Cierra la conexión TCP una vez finalizada la comunicación.
%%      - Si la conexión falla:
%%        - Imprime un mensaje indicando que no se pudo conectar al nodo y registra la causa del error.
%% 4. Una vez que se han enviado todas las solicitudes, invoca la función `collect_tcp_responses/1` con un tiempo límite (`Timeout`).
%%    - Esta función espera respuestas de búsqueda y procesa cada línea recibida para identificar archivos que coincidan con el patrón.
%% 5. Manejo de errores:
%%    - Si no se puede establecer la conexión TCP con algún nodo, la función captura y registra los errores, pero continúa con la búsqueda en los demás nodos.
 

buscar_tcp(NodeId, Pattern) ->
  KnownNodes = utils:load_register(),
  Self = self(),
  io:format("Buscando archivos con patrón ~s en nodos conocidos...~n", [Pattern]),
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


%% Recibe todas las respuestas del socket y las envía al proceso principal.
%% - Lee mensajes del socket asociado en bucles hasta que se agote el tiempo o la conexión se cierre.
%% - Envía cada mensaje recibido al proceso que inició la solicitud.
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
%% Recoge las respuestas de búsqueda recibidas por TCP.
%% - Escucha mensajes con respuestas de búsqueda durante un tiempo límite especificado.
%% - Procesa cada respuesta recibida utilizando una función auxiliar.
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

%% Procesa cada línea de respuesta recibida por TCP.
%% - Analiza las líneas recibidas para identificar respuestas válidas al patrón de búsqueda.
%% - Imprime los detalles de los archivos encontrados o registra respuestas no reconocidas.
process_line(Line) ->
  case string:tokens(Line, " ") of
    ["SEARCH_RESPONSE", NodoID, File, SizeStr] ->
      io:format("Archivo ~s de nodo ~s, tamaño ~s bytes~n", [File, NodoID, SizeStr]);
    _ ->
      io:format("Respuesta no reconocida: ~s~n", [Line])
  end.

%% Obtiene un ID valido para el nodo actual.
get_valid_id(UdpSocket, TriedIds) ->
  Id = utils:get_random_id(),
  case lists:member(Id, TriedIds) of
    true ->
      get_valid_id(UdpSocket, TriedIds);
    false ->
      try
        send_name_request(UdpSocket, Id),
        case wait_invalid_name(UdpSocket, Id) of
          true ->
            RandSleep = 2000 + rand:uniform(8000),
            timer:sleep(RandSleep),
            get_valid_id(UdpSocket, [Id | TriedIds]);
          false ->
            list_to_binary(Id)
        end
      catch
        error:{failed_name_request, Reason} ->
          io:format("Failed sending name request: ~p~n", [Reason]),
          erlang:halt(1)
      end
  end.

%% Envia una solicitud de nombre usando UDP.
send_name_request(UdpSocket, Id) ->
  Msg = <<"NAME_REQUEST ", (list_to_binary(Id))/binary, "\n">>,
  case gen_udp:send(UdpSocket, {255, 255, 255, 255}, ?UDP_PORT, Msg) of
    ok ->
      ok;
    {error, Reason} ->
      error({failed_name_request, Reason})
  end.

%% Espera una respuesta de nombre invalido.
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

%% Envia mensajes HELLO periodicamente para anunciar el nodo.
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

%% Loop principal para manejar mensajes UDP y actualizar nodos conocidos.
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
              utils:save_register(ActiveNodes),
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


%% Elimina nodos inactivos de la lista basada en un tiempo de espera especificado.
remove_inactive_nodes(Nodes, Timeout) ->
  CurrentTime = os:system_time(second),
  maps:filter(fun(_NodeId, #{last_seen := LastSeen}) -> CurrentTime - LastSeen =< Timeout
              end,
              Nodes).

%% Envio de archivos mayores a 4 MB
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

%% Recepcion de archivos
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
      FilePath = filename:join("descargas", FileName),
      io:format("Guardando archivo pequeño en: ~s~n", [FilePath]),
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
  FilePath = filename:join("descargas", FileName),
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
