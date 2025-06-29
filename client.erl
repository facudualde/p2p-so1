-module(client).
-export([descargar_archivo/3]).

-define(DEFAULT_CHUNK_SIZE, 1048576).
-define(STATUS_OK, 101).
-define(STATUS_CHUNK, 111).
-define(STATUS_FILE_NOT_FOUND, 112).
-define(STATUS_OPEN_FAILED, 113).
-define(STATUS_READ_FAILED, 114).
-define(STATUS_BAD_REQUEST, 115).

descargar_archivo(IP, Puerto, NombreArchivo) ->
  case gen_tcp:connect(IP, Puerto, [binary, {active, false}]) of
    {ok, Socket} ->
      Request = <<"DOWNLOAD_REQUEST ", NombreArchivo/binary, "\n">>,
      ok = gen_tcp:send(Socket, Request),
      recibir_archivo(Socket, NombreArchivo, <<>>);
    {error, Reason} ->
      io:format("No se pudo conectar a ~p:~p - ~p~n", [IP, Puerto, Reason]),
      {error, Reason}
  end.

recibir_archivo(Socket, NombreArchivo, Acc) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, <<?STATUS_OK:8, FileSize:32/integer-unsigned-big, FileContent/binary>>} ->
      guardar_archivo(NombreArchivo, FileContent),
      io:format("El archivo ~s fue recibido. Tiene ~p bytes.~n", [NombreArchivo, FileSize]),
      ok;

    {ok, <<?STATUS_CHUNK:8, ChunkIndex:16/integer-unsigned-big, ChunkSize:32/integer-unsigned-big, ChunkData/binary>>} ->
      io:format("Fue recibido un chunk ~p de tamaño ~p~n", [ChunkIndex, ChunkSize]),
      recibir_archivo(Socket, NombreArchivo, <<Acc/binary, ChunkData/binary>>);

    {ok, <<?STATUS_FILE_NOT_FOUND:8>>} ->
      io:format("Error: archivo no encontrado~n"),
      {error, file_not_found};

    {ok, <<?STATUS_OPEN_FAILED:8>>} ->
      io:format("Error: no se pudo abrir el archivo~n"),
      {error, open_failed};

    {ok, <<?STATUS_READ_FAILED:8>>} ->
      io:format("Error: no se pudo leer el archivo~n"),
      {error, read_failed};

    {ok, <<?STATUS_BAD_REQUEST:8>>} ->
      io:format("Error: pedido mal formado~n"),
      {error, bad_request};

    {error, closed} ->
      guardar_archivo(NombreArchivo, Acc),
      io:format("Archivo grande ~s guardado correctamente.~n", [NombreArchivo]),
      ok;

    {error, Reason} ->
      io:format("Error en recv: ~p~n", [Reason]),
      {error, Reason}
  end.

guardar_archivo(NombreArchivo, Binario) ->
  Dir = "descargados/",
  filelib:ensure_dir(Dir),
  Nombre = binary_to_list(NombreArchivo),
  Path = filename:join(Dir, Nombre),
  case file:write_file(Path, Binario) of
    ok ->
      io:format("El archivo fue guardado en ~s~n", [Path]);
    {error, Reason} ->
      io:format("Error al guardar el archivo: ~p~n", [Reason])
  end.
