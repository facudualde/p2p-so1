-module(prueba).
-export([start/0, loop/0, download/2, resolve_node/1]).
start() ->
    io:format(">~n"),
    loop().

loop() ->
    io:format("> "),
    case io:get_line("") of
        eof ->
            io:format("Fin de entrada.~n"),
            ok;
        Line ->
            Command = string:trim(Line),
            Tokens = string:tokens(Command, " "),
            case Tokens of
                ["descargar", Archivo, NodoId] ->
                    io:format("Iniciando descarga: archivo=~s, nodo=~s~n", [Archivo, NodoId]),
                    spawn(?MODULE, download, [Archivo, NodoId]),
                    loop();
                ["salir"] ->
                    io:format("¡Hasta luego!~n"),
                    ok;
                "id_nodo" ->
                    io:format("Comando 'start' ejecutado~n"),
                    loop();
                "listar_mis_archivos" ->
                    io:format("Comando 'stop' ejecutado~n"),
                    loop();
                "status" ->
                    io:format("Comando 'status' ejecutado~n"),
                    loop();
                _ ->
                    io:format("Comando desconocido: ~s~n", [Command]),
                    loop()
            end
    end.


%% Simulador: resuelve un nodo_id a {IP, Puerto}.
resolve_node("nodo123") -> {ok, {"127.0.0.1", 2345}};
resolve_node(_) -> {error, nodo_desconocido}.

%% Proceso que abre la conexión y manda el DOWNLOAD_REQUEST
download(Archivo, NodoId) ->
    case resolve_node(NodoId) of
        {ok, {IpStr, Port}} ->
            io:format("Conectando a ~s:~p~n", [IpStr, Port]),
            case gen_tcp:connect(IpStr, Port, [binary, {packet, 0}]) of
                {ok, Socket} ->
                    Request = io_lib:format("DOWNLOAD_REQUEST ~s~n", [Archivo]),
                    ok = gen_tcp:send(Socket, lists:flatten(Request)),
                    receive_response(Socket),
                    gen_tcp:close(Socket),
                    io:format("Conexión cerrada~n"),
                    ok;
                {error, Reason} ->
                    io:format("Error conectando al nodo: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Nodo desconocido: ~p~n", [Reason])
    end.

%% Recibir respuesta del nodo
receive_response(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            io:format("Respuesta recibida: ~p~n", [Data]),
            receive_response(Socket);
        {error, timeout} ->
            io:format("Timeout esperando respuesta~n"),
            ok;
        {error, closed} ->
            io:format("Conexión cerrada por el nodo remoto~n"),
            ok
    end.
