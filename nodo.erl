-module(nodo).
-export([shared_files/0, get_random_id/0, start_node/2, send_hello/3, loop/4,remove_inactive_nodes/2,start/0,command_loop/2,get_valid_id/2]).
-define(UDP_PORT, 12346).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%CLIENTE AGREGADO TEST

start() ->
    io:format("Iniciando nodo...~n"),
    case gen_udp:open(?UDP_PORT, [binary, {active, true}, {reuseaddr, true}, {broadcast, true}, {ip, {0,0,0,0}}]) of
        {ok, Socket} ->
            Id = get_valid_id(Socket, []),
            io:format("Nodo iniciado con ID: ~s~n", [Id]),
            PidLoop = spawn(fun() -> loop(Socket, Id, [binary_to_list(Id)], #{}) end),
            PidSend = spawn(fun() -> send_hello(Socket, Id, ?UDP_PORT) end),
            spawn(fun() -> cli_loop(Socket, Id, PidLoop) end);

        {error, Reason} ->
            io:format("Error al iniciar el nodo: ~p~n", [Reason])
    end.

command_loop(Socket, Id) ->
    io:format("Comandos disponibles:~n 1. ID_Nodo~n 2. GET_FILES~n 3. EXIT~nCLI> "),
    case io:get_line("") of
        "1\n" ->
            io:format("El ID del Nodo es :~i ~n", [Id]),
            command_loop(Socket, Id);
        "2\n" ->
            io:format("Archivos compartidos:~n"),
            Files = shared_files(),
            io:format("~p~n", [Files]),
            command_loop(Socket, Id);
        "3\n" ->
            io:format("Cerrando nodo...~n"),
            gen_udp:close(Socket),
            io:format("Nodo cerrado.~n");
        _ ->
            io:format("Comando no reconocido. Intente nuevamente.~n"),
            command_loop(Socket, Id)
    end.

start_node(Socket,MyId) ->
    loop(Socket, MyId, [binary_to_list(MyId)], #{}).


loop(Socket, MyId, MyRequestedIds, KnownNodes) ->
    receive
        {udp, Socket, Ip, _Port, Msg} ->
            inet:setopts(Socket, [{active, true}]),
            % Procesar mensaje UDP igual que antes
            ...
            loop(Socket, MyId, MyRequestedIds, KnownNodes);

        {get_id, FromPid} ->
            FromPid ! {id_response, MyId},
            loop(Socket, MyId, MyRequestedIds, KnownNodes);

        {stop} ->
            io:format("Loop detenido.~n"),
            ok;

        Other ->
            io:format("Mensaje desconocido en loop: ~p~n", [Other]),
            loop(Socket, MyId, MyRequestedIds, KnownNodes)
    after 5000 ->
        ActiveNodes = remove_inactive_nodes(KnownNodes, 45),
        loop(Socket, MyId, MyRequestedIds, ActiveNodes)
    end.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
          io:format("Loop iniciado para recibir mensajes...~s ~n", [MsgStr]),

      CurrentTime = erlang:monotonic_time(second),
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
              ActiveNodes = maps:put(NodeId, #{ip => Ip, port => Port,last_seen=>CurrentTime}, KnownNodes),
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
  CurrentTime = erlang:monotonic_time(second),
  maps:filter(fun(_NodeId, #{last_seen := LastSeen}) ->CurrentTime - LastSeen =< Timeout end,Nodes).