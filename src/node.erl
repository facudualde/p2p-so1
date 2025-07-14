-module(node).

-export([print_search_response/2, send_search_request/4, handle_client/1, run/0,
         invalid_name/3, get_id/2, send_name_request/2, join_network/2, hello/2,
         remove_inactive_nodes/0, cli/1, create_tcp_server/0, run_tcp_server/2, download/3,
         send_small_file/3, receive_small_file/3, send_download_request/2]).

-define(DOWNLOADS_PATH, "downloads").
-define(SHARED_PATH, "shared").
-define(UDP_PORT, 12346).
-define(TCP_PORT, 12345).
-define(DEFAULT_CHUNK_SIZE, 1048576).
-define(FOUR_MB, 4 * 1024 * 1024).
-define(STATUS_OK, 101).
-define(STATUS_CHUNK, 111).
% -define(STATUS_FILE_NOT_FOUND, 112).
% -define(STATUS_OPEN_FAILED, 113).
% -define(STATUS_READ_FAILED, 114).
% -define(STATUS_BAD_REQUEST, 115).
-define(TIMEOUT_INVALID_NAME, 10000).
-define(TIMEOUT_SEARCH_RESPONSE, 3000).

-include_lib("kernel/include/file.hrl").

invalid_name(UdpSocket, Id, Timeout) ->
  Start = erlang:monotonic_time(millisecond),
  receive
    {udp, _Socket, _Ip, _Port, Res} ->
      Msg = binary_to_list(Res),
      Tokens = string:tokens(Msg, " \n"),
      case Tokens of
        ["INVALID_NAME", _ReqId] ->
          End = erlang:monotonic_time(millisecond) - Start,
          Remaining = Timeout - End,
          invalid_name(UdpSocket, Id, Remaining);
        _ ->
          End = erlang:monotonic_time(millisecond) - Start,
          Remaining = Timeout - End,
          invalid_name(UdpSocket, Id, Remaining)
      end
  after Timeout ->
    false
  end.

send_name_request(UdpSocket, Id) ->
  Msg = list_to_binary("NAME_REQUEST " ++ Id),
  case gen_udp:send(UdpSocket, {255, 255, 255, 255}, ?UDP_PORT, Msg) of
    ok ->
      ok;
    {error, Reason} ->
      error({name_request_failed, Reason})
  end.

get_id(UdpSocket, InvalidIds) ->
  Id = utils:get_random_id(),
  case lists:member(Id, InvalidIds) of
    true ->
      get_id(UdpSocket, InvalidIds);
    false ->
      Id
  end.

join_network(UdpSocket, InvalidIds) ->
  Id = get_id(UdpSocket, InvalidIds),
  send_name_request(UdpSocket, Id),
  case invalid_name(UdpSocket, Id, ?TIMEOUT_INVALID_NAME) of
    true ->
      WaitTime = 2000 + rand:uniform(8000),
      receive after WaitTime ->
        join_network(UdpSocket, [Id | InvalidIds])
      end;
    false ->
      {Id, InvalidIds}
  end.

wait([]) ->
  ok;
wait(Refs) ->
  receive
    {'DOWN', Ref, process, _Pid, _Reason} ->
      wait(lists:delete(Ref, Refs))
  end.

clean(UdpSocket, TcpSocket, Refs) ->
  hello ! stop,
  remove_inactive_nodes ! stop,
  gen_tcp:close(TcpSocket),
  wait(Refs),
  gen_udp:close(UdpSocket),
  utils:reset_register(),
  ok.

loop(UdpSocket, TcpSocket, Id, InvalidIds, Refs) ->
  receive
    stop ->
      clean(UdpSocket, TcpSocket, Refs),
      init:stop();
    {udp, _Socket, Ip, _Port, Req} ->
      Msg = binary_to_list(Req),
      Tokens = string:tokens(Msg, " \n"),
      case Tokens of
        ["HELLO", NodeId, NodePort] ->
          if NodeId =:= Id ->
               loop(UdpSocket, TcpSocket, Id, InvalidIds, Refs);
             true ->
               Node =
                 #{list_to_binary("port") => NodePort,
                   list_to_binary("ip") => list_to_binary(inet:ntoa(Ip)),
                   list_to_binary("last_seen") => erlang:monotonic_time(seconds)},
               ActiveNodes = maps:put(list_to_binary(NodeId), Node, utils:load_register()),
               utils:save_register(ActiveNodes),
               loop(UdpSocket, TcpSocket, Id, InvalidIds, Refs)
          end;
        ["NAME_REQUEST", ReqId] ->
          case ReqId =:= Id orelse lists:member(ReqId, InvalidIds) of
            true ->
              InvalidMsg = list_to_binary("INVALID_NAME " ++ ReqId ++ "\n"),
              case gen_udp:send(UdpSocket, Ip, ?UDP_PORT, InvalidMsg) of
                ok ->
                  loop(UdpSocket, TcpSocket, Id, InvalidIds, Refs);
                {error, Reason} ->
                  error({udp_send_failed, Reason})
              end;
            false ->
              loop(UdpSocket, TcpSocket, Id, InvalidIds, Refs)
          end;
        _ ->
          ok
      end
  end.

send_hello(UdpSocket, Id) ->
  Msg = list_to_binary("HELLO " ++ Id ++ " " ++ integer_to_list(?UDP_PORT) ++ "\n"),
  case gen_udp:send(UdpSocket, {255, 255, 255, 255}, ?UDP_PORT, Msg) of
    ok ->
      ok;
    {error, Reason} ->
      error({udp_send_failed, Reason})
  end.

hello(UdpSocket, Id) ->
  Ref = erlang:send_after(15000 + rand:uniform(5000), self(), continue),
  receive
    stop ->
      erlang:cancel_timer(Ref),
      ok;
    continue ->
      send_hello(UdpSocket, Id),
      hello(UdpSocket, Id)
  end.

remove_inactive_nodes() ->
  Ref = erlang:send_after(5000, self(), update),
  receive
    stop ->
      erlang:cancel_timer(Ref),
      ok;
    update ->
      Nodes = utils:load_register(),
      CurrentTime = erlang:monotonic_time(seconds),
      Update =
        maps:filter(fun(_, #{<<"last_seen">> := LastSeen}) -> CurrentTime - LastSeen =< 45 end,
                    Nodes),
      utils:save_register(Update),
      remove_inactive_nodes()
  end.

print(State) ->
  maps:foreach(fun(NodeId, Msgs) ->
                  io:format("~nNode id: ~p~n", [binary_to_list(NodeId)]),
                  lists:foreach(fun(Msg) ->
                                   Lines = string:tokens(Msg, "\n"),
                                   lists:foreach(fun(Line) ->
                                                    case string:tokens(Line, " ") of
                                                      ["SEARCH_RESPONSE",
                                                       _NodeId,
                                                       FileName,
                                                       FileSize] ->
                                                        io:format("~p - ~p bytes~n",
                                                                  [FileName,
                                                                   list_to_integer(FileSize)]);
                                                      _ -> io:format("Bad answer~n")
                                                    end
                                                 end,
                                                 Lines)
                                end,
                                Msgs)
               end,
               State).

print_search_response(State, NodeIds) ->
  receive
    {NodeId, done} ->
      Pending = lists:delete(NodeId, NodeIds),
      case Pending of
        [] ->
          print(State);
        _ ->
          print_search_response(State, Pending)
      end;
    {NodeId, Msg} ->
      NewState = maps:update_with(NodeId, fun(Msgs) -> [Msg | Msgs] end, [Msg], State),
      print_search_response(NewState, NodeIds)
  end.

collect_search_responses(ClientSocket, NodeId) ->
  case gen_tcp:recv(ClientSocket, 0, ?TIMEOUT_SEARCH_RESPONSE) of
    {ok, Data} ->
      printer ! {NodeId, binary_to_list(Data)},
      collect_search_responses(ClientSocket, NodeId);
    {error, timeout} ->
      printer ! {NodeId, done},
      ok;
    {error, closed} ->
      printer ! {NodeId, done},
      ok
  end.

send_search_request(Id, Ip, NodeId, Pattern) ->
  case gen_tcp:connect(binary_to_list(Ip),
                       ?TCP_PORT,
                       [binary, {active, false}, {reuseaddr, true}, {packet, 0}])
  of
    {ok, ClientSocket} ->
      Msg = list_to_binary("SEARCH_REQUEST " ++ Id ++ " " ++ Pattern ++ "\n"),
      gen_tcp:send(ClientSocket, Msg),
      collect_search_responses(ClientSocket, NodeId),
      gen_tcp:close(ClientSocket);
    {error, Reason} ->
      io:format("Ups: ~p~n", [Reason])
  end.

distributed_search(Id, Pattern) ->
  io:format("Doing distributed search...~n"),
  Nodes = utils:load_register(),
  NodeIds = maps:keys(Nodes),
  {PrinterPid, PrinterRef} = spawn_monitor(?MODULE, print_search_response, [#{}, NodeIds]),
  register(printer, PrinterPid),
  maps:foreach(fun(NodeId, #{<<"ip">> := Ip, <<"port">> := Port}) ->
                  spawn(?MODULE, send_search_request, [Id, Ip, NodeId, Pattern])
               end,
               Nodes),
  PrinterRef.

cli(Id) ->
  io:format("~nAvailable commands:~n"),
  io:format("1: node id~n"),
  io:format("2: list known nodes in the network~n"),
  io:format("3: list shared files~n"),
  io:format("4: list downloaded files~n"),
  io:format("5: search files in the network~n"),
  io:format("6: download file from the network~n"),
  io:format("7: exit~n"),
  case string:trim(
         io:get_line("input: "))
  of
    "1" ->
      io:format("~nYour id: ~s~n", [Id]),
      cli(Id);
    "2" ->
      utils:show_register(),
      cli(Id);
    "3" ->
      utils:shared_files(),
      cli(Id);
    "4" ->
      utils:downloaded_files(),
      cli(Id);
    "5" ->
      Pattern =
        string:trim(
          io:get_line("Pattern: ")),
      case Pattern of
        "" ->
          io:format("~nBad argument~n"),
          cli(Id);
        _ ->
          Ref = distributed_search(Id, Pattern),
          wait([Ref]),
          cli(Id)
      end;
    "6" ->
      FileName =
        string:trim(
          io:get_line("File name: ")),
      NodeId =
        string:trim(
          io:get_line("Node id: ")),
      send_download_request(FileName, NodeId),
      cli(Id);
    "7" ->
      io:format("Bye~n"),
      loop ! stop,
      ok;
    _ ->
      io:format("~nSyntax error, type one of the command numbers above.~n"),
      cli(Id)
  end.

send_big_file(FD, ClientSocket, ChunkIndex) ->
  io:format("HOla1~n"),
  case file:read(FD, ?DEFAULT_CHUNK_SIZE) of
    eof ->
      io:format("HOla3~n"),
      file:close(FD),
      ok;
    {ok, FileContent} ->
      io:format("HOla2~n"),
      ContentSize = byte_size(FileContent),
      Payload =
        if ContentSize == ?DEFAULT_CHUNK_SIZE ->
             <<?STATUS_CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big,
               FileContent:?DEFAULT_CHUNK_SIZE/binary>>;
           % ContentSize can't never be greater than
           % DEFAULT_CHUNK_SIZE, since we are reading
           % DEFAULT_CHUNK_SIZE bytes in file:read().
           true ->
             <<?STATUS_CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ContentSize:32/integer-unsigned-big,
               FileContent:ContentSize/binary>>
        end,
      gen_tcp:send(ClientSocket, Payload),
      send_big_file(FD, ClientSocket, ChunkIndex + 1);
    {error, Reason} ->
      error({read_failed, Reason})
  end.

handle_client(ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, Req} ->
      Msg = binary_to_list(Req),
      Tokens = string:tokens(Msg, " \n"),
      case Tokens of
        ["SEARCH_REQUEST", NodeId, Pattern] ->
          case utils:search(Pattern) of
            [] ->
              ok;
            Files ->
              lists:foreach(fun({FileName, FileSize}) ->
                               Res =
                                 "SEARCH_RESPONSE "
                                 ++ NodeId
                                 ++ " "
                                 ++ FileName
                                 ++ " "
                                 ++ integer_to_list(FileSize)
                                 ++ "\n",
                               case gen_tcp:send(ClientSocket, list_to_binary(Res)) of
                                 ok -> ok;
                                 {error, Reason} -> error({tcp_send_failed, Reason})
                               end
                            end,
                            Files),
              ok
          end,
          gen_tcp:close(ClientSocket),
          ok;
        ["DOWNLOAD_REQUEST", FileName] ->
          Path = filename:join([?SHARED_PATH, FileName]),
          {ok, Info} = file:read_file_info(Path),
          FileSize = Info#file_info.size,
          io:format("~nchau~n"),
          if FileSize =< ?FOUR_MB ->
               send_small_file(FileName, FileSize, ClientSocket);
             true ->
               case file:open(Path, [read, binary]) of
                 {ok, FD} ->
                   Payload =
                     <<?STATUS_OK:8/integer-unsigned-big,
                       FileSize:32/integer-unsigned-big,
                       ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big>>,
                   gen_tcp:send(ClientSocket, Payload),
                   send_big_file(FD, ClientSocket, 0);
                 {error, Reason} ->
                   error({open_file_failed, Reason})
               end
          end;
        _ ->
          io:format("wtf?~n")
      end;
    {error, closed} ->
      ok
  end.

run_tcp_server(TcpSocket, Refs) ->
  case gen_tcp:accept(TcpSocket) of
    {ok, ClientSocket} ->
      {_, ClientRef} = spawn_monitor(?MODULE, handle_client, [ClientSocket]),
      run_tcp_server(TcpSocket, [ClientRef | Refs]);
    {error, closed} ->
      wait(Refs),
      ok
  end.

create_tcp_server() ->
  case gen_tcp:listen(?TCP_PORT, [binary, {active, false}, {reuseaddr, true}, {packet, 0}])
  of
    {ok, TcpSocket} ->
      io:format("Tcp server running~n"),
      TcpSocket;
    {error, Reason} ->
      error({tcp_open_failed, Reason})
  end.

run() ->
  case gen_udp:open(?UDP_PORT,
                    % Incoming UDP packets are delivered as binaries (<<>>) instead of lists.
                    [binary,
                     % The socket is in passive mode, gen_udp:recv/2 must be called to receive data.
                     {active, true},
                     % Allows multiple processes to bind to the same port.
                     {reuseaddr, true},
                     %         Enables the socket to send or receive broadcast packets.
                     {broadcast, true},
                     % Binds the socket to all local network interfaces.
                     {ip, {0, 0, 0, 0}}])
  of
    {ok, UdpSocket} ->
      io:format("Joining network...~n"),

      {Id, InvalidIds} = join_network(UdpSocket, []),

      send_hello(UdpSocket, Id),
      {HelloPid, HelloRef} = spawn_monitor(?MODULE, hello, [UdpSocket, Id]),
      register(hello, HelloPid),

      {RemoveInactiveNodesPid, RemoveInactiveNodesRef} =
        spawn_monitor(?MODULE, remove_inactive_nodes, []),
      register(remove_inactive_nodes, RemoveInactiveNodesPid),

      TcpSocket = create_tcp_server(),
      {TcpServerPid, TcpServerRef} = spawn_monitor(?MODULE, run_tcp_server, [TcpSocket, []]),
      register(tcp_server, TcpServerPid),

      {CliPid, CliRef} = spawn_monitor(?MODULE, cli, [Id]),
      register(cli, CliPid),

      register(loop, self()),
      loop(UdpSocket,
           TcpSocket,
           Id,
           InvalidIds,
           [HelloRef, RemoveInactiveNodesRef, CliRef, TcpServerRef]);
    {error, Reason} ->
      error({udp_open_failed, Reason})
  end.

send_small_file(FileName, FileSize, ClientSocket) ->
  FilePath = filename:join([?SHARED_PATH, FileName]),
  io:format("~nSENT~n"),
  case file:read_file(FilePath) of
    {ok, FileContent} ->
      Msg =
        <<?STATUS_OK:8/big-unsigned-integer,
          FileSize:32/big-unsigned-integer,
          FileContent/binary>>,
      gen_tcp:send(ClientSocket, Msg);
    {error, Reason} ->
      error({read_file_error, Reason})
  end.

download(FileName, NodeId, ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 5) of
    {ok, <<?STATUS_OK:8, FileSize:32/big-unsigned-integer>>} ->
      if FileSize =< ?FOUR_MB ->
           receive_small_file(FileName, FileSize, ClientSocket);
         true ->
           case gen_tcp:recv(ClientSocket, 4) of
             {ok, ChunkSize} ->
               Path = filename:join([?DOWNLOADS_PATH, FileName]),
               {ok, FD} = file:open(Path, [write, binary]),
               receive_big_file(FD, ChunkSize, ClientSocket, 0, FileSize)
           end
      end
  end.

send_download_request(FileName, NodeId) ->
  case utils:load_node_info(NodeId) of
    #{<<"ip">> := Ip, <<"port">> := Port} ->
      case gen_tcp:connect(binary_to_list(Ip),
                           ?TCP_PORT,
                           [binary, {active, false}, {reuseaddr, true}, {packet, 0}])
      of
        {ok, ClientSocket} ->
          Msg = list_to_binary("DOWNLOAD_REQUEST " ++ FileName ++ " " ++ "\n"),
          case gen_tcp:send(ClientSocket, Msg) of
            ok ->
              {DownloadPid, DownloadRef} =
                spawn_monitor(?MODULE, download, [FileName, NodeId, ClientSocket]);
            {error, Reason} ->
              error({tcp_send_failed, Reason})
          end;
        {error, Reason} ->
          error({tcp_connect_failed, Reason})
      end;
    _ ->
      io:format("~nUnkown node id~n"),
      ok
  end.

receive_big_file(FD, ChunkSize, ClientSocket, BytesReceived, TotalSize) ->
  case gen_tcp:recv(ClientSocket, 7, 4000) of
    {ok,
     <<?STATUS_CHUNK:8,
       ChunkIndex:16/big-unsigned-integer,
       CurrentChunkSize:32/big-unsigned-integer>>} ->

      case gen_tcp:recv(ClientSocket, CurrentChunkSize, 4000) of
        {ok, Content} ->
          file:write(FD, Content),
          NewTotal = BytesReceived + CurrentChunkSize,
          io:format("Chunk ~p recibido (~p bytes), total acumulado: ~p/~p~n",
                    [ChunkIndex, CurrentChunkSize, NewTotal, TotalSize]),
          if
            NewTotal >= TotalSize ->
              file:close(FD),
              io:format("Transferencia completa.~n"),
              ok;
            true ->
              receive_big_file(FD, ChunkSize, ClientSocket, NewTotal, TotalSize)
          end
      end;

    {error, Reason} ->
      io:format("Error de socket: ~p~n", [Reason]),
      file:close(FD),
      {error, Reason}
  end.


receive_small_file(FileName, FileSize, ClientSocket) ->
  io:format("~nHola~n"),
  case gen_tcp:recv(ClientSocket, FileSize) of
    {ok, FileContent} ->
      % io:format("~nhola~n"),
      FilePath = filename:join([?DOWNLOADS_PATH, FileName]),
      file:write_file(FilePath, FileContent);
    {error, Reason} ->
      io:format("~nAlgo pasó: ~p~n", [Reason])
  end.
