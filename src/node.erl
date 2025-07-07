-module(node).

-export([run/0, invalid_name/3, get_id/2, send_name_request/2, join_network/2, hello/2,
         remove_inactive_nodes/0, cli/1]).

-define(UDP_PORT, 12346).
-define(TCP_PORT, 12345).
% -define(DEFAULT_CHUNK_SIZE, 1048576).
% -define(FOUR_MB, 4 * 1024 * 1024).
% -define(STATUS_OK, 101).
% -define(STATUS_CHUNK, 111).
% -define(STATUS_FILE_NOT_FOUND, 112).
% -define(STATUS_OPEN_FAILED, 113).
% -define(STATUS_READ_FAILED, 114).
% -define(STATUS_BAD_REQUEST, 115).
-define(TIMEOUT_INVALID_NAME, 10000).

% -include_lib("kernel/include/file.hrl").

invalid_name(UdpSocket, Id, Timeout) ->
  Start = erlang:monotonic_time(millisecond),
  receive
    {udp, _Socket, Ip, _Port, Res} ->
      Msg = binary_to_list(Res),
      Tokens = string:tokens(Msg, " \n"),
      case Tokens of
        ["INVALID_NAME", ReqId] ->
          if ReqId =:= Id ->
               true;
             true ->
               End = erlang:monotonic_time(millisecond) - Start,
               Remaining = Timeout - End,
               invalid_name(UdpSocket, Id, Remaining)
          end;
        ["NAME_REQUEST", ReqId] ->
          MyIp = utils:get_my_ip(),
          if Ip =/= MyIp andalso ReqId =:= Id ->
               true;
             true ->
               End = erlang:monotonic_time(millisecond) - Start,
               Remaining = Timeout - End,
               invalid_name(UdpSocket, Id, Remaining)
          end;
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
      io:format("ID: ~p~n", [Id]),
      {Id, InvalidIds}
  end.

wait([]) ->
  ok;
wait(Refs) ->
  receive
    {'DOWN', Ref, process, _Pid, _Reason} ->
      wait(lists:delete(Ref, Refs))
  end.

clean(UdpSocket, Refs) ->
  hello_sender ! stop,
  daemon ! stop,
  gen_udp:close(UdpSocket),
  wait(Refs),
  utils:reset_register(),
  ok.

loop(UdpSocket, Id, InvalidIds, Refs) ->
  receive
    stop ->
      clean(UdpSocket, Refs),
      init:stop();
    {udp, _Socket, Ip, _Port, Req} ->
      Msg = binary_to_list(Req),
      Tokens = string:tokens(Msg, " \n"),
      case Tokens of
        ["HELLO", NodeId, NodePort] ->
          if NodeId =:= Id ->
               loop(UdpSocket, Id, InvalidIds, Refs);
             true ->
               Node =
                 #{list_to_binary("port") => NodePort,
                   list_to_binary("ip") => list_to_binary(inet:ntoa(Ip)),
                   list_to_binary("last_seen") => erlang:monotonic_time(seconds)},
               ActiveNodes = maps:put(list_to_binary(NodeId), Node, utils:load_register()),
               utils:save_register(ActiveNodes),
               loop(UdpSocket, Id, InvalidIds, Refs)
          end;
        ["NAME_REQUEST", ReqId] ->
          case ReqId =:= Id orelse lists:member(ReqId, InvalidIds) of
            true ->
              InvalidMsg = list_to_binary("INVALID_NAME " ++ ReqId ++ "\n"),
              case gen_udp:send(UdpSocket, Ip, ?UDP_PORT, InvalidMsg) of
                ok ->
                  loop(UdpSocket, Id, InvalidIds, Refs);
                {error, Reason} ->
                  error({udp_send_failed, Reason})
              end;
            false ->
              loop(UdpSocket, Id, InvalidIds, Refs)
          end;
        _ ->
          ok
      end
  end.

send_hello(UdpSocket, Id) ->
  Msg = list_to_binary("HELLO " ++ Id ++ " " ++ integer_to_list(?TCP_PORT) ++ "\n"),
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
      Msg = list_to_binary("HELLO " ++ Id ++ " " ++ integer_to_list(?TCP_PORT) ++ "\n"),
      case gen_udp:send(UdpSocket, {255, 255, 255, 255}, ?UDP_PORT, Msg) of
        ok ->
          send_hello(UdpSocket, Id),
          hello(UdpSocket, Id);
        {error, Reason} ->
          error({udp_send_failed, Reason})
      end
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

cli(Id) ->
  io:format("~nAvailable commands:~n"),
  io:format("1: show node id~n"),
  io:format("2: list shared files~n"),
  io:format("3: list downloaded files"),
  io:format("4: search files in the network~n"),
  io:format("5: download file from the network~n"),
  io:format("6: exit~n"),
  case string:trim(
         io:get_line("input: "))
  of
    "1" ->
      io:format("Your id: ~s~n", [Id]),
      cli(Id);
    "5" ->
      io:format("Bye~n"),
      loop ! stop;
    _ ->
      io:format("Syntax error, type one of the command numbers above.~n"),
      cli(Id)
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
      register(hello_sender, HelloPid),
      {DaemonPid, DaemonRef} = spawn_monitor(?MODULE, remove_inactive_nodes, []),
      register(daemon, DaemonPid),
      spawn_monitor(?MODULE, cli, [Id]),
      register(loop, self()),
      loop(UdpSocket, Id, InvalidIds, [HelloRef, DaemonRef]);
    {error, Reason} ->
      error({udp_open_failed, Reason})
  end.
