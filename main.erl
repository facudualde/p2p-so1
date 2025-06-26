-module(main).
-export([main/0,start_node/0,loop/1,test_send/0]).
-define(TCP_PORT, 12345).
-define(UDP_PORT, 12346).
main() ->
  % iniciar servidor
    start_node().


start_node() ->
  case gen_udp:open(?UDP_PORT, [binary, {active, true}, {reuseaddr, true}, {broadcast, true}]) of
  {ok, ServerUdp} ->
    io:format("UDP ser iniciado en  ~p~n", [?UDP_PORT]),
    loop(ServerUdp);
  {error, Reason} ->
    io:format("Error al iniciar el servidor UDP: ~p~n", [Reason]),
    {error, Reason}
end.

loop(ServerUdp) ->
  receive
    {udp, ServerUdp, Ip, _Port, Msg} ->
      MsgStr = binary_to_list(Msg),
      case string:tokens(MsgStr, " \n") of
        ["HELLO", NodeId, PortStr] ->
          Port = list_to_integer(PortStr),
          io:format("Se recibio HELLO de ~s en el puerto ~p:~p~n", [NodeId, Ip, Port]),
          loop(ServerUdp);

        _Other ->
          io:format("Mensaje desconocido recibido: ~s~n", [MsgStr]),
          loop(ServerUdp)
      end
  end.

test_send() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  gen_udp:send(Socket, {127,0,0,1}, ?UDP_PORT, <<"HELLO TestNode 12345\n">>),
  gen_udp:close(Socket).