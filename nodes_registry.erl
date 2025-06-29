-module(nodes_registry).
-export([load/0, save/1, add_node/3]).

load() ->
  case file:read_file("nodes_registry.json") of
    {ok, Bin} -> 
      NodeInfo = jsx:decode(Bin, [return_maps]),
      maps:fold(fun(K, V, Acc) ->
        maps:put(binary_to_list(K), V, Acc)
      end, #{}, NodeInfo);
    {error, _} -> #{}
  end.
 
save(Map) ->
  AccMap = maps:fold(fun(K, V, Acc) ->
    case is_list(K) of
      true -> maps:put(list_to_binary(K), V, Acc);
      false -> maps:put(K, V, Acc)
    end
  end, #{}, Map),
  file:write_file("nodes_registry.json", jsx:encode(AccMap)).

add_node(Id, Ip, Port) ->
  Nodes = load(),
  NewNodes = maps:put(Id, #{ip => Ip, port => Port}, Nodes),
  save(NewNodes).