-module(utils).

-export([get_random_id/0, load_register/0, save_register/1, update_register/3]).

get_random_id() ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
  lists:foldl(fun(_, Acc) ->
                 [lists:nth(
                    rand:uniform(length(AllowedChars)), AllowedChars)]
                 ++ Acc
              end,
              [],
              lists:seq(1, 4)).

load_register() ->
  case file:read_file("../nodes_registry.json") of
    {ok, Bin} ->
      NodeInfo = jsx:decode(Bin, [return_maps]),
      maps:fold(fun(K, V, Acc) -> maps:put(binary_to_list(K), V, Acc) end, #{}, NodeInfo);
    {error, _} ->
      #{}
  end.

save_register(Map) ->
  AccMap =
    maps:fold(fun(K, V, Acc) ->
                 case is_list(K) of
                   true -> maps:put(list_to_binary(K), V, Acc);
                   false -> maps:put(K, V, Acc)
                 end
              end,
              #{},
              Map),
  file:write_file("nodes_registry.json", jsx:encode(AccMap)).

update_register(Id, Ip, Port) ->
  Nodes = load_register(),
  NewNodes = maps:put(Id, #{ip => Ip, port => Port}, Nodes),
  save_register(NewNodes).
