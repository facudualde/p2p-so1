-module(utils).

-export([get_random_id/0, load_register/0, save_register/1, update_register/3,
         get_my_ip/0]).

-define(ALLOWED_CHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
-define(NODES_REGISTER_PATH, "nodes_register.json").

get_my_ip() ->
  {ok, [{MyIp, _, _} | _]} = inet:getif(),
  MyIp.

get_random_id() ->
  lists:foldl(fun(_, Acc) ->
                 [lists:nth(
                    rand:uniform(length(?ALLOWED_CHARS)), ?ALLOWED_CHARS)]
                 ++ Acc
              end,
              [],
              lists:seq(1, 4)).

load_register() ->
  case file:read_file(?NODES_REGISTER_PATH) of
    {ok, Bin} ->
      jsx:decode(Bin, [return_maps]);
    {error, Reason} ->
      error({read_failed, Reason})
  end.

save_register(Map) ->
  file:write_file(?NODES_REGISTER_PATH, jsx:encode(Map)).

update_register(Id, Ip, Port) ->
  Nodes = load_register(),
  Update = maps:put(Id, #{ip => list_to_binary(Ip), port => Port}, Nodes),
  save_register(Update).
