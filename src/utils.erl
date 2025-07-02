-module(utils).

-export([get_random_id/0, load_register/0, save_register/1, update_register/3,handle_file_error/1,shared_files/0]).
-define(STATUS_FILE_NOT_FOUND, 112).
-define(STATUS_OPEN_FAILED, 113).
-define(STATUS_READ_FAILED, 114).
-define(STATUS_BAD_REQUEST, 115).
-define(SHARED_DIR, "compartida").
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
  case file:read_file("nodes_registry.json") of
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

%% Devuelve la lista de archivos compartidos en el directorio correspondiente.
shared_files() ->
  case file:list_dir(?SHARED_DIR) of
    {ok, Filenames} -> 
      Filenames;
    {error, Reason} -> 
      error({list_dir_failed, Reason})
  end.
  