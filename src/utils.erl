-module(utils).

-export([search/1, get_random_id/0, load_register/0, save_register/1, get_my_ip/0,
         reset_register/0, show_register/0, shared_files/0, downloaded_files/0, load_node_info/1,
         ensure_directory_exists/1]).

-define(ALLOWED_CHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
-define(NODES_REGISTER_PATH, "nodes_register.json").
-define(SHARED_PATH, "shared/").
-define(DOWNLOADS_PATH, "downloads/").

-include_lib("kernel/include/file.hrl").

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

reset_register() ->
  file:write_file(?NODES_REGISTER_PATH, "{}").

show_register() ->
  Nodes = load_register(),
  case maps:size(Nodes) of
    0 ->
      io:format("~nEmpty register~n");
    _ ->
      maps:fold(fun(Key, Value, _) ->
                   io:format("~n"),
                   {ok, Ip} = maps:find(<<"ip">>, Value),
                   {ok, Port} = maps:find(<<"port">>, Value),
                   io:format("Node id: ~p~n", [binary_to_list(Key)]),
                   io:format("ip: ~p~n", [binary_to_list(Ip)]),
                   io:format("port: ~p~n", [Port])
                end,
                ok,
                Nodes)
  end.

shared_files() ->
  io:format("~n"),
  ok = ensure_directory_exists(?SHARED_PATH),
  case file:list_dir(?SHARED_PATH) of
    {ok, List} ->
      if length(List) =:= 0 ->
           io:format("Empty directory~n");
         true ->
           lists:foreach(fun(File) ->
                            {ok, Info} = file:read_file_info(?SHARED_PATH ++ "/" ++ File),
                            io:format("~p -> size(bytes): ~p~n", [File, Info#file_info.size])
                         end,
                         List)
      end
  end.

downloaded_files() ->
  io:format("~n"),
  ok = ensure_directory_exists(?DOWNLOADS_PATH),
  case file:list_dir(?DOWNLOADS_PATH) of
    {ok, List} ->
      if length(List) =:= 0 ->
           io:format("Empty directory~n");
         true ->
           lists:foreach(fun(File) ->
                            {ok, Info} = file:read_file_info(?DOWNLOADS_PATH ++ "/" ++ File),
                            io:format("~p -> size(bytes): ~p~n", [File, Info#file_info.size])
                         end,
                         List)
      end
  end.

search(Pattern) ->
  lists:map(fun(FilePath) ->
               {ok, Info} = file:read_file_info(FilePath),
               FileSize = Info#file_info.size,
               FileName = filename:basename(FilePath),
               {FileName, FileSize}
            end,
            filelib:wildcard(?SHARED_PATH ++ Pattern)).

load_node_info(NodeId) ->
  KnownNodes = load_register(),
  case maps:find(list_to_binary(NodeId), KnownNodes) of
    {ok, Info} ->
      Info;
    error ->
      error(node_info_failed)
  end.

ensure_directory_exists(Dir) ->
  case file:read_file_info(Dir) of
    {ok, #file_info{type = directory}} ->
      ok;
    _ ->
      file:make_dir(Dir)
  end.

