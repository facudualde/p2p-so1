-module(server).

-export([start/0, loop/1, handle_client/1, big_file/3]).

-include_lib("kernel/include/file.hrl").

-define(PORT, 12345).
-define(DEFAULT_CHUNK_SIZE, 1048576). % 1MB
-define(FOUR_MB, 4 * 1024 * 1024).
-define(STATUS_OK, 101).
-define(STATUS_CHUNK, 111).
-define(STATUS_FILE_NOT_FOUND, 112).
-define(STATUS_OPEN_FAILED, 113).
-define(STATUS_READ_FAILED, 114).
-define(STATUS_BAD_REQUEST, 115).

start() ->
  {ok, ServerSocket} =
    gen_tcp:listen(?PORT, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
  spawn(?MODULE, loop, [ServerSocket]).

loop(ServerSocket) ->
  case gen_tcp:accept(ServerSocket) of
    {ok, ClientSocket} ->
      io:format("Cliente conectado. ~n"),
      spawn(?MODULE, handle_client, [ClientSocket]);
    {error, Reason} ->
      io:format("Error: el cliente no se pudo conectar. ~p~n", [Reason])
  end,
  loop(ServerSocket).

big_file(ClientSocket, FD, ChunkIndex) ->
  case file:read(FD, ?DEFAULT_CHUNK_SIZE) of
    eof ->
      ok;
    {ok, FileContent} ->
      ContentSize = byte_size(FileContent),
      Payload =
        if ContentSize == ?DEFAULT_CHUNK_SIZE ->
             <<?STATUS_CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big,
               FileContent:?DEFAULT_CHUNK_SIZE/binary>>;
           true ->
             <<?STATUS_CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ContentSize:32/integer-unsigned-big,
               FileContent:ContentSize/binary>>
        end,
      gen_tcp:send(ClientSocket, Payload),
      big_file(ClientSocket, FD, ChunkIndex + 1);
    {error, Reason} ->
      error({read_failed, Reason})
  end.

small_file(ClientSocket, FD, FileSize) ->
  case file:read(FD, FileSize) of
    eof ->
      ok;
    {ok, FileContent} ->
      Payload =
        <<?STATUS_OK:8/integer-unsigned-big,
          FileSize:32/integer-unsigned-big,
          FileContent/binary>>,
      gen_tcp:send(ClientSocket, Payload);
    {error, Reason} ->
      error({read_failed, Reason})
  end.

send_file(ClientSocket, {FilePath, FileSize}) ->
  io:format("File size: ~p~n", [FileSize]),
  case file:open(FilePath, [read, binary]) of
    {ok, FD} ->
      if FileSize =< ?FOUR_MB ->
           small_file(ClientSocket, FD, FileSize);
         true ->
           Payload =
             <<?STATUS_OK:8/integer-unsigned-big,
               FileSize:32/integer-unsigned-big,
               ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big>>,
           gen_tcp:send(ClientSocket, Payload),
           big_file(ClientSocket, FD, 0),
           ok
      end,
      file:close(FD);
    {error, Reason} ->
      error({open_failed, Reason})
  end.

send_error_response(ClientSocket, StatusCode) ->
  gen_tcp:send(ClientSocket, <<StatusCode:8/big-unsigned-integer>>).

find_file(FileName) ->
  case file:read_file_info("compartida/" ++ FileName) of
    {ok, FileInfo} ->
      {"compartida/" ++ FileName, FileInfo#file_info.size};
    {error, Reason} ->
      error({file_not_found, Reason})
  end.


handle_error(ClientSocket, StatusCode, Reason) ->
  io:format("Error: ~p~n", [Reason]),
  send_error_response(ClientSocket, StatusCode),
  handle_client(ClientSocket).

handle_client(ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, RawMsg} ->
      Msg = lists:delete($\n, binary_to_list(RawMsg)),
      Tokens = string:tokens(Msg, " "),
      case Tokens of
        ["DOWNLOAD_REQUEST", FileName] ->
          io:format("File requested: ~p~n", [FileName]),
          try
            FileInfo = find_file(FileName),
            send_file(ClientSocket, FileInfo)
          catch
            error:{file_not_found, Reason} ->
              handle_error(ClientSocket, ?STATUS_FILE_NOT_FOUND, Reason);
            error:{open_failed, Reason} ->
              handle_error(ClientSocket, ?STATUS_OPEN_FAILED, Reason);
            error:{read_failed, Reason} ->
              handle_error(ClientSocket, ?STATUS_READ_FAILED, Reason)
          end;
        ["SEARCH_REQUEST", FromNode, Pattern] ->
          io:format("Recibí SEARCH_REQUEST desde ~s: patrón ~s~n", [FromNode, Pattern]),
          Files = nodo:file_match(Pattern),
          lists:foreach(fun(File) ->
            case file:read_file_info("compartida/" ++ File) of
              {ok, FileInfo} ->
                Size = FileInfo#file_info.size,
                Response = io_lib:format("SEARCH_RESPONSE ~s ~s ~p~n", [FromNode, File, Size]),
                gen_tcp:send(ClientSocket, list_to_binary(Response));
              _ -> ok
            end
          end, Files),
          gen_tcp:close(ClientSocket); %% Cerrá el socket después
        _ ->
          handle_error(ClientSocket, ?STATUS_BAD_REQUEST, "Bad request")
      end;
    {error, closed} ->
      io:format("El cliente se desconecto. ~n"),
      ok
  end.