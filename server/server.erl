-module(server).

-export([start/0, loop/1, handle_client/1, big_file/3]).

-include_lib("kernel/include/file.hrl").

-define(PORT, 1234).
-define(DEFAULT_CHUNK_SIZE, 1048576). % 1MB
-define(CHUNK, 111).
-define(FOUR_MB, 4 * 1024 * 1024).
-define(OK, 101).
-define(NOTFOUND, 112).

start() ->
  {ok, ServerSocket} =
    gen_tcp:listen(?PORT, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
  spawn(?MODULE, loop, [ServerSocket]).

loop(ServerSocket) ->
  case gen_tcp:accept(ServerSocket) of
    {ok, ClientSocket} ->
      io:format("Client connected!~n"),
      spawn(?MODULE, handle_client, [ClientSocket])
  end,
  loop(ServerSocket).

% Avoid command injections.
no_injections(String) ->
  UnsafeChars = ";|&><$`'\"()\\!#~",
  not lists:any(fun(Char) -> lists:member(Char, UnsafeChars) end, String).

% Send blocks of data of 4096 bytes.
big_file(ClientSocket, FD, ChunkIndex) ->
  case file:read(FD, ?DEFAULT_CHUNK_SIZE) of
    eof ->
      ok;
    {ok, FileContent} ->
      ContentSize = byte_size(FileContent),
      Payload =
        if ContentSize == ?DEFAULT_CHUNK_SIZE ->
             <<?CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ?DEFAULT_CHUNK_SIZE:32/integer-unsigned-big,
               FileContent:?DEFAULT_CHUNK_SIZE/binary>>;
           % ContentSize can't never be greater than
           % DEFAULT_CHUNK_SIZE, since we are reading
           % DEFAULT_CHUNK_SIZE bytes in file:read().
           true ->
             <<?CHUNK:8/integer-unsigned-big,
               ChunkIndex:16/integer-unsigned-big,
               ContentSize:32/integer-unsigned-big,
               FileContent:ContentSize/binary>>
        end,
      case gen_tcp:send(ClientSocket, Payload) of
        ok ->
          big_file(ClientSocket, FD, ChunkIndex + 1);
        {error, Reason2} ->
          error({send_failed, Reason2})
      end;
    {error, Reason} ->
      error({read_failed, Reason})
  end.

small_file(ClientSocket, FD, FileSize) ->
  case file:read(FD, FileSize) of
    eof ->
      ok;
    {ok, FileContent} ->
      Payload =
        <<?OK:8/integer-unsigned-big, FileSize:32/integer-unsigned-big, FileContent/binary>>,
      gen_tcp:send(ClientSocket, Payload);
    {error, Reason} ->
      gen_tcp:send(ClientSocket, <<?NOTFOUND:8/integer-unsigned-big>>),
      error({read_failed, Reason})
  end.

% What happens if we failed sending the zip? Do we try it
% again?
send_file(ClientSocket, {FilePath, FileSize}) ->
  io:format("File size: ~p~n", [FileSize]),
  case file:open(FilePath, [read, binary]) of
    {ok, FD} ->
      if FileSize =< ?FOUR_MB ->
           small_file(ClientSocket, FD, FileSize);
         true ->
           Payload =
             <<?OK:8/integer-unsigned-big,
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
  gen_tcp:send(ClientSocket, <<StatusCode:32/big-unsigned-integer>>).

find_file(FileName, ClientSocket) ->
  case file:read_file_info("../shared/" ++ FileName) of
    {ok, FileInfo} ->
      {"../shared/" ++ FileName, FileInfo#file_info.size};
    {error, Reason} ->
      gen_tcp:send(ClientSocket, <<?NOTFOUND:8/integer-unsigned-big>>),
      error({file_not_found, Reason})
  end.

handle_client(ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, RawMsg} ->
      Msg = lists:delete($\n, binary_to_list(RawMsg)),
      Tokens = string:tokens(Msg, " "),
      case Tokens of
        ["DOWNLOAD_REQUEST", FileName] ->
          io:format("File requested: ~p~n", [FileName]),
          case no_injections(FileName) of
            false ->
              io:format("Illegal character found~n"),
              send_error_response(ClientSocket, 400),
              handle_client(ClientSocket);
            true ->
              try
                FileInfo = find_file(FileName, ClientSocket),
                send_file(ClientSocket, FileInfo)
              catch
                error:Reason ->
                  io:format("Error: ~p~n", [Reason]),
                  handle_client(ClientSocket)
              end
          end;
        _ ->
          io:format("Bad request~n"),
          send_error_response(ClientSocket, 400),
          handle_client(ClientSocket)
      end;
    {error, closed} ->
      io:format("Client disconnected~n"),
      ok
  end.
