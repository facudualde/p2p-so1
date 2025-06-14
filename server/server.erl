-module(server).

-export([start/0, loop/1, handle_client/1]).

-define(PORT, 1234).

start() ->
  {ok, ServerSocket} =
    gen_tcp:listen(?PORT, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
  spawn(?MODULE, loop, [ServerSocket]).

loop(ServerSocket) ->
  case gen_tcp:accept(ServerSocket) of
    {ok, ClientSocket} ->
      spawn(?MODULE, handle_client, [ClientSocket])
  end,
  loop(ServerSocket).

% Convert pid to string. For example,
% <0.123.0> would be "01230".
parse_pid(Pid) ->
  Tokens =
    string:tokens(
      erlang:pid_to_list(Pid), "<>."),
  lists:concat(Tokens).

% Avoid command injections.
no_injections(String) ->
  UnsafeChars = ";|&><$`'\"()\\!#~",
  not lists:any(fun(Char) -> lists:member(Char, UnsafeChars) end, String).

% Useful for small and medium files, but it could be a problem for big files.
% In that case, we should send blocks of bytes.
% What happens if we failed sending the zip? Do we try it
% again?
send_files(ClientSocket, Zip) ->
  case file:read_file(Zip) of
    {ok, Bin} ->
      case gen_tcp:send(ClientSocket, Bin) of
        ok ->
          io:fwrite("Zip successfully sent~n"),
          ok;
        {error, Reason2} ->
          error({send_failed, Reason2})
      end;
    {error, Reason} ->
      error({read_failed, Reason})
  end.

remove_zip(Zip) ->
  case file:delete(Zip) of
    ok ->
      ok;
    {error, Reason} ->
      error({delete_tar_failed, Reason})
  end.

% Create zip file and return the name.
% For example, if a process asks for file "test.txt",
% and the pid of the handle_client process is <0.123.0>,
% the zip file would be "toBeShared01230.tar.gz".
create_zip(Pid, FilePath) ->
  case file:read_file_info("shared/" ++ FilePath) of
    {ok, _} ->
      ParsedPid = parse_pid(Pid),
      Zip = "toBeShared" ++ ParsedPid ++ ".tar.gz",
      io:fwrite("Zip: ~p~n", [Zip]),
      case erl_tar:create(Zip, ["shared/" ++ FilePath]) of
        ok ->
          Zip;
        {error, Reason2} ->
          error({create_tar_failed, Reason2})
      end;
    {error, Reason} ->
      error({file_not_found, Reason})
  end.

handle_client(ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, FilePathBin} ->
      FilePath = lists:delete($\n, binary_to_list(FilePathBin)),
      io:format("File requested: ~p~n", [FilePath]),
      case no_injections(FilePath) of
        false ->
          io:fwrite("Illegal character found!~n");
        true ->
          try
            Zip = create_zip(self(), FilePath),
            send_files(ClientSocket, Zip),
            remove_zip(Zip)
          catch
            error:Reason ->
              io:format("Error: ~p~n", [Reason]),
              {error, Reason}
          end
      end,
      handle_client(ClientSocket);
    {error, closed} ->
      io:fwrite("Client disconnected"),
      ok
  end.

% Ask about global:set_lock(lock) and global:del_lock(lock)
