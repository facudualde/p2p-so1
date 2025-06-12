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

handle_client(ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, FilePathBin} ->
      FilePath = lists:delete($\n, binary_to_list(FilePathBin)),
      io:format("File requested: ~p~n", [FilePath]),
      io:format("zip toBeShared.zip shared/" ++ FilePath),
      % -j to ignore path and not include shared/ inside
      % the zip
      % We should use a try/catch system to handle the
      % error case (file might not exist).
      % Also, we have to check user input to avoid
      % command injections.
      os:cmd("zip -j toBeShared.zip shared/" ++ FilePath ++ "; rm toBeShared.zip")
  end,
  % After this, we can send the zip folder to the client
  % to be decompressed.
  ok.


%handle_client(ClientSocket) ->
 % global:set_lock(ziplock), %% espera si otro ya tiene el lock
  %try
   % case gen_tcp:recv(ClientSocket, 0) of
    %  {ok, FilePathBin} ->
    %   FilePath = lists:delete($\n, binary_to_list(FilePathBin)),
    %    ZipName = "toBeShared.zip",
    %    io:format("Zipeando: ~p~n", [FilePath]),
    %    os:cmd("zip -j " ++ ZipName ++ " shared/" ++ FilePath),
    %    gen_tcp:send(ClientSocket, <<"Archivo comprimido\n">>)
  %  end
%  after
%    global:del_lock(ziplock)
%  end.