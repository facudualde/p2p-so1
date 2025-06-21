-module(nodo).
-export([init/0]).

init() ->
  case file:list_dir("compartida") of
    {ok, Filenames} ->
      io:fwrite("Archivos compartidos: ~p ~n", [Filenames]),
      Filenames;
    {error, Reason} ->
      io:fwrite("Error. No se pudo leer la carpeta compartida: ~p ~n", [Reason]),
      []
  end.


