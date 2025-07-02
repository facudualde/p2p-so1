Requisitos:
-rebar3
Como ejecutar:

-rebar3 compile
-make
-rebar3 shell
-c("src/node").
-c("src/utils").
-node:run().


Comandos de la CLI:
Presenta un menú con los siguientes comandos disponibles:
    - "1": Ver el identificador del nodo actual.
    - "2": Listar los archivos compartidos en el nodo actual.
    - "3": Buscar archivos en nodos conocidos a través de TCP, especificando un nombre o patrón.
    - "4": Descargar un archivo desde nodos conocidos a través de TCP.
    - "5": Salir del programa.