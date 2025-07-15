### Sistema P2P Simple para Compartir Archivos en LAN

Este trabajo práctico implementa una red P2P en Erlang sobre una red LAN, donde múltiples nodos pueden buscar y transferirse archivos entre sí.

Este proyecto cuenta con el uso del paquete [jsx](https://github.com/talentdeficit/jsx), el cual sirve para la manipulación de los datos en el archivo `nodes_register.json`. Para compilarlo junto con el proyecto se utiliza el administrador de paquetes [rebar3](https://github.com/erlang/rebar3).<br>

### Instrucciones de uso
1) En caso de no tener instalado `rebar3`, ejecutar el comando `make setup`.<br>
2) Crear las carpetas `downloads` y `shared` en el directorio raíz, en las cuales irán los archivos descargados y compartidos, respectivamente.<br>
3) Ejecutar `make build` para compilar el proyecto.
4) Ejecutar `make run` para ejecutarlo.<br>
5) En caso de querer volver a correr el proyecto desde el inicio, ejecutar `make clean` antes del paso 3.

### Comandos CLI
1: mostrar nuestro id.<br>
2: listar nodos conocidos en la red.<br>
3: listar archivos compartidos en la red.<br> 
4: listar archivos descargados.<br>
5: buscar archivos en la red.<br>
6: descargar archivo de un nodo.<br>
7: salir.

### Prueba en docker
En caso de querer probar el proyecto en una misma computadora, se pueden crear 3 contenedores de docker conectados a través de una red virtual. Para ello se necesita tener instalado [docker](https://www.docker.com/) y [docker-compose](https://docs.docker.com/compose/). Seguir los siguientes pasos:
1) Ejecutar `make docker-up` para crear los contenedores (thor, loki y odin).
2) Ejecutar `make docker-thor` para conectarse al contenedor thor.
3) Ejecutar `make docker-loki` para conectarse al contenedor loki.
4) Ejecutar `make docker-odin` para conectarse al contenedor odin.

Estando dentro de un contenedor, ejecutar los pasos 3 y 4 de las instrucciones de uso.

En caso de querer eliminar los contenedores, ejecutar `make docker-down`.