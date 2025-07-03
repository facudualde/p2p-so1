build:
	touch nodes_register.json && echo "{}" > nodes_register.json
	rebar3 compile
	erlc -o src src/*.erl 

run:
	erl -noshell \
  -pa src \
  -pa _build/default/lib/jsx/ebin \
  -s node run -s init stop

clean:
	rm src/*.beam
	rm -rf _build
	rm nodes_register.json

docker-up:
	sudo docker-compose up --build -d

docker-down:
	sudo docker-compose down

docker-thor:
	sudo docker exec -it thor bash

docker-loki:
	sudo docker exec -it loki bash

docker-odin:
	sudo docker exec -it odin bash
