build:
	touch nodes_register.json && echo "{}" > nodes_register.json
	rebar3 compile
	mkdir build
	mkdir shared 
	mkdir downloads
	erlc -o build src/*.erl 

run:
	erl -noshell \
  -pa build \
  -pa _build/default/lib/jsx/ebin \
  -s node run -s init stop

clean:
	rm -rf build
	rm -rf _build
	rm -rf shared
	rm -rf downloads
	rm nodes_register.json

docker-up:
	sudo docker-compose up --build -d

docker-down:
	sudo docker-compose down

docker-thor:
	sudo docker start thor
	sudo docker exec -it thor bash

docker-loki:
	sudo docker start loki
	sudo docker exec -it loki bash

docker-odin:
	sudo docker start odin
	sudo docker exec -it odin bash
