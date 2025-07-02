build:
	rebar3 compile
	erlc -o src src/*.erl 

run:
	erl -noshell -pa src -s node run -s init stop

clean:
	rm src/*.beam

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
