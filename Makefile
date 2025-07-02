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
