#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define SERVER_UDP_PORT 12346
#define BUFFER_SIZE 1024

void send_command(int sock, struct sockaddr_in* server_addr, const char* command) {
    if (sendto(sock, command, strlen(command), 0, (struct sockaddr*)server_addr, sizeof(*server_addr)) < 0) {
        perror("Error enviando comando");
    }
}

void receive_answer(int sock) {
    char buffer[BUFFER_SIZE];
    struct sockaddr_in from_addr;
    socklen_t from_len = sizeof(from_addr);

    int bytes_recived = recvfrom(sock, buffer, sizeof(buffer) - 1, 0, (struct sockaddr*)&from_addr, &from_len);
    if (bytes_recived < 0) {
        perror("Error recibiendo respuesta");
        return;
    }

    buffer[bytes_recived] = '\0';
    printf("Respuesta del servidor: %s\n", buffer);
}

void menu(int udp_sock, struct sockaddr_in* server_addr) {
    char comando[BUFFER_SIZE];

    while (1) {
        printf("Ingrese un comando (id_nodo, listar_mis_archivos, salir): ");
        printf("CLI> ");
        fgets(comando, sizeof(comando), stdin);
        comando[strcspn(comando, "\n")] = 0;

        if (strcmp(comando, "id_nodo") == 0) {
            send_command(udp_sock, server_addr, "GET_ID\n");
            receive_answer(udp_sock);
        } else if (strcmp(comando, "listar_mis_archivos") == 0) {
            send_command(udp_sock, server_addr, "GET_FILES");
            receive_answer(udp_sock);
        } else if (strcmp(comando, "salir") == 0) {
            printf("Saliendo\n");
            break;
        } else {
            printf("Comando no reconocido: %s\n", comando);
        }
    }
}

int main() {
    int udp_sock;
    struct sockaddr_in server_addr;


    udp_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (udp_sock < 0) {
        perror("Error creando socket UDP");
        exit(EXIT_FAILURE);
    }
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(SERVER_UDP_PORT);
    inet_pton(AF_INET, "127.0.0.1", &server_addr.sin_addr);


    menu(udp_sock, &server_addr);

    close(udp_sock);
    return 0;
}
