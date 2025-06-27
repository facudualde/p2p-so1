#include "utils.h"

void menu(int udp_sock, struct sockaddr_in* server_addr, int tcp_socket) {
    char comando[BUFFER_SIZE];

    while (1) {
        printf("Ingrese un comando (id_nodo, listar_mis_archivos,descargar ,salir): ");
        printf("CLI> ");
        fgets(comando, sizeof(comando), stdin);
        comando[strcspn(comando, "\n")] = 0;

        if (strcmp(comando, "id_nodo") == 0) {
            send_command(udp_sock, server_addr, "GET_ID\n");
            receive_answer(udp_sock);
        } else if (strcmp(comando, "listar_mis_archivos") == 0) {
            send_command(udp_sock, server_addr, "GET_FILES");
            receive_answer(udp_sock);
        } else if (strcmp(comando, "descargar ") == 0) {
            printf("fucion descargar %s\n", comando);
        } else if (strcmp(comando, "salir") == 0) {
            printf("Saliendo\n");
            break;
        } else {
            printf("Comando no reconocido: %s\n", comando);
        }
    }
}

void setup_sockaddr(struct sockaddr_in* addr, const char* ip, uint16_t port) {
    memset(addr, 0, sizeof(struct sockaddr_in));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    if (inet_pton(AF_INET, ip, &addr->sin_addr) <= 0) {
        handle_error("Invalid IP address");
    }
}


int main() {
    int udp_sock,tcp_sock;
    struct sockaddr_in udp_server,tcp_server;
    setup_sockaddr(&udp_server, SERVER_ADDRESS, SERVER_UDP_PORT); // para udp
    setup_sockaddr(&tcp_server, SERVER_ADDRESS, TCP_PORT); // para tcp


    udp_sock = socket(AF_INET, SOCK_DGRAM, 0);
    tcp_sock = socket(AF_INET, SOCK_STREAM, 0);
    error_handler(udp_sock, "Error creando socket UDP") ?exit(EXIT_FAILURE):0; 
    error_handler(tcp_sock, "Error creando socket TCP") ?exit(EXIT_FAILURE):0; 




    menu(udp_sock, &udp_server,tcp_sock);

    close(udp_sock);
    return 0;
}
