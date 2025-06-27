#include "utils.h"


void descargar(int tcp_socket){
char input[INPUT_LENGTH];
  memset(input, 0, INPUT_LENGTH);
  printf("File to download: ");
  fgets(input, INPUT_LENGTH, stdin);
  trim(input);

  char request[REQ_LENGTH];
  strcpy(request, "DOWNLOAD_REQUEST ");
  strcat(request, input);
  size_t reqLen = strlen(request);
  if (write(tcp_socket, request, reqLen) != reqLen) {
    printf("Write size error (download request)\n");
    clean(false, true, NULL, NULL);
    exit(1);
  }

  uint8_t statusCode;
  if (read(tcp_socket, &statusCode, 1) != 1) {
    printf("Read size error (status byte)\n");
    clean(false, true, NULL, NULL);
    exit(1);
  }

  switch (statusCode) {
  case STATUS_FILE_NOT_FOUND:
    printf("File not found on server side\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  case STATUS_OPEN_FAILED:
    printf("Open file failed on server side\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  case STATUS_READ_FAILED:
    printf("Read file failed on server side\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  case STATUS_BAD_REQUEST:
    printf("Bad request\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  }

  uint32_t fileSize;
  if (read(tcp_socket, &fileSize, 4) != 4) {
    printf("Read size error (file size)\n");
    clean(false, true, NULL, NULL);
    exit(1);
  }
  fileSize = ntohl(fileSize);

  char path[PATH_LENGTH];
  strcpy(path, "./downloads/");
  input[strcspn(input, "\n")] = 0;
  strcat(path, input);
  int outFd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (!outFd) {
    printf("Error while creating the file\n");
    clean(true, true, NULL, NULL);
    exit(1);
  }

  if (fileSize > FOUR_MB) {
    uint32_t defaultChunkSize;
    if (read(tcp_socket, &defaultChunkSize, 4) != 4) {
      printf("Read size error (default chunk size)\n");
      clean(true, true, path, NULL);
      exit(1);
    }
    defaultChunkSize = ntohl(defaultChunkSize);

    uint8_t *buffer = malloc(sizeof(uint8_t) * fileSize);
    uint32_t totalRead = 0;
    while (totalRead < fileSize) {
      statusCode = 0;
      if (read(tcp_socket, &statusCode, 1) != 1) {
        printf("Read size error (status byte)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      if (statusCode != STATUS_CHUNK) {
        printf("Chunk error\n");
        clean(true, true, path, buffer);
        exit(1);
      }

      uint16_t chunkIndex;
      if (read(tcp_socket, &chunkIndex, 2) != 2) {
        printf("Read size error (chunk index)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      chunkIndex = ntohs(chunkIndex);

      uint32_t contentSize;
      if (read(tcp_socket, &contentSize, 4) != 4) {
        printf("Read size error (content size)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      contentSize = ntohl(contentSize);

      uint32_t bytesRead = 0;
      while (bytesRead < contentSize) {
        uint32_t n = read(tcp_socket, buffer + bytesRead, contentSize - bytesRead);
        if (n <= 0) {
          printf("Read size error (chunk content)\n");
          clean(true, true, path, buffer);
          exit(1);
        }
        bytesRead += n;
      }

      if (write(outFd, buffer, contentSize) != contentSize) {
        printf("Write size error (chunk content)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      totalRead += contentSize;
    }
    free(buffer);
  } else {
    uint8_t buffer[fileSize];
    uint32_t n = read(tcp_socket, buffer, fileSize);
    if (n != fileSize) {
      printf("Read size error (file size)\n");
      clean(true, true, path, NULL);
      exit(1);
    }
    if (write(outFd, buffer, n) != n) {
      printf("Write size error (output file descriptor)\n");
      clean(true, true, path, NULL);
    }
  }

  printf("File received and saved\n");
}

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
        } else if (strcmp(comando, "descargar") == 0) {
            
            descargar(tcp_socket);
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


    if (connect(tcp_sock, (struct sockaddr *)&tcp_server, sizeof(tcp_server)) < 0) {
    perror("Error conectando el socket TCP");
    close(tcp_sock);
    exit(EXIT_FAILURE);
}

    menu(udp_sock, &udp_server,tcp_sock);

    close(udp_sock);
    return 0;
}
