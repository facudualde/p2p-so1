#include "utils.h"

void handle_error(const char* msg) {
    perror(msg);
    exit(EXIT_FAILURE);
}
bool error_handler(int sock, const char* error_message) {
    if (sock < 0) {
        printf("%s\n", error_message);
        close(sock);
        return true;
    }
   return false;
}

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

void clean(int outFd, int sockFd, char *path, uint8_t *buffer) {
  if (outFd)
    close(outFd);
  if (sockFd)
    close(sockFd);
  if (path != NULL)
    unlink(path);
  if (buffer != NULL)
    free(buffer);

  return;
}

void trim(char *str) {
  char *start = str;
  char *end;

  while (isspace((unsigned char)*start))
    start++;

  // Only spaces
  if (*start == 0)
    return;

  end = start + strlen(start) - 1;
  while (end > start && isspace((unsigned char)*end))
    end--;

  *(end + 1) = '\0';

  return;
}