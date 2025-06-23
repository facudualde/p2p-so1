#include <arpa/inet.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#define SERVER_ADDRESS "127.0.0.1"
#define PORT 1234
#define FOUR_MB (4 * 1024 * 1024)

int main() {

  struct sockaddr_in servAddr;
  servAddr.sin_family = AF_INET;
  inet_pton(AF_INET, SERVER_ADDRESS, &servAddr.sin_addr);
  servAddr.sin_port = htons(PORT);

  int sockFd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockFd < 0) {
    printf("Socket creation failed\n");
    exit(1);
  }

  if (connect(sockFd, (struct sockaddr *)&servAddr, sizeof(servAddr)) < 0) {
    printf("Connection failed\n");
    exit(1);
  }

  char input[50];
  printf("File to download: ");
  fgets(input, 50, stdin);

  char request[100];
  strcpy(request, "DOWNLOAD_REQUEST ");
  strcat(request, input);
  write(sockFd, request, strlen(request));

  uint8_t statusCode;
  if (read(sockFd, &statusCode, 1) != 1) {
    printf("Read size error 1\n");
    exit(1);
  }
  printf("status code: %u\n", statusCode);
  if (statusCode == 112) {
    printf("File not found\n");
    exit(1);
  }

  ssize_t netValue = 0;

  if (read(sockFd, &netValue, 4) != 4) {
    printf("Read size error 2\n");
    exit(1);
  }

  uint32_t fileSize = ntohl(netValue);
  netValue = 0;

  char path[100];
  strcpy(path, "../downloads/");
  input[strcspn(input, "\n")] = 0;
  strcat(path, input);
  int outFd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (!outFd) {
    printf("Error while creating the file\n");
    exit(1);
  }

  if (fileSize > FOUR_MB) {
    printf("Big file\n");
  } else {
    printf("Small file\n");
    uint8_t buffer[fileSize];
    uint32_t bytesLeft = fileSize;
    while (bytesLeft > 0) {
      uint32_t toBeRead = bytesLeft < fileSize ? bytesLeft : fileSize;
      uint32_t n = read(sockFd, buffer, toBeRead);
      if (n != toBeRead) {
        printf("Read size error 3\n");
        close(outFd);
        close(sockFd);
        exit(1);
      }
      write(outFd, buffer, n);
      bytesLeft -= n;
    }
  }

  printf("File received and saved\n");

  close(outFd);
  close(sockFd);

  return 0;
}
