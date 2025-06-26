#include <arpa/inet.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#define SERVER_ADDRESS "127.0.0.1"
#define PORT 1234
#define FOUR_MB (4 * 1024 * 1024)
#define INPUT_LENGTH 50
#define REQ_LENGTH 100
#define PATH_LENGTH 100
#define OK 101
#define CHUNK 111
#define NOT_FOUND 112
#define OPEN_FAILED 113
#define READ_FAILED 114
#define BAD_REQUEST 115

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
    clean(false, true, NULL, NULL);
    exit(1);
  }

  char input[INPUT_LENGTH];
  memset(input, 0, INPUT_LENGTH);
  printf("File to download: ");
  fgets(input, INPUT_LENGTH, stdin);
  trim(input);

  char request[REQ_LENGTH];
  strcpy(request, "DOWNLOAD_REQUEST ");
  strcat(request, input);
  size_t reqLen = strlen(request);
  if (write(sockFd, request, reqLen) != reqLen) {
    printf("Write size error (download request)\n");
    clean(false, true, NULL, NULL);
    exit(1);
  }

  uint8_t statusCode;
  if (read(sockFd, &statusCode, 1) != 1) {
    printf("Read size error (status byte)\n");
    clean(false, true, NULL, NULL);
    exit(1);
  }

  switch (statusCode) {
  case NOT_FOUND:
    printf("File not found on server side\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  case OPEN_FAILED:
    printf("Open file failed on server side\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  case READ_FAILED:
    printf("Read file failed on server side\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  case BAD_REQUEST:
    printf("Bad request\n");
    clean(false, true, NULL, NULL);
    exit(1);
    break;
  }

  uint32_t fileSize;
  if (read(sockFd, &fileSize, 4) != 4) {
    printf("Read size error (file size)\n");
    clean(false, true, NULL, NULL);
    exit(1);
  }
  fileSize = ntohl(fileSize);

  char path[PATH_LENGTH];
  strcpy(path, "../downloads/");
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
    if (read(sockFd, &defaultChunkSize, 4) != 4) {
      printf("Read size error (default chunk size)\n");
      clean(true, true, path, NULL);
      exit(1);
    }
    defaultChunkSize = ntohl(defaultChunkSize);

    uint8_t *buffer = malloc(sizeof(uint8_t) * fileSize);
    uint32_t totalRead = 0;
    while (totalRead < fileSize) {
      statusCode = 0;
      if (read(sockFd, &statusCode, 1) != 1) {
        printf("Read size error (status byte)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      if (statusCode != CHUNK) {
        printf("Chunk error\n");
        clean(true, true, path, buffer);
        exit(1);
      }

      uint16_t chunkIndex;
      if (read(sockFd, &chunkIndex, 2) != 2) {
        printf("Read size error (chunk index)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      chunkIndex = ntohs(chunkIndex);

      uint32_t contentSize;
      if (read(sockFd, &contentSize, 4) != 4) {
        printf("Read size error (content size)\n");
        clean(true, true, path, buffer);
        exit(1);
      }
      contentSize = ntohl(contentSize);

      uint32_t bytesRead = 0;
      while (bytesRead < contentSize) {
        uint32_t n = read(sockFd, buffer + bytesRead, contentSize - bytesRead);
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
    uint32_t n = read(sockFd, buffer, fileSize);
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

  return 0;
}
