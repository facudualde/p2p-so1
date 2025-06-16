#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>

#define SERVER_ADDRESS "127.0.0.1"
#define PORT 1234
#define BUFFER_SIZE 4096

int deleteTar(char *fileName)
{
    int pid = fork();
    if (pid == 0)
    { // Child process
        char *args[] = {"rm", "../downloads/toBeShared.tar.gz", NULL};
        execvp("rm", args);
        printf("rm failed\n");
        return 1;
    }
    else
    { // Parent process
        wait(NULL);
        return 0;
    }
}

int extractTar(char *fileName)
{
    int pid = fork();
    if (pid == 0)
    { // Child process
        char *args[] = {"tar", "-xf", "../downloads/toBeShared.tar.gz", "-C", "../downloads/", NULL};
        execvp("tar", args);
        printf("tar failed\n");
        return 1;
    }
    else
    { // Parent process
        wait(NULL);
        return deleteTar(fileName);
    }
}

int main()
{
    char *fileName = "folder\n";

    struct sockaddr_in servAddr;
    servAddr.sin_family = AF_INET;
    inet_pton(AF_INET, SERVER_ADDRESS, &servAddr.sin_addr);
    servAddr.sin_port = htons(PORT);

    int sockFd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockFd < 0)
    {
        printf("socket creation failed\n");
        exit(1);
    }

    if (connect(sockFd, (struct sockaddr *)&servAddr, sizeof(servAddr)) < 0)
    {
        printf("connection failed\n");
        exit(1);
    }

    // Enviar nombre del archivo al servidor
    write(sockFd, fileName, strlen(fileName));

    uint64_t zipSize;

    if (read(sockFd, &zipSize, 8) != 8)
    {
        printf("read size error\n");
        exit(1);
    }

    int outFd = open("../downloads/toBeShared.tar.gz", O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (!outFd)
    {
        printf("error while creating the file\n");
        exit(1);
    }

    uint8_t buffer[BUFFER_SIZE];
    uint64_t bytesLeft = zipSize;
    while (bytesLeft > 0)
    {
        uint64_t toBeRead = bytesLeft < BUFFER_SIZE ? bytesLeft : BUFFER_SIZE;
        uint64_t n = read(sockFd, buffer, toBeRead);
        write(outFd, buffer, n);
        bytesLeft -= n;
    }

    printf("Archivo recibido y guardado.\n");

    close(outFd);

    extractTar(fileName);

    close(sockFd);
    return 0;
}
