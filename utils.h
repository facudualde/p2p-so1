// utils.h
#ifndef UTIL_H
#define UTIL_H

// Librerias
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <ctype.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>


#define SERVER_ADDRESS "127.0.0.1"
#define TCP_PORT 1234
#define FOUR_MB (4 * 1024 * 1024)
#define INPUT_LENGTH 50
#define REQ_LENGTH 100
#define PATH_LENGTH 100
#define STATUS_OK 101
#define STATUS_CHUNK 111
#define STATUS_FILE_NOT_FOUND 112
#define STATUS_OPEN_FAILED 113
#define STATUS_READ_FAILED 114
#define STATUS_BAD_REQUEST 115

#define BUFFER_SIZE 1024
#define SERVER_UDP_PORT 12346


//Funciones Auxiliares
void clean(int outFd, int sockFd, char *path, uint8_t *buffer);
void trim(char *str) ;
bool error_handler(int sock, const char* error_message);
void handle_error(const char* msg);
void send_command(int sock, struct sockaddr_in* server_addr, const char* command) ;
void receive_answer(int sock) ;

#endif // UTIL_H