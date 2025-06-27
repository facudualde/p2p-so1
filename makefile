TARGET = main
SOURCES = client2.c utils.c 
OBJECTS = $(SOURCES:.c=.o)
CC = gcc
CFLAGS = -Wall -pthread
LDFLAGS = -pthread
all: $(TARGET)
$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) -o $(TARGET) $(LDFLAGS)
%.o: %.c utils.h
	$(CC) $(CFLAGS) -c $< -o $@
clean:
	rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean
