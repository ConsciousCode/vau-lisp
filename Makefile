CC = gcc
CFLAGS = -Wall -Wextra -Werror
SRC = $(wildcard src/*.c)
LIBS = -lreadline

out/debug: $(SRC)
	$(CC) $(CFLAGS) -gdwarf-2 -g3 -o $@ $^ $(LIBS)

out/esp: $(SRC)
	$(CC) $(CFLAGS) -Os -o $@ $^ $(LIBS)

clean:
	rm -f out/*

.PHONY: clean