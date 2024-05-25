CC = gcc
CFLAGS = -Wall -Wextra -Werror -gdwarf-2 -g3
SRC = $(wildcard src/*.c)
LIBS = -lreadline

out/esp: $(SRC)
	@mkdir -p out
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f out/*

.PHONY: clean