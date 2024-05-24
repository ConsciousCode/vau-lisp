CC = gcc
CFLAGS = -Wall -Wextra -Werror -g

out/esp: lisp.c
	mkdir -p out
	$(CC) $(CFLAGS) -o $@ lisp.c

clean:
	rm -f out/*

.PHONY: clean