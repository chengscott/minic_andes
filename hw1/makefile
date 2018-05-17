all: scanner.l
	flex scanner.l
	gcc lex.yy.c -w -lfl -o scanner

.PHONY: clean
clean:
	rm -f lex.yy.c scanner
