all: scanner.l parser.y
	flex scanner.l
	byacc -d -v parser.y
	gcc -o parser lex.yy.c y.tab.c

.PHONY: clean
clean:
	rm -f lex.yy.c parser y.tab.h y.tab.c y.output
