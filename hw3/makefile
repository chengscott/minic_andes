all: codegen

codegen: scanner.l parser.y
	flex scanner.l
	byacc -d -v parser.y
	gcc -o codegen lex.yy.c y.tab.c symbol_table.c

.PHONY: test
test: codegen
	-./codegen < ~/testcase/test-3/if-else.c > /dev/null
	cat upper.s assembly lower.s > Blink.s

.PHONY: clean
clean:
	rm -f lex.yy.c codegen y.tab.h y.tab.c y.output Blink.s assembly
