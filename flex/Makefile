all: test1

#.PRECIOUS: %.yy.cc

%.yy.c: %.flex
	flex -o $@ $^

%.yy.cc: %.flex
	flex -o $@ $^

%.yy.o: %.yy.cc
	g++ -c -Wall -Wextra $^

test1: test1.yy.o
	g++ -o $@ $^ -lfl

.PHONY: clean
clean:
	rm -f test1 *.yy.cc *.yy.c *.o