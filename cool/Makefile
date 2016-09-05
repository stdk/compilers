all: fact list

fact: fact.cl atoi.cl
	coolc -o $@ $^

list: list.cl atoi.cl
	coolc -o $@ $^

.PHONY: clean
clean:
	rm -f fact list *.s
