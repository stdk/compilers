all: test

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

obj = test.y.o test.flex.o

%.d: %.cc
	g++ -MM -MG -MP -MT "$(<:.cc=.o) $@" -MF $@ $<

%.y.cc %.y.hh: %.y
	bison -d -o $*.y.cc $<

%.flex.cc %.flex.hh: %.flex
	flex --header-file=$*.flex.hh -o $*.flex.cc $^

%.o: %.cc
	g++ -c -Wall -Wextra $<

test: ${obj}
	g++ -o $@ $^ -lfl

.PHONY: clean
clean:
	rm -f test *.o *.d $(obj:.o=.cc) $(obj:.o=.hh)

include $(obj:.o=.d)	