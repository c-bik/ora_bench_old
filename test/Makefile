
INCLUDEFLAGS = -I../_build/test/lib/oranif/c_src/odpi/include \
			   -I../_build/test/lib/oranif/c_src/odpi

CFLAGS = $(INCLUDEFLAGS) -O2 -ggdb -Wall -fPIC -std=c11
LDFLAGS = -ldl -lpthread

all: bin/OracleBulkInsert

bin/OracleBulkInsert: OracleBulkInsert.c
	gcc -o bin/OracleBulkInsert $(CFLAGS) OracleBulkInsert.c $(LDFLAGS)

clean:
	rm -rf bin/OracleBulkInsert
