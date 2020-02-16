CFLAGS =-O -Wall -Wno-unused-variable -Wno-unused-but-set-variable -I/opt/chicken-scheme/include/chicken -fPIC
LDFLAGS = -L/opt/chicken-scheme/lib

LIBS = -lchicken
CHICKEN_FLAGS=-fixnum-arithmetic -lfa2

test: x.o foo.o
	gcc $(LDFLAGS) -lchicken -o $@ $^

foo.o: foo.c
	gcc $(CFLAGS) -c -o $@ $<

x.o: x.c
	gcc $(CFLAGS) -DC_EMBEDDED -c -o $@ $<

x.c: x.scm
	/opt/chicken-scheme/bin/chicken $(CHICKEN_FLAGS) $<
