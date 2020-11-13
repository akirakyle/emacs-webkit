CFLAGS   = -std=c99 -Wall -Wextra -Wno-unused-parameter -O3 -g -fpic `pkg-config --cflags gtk+-3.0 webkit2gtk-4.0 --libs webkit2gtk-4.0`

all : webkit-module.so

webkit-module.so : webkit-module.c
	$(CC) -shared $(CFLAGS) -o $@ $^

clean :
	$(RM) webkit-module.so

.PHONY : clean all
