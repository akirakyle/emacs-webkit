CFLAGS   = -std=c99 -s -Wall -Wextra -Wno-unused-parameter -O3 -g3 -fpic `pkg-config --cflags gtk+-3.0 webkit2gtk-4.0`

all : webkitgtk-module.so

webkitgtk-module.so : webkitgtk-module.c
	$(CC) -shared $(CFLAGS) -o $@ $^

clean :
	$(RM) webkitgtk-module.so

.PHONY : clean all
