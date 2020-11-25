LIBS = gtk+-3.0 webkit2gtk-4.0
CFLAGS = -std=c99 -Wall -Wextra -Wno-unused-parameter -Wl,--no-as-needed -fpic 
CFLAGS += `pkg-config --cflags $(LIBS)`
LDFLAGS += `pkg-config --libs $(LIBS)`

all : webkit-module.so

debug: CFLAGS += -DDEBUG -g
debug: webkit-module.so

webkit-module.so : webkit-module.c
	$(CC) -shared $(CFLAGS) $(LDFLAGS) -o $@ $^

clean :
	$(RM) webkit-module.so

.PHONY : clean all
