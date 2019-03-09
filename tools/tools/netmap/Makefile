#
# $FreeBSD$
#
# For multiple programs using a single source file each,
# we can just define 'progs' and create custom targets.
<<<<<<< HEAD
PROGS	=	pkt-gen nmreplay bridge vale-ctl lb
=======
PROGS	=	pkt-gen bridge vale-ctl lb
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

CLEANFILES = $(PROGS) *.o
MAN=
CFLAGS += -Werror -Wall
CFLAGS += -Wextra

LDFLAGS += -lpthread
.ifdef WITHOUT_PCAP
CFLAGS += -DNO_PCAP
.else
LDFLAGS += -lpcap
.endif
LDFLAGS += -lm # used by nmreplay

.include <bsd.prog.mk>
.include <bsd.lib.mk>

all: $(PROGS)

pkt-gen: pkt-gen.o
	$(CC) $(CFLAGS) -o pkt-gen pkt-gen.o $(LDFLAGS)

bridge: bridge.o
	$(CC) $(CFLAGS) -o bridge bridge.o

nmreplay: nmreplay.o
	$(CC) $(CFLAGS) -o nmreplay nmreplay.o $(LDFLAGS)

vale-ctl: vale-ctl.o
	$(CC) $(CFLAGS) -o vale-ctl vale-ctl.o

lb: lb.o pkt_hash.o
	$(CC) $(CFLAGS) -o lb lb.o pkt_hash.o $(LDFLAGS)
