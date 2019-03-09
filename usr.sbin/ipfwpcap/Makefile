#
# From: Id: Makefile,v 1.2 2004/01/15 16:20:56 pkern Exp
#
# $FreeBSD$
#

PROG=	ipfwpcap

LIBADD=	pcap
<<<<<<< HEAD
CFLAGS+=-I${SRCTOP}/contrib/libpcap
=======
CFLAGS+=-I${.CURDIR}/../../contrib/libpcap
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

MAN=	ipfwpcap.8

.include <bsd.prog.mk>

test:	$(CMD)
	-rm /var/run/ipfwpcap.2000.pid
	./ipfwpcap -d 2000 - | tcpdump -r - -n -s 2000 -X

