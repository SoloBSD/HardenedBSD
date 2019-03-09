# $FreeBSD$

.PATH: ${SRCTOP}/contrib/pnpinfo

PROG=	pnpinfo
MAN=	pnpinfo.8

CFLAGS+= -I${SRCTOP}/sys
<<<<<<< HEAD
=======

.if ${MACHINE} == "pc98"
CFLAGS+= -DPC98
.endif
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

WARNS?=	2

.include <bsd.prog.mk>
