# $FreeBSD$

.PATH: ${.CURDIR:H}/fdread

PROG=	fdformat
MAN=	fdformat.8
SRCS=	fdformat.c fdutil.c

CFLAGS+= -I${.CURDIR:H}/fdread
<<<<<<< HEAD
=======

.if ${MACHINE} == "pc98"
CFLAGS+= -DPC98
.endif
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.include <bsd.prog.mk>
