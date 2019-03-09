#	@(#)Makefile	8.1 (Berkeley) 6/6/93
# $FreeBSD$

.PATH: ${SRCTOP}/lib/libc/gen		# for pw_scan.c

PROG=	pwd_mkdb
MAN=	pwd_mkdb.8
SRCS=	pw_scan.c pwd_mkdb.c

<<<<<<< HEAD
CFI_OVERRIDE=-fno-sanitize=cfi-icall

=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
CFLAGS+= -I${SRCTOP}/lib/libc/gen		# for pw_scan.h

.include <bsd.prog.mk>
