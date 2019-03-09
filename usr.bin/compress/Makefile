#	@(#)Makefile	8.2 (Berkeley) 4/17/94
# $FreeBSD$

.include <src.opts.mk>

PROG=	compress
SRCS=	compress.c zopen.c
LINKS=	${BINDIR}/compress ${BINDIR}/uncompress
MLINKS=	compress.1 uncompress.1

# XXX zopen is not part of libc
# MAN=zopen.3

<<<<<<< HEAD
HAS_TESTS=
SUBDIR.${MK_TESTS}+= tests
=======
.if ${MK_TESTS} != "no"
SUBDIR+=	tests
.endif
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.include <bsd.prog.mk>
