# $FreeBSD$

.include <src.opts.mk>

PROG=	diff
<<<<<<< HEAD
SRCS=	diff.c diffdir.c diffreg.c xmalloc.c pr.c

HAS_TESTS=
SUBDIR.${MK_TESTS}+= tests
=======
SRCS=	diff.c diffdir.c diffreg.c xmalloc.c

.if ${MK_TESTS} != "no"
SUBDIR+= tests
.endif
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.include <bsd.prog.mk>
