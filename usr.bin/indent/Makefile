#	@(#)Makefile	8.1 (Berkeley) 6/6/93
# $FreeBSD$

.include <src.opts.mk>

PROG=	indent
SRCS=	indent.c io.c lexi.c parse.c pr_comment.c args.c

NO_WMISSING_VARIABLE_DECLARATIONS=

<<<<<<< HEAD
HAS_TESTS=
SUBDIR.${MK_TESTS}+= tests
=======
.if ${MK_TESTS} != "no"
SUBDIR+=	tests
.endif
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.include <bsd.prog.mk>
