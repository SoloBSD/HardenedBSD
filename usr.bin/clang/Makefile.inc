# $FreeBSD$

WARNS?=		0

<<<<<<< HEAD
.include <bsd.compiler.mk>

.if ${COMPILER_TYPE} == "clang"
DEBUG_FILES_CFLAGS= -gline-tables-only
.else
DEBUG_FILES_CFLAGS= -g1
.endif

=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
.include "../Makefile.inc"
