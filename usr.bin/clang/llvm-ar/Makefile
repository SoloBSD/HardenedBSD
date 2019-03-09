# $FreeBSD$

<<<<<<< HEAD
.include <src.opts.mk>

PROG_CXX=	llvm-ar

SRCDIR=		tools/llvm-ar
SRCS+=		llvm-ar.cpp

LIBADD+=	z

LINKS+=		${BINDIR}/llvm-ar ${BINDIR}/llvm-ranlib
=======
PROG_CXX=	llvm-ar

SRCDIR=		tools/llvm-ar
SRCS+=		llvm-ar.cpp
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.if ${MK_LLVM_AR_IS_AR} != "no"
SYMLINKS=	${BINDIR}/llvm-ar ${BINDIR}/ar \
    ${BINDIR}/llvm-ar ${BINDIR}/ranlib
.endif

<<<<<<< HEAD
=======
LINKS+=		${BINDIR}/llvm-ar ${BINDIR}/llvm-ranlib

>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
.include "../llvm.prog.mk"
