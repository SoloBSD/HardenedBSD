# $FreeBSD$

<<<<<<< HEAD
.include <src.opts.mk>

=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
PROG_CXX=	llvm-objdump
MAN=

SRCDIR=		tools/llvm-objdump
SRCS+=		COFFDump.cpp
SRCS+=		ELFDump.cpp
SRCS+=		MachODump.cpp
SRCS+=		WasmDump.cpp
SRCS+=		llvm-objdump.cpp

LIBADD+=	z

<<<<<<< HEAD
.if ${MK_LLVM_OBJDUMP_IS_OBJDUMP} != "no"
SYMLINKS=	${BINDIR}/llvm-objdump \
    ${BINDIR}/objdump
.endif

=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
.include "../llvm.prog.mk"
