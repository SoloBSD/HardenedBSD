# $FreeBSD$

.include "${SRCTOP}/lib/clang/clang.pre.mk"

CFLAGS+=	-I${OBJTOP}/lib/clang/libclang
CFLAGS+=	-I${OBJTOP}/lib/clang/libllvm

.include "${SRCTOP}/lib/clang/clang.build.mk"

LIBDEPS+=	clang
LIBDEPS+=	llvm

.for lib in ${LIBDEPS}
DPADD+=		${OBJTOP}/lib/clang/lib${lib}/lib${lib}.a
LDADD+=		${OBJTOP}/lib/clang/lib${lib}/lib${lib}.a
.endfor

PACKAGE=	clang

LIBADD+=	ncursesw
LIBADD+=	pthread
<<<<<<< HEAD
<<<<<<< HEAD
=======
LIBADD+=	z
>>>>>>> b501965a545d878d6271e7a01b6925f70aaae287

NOCFI=		yes
=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.include <bsd.prog.mk>
