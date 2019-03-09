#	$NetBSD: Makefile,v 1.4 2011/02/16 01:31:33 joerg Exp $
#	$FreeBSD$
#	$OpenBSD: Makefile,v 1.6 2003/06/25 15:00:04 millert Exp $

.include <src.opts.mk>

NOCFI=	yes

.if ${MK_BSD_GREP} == "yes"
PROG=	grep
MAN1=	grep.1 zgrep.1
.else
PROG=	bsdgrep
CLEANFILES+= bsdgrep.1
MAN1=	bsdgrep.1 zgrep.1

bsdgrep.1: grep.1
	${CP} ${.ALLSRC} ${.TARGET}
.endif
SRCS=	file.c grep.c queue.c util.c

<<<<<<< HEAD
SCRIPTS=	zgrep.sh
LINKS=		${BINDIR}/zgrep ${BINDIR}/zfgrep \
		${BINDIR}/zgrep ${BINDIR}/zegrep \
		${BINDIR}/zgrep ${BINDIR}/bzgrep \
		${BINDIR}/zgrep ${BINDIR}/bzegrep \
		${BINDIR}/zgrep ${BINDIR}/bzfgrep \
		${BINDIR}/zgrep ${BINDIR}/lzgrep \
		${BINDIR}/zgrep ${BINDIR}/lzegrep \
		${BINDIR}/zgrep ${BINDIR}/lzfgrep \
		${BINDIR}/zgrep ${BINDIR}/xzgrep \
		${BINDIR}/zgrep ${BINDIR}/xzegrep \
		${BINDIR}/zgrep ${BINDIR}/xzfgrep \
		${BINDIR}/zgrep ${BINDIR}/zstdgrep \
		${BINDIR}/zgrep ${BINDIR}/zstdegrep \
		${BINDIR}/zgrep ${BINDIR}/zstdegrep

MLINKS=		zgrep.1 zfgrep.1 \
		zgrep.1 zegrep.1 \
		zgrep.1 bzgrep.1 \
		zgrep.1 bzegrep.1 \
		zgrep.1 bzfgrep.1 \
		zgrep.1 lzgrep.1 \
		zgrep.1 lzegrep.1 \
		zgrep.1 lzfgrep.1 \
		zgrep.1 xzgrep.1 \
		zgrep.1 xzegrep.1 \
		zgrep.1 xzfgrep.1 \
		zgrep.1 zstdgrep.1 \
		zgrep.1 zstdegrep.1 \
		zgrep.1 zstdfgrep.1
=======
.if ${MK_BSD_GREP_FASTMATCH} == "yes"
# Extra files ported backported for some regex improvements
.PATH: ${.CURDIR}/regex
SRCS+=	fastmatch.c hashtable.c tre-compile.c tre-fastmatch.c
CFLAGS+=-I${.CURDIR}/regex
.else
CFLAGS+= -DWITHOUT_FASTMATCH
.endif
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

CFLAGS.gcc+= --param max-inline-insns-single=500

.if ${MK_BSD_GREP} == "yes"
LINKS+=	${BINDIR}/grep ${BINDIR}/egrep \
	${BINDIR}/grep ${BINDIR}/fgrep \
	${BINDIR}/grep ${BINDIR}/rgrep \
<<<<<<< HEAD
=======
	${BINDIR}/grep ${BINDIR}/zgrep \
	${BINDIR}/grep ${BINDIR}/zegrep \
	${BINDIR}/grep ${BINDIR}/zfgrep
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

MLINKS+= grep.1 egrep.1 \
	grep.1 fgrep.1 \
<<<<<<< HEAD
	grep.1 rgrep.1
.endif

.if ${MK_GNU_GREP_COMPAT} != "no"
CFLAGS+= -I${SYSROOT:U${DESTDIR}}/usr/include/gnu -DWITH_GNU
=======
	grep.1 rgrep.1 \
	grep.1 zgrep.1 \
	grep.1 zegrep.1 \
	grep.1 zfgrep.1
.endif

LIBADD=	z

.if ${MK_LZMA_SUPPORT} != "no"
LIBADD+=	lzma

.if ${MK_BSD_GREP} == "yes"
LINKS+=	${BINDIR}/${PROG} ${BINDIR}/xzgrep \
	${BINDIR}/${PROG} ${BINDIR}/xzegrep \
	${BINDIR}/${PROG} ${BINDIR}/xzfgrep \
	${BINDIR}/${PROG} ${BINDIR}/lzgrep \
	${BINDIR}/${PROG} ${BINDIR}/lzegrep \
	${BINDIR}/${PROG} ${BINDIR}/lzfgrep

MLINKS+= grep.1 xzgrep.1 \
	 grep.1 xzegrep.1 \
	 grep.1 xzfgrep.1 \
	 grep.1 lzgrep.1 \
	 grep.1 lzegrep.1 \
	 grep.1 lzfgrep.1
.endif
.else
CFLAGS+= -DWITHOUT_LZMA
.endif

.if ${MK_BZIP2_SUPPORT} != "no"
LIBADD+=	bz2

.if ${MK_BSD_GREP} == "yes"
LINKS+= ${BINDIR}/grep ${BINDIR}/bzgrep \
	${BINDIR}/grep ${BINDIR}/bzegrep \
	${BINDIR}/grep ${BINDIR}/bzfgrep
MLINKS+= grep.1 bzgrep.1 \
	 grep.1 bzegrep.1 \
	 grep.1 bzfgrep.1
.endif
.else
CFLAGS+= -DWITHOUT_BZIP2
.endif

.if ${MK_GNU_GREP_COMPAT} != "no"
CFLAGS+= -I${DESTDIR}/usr/include/gnu -DWITH_GNU
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
LIBADD+=	gnuregex
.endif

HAS_TESTS=
SUBDIR.${MK_TESTS}+= tests

.include <bsd.prog.mk>
