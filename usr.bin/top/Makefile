# $FreeBSD$

<<<<<<< HEAD
.include <src.opts.mk>
=======
TOPDIR=	${SRCTOP}/contrib/top
.PATH: ${TOPDIR}
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

PROG=	top
SRCS=	commands.c display.c machine.c screen.c top.c \
	username.c utils.c
MAN=	top.1

.if ${COMPILER_TYPE} == "gcc"
.if ${COMPILER_VERSION} >= 50000
CFLAGS.gcc=-Wno-error=discarded-qualifiers -Wno-error=incompatible-pointer-types
.else #base gcc
NO_WERROR=
.endif
.endif
CFLAGS.clang=-Wno-error=incompatible-pointer-types-discards-qualifiers

<<<<<<< HEAD
LIBADD=	ncursesw m kvm jail util sbuf
=======
LIBADD=	ncursesw m kvm jail

CLEANFILES= sigdesc.h
SIGCONV_AWK= ${SRCTOP}/contrib/top/sigconv.awk
STAGED_INCLUDE_DIR?= ${DESTDIR}/usr/include
SIGNAL_H= ${STAGED_INCLUDE_DIR}/sys/signal.h
sigdesc.h: ${SIGCONV_AWK} ${SIGNAL_H}
	awk -f ${SIGCONV_AWK} < ${SIGNAL_H} > ${.TARGET}

CLEANFILES+= top.local.h top.x
.SUFFIXES: .xs .x .hs .h
.xs.x .hs.h:
	@${ECHO} Making ${.TARGET} from ${.IMPSRC}
	@sed -e's,%LoadMax%,5.0,g' \
	-e's,%TableSize%,20011,g' \
	-e's,%NominalTopn%,18,g' \
	-e's,%topn%,-1,g' \
	-e's,%delay%,2,g' \
	-e's,%random%,1,g' \
	${.IMPSRC} > ${.TARGET}

CLEANFILES+= top.1
top.1: top.x top.local.1
	cat ${.ALLSRC} > ${.TARGET}

>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
.include <bsd.prog.mk>
