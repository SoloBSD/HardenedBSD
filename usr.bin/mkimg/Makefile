# $FreeBSD$

.include <src.opts.mk>

PROG=	mkimg
<<<<<<< HEAD
SRCS+=	format.c image.c mkimg.c scheme.c uuid.c
=======
SRCS=	format.c image.c mkimg.c scheme.c uuid.c
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
MAN=	mkimg.1

MKIMG_VERSION=20161016
mkimg.o: Makefile

CFLAGS+=-DMKIMG_VERSION=${MKIMG_VERSION}
CFLAGS+=-DSPARSE_WRITE
<<<<<<< HEAD
CFLAGS+=-I${SRCTOP}/sys/sys/disk
=======
CFLAGS+=-I${.CURDIR:H:H}/sys
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

# List of formats to support
SRCS+= \
	qcow.c \
	raw.c \
	vhd.c \
	vmdk.c

# List of schemes to support
SRCS+=	\
	apm.c \
	bsd.c \
	ebr.c \
	gpt.c \
	mbr.c \
	vtoc8.c

BINDIR?=/usr/bin

LIBADD=	util

WARNS?=	6

HAS_TESTS=
SUBDIR.${MK_TESTS}+= tests

.include <bsd.prog.mk>
