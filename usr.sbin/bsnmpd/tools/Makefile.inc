# $FreeBSD$
# Author: Shteryana Shopova <syrinx@FreeBSD.org>

BINDIR?= /usr/bin
PACKAGE=	bsnmp

CFLAGS+= -I. -I${.CURDIR}

<<<<<<< HEAD
=======
.if exists(${.OBJDIR:H}/libbsnmptools)
LIBBSNMPTOOLSDIR=      ${.OBJDIR:H}/libbsnmptools
.else
LIBBSNMPTOOLSDIR=      ${.CURDIR:H}/libbsnmptools
.endif
LIBBSNMPTOOLS=         ${LIBBSNMPTOOLSDIR}/libbsnmptools.a

>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
WARNS?=		6
