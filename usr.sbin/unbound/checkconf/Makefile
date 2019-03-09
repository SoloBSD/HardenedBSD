# $FreeBSD$

# Vendor sources and generated files
LDNSDIR= ${SRCTOP}/contrib/ldns
UNBOUNDDIR= ${SRCTOP}/contrib/unbound

.PATH: ${UNBOUNDDIR} ${UNBOUNDDIR}/smallapp ${UNBOUNDDIR}/util ${UNBOUNDDIR}/doc

<<<<<<< HEAD
PROG=	local-unbound-checkconf
=======
PROG=	unbound-checkconf
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
SRCS=	ub_event.c unbound-checkconf.c worker_cb.c
CFLAGS+=	-I${UNBOUNDDIR} -I${LDNSDIR}
LIBADD=	unbound pthread
MAN=	local-unbound-checkconf.8

CFI_OVERRIDE=-fno-sanitize=cfi-icall

.include <bsd.prog.mk>
