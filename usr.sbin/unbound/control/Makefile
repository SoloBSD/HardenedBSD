# $FreeBSD$

# Vendor sources and generated files
LDNSDIR= ${SRCTOP}/contrib/ldns
UNBOUNDDIR= ${SRCTOP}/contrib/unbound

.PATH: ${UNBOUNDDIR} ${UNBOUNDDIR}/smallapp ${UNBOUNDDIR}/util ${UNBOUNDDIR}/doc

<<<<<<< HEAD
PROG=	local-unbound-control
=======
PROG=	unbound-control
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
SRCS=	ub_event.c unbound-control.c worker_cb.c
CFLAGS+=	-I${UNBOUNDDIR} -I${LDNSDIR}
LIBADD=	unbound crypto ssl pthread
MAN=	local-unbound-control.8

.include <bsd.prog.mk>
