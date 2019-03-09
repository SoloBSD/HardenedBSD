#	@(#)Makefile	8.1 (Berkeley) 6/6/93
# $FreeBSD$

PROG=	rmt
MAN=	rmt.8

# called from /usr/src/etc/Makefile
etc-rmt:
	rm -f ${DESTDIR}/etc/rmt
<<<<<<< HEAD
	${INSTALL_RSYMLINK} ..${BINDIR}/rmt ${DESTDIR}/etc/rmt
=======
	ln -s ..${BINDIR}/rmt ${DESTDIR}/etc/rmt
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

.include <bsd.prog.mk>
