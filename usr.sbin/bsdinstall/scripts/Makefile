# $FreeBSD$

<<<<<<< HEAD
.include <src.opts.mk>

SCRIPTS= auto adduser bootconfig checksum config docsinstall entropy hardening \
	 hostname jail keymap mirrorselect mount netconfig netconfig_ipv4 \
	 netconfig_ipv6 rootpass script services time umount wlanconfig \
	 zfsboot
=======
SCRIPTS= auto adduser checksum config docsinstall entropy hardening hostname jail \
	 keymap mirrorselect mount netconfig netconfig_ipv4 netconfig_ipv6 \
	 rootpass script services time umount wlanconfig zfsboot
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
BINDIR= ${LIBEXECDIR}/bsdinstall

MAN=

services!
.if ${MK_OPENNTPD} == "no"
	${CP} ${.CURDIR}/services.iscntpd ${.CURDIR}/services
.else
	${CP} ${.CURDIR}/services.openntpd ${.CURDIR}/services
.endif

.include <bsd.prog.mk>
