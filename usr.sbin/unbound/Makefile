# $FreeBSD$
.include <src.opts.mk>

SUBDIR=	daemon anchor checkconf control host
<<<<<<< HEAD
SUBDIR+= setup
=======
SUBDIR+= local-setup
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
SUBDIR_PARALLEL=

.if ${MK_LIBRESSL} != "no"
CFLAGS+= -DHAVE_LIBRESSL=1
.endif

.include <bsd.subdir.mk>
