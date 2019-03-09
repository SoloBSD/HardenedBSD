#	$FreeBSD$
#

.PATH:	${SRCDIR}/ffs ${SRCTOP}/sys/ufs/ffs
<<<<<<< HEAD
=======

CFLAGS+=	-I${SRCTOP}/sys/ufs/ffs

>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
SRCS+=	ffs_alloc.c ffs_balloc.c ffs_bswap.c ffs_subr.c ufs_bmap.c
SRCS+=	buf.c mkfs.c

# Reach-over source from sys/ufs/ffs
SRCS+=	ffs_tables.c
