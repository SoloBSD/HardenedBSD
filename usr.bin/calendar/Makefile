#	@(#)Makefile	8.1 (Berkeley) 6/6/93
# $FreeBSD$

.include <src.opts.mk>

PROG=	calendar
SRCS=	calendar.c locale.c events.c dates.c parsedata.c io.c day.c \
	ostern.c paskha.c pom.c sunpos.c
LIBADD=	m
INTER=          de_AT.ISO_8859-15 de_DE.ISO8859-1 fr_FR.ISO8859-1 \
		hr_HR.ISO8859-2 hu_HU.ISO8859-2 pt_BR.ISO8859-1 \
		pt_BR.UTF-8 ru_RU.KOI8-R ru_RU.UTF-8 uk_UA.KOI8-U
DE_LINKS=       de_DE.ISO8859-15
FR_LINKS=       fr_FR.ISO8859-15

<<<<<<< HEAD
FILESGROUPS+=	CALS
CALS=	calendars/calendar.all \
	calendars/calendar.australia \
	calendars/calendar.birthday \
	calendars/calendar.brazilian \
	calendars/calendar.christian \
	calendars/calendar.computer \
	calendars/calendar.croatian \
	calendars/calendar.dutch \
	calendars/calendar.freebsd \
	calendars/calendar.french \
	calendars/calendar.german \
	calendars/calendar.history \
	calendars/calendar.holiday \
	calendars/calendar.hungarian \
	calendars/calendar.judaic \
	calendars/calendar.lotr \
	calendars/calendar.music \
	calendars/calendar.newzealand \
	calendars/calendar.russian \
	calendars/calendar.southafrica \
	calendars/calendar.ukrainian \
	calendars/calendar.usholiday \
	calendars/calendar.world
CALSDIR=	${SHAREDIR}/calendar

CAL_de_AT.ISO_8859-15=	calendar.feiertag

CAL_de_DE.ISO8859-1=	calendar.all \
	calendar.feiertag \
	calendar.geschichte \
	calendar.kirche \
	calendar.literatur \
	calendar.musik \
	calendar.wissenschaft

CAL_fr_FR.ISO8859-1=	calendar.all \
	calendar.fetes \
	calendar.french \
	calendar.jferies \
	calendar.proverbes

CAL_hr_HR.ISO8859-2=	calendar.all \
	calendar.praznici

CAL_hu_HU.ISO8859-2=	calendar.all \
	calendar.nevnapok \
	calendar.unnepek

CAL_pt_BR.ISO8859-1=	calendar.all \
	calendar.commemorative \
	calendar.holidays \
	calendar.mcommemorative

CAL_pt_BR.UTF-8=	calendar.all \
	calendar.commemorative \
	calendar.holidays \
	calendar.mcommemorative

CAL_ru_RU.KOI8-R=	calendar.all \
	calendar.common \
	calendar.holiday \
	calendar.military \
	calendar.orthodox \
	calendar.pagan

CAL_ru_RU.UTF-8=	calendar.all \
	calendar.common \
	calendar.holiday \
	calendar.military \
	calendar.orthodox \
	calendar.pagan

CAL_uk_UA.KOI8-U=	calendar.all \
	calendar.holiday \
	calendar.misc \
	calendar.orthodox

.for lang in ${INTER}
FILESGROUPS+=	CALS_${lang}
CALS_${lang}DIR=	${SHAREDIR}/calendar/${lang}
.for file in ${CAL_${lang}}
CALS_${lang}+=	${file:S@^@calendars/${lang}/@}
=======
beforeinstall:
	${INSTALL} ${TAG_ARGS} -o ${BINOWN} -g ${BINGRP} -m ${TEXTMODE} \
	    ${.CURDIR}/calendars/calendar.* ${DESTDIR}${SHAREDIR}/calendar
.for lang in ${INTER}
	${INSTALL} ${TAG_ARGS} -o ${BINOWN} -g ${BINGRP} -m ${TEXTMODE} \
		${.CURDIR}/calendars/${lang}/calendar.* \
		${DESTDIR}${SHAREDIR}/calendar/${lang} 
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
.endfor
.endfor


.for link in ${DE_LINKS}
<<<<<<< HEAD
SYMLINKS+=	de_DE.ISO8859-1 ${SHAREDIR}/calendar/${link}
.endfor
.for link in ${FR_LINKS}
SYMLINKS+=	fr_FR.ISO8859-1 ${SHAREDIR}/calendar/${link}
=======
	rm -rf ${DESTDIR}${SHAREDIR}/calendar/${link}
	${INSTALL} ${TAG_ARGS} -l s de_DE.ISO8859-1 \
	    ${DESTDIR}${SHAREDIR}/calendar/${link}
.endfor
.for link in ${FR_LINKS}
	rm -rf ${DESTDIR}${SHAREDIR}/calendar/${link}
	${INSTALL} ${TAG_ARGS} -l s fr_FR.ISO8859-1 \
	    ${DESTDIR}${SHAREDIR}/calendar/${link}
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
.endfor

HAS_TESTS=
SUBDIR.${MK_TESTS}+= tests

.include <bsd.prog.mk>
