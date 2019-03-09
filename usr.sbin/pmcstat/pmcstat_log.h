/*-
 * SPDX-License-Identifier: BSD-2-Clause-FreeBSD
 *
 * Copyright (c) 2005-2007, Joseph Koshy
 * Copyright (c) 2007 The FreeBSD Foundation
 * Copyright (c) 2009, Fabien Thomas
 * All rights reserved.
 *
 * Portions of this software were developed by A. Joseph Koshy under
 * sponsorship from the FreeBSD Foundation and Google, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$
 */

#ifndef	_PMCSTAT_LOG_H_
#define	_PMCSTAT_LOG_H_

<<<<<<< HEAD
#include <libpmcstat.h>
=======
typedef const void *pmcstat_interned_string;

/*
 * A 'pmcstat_process' structure models processes.  Each process is
 * associated with a set of pmcstat_pcmap structures that map
 * addresses inside it to executable objects.  This set is implemented
 * as a list, kept sorted in ascending order of mapped addresses.
 *
 * 'pp_pid' holds the pid of the process.  When a process exits, the
 * 'pp_isactive' field is set to zero, but the process structure is
 * not immediately reclaimed because there may still be samples in the
 * log for this process.
 */

struct pmcstat_process {
	LIST_ENTRY(pmcstat_process) pp_next;	/* hash-next */
	pid_t			pp_pid;		/* associated pid */
	int			pp_isactive;	/* whether active */
	uintfptr_t		pp_entryaddr;	/* entry address */
	TAILQ_HEAD(,pmcstat_pcmap) pp_map;	/* address range map */
};
extern LIST_HEAD(pmcstat_process_hash_list, pmcstat_process) pmcstat_process_hash[PMCSTAT_NHASH];

/*
 * A 'pmcstat_image' structure describes an executable program on
 * disk.  'pi_execpath' is a cookie representing the pathname of
 * the executable.  'pi_start' and 'pi_end' are the least and greatest
 * virtual addresses for the text segments in the executable.
 * 'pi_gmonlist' contains a linked list of gmon.out files associated
 * with this image.
 */

enum pmcstat_image_type {
	PMCSTAT_IMAGE_UNKNOWN = 0,	/* never looked at the image */
	PMCSTAT_IMAGE_INDETERMINABLE,	/* can't tell what the image is */
	PMCSTAT_IMAGE_ELF32,		/* ELF 32 bit object */
	PMCSTAT_IMAGE_ELF64,		/* ELF 64 bit object */
	PMCSTAT_IMAGE_AOUT		/* AOUT object */
};

struct pmcstat_image {
	LIST_ENTRY(pmcstat_image) pi_next;	/* hash link */
	pmcstat_interned_string	pi_execpath;    /* cookie */
	pmcstat_interned_string pi_samplename;  /* sample path name */
	pmcstat_interned_string pi_fullpath;    /* path to FS object */
	pmcstat_interned_string pi_name;	/* display name */

	enum pmcstat_image_type pi_type;	/* executable type */

	/*
	 * Executables have pi_start and pi_end; these are zero
	 * for shared libraries.
	 */
	uintfptr_t	pi_start;	/* start address (inclusive) */
	uintfptr_t	pi_end;		/* end address (exclusive) */
	uintfptr_t	pi_entry;	/* entry address */
	uintfptr_t	pi_vaddr;	/* virtual address where loaded */
	int		pi_isdynamic;	/* whether a dynamic object */
	int		pi_iskernelmodule;
	pmcstat_interned_string pi_dynlinkerpath; /* path in .interp */

	/* All symbols associated with this object. */
	struct pmcstat_symbol *pi_symbols;
	size_t		pi_symcount;

	/* Handle to addr2line for this image. */
	FILE *pi_addr2line;

	/*
	 * Plugins private data
	 */

	/* gprof:
	 * An image can be associated with one or more gmon.out files;
	 * one per PMC.
	 */
	LIST_HEAD(,pmcstat_gmonfile) pi_gmlist;
};
extern LIST_HEAD(pmcstat_image_hash_list, pmcstat_image) pmcstat_image_hash[PMCSTAT_NHASH];
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

extern struct pmcstat_stats pmcstat_stats; /* statistics */
extern struct pmcstat_process *pmcstat_kernproc; /* kernel 'process' */
extern int pmcstat_npmcs; /* PMC count. */

/*
 * Top mode global options.
 */
extern float pmcstat_threshold; /* Threshold to filter node. */
extern int pmcstat_pmcinfilter; /* PMC index displayed. */

/* Function prototypes */
const char *pmcstat_pmcid_to_name(pmc_id_t _pmcid);
const char *pmcstat_pmcindex_to_name(int pmcin);
struct pmcstat_pmcrecord *pmcstat_pmcindex_to_pmcr(int pmcin);
int pmcstat_image_addr2line(struct pmcstat_image *image, uintfptr_t addr,
    char *sourcefile, size_t sourcefile_len, unsigned *sourceline,
    char *funcname, size_t funcname_len);

#endif	/* _PMCSTAT_LOG_H_ */
