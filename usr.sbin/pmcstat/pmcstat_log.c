/*-
 * SPDX-License-Identifier: BSD-2-Clause-FreeBSD
 *
 * Copyright (c) 2005-2007, Joseph Koshy
 * Copyright (c) 2007 The FreeBSD Foundation
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
 */

/*
 * Transform a hwpmc(4) log into human readable form, and into
 * gprof(1) compatible profiles.
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

#include <sys/param.h>
#include <sys/endian.h>
#include <sys/cpuset.h>
#include <sys/gmon.h>
#include <sys/imgact_aout.h>
#include <sys/imgact_elf.h>
#include <sys/mman.h>
#include <sys/pmc.h>
#include <sys/queue.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <netinet/in.h>

#include <assert.h>
#include <curses.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <gelf.h>
#include <libgen.h>
#include <limits.h>
#include <netdb.h>
#include <pmc.h>
#include <pmclog.h>
#include <sysexits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pmcstat.h"
#include "pmcstat_log.h"
#include "pmcstat_top.h"

/*
 * PUBLIC INTERFACES
 *
 * pmcstat_initialize_logging()	initialize this module, called first
 * pmcstat_shutdown_logging()		orderly shutdown, called last
 * pmcstat_open_log()			open an eventlog for processing
 * pmcstat_process_log()		print/convert an event log
 * pmcstat_display_log()		top mode display for the log
 * pmcstat_close_log()			finish processing an event log
 *
 * IMPLEMENTATION NOTES
 *
 * We correlate each 'callchain' or 'sample' entry seen in the event
 * log back to an executable object in the system. Executable objects
 * include:
 * 	- program executables,
 *	- shared libraries loaded by the runtime loader,
 *	- dlopen()'ed objects loaded by the program,
 *	- the runtime loader itself,
 *	- the kernel and kernel modules.
 *
 * Each process that we know about is treated as a set of regions that
 * map to executable objects.  Processes are described by
 * 'pmcstat_process' structures.  Executable objects are tracked by
 * 'pmcstat_image' structures.  The kernel and kernel modules are
 * common to all processes (they reside at the same virtual addresses
 * for all processes).  Individual processes can have their text
 * segments and shared libraries loaded at process-specific locations.
 *
 * A given executable object can be in use by multiple processes
 * (e.g., libc.so) and loaded at a different address in each.
 * pmcstat_pcmap structures track per-image mappings.
 *
 * The sample log could have samples from multiple PMCs; we
 * generate one 'gmon.out' profile per PMC.
 *
 * IMPLEMENTATION OF GMON OUTPUT
 *
 * Each executable object gets one 'gmon.out' profile, per PMC in
 * use.  Creation of 'gmon.out' profiles is done lazily.  The
 * 'gmon.out' profiles generated for a given sampling PMC are
 * aggregates of all the samples for that particular executable
 * object.
 *
 * IMPLEMENTATION OF SYSTEM-WIDE CALLGRAPH OUTPUT
 *
 * Each active pmcid has its own callgraph structure, described by a
 * 'struct pmcstat_callgraph'.  Given a process id and a list of pc
 * values, we map each pc value to a tuple (image, symbol), where
 * 'image' denotes an executable object and 'symbol' is the closest
 * symbol that precedes the pc value.  Each pc value in the list is
 * also given a 'rank' that reflects its depth in the call stack.
 */

struct pmcstat_pmcs pmcstat_pmcs = LIST_HEAD_INITIALIZER(pmcstat_pmcs);

/*
 * All image descriptors are kept in a hash table.
 */
struct pmcstat_image_hash_list pmcstat_image_hash[PMCSTAT_NHASH];

/*
 * All process descriptors are kept in a hash table.
 */
struct pmcstat_process_hash_list pmcstat_process_hash[PMCSTAT_NHASH];

struct pmcstat_stats pmcstat_stats; /* statistics */
static int ps_samples_period; /* samples count between top refresh. */

struct pmcstat_process *pmcstat_kernproc; /* kernel 'process' */

#include "pmcpl_gprof.h"
#include "pmcpl_callgraph.h"
#include "pmcpl_annotate.h"
#include "pmcpl_annotate_cg.h"
#include "pmcpl_calltree.h"

static struct pmc_plugins plugins[] = {
	{
		.pl_name		= "none",
	},
	{
		.pl_name		= "callgraph",
		.pl_init		= pmcpl_cg_init,
		.pl_shutdown		= pmcpl_cg_shutdown,
		.pl_process		= pmcpl_cg_process,
		.pl_topkeypress		= pmcpl_cg_topkeypress,
		.pl_topdisplay		= pmcpl_cg_topdisplay
	},
	{
		.pl_name		= "gprof",
		.pl_shutdown		= pmcpl_gmon_shutdown,
		.pl_process		= pmcpl_gmon_process,
		.pl_initimage		= pmcpl_gmon_initimage,
		.pl_shutdownimage	= pmcpl_gmon_shutdownimage,
		.pl_newpmc		= pmcpl_gmon_newpmc
	},
	{
		.pl_name		= "annotate",
		.pl_process		= pmcpl_annotate_process
	},
	{
		.pl_name		= "calltree",
		.pl_configure		= pmcpl_ct_configure,
		.pl_init		= pmcpl_ct_init,
		.pl_shutdown		= pmcpl_ct_shutdown,
		.pl_process		= pmcpl_ct_process,
		.pl_topkeypress		= pmcpl_ct_topkeypress,
		.pl_topdisplay		= pmcpl_ct_topdisplay
	},
	{
		.pl_name		= "annotate_cg",
		.pl_process		= pmcpl_annotate_cg_process
	},

	{
		.pl_name		= NULL
	}
};

static int pmcstat_mergepmc;

int pmcstat_pmcinfilter = 0; /* PMC filter for top mode. */
float pmcstat_threshold = 0.5; /* Cost filter for top mode. */

/*
 * Prototypes
 */

static void pmcstat_stats_reset(int _reset_global);

/*
 * PMC count.
 */
int pmcstat_npmcs;

/*
 * PMC Top mode pause state.
 */
static int pmcstat_pause;

static void
pmcstat_stats_reset(int reset_global)
{
	struct pmcstat_pmcrecord *pr;

	/* Flush PMCs stats. */
	LIST_FOREACH(pr, &pmcstat_pmcs, pr_next) {
		pr->pr_samples = 0;
		pr->pr_dubious_frames = 0;
	}
	ps_samples_period = 0;

	/* Flush global stats. */
	if (reset_global)
		bzero(&pmcstat_stats, sizeof(struct pmcstat_stats));
}

/*
<<<<<<< HEAD
=======
 * Compute a 'hash' value for a string.
 */

static int
pmcstat_string_compute_hash(const char *s)
{
	unsigned hash;

	for (hash = 2166136261; *s; s++)
		hash = (hash ^ *s) * 16777619;

	return (hash & PMCSTAT_HASH_MASK);
}

/*
 * Intern a copy of string 's', and return a pointer to the
 * interned structure.
 */

pmcstat_interned_string
pmcstat_string_intern(const char *s)
{
	struct pmcstat_string *ps;
	const struct pmcstat_string *cps;
	int hash, len;

	if ((cps = pmcstat_string_lookup(s)) != NULL)
		return (cps);

	hash = pmcstat_string_compute_hash(s);
	len  = strlen(s);

	if ((ps = malloc(sizeof(*ps))) == NULL)
		err(EX_OSERR, "ERROR: Could not intern string");
	ps->ps_len = len;
	ps->ps_hash = hash;
	ps->ps_string = strdup(s);
	LIST_INSERT_HEAD(&pmcstat_string_hash[hash], ps, ps_next);
	return ((pmcstat_interned_string) ps);
}

const char *
pmcstat_string_unintern(pmcstat_interned_string str)
{
	const char *s;

	s = ((const struct pmcstat_string *) str)->ps_string;
	return (s);
}

pmcstat_interned_string
pmcstat_string_lookup(const char *s)
{
	struct pmcstat_string *ps;
	int hash, len;

	hash = pmcstat_string_compute_hash(s);
	len = strlen(s);

	LIST_FOREACH(ps, &pmcstat_string_hash[hash], ps_next)
	    if (ps->ps_len == len && ps->ps_hash == hash &&
		strcmp(ps->ps_string, s) == 0)
		    return (ps);
	return (NULL);
}

static int
pmcstat_string_lookup_hash(pmcstat_interned_string s)
{
	const struct pmcstat_string *ps;

	ps = (const struct pmcstat_string *) s;
	return (ps->ps_hash);
}

/*
 * Initialize the string interning facility.
 */

static void
pmcstat_string_initialize(void)
{
	int i;

	for (i = 0; i < PMCSTAT_NHASH; i++)
		LIST_INIT(&pmcstat_string_hash[i]);
}

/*
 * Destroy the string table, free'ing up space.
 */

static void
pmcstat_string_shutdown(void)
{
	int i;
	struct pmcstat_string *ps, *pstmp;

	for (i = 0; i < PMCSTAT_NHASH; i++)
		LIST_FOREACH_SAFE(ps, &pmcstat_string_hash[i], ps_next,
		    pstmp) {
			LIST_REMOVE(ps, ps_next);
			free(ps->ps_string);
			free(ps);
		}
}

/*
 * Determine whether a given executable image is an A.OUT object, and
 * if so, fill in its parameters from the text file.
 * Sets image->pi_type.
 */

static void
pmcstat_image_get_aout_params(struct pmcstat_image *image)
{
	int fd;
	ssize_t nbytes;
	struct exec ex;
	const char *path;
	char buffer[PATH_MAX];

	path = pmcstat_string_unintern(image->pi_execpath);
	assert(path != NULL);

	if (image->pi_iskernelmodule)
		errx(EX_SOFTWARE,
		    "ERROR: a.out kernel modules are unsupported \"%s\"", path);

	(void) snprintf(buffer, sizeof(buffer), "%s%s",
	    args.pa_fsroot, path);

	if ((fd = open(buffer, O_RDONLY, 0)) < 0 ||
	    (nbytes = read(fd, &ex, sizeof(ex))) < 0) {
		if (args.pa_verbosity >= 2)
			warn("WARNING: Cannot determine type of \"%s\"",
			    path);
		image->pi_type = PMCSTAT_IMAGE_INDETERMINABLE;
		if (fd != -1)
			(void) close(fd);
		return;
	}

	(void) close(fd);

	if ((unsigned) nbytes != sizeof(ex) ||
	    N_BADMAG(ex))
		return;

	image->pi_type = PMCSTAT_IMAGE_AOUT;

	/* TODO: the rest of a.out processing */

	return;
}

/*
 * Helper function.
 */

static int
pmcstat_symbol_compare(const void *a, const void *b)
{
	const struct pmcstat_symbol *sym1, *sym2;

	sym1 = (const struct pmcstat_symbol *) a;
	sym2 = (const struct pmcstat_symbol *) b;

	if (sym1->ps_end <= sym2->ps_start)
		return (-1);
	if (sym1->ps_start >= sym2->ps_end)
		return (1);
	return (0);
}

/*
 * Map an address to a symbol in an image.
 */

struct pmcstat_symbol *
pmcstat_symbol_search(struct pmcstat_image *image, uintfptr_t addr)
{
	struct pmcstat_symbol sym;

	if (image->pi_symbols == NULL)
		return (NULL);

	sym.ps_name  = NULL;
	sym.ps_start = addr;
	sym.ps_end   = addr + 1;

	return (bsearch((void *) &sym, image->pi_symbols,
		    image->pi_symcount, sizeof(struct pmcstat_symbol),
		    pmcstat_symbol_compare));
}

/*
 * Add the list of symbols in the given section to the list associated
 * with the object.
 */
static void
pmcstat_image_add_symbols(struct pmcstat_image *image, Elf *e,
    Elf_Scn *scn, GElf_Shdr *sh)
{
	int firsttime;
	size_t n, newsyms, nshsyms, nfuncsyms;
	struct pmcstat_symbol *symptr;
	char *fnname;
	GElf_Sym sym;
	Elf_Data *data;

	if ((data = elf_getdata(scn, NULL)) == NULL)
		return;

	/*
	 * Determine the number of functions named in this
	 * section.
	 */

	nshsyms = sh->sh_size / sh->sh_entsize;
	for (n = nfuncsyms = 0; n < nshsyms; n++) {
		if (gelf_getsym(data, (int) n, &sym) != &sym)
			return;
		if (GELF_ST_TYPE(sym.st_info) == STT_FUNC)
			nfuncsyms++;
	}

	if (nfuncsyms == 0)
		return;

	/*
	 * Allocate space for the new entries.
	 */
	firsttime = image->pi_symbols == NULL;
	symptr = reallocarray(image->pi_symbols,
	    image->pi_symcount + nfuncsyms, sizeof(*symptr));
	if (symptr == image->pi_symbols) /* realloc() failed. */
		return;
	image->pi_symbols = symptr;

	/*
	 * Append new symbols to the end of the current table.
	 */
	symptr += image->pi_symcount;

	for (n = newsyms = 0; n < nshsyms; n++) {
		if (gelf_getsym(data, (int) n, &sym) != &sym)
			return;
		if (GELF_ST_TYPE(sym.st_info) != STT_FUNC)
			continue;
		if (sym.st_shndx == STN_UNDEF)
			continue;

		if (!firsttime && pmcstat_symbol_search(image, sym.st_value))
			continue; /* We've seen this symbol already. */

		if ((fnname = elf_strptr(e, sh->sh_link, sym.st_name))
		    == NULL)
			continue;
#ifdef __arm__
		/* Remove spurious ARM function name. */
		if (fnname[0] == '$' &&
		    (fnname[1] == 'a' || fnname[1] == 't' ||
		    fnname[1] == 'd') &&
		    fnname[2] == '\0')
			continue;
#endif

		symptr->ps_name  = pmcstat_string_intern(fnname);
		symptr->ps_start = sym.st_value - image->pi_vaddr;
		symptr->ps_end   = symptr->ps_start + sym.st_size;
		symptr++;

		newsyms++;
	}

	image->pi_symcount += newsyms;
	if (image->pi_symcount == 0)
		return;

	assert(newsyms <= nfuncsyms);

	/*
	 * Return space to the system if there were duplicates.
	 */
	if (newsyms < nfuncsyms)
		image->pi_symbols = reallocarray(image->pi_symbols,
		    image->pi_symcount, sizeof(*symptr));

	/*
	 * Keep the list of symbols sorted.
	 */
	qsort(image->pi_symbols, image->pi_symcount, sizeof(*symptr),
	    pmcstat_symbol_compare);

	/*
	 * Deal with function symbols that have a size of 'zero' by
	 * making them extend to the next higher address.  These
	 * symbols are usually defined in assembly code.
	 */
	for (symptr = image->pi_symbols;
	     symptr < image->pi_symbols + (image->pi_symcount - 1);
	     symptr++)
		if (symptr->ps_start == symptr->ps_end)
			symptr->ps_end = (symptr+1)->ps_start;
}

/*
 * Examine an ELF file to determine the size of its text segment.
 * Sets image->pi_type if anything conclusive can be determined about
 * this image.
 */

static void
pmcstat_image_get_elf_params(struct pmcstat_image *image)
{
	int fd;
	size_t i, nph, nsh;
	const char *path, *elfbase;
	char *p, *endp;
	uintfptr_t minva, maxva;
	Elf *e;
	Elf_Scn *scn;
	GElf_Ehdr eh;
	GElf_Phdr ph;
	GElf_Shdr sh;
	enum pmcstat_image_type image_type;
	char buffer[PATH_MAX];

	assert(image->pi_type == PMCSTAT_IMAGE_UNKNOWN);

	image->pi_start = minva = ~(uintfptr_t) 0;
	image->pi_end = maxva = (uintfptr_t) 0;
	image->pi_type = image_type = PMCSTAT_IMAGE_INDETERMINABLE;
	image->pi_isdynamic = 0;
	image->pi_dynlinkerpath = NULL;
	image->pi_vaddr = 0;

	path = pmcstat_string_unintern(image->pi_execpath);
	assert(path != NULL);

	/*
	 * Look for kernel modules under FSROOT/KERNELPATH/NAME,
	 * and user mode executable objects under FSROOT/PATHNAME.
	 */
	if (image->pi_iskernelmodule)
		(void) snprintf(buffer, sizeof(buffer), "%s%s/%s",
		    args.pa_fsroot, args.pa_kernel, path);
	else
		(void) snprintf(buffer, sizeof(buffer), "%s%s",
		    args.pa_fsroot, path);

	e = NULL;
	if ((fd = open(buffer, O_RDONLY, 0)) < 0 ||
	    (e = elf_begin(fd, ELF_C_READ, NULL)) == NULL ||
	    (elf_kind(e) != ELF_K_ELF)) {
		if (args.pa_verbosity >= 2)
			warnx("WARNING: Cannot determine the type of \"%s\".",
			    buffer);
		goto done;
	}

	if (gelf_getehdr(e, &eh) != &eh) {
		warnx(
		    "WARNING: Cannot retrieve the ELF Header for \"%s\": %s.",
		    buffer, elf_errmsg(-1));
		goto done;
	}

	if (eh.e_type != ET_EXEC && eh.e_type != ET_DYN &&
	    !(image->pi_iskernelmodule && eh.e_type == ET_REL)) {
		warnx("WARNING: \"%s\" is of an unsupported ELF type.",
		    buffer);
		goto done;
	}

	image_type = eh.e_ident[EI_CLASS] == ELFCLASS32 ?
	    PMCSTAT_IMAGE_ELF32 : PMCSTAT_IMAGE_ELF64;

	/*
	 * Determine the virtual address where an executable would be
	 * loaded.  Additionally, for dynamically linked executables,
	 * save the pathname to the runtime linker.
	 */
	if (eh.e_type == ET_EXEC) {
		if (elf_getphnum(e, &nph) == 0) {
			warnx(
"WARNING: Could not determine the number of program headers in \"%s\": %s.",
			    buffer,
			    elf_errmsg(-1));
			goto done;
		}
		for (i = 0; i < eh.e_phnum; i++) {
			if (gelf_getphdr(e, i, &ph) != &ph) {
				warnx(
"WARNING: Retrieval of PHDR entry #%ju in \"%s\" failed: %s.",
				    (uintmax_t) i, buffer, elf_errmsg(-1));
				goto done;
			}
			switch (ph.p_type) {
			case PT_DYNAMIC:
				image->pi_isdynamic = 1;
				break;
			case PT_INTERP:
				if ((elfbase = elf_rawfile(e, NULL)) == NULL) {
					warnx(
"WARNING: Cannot retrieve the interpreter for \"%s\": %s.",
					    buffer, elf_errmsg(-1));
					goto done;
				}
				image->pi_dynlinkerpath =
				    pmcstat_string_intern(elfbase +
				        ph.p_offset);
				break;
			case PT_LOAD:
				if ((ph.p_flags & PF_X) != 0 &&
				    (ph.p_offset & (-ph.p_align)) == 0)
					image->pi_vaddr = ph.p_vaddr & (-ph.p_align);
				break;
			}
		}
	}

	/*
	 * Get the min and max VA associated with this ELF object.
	 */
	if (elf_getshnum(e, &nsh) == 0) {
		warnx(
"WARNING: Could not determine the number of sections for \"%s\": %s.",
		    buffer, elf_errmsg(-1));
		goto done;
	}

	for (i = 0; i < nsh; i++) {
		if ((scn = elf_getscn(e, i)) == NULL ||
		    gelf_getshdr(scn, &sh) != &sh) {
			warnx(
"WARNING: Could not retrieve section header #%ju in \"%s\": %s.",
			    (uintmax_t) i, buffer, elf_errmsg(-1));
			goto done;
		}
		if (sh.sh_flags & SHF_EXECINSTR) {
			minva = min(minva, sh.sh_addr);
			maxva = max(maxva, sh.sh_addr + sh.sh_size);
		}
		if (sh.sh_type == SHT_SYMTAB || sh.sh_type == SHT_DYNSYM)
			pmcstat_image_add_symbols(image, e, scn, &sh);
	}

	image->pi_start = minva;
	image->pi_end   = maxva;
	image->pi_type  = image_type;
	image->pi_fullpath = pmcstat_string_intern(buffer);

	/* Build display name
	 */
	endp = buffer;
	for (p = buffer; *p; p++)
		if (*p == '/')
			endp = p+1;
	image->pi_name = pmcstat_string_intern(endp);

 done:
	(void) elf_end(e);
	if (fd >= 0)
		(void) close(fd);
	return;
}

/*
 * Given an image descriptor, determine whether it is an ELF, or AOUT.
 * If no handler claims the image, set its type to 'INDETERMINABLE'.
 */

void
pmcstat_image_determine_type(struct pmcstat_image *image)
{
	assert(image->pi_type == PMCSTAT_IMAGE_UNKNOWN);

	/* Try each kind of handler in turn */
	if (image->pi_type == PMCSTAT_IMAGE_UNKNOWN)
		pmcstat_image_get_elf_params(image);
	if (image->pi_type == PMCSTAT_IMAGE_UNKNOWN)
		pmcstat_image_get_aout_params(image);

	/*
	 * Otherwise, remember that we tried to determine
	 * the object's type and had failed.
	 */
	if (image->pi_type == PMCSTAT_IMAGE_UNKNOWN)
		image->pi_type = PMCSTAT_IMAGE_INDETERMINABLE;
}

/*
 * Locate an image descriptor given an interned path, adding a fresh
 * descriptor to the cache if necessary.  This function also finds a
 * suitable name for this image's sample file.
 *
 * We defer filling in the file format specific parts of the image
 * structure till the time we actually see a sample that would fall
 * into this image.
 */

static struct pmcstat_image *
pmcstat_image_from_path(pmcstat_interned_string internedpath,
    int iskernelmodule)
{
	int hash;
	struct pmcstat_image *pi;

	hash = pmcstat_string_lookup_hash(internedpath);

	/* First, look for an existing entry. */
	LIST_FOREACH(pi, &pmcstat_image_hash[hash], pi_next)
	    if (pi->pi_execpath == internedpath &&
		  pi->pi_iskernelmodule == iskernelmodule)
		    return (pi);

	/*
	 * Allocate a new entry and place it at the head of the hash
	 * and LRU lists.
	 */
	pi = malloc(sizeof(*pi));
	if (pi == NULL)
		return (NULL);

	pi->pi_type = PMCSTAT_IMAGE_UNKNOWN;
	pi->pi_execpath = internedpath;
	pi->pi_start = ~0;
	pi->pi_end = 0;
	pi->pi_entry = 0;
	pi->pi_vaddr = 0;
	pi->pi_isdynamic = 0;
	pi->pi_iskernelmodule = iskernelmodule;
	pi->pi_dynlinkerpath = NULL;
	pi->pi_symbols = NULL;
	pi->pi_symcount = 0;
	pi->pi_addr2line = NULL;

	if (plugins[args.pa_pplugin].pl_initimage != NULL)
		plugins[args.pa_pplugin].pl_initimage(pi);
	if (plugins[args.pa_plugin].pl_initimage != NULL)
		plugins[args.pa_plugin].pl_initimage(pi);

	LIST_INSERT_HEAD(&pmcstat_image_hash[hash], pi, pi_next);

	return (pi);
}

/*
 * Record the fact that PC values from 'start' to 'end' come from
 * image 'image'.
 */

static void
pmcstat_image_link(struct pmcstat_process *pp, struct pmcstat_image *image,
    uintfptr_t start)
{
	struct pmcstat_pcmap *pcm, *pcmnew;
	uintfptr_t offset;

	assert(image->pi_type != PMCSTAT_IMAGE_UNKNOWN &&
	    image->pi_type != PMCSTAT_IMAGE_INDETERMINABLE);

	if ((pcmnew = malloc(sizeof(*pcmnew))) == NULL)
		err(EX_OSERR, "ERROR: Cannot create a map entry");

	/*
	 * Adjust the map entry to only cover the text portion
	 * of the object.
	 */

	offset = start - image->pi_vaddr;
	pcmnew->ppm_lowpc  = image->pi_start + offset;
	pcmnew->ppm_highpc = image->pi_end + offset;
	pcmnew->ppm_image  = image;

	assert(pcmnew->ppm_lowpc < pcmnew->ppm_highpc);

	/* Overlapped mmap()'s are assumed to never occur. */
	TAILQ_FOREACH(pcm, &pp->pp_map, ppm_next)
	    if (pcm->ppm_lowpc >= pcmnew->ppm_highpc)
		    break;

	if (pcm == NULL)
		TAILQ_INSERT_TAIL(&pp->pp_map, pcmnew, ppm_next);
	else
		TAILQ_INSERT_BEFORE(pcm, pcmnew, ppm_next);
}

/*
 * Unmap images in the range [start..end) associated with process
 * 'pp'.
 */

static void
pmcstat_image_unmap(struct pmcstat_process *pp, uintfptr_t start,
    uintfptr_t end)
{
	struct pmcstat_pcmap *pcm, *pcmtmp, *pcmnew;

	assert(pp != NULL);
	assert(start < end);

	/*
	 * Cases:
	 * - we could have the range completely in the middle of an
	 *   existing pcmap; in this case we have to split the pcmap
	 *   structure into two (i.e., generate a 'hole').
	 * - we could have the range covering multiple pcmaps; these
	 *   will have to be removed.
	 * - we could have either 'start' or 'end' falling in the
	 *   middle of a pcmap; in this case shorten the entry.
	 */
	TAILQ_FOREACH_SAFE(pcm, &pp->pp_map, ppm_next, pcmtmp) {
		assert(pcm->ppm_lowpc < pcm->ppm_highpc);
		if (pcm->ppm_highpc <= start)
			continue;
		if (pcm->ppm_lowpc >= end)
			return;
		if (pcm->ppm_lowpc >= start && pcm->ppm_highpc <= end) {
			/*
			 * The current pcmap is completely inside the
			 * unmapped range: remove it entirely.
			 */
			TAILQ_REMOVE(&pp->pp_map, pcm, ppm_next);
			free(pcm);
		} else if (pcm->ppm_lowpc < start && pcm->ppm_highpc > end) {
			/*
			 * Split this pcmap into two; curtail the
			 * current map to end at [start-1], and start
			 * the new one at [end].
			 */
			if ((pcmnew = malloc(sizeof(*pcmnew))) == NULL)
				err(EX_OSERR,
				    "ERROR: Cannot split a map entry");

			pcmnew->ppm_image = pcm->ppm_image;

			pcmnew->ppm_lowpc = end;
			pcmnew->ppm_highpc = pcm->ppm_highpc;

			pcm->ppm_highpc = start;

			TAILQ_INSERT_AFTER(&pp->pp_map, pcm, pcmnew, ppm_next);

			return;
		} else if (pcm->ppm_lowpc < start && pcm->ppm_highpc <= end)
			pcm->ppm_highpc = start;
		else if (pcm->ppm_lowpc >= start && pcm->ppm_highpc > end)
			pcm->ppm_lowpc = end;
		else
			assert(0);
	}
}

/*
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
 * Resolve file name and line number for the given address.
 */
int
pmcstat_image_addr2line(struct pmcstat_image *image, uintfptr_t addr,
    char *sourcefile, size_t sourcefile_len, unsigned *sourceline,
    char *funcname, size_t funcname_len)
{
	static int addr2line_warn = 0;

	char *sep, cmdline[PATH_MAX], imagepath[PATH_MAX];
	unsigned l;
	int fd;

	if (image->pi_addr2line == NULL) {
		/* Try default debug file location. */
		snprintf(imagepath, sizeof(imagepath),
		    "/usr/lib/debug/%s%s.debug",
		    args.pa_fsroot,
		    pmcstat_string_unintern(image->pi_fullpath));
		fd = open(imagepath, O_RDONLY);
		if (fd < 0) {
			/* Old kernel symbol path. */
			snprintf(imagepath, sizeof(imagepath), "%s%s.symbols",
			    args.pa_fsroot,
			    pmcstat_string_unintern(image->pi_fullpath));
			fd = open(imagepath, O_RDONLY);
			if (fd < 0) {
				snprintf(imagepath, sizeof(imagepath), "%s%s",
				    args.pa_fsroot,
				    pmcstat_string_unintern(
				        image->pi_fullpath));
			}
		}
		if (fd >= 0)
			close(fd);
		/*
		 * New addr2line support recursive inline function with -i
		 * but the format does not add a marker when no more entries
		 * are available.
		 */
		snprintf(cmdline, sizeof(cmdline), "addr2line -Cfe \"%s\"",
		    imagepath);
		image->pi_addr2line = popen(cmdline, "r+");
		if (image->pi_addr2line == NULL) {
			if (!addr2line_warn) {
				addr2line_warn = 1;
				warnx(
"WARNING: addr2line is needed for source code information."
				    );
			}
			return (0);
		}
	}

	if (feof(image->pi_addr2line) || ferror(image->pi_addr2line)) {
		warnx("WARNING: addr2line pipe error");
		pclose(image->pi_addr2line);
		image->pi_addr2line = NULL;
		return (0);
	}

	fprintf(image->pi_addr2line, "%p\n", (void *)addr);

	if (fgets(funcname, funcname_len, image->pi_addr2line) == NULL) {
		warnx("WARNING: addr2line function name read error");
		return (0);
	}
	sep = strchr(funcname, '\n');
	if (sep != NULL)
		*sep = '\0';

	if (fgets(sourcefile, sourcefile_len, image->pi_addr2line) == NULL) {
		warnx("WARNING: addr2line source file read error");
		return (0);
	}
	sep = strchr(sourcefile, ':');
	if (sep == NULL) {
		warnx("WARNING: addr2line source line separator missing");
		return (0);
	}
	*sep = '\0';
	l = atoi(sep+1);
	if (l == 0)
		return (0);
	*sourceline = l;
	return (1);
}

/*
 * Given a pmcid in use, find its human-readable name.
 */

const char *
pmcstat_pmcid_to_name(pmc_id_t pmcid)
{
	struct pmcstat_pmcrecord *pr;

	LIST_FOREACH(pr, &pmcstat_pmcs, pr_next)
	    if (pr->pr_pmcid == pmcid)
		    return (pmcstat_string_unintern(pr->pr_pmcname));

	return NULL;
}

/*
 * Convert PMC index to name.
 */

const char *
pmcstat_pmcindex_to_name(int pmcin)
{
	struct pmcstat_pmcrecord *pr;

	LIST_FOREACH(pr, &pmcstat_pmcs, pr_next)
		if (pr->pr_pmcin == pmcin)
			return pmcstat_string_unintern(pr->pr_pmcname);

	return NULL;
}

/*
 * Return PMC record with given index.
 */

struct pmcstat_pmcrecord *
pmcstat_pmcindex_to_pmcr(int pmcin)
{
	struct pmcstat_pmcrecord *pr;

	LIST_FOREACH(pr, &pmcstat_pmcs, pr_next)
		if (pr->pr_pmcin == pmcin)
			return pr;

	return NULL;
}

/*
 * Print log entries as text.
 */

static int
pmcstat_print_log(void)
{
	struct pmclog_ev ev;
	uint32_t npc;

	while (pmclog_read(args.pa_logparser, &ev) == 0) {
		assert(ev.pl_state == PMCLOG_OK);
		switch (ev.pl_type) {
		case PMCLOG_TYPE_CALLCHAIN:
			PMCSTAT_PRINT_ENTRY("callchain",
			    "%d 0x%x %d %d %c", ev.pl_u.pl_cc.pl_pid,
			    ev.pl_u.pl_cc.pl_pmcid,
			    PMC_CALLCHAIN_CPUFLAGS_TO_CPU(ev.pl_u.pl_cc. \
				pl_cpuflags), ev.pl_u.pl_cc.pl_npc,
			    PMC_CALLCHAIN_CPUFLAGS_TO_USERMODE(ev.pl_u.pl_cc.\
			        pl_cpuflags) ? 'u' : 's');
			for (npc = 0; npc < ev.pl_u.pl_cc.pl_npc; npc++)
				PMCSTAT_PRINT_ENTRY("...", "%p",
				    (void *) ev.pl_u.pl_cc.pl_pc[npc]);
			break;
		case PMCLOG_TYPE_CLOSELOG:
			PMCSTAT_PRINT_ENTRY("closelog",);
			break;
		case PMCLOG_TYPE_DROPNOTIFY:
			PMCSTAT_PRINT_ENTRY("drop",);
			break;
		case PMCLOG_TYPE_INITIALIZE:
			PMCSTAT_PRINT_ENTRY("initlog","0x%x \"%s\"",
			    ev.pl_u.pl_i.pl_version,
			    pmc_name_of_cputype(ev.pl_u.pl_i.pl_arch));
			if ((ev.pl_u.pl_i.pl_version & 0xFF000000) !=
			    PMC_VERSION_MAJOR << 24)
				warnx(
"WARNING: Log version 0x%x != expected version 0x%x.",
				    ev.pl_u.pl_i.pl_version, PMC_VERSION);
			break;
		case PMCLOG_TYPE_MAP_IN:
			PMCSTAT_PRINT_ENTRY("map-in","%d %p \"%s\"",
			    ev.pl_u.pl_mi.pl_pid,
			    (void *) ev.pl_u.pl_mi.pl_start,
			    ev.pl_u.pl_mi.pl_pathname);
			break;
		case PMCLOG_TYPE_MAP_OUT:
			PMCSTAT_PRINT_ENTRY("map-out","%d %p %p",
			    ev.pl_u.pl_mo.pl_pid,
			    (void *) ev.pl_u.pl_mo.pl_start,
			    (void *) ev.pl_u.pl_mo.pl_end);
			break;
		case PMCLOG_TYPE_PMCALLOCATE:
			PMCSTAT_PRINT_ENTRY("allocate","0x%x \"%s\" 0x%x",
			    ev.pl_u.pl_a.pl_pmcid,
			    ev.pl_u.pl_a.pl_evname,
			    ev.pl_u.pl_a.pl_flags);
			break;
		case PMCLOG_TYPE_PMCALLOCATEDYN:
			PMCSTAT_PRINT_ENTRY("allocatedyn","0x%x \"%s\" 0x%x",
			    ev.pl_u.pl_ad.pl_pmcid,
			    ev.pl_u.pl_ad.pl_evname,
			    ev.pl_u.pl_ad.pl_flags);
			break;
		case PMCLOG_TYPE_PMCATTACH:
			PMCSTAT_PRINT_ENTRY("attach","0x%x %d \"%s\"",
			    ev.pl_u.pl_t.pl_pmcid,
			    ev.pl_u.pl_t.pl_pid,
			    ev.pl_u.pl_t.pl_pathname);
			break;
		case PMCLOG_TYPE_PMCDETACH:
			PMCSTAT_PRINT_ENTRY("detach","0x%x %d",
			    ev.pl_u.pl_d.pl_pmcid,
			    ev.pl_u.pl_d.pl_pid);
			break;
		case PMCLOG_TYPE_PROCCSW:
			PMCSTAT_PRINT_ENTRY("cswval","0x%x %d %jd",
			    ev.pl_u.pl_c.pl_pmcid,
			    ev.pl_u.pl_c.pl_pid,
			    ev.pl_u.pl_c.pl_value);
			break;
		case PMCLOG_TYPE_PROCEXEC:
			PMCSTAT_PRINT_ENTRY("exec","0x%x %d %p \"%s\"",
			    ev.pl_u.pl_x.pl_pmcid,
			    ev.pl_u.pl_x.pl_pid,
			    (void *) ev.pl_u.pl_x.pl_entryaddr,
			    ev.pl_u.pl_x.pl_pathname);
			break;
		case PMCLOG_TYPE_PROCEXIT:
			PMCSTAT_PRINT_ENTRY("exitval","0x%x %d %jd",
			    ev.pl_u.pl_e.pl_pmcid,
			    ev.pl_u.pl_e.pl_pid,
			    ev.pl_u.pl_e.pl_value);
			break;
		case PMCLOG_TYPE_PROCFORK:
			PMCSTAT_PRINT_ENTRY("fork","%d %d",
			    ev.pl_u.pl_f.pl_oldpid,
			    ev.pl_u.pl_f.pl_newpid);
			break;
		case PMCLOG_TYPE_USERDATA:
			PMCSTAT_PRINT_ENTRY("userdata","0x%x",
			    ev.pl_u.pl_u.pl_userdata);
			break;
		case PMCLOG_TYPE_SYSEXIT:
			PMCSTAT_PRINT_ENTRY("exit","%d",
			    ev.pl_u.pl_se.pl_pid);
			break;
		default:
			fprintf(args.pa_printfile, "unknown event (type %d).\n",
			    ev.pl_type);
		}
	}

	if (ev.pl_state == PMCLOG_EOF)
		return (PMCSTAT_FINISHED);
	else if (ev.pl_state == PMCLOG_REQUIRE_DATA)
		return (PMCSTAT_RUNNING);

	errx(EX_DATAERR,
	    "ERROR: event parsing failed (record %jd, offset 0x%jx).",
	    (uintmax_t) ev.pl_count + 1, ev.pl_offset);
	/*NOTREACHED*/
}

/*
 * Public Interfaces.
 */

/*
 * Process a log file in offline analysis mode.
 */

int
pmcstat_process_log(void)
{

	/*
	 * If analysis has not been asked for, just print the log to
	 * the current output file.
	 */
	if (args.pa_flags & FLAG_DO_PRINT)
		return (pmcstat_print_log());
	else
		return (pmcstat_analyze_log(&args, plugins, &pmcstat_stats, pmcstat_kernproc,
		    pmcstat_mergepmc, &pmcstat_npmcs, &ps_samples_period));
}

/*
 * Refresh top display.
 */

static void
pmcstat_refresh_top(void)
{
	int v_attrs;
	float v;
	char pmcname[40];
	struct pmcstat_pmcrecord *pmcpr;

	/* If in pause mode do not refresh display. */
	if (pmcstat_pause)
		return;

	/* Wait until PMC pop in the log. */
	pmcpr = pmcstat_pmcindex_to_pmcr(pmcstat_pmcinfilter);
	if (pmcpr == NULL)
		return;

	/* Format PMC name. */
	if (pmcstat_mergepmc)
		snprintf(pmcname, sizeof(pmcname), "[%s]",
		    pmcstat_string_unintern(pmcpr->pr_pmcname));
	else
		snprintf(pmcname, sizeof(pmcname), "%s.%d",
		    pmcstat_string_unintern(pmcpr->pr_pmcname),
		    pmcstat_pmcinfilter);

	/* Format samples count. */
	if (ps_samples_period > 0)
		v = (pmcpr->pr_samples * 100.0) / ps_samples_period;
	else
		v = 0.;
	v_attrs = PMCSTAT_ATTRPERCENT(v);

	PMCSTAT_PRINTBEGIN();
	PMCSTAT_PRINTW("PMC: %s Samples: %u ",
	    pmcname,
	    pmcpr->pr_samples);
	PMCSTAT_ATTRON(v_attrs);
	PMCSTAT_PRINTW("(%.1f%%) ", v);
	PMCSTAT_ATTROFF(v_attrs);
	PMCSTAT_PRINTW(", %u unresolved\n\n",
	    pmcpr->pr_dubious_frames);
	if (plugins[args.pa_plugin].pl_topdisplay != NULL)
		plugins[args.pa_plugin].pl_topdisplay();
	PMCSTAT_PRINTEND();
}

/*
 * Find the next pmc index to display.
 */

static void
pmcstat_changefilter(void)
{
	int pmcin;
	struct pmcstat_pmcrecord *pmcr;

	/*
	 * Find the next merge target.
	 */
	if (pmcstat_mergepmc) {
		pmcin = pmcstat_pmcinfilter;

		do {
			pmcr = pmcstat_pmcindex_to_pmcr(pmcstat_pmcinfilter);
			if (pmcr == NULL || pmcr == pmcr->pr_merge)
				break;

			pmcstat_pmcinfilter++;
			if (pmcstat_pmcinfilter >= pmcstat_npmcs)
				pmcstat_pmcinfilter = 0;

		} while (pmcstat_pmcinfilter != pmcin);
	}
}

/*
 * Top mode keypress.
 */

int
pmcstat_keypress_log(void)
{
	int c, ret = 0;
	WINDOW *w;

	w = newwin(1, 0, 1, 0);
	c = wgetch(w);
	wprintw(w, "Key: %c => ", c);
	switch (c) {
	case 'c':
		wprintw(w, "enter mode 'd' or 'a' => ");
		c = wgetch(w);
		if (c == 'd') {
			args.pa_topmode = PMCSTAT_TOP_DELTA;
			wprintw(w, "switching to delta mode");
		} else {
			args.pa_topmode = PMCSTAT_TOP_ACCUM;
			wprintw(w, "switching to accumulation mode");
		}
		break;
	case 'm':
		pmcstat_mergepmc = !pmcstat_mergepmc;
		/*
		 * Changing merge state require data reset.
		 */
		if (plugins[args.pa_plugin].pl_shutdown != NULL)
			plugins[args.pa_plugin].pl_shutdown(NULL);
		pmcstat_stats_reset(0);
		if (plugins[args.pa_plugin].pl_init != NULL)
			plugins[args.pa_plugin].pl_init();

		/* Update filter to be on a merge target. */
		pmcstat_changefilter();
		wprintw(w, "merge PMC %s", pmcstat_mergepmc ? "on" : "off");
		break;
	case 'n':
		/* Close current plugin. */
		if (plugins[args.pa_plugin].pl_shutdown != NULL)
			plugins[args.pa_plugin].pl_shutdown(NULL);

		/* Find next top display available. */
		do {
			args.pa_plugin++;
			if (plugins[args.pa_plugin].pl_name == NULL)
				args.pa_plugin = 0;
		} while (plugins[args.pa_plugin].pl_topdisplay == NULL);

		/* Open new plugin. */
		pmcstat_stats_reset(0);
		if (plugins[args.pa_plugin].pl_init != NULL)
			plugins[args.pa_plugin].pl_init();
		wprintw(w, "switching to plugin %s",
		    plugins[args.pa_plugin].pl_name);
		break;
	case 'p':
		pmcstat_pmcinfilter++;
		if (pmcstat_pmcinfilter >= pmcstat_npmcs)
			pmcstat_pmcinfilter = 0;
		pmcstat_changefilter();
		wprintw(w, "switching to PMC %s.%d",
		    pmcstat_pmcindex_to_name(pmcstat_pmcinfilter),
		    pmcstat_pmcinfilter);
		break;
	case ' ':
		pmcstat_pause = !pmcstat_pause;
		if (pmcstat_pause)
			wprintw(w, "pause => press space again to continue");
		break;
	case 'q':
		wprintw(w, "exiting...");
		ret = 1;
		break;
	default:
		if (plugins[args.pa_plugin].pl_topkeypress != NULL)
			if (plugins[args.pa_plugin].pl_topkeypress(c, (void *)w))
				ret = 1;
	}

	wrefresh(w);
	delwin(w);
	return ret;
}


/*
 * Top mode display.
 */

void
pmcstat_display_log(void)
{

	pmcstat_refresh_top();

	/* Reset everythings if delta mode. */
	if (args.pa_topmode == PMCSTAT_TOP_DELTA) {
		if (plugins[args.pa_plugin].pl_shutdown != NULL)
			plugins[args.pa_plugin].pl_shutdown(NULL);
		pmcstat_stats_reset(0);
		if (plugins[args.pa_plugin].pl_init != NULL)
			plugins[args.pa_plugin].pl_init();
	}
}

/*
 * Configure a plugins.
 */

void
pmcstat_pluginconfigure_log(char *opt)
{

	if (strncmp(opt, "threshold=", 10) == 0) {
		pmcstat_threshold = atof(opt+10);
	} else {
		if (plugins[args.pa_plugin].pl_configure != NULL) {
			if (!plugins[args.pa_plugin].pl_configure(opt))
				err(EX_USAGE,
				    "ERROR: unknown option <%s>.", opt);
		}
	}
}

void
pmcstat_log_shutdown_logging(void)
{

	pmcstat_shutdown_logging(&args, plugins, &pmcstat_stats);
}

void
pmcstat_log_initialize_logging(void)
{

	pmcstat_initialize_logging(&pmcstat_kernproc,
	    &args, plugins, &pmcstat_npmcs, &pmcstat_mergepmc);
}
