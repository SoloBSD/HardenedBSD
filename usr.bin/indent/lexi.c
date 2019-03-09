/*-
<<<<<<< HEAD
 * SPDX-License-Identifier: BSD-4-Clause
 *
=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
 * Copyright (c) 1985 Sun Microsystems, Inc.
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if 0
#ifndef lint
static char sccsid[] = "@(#)lexi.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */
#endif
#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

/*
 * Here we have the token scanner for indent.  It scans off one token and puts
 * it in the global variable "token".  It returns a code, indicating the type
 * of token scanned.
 */

#include <err.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#include "indent_globs.h"
#include "indent_codes.h"
#include "indent.h"

<<<<<<< HEAD
=======
#define alphanum 1
#ifdef undef
#define opchar 3
#endif

>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
struct templ {
    const char *rwd;
    int         rwcode;
};

/*
 * This table has to be sorted alphabetically, because it'll be used in binary
 * search. For the same reason, string must be the first thing in struct templ.
 */
struct templ specials[] =
{
<<<<<<< HEAD
    {"_Bool", 4},
    {"_Complex", 4},
    {"_Imaginary", 4},
    {"auto", 10},
    {"bool", 4},
    {"break", 9},
    {"case", 8},
=======
    {"switch", 7},
    {"case", 8},
    {"break", 9},
    {"struct", 3},
    {"union", 3},
    {"enum", 3},
    {"default", 8},
    {"int", 4},
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
    {"char", 4},
    {"complex", 4},
    {"const", 4},
    {"continue", 12},
    {"default", 8},
    {"do", 6},
    {"double", 4},
    {"else", 6},
    {"enum", 3},
    {"extern", 10},
    {"float", 4},
    {"for", 5},
    {"global", 4},
    {"goto", 9},
    {"if", 5},
    {"imaginary", 4},
    {"inline", 12},
    {"int", 4},
    {"long", 4},
    {"offsetof", 1},
    {"register", 10},
    {"restrict", 12},
    {"return", 9},
    {"short", 4},
    {"signed", 4},
    {"sizeof", 2},
    {"static", 10},
    {"struct", 3},
    {"switch", 7},
    {"typedef", 11},
    {"union", 3},
    {"unsigned", 4},
    {"void", 4},
    {"volatile", 4},
<<<<<<< HEAD
    {"while", 5}
=======
    {"goto", 9},
    {"return", 9},
    {"if", 5},
    {"while", 5},
    {"for", 5},
    {"else", 6},
    {"do", 6},
    {"sizeof", 2},
    {"offsetof", 1},
    {0, 0}
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
};

const char **typenames;
int         typename_count;
int         typename_top = -1;

/*
 * The transition table below was rewritten by hand from lx's output, given
 * the following definitions. lx is Katherine Flavel's lexer generator.
 *
 * O  = /[0-7]/;        D  = /[0-9]/;          NZ = /[1-9]/;
 * H  = /[a-f0-9]/i;    B  = /[0-1]/;          HP = /0x/i;
 * BP = /0b/i;          E  = /e[+\-]?/i D+;    P  = /p[+\-]?/i D+;
 * FS = /[fl]/i;        IS = /u/i /(l|L|ll|LL)/? | /(l|L|ll|LL)/ /u/i?;
 *
 * D+           E  FS? -> $float;
 * D*    "." D+ E? FS? -> $float;
 * D+    "."    E? FS? -> $float;    HP H+           IS? -> $int;
 * HP H+        P  FS? -> $float;    NZ D*           IS? -> $int;
 * HP H* "." H+ P  FS? -> $float;    "0" O*          IS? -> $int;
 * HP H+ "."    P  FS  -> $float;    BP B+           IS? -> $int;
 */
static char const *table[] = {
    /*                examples:
                                     00
             s                      0xx
             t                    00xaa
             a     11       101100xxa..
             r   11ee0001101lbuuxx.a.pp
             t.01.e+008bLuxll0Ll.aa.p+0
    states:  ABCDEFGHIJKLMNOPQRSTUVWXYZ */
    ['0'] = "CEIDEHHHIJQ  U  Q  VUVVZZZ",
    ['1'] = "DEIDEHHHIJQ  U  Q  VUVVZZZ",
    ['7'] = "DEIDEHHHIJ   U     VUVVZZZ",
    ['9'] = "DEJDEHHHJJ   U     VUVVZZZ",
    ['a'] = "             U     VUVV   ",
    ['b'] = "  K          U     VUVV   ",
    ['e'] = "  FFF   FF   U     VUVV   ",
    ['f'] = "    f  f     U     VUVV  f",
    ['u'] = "  MM    M  i  iiM   M     ",
    ['x'] = "  N                       ",
    ['p'] = "                    FFX   ",
    ['L'] = "  LLf  fL  PR   Li  L    f",
    ['l'] = "  OOf  fO   S P O i O    f",
    ['+'] = "     G                 Y  ",
    ['.'] = "B EE    EE   T      W     ",
    /*       ABCDEFGHIJKLMNOPQRSTUVWXYZ */
    [0]   = "uuiifuufiuuiiuiiiiiuiuuuuu",
};

static int
strcmp_type(const void *e1, const void *e2)
{
    return (strcmp(e1, *(const char * const *)e2));
}

int
lexi(struct parser_state *state)
{
    int         unary_delim;	/* this is set to 1 if the current token
				 * forces a following operator to be unary */
    int         code;		/* internal code to be returned */
    char        qchar;		/* the delimiter character for a string */

    e_token = s_token;		/* point to start of place to save token */
    unary_delim = false;
    state->col_1 = state->last_nl;	/* tell world that this token started
					 * in column 1 iff the last thing
					 * scanned was a newline */
    state->last_nl = false;

    while (*buf_ptr == ' ' || *buf_ptr == '\t') {	/* get rid of blanks */
	state->col_1 = false;	/* leading blanks imply token is not in column
				 * 1 */
	if (++buf_ptr >= buf_end)
	    fill_buffer();
    }

    /* Scan an alphanumeric token */
    if (isalnum((unsigned char)*buf_ptr) ||
	*buf_ptr == '_' || *buf_ptr == '$' ||
	(buf_ptr[0] == '.' && isdigit((unsigned char)buf_ptr[1]))) {
	/*
	 * we have a character or number
	 */
	struct templ *p;

<<<<<<< HEAD
	if (isdigit((unsigned char)*buf_ptr) ||
	    (buf_ptr[0] == '.' && isdigit((unsigned char)buf_ptr[1]))) {
	    char s;
	    unsigned char i;

	    for (s = 'A'; s != 'f' && s != 'i' && s != 'u'; ) {
		i = (unsigned char)*buf_ptr;
		if (i >= nitems(table) || table[i] == NULL ||
		    table[i][s - 'A'] == ' ') {
		    s = table[0][s - 'A'];
		    break;
		}
		s = table[i][s - 'A'];
		CHECK_SIZE_TOKEN(1);
		*e_token++ = *buf_ptr++;
		if (buf_ptr >= buf_end)
		    fill_buffer();
	    }
	    /* s now indicates the type: f(loating), i(integer), u(nknown) */
=======
	if (isdigit(*buf_ptr) || (buf_ptr[0] == '.' && isdigit(buf_ptr[1]))) {
	    enum base {
		BASE_2, BASE_8, BASE_10, BASE_16
	    };
	    int         seendot = 0,
	                seenexp = 0,
			seensfx = 0;
	    enum base	in_base = BASE_10;

	    if (*buf_ptr == '0') {
		if (buf_ptr[1] == 'b' || buf_ptr[1] == 'B')
		    in_base = BASE_2;
		else if (buf_ptr[1] == 'x' || buf_ptr[1] == 'X')
		    in_base = BASE_16;
		else if (isdigit(buf_ptr[1]))
		    in_base = BASE_8;
	    }
	    switch (in_base) {
	    case BASE_2:
		*e_token++ = *buf_ptr++;
		*e_token++ = *buf_ptr++;
		while (*buf_ptr == '0' || *buf_ptr == '1') {
		    CHECK_SIZE_TOKEN;
		    *e_token++ = *buf_ptr++;
		}
		break;
	    case BASE_8:
		*e_token++ = *buf_ptr++;
		while (*buf_ptr >= '0' && *buf_ptr <= '8') {
		    CHECK_SIZE_TOKEN;
		    *e_token++ = *buf_ptr++;
		}
		break;
	    case BASE_16:
		*e_token++ = *buf_ptr++;
		*e_token++ = *buf_ptr++;
		while (isxdigit(*buf_ptr)) {
		    CHECK_SIZE_TOKEN;
		    *e_token++ = *buf_ptr++;
		}
		break;
	    case BASE_10:
		while (1) {
		    if (*buf_ptr == '.') {
			if (seendot)
			    break;
			else
			    seendot++;
		    }
		    CHECK_SIZE_TOKEN;
		    *e_token++ = *buf_ptr++;
		    if (!isdigit(*buf_ptr) && *buf_ptr != '.') {
			if ((*buf_ptr != 'E' && *buf_ptr != 'e') || seenexp)
			    break;
			else {
			    seenexp++;
			    seendot++;
			    CHECK_SIZE_TOKEN;
			    *e_token++ = *buf_ptr++;
			    if (*buf_ptr == '+' || *buf_ptr == '-')
				*e_token++ = *buf_ptr++;
			}
		    }
		}
		break;
	    }
	    while (1) {
		if (!(seensfx & 1) && (*buf_ptr == 'U' || *buf_ptr == 'u')) {
		    CHECK_SIZE_TOKEN;
		    *e_token++ = *buf_ptr++;
		    seensfx |= 1;
		    continue;
		}
		if (!(seensfx & 2) && (strchr("fFlL", *buf_ptr) != NULL)) {
		    CHECK_SIZE_TOKEN;
		    if (buf_ptr[1] == buf_ptr[0])
		        *e_token++ = *buf_ptr++;
		    *e_token++ = *buf_ptr++;
		    seensfx |= 2;
		    continue;
		}
		break;
	    }
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
	}
	else
	    while (isalnum((unsigned char)*buf_ptr) ||
	        *buf_ptr == BACKSLASH ||
		*buf_ptr == '_' || *buf_ptr == '$') {
		/* fill_buffer() terminates buffer with newline */
		if (*buf_ptr == BACKSLASH) {
		    if (*(buf_ptr + 1) == '\n') {
			buf_ptr += 2;
			if (buf_ptr >= buf_end)
			    fill_buffer();
			} else
			    break;
		}
		CHECK_SIZE_TOKEN(1);
		/* copy it over */
		*e_token++ = *buf_ptr++;
		if (buf_ptr >= buf_end)
		    fill_buffer();
	    }
<<<<<<< HEAD
	*e_token = '\0';
=======
	*e_token++ = '\0';
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2

	if (s_token[0] == 'L' && s_token[1] == '\0' &&
	      (*buf_ptr == '"' || *buf_ptr == '\''))
	    return (strpfx);

	while (*buf_ptr == ' ' || *buf_ptr == '\t') {	/* get rid of blanks */
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}
<<<<<<< HEAD
	state->keyword = 0;
	if (state->last_token == structure && !state->p_l_follow) {
=======
	ps.keyword = 0;
	if (l_struct && !ps.p_l_follow) {
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
				/* if last token was 'struct' and we're not
				 * in parentheses, then this token
				 * should be treated as a declaration */
	    state->last_u_d = true;
	    return (decl);
	}
<<<<<<< HEAD
=======
	ps.last_u_d = l_struct;	/* Operator after identifier is binary
				 * unless last token was 'struct' */
	l_struct = false;
	last_code = ident;	/* Remember that this is the code we will
				 * return */

	if (auto_typedefs) {
	    const char *q = s_token;
	    size_t q_len = strlen(q);
	    /* Check if we have an "_t" in the end */
	    if (q_len > 2 &&
	        (strcmp(q + q_len - 2, "_t") == 0)) {
	        ps.keyword = 4;	/* a type name */
		ps.last_u_d = true;
	        goto found_auto_typedef;
	    }
	}

>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
	/*
	 * Operator after identifier is binary unless last token was 'struct'
	 */
<<<<<<< HEAD
	state->last_u_d = (state->last_token == structure);

	p = bsearch(s_token,
	    specials,
	    sizeof(specials) / sizeof(specials[0]),
	    sizeof(specials[0]),
	    strcmp_type);
	if (p == NULL) {	/* not a special keyword... */
	    char *u;

	    /* ... so maybe a type_t or a typedef */
	    if ((opt.auto_typedefs && ((u = strrchr(s_token, '_')) != NULL) &&
	        strcmp(u, "_t") == 0) || (typename_top >= 0 &&
		  bsearch(s_token, typenames, typename_top + 1,
		    sizeof(typenames[0]), strcmp_type))) {
		state->keyword = 4;	/* a type name */
		state->last_u_d = true;
	        goto found_typename;
	    }
	} else {			/* we have a keyword */
	    state->keyword = p->rwcode;
	    state->last_u_d = true;
=======
	for (p = specials; (j = p->rwd) != NULL; p++) {
	    const char *q = s_token;	/* point at scanned token */
	    if (*j++ != *q++ || *j++ != *q++)
		continue;	/* This test depends on the fact that
				 * identifiers are always at least 1 character
				 * long (ie. the first two bytes of the
				 * identifier are always meaningful) */
	    if (q[-1] == 0)
		break;		/* If its a one-character identifier */
	    while (*q++ == *j)
		if (*j++ == 0)
		    goto found_keyword;	/* I wish that C had a multi-level
					 * break... */
	}
	if (p->rwd) {		/* we have a keyword */
    found_keyword:
	    ps.keyword = p->rwcode;
	    ps.last_u_d = true;
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
	    switch (p->rwcode) {
	    case 7:		/* it is a switch */
		return (swstmt);
	    case 8:		/* a case or default */
		return (casestmt);

	    case 3:		/* a "struct" */
		/* FALLTHROUGH */
	    case 4:		/* one of the declaration keywords */
<<<<<<< HEAD
	    found_typename:
		if (state->p_l_follow) {
		    /* inside parens: cast, param list, offsetof or sizeof */
		    state->cast_mask |= (1 << state->p_l_follow) & ~state->not_cast_mask;
=======
	    found_auto_typedef:
		if (ps.p_l_follow) {
		    /* inside parens: cast, param list, offsetof or sizeof */
		    ps.cast_mask |= (1 << ps.p_l_follow) & ~ps.not_cast_mask;
		    break;
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
		}
		if (state->last_token == period || state->last_token == unary_op) {
		    state->keyword = 0;
		    break;
		}
		if (p != NULL && p->rwcode == 3)
		    return (structure);
		if (state->p_l_follow)
		    break;
		return (decl);

	    case 5:		/* if, while, for */
		return (sp_paren);

	    case 6:		/* do, else */
		return (sp_nparen);

<<<<<<< HEAD
	    case 10:		/* storage class specifier */
		return (storage);

	    case 11:		/* typedef */
		return (type_def);

=======
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
	    default:		/* all others are treated like any other
				 * identifier */
		return (ident);
	    }			/* end of switch */
	}			/* end of if (found_it) */
	if (*buf_ptr == '(' && state->tos <= 1 && state->ind_level == 0 &&
	    state->in_parameter_declaration == 0 && state->block_init == 0) {
	    char *tp = buf_ptr;
	    while (tp < buf_end)
		if (*tp++ == ')' && (*tp == ';' || *tp == ','))
		    goto not_proc;
	    strncpy(state->procname, token, sizeof state->procname - 1);
	    if (state->in_decl)
		state->in_parameter_declaration = 1;
	    return (funcname);
    not_proc:;
	}
	/*
	 * The following hack attempts to guess whether or not the current
	 * token is in fact a declaration keyword -- one that has been
	 * typedefd
	 */
<<<<<<< HEAD
	else if (!state->p_l_follow && !state->block_init &&
	    !state->in_stmt &&
	    ((*buf_ptr == '*' && buf_ptr[1] != '=') ||
		isalpha((unsigned char)*buf_ptr)) &&
	    (state->last_token == semicolon || state->last_token == lbrace ||
		state->last_token == rbrace)) {
	    state->keyword = 4;	/* a type name */
	    state->last_u_d = true;
=======
	if (((*buf_ptr == '*' && buf_ptr[1] != '=') || isalpha(*buf_ptr) || *buf_ptr == '_')
		&& !ps.p_l_follow
	        && !ps.block_init
		&& (ps.last_token == rparen || ps.last_token == semicolon ||
		    ps.last_token == decl ||
		    ps.last_token == lbrace || ps.last_token == rbrace)) {
	    ps.keyword = 4;	/* a type name */
	    ps.last_u_d = true;
	    last_code = decl;
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
	    return decl;
	}
	if (state->last_token == decl)	/* if this is a declared variable,
					 * then following sign is unary */
	    state->last_u_d = true;	/* will make "int a -1" work */
	return (ident);		/* the ident is not in the list */
    }				/* end of procesing for alpanum character */

    /* Scan a non-alphanumeric token */

    CHECK_SIZE_TOKEN(3);		/* things like "<<=" */
    *e_token++ = *buf_ptr;		/* if it is only a one-character token, it is
				 * moved here */
    *e_token = '\0';
    if (++buf_ptr >= buf_end)
	fill_buffer();

    switch (*token) {
    case '\n':
	unary_delim = state->last_u_d;
	state->last_nl = true;	/* remember that we just had a newline */
	code = (had_eof ? 0 : newline);

	/*
	 * if data has been exhausted, the newline is a dummy, and we should
	 * return code to stop
	 */
	break;

    case '\'':			/* start of quoted character */
    case '"':			/* start of string */
	qchar = *token;
	do {			/* copy the string */
	    while (1) {		/* move one character or [/<char>]<char> */
		if (*buf_ptr == '\n') {
		    diag2(1, "Unterminated literal");
		    goto stop_lit;
		}
		CHECK_SIZE_TOKEN(2);
		*e_token = *buf_ptr++;
		if (buf_ptr >= buf_end)
		    fill_buffer();
		if (*e_token == BACKSLASH) {	/* if escape, copy extra char */
		    if (*buf_ptr == '\n')	/* check for escaped newline */
			++line_no;
		    *++e_token = *buf_ptr++;
		    ++e_token;	/* we must increment this again because we
				 * copied two chars */
		    if (buf_ptr >= buf_end)
			fill_buffer();
		}
		else
		    break;	/* we copied one character */
	    }			/* end of while (1) */
	} while (*e_token++ != qchar);
stop_lit:
	code = ident;
	break;

    case ('('):
    case ('['):
	unary_delim = true;
	code = lparen;
	break;

    case (')'):
    case (']'):
	code = rparen;
	break;

    case '#':
	unary_delim = state->last_u_d;
	code = preesc;
	break;

    case '?':
	unary_delim = true;
	code = question;
	break;

    case (':'):
	code = colon;
	unary_delim = true;
	break;

    case (';'):
	unary_delim = true;
	code = semicolon;
	break;

    case ('{'):
	unary_delim = true;

	/*
	 * if (state->in_or_st) state->block_init = 1;
	 */
	/* ?	code = state->block_init ? lparen : lbrace; */
	code = lbrace;
	break;

    case ('}'):
	unary_delim = true;
	/* ?	code = state->block_init ? rparen : rbrace; */
	code = rbrace;
	break;

    case 014:			/* a form feed */
	unary_delim = state->last_u_d;
	state->last_nl = true;	/* remember this so we can set 'state->col_1'
				 * right */
	code = form_feed;
	break;

    case (','):
	unary_delim = true;
	code = comma;
	break;

    case '.':
	unary_delim = false;
	code = period;
	break;

    case '-':
    case '+':			/* check for -, +, --, ++ */
	code = (state->last_u_d ? unary_op : binary_op);
	unary_delim = true;

	if (*buf_ptr == token[0]) {
	    /* check for doubled character */
	    *e_token++ = *buf_ptr++;
	    /* buffer overflow will be checked at end of loop */
	    if (state->last_token == ident || state->last_token == rparen) {
		code = (state->last_u_d ? unary_op : postop);
		/* check for following ++ or -- */
		unary_delim = false;
	    }
	}
	else if (*buf_ptr == '=')
	    /* check for operator += */
	    *e_token++ = *buf_ptr++;
	else if (*buf_ptr == '>') {
	    /* check for operator -> */
	    *e_token++ = *buf_ptr++;
	    unary_delim = false;
	    code = unary_op;
	    state->want_blank = false;
	}
	break;			/* buffer overflow will be checked at end of
				 * switch */

    case '=':
	if (state->in_or_st)
	    state->block_init = 1;
	if (*buf_ptr == '=') {/* == */
	    *e_token++ = '=';	/* Flip =+ to += */
	    buf_ptr++;
	    *e_token = 0;
	}
	code = binary_op;
	unary_delim = true;
	break;
	/* can drop thru!!! */

    case '>':
    case '<':
    case '!':			/* ops like <, <<, <=, !=, etc */
	if (*buf_ptr == '>' || *buf_ptr == '<' || *buf_ptr == '=') {
	    *e_token++ = *buf_ptr;
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}
	if (*buf_ptr == '=')
	    *e_token++ = *buf_ptr++;
	code = (state->last_u_d ? unary_op : binary_op);
	unary_delim = true;
	break;

    case '*':
	unary_delim = true;
	if (!state->last_u_d) {
	    if (*buf_ptr == '=')
		*e_token++ = *buf_ptr++;
	    code = binary_op;
	    break;
	}
	while (*buf_ptr == '*' || isspace((unsigned char)*buf_ptr)) {
	    if (*buf_ptr == '*') {
		CHECK_SIZE_TOKEN(1);
		*e_token++ = *buf_ptr;
	    }
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}
	if (ps.in_decl) {
	    char *tp = buf_ptr;

	    while (isalpha((unsigned char)*tp) ||
		   isspace((unsigned char)*tp)) {
		if (++tp >= buf_end)
		    fill_buffer();
	    }
	    if (*tp == '(')
		ps.procname[0] = ' ';
	}
	code = unary_op;
	break;

    default:
	if (token[0] == '/' && *buf_ptr == '*') {
	    /* it is start of comment */
	    *e_token++ = '*';

	    if (++buf_ptr >= buf_end)
		fill_buffer();

	    code = comment;
	    unary_delim = state->last_u_d;
	    break;
	}
	while (*(e_token - 1) == *buf_ptr || *buf_ptr == '=') {
	    /*
	     * handle ||, &&, etc, and also things as in int *****i
	     */
	    CHECK_SIZE_TOKEN(1);
	    *e_token++ = *buf_ptr;
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}
	code = (state->last_u_d ? unary_op : binary_op);
	unary_delim = true;


    }				/* end of switch */
    if (buf_ptr >= buf_end)	/* check for input buffer empty */
	fill_buffer();
    state->last_u_d = unary_delim;
    CHECK_SIZE_TOKEN(1);
    *e_token = '\0';		/* null terminate the token */
    return (code);
}

/* Initialize constant transition table */
void
init_constant_tt(void)
{
    table['-'] = table['+'];
    table['8'] = table['9'];
    table['2'] = table['3'] = table['4'] = table['5'] = table['6'] = table['7'];
    table['A'] = table['C'] = table['D'] = table['c'] = table['d'] = table['a'];
    table['B'] = table['b'];
    table['E'] = table['e'];
    table['U'] = table['u'];
    table['X'] = table['x'];
    table['P'] = table['p'];
    table['F'] = table['f'];
}

void
alloc_typenames(void)
{

    typenames = (const char **)malloc(sizeof(typenames[0]) *
        (typename_count = 16));
    if (typenames == NULL)
	err(1, NULL);
}

void
add_typename(const char *key)
{
    int comparison;
    const char *copy;

    if (typename_top + 1 >= typename_count) {
	typenames = realloc((void *)typenames,
	    sizeof(typenames[0]) * (typename_count *= 2));
	if (typenames == NULL)
	    err(1, NULL);
    }
    if (typename_top == -1)
	typenames[++typename_top] = copy = strdup(key);
    else if ((comparison = strcmp(key, typenames[typename_top])) >= 0) {
	/* take advantage of sorted input */
	if (comparison == 0)	/* remove duplicates */
	    return;
<<<<<<< HEAD
	typenames[++typename_top] = copy = strdup(key);
    }
    else {
	int p;

	for (p = 0; (comparison = strcmp(key, typenames[p])) > 0; p++)
	    /* find place for the new key */;
	if (comparison == 0)	/* remove duplicates */
	    return;
	memmove(&typenames[p + 1], &typenames[p],
	    sizeof(typenames[0]) * (++typename_top - p));
	typenames[p] = copy = strdup(key);
    }

    if (copy == NULL)
	err(1, NULL);
=======
	else
	    p++;
    if (p >= specials + sizeof specials / sizeof specials[0])
	return;			/* For now, table overflows are silently
				 * ignored */
    p->rwd = key;
    p->rwcode = val;
    p[1].rwd = NULL;
    p[1].rwcode = 0;
>>>>>>> 930409367ecf72a59ee5666730e1b84ac90527b2
}
