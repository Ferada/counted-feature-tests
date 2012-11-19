<!-- -*- mode: markdown; coding: utf-8; -*- -->

COUNTED-FEATURE-TESTS - Numeric prefix for standard CL #+ and #- reader
macros.

Copyright (C) 2012 Olof-Joachim Frahm

Released as Public Domain.  Uses two definitions from SBCL:

    ;;;; This software is part of the SBCL system. See the README file for
    ;;;; more information.
    ;;;;
    ;;;; This software is derived from the CMU CL system, which was
    ;;;; written at Carnegie Mellon University and released into the
    ;;;; public domain. The software is in the public domain and is
    ;;;; provided with absolutely no warranty. See the COPYING and CREDITS
    ;;;; files for more information.

Uses fiveam for defining tests.

# USAGE

Uses same semantics as CLSQLs reader macros:  Use
`ENABLE-COUNTED-FEATURE-TESTS-SYNTAX` to enable and `DISABLE-` to
disable the syntax.  `LOCALLY-ENABLE-` / `LOCALLY-DISABLE-` to switch
until `RESTORE-COUNTED-FEATURE-TESTS-SYNTAX` is called.
`FILE-ENABLE-COUNTED-FEATURE-TESTS-SYNTAX` to enable it just for a
single file.

The syntax enables a numeric prefix to the `#+` and `#-` (sharp-sign
plus and minus) reader macros (positive of course), so that those skip
that exact number of next forms.  This helps e.g. during ASDF
definitions, feature-dependent keyword arguments and so on.

# EXAMPLES

    ;; (list #2+(or)
    ;;       :a-keyword-argument and-its-value
    ;;       'other 'stuff)
    ;; => (OTHER STUFF)

    ;; #0+is 'useless
    ;; => USELESS

    ;; #+(and) 42
    ;; OR #1+(and) 42
    ;; => 42
