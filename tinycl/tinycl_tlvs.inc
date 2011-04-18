// -*- Mode: C++; -*-
// tinycl_tlvs.inc
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//

#include "./tinycl_begin_list.h"

//////////////////////////////////////////////////////////////////////
//
// TLV
//
deftlv(cl, A,                           nil)
deftlv(cl, AA,                          nil)
deftlv(cl, AAA,                         nil)
deftlv(cl, Abreak_on_signalsA,          nil)
deftlv(cl, Acompile_file_pathnameA,     nil)
deftlv(cl, Acompile_file_truenameA,     nil)
deftlv(cl, Acompile_printA,             nil)
deftlv(cl, Acompile_verboseA,           t)
deftlv(cl, Adebugger_hookA,             nil)
deftlv(cl, Adebug_ioA,                  nil)
deftlv(cl, Adefault_pathname_defaultsA, nil)
deftlv(cl, Aerror_outputA,              nil)
deftlv(cl, AfeaturesA,                  list(Kansi_cl, Kcommon_lisp, Kieee_floating_point))
deftlv(cl, Agensym_counterA,            one)
deftlv(cl, Aload_pathnameA,             nil)
deftlv(cl, Aload_printA,                nil)
deftlv(cl, Aload_truenameA,             nil)
deftlv(cl, Aload_verboseA,              t)
deftlv(cl, Amacroexpand_hookA,          nil)
deftlv(cl, AmodulesA,                   nil)
deftlv(cl, ApackageA,                   PKG_cl_user)
deftlv(cl, Aprint_arrayA,               nil)
deftlv(cl, Aprint_baseA,                Fixnum::Encode(10))

#if MODERN_MODE
    deftlv(cl, Aprint_caseA,                Kdowncase)
#else
    deftlv(cl, Aprint_caseA,                Kupcase)
#endif // MODERN_MODE

deftlv(cl, Aprint_circleA,              nil)
deftlv(cl, Aprint_escapeA,              t)
deftlv(cl, Aprint_gensymA,              t)
deftlv(cl, Aprint_lengthA,              nil)
deftlv(cl, Aprint_levelA,               nil)
deftlv(cl, Aprint_linesA,               nil)
deftlv(cl, Aprint_miser_widthA,         nil)
deftlv(cl, Aprint_pprint_dispatchA,     nil)
deftlv(cl, Aprint_prettyA,              nil)
deftlv(cl, Aprint_radixA,               nil)
deftlv(cl, Aprint_readablyA,            nil)
deftlv(cl, Aprint_right_marginA,        nil)
deftlv(cl, Aquery_ioA,                  nil)
deftlv(cl, Arandom_stateA,              nil)
deftlv(cl, Aread_baseA,                 Fixnum::Encode(10))
deftlv(cl, Aread_default_float_formatA, Qsingle_float)
deftlv(cl, Aread_evalA,                 t)
deftlv(cl, Aread_suppressA,             nil)
deftlv(cl, AreadtableA,                 VAR(Astandard_readtableA))
deftlv(cl, Astandard_inputA,            nil)
deftlv(cl, Astandard_outputA,           nil)
deftlv(cl, Aterminal_ioA,               nil)
deftlv(cl, Atrace_outputA,              nil)
deftlv(cl, S,                           nil)    // /
deftlv(cl, SS,                          nil)    // //
deftlv(cl, SSS,                         nil)    // ///
deftlv(cl, P,                           nil)    // +
deftlv(cl, PP,                          nil)    // ++
deftlv(cl, PPP,                         nil)    // +++
deftlv(cl, _,                           nil)    // -

deftlv(si, AenvironmentA,               VAR(Aruntime_environmentA))
deftlv(si, Aexternal_formatA,           VAR(Adefault_external_formatA))
deftlv(si, Aread_backquoteA,            zero)
deftlv(si, Aread_line_number_tableA,    nil)
deftlv(si, Aread_start_line_numberA,    nil)
deftlv(si, Arestart_clustersA,          nil)

// Variables used by compiler
deftlv(c, c_AcontextA,          nil)
deftlv(c, c_AoptionsA,          nil)
deftlv(c, c_Aprocessing_modeA,  nil)
deftlv(c, c_AsituationA,        nil)

// Variables used by developer
deftlv(devel, Acmdl_levelA, zero)
deftlv(devel, AconditionA,  nil)

#include "./tinycl_end_list.h"
