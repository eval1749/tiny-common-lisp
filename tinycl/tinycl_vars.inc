// -*- Mode: C++; -*-
// tinycl_vars.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_vars.inc#13 $
//

#include "./tinycl_begin_list.h"

defconstant(cl, array_dimension_limit,
    Fixnum::Encode(Arch::ArrayDimensionLimit) )

defconstant(cl, array_rank_limit,
    Fixnum::Encode(Arch::ArrayRankLimit) )

defconstant(cl, array_total_size_limit,
    Fixnum::Encode(Arch::ArrayTotalSizeLimit) )

defconstant(cl, call_arguments_limit,
    Fixnum::Encode(Arch::MultipleValuesLimit) )

defconstant(cl, lambda_list_keywords,
    list(QAallow_other_keys, QAaux, QAbody, QAenvironment, QAkey,
         QAoptional, QArest, QAwhole ))

defconstant(cl, lambda_parameters_limit,
    Fixnum::Encode(Arch::MultipleValuesLimit) )

defconstant(cl, multiple_values_limit,
    Fixnum::Encode(Arch::MultipleValuesLimit) )

defconstant(cl, most_negative_fixnum,
    Fixnum::Encode(Fixnum::MostNegative) )

defconstant(cl, most_positive_fixnum,
    Fixnum::Encode(Fixnum::MostPositive) )

defconstant(cl, nil, nil)

defconstant(cl, pi,
    reinterpret_cast<Internal::Float64Impl*>(
       & reinterpret_cast<StaticBino*>(BINO_BASE)->m_pi64 )->
            Encode() )

defconstant(cl, t, t)

defconstant(si, Pstandard_method_combinationP,
    makeStandardMethodCombination() )

defvar(si, Acaller_tableA,                  nil)
defvar(si, Achar_name_tableA,               nil)
defvar(si, Acharset_tableA,                 nil)
defvar(si, Acommand_line_argumentsA,        nil)
defvar(si, Adefault_external_formatA,       nil)
defvar(si, Adll_file_tableA,                nil)
defvar(si, Aimage_save_timeA,               nil)
defvar(si, Aname_char_tableA,               nil)
defvar(si, ApackagesA,                      nil)
defvar(si, Apathname_hostsA,                nil)
defvar(si, Aruntime_environmentA,           nil)
defvar(si, Asetf_tableA,                    nil)

defvar(si, Astandard_readtableA,
    makeStandardReadtable() )

defvar(si, Atlv_indexA,                     nil)
defvar(si, Atlv_vectorA,                    nil)
defvar(si, Avalue_tableA,                   nil)

defconstant(dom, dom_Punspecified_namespaceP,
    MARKER_unbound )

// Map from namespace-URI to namespace object
defvar(dom, dom_Anamespace_tableA,
    make_hash_table(Ktest, Qequal) )

#include "./tinycl_end_list.h"
