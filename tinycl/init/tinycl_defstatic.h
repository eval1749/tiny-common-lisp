// -*- Mode: C++; -*-
// tinycl_defstatic.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_defstatic.h#2 $
//

const Int CLASSD_BASE = STATIC_OBJECT_BASE;

const Int CLASS_BASE = CLASSD_BASE +
    sizeof(Layout_classd) * ClassDIndex_Limit;

const Int SYMBOL_BASE = CLASS_BASE +
    (sizeof(Layout_instance) + sizeof(Layout_class)) * ClassIndex_Limit;

const Int SETF_BASE = SYMBOL_BASE +
    sizeof(Layout_symbol) *  SymbolIndex_Limit;

const Int TLVREC_BASE = SETF_BASE +
    sizeof(Layout_setf_cell) * SetfCellIndex_Limit;

const Int VAR_BASE = TLVREC_BASE +
    sizeof(Layout_tlv_record) * (TLV_Limit - TLV_Start);

const Int PKG_BASE = VAR_BASE +
    sizeof(Layout_value_cell) * ValueCellIndex_Limit;

const Int STATIC_OBJECT_END = PKG_BASE +
    sizeof(Layout_package) * PackageIndex_Limit;

#define defabstract(mp_cname, mp_meta, mp_layout) \
    defstatic_(CLASS_ ## mp_cname, CLASS_BASE + \
        (sizeof(Layout_instance) + sizeof(Layout_class)) * \
            ClassIndex_ ## mp_cname + \
        Arch::Tag_Record );

#define deflayout(mp_cname, mp_meta, mp_layout) \
    defstatic_(CLASSD_ ## mp_cname, CLASSD_BASE + \
        sizeof(Layout_classd) * ClassDIndex_ ## mp_cname + \
        Arch::Tag_Record ); \
    defabstract(mp_cname, mp_meta, mp_layout)

#define defkeyword(mp_cname, mp_lname) \
    defstatic_symbol(K ## mp_cname)

#define defexternal(mp_pkg, mp_cname, mp_lname) \
    definternal(mp_pkg, mp_cname, mp_lname)

#define defpackage(mp_cname, mp_lname) \
    defstatic_(PKG_ ## mp_cname, \
        PKG_BASE + Arch::Tag_Record + \
        sizeof(Layout_package) * PackageIndex_ ## mp_cname );

#define definternal(mp_pkg, mp_cname, mp_lname) \
    defstatic_symbol(Q ## mp_cname)

#define defsetf(mp_cname) \
    defstatic_(SETF_ ## mp_cname, \
        SETF_BASE + Arch::Tag_Record + \
        sizeof(Layout_setf_cell) * SetfCellIndex_ ## mp_cname );

#define deftlv(mp_pkg, mp_cname, mp_lname) \
    defstatic_(TLVREC_ ## mp_cname, \
        TLVREC_BASE + Arch::Tag_Record + \
        sizeof(Layout_tlv_record) * (TLV_ ## mp_cname - TLV_Start) );

#define defvar(mp_pkg, mp_cname, mp_init) \
    defstatic_(VAR_ ## mp_cname, \
        VAR_BASE + Arch::Tag_Record  + \
        sizeof(Layout_value_cell) * ValueCellIndex_ ## mp_cname );
