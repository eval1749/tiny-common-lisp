//////////////////////////////////////////////////////////////////////////////
//
// Editor - Lisp Object Declarations
// editor/ed_object.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_objects.h#3 $
//
#if !defined(INCLUDE_editor_objects_h)
#define INCLUDE_editor_objects_h

namespace Editor
{

enum ClassIndex
{
    #define deflayout(mp_cname, mp_meta, mp_layout) \
        ClassIndex_ ## mp_cname,

    #include "./ed_layout.inc"

    ClassIndex_Limit,
}; // ClassIndex

enum ClassDIndex
{
    #define defabstract(mp_cname, mp_meta, mp_layout)

    #define deflayout(mp_cname, mp_meta, mp_layout) \
        ClassDIndex_ ## mp_cname,

    #include "./ed_layout.inc"

    ClassDIndex_Limit,
}; // ClassDIndex

enum SymbolIndex
{
    #define defkeyword(mp_cname, mp_lname) SymbolIndex_K ## mp_cname,
    #define definternal(mp_pkg, mp_cname, mp_lname) SymbolIndex_Q ## mp_cname,
    #include "./ed_objects.inc"
    SymbolIndex_Limit,
}; // SymbolIndex

enum SetfCellIndex
{
    #define defsetf(mp_cname) SetfCellIndex_ ## mp_cname,
    #include "./ed_objects.inc" 
    SetfCellIndex_Limit,
}; // SetfCellIndex

enum Tlv
{
    #define deftlv(mp_pkg, mp_cname, mp_lname) TLV_ ## mp_cname,
    TLV_Zero = TinyCl::TLV_Limit - 1,
    #include "./ed_objects.inc"
    TLV_Limit,
}; // Tlv

enum ValueCellIndex
{
    #define defvar(mp_pkg, mp_cname, mp_init) ValueCellIndex_ ## mp_cname,
    #include "./ed_objects.inc"
    ValueCellIndex_Limit,
}; // ValueCellIndex

enum PackageIndex
{
    #define defpackage(mp_cname, mp_params) PackageIndex_ ## mp_cname,
    #include "./ed_objects.inc"
    PackageIndex_Limit,
}; // PackageIndex

using TinyCl::Arch;
using TinyCl::Layout_class;
using TinyCl::Layout_classd;
using TinyCl::Layout_instance;
using TinyCl::Layout_setf_cell;
using TinyCl::Layout_symbol;
using TinyCl::Layout_tlv_record;
using TinyCl::Layout_value_cell;

const Int STATIC_OBJECT_BASE = TinyCl::STATIC_OBJECT_END;
const Int TLV_Start = TinyCl::TLV_Limit;
#include "../tinycl/init/tinycl_defstatic.h"
#define TINYCL_LIST
#include "./ed_objects.inc"
#include "./ed_layout.inc"
#undef TINYCL_LIST
#include "../tinycl/tinycl_end_list.h"

} // Editor

#endif //!defined(INCLUDE_editor_objects_h)
