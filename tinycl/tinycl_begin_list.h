// -*- Mode: C++; -*-
// TinyCl - Symbols
// tinycl_begin_list.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_begin_list.h#5 $
//

// Layout
#if !defined(defabstract)
    #define defabstract(mp_cname, mp_meta, mp_layout) \
        deflayout(mp_cname, mp_meta, mp_layout)
#endif

#if !defined(deflayout)
    #define deflayout(mp_cname, mp_meta, mp_layout)
#endif

#if !defined(defstorage)
    #define defstorage(mp_cname, mp_meta, mp_layout) \
        deflayout(mp_cname, mp_meta, mp_layout)
#endif

#if !defined(direct_slot_)
    #define direct_slot_(mp_cname, mp_initarg, mp_ty) \
        direct_slot__(Val, m_ ## mp_cname, mp_initarg, mp_ty)
#endif

#if !defined(direct_slot__)
    #define direct_slot__(mp_cty, mp_field, mp_initarg, mp_ty)
#endif

#if !defined(direct_super_)
    #define direct_super_(mp_super)
#endif

#if !defined(endabstract)
    #define endabstract(mp_name) endlayout(mp_name)
#endif

#if !defined(endlayout)
    #define endlayout(mp_name)
#endif

#if !defined(endstorage)
    #define endstorage(mp_name) endlayout(mp_name)
#endif

#if !defined(inherited_slot_)
    #define inherited_slot_(mp_name)
#endif

// Symbols
#if !defined(defexternal)
    #define defexternal(mp_pkg, mp_cname, mp_lname) \
        definternal(mp_pkg, mp_cname, mp_lname)
#endif // !defined(defexternal)

#if !defined(defkeyword)
    #define defkeyword(mp_cname, mp_lname)
#endif // !defined(defkeyword)

#if !defined(defpackage)
    #define defpackage(mp_cname, mp_lname)
#endif // !defined(defpackage)

#if !defined(definternal)
    #define definternal(mp_pkg, mp_cname, mp_lname)
#endif // !defined(definternal)

#if ! defined(defloat32)
    #define defloat32(mp_pkg, mp_cname, mp_s, mp_e, mp_i) \
        defconstant(mp_pkg, mp_cname, 0)
#endif

#if ! defined(defloat64)
    #define defloat64(mp_pkg, mp_cname, mp_s, mp_e, mp_h, mp_l) \
        defloat32(mp_pkg, mp_cname, 0, 0, 0)
#endif

// Thread Local Variable
#if !defined(deftlv)
    #define deftlv(mp_pkg, mp_cname, mp_init)
#endif // !defined(deftlv)

// Variables
#if !defined(defconstant)
    #define defconstant(mp_pkg, mp_cname, mp_init) \
        defvar(mp_pkg, mp_cname, mp_init)
#endif // !defined(defconstant)

#if !defined(defvar)
    #define defvar(mp_pkg, mp_cname, mp_init)
#endif // !defined(defvar)

#if !defined(FUN_BOOL)
    #define FUN_BOOL(mp_name, mp_n)
#endif

#if !defined(FUN_ENTRY)
    #define FUN_ENTRY(mp_name, mp_min, mp_max, mp_vals, mp_rest, mp_fn)
#endif

#if !defined(FUN_FIX)
    #define FUN_FIX(mp_name, mp_n, mp_vals)
#endif

#if !defined(FUN_FIX_SETF)
    #define FUN_FIX_SETF(mp_name, mp_n)
#endif

#if !defined(FUN_REST)
    #define FUN_REST(mp_name, mp_min, mp_max, mp_vals)
#endif

#if !defined(FUN_REST_SETF)
    #define FUN_REST_SETF(mp_name, mp_min, mp_max)
#endif

#if !defined(FUN_VAR)
    #define FUN_VAR(mp_name, mp_min, mp_max, mp_vals)
#endif

#if !defined(FUN_VAR_SETF)
    #define FUN_VAR_SETF(mp_name, mp_min, mp_max)
#endif

#if !defined(METHOD_FIX)
    #define METHOD_FIX(mp_name, mp_class, mp_nparams, mp_nvals)
#endif

#if !defined(METHOD_FIX_SETF)
    #define METHOD_FIX_SETF(mp_name, mp_class, mp_nparams)
#endif

#if !defined(METHOD_REST)
    #define METHOD_REST(mp_name, mp_class, mp_min, mp_max, mp_nvals)
#endif

#if !defined(METHOD_VAR)
    #define METHOD_VAR(mp_name, mp_class, mp_min, mp_max, mp_nvals, mp_csig)
#endif

