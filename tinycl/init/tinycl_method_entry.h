// -*- Mode: C++; -*-
// TinyCl - Macros for Method Entry Table
// tinycl/init/tinycl_method_entry.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_method_entry.h#1 $
//

#define METHOD_FIX(mp_name, mp_class, mp_min, mp_nvals) \
    METHOD_ENTRY(mp_name, mp_class, \
        mp_min, mp_min, mp_nvals, 0, 10 )

#define METHOD_FIX_SETF(mp_name, mp_class, mp_min) \
    METHOD_ENTRY_SETF(mp_name, mp_class, mp_min, mp_min, 1, 0, 10)

#define METHOD_ENTRY( \
            mp_name, mp_class, \
            mp_min, mp_max, mp_nvals, mp_sig, mp_cache_size ) \
    { \
        mp_cache_size, \
        CLASS_ ## mp_class, \
        Q ## mp_name, \
        mp_min, mp_max, mp_sig, mp_nvals, \
        # mp_name "__" # mp_class, \
    },

#define METHOD_ENTRY_SETF( \
            mp_name, mp_class, \
            mp_min, mp_max, mp_nvals, mp_sig, mp_cache_size ) \
    { \
        mp_cache_size, \
        CLASS_ ## mp_class, \
        SETF_ ## mp_name, \
        mp_min, mp_max, mp_sig, mp_nvals, \
        "setf_" # mp_name "__" # mp_class, \
    },

#define METHOD_REST(mp_name, mp_class, mp_min, mp_max, mp_nvals) \
    METHOD_ENTRY(mp_name, mp_class, mp_min, mp_max, mp_nvals, 1, 10)

#define METHOD_VAR(mp_name, mp_class, mp_min, mp_max, mp_nvals, mp_csig) \
    { \
        13, \
        CLASS_ ## mp_class, \
        Q ## mp_name, \
        mp_min, mp_max, 0, mp_nvals, \
        # mp_name "__" # mp_class "_", \
    },
