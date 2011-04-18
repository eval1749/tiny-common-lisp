// -*- Mode: C++; -*-
// TinyCl - Finalize Class
// init/tinycl_finalize_class
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_finalize_class.h#1 $
//
#define defabstract(mp_cname, mp_meta, mp_layout)

#define deflayout(mp_cname, mp_meta, mp_layout) \
    FinalizeInheritance(CLASS_ ## mp_cname);
