// -*- Mode: C++; -*-
//
// TinyCl - Method Installer
// tinycl_install_method.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_install_arity.h#1 $
//

#define METHOD_VAR(mp_name, mp_class, mp_min, mp_max, mp_nvals, mp_csig) \
    Val mp_name ## __ ## mp_class mp_csig; \
    defun_optional(mp_name ## __ ## mp_class, mp_min, mp_max)
