// -*- Mode: C++; -*-
//
// TinyCl - Install Static Objects
// tinycl_init_install.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_install_object.h#2 $
//

#define defexternal(mp_pkg, mp_cname, mp_lname) \
    InstallStaticExternalSymbol(PKG_ ## mp_pkg, Q ## mp_cname, mp_lname);

#define defkeyword(mp_cname, mp_lname) \
    InstallStaticExternalSymbol(PKG_keyword, K ## mp_cname, mp_lname);

#define defconstant(mp_pkg, mp_cname, mp_init) \
    InstallStaticValueCell( \
        VAR_ ## mp_cname, \
        Q ## mp_cname, \
        mp_init, Kconstant );

#define defpackage(mp_cname, mp_params) \
    InstallStaticPackage mp_params;

#define defsetf(mp_cname) \
    InstallStaticSetfCell(SETF_ ## mp_cname, Q ## mp_cname);

#define definternal(mp_pkg, mp_cname, mp_lname) \
    InstallStaticInternalSymbol(PKG_ ## mp_pkg, Q ## mp_cname, mp_lname);

#define deftlv(mp_pkg, mp_cname, mp_init) \
    InstallStaticTlvRecord( \
        TLVREC_ ## mp_cname, \
        Q ## mp_cname, \
        TLV_ ## mp_cname, \
        mp_init );

#define defvar(mp_pkg, mp_cname, mp_init) \
    InstallStaticValueCell( \
        VAR_ ## mp_cname, \
        Q ## mp_cname, \
        mp_init, \
        Kspecial );

#define defloat64(mp_pkg, mp_name, mp_s, mp_e, mp_h, mp_l) \
    defconstant(mp_pkg, mp_name, reinterpret_cast<Val>( \
        BINO_BASE + Arch::Tag_Record + \
        offsetof(StaticBino, m_ ## mp_name) ) );

#define defloat32(mp_pkg, mp_name, mp_s, mp_e, mp_i) \
    defloat64(mp_pkg, mp_name, 0, 0, 0, 0)

