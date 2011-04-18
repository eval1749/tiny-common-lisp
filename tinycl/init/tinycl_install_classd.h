// -*- Mode: C++; -*-
// tinycl_install_classd.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_install_classd.h#1 $
//

#define defabstract(mp_cname, mp_meta, mp_layout) \
    InstallStaticClassD( \
        CLASSD_ ## mp_meta ## _class, \
        Q ## mp_cname, \
        nil, \
        CLASS_  ## mp_cname, \
        ClassD::Format_None, \
        0 );

#define deflayout(mp_cname, mp_meta, mp_layout) \
    InstallStaticClassD( \
        CLASSD_ ## mp_meta ## _class, \
        Q ## mp_cname, \
        CLASSD_ ## mp_cname, \
        CLASS_  ## mp_cname, \
        ClassD::Format_Record, \
        sizeof(Layout_ ## mp_cname) );

#define defstorage(mp_cname, mp_meta, mp_layout) \
    InstallStaticClassD( \
        CLASSD_ ## mp_meta ## _class, \
        Q ## mp_cname, \
        CLASSD_ ## mp_cname, \
        CLASS_  ## mp_cname, \
        ClassD::Format_Instance, \
        sizeof(Layout_ ## mp_cname) );

