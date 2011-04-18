// -*- Mode: C++; -*-
// TinyCl - Packages
// tinycl_pkgs.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_syms.inc#18 $
//

#include "./tinycl_begin_list.h"

defpackage(cl, (PKG_cl,
    "COMMON-LISP CL", 2003, 2, nil ) )

defpackage(keyword, (PKG_keyword,
    "KEYWORD", 2003, 2, nil ) )

// Below packages use cl.
defpackage(clos, (PKG_clos,
    "CLOS", 301, 2, PKG_cl, nil ) )

defpackage(cl_user, (PKG_cl_user,
    "COMMON-LISP-USER CL-USER", 301, 301, PKG_cl, nil ) )

defpackage(ext, (PKG_ext,
    "EXTENSION EXT", 1001, 31, PKG_cl, nil ) )

// Below packages use cl and ext.
defpackage(c, (PKG_c,
    "COMPILER C", 31, 301, PKG_cl, PKG_ext, nil ) )

defpackage(devel, (PKG_devel,
    "DEVELOPER DEVEL", 301, 301, PKG_cl, PKG_ext, nil ) )

defpackage(si, (PKG_si,
    "SYSTEM SI", 301, 301, PKG_cl, PKG_ext, nil ) )

defpackage(dom, (PKG_dom,
    "XML.DOM DOM", 301, 301, PKG_cl, PKG_ext, nil ) )

#include "./tinycl_end_list.h"
