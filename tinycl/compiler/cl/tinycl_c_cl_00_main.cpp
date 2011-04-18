#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - Parse
// tinycl_c_cl_00_main.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/cl/tinycl_c_cl_00_main.cpp#1 $
//
#include "./tinycl_c_cl_defs.h"

namespace TinyCl
{

namespace Compiler
{

using namespace Private;

PassParse::ParserTab PassParse::s_oParserTab;

void PassParse::Run()
{
    m_funtab = make_hash_table(Ktest, Qeq, Ksize, Fixnum::Encode(1001));
    m_vartab = make_hash_table(Ktest, Qeq, Ksize, Fixnum::Encode(1001));

    Function* pToplevelFun = new Function(
        Function::Flavor_Toplevel,
        Ktoplevel );

    {
        OwnerScope oOwner(pToplevelFun);
        LexEnv oLexEnv(tyValuesRestT);
        LexEnvScope oLexScope(&oLexEnv);
        Expect oExpect(tyValuesRestT);
        parseForm(&oExpect, Context::Get()->m_form);
        optimizeLexVars();
    }

    pToplevelFun->Clean();
} // PassParse::Run

DEFPASS(Parse)

} // Compiler

} // TinyCl
