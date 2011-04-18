//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions for Initialization
// tinycl_init.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_init.h#4 $
//
#if !defined(INCLUDE_tinycl_init_h)
#define INCLUDE_tinycl_init_h

#include "../tinycl.h"

namespace TinyCl
{

//typedef Val (__fastcall *Fn)();

struct FunEntry
{
    Val         m_fname;
    int         m_iMin;
    int         m_iMax;
    int         m_iRest;
    int         m_iVals;
    const char* m_psz;
}; // FunEntry

#if 0
typedef bool (__fastcall *boolFn1)(Val);
typedef bool (__fastcall *boolFn2)(Val, Val);
typedef Val (__fastcall *Fn1)(Val);
typedef Val (__fastcall *Fn2)(Val, Val);
typedef Val (__fastcall *Fn3)(Val, Val, Val);
typedef Val (__fastcall *FnV)(Thread*);
#endif

struct MethodEntry
{
    int m_iCacheSize;
    Val m_class;
    Val m_fname;
    int m_iMin;
    int m_iMax;
    int m_iRest;
    int m_iVals;
    const char* m_psz;
}; // MethodEntry

#define defun_optional(mp_name, mp_min, mp_max) \
    defun(mp_name ## _, OPTIONAL_LAMBDA_LIST_ ## mp_max) \
    { \
        switch (Fixnum::Decode_(n)) \
        { \
            DISPATCH_BY_ARITY_ ## mp_min ## _ ## mp_max (mp_name) \
            default: CAN_NOT_HAPPEN(); \
        } \
    } // defun

#define defun_setf_optional(mp_name, mp_min, mp_max) \
    defun_setf(mp_name ## _, OPTIONAL_LAMBDA_LIST_ ## mp_max) \
    { \
        switch (Fixnum::Decode_(n)) \
        { \
            DISPATCH_BY_ARITY_ ## mp_min ## _ ## mp_max (setf_ ## mp_name) \
            default: CAN_NOT_HAPPEN(); \
        } \
    } // defun

#define DISPATCH_BY_ARITY_0_1(mp_name) \
    case 0: return mp_name(); \
    case 1: return mp_name(a);

#define DISPATCH_BY_ARITY_0_3(mp_name) \
    case 0: return mp_name(); \
    DISPATCH_BY_ARITY_1_3(mp_name)

#define DISPATCH_BY_ARITY_0_4(mp_name) \
    DISPATCH_BY_ARITY_0_3(mp_name); \
    case 4: return mp_name(a,b,c,d);

#define DISPATCH_BY_ARITY_1_2(mp_name) \
    case 1: return mp_name(a); \
    case 2: return mp_name(a,b);

#define DISPATCH_BY_ARITY_1_3(mp_name) \
    case 1: return mp_name(a); \
    DISPATCH_BY_ARITY_2_3(mp_name)

#define DISPATCH_BY_ARITY_2_3(mp_name) \
    case 2: return mp_name(a,b); \
    case 3: return mp_name(a,b,c); \

#define DISPATCH_BY_ARITY_2_4(mp_name) \
    DISPATCH_BY_ARITY_2_3(mp_name); \
    case 4: return mp_name(a,b,c,d);

#define DISPATCH_BY_ARITY_3_4(mp_name) \
    case 3: return mp_name(a,b,c); \
    case 4: return mp_name(a,b,c,d); \

#define OPTIONAL_LAMBDA_LIST_1 (Val n, Val a)
#define OPTIONAL_LAMBDA_LIST_2 (Val n, Val a, Val b)
#define OPTIONAL_LAMBDA_LIST_3 (Val n, Val a, Val b, Val c)
#define OPTIONAL_LAMBDA_LIST_4 (Val n, Val a, Val b, Val c, Val d)


Thread* Initialize(const InitParams*);

void FinalizeInheritance(Val);

void InstallStaticClassD(
    Val             metaclassd,
    Val             name,
    Val             instanced,
    Val             klass, 
    ClassD::Format  eFormat,
    size_t          cbFixed );

void InstallStaticClass(Val klass, ... );
void InstallStaticExternalSymbol(Val pkg, Val sym, const char* psz);
void InstallStaticFunctions(const FunEntry*, uint);
void InstallStaticInternalSymbol(Val pkg, Val sym, const char* psz);
void InstallStaticMethods(const MethodEntry*, uint);
void InstallStaticPackage(Val, const char*, int, int, ...);
void InstallStaticSetfCell(Val cell, Val name);

void InstallPredicate(Val name, Val classd);
void InstallPredicate(Val name, Val classd_min, Val classd_max);
void InstallPredicate(Val name, Val or, Val classd1, Val classd2);

#define INSTALL_STATIC_READER(mp_name, mp_class, mp_field) \
    InstallStaticReader( \
        Q ## mp_name, \
        CLASSD_ ## mp_class, \
        offsetof(Layout_ ## mp_class, m_ ## mp_field) );

Val InstallStaticReader(Val, Val, size_t);

void InstallStaticTlvRecord(
    Val tlvrec,
    Val name,
    Int iIndex,
    Val init );

void InstallStaticValueCell(
    Val cell,
    Val name,
    Val init,
    Val kind );

Val InternSymbol(const char*);

void InstallType(Val, Val);

Val MakeDiscriminator(Val gf, int iVals);

Val MakeWrapper(
    Val         fname,
    int         iMin,
    int         iMax,
    int         iRest,
    int         iVals,
    int         iKernel,
    const char* pszProc,
    const char* pszLib = NULL );

void PopulateMethodCache(Val);

} // TinyCl

#endif //!defined(INCLUDE_tinycl_init_h)
