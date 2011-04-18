//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - SxHash Area Definitions
// tinycl_sxhash.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_sxhash.h#2 $
//
#if !defined(INCLUDE_tinycl_sxhash_h)
#define INCLUDE_tinycl_sxhash_h

#include "./tinycl.h"

namespace TinyCl
{

/// <summary>
///   Area interface of SxHash Area.
/// </summary>
class SxHashArea : public Area_<SxHashArea, Mm::Area::ScanType_SxHash>
{
    protected: static SxHashArea* sm_pSxHashArea;

    // ctor
    protected: SxHashArea(uint nFlags, size_t cbArea) :
        Base(nFlags, cbArea) {}

    protected: SxHashArea() :
            Base(0, 0)
        { CAN_NOT_HAPPEN(); }

    // [G]
    public: static SxHashArea* Get()
        { return sm_pSxHashArea; }

    // [I]
    public: static void Initialize();

    // [M]
    protected: static SxHashArea* makeSxHashArea(size_t cbArea)
    {
        const uint nFlags = ScanType_() | Age_System;
        return new(Mm::GetDataArea(nFlags, cbArea - sizeof(Area)))
            SxHashArea(nFlags, cbArea);
    } // makeSxHashArea

    // [R]
    public: void Reinitialize();
}; // SxHashArea

/// <summary>
///   API for computing hash code for an object.
/// </summary>
class SxHash
{
    public: static Val Compute(Val);
}; // SxHash

} // TinyCl

#endif //!defined(INCLUDE_tinycl_sxhash_h)
