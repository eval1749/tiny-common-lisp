//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions
// tinycl.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_gc.h#4 $
//
#if !defined(INCLUDE_tinycl_gc_h)
#define INCLUDE_tinycl_gc_h

#include "./tinycl.h"

namespace TinyCl
{

/// <summary>
///   Use forward-cell object for placeholder for linking From-Space object
///   to To-Space object.
///   <list>
///     <item><description>
///       Forward cell object exists only during GC or die at end of garbage
///       collection.
///     </description></item>
///     <item><description>
///       Gc::Verify checks forward cell objects in lisp heap and signals
///       error if found.
///     </description></item>
///   </list>
/// </summary>
struct Layout_forward_cell
{
    Val m_classd;
    Val m_value;
}; // Layout_forward_cell

/// <summary>
///  Forward cell object helper.
/// </summary>
class ForwardCell :
    public HeapObject_<Layout_forward_cell>
{
    static const Int Marker = Arch::Tag_Cons;

    // ctor
    public: ForwardCell(Val);

    // [D]
    public: static Int Decode_(const Datum* const);

    // [G]
    public: Val Get() const
        { return m_value; }

    // [I]
    public: static bool Is_(const Datum* const);
}; // ForwardCell

/// <summary>
///   The Garbage Collector
/// </summary>
class Gc : public Mm
{
    public: static const int MaxAge = Area::Age_GcMax;
    public: static const int MinAge = Area::Age_GcMin;

    private: static int sm_iMaxAge;

    // Entry Point
    public: static void Run(Thread*, int = MaxAge);
    public: static void UpdateRs();

    // [A]
    protected: static int  ageOf(Val);

    // [C]
    private: static bool canGc();

    // [G]
    public: static int GetMaxAge() { return sm_iMaxAge; }

    // [F]
    private: static void fixSxHashTable();

    // [M]
    protected: static Area* mapToArea(Val);
    private:   static Val   moveCons(Area*, Val);
    private:   static Val   moveFunObj(Area*, Val);
    private:   static Val   moveInstance(Area*, Val);
    private:   static Val   moveRecord(Area*, Val);
    private:   static Val   moveObject(Val);

    // [P]
    private: static void prepareCode(Val, Val);

    // [R]
    private: static void remember(int, Val*);
    private: static void rememberCode(int, Val);
    private: static void rememberCodeIf(int, Val);  // arch
    private: static Val  resolve(Val);
    private: static void resetWriteWatch();         // arch

    // [S]
    protected: static bool scanArea(Area*);
    protected: static void scanCode(int, Val); // arch
    private:   static void scanRange(int, Val*, Val*);

    // [U]
    protected: static void updateCell(uint, Val*);
    private:   static void updateFrame(Thread*, Frame*);  // arch
    private:   static void updateGcAnchors();
    private:   static void updateRange(Val*, Val*);
    private:   static void updateThread(Thread*);
    private:   static void updateThreadAfter(Thread*);    // arch
    private:   static void updateThreadBefore(Thread*);   // arch
}; // Gc

} // TinyCl

#endif //!defined(INCLUDE_tinycl_gc_h)
