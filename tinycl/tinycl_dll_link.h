//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Definitions
// tinycl.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_dll_link.h#3 $
//
#if !defined(INCLUDE_tinycl_dll_link_h)
#define INCLUDE_tinycl_dll_link_h

#include "./tinycl.h"

namespace TinyCl
{

class DllFileInfo;
class DllProcInfo;
struct DllEntry;

//////////////////////////////////////////////////////////////////////
//
// DllEntry
//
struct DllEntry
{
    void*   m_pfn;
    Val     m_proc_info;
}; // DllEntry

//////////////////////////////////////////////////////////////////////
//
// DllLibraryInfo
//
class DllFileInfo : 
    public Record_<DllFileInfo, Layout_dll_file_info>
{
    public: static Val ClassD_() { return CLASSD_dll_file_info; }

    // Val  m_classd;       // [0]
    //Val m_handle;           // [1]
    //Val m_filename;         // [2]
    //Val m_proc_table;       // [3] Mapping procedure name to DllProcInfo
}; // DllFileInfo

//////////////////////////////////////////////////////////////////////
//
// DllProcInfo
//
class DllProcInfo : 
    public Record_<DllProcInfo, Layout_dll_proc_info>
{
    public: static Val ClassD_() { return CLASSD_dll_proc_info; }

    // Val  m_classd;       // [0]
    //Val m_file_info;        // [1]
    //Val m_proc_name;        // [2]
    //Val m_entry;            // [3]
}; // DllProcInfo

//////////////////////////////////////////////////////////////////////
//
// Dll Link Error
//
class DllLinkError : 
    public Instance_<DllLinkError, Layout_dll_link_error>
{
    //Val m_arguments;
    //Val m_filename;
    //Val m_name;
    //Val m_code;
}; // DllLinkError

//////////////////////////////////////////////////////////////////////
//
// DllLink Area
//
class DllLinkArea : public Area_<DllLinkArea, Mm::Area::ScanType_DllLink>
{
    private: static Mm::Areas sm_oDllLinkAreas;

    public: void* operator new(size_t)
    { 
        return Mm::GetDataArea(
            ScanType_() | Age_System,
            sizeof(DllEntry) );
    } // operator new

    // ctor
    public: DllLinkArea() :
        Base(ScanType_() | Age_System, k_cbUnit) {}

    // [A]
    public: static Val  AddEntry(Val);

    // [E]
    public: class EnumEntry
    {
        protected: DllEntry* m_pRunner;
        protected: DllEntry* m_pEnd;

        public: EnumEntry(DllLinkArea* p) :
            m_pRunner(p->GetTop<DllEntry>()),
            m_pEnd(p->GetFree<DllEntry>()) {}

        public: bool AtEnd() const { return m_pRunner >= m_pEnd; }
        public: DllEntry* Get() const { ASSERT(! AtEnd()); return m_pRunner; }
        public: void Next() { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumEntry

    // [R]
    public: void Reinitialize();
}; // DllLinkArea

void DllRestart(HMODULE);

// DllLinkStab is defined in arch/[arch]/[arch]_ke_mach.cpp
extern "C" Val __fastcall   DllLinkStab();

// DllResolve
//  Resolve DLL link. This function is called by DllLinkStab.
extern "C" void* __fastcall DllResolve(Thread*, DllEntry*);

} // TinyCl

#endif //!defined(INCLUDE_tinycl_dll_link_h)
