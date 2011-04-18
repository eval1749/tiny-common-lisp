#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Common Kernel
// tinycl_kernel.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_dll_link.cpp#1 $
//
#include "./tinycl_dll_link.h"

namespace TinyCl
{

namespace
{

// make_dll_file_info
Val make_dll_file_info(Val filename)
{
    assert_latch_locked(VAR(Adll_link_latchA));

    Val fileinfo = Thread::Get()->AllocRecord(CLASSD_dll_file_info);
    DllFileInfo* pFileInfo = fileinfo->StaticCast<DllFileInfo>();
        pFileInfo->m_handle    = Fixnum::Encode(0);
        pFileInfo->m_pathname  = filename;
        pFileInfo->m_table     = make_hash_table(
            Ktest, Qequal, 
            Ksize, Fixnum::Encode(501) );
    return fileinfo;
} // make_dll_file_info


// /Fo $(OutDir)\$(InputName).obj 
Val make_dll_proc_info(Val fileinfo, Val procname)
{
    assert_latch_locked(VAR(Adll_link_latchA));

    Val procinfo = Thread::Get()->AllocRecord(CLASSD_dll_proc_info);
    DllProcInfo* pProcInfo = procinfo->StaticCast<DllProcInfo>();
        pProcInfo->m_dll_file_info = fileinfo;
        pProcInfo->m_name = procname;

        pProcInfo->m_entry = DllLinkArea::AddEntry(procinfo);

    return procinfo;
} // make_dll_proc_info

} // namespace

extern Mm::Area g_oEmptyArea;

Mm::Areas DllLinkArea::sm_oDllLinkAreas;

Val DllLinkArea::AddEntry(Val procinfo)
{
    assert_latch_locked(VAR(Adll_link_latchA));

    for (;;)
    {
        DllLinkArea* pArea = sm_oDllLinkAreas.GetFirst()->
            StaticCast<DllLinkArea>();

        if (pArea->m_ofsFree + sizeof(DllEntry) < pArea->m_cbArea)
        {
            DllEntry* pEntry = reinterpret_cast<DllEntry*>(
                pArea->ToInt() + pArea->m_ofsFree );

            pEntry->m_proc_info = procinfo;
            pEntry->m_pfn = DllLinkStab;

            pArea->m_ofsFree += sizeof(DllEntry);

            return Fixnum::Encode(pEntry);
        } // if

        pArea = new DllLinkArea;
        sm_oDllLinkAreas.Append(pArea);
    } // for
} // DllLinkArea::AddEntry

void DllLinkArea::Reinitialize()
{
    sm_oDllLinkAreas.Append(this);

    foreach (EnumEntry, oEnum, this)
    {
        DllEntry* pEntry = oEnum.Get();
        pEntry->m_pfn = DllLinkStab;
    } // for each entry
} // DllLinkArea::Reinitialize

void DllRestart(HMODULE hSelf)
{
    Val htb = VAR(Adll_file_tableA);
    
    if (! hash_table_p(htb))
    {
        htb = make_hash_table(
            Ktest,  Qequal,
            Ksize,  Fixnum::Encode(31) );

        Val pathname = make_string(".");
        Val file_info = make_dll_file_info(pathname);

        setf_gethash(file_info, pathname, htb);

        VAR(Adll_file_tableA) = htb;
        
        (new DllLinkArea)->Reinitialize();
    } // if
    
    foreach (HashTable::Enum, oEnum, htb)
    {
        DllFileInfo* pFileInfo = oEnum.GetVal()->StaticCast<DllFileInfo>();
        pFileInfo->m_handle = Fixnum::Encode(0);
        
        SimpleString* pPathname = 
            pFileInfo->m_pathname->StaticCast<SimpleString>();
        if (pPathname->GetLength() == 1 &&
            pPathname->GetStart()[0] == '.' )
        {
            pFileInfo->m_handle = Fixnum::Encode(hSelf);
        }
    } // for each key
} // DllRestart

// intern_dll_entry
defun(intern_dll_entry, (Val filename, Val procname))
{
    Val fileinfo;
    {
        with_exclusive_latch(VAR(Adll_link_latchA));

        Val htb = VAR(Adll_file_tableA);

        fileinfo = gethash(filename, htb);
        if (nil == fileinfo)
        {
            fileinfo = make_dll_file_info(filename);
            setf_gethash(fileinfo, filename, htb);
        }
    } // fileinfo

    Val procinfo;
    {
        with_exclusive_latch(VAR(Adll_link_latchA));

        Val htb = fileinfo->StaticCast<DllFileInfo>()->m_table;

        procinfo = gethash(procname, htb);
        if (nil == procinfo)
        {
            procinfo = make_dll_proc_info(fileinfo, procname);
            setf_gethash(procinfo, procname, htb);
        }
    } // procinfo

    return procinfo->StaticCast<DllProcInfo>()->m_entry;
} // intern_dll_entry

} // TinyCl
