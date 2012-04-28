#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Garbage Collector
// tinycl_gc.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/win/tinycl_win_mm.cpp#5 $
//
#define DEBUG_GC    1
#define DEBUG_LOAD  1
#include "./tinycl_win.h"

#include "../tinycl_dll_link.h"
#include "../tinycl_gc.h"
#include "../tinycl_sxhash.h"

namespace TinyCl
{

static const int cbWatch = 1024 * 64;

typedef Mm::Area Area;

class WinMm : public Mm
{
    // [A]
    public: static Area* Alloc(size_t cbArea)
    {
        #if DEBUG_ALLOC
            DEBUG_PRINTF("commit=%p cb=%d\n", sm_pCommit, cbArea);
        #endif

        Area* pNext = reinterpret_cast<Area*>(sm_pCommit->ToInt() + cbArea);

        if (pNext > sm_pEnd)
        {
            Debugger::Fail("Memory Exhausted.");
        }

        void* pvArea = ::VirtualAlloc(
            sm_pCommit,
            cbArea,
            MEM_COMMIT,
            PAGE_READWRITE );
        if (NULL == pvArea)
        {
            DWORD dwError = ::GetLastError();
            Debugger::Fail("VirtualAlloc, %d", dwError);
        }

        sm_pCommit = pNext;

        return reinterpret_cast<Area*>(pvArea);
    } // Alloc
}; // WinMm

namespace
{

static Val internalPlatformError(const char* psz)
{
    DWORD dwError = ::GetLastError();
    Thread::Get()->m_n = two;
    Thread::Get()->mv_value[1] = MakeUInt(dwError);
    return make_string(psz);
} // internalPlatformError

} // namespace

// [G]
/// <summary>
///   Allocate code area.
/// </summary>
Mm::Area* Mm::GetCodeArea(uint nFlags, size_t cbData)
{
    Area* pArea = GetDataArea(nFlags, cbData);

    DWORD dwOldProtect;
    BOOL fSucceeded = ::VirtualProtect(
        pArea,
        pArea->m_cbArea,
        PAGE_EXECUTE_READWRITE,
        &dwOldProtect );
    if (! fSucceeded)
    {
        Debugger::Fail("VirtualProtect PAGE_EXECUTE_READWRITE");
    }

    return pArea;
} // Mm::GetCodeArea

/// <summary>
///   Allocate data area.
/// </summary>
Mm::Area* Mm::GetDataArea(uint nFlags, size_t cbData)
{
    size_t cbArea = RoundUp(cbData + sizeof(Area), AllocUnit);
    sm_cbAllocSoFar += cbArea;

    Area* pArea = sm_oFreeAreas.GetFirst();
    for (;;)
    {
        if (NULL == pArea)
        {
            pArea = WinMm::Alloc(cbArea);

            #if DEBUG_GC
                DEBUG_PRINTF("new %p cb=%d sofar=%d\n",
                    pArea, cbArea, sm_cbAllocSoFar );
            #endif
            break;
        } // if

        if (pArea->m_cbArea == cbArea)
        {
            sm_oFreeAreas.Delete(pArea);
            #if DEBUG_GC
                DEBUG_PRINTF("free %p cb=%d sofar=%d\n",
                    pArea, cbArea, sm_cbAllocSoFar );
            #endif
            break;
        }

        if (pArea->m_cbArea > cbArea)
        {
            Area* pBelow = reinterpret_cast<Area*>(
                pArea->ToInt() + cbArea );

            new(pBelow) Area(
                Area::ScanType_None,
                pArea->m_cbArea - cbArea );

            sm_oFreeAreas.InsertBefore(pBelow, pArea);
            sm_oFreeAreas.Delete(pArea);

            #if DEBUG_GC
                DEBUG_PRINTF("extract %p cb=%d sofar=%d\n",
                    pArea, cbArea, sm_cbAllocSoFar );
            #endif
            break;
        } // if

        pArea = pArea->GetNext();
    } // for

    pArea = new(pArea) Area(nFlags, cbArea);
    return pArea;
} // Mm::GetDataArea

// [I]

// REVIEW 2008-01-13 yosi@msn.com Should we return min dirty page and max
// dirty page?
bool Mm::Area::IsModified() const
{
    switch (GetScanType())
    {
    case ScanType_Code:
    case ScanType_Cons:
    case ScanType_Record:
        break;

    default:
        return false;
    } // switch scanType

    for (
        uint8* pbRunner = GetTop_<uint8>();
        pbRunner < GetBtm_<uint8>();
        pbRunner += cbWatch )
    {
        void* rgpvWritten[16];
        ULONG_PTR cWrittens = lengthof(rgpvWritten);
        ULONG cbPage;
        UINT nRet = ::GetWriteWatch(
            0,
            pbRunner,
            cbWatch,
            rgpvWritten,
            &cWrittens,
            &cbPage );
        if (0 != nRet)
        {
            Debugger::Fail("GetWriteWatch");
        }

        if (cWrittens >= 1)
        {
            return true;
        }
    } // for pbRunner

    return false;
} // Mm::Area::IsModified

// [L]
Val Mm::Load(const char16* pwszImage)
{
    FileHandle shImage = ::CreateFileW(
        pwszImage,
        GENERIC_READ,                           // dwAccess,
        FILE_SHARE_READ | FILE_SHARE_DELETE,    // dwShare,
        NULL,
        OPEN_EXISTING,                          // dwCreate,
        FILE_FLAG_SEQUENTIAL_SCAN,              // dwFlags
        NULL );

    if (INVALID_HANDLE_VALUE == shImage.h)
    {
        return internalPlatformError("CreateFile");
    }

    return Load(shImage);
} // Mm::Load

Val Mm::Load(HANDLE hImage)
{
    class Local
    {
        public: static Val ReadImage(
            HANDLE const    hFile,
            void* const     pv,
            Int             cbRest)
        {
            uint8* pb = reinterpret_cast<uint8*>(pv);
            while (cbRest > 0)
            {
                DWORD cbRead;
                BOOL fSucceeded = ::ReadFile(
                    hFile,
                    pb,
                    cbRest,
                    &cbRead,
                    NULL );
                if (! fSucceeded)
                {
                    return internalPlatformError("ReadFile");
                }

                if (0 == cbRead)
                {
                    ::SetLastError(ERROR_HANDLE_EOF);
                    return internalPlatformError("ReadFile");
                }

                pb += cbRead;
                cbRest -= cbRead;
            } // while
            return nil;
        } // readImage
    }; // Local

    DWORD cbImage = ::GetFileSize(hImage, NULL);
    if (INVALID_FILE_SIZE == cbImage)
    {
        return internalPlatformError("GetFileSize");
    }

    if (0 != cbImage % AllocUnit)
    {
        return internalPlatformError("GetFileSize");
    }

    Area* pArea = reinterpret_cast<Area*>(sm_pStart);

    while (cbImage)
    {
        pArea = reinterpret_cast<Area*>(::VirtualAlloc(
            pArea,
            AllocUnit,
            MEM_COMMIT,
            PAGE_READWRITE ) );
        if (NULL == pArea)
        {
            return internalPlatformError("VirtualAlloc");
        }

        {
            Val const ret = Local::ReadImage(hImage, pArea, AllocUnit);
            if (nil != ret)
            {
                return ret;
            }
        }

        cbImage -= AllocUnit;

        {
            Area* const pCommit = reinterpret_cast<Area*>(
                pArea->m_pSelf->ToInt() + pArea->m_cbArea );

            if (pCommit > sm_pCommit)
            {
                void* pvCommit = ::VirtualAlloc(
                    sm_pCommit,
                    pCommit->ToInt() - sm_pCommit->ToInt(),
                    MEM_COMMIT,
                    PAGE_READWRITE );
                if (NULL == pvCommit)
                {
                    return internalPlatformError("VirtualAlloc");
                }

                sm_pCommit = pCommit;
            } // if
        }

        // Relocate area if needed
        if (pArea->m_pSelf != pArea)
        {
            Area* const pOrig = pArea->m_pSelf;

            ::CopyMemory(pOrig, pArea, AllocUnit);

            new (pArea) Area(
                Area::ScanType_None,
                pOrig->ToInt() - pArea->ToInt() );

            sm_oFreeAreas.Append(pArea);

            pArea = pOrig;
        } // if

        #if DEBUG_LOAD
            DEBUG_PRINTF("%p %s.%d %d/%d\n",
                pArea, pArea->GetString(), pArea->GetAge(),
                pArea->m_ofsFree, pArea->m_cbArea );
        #endif

        ASSERT(pArea->GetScan() != Area::Scan_None);

        if (Int const cbRest = pArea->m_cbArea - AllocUnit)
        {
            if (cbRest < 0)
            {
                Debugger::Fail("Broken image");
            }

            Val ret = Local::ReadImage(
                hImage,
                reinterpret_cast<void*>(pArea->ToInt() + AllocUnit),
                cbRest );
            if (nil != ret)
            {
                return ret;
            }

            cbImage -= cbRest;
        } // if

        switch (pArea->GetScanType())
        {
        case Area::ScanType_BinObj:
        case Area::ScanType_Cons:
        case Area::ScanType_Record:
            if (pArea->IsReadOnly())
            {
                pArea->SetReadOnly();
            }
            break;

        case Area::ScanType_Code:
        {
            DWORD dwProtect;
            BOOL const fSucceeded = ::VirtualProtect(
                pArea,
                pArea->m_cbArea,
                pArea->IsReadOnly() ?
                    PAGE_EXECUTE_READ :
                    PAGE_EXECUTE_READWRITE,
                &dwProtect );
            if (! fSucceeded)
            {
                Debugger::Fail("VirtualProtect");
            }
            break;
        } // code

        case Area::ScanType_DllLink:
            pArea->StaticCast<DllLinkArea>()->Reinitialize();
            break;

        case Area::ScanType_SxHash:
            pArea->StaticCast<SxHashArea>()->Reinitialize();
            break;
        } // switch scanType

        pArea = pArea->GetBtm_<Area>();
    } // while

    Thread::Get()->m_n = two;
    Thread::Get()->mv_value[1] = MakeInt(cbImage);
    return nil;
} // Mm::Load

// [R]
void Mm::Area::ResetWriteWatch()
{
    ::ResetWriteWatch(this, m_cbArea);
} // Mm::Area::ResetWriteWatch

void Mm::Area::Reset()
{
    #if DEBUG_GC
        DEBUG_PRINTF("%p %s.%d %d/%d\n",
            this, GetString(), GetAge(), m_ofsFree, m_cbArea );
    #endif

    size_t cbArea = m_cbArea;
    ASSERT(0 != cbArea);
    ::ZeroMemory(this, cbArea);
    new(this) Area(ScanType_None, cbArea);

    DWORD dwProtect;
    BOOL fSucceeded = ::VirtualProtect(
        this,
        cbArea,
        PAGE_READWRITE,
        &dwProtect );
    if (! fSucceeded)
    {
        DWORD dwError = ::GetLastError();
        Debugger::Fail("VirtualProtect %d", dwError);
    }
} // Mm::Area::Reset

void Gc::resetWriteWatch()
{
    ::ResetWriteWatch(sm_pStart, sm_pCommit->ToInt() - sm_pStart->ToInt());
} // Gc::resetWriteWatch

// [S]
Val Mm::Save(const char16* const pwszImage)
{
    Gc::UpdateRs();

    FileHandle shImage = ::CreateFileW(
        pwszImage,
        GENERIC_WRITE,                          // dwAccess,
        FILE_SHARE_READ | FILE_SHARE_DELETE,    // dwShare,
        NULL,                                   // pSecurity
        CREATE_ALWAYS,                          // dwCreate,
        FILE_FLAG_SEQUENTIAL_SCAN,              // dwFlagsAndAttrs
        NULL );                                 // hTemplate

    if (INVALID_HANDLE_VALUE == shImage.h)
    {
        return internalPlatformError("CreateFile");
    }

    Int cbImage = 0;
    foreach (EnumArea, oEnum, Thread::Get())
    {
        const Area* const pArea = oEnum.Get();
        if (pArea->GetScanType() == Area::ScanType_None)
        {
            continue;
        }

        DWORD cbWritten;
        BOOL const fSucceeded = ::WriteFile(
            shImage,
            pArea,
            static_cast<DWORD>(pArea->m_cbArea),
            &cbWritten,
            NULL );
        if (! fSucceeded)
        {
            return internalPlatformError("WriteFile");
        }

        cbImage += pArea->m_cbArea;
    } // for each area

    Thread::Get()->m_n = two;
    Thread::Get()->mv_value[1] = MakeInt(cbImage);
    return nil;
} // Mm::Save

void Mm::Area::SetReadOnly()
{
    DWORD dwProtect;
    if (GetScanType() == ScanType_Code)
    {
        dwProtect = PAGE_EXECUTE_READ;
    }
    else
    {
        dwProtect = PAGE_READONLY;
    }

    m_nFlags |= Flags_ReadOnly;

    BOOL fSucceeded = ::VirtualProtect(
        this,
        m_cbArea,
        dwProtect,
        &dwProtect );
    if (! fSucceeded)
    {
        Debugger::Fail("VirtualProtect");
    }
} // Mm::Area::SetReadOnly

void Mm::Start(const InitParams* pParams)
{
    sm_pStart = reinterpret_cast<Area*>(LISP_BASE);

    const int MegaByte = 1024 * 1024;
    const int k_cbTotal  = pParams->m_nTotalMb * MegaByte;

    // FIXME 2008-01-12 yosi@msn.com We should not set WriteWatch for
    // Gen0.
    void* pvStart = ::VirtualAlloc(
        reinterpret_cast<void*>(LISP_BASE),
        k_cbTotal,
        MEM_RESERVE | MEM_WRITE_WATCH,
        PAGE_READWRITE );
    if (NULL == pvStart)
    {
        DWORD dwError = ::GetLastError();

        Debugger::Fail(
            "VirtualAlloc MEM_RESERVE @ 0x%lx %d",
            sm_pStart,
            dwError );
    } // if

    sm_pEnd = reinterpret_cast<Area*>(sm_pStart->ToInt() + k_cbTotal);

    const int cbReco = static_cast<int>(
        reinterpret_cast<Int>(pParams->m_pvStaticEnd) - RECO_BASE );

    const int cbRecoArea = RoundUp(cbReco, AllocUnit);

    void* pvStatic = ::VirtualAlloc(
        pvStart,
        BINO_AREA_SIZE + cbRecoArea,
        MEM_COMMIT,
        PAGE_READWRITE );
    if (NULL == pvStatic)
    {
        DWORD dwError = ::GetLastError();

        Debugger::Fail(
            "VirtualAlloc MEM_COMMIT @ 0x%lx %d",
            sm_pStart,
            dwError );
    } // if

    Area* pRecoArea = new(reinterpret_cast<void*>(RECO_BASE)) Area(
        Area::ScanType_Record | Area::Age_Static,
        cbRecoArea );

    pRecoArea->m_ofsFree = cbReco;

    Area* pBinoArea = new(reinterpret_cast<void*>(BINO_BASE)) Area(
        Area::ScanType_BinObj | Area::Age_Static,
        BINO_AREA_SIZE );

    pBinoArea->m_ofsFree = sizeof(StaticBino);

    sm_pCommit = reinterpret_cast<Area*>(
        pRecoArea->ToInt() + pRecoArea->m_cbArea );
} // Start

} // TinyCl
