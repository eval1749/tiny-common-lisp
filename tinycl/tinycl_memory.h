//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - TinyCl Memory Manager Definitions
// tinycl_memory.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_memory.h#6 $
//
#if !defined(INCLUDE_tinycl_memory_h)
#define INCLUDE_tinycl_memory_h

#if !defined(INCLUDE_tinycl_h)
    #error "You must include tinycl.h instead of tinycl_memory.h directly."
#endif

namespace TinyCl
{

class CodeObject;
class Gc;
class Record;
class Thread;

struct InitParams
{
    HANDLE      m_hImage;
    HMODULE     m_hSelf;
    int         m_nTotalMb;
    void*       m_pvStaticEnd;
}; // InitParams

// Memory Manager
class Mm
{
    public: class Area;

    public: class Area :
        public AsInt,
        public DoubleLinkedItem_<Area>
    {
        private: typedef Area Self;
        private: typedef Area Base_;

        protected: static const int k_cbUnit = 1024 * 64;

        // [D]
        public: template<class T> T* DynamicCast() const
        {
            Base_* p = static_cast<Base_*>(const_cast<Self*>(this));
            return T::Is_(p) ? reinterpret_cast<T*>(p) : NULL;
        } // DynamicCast

        // [I]
        public: template<class T> bool Is() const
        {
            Base_* p = static_cast<Base_*>(const_cast<Self*>(this));
            return T::Is_(p);
        } // Is

        // [S]
        public: template<class T> T* StaticCast() const
        {
            T* p = DynamicCast<T>();
            ASSERT(NULL != p);
            return p;
        } // StaticCast

        public: enum Scan
        {
            Scan_None       = 0,

            Scan_BinObj     = 1,
            Scan_Code       = 2,
            Scan_Cons       = 3,   // no header
            Scan_Record     = 4,
            Scan_5          = 5,
            Scan_6          = 6,
            Scan_Large      = 7,
            Scan_Weak       = 8,

            Scan_Limit,

            Scan_First      = 1,
            Scan_Shift      = 16,
        }; // Scan

        public: enum ScanType
        {
            // Aged
            ScanType_None       = Scan_None << Scan_Shift,

            ScanType_BinObj     = Scan_BinObj << Scan_Shift,
            ScanType_Code       = Scan_Code   << Scan_Shift,
            ScanType_Cons       = Scan_Cons   << Scan_Shift,
            ScanType_Record     = Scan_Record << Scan_Shift,
            ScanType_5          = Scan_5      << Scan_Shift,
            ScanType_6          = Scan_6      << Scan_Shift,
            ScanType_Large      = Scan_Large  << Scan_Shift,
            ScanType_Weak       = Scan_Weak   << Scan_Shift,

            // No age
            ScanType_9          = 0x00090000,
            ScanType_DllLink    = 0x000A0000,
            ScanType_SxHash     = 0x000B0000,
            ScanType_Rs         = 0x000C0000,

            // Outside of lisp heap
            ScanType_GcAnchor   = 0x000D0000,
            ScanType_ObStack    = 0x000E0000,
            ScanType_Thread     = 0x000F0000,
        }; // ScanType

        public: enum Space
        {
            ToSpace     = 0x00,
            FromSpace   = 0x80,
        }; // Direction

        public: enum Age
        {
            //Age_Max     = 127,
            Age_Max     = 9,

            Age_System  = 9,
            Age_Static  = 8,
            Age_7       = 7,
            Age_6       = 6,
            Age_ScanMax = 5,
            Age_GcMax   = 5,
            Age_GcMin   = 2,
            Age_Min     = 0,
        }; // Age

        public: enum Flags
        {
            Flags_AgeMask       = 0x0000007F,
            Flags_DirectionMask = 0x00000080,
            Flags_ScanMask      = 0x000F0000,
            Flags_ReadOnly      = 0x00100000,
        }; // Flags

        // Area* m_pNext;       // [0]
        // Area* m_pPrev;       // [1]
        public: Area*   m_pSelf;        // [2]
        public: size_t  m_cbArea;       // [3]
        public: size_t  m_ofsFree;      // [4]
        public: size_t  m_ofsScan;      // [5]
        public: size_t  m_nFlags;       // [6]
        public: size_t  m_not_used_7;   // [7]

        public: void* operator new(size_t, void* pv)
            { return pv; }

        public: Area() {}

        public: Area(uint nFlags, size_t cb) :
            m_cbArea(cb),
            m_nFlags(nFlags),
            m_ofsFree(sizeof(Area)),
            m_ofsScan(sizeof(Area))
        {
            m_pSelf = this;
        } // Area

        // [A]
        public: void* Alloc(size_t);

        // [E]
        public: class EnumCode
        {
            private: CodeObject*    m_pCurr;
            private: CodeObject*    m_pEnd;
            private: CodeObject*    m_pStart;

            public: EnumCode(const Area* p) :
                m_pCurr(p->GetTop_<CodeObject>()),
                m_pEnd(p->GetFree_<CodeObject>()),
                m_pStart(p->GetTop_<CodeObject>()) {}

            public: bool AtEnd() const
                { return m_pCurr >= m_pEnd; }

            public: CodeObject* Get()   const
                { ASSERT(!AtEnd()); return m_pCurr; }

            public: void Next();
        }; // EnumCode

        public: class EnumRecord
        {
            private: Record*    m_pCurr;
            private: Record*    m_pEnd;
            private: Record*    m_pStart;

            public: EnumRecord(const Area* p) :
                m_pCurr(p->GetTop_<Record>()),
                m_pEnd(p->GetFree_<Record>()),
                m_pStart(p->GetTop_<Record>()) {}

            public: bool AtEnd() const
                { return m_pCurr >= m_pEnd; }

            public: Record* Get() const
                { ASSERT(!AtEnd()); return m_pCurr; }

            public: void Next();
        }; // EnumObject

        // [G]
        public: int GetAge() const
            { return m_nFlags & Flags_AgeMask; }

        public: template<typename T> T* GetBtm_() const
            { return reinterpret_cast<T*>(ToInt() + m_cbArea); }

        public: size_t GetFreeSize() const
            { return m_cbArea - m_ofsFree; }

        public: template<typename T> T* GetFree_() const
            { return reinterpret_cast<T*>(ToInt() + m_ofsFree); }

        public: template<typename T> T* GetScan_() const
            { return reinterpret_cast<T*>(ToInt() + m_ofsScan); }

        public: Scan GetScan() const
            { return static_cast<Scan>(GetScanType() >> Scan_Shift); }

        public: ScanType GetScanType() const
            { return static_cast<ScanType>(m_nFlags & Flags_ScanMask); }

        public: const char* GetString() const
        {
            static const char* const k_rgpszScanType[] =
            {
                "free", // 0
                "bino", // 1
                "code", // 2
                "cons", // 3
                "reco", // 4
                "0005", // 5
                "0006", // 6
                "big ", // 7
                "weak", // 8
                "0009", // 9
                "dll ", // 10
                "htb ", // 11
                "rs  ", // 12
                "gc  ", // 13
                "ostk", // 14
                "th  ", // 15
            }; // k_rgpszScanType

            return k_rgpszScanType[GetScan()];
        } // GetString

        public: template<typename T> T* GetTop_() const
            { return reinterpret_cast<T*>(ToInt() + sizeof(Area)); }

        // [I]
        public: bool IsEmptyArea() const
            { return Mm::GetEmptyArea() == this; }

        public: bool IsFromSpace() const
            { return 0 != (m_nFlags & FromSpace); }

        public: bool IsModified() const;    // arch

        public: bool IsReadOnly() const
            { return 0 != (m_nFlags & Flags_ReadOnly); }

        // [R]
        public: void Reset();               // arch
        public: void ResetWriteWatch();     // arch

        // [S]
        public: void SetReadOnly();         // arch
    }; // Area

    public: typedef DoubleLinkedList_<Area> Areas;

    public: class GcAnchor :
        public DoubleLinkedItem_<GcAnchor>
    {
        friend class Gc;

        private: Val    m_value;

        public: GcAnchor(Val value) :
            m_value(value) {}

        public: Val Get() const { return m_value; }
    }; // GcAnchor

    public: typedef DoubleLinkedList_<GcAnchor> GcAnchors;

    public: class AreaManager
    {
        private: Area* m_rgpArea[Area::Scan_Limit];

        public: AreaManager();

        // [A]
        public: Area* Alloc(uint, size_t);

        // [G]
        public: Area* Get(Area::Scan eScan)
        {
            return m_rgpArea[eScan];
        } // Get

        // [R]
        public: void Reset();

        // [S]
        public: void Set(Area*);
    }; // AreaManager

    protected: static size_t      sm_cbAllocSoFar;
    protected: static Areas       sm_oFreeAreas;
    public:    static Area        sm_oEmptyArea;
    public:    static GcAnchors   sm_oGcAnchors;
    protected: static Area*       sm_pCommit;
    protected: static Area*       sm_pEnd;
    protected: static Area*       sm_pStart;

    // [A]
    public: static void AddFreeArea(Area*);

    // [C]
    public: static GcAnchor* CreateGcAnchor(Val);

    // [D]
    public: static void DestroyGcAnchor(GcAnchor*);

    // [E]
    public: class EnumArea
    {
        private: Area* m_p;
        private: Area* m_pEnd;
        private: Area* m_pStart;

        public: EnumArea(const Thread*) :
            m_p(Mm::GetStart()),
            m_pEnd(Mm::GetCommit()),
            m_pStart(Mm::GetStart()) {}

        public: bool  AtEnd() const { return m_p == m_pEnd; }

        public: Area* Get() const { ASSERT(!AtEnd()); return m_p; }

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_p = reinterpret_cast<Area*>(m_p->ToInt() + m_p->m_cbArea);
        } // Next
    }; // EnumArea

    // [F]
    public: static void FinalizeStatic(Thread*);

    // [G]
    public: static Area* GetDataArea(uint, size_t);
    public: static Area* GetCodeArea(uint, size_t);
    public: static Area* GetEmptyArea() { return &Mm::sm_oEmptyArea; }
    public: static Area* GetEnd()       { return sm_pEnd; }
    public: static Area* GetCommit()    { return sm_pCommit; }
    public: static Area* GetStart()     { return sm_pStart; }

    // [I]
    public: static bool IsHeapObject(Val);

    public: static bool IsHeapPtr(const void* pv)
    {
        const Area* const p = reinterpret_cast<const Area*>(pv);
        return p >= Mm::GetStart() && p <= Mm::GetCommit();
    } // IsHeapPtr

    // [L]
    public: static Val Load(const char16*);         // platform
    public: static Val Load(HANDLE);                // platform

    // [S]
    public: static Val  Save(const char16*);        // platform
    public: static void Start(const InitParams*);   // platform
}; // Mm

CASSERT(sizeof(Mm::Area) == sizeof(Val) * 8);


template<class T, Mm::Area::ScanType t_eScanType>
class Area_ : public Mm::Area
{
    public: static ScanType ScanType_() { return t_eScanType; }
    
    protected: typedef Area_<T, t_eScanType> Base;

    protected: Area_(uint n, size_t cb) :
        Mm::Area(n, cb) {}

    // [G]
    public: template<typename E> E* GetBottom() const
        { return reinterpret_cast<E*>(ToInt() + m_cbArea); }

    public: template<typename E> E* GetFree() const
        { return reinterpret_cast<E*>(ToInt() + m_ofsFree); }

    public: template<typename E> E* GetTop() const
        { return reinterpret_cast<E*>(ToInt() + sizeof(Mm::Area)); }

    // [I]
    public: static bool Is_(const Area* p)
        { return p->GetScanType() == T::ScanType_(); }
}; // Area_

} // TinyCl

#endif //!defined(INCLUDE_tinycl_memory_h)
