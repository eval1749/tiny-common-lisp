//////////////////////////////////////////////////////////////////////////////
//
// Editor - Text Page
// eidtor/peer/peer_text_page.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_text_page.h#3 $
//
#if !defined(INCLUDE_editore_peer_text_page_h)
#define INCLUDE_editore_peer_text_page_h

#include "./peer_defs.h"

#include "./peer_style.h"

namespace Editor
{

namespace Peer
{

class FormatParams;
class TextLine;
class TextPage;
class TextRun;

class FormatParams
{
    public: COLORREF        m_crBackground;
    public: uint            m_cwchTab;
    public: int             m_cxLeftMargin;
    public: RenderStyle     m_oSelectionStyle;
    public: const Buffer*   m_pBuffer;
    public: Rect            m_rc;

    public: Posn    m_selection_start;
    public: Posn    m_selection_end;
    public: Val     m_tick;

    // [H]
    public: bool IsInSelection(Posn posn) const
    {
        return xge(posn, m_selection_start) && xlt(posn, m_selection_end);
    } // HasPosn

    // [I]
    public: bool IsDirty(const FormatParams*) const;
}; // FormatParams

class TextRun :
    public Castable_<TextRun>,
    public DoubleLinkedItem_<TextRun, TextLine>,
    public ObjectInHeap
{
    public: int         m_cy;
    public: int         m_cyAscent;
    public: int         m_cyDescent;
    public: RenderStyle m_oStyle;
    public: int         m_xLeft;
    public: int         m_xRight;

    protected: Posn    m_end;
    protected: Posn    m_start;

    // [E]
    public: virtual bool Equal(const TextRun*) const;

    // [G]
    public: virtual const char* GetKind()    const = 0;

    public: int  GetAscent()  const { return m_cyAscent; }
    public: int  GetDescent() const { return m_cyDescent; }
    public: Posn GetEnd()     const { return m_end; }
    public: int  GetHeight()  const { return m_cy; }
    public: int  GetLeft()    const { return m_xLeft; }
    public: int  GetRight()   const { return m_xRight; }
    public: Posn GetStart()   const { return m_start; }
    public: int  GetWidth()   const { return m_xRight - m_xLeft; }

    // [H]
    public: UInt Hash() const;

    // [M]
    public: virtual int MapPosnToX(HDC, Posn posn, int* out_x) const
    {
        if (xlt(posn, m_start)) return 0;
        if (xge(posn, m_end))   return 0;
        *out_x = m_xLeft;
        return m_cy;
    } // MapPosnToX

    public: virtual Posn MapXToPosn(HDC, int x) const
    {
        if (x < m_xLeft || x >= m_xRight) return nil;
        if (m_start == m_end) return nil;
        return m_start;
    } // MapXtoPosn

    // [R]
    public: virtual void Render(HDC, const RECT*) const = 0;

    public: void Render(HDC hdc, int y) const
    {
        Rect rc(m_xLeft, y, m_xRight, y + m_cy);
        Render(hdc, &rc);
    } // Render
}; // TextRun

class TextLine :
    public    DoubleLinkedItem_<TextLine, TextPage>,
    protected DoubleLinkedList_<TextRun, TextLine>,
    public    ObjectInHeap
{
    public: typedef DoubleLinkedList_<TextRun, TextLine> TextRuns;

    public: int     m_cyDescent;
    public: int     m_cy;
    private: UInt   m_nHash;

    public: TextLine() :
        m_cy(0),
        m_cyDescent(0),
        m_nHash(0) {}

    // [A]
    public: void AddRun(TextRun*);

    // [E]
    public: class EnumRun : public TextRuns::Enum
        { public: EnumRun(const TextLine* p) : TextRuns::Enum(p) {} };

    public: bool Equal(const TextLine*) const;

    // [F]
    public: void Finalize();

    // [G]
    public: Posn     GetEnd()      const { return GetLastRun()->GetEnd(); }
    public: TextRun* GetFirstRun() const { return GetFirst(); }
    public: int      GetHeight()   const { return m_cy; }
    public: TextRun* GetLastRun()  const { return GetLast(); }
    public: Posn     GetStart()    const { return GetFirstRun()->GetStart(); }
    public: int      xGetWidth()    const;

    // [M]
    public: int  MapPosnToX(HDC, Posn, int*) const;
    public: Posn MapXToPosn(HDC, int) const;

    // [R]
    public: void Render(HDC, int) const;
}; // TextLine

/// <summary>
///   Represents text page.
/// </summary>
class TextPage :
    protected DoubleLinkedList_<TextLine, TextPage>,
    public    ObjectInHeap
{
    private: typedef DoubleLinkedList_<TextLine, TextPage> Lines;

    private: int            m_cy;
    public:  HANDLE         m_hHeap;
    private: Lines          m_oLineCache;
    private: FormatParams   m_oParams;

    // ctor
    public:  TextPage(const FormatParams*);
    private: TextPage(HANDLE, const FormatParams*);
    public: ~TextPage();

    // [A]
    public: TextLine* AppendLine(TextLine* pLine)
    {
        m_cy += pLine->GetHeight();
        return Append(pLine);
    } // AppendLine

    // [C]
    public: static TextPage* Create(HDC, const FormatParams*, Posn, Posn);

    // [E]
    public: class EnumLine : public Lines::Enum
    {
        public: EnumLine(const TextPage* p) :
            Lines::Enum(p) {}
    }; // EnumLine

    // [D]
    public: void Destroy()
    { 
        if (NULL != this)
        {
            ::HeapFree(m_hHeap, 0, this);
        }
    } // Destroy

    // [F]
    private: void     fillBottom(HDC, int) const;
    private: void     fillRight(HDC, const TextLine*, int) const;
    public: TextLine* FindLine(Posn) const;
    public: void      Format(HDC, Posn, Posn);
    public: TextLine* FormatLine(HDC, Posn);

    // [G]
    public: int GetBottom() const
        { return m_oParams.m_rc.bottom; }

    private: TextLine* getCacheEndOf(Posn);
    private: TextLine* getCacheStartOf(Posn);

    public: Posn GetEnd() const
        { return GetLastLine()->GetEnd(); }

    public: TextLine* GetFirstLine() const
        { return GetFirst(); }

    public: TextLine* GetLastLine() const
        { return GetLast(); }

    public: int GetLeft() const
        { return m_oParams.m_rc.left; }

    public: int GetRight() const
        { return m_oParams.m_rc.right; }

    public: Posn GetStart() const
        { return GetFirstLine()->GetStart(); }

    public: int GetTop() const
        { return m_oParams.m_rc.top; }

    // [I]
    public: TextLine* InsertLine(TextLine* pLine, TextLine* pRefLine)
    {
        m_cy += pLine->GetHeight();
        return InsertBefore(pLine, pRefLine);
    } // PrependLine

    public: bool IsClean(const FormatParams*) const;
    public: bool IsPosnFullyVisible(Posn) const;

    // [M]
    public: Posn      MapPointToPosn(HDC, POINT) const;
    public: int       MapPosnToPoint(HDC, Posn, POINT*) const;
    public: TextLine* MapYToLine(int) const;

    // [P]
    private: void putCache(TextLine*);

    // [R]
    public: void Render(HDC) const;
    public: void Render(HDC, const RECT*) const;
    public: bool Render(HDC, HWND, const TextPage*) const;

    private: void renderAux(
        HWND,
        TextLine*,
        TextLine*,
        int,
        TextLine*,
        TextLine*,
        int,
        TextLine**,
        TextLine** ) const;

    private: void renderLine(HDC hdc, const TextLine* pLine, int y) const
    {
        pLine->Render(hdc, y);
        fillRight(hdc, pLine, y);
    } // renderLine

    // [S]
    public:  bool ScrollDown(HDC);
    private: bool scrollDown();
    public:  bool ScrollToPosn(HDC, Posn);
    public:  bool ScrollUp(HDC);
    public:  bool scrollUp();

    private: void setupDc(HDC hdc) const
    {
        ::SetTextAlign(hdc, TA_BASELINE | TA_NOUPDATECP);
        ::SelectObject(hdc, ::GetStockObject(DC_PEN));
    } // setupDc
}; // TextPage

} // Peer

} // Editor

#endif //!defined(INCLUDE_editore_peer_text_page_h)
