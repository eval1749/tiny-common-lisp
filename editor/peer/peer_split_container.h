//////////////////////////////////////////////////////////////////////////////
//
// Editor - Split Container
// eidtor/peer/peer_split_container.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_split_container.h#7 $
//
#if !defined(INCLUDE_editor_peer_split_container_h)
#define INCLUDE_editor_peer_split_container_h

#include "./peer_container.h"

namespace Editor
{

namespace Peer
{

/// <summary>
///  Represents splittable pane container.
/// </summary>
class SplitContainer : public Window_<SplitContainer, Container>
{
    public: static const char* Kind_()  { return "SplitContainer"; }

    public: enum Hit
    {
        Hit_None,
        Hit_Box,
        Hit_HGrip,
        Hit_HSplitter,
        Hit_VGrip,
        Hit_VSplitter,
    }; // Hit

    public:  class Box;
    private: class Drag;
    public:  class ParentBox;

    /// <summary>
    ///   Represents splitable unit.
    /// </summary>
    public: class Box :
        public Castable_<Box>,
        public ChildItem_<Box, ParentBox>
    {
        public: typedef Hit Hit;
        public: typedef ParentBox ParentBox;

        public: Rect m_rc;

        // dtor
        public: virtual ~Box() {}

        // [C]
        public: virtual Window* ComputeActiveWindow() const = 0;

        // [D]
        public: virtual void Destroy() = 0;
        public: virtual void Draw(HDC const) const = 0;

        // [F]
        /// <summary>
        ///   Finds a box contains specified window.
        /// </summary>
        public: virtual Box* FindBox(Window* const) const = 0;

        // [G]
        public: int        GetBottom() const { return m_rc.bottom; }
        public: int        GetHeight() const { return m_rc.GetHeight(); }
        public: int        GetLeft()   const { return m_rc.left; }
        public: virtual const char* GetKind() const = 0;
        public: ParentBox* GetParent() const { return m_pParent; }
        public: int        GetRight()  const { return m_rc.right; }
        public: int        GetTop()    const { return m_rc.top; }
        public: int        GetWidth()  const { return m_rc.GetWidth(); }

        // [H]
        public: virtual Hit HitTest(POINT const, Box** const) const = 0;

        // [O]
        public: virtual bool OnIdle(uint const) = 0;

        // [R]
        public: virtual void Realize(Container* const) = 0;

        public: virtual void Resize(const Rect* const) = 0;

        // [S]
        public: ParentBox* SetParent(ParentBox* const p)
            { return m_pParent = p; }
    }; // Box

    public: template<class T, class B = Box>
        class Box_ : public B
    {
        protected: typedef Box_<T, B> Super;

        public: override const char* GetKind() const
            { return T::Kind_(); }

        public: override bool Is_(const char* psz) const
            { return T::Kind_() == psz || B::Is_(psz); }
    }; // Box_

    public: typedef ChildList_<Box, ParentBox> Boxes;

    /// <summary>
    ///  Represents parent box.
    /// </summary>
    public: class ParentBox :
        public Box_<ParentBox>,
        public Boxes
    {
        public: static const char* Kind_() { return "ParentBox"; }

        // [R]
        public: virtual void Remove(Box* const) = 0;
        public: virtual void Replace(Box* const, Box* const) = 0;
    }; // ParentBox

    /// <summary>
    ///  Represents root box.
    /// </summary>
    private: class RootBox : public Box_<RootBox, ParentBox>
    {
        public: static const char* Kind_() { return "RootBox"; }

        private: SplitContainer* m_pContainer;

        // ctor
        public: RootBox(SplitContainer* const pContainer) :
            m_pContainer(pContainer) {}

        public: virtual ~RootBox() 
        {
            #if DEBUG_DESTROY
                DEBUG_PRINTF("%p\n");
            #endif
            delete GetFirst();
        } // ~RootBox

        // [C]
        public: override Window* ComputeActiveWindow() const
         {
            if (Box* const pBox = GetFirst())
            {
                return pBox->ComputeActiveWindow();
            }
            return NULL;
        } // ComputeActiveWindow

        // [D]
        public: override void Destroy()
        {
            if (Box* const pBox = GetFirst())
            {
                pBox->Destroy();
            }
        } // Destroy

        public: override void Draw(HDC const hdc) const
        {
            if (Box* const pBox = GetFirst())
            {
                pBox->Draw(hdc);
            }
        } // Draw

        // [F]
        public: override Box* FindBox(Window* const pWindow) const
        {
            if (Box* const pBox = GetFirst())
            {
                return pBox->FindBox(pWindow);
            }
            return NULL;
        } // FindBox

        // [G]
        public: Box* GetBox() const
            { return GetFirst(); }

        public: virtual Hit HitTest(
            POINT const pt,
            Box** const out_pBox ) const
        {
            if (Box* const pBox = GetFirst())
            {
                return pBox->HitTest(pt, out_pBox);
            }

            *out_pBox = NULL;
            return Hit_None;
        } // HitTest

        public: Hit HitTest(POINT const pt) const
        {
            Box* pBox;
            return HitTest(pt, &pBox);
        } // HitTest

        // [O]
        public: override bool OnIdle(uint const nCount)
        {
            if (Box* const pBox = GetFirst())
            {
                return pBox->OnIdle(nCount);
            }
            return false;
        } // OnIdle

        // [R]
        public: void Realize(Container* const pParent)
        {
            if (Box* const pBox = GetFirst())
            {
                pBox->Realize(pParent);
            }
        } // Realize

        private: void Remove(Box* const pBox)
        {
            ASSERT(GetFirst() == pBox);
            Delete(pBox);
        } // Remove
        
        public: override void Replace(Box* const pNew, Box* const pOld)
        {
            ASSERT(GetFirst() == pOld);
            Boxes::Replace(pNew, pOld);
        } // Replace

        public: void Resize(const Rect* const prc)
        {
            m_rc = *prc;

            if (Box* const pBox = GetFirst())
            {
                pBox->Resize(prc);
            }
        } // Resize

        // [S]
        public: Box* SetBox(Box* const pBox)
        {
            ASSERT(IsEmpty());
            return Append(pBox);
        } // SetBox
    }; // RootBox

    /// <summary>
    ///   Represents dragging state.
    /// </summary>
    private: class Drag
    {
        private: Hit    m_eHit;
        private: Box*   m_pBox;

        // ctor
        public: Drag() :
            m_eHit(Hit_None),
            m_pBox(NULL) {}

        // [I]
        public: Box* IsActive(HWND const hwnd) const
        {
            if (NULL == m_pBox)
            {
                return NULL;
            }

            if (::GetCapture() != hwnd)
            {
                return NULL;
            }

            return m_pBox;
        } // IsActive

        // [M]
        private: void moveHSplitter(SplitContainer*, POINT, Box*, bool);
        public:  void MoveSplitter(SplitContainer*, POINT, Box*, bool);
        private: void moveVGrip(SplitContainer*, POINT, Box*, bool);
        private: void moveVSplitter(SplitContainer*, POINT, Box*, bool);

        // [S]
        public: void Start(
            HWND const hwnd,
            Hit  const eHit,
            Box* const pBox )
        {
            m_pBox = pBox;
            if (NULL != pBox)
            {
                ::SetCapture(hwnd);
                m_eHit = eHit;
            }
        } // Start

        public: Box* Stop(HWND const hwnd)
        {
            Box* const pBox = m_pBox;
            if (NULL == pBox)
            {
                return NULL;
            }

            if (::GetCapture() == hwnd)
            {
                ::ReleaseCapture();
            }

            m_pBox = NULL;
            return pBox;
        }  // Stop
    }; // Drag

    private:   Drag     m_oDrag;
    protected: RootBox  m_oRootBox;
    private:   Rect     m_rc;

    // ctor
    public: SplitContainer();
    public: ~SplitContainer();

    // [A]
    public: override Window* AppendChild(Window*);

    // [D]
    protected: void drawSplitters()
    {
        Dc hdc(m_hwnd);
        m_oRootBox.Draw(hdc);
    } // drawSplitters

    // [O]
    private: int onCreate(CREATESTRUCT*);

    public: bool OnIdle(uint nCount)
        { return m_oRootBox.OnIdle(nCount); }

    protected: override LRESULT onMessage(UINT, WPARAM, LPARAM);

    // [R]
    public: override Window* RemoveChild(Window*);

    // [S]
    public: bool SplitHorizontally(
        Window* pNewWin,
        Window* pRefWin,
        int     iPercent = 50,
        int     cxRefWin = 0 );

    public: bool SplitVertically(
        Window* pNewWin,
        Window* pRefWin,
        int     iPercent = 50,
        int     cyRefWin = 0 );
}; // SplitContainer

} // Peer
} // Editor

#endif // !defined(INCLUDE_editor_peer_split_container_h)
