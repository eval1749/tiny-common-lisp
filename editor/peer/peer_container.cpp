#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Peer Container
// editor/peer/peer_contaner.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_container.cpp#6 $
//
#include "./peer_container.h"

#include "./ctrl_StatusBar.h"
#include "./ctrl_TitleBar.h"

namespace Editor
{
namespace Peer
{

// [A]
Window* Container::AppendChild(Window* pWindow)
{
    if (Container* pContainer = pWindow->GetParent())
    {
        if (pContainer != this)
        {
            pContainer->RemoveChild(pWindow);
        }
    }

    if (IsRealized())
    {
        if (pWindow->IsRealized())
        {
            ::SetParent(*pWindow, m_hwnd);
        }
        else
        {
            pWindow->Realize(this);
        }
    }

    return ChildWindows::Append(pWindow);
} // Container::AppendChild

bool Container::DrawStatusBar(StatusBar* const pStatusBar)
{
    if (Window* pActive = GetActiveWindow())
    {
        return pActive->DrawStatusBar(pStatusBar);
    }

    return false;
} // Container::DrawStatusBar

bool Container::DrawTitleBar(TitleBar* const pTitleBar)
{
    if (Window* pActive = GetActiveWindow())
    {
        return pActive->DrawTitleBar(pTitleBar);
    }

    return false;
} // Container::DrawTitleBar

// [O]
int Container::onCreate(CREATESTRUCT*)
{
    foreach (EnumChild, oEnum, this)
    {
        oEnum.Get()->Realize(this);
    } // for child
    return 0;
} // Container::onCreate

LRESULT Container::onMessage(
    uint    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case PEER_WM_ACTIVATE:
    {
        ::SendMessage(
            *GetParent(),
            PEER_WM_ACTIVATE,
            0,
            reinterpret_cast<LPARAM>(m_hwnd) );

        if (Window* pWindow = Window::FromHwnd(lParam))
        {
            ::SetFocus(*pWindow);
        }
        return 0;
    } // PEER_WM_ACTIVATE

    case WM_GETTEXT:
    case WM_GETTEXTLENGTH:
    {
        if (NULL != m_pActive)
        {
            return ::SendMessage(*m_pActive, uMsg, wParam, lParam);
        }
        return 0;
    } // WM_GETTEXT

    case WM_SETFOCUS:
        if (NULL != m_pActive)
        {
            ::SetFocus(*m_pActive);
        }
        return 0;
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // Container::onMessage

// [R]
Window* Container::RemoveChild(Window* pWindow)
{
    class Internal
    {
        public: static Window* GetLeaf(Window* pWindow)
        {
            while (Container* pContainer = pWindow->DynamicCast<Container>())
            {
                if (Window* pFirst = pContainer->GetFirstChild())
                {
                    pWindow = pFirst;
                }
                else
                {
                    return pContainer;
                }
            } // while container
            return pWindow;
        } // GetLeaf
    }; // Internal

    ASSERT(pWindow->GetParent() == this);

    if (m_pActive == pWindow)
    {
        if (Window* pPrev = m_pActive->GetPrecedingSibling())
        {
            m_pActive = Internal::GetLeaf(pPrev);
        }
        else if (Window* pNext = m_pActive->GetFollowingSibling())
        {
            m_pActive = Internal::GetLeaf(pNext);
        }
        else
        {
            m_pActive = NULL;
        }
    } // if

    return ChildWindows::Delete(pWindow);
} // Container::RemoveChild

void Container::updateTitle()
{
    if (Window* pWindow = GetActiveWindow())
    {
        char16 wsz[100];
        if (! ::GetWindowText(*pWindow, wsz, lengthof(wsz)))
        {
            wsz[0] = 0;
        }
        ::SetWindowText(m_hwnd, wsz);
    }
    else
    {
        ::SetWindowText(m_hwnd, L"");
    }
} // Container::updateTitle

} // Peer
} // Editor
