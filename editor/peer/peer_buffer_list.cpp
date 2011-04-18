#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Windows Peer - Buffer List
// editor/peer/peer_buffer_list.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/peer/peer_buffer_list.cpp#3 $
//
#include "./peer_buffer_list.h"

#include "../ed_buffer.h"

#include "./peer_root_window.h"

namespace Editor
{

namespace Peer
{

// ctor
BufferListWindow::BufferListWindow() :
    m_hwndListView(NULL) {}

// [G]
BufferListWindow* BufferListWindow::Get()
{
    foreach (RootWindow::EnumFollowing, oEnum, RootWindow::Get())
    {
        if (BufferListWindow* pWindow = 
                oEnum.Get()->DynamicCast<BufferListWindow>() )
        {
            pWindow->Refresh();
            return pWindow;
        }
    } // for each window

    return new BufferListWindow;
} // BufferListWindow::Get

// [O]
LRESULT BufferListWindow::onMessage(
    uint    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case WM_CREATE:
    {
        CREATESTRUCT* p = reinterpret_cast<CREATESTRUCT*>(lParam);

        uint dwExStyle = 0;
            dwExStyle |= LVS_EX_DOUBLEBUFFER;
            dwExStyle |= LVS_EX_FULLROWSELECT;
            //dwExStyle |= LVS_EX_GRIDLINES;
            dwExStyle |= LVS_EX_HEADERDRAGDROP;
            dwExStyle |= LVS_EX_LABELTIP;
            dwExStyle |= LVS_EX_UNDERLINEHOT;
            //dwExStyle |= LVS_EX_TRACKSELECT;

        uint dwStyle = WS_CHILD | WS_VISIBLE | LVS_REPORT;
            dwStyle |= LVS_SHAREIMAGELISTS;
            dwStyle |= LVS_SHOWSELALWAYS;

        m_hwndListView = ::CreateWindowEx(
            0,//dwExStyle,      // dwExStyle
            WC_LISTVIEW,
            NULL,           // title
            dwStyle,
            0,
            0,
            p->cx,
            p->cy,
            *this,  // parent
            reinterpret_cast<HMENU>(CtrlId_ListView),
            g_hInstance,
            NULL );

        ListView_SetExtendedListViewStyleEx(
            m_hwndListView,
            dwExStyle,
            dwExStyle );

#if 0
        ListView_SetImageList(
            m_hwndListView,
            Application::Get()->GetIconList(),
            LVSIL_SMALL );
#endif

        struct ColumnDef
        {
            const char16*   m_pwsz;
            int             m_cx;
            int             m_fmt;
        }; // ColumnDef

        static ColumnDef k_rgoColumn[] =
        {
            { L"Name",      150, LVCFMT_LEFT },
            { L"Size",       60, LVCFMT_RIGHT },
            { L"State",      60, LVCFMT_LEFT },
            { L"Saved At",  100, LVCFMT_LEFT },
            { L"File",      300, LVCFMT_LEFT },
        }; // k_rgoColumn

        LVCOLUMN oColumn;
        oColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
        //oColumn.iSubItem = 0;
        for (
            const ColumnDef* p = k_rgoColumn;
            p < k_rgoColumn + lengthof(k_rgoColumn);
            p++ )
        {
            oColumn.cx  = p->m_cx;
            oColumn.fmt = p->m_fmt;
            oColumn.pszText = const_cast<char16*>(p->m_pwsz);
            ListView_InsertColumn(m_hwndListView, p - k_rgoColumn, &oColumn);
            oColumn.iSubItem += 1;
        } // for each ColumnDef

        // FIXME 2008-01-27 yosi@msn.com We should localize buffer name
        // "*Buffer List*".
        ::SetWindowText(*this, L"*Buffer List*");

        Refresh();
        return 0;
    } // WM_CREATE

    case WM_SETFOCUS:
        ::SetFocus(m_hwndListView);
        return 0;

    case WM_WINDOWPOSCHANGED:
    {
        WINDOWPOS* p = reinterpret_cast<WINDOWPOS*>(lParam);
        if (p->flags & SWP_HIDEWINDOW)
        {
            ::ShowWindow(m_hwndListView, SW_HIDE);
        }
        else
        {
            ::SetWindowPos(
                m_hwndListView,
                NULL,
                0,
                0,
                p->cx,
                p->cy,
                SWP_NOZORDER | SWP_SHOWWINDOW );
        }
        return 0;
    } // WM_WINDOWPOSCHANGED
    } // switch uMsg

    return Super::onMessage(uMsg, wParam, lParam);
} // BufferListWindow::onMessage

void BufferListWindow::Refresh()
{
    ListView_DeleteAllItems(m_hwndListView);

    foreach (List::Enum, oEnum, VAR(AbuffersA))
    {
        Buffer* pBuffer = oEnum.Get()->DynamicCast<Buffer>();
        if (NULL == pBuffer) continue;

        LVITEM oItem;
        oItem.mask = LVIF_IMAGE | LVIF_PARAM | LVIF_TEXT;

        oItem.iItem = 0;

#if 0
        oItem.iImage   = pBuffer->GetMode()->GetIcon();
#else
        oItem.iImage = 0;
#endif
        oItem.iSubItem = 0;
        oItem.lParam   = reinterpret_cast<LPARAM>(pBuffer);
        oItem.pszText  = const_cast<char16*>(pBuffer->GetName());
        ListView_InsertItem(m_hwndListView, &oItem);

        oItem.mask = LVIF_TEXT;
        char16 wsz[100];

        // Size
        oItem.iSubItem = 1;
        ::wsprintf(wsz, L"%d", Fixnum::Decode_(pBuffer->GetEnd()));
        oItem.pszText = wsz;
        ListView_SetItem(m_hwndListView, &oItem);

        // State
        {
            char16* pwsz = wsz;
            *pwsz++ = pBuffer->IsModified()     ? '*' : '-';
            *pwsz++ = pBuffer->IsReadOnly()     ? '%' : '-';
            *pwsz++ = pBuffer->IsNotReady()     ? '!' : '-';
            *pwsz++ = nil == pBuffer->m_windows ? '-' : 'w';
            *pwsz = 0;

            oItem.iSubItem = 2;
            oItem.pszText = wsz;
            ListView_SetItem(m_hwndListView, &oItem);
        }

        // Last Saved
        {
            if (0 == *pBuffer->GetFileName())
            {
                 wsz[0] = 0;
            }
            else
            {
            #if 0
                // FIXME 2007-08-05 We should use localized date time format.
                FILETIME ft;
                ::FileTimeToLocalFileTime(pBuffer->GetLastWriteTime(), &ft);
                SYSTEMTIME st;
                ::FileTimeToSystemTime(&ft, &st);
                ::wsprintf(wsz, L"%d/%d/%d %02d:%02d:%02d",
                    st.wMonth,
                    st.wDay,
                    st.wYear,
                    st.wHour,
                    st.wMinute,
                    st.wSecond );
            #endif
            }

            oItem.iSubItem = 3;
            oItem.pszText = wsz;
            ListView_SetItem(m_hwndListView, &oItem);
        }

        // File
        {
            oItem.iSubItem = 4;
            oItem.pszText = const_cast<char16*>(pBuffer->GetFileName());
            ListView_SetItem(m_hwndListView, &oItem);
        }
    } // for each bufer
    
    class Internal
    {
        public: static int CALLBACK compareItems(LPARAM a, LPARAM b, LPARAM)
        {
            Buffer* pa = reinterpret_cast<Buffer*>(a);
            Buffer* pb = reinterpret_cast<Buffer*>(b);
            return ::lstrcmpW(pa->GetName(), pb->GetName());
        } // compareItems
    }; // Internal

    ListView_SortItems(m_hwndListView, Internal::compareItems, NULL);
} // BufferListWindow::Refresh

} // Peer
} // Editor
