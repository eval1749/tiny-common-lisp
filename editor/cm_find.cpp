#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Find or Replace
// editor/cm_find.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_find.cpp#6 $
//
//
#include "./cm_cmdl.h"

#include "./ed_buffer.h"
#include "./ed_range.h"
#include "./ed_selection.h"
#include "./ed_window.h"

#include "./peer/peer_rich_dialog.h"
#include "./peer/peer_rich_view.h"

#include "./rtl_defs.h"

#include "../tinycl/rtl/tinycl_regex.h"

namespace Editor
{

/// <summary>
///   Implementation of <c>find_or_replace</c> command.
/// </summary>
defcommand(find_or_replace)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection)
    {
        return nil;
    }

    using namespace Editor::Peer::RichView;

    class MyDialog : public Peer::RichDialog
    {
        public: enum Ctrl
        {
            Ctrl_FindNext,
            Ctrl_FindPrev,
            Ctrl_FindWhat,
            Ctrl_ReplaceWith,
        }; // Ctrl

        private: Selection* m_pSelection;

        public: MyDialog(Selection* pSelection, Markup::Node* p) :
            m_pSelection(pSelection),
            Peer::RichDialog(p) {}

        private: virtual void onCommand(uint nCtrlId, uint nNotify) override
        {
            switch (nCtrlId)
            {
            case Ctrl_FindNext:
            case Ctrl_FindPrev:
                switch (nNotify)
                {
                case BN_CLICKED:
                    onFindNext_Click(nCtrlId);
                    break;
                } // switch nNotify
                break;
            } // switch nCtrlId
        } // onCommand

        private: void onFindNext_Click(uint const nCtrlId)
        {
            Val const src = getControlText(Ctrl_FindWhat);
            if (zero == length(src))
            {
                return;
            }

            bool const fBackward = Ctrl_FindPrev == nCtrlId;

            TinyCl::Thread* const pth = TinyCl::Thread::Get();
            pth->mv_value[0] = src;
            pth->mv_value[1] = Kignore_case;
            pth->mv_value[2] = t;
            pth->mv_value[3] = Kfrom_end;
            pth->mv_value[4] = fBackward ? t : nil;
            pth->m_n = Fixnum::Encode(5);
            // FIXME 2008-07-13 yosi@msn.com We need to handle regex
            // compilation error.
            Val regex = TinyCl::compile_regexV(pth);

            Buffer* const pBuffer = m_pSelection->GetBuffer();

            Val const range = make_range(
                pBuffer->Encode(),
                fBackward ? pBuffer->GetStart() : m_pSelection->GetEnd(),
                fBackward ? m_pSelection->GetStart() : pBuffer->GetEnd() );

            Val const match = buffer_match(regex, range);

            Si::RegexMatch* pMatch = match->StaticCast<Si::RegexMatch>();

            bool fWhole = true;

            if (fWhole)
            {
                // Wrapped search
                if (nil == pMatch->m_matched_p)
                {
                    range->StaticCast<Range>()->m_start = fBackward ?
                        m_pSelection->GetEnd() :
                        pBuffer->GetStart();

                    range->StaticCast<Range>()->m_end = fBackward ?
                        pBuffer->GetEnd() :
                        m_pSelection->GetStart();

                    Val match = buffer_match(regex, range);
                    pMatch = match->StaticCast<Si::RegexMatch>();
                }
            } // if fWhole

            if (nil != pMatch->m_matched_p)
            {
                m_pSelection->SetRange(svref(pMatch->m_captures, zero));
                // FIXME 2008-07-13 yosi@msn.com If backward search, we should
                // set active end as start of selection.
                m_pSelection->SetActive(Kend);

                Editor::Window* pWindow = m_pSelection->m_window->
                    StaticCast<Editor::Window>();

                pWindow->GetPeer()->OnIdle(0);
            }
            else
            {
                // FIXME 2008-07-13 yosi@msn.com Display "Not found" message.
            }
        } // onFindNext_Click
    }; // MyDialog

    static Markup::Body* pBody;

    if (NULL == pBody)
    {
        pBody = new Markup::Body;
        pBody->AppendChild(new Markup::Text("Fi_nd What:"));
        pBody->AppendChild(new Markup::Br);
        pBody->AppendChild(new Markup::TextBox(MyDialog::Ctrl_FindWhat, 20));
        pBody->AppendChild(new Markup::Br);

        pBody->AppendChild(new Markup::Text("Re_place With:"));
        pBody->AppendChild(new Markup::Br);
        pBody->AppendChild(new Markup::TextBox(MyDialog::Ctrl_ReplaceWith, 20));
        pBody->AppendChild(new Markup::Br);

        pBody->AppendChild(new Markup::Button(MyDialog::Ctrl_FindNext))->
                AppendChild(new Markup::Text("_Find Next"));

        pBody->AppendChild(new Markup::Button(MyDialog::Ctrl_FindPrev))->
            AppendChild(new Markup::Text("Find P_revious"));
    } // if

    MyDialog* pDialog = new MyDialog(pSelection, pBody);
    pDialog->DoModal();

    return nil;
} // find_or_replace

defcommand(goto_line)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    using namespace Editor::Peer::RichView;

    class MyDialog : public Peer::RichDialog
    {
        public: enum Ctrl
        {
            Ctrl_Cancel,
            Ctrl_LineNum,
            Ctrl_Ok,
        }; // Ctrl

        private: Selection* m_pSelection;

        public: MyDialog(Selection* pSelection, Markup::Node* p) :
            m_pSelection(pSelection),
            Peer::RichDialog(p) {}

        private: virtual void onCommand(uint nCtrlId, uint nNotify) override
        {
            switch (nCtrlId)
            {
            case Ctrl_Cancel:
                switch (nNotify)
                {
                case BN_CLICKED:
                    ::DestroyWindow(*this);
                    break;
                } // switch nNotify
                break;

            case Ctrl_Ok:
                switch (nNotify)
                {
                case BN_CLICKED:
                    onOk_Click();
                    break;
                } // switch nNotify
                break;
            } // switch nCtrlId
        } // onCommand

        private: void onOk_Click()
        {
            HWND hwndLine = getControl(Ctrl_LineNum);
            char16 wsz[100];
            ::GetWindowTextW(hwndLine, wsz, lengthof(wsz));

            int nN = 0;
            for (const char16* pwsz = wsz; 0 != *pwsz; pwsz++)
            {
                if (*pwsz < '0' || *pwsz > '9') break;
                nN *= 10;
                nN += *pwsz - '0';
            } // for pwsz

            m_pSelection->SetStart(zero);
            for (;;)
            {
                nN -= 1;
                if (nN <= 0) break;
                Count k = m_pSelection->To<Range>()->Move(Kparagraph, one);
                if (zero == k) break;
            } // nN
            ::DestroyWindow(*this);
        } // onOk_Click
    }; // MyDialog

    static Markup::Body* pBody;

    if (NULL == pBody)
    {
        pBody = new Markup::Body;
        pBody->AppendChild(new Markup::Text("Line number (~D-~D):"));
        pBody->AppendChild(new Markup::Br);
        pBody->AppendChild(new Markup::TextBox(MyDialog::Ctrl_LineNum, 20));
        pBody->AppendChild(new Markup::Br);

        pBody->AppendChild(new Markup::Button(MyDialog::Ctrl_Ok))->
                AppendChild(new Markup::Text("Ok"));

        pBody->AppendChild(new Markup::Button(MyDialog::Ctrl_Cancel))->
                AppendChild(new Markup::Text("Cancel"));
    } // if

    MyDialog* pDialog = new MyDialog(pSelection, pBody);
    pDialog->DoModal();

    return nil;
} // goto_line

} // Editor
