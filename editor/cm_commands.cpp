#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Editor - Commands
// editor/cm_commands.cpp
//
// Copyright (C) 1996-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/cm_commands.cpp#13 $
//
//
#include "./cm_cmdl.h"
#include "./cm_keymap.h"

#include "./ed_buffer.h"
#include "./ed_frame.h"
#include "./ed_range.h"
#include "./ed_selection.h"
#include "./ed_style.h"
#include "./ed_text_window.h"

#include "./rtl_defs.h"

#include "./peer/peer_buffer_list.h"
#include "./peer/peer_file_dialog.h"
#include "./peer/peer_frame.h"
#include "./peer/peer_root_window.h"
#include "./peer/peer_text_window.h"
#include "./peer/peer_split_container.h"

namespace Editor
{

static Val motionArg()
{
    Val arg = TLV(AprefixA);
    if (fixnump(arg))
    {
        return arg;
    }

    return one;
} // motionArg

#define define_motion_command(mp_dir, mp_motion, mp_unit) \
    defcommand(mp_dir ## _ ## mp_unit) \
    { \
        Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>(); \
        if (NULL == pSelection) return nil; \
        Val arg = motionArg(); \
        pSelection->Move ## mp_motion (K ## mp_unit, arg, nil); \
        return nil; \
    } \
    defcommand(mp_dir ## _ ## mp_unit ## _extend) \
    { \
        Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>(); \
        if (NULL == pSelection) return nil; \
        Val arg = motionArg(); \
        pSelection->Move ## mp_motion (K ## mp_unit, arg, Kextend); \
        return nil; \
    } // define_motion_commad

#define define_start_end_command(mp_name, mp_motion, mp_unit) \
    defcommand(mp_name) \
    { \
        Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>(); \
        if (NULL == pSelection) return nil; \
        return pSelection->mp_motion (K ## mp_unit, nil); \
    } \
    defcommand(mp_name ## _extend) \
    { \
        Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>(); \
        if (NULL == pSelection) return nil; \
        return pSelection->mp_motion (K ## mp_unit, Kextend); \
    } // define_start_end_command

#define define_delete_command(mp_dir, mp_unit, mp_arg) \
    defcommand(mp_dir ## _delete_ ## mp_unit) \
    { \
        Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>(); \
        if (NULL == pSelection) return nil; \
        Val arg = motionArg(); \
        return pSelection->Delete(K ## mp_unit, mp_arg); \
    } // define_delete_command

#define Kchar   Kcharacter

// [B]
define_delete_command(backward, char, xsub(zero, arg))
define_delete_command(backward, word, xsub(zero, arg))

define_motion_command(backward, Left, char)
define_motion_command(backward, Left, word)
define_motion_command(backward, Up,   line)
define_motion_command(backward, Up,   screen)
define_motion_command(backward, Up,   window)

// [C]
defcommand(close_buffer)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    if (can_close_p(pSelection->m_buffer))
    {
        return nil;
    }

    return close_buffer(pSelection->m_buffer);
} // close_buffer

defcommand(close_other_windows)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Peer::Window* pWindow = pSelection->GetWindow();

    if (Peer::Container* pContainer =
            Peer::Window::FromHwnd(::GetParent(*pWindow))->
                DynamicCast<Peer::Container>() )
    {
        Peer::Container::EnumChild oEnum(pContainer);
        while (! oEnum.AtEnd())
        {
            Peer::Window* pDelete = oEnum.Get();
            oEnum.Next();
            if (pDelete != pWindow)
            {
                ::DestroyWindow(*pDelete);
            }
        } // while
    } //if container
    return nil;
} // close_other_windows

defcommand(close_this_window)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    ::DestroyWindow(pSelection->GetWindow()->GetHwnd());
    return nil;
} // clsose_this_window

defcommand(copy_to_clipboard)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    return pSelection->To<Range>()->Copy();
} // copy_to_clipboard

defcommand(cut_to_clipboard)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    return pSelection->To<Range>()->Cut();
} // copy_to_clipboard

// [E]
define_start_end_command(end_key, EndKey, line)
define_start_end_command(end_of_buffer, EndKey, buffer)

defcommand(enter_key)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Val arg = motionArg();
    if (xlt(arg, zero)) return nil;

    // Get leading whitepaces
    Val spaces;
    {
        StackRange oRange(pSelection);
        oRange.StartOf(Kparagraph);
        oRange.MoveEndWhile(" \t");
        spaces = oRange.GetText();
    } // spaces

    // We don't merge enter-key records.
    Buffer::UndoBlock oUndo(pSelection, Qenter_key);

    // Delete trailing whitespaces
    {
        StackRange oRange(pSelection);
        oRange.MoveStartWhile(" \t", Kbackward);
        oRange.SetText(NULL, 0);
    }

    pSelection->TypeChar(Character::FromCode(0x0A), arg);
    pSelection->To<Range>()->MoveEndWhile(" \t");
    pSelection->To<Range>()->SetText(spaces);
    pSelection->To<Range>()->Collapse(Kend);
    return nil;
} // enter_key

defcommand(eval_last_form)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Val form;
    {
        StackRange oRange(pSelection);

        if (oRange.GetStart() == oRange.GetEnd())
        {
            Posn posn = oRange.To<Range>()->FindOpenParen();
            if (nil != posn) oRange.SetStart(posn);
        } // if

        if (oRange.GetStart() == oRange.GetEnd())
        {
            return nil;
        }

        Val in = make_buffer_input_stream(oRange.Encode());
        Val cond = Si::CallFunction(Qread, &in, 1);
        if (nil == cond)
        {
            form = Thread::Get()->mv_value[0];
        }
        else
        {
            form = list(Qquote, cond);
        }
    } // form


    DEBUG_FORMAT("form=~S~%", form);
    Val cond = Si::CallFunction(Qeval, &form, 1);

    pSelection = TLV(AselectionA)->DynamicCast<Selection>();

    Val vals = nil == cond ? Thread::Get()->ValuesToList() : cond;

    Val out = make_buffer_output_stream(
        pSelection->m_buffer,
        pSelection->GetEnd() );

    print(vals, out);

    return nil;
} // eval_las_form

// [F]
define_delete_command(forward, char, arg)
define_delete_command(forward, word, arg)

define_motion_command(forward, Right, char)
define_motion_command(forward, Right, word)
define_motion_command(forward, Down,  line)
define_motion_command(forward, Down,  screen)
define_motion_command(forward, Down,  window)

// [G]
defcommand(goto_close_paren)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    Posn posn = pSelection->To<Range>()->FindCloseParen();
    if (nil != posn) pSelection->SetRange(posn, posn);
    return posn;
} // goto_close_parent

defcommand(goto_close_paren_extend)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    if (pSelection->IsStartActive()) pSelection->Collapse(Kend);
    Posn posn = pSelection->To<Range>()->FindCloseParen();
    if (nil != posn)
    {
        pSelection->SetEnd(posn);
        pSelection->SetActive(Kend);
    }
    return posn;
} // goto_close_parent_extend

defcommand(goto_open_paren)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    Posn posn = pSelection->To<Range>()->FindOpenParen();
    if (nil != posn) pSelection->SetRange(posn, posn);
    return posn;
} // goto_open_parent

defcommand(goto_open_paren_extend)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    if (! pSelection->IsStartActive()) pSelection->Collapse(Kstart);
    Posn posn = pSelection->To<Range>()->FindOpenParen();
    if (nil != posn)
    {
        pSelection->SetStart(posn);
        pSelection->SetActive(Kstart);
    }
    return posn;
} // goto_open_parent_extend

defcommand(graphic_key)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Val arg = motionArg();

    CommandState* pCmdState = VAR(Acommand_stateA)->
        StaticCast<CommandState>();

    Val ch = car(pCmdState->m_events);
    if (! characterp(ch)) return nil;

    Buffer::UndoBlock oUndo(pSelection, Qgraphic_key, pSelection->GetStart());
    pSelection->TypeChar(ch, arg);

    // FIXME 2007-12-28 yosi@msn.com We should get closed parenthesis
    // from syntax table.
    Posn posn = nil;
    switch (ch->StaticCast<Character>()->ToCode())
    {
    case CloseParen:
        posn = pSelection->To<Range>()->FindOpenParen(OpenParen, CloseParen);
        break;

    case CloseBrace:
        posn = pSelection->To<Range>()->FindOpenParen(OpenBrace, CloseBrace);
        break;

    case CloseBracket:
        posn = pSelection->To<Range>()->FindOpenParen(OpenBracket, CloseBracket);
        break;
    } // switch ch

    if (nil != posn)
    {
        Val wait = xlt(posn, pSelection->GetWindow()->GetStart()) ?
            Fixnum::Encode(500) :
            Fixnum::Encode(100);

        pSelection->GetWindow()->Blink(posn, wait);
    }
    else
    {
        // No matched parenthesis
    } // if

    return nil;
} // graphic_key

// [H]
define_start_end_command(home_key, HomeKey, line)

// [L]
defcommand(list_buffer)
{
    Peer::BufferListWindow* pBufferList = Peer::BufferListWindow::Get();

    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL != pSelection)
    {
        Peer::Window* pWindow = pSelection->GetWindow();
        Peer::Frame* pFrame = pWindow->GetFrame();

        if (NULL == pBufferList->GetParent())
        {
            pFrame->AppendChild(pBufferList);
            return nil;
        }

        if (pBufferList->GetFrame() == pFrame)
        {
            pBufferList->Activate();
            return nil;
        }

        pWindow->GetParent()->ReplaceChild(pBufferList, pWindow);
        return nil;
    } // if

    if (Peer::Frame* pFrame = 
            Peer::RootWindow::Get()->GetActiveWindow()->
                DynamicCast<Peer::Frame>() )
    {
        pFrame->AppendChild(pBufferList);
        return nil;
    }

    return nil;
} // list_buffer

// [N]

/// <summary>
///   Display activate buffer in new window.
/// </summary>
defcommand(new_frame)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    Val frame = (new Frame)->Encode();
    Val window = (new TextWindow(pSelection->m_buffer))->Encode();
    window->StaticCast<TextWindow>()->m_selection->StaticCast<Selection>()->
        SetRange(
            pSelection->GetStart(),
            pSelection->GetEnd() );
    add_window(frame, window);
    //realize_instance(frame);
    show_window(frame);
    return frame;
} // new_frame

/// <summary>
///   Move active buffer to new frame window.
/// </summary>
defcommand(new_frame_and_close)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    // FIXME 2008-08-04 yosi@msn.com We've not set frame-first-window and
    // frame-last-window yet.
    #if 0
    {
        Frame* pFrame = pSelection->GetWindow()->GetFrame()->
            GetObject()->StaticCast<Frame>();

        if (pFrame->m_first_window == pFrame->m_last_window)
        {
            format(t, "This is a single window in ~S.~%", pFrame->Encode());
            return nil;
        }
    }
    #else
    {
        Peer::Frame* pFrame = pSelection->GetWindow()->GetFrame();
        if (pFrame->GetContainer()->GetFirstChild() ==
            pFrame->GetContainer()->GetLastChild() )
        {
            format(t, "This is a single window in ~S.~%",
                pFrame->GetObject() );

            return nil;
        }
    }
    #endif

    Val frame = (new Frame)->Encode();

    Val window = (new TextWindow(pSelection->m_buffer))->Encode();

    window->StaticCast<TextWindow>()->m_selection->StaticCast<Selection>()->
        SetRange(
            pSelection->GetStart(),
            pSelection->GetEnd() );

    add_window(frame, window);
    realize_instance(frame);
    show_window(frame);

    ::DestroyWindow(pSelection->GetWindow()->GetHwnd());

    //activate_window(window);

    return window;
} // new_frame_and_close

defcommand(next_window)
{
    class Internal
    {
        private: static Peer::Window* firstLeaf(Peer::Window* pWindow)
        {
            while (Peer::Container* pContainer =
                    pWindow->DynamicCast<Peer::Container>() )
            {
                if (Peer::Window* pFirst = pContainer->GetFirstChild())
                {
                    pWindow = pFirst;
                }
                else
                {
                    return pContainer;
                }
            } // while

            return pWindow;
        } // firstLeaf

        public: static Peer::Window* GetNext(Peer::Window* pWindow)
        {
            if (Peer::Window* pNext = pWindow->GetFollowing())
            {
                if (pNext->GetFrame() == pWindow->GetFrame())
                {
                    return firstLeaf(pNext);
                }
            } // if next
            return firstLeaf(pWindow->GetFrame());
        } // GetNext
    }; // Internal

    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    if (Peer::Window* pNext = Internal::GetNext(pSelection->GetWindow()))
    {
        // FIXME 2007-12-30 yosi@msn.com We should return window object.
        // FIXME 2007-12-30 yosi@msn.com Should we switch between text
        // window only?
        pNext->Activate();
        return t;
    }

    return nil;
} // next_window

// [O]

// Load file in new tab or new frame
// Note: Set initial directory of file dialog from current buffer.
defcommand(open_file)
{
    Peer::FileDialog::Param oParam;

    Peer::FileDialog oFileDialog;
    if (! oFileDialog.GetOpenFileName(&oParam))
    {
        return nil;
    }

    char16 wszFileName[MAX_PATH + 1];
    char16* pwszFile;
    ::GetFullPathName(
        oParam.m_wsz,
        lengthof(wszFileName),
        wszFileName,
        &pwszFile );

    Val buffer = nil;

    foreach (List::Enum, oEnum, VAR(AbuffersA))
    {
        Val runner = oEnum.Get();
        Buffer* pBuffer = runner->StaticCast<Buffer>();
        if (0 == ::lstrcmpiW(pBuffer->GetFileName(), wszFileName))
        {
            buffer = runner;
            break;
        }
    } // for each buffer

    if (nil == buffer)
    {
        buffer = make_buffer(make_string(pwszFile));
        buffer->StaticCast<Buffer>()->Load(wszFileName);
    }

    Val window = (new TextWindow(buffer))->Encode();
    realize_instance(window);

    if (nil == TLV(AprefixA))
    {
        if (Selection* pSelection =
            TLV(AselectionA)->DynamicCast<Selection>() )
        {
            if (Peer::Frame* pFrame = pSelection->GetWindow()->GetFrame())
            {
                add_window(pFrame->GetObject(), window);
                return buffer;
            }
        } // if selection
    } // if prefix argument

    Val frame = (new Frame)->Encode();
    add_window(frame, window);
    realize_instance(frame);
    show_window(frame);

    return buffer;
} // open_file

// [P]
defcommand(paste_from_clipboard)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;
    return pSelection->To<Range>()->Paste();
} // paster_from_clipboard

defcommand(previous_window)
{
    class Internal
    {
        private: static Peer::Window* lastLeaf(Peer::Window* pWindow)
        {
            while (Peer::Container* pContainer =
                    pWindow->DynamicCast<Peer::Container>() )
            {
                if (Peer::Window* pLast = pContainer->GetLastChild())
                {
                    pWindow = pLast;
                }
                else
                {
                    return pContainer;
                }
            } // while

            return pWindow;
        } // lastLeaf

        public: static Peer::Window* GetPrev(Peer::Window* pWindow)
        {
            if (Peer::Window* pPrev = pWindow->GetPreceding())
            {
                if (pPrev->GetFrame() == pWindow->GetFrame())
                {
                    return lastLeaf(pPrev);
                }
            } // if prev
            return lastLeaf(pWindow->GetFrame());
        } // GetPrev
    }; // Internal

    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    if (Peer::Window* pPrev = Internal::GetPrev(pSelection->GetWindow()))
    {
        // FIXME 2007-12-30 yosi@msn.com We should return window object.
        // FIXME 2007-12-30 yosi@msn.com Should we switch between text window only?
        pPrev->Activate();
        return t;
    }

    return nil;
} // previous_window

// [R]
defcommand(reconvert)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    if (pSelection->GetStart() == pSelection->GetEnd())
    {
        pSelection->StartOf(Kword, Kextend);
    }

    pSelection->GetWindow()->Reconvert(
        pSelection->GetStart(),
        pSelection->GetEnd() );

    return nil;
} // reconvert

defcommand(redo)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Posn lPosn = pSelection->GetBuffer()->Redo(
        pSelection->GetActivePosn() );

    if (nil != lPosn)
    {
        pSelection->MoveTo(lPosn);
    }
    else
    {
        format(t, "No more redo.");
    }

    return nil;
} // redo

// [S]
defcommand(save_file)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    bool fSaveAs = nil != TLV(AprefixA);

    Buffer* pBuffer = pSelection->GetBuffer();


    Peer::FileDialog::Param oParam;

    if (fSaveAs || 0 == pBuffer->GetFileName()[0])
    {
        ::lstrcpyW(oParam.m_wsz, pBuffer->GetFileName());

        Peer::FileDialog oFileDialog;
        if (! oFileDialog.GetSaveFileName(&oParam))
        {
            return nil;
        }

        const char16* pwszName = ::lstrrchrW(oParam.m_wsz, '\\');
        if (NULL == pwszName)
        {
            pwszName = ::lstrrchrW(oParam.m_wsz, '/');
            if (NULL == pwszName)            {
                pwszName = oParam.m_wsz + 1;
            }
        }

        // Skip slash(/)
        pwszName++;

        rename_buffer(pBuffer->Encode(), make_string(pwszName));
    }
    else if (! pBuffer->IsModified())
    {
        // Buffer isn't modified.
        format(t, "~S isn't modified.~%", pBuffer->Encode());
        return nil;
    } // if

    if (! pBuffer->Save(oParam.m_wsz))
    {
        return nil;
    }

    return pSelection->m_buffer;
} // save_file

defcommand(select_all)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    pSelection->StartOf(Kbuffer);
    Count k = pSelection->EndOf(Kbuffer, Kextend);
    pSelection->SetActive(Kend);
    return k;
} // select_all

defcommand(split_window_horizontally)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Peer::Window* pCurWin = pSelection->GetWindow();

    if (Peer::SplitContainer* pContainer = pCurWin->GetParent()->
                DynamicCast<Peer::SplitContainer>() )
    {
        Val newwin = (new TextWindow(pSelection->m_buffer))->Encode();
        realize_instance(newwin);
        Peer::Window* pNewWin = newwin->StaticCast<TextWindow>()->GetPeer();
        if (pContainer->SplitHorizontally(pNewWin, pCurWin))
        {
            return nil;
        } // if
    } // if

    // FIXME 2007-12-29 yosi@msn.com NYI: Display warning

    return nil;
} // split_window_horizontally

defcommand(split_window_vertically)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Peer::Window* pCurWin = pSelection->GetWindow();

    if (Peer::SplitContainer* pContainer = pCurWin->GetParent()->
                DynamicCast<Peer::SplitContainer>() )
    {
        Val newwin = (new TextWindow(pSelection->m_buffer))->Encode();
        realize_instance(newwin);
        Peer::Window* pNewWin = newwin->StaticCast<TextWindow>()->GetPeer();
        if (pContainer->SplitVertically(pNewWin, pCurWin))
        {
            return nil;
        } // if
    } // if

    // FIXME 2007-12-29 yosi@msn.com NYI: Display warning

    return nil;
} // split_window_vertically

define_start_end_command(start_of_buffer, HomeKey, buffer)

// [U]
defcommand(undo)
{
    Selection* pSelection = TLV(AselectionA)->DynamicCast<Selection>();
    if (NULL == pSelection) return nil;

    Posn lPosn = pSelection->GetBuffer()->Undo(
        pSelection->GetActivePosn() );

    if (nil != lPosn)
    {
        pSelection->MoveTo(lPosn);
    }
    else
    {
        format(t, "No more undo.");
    }
    
    return nil;
} // undo

} // Editor
