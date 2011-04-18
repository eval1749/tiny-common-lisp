//////////////////////////////////////////////////////////////////////////////
//
// Editor - Window
// editor/ed_window.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_window.h#2 $
//
#if !defined(INCLUDE_editor_window_h)
#define INCLUDE_editor_window_h

#include "./ed_defs.h"

namespace Editor
{

template<class T, typename L, class P>
class Window_ : public EditorObject_<T, L>
{
    protected: typedef Window_<T, L, P> Super;

    protected: Window_()
    {
        m_active_tick = zero;
        m_parent      = nil;
        m_peer        = zero;
        m_visible     = t;
    } // Window_

    // [A]
    public: void AttachPeer()
    {
        if (HasPeer())
        {
            error(Si::Qalready_realized, Kobject, Encode());
        }

        P* pPeer = new P(Encode());
        if (NULL == pPeer)
        {
            error(Qstorage_condition);
        }

        m_peer = Fixnum::Encode(pPeer);
    } // AttachPeer

    // [D]
    public: void DetachPeer()
    {
        ASSERT(HasPeer());
        m_peer = zero;
    } // DetachPeer

    // [G]
    public: P* GetPeer() const 
    {
        ASSERT(HasPeer());
        return m_peer->To<P>();
    } // GetPeer

    public: static bool Is_(const TinyCl::Datum* const x)
    {
        if (Record* p = x->DynamicCast<Record>())
        {
            return T::ClassD_() == p->m_classd;
        }
        return false;
    } // Is_

    // [H]
    public: bool HasPeer() const
    {
        return zero != m_peer;
    } // HasPeer
}; // Window_

template<class T, typename L, class P>
class Container_ : public Window_<T, L, P>
{
    protected: typedef Container_<T, L, P> Super;

    protected: Container_()
    {
        m_child_windows = nil;
    } // Container_

    public: class EnumChild : public List::Enum
    {
        public: EnumChild(L* p) :
            List::Enum(p->m_child_windows) {}
    }; // EnumChild
}; // Container_

class Window :
    public Window_<Window, Layout_window, Peer::Window>
{
    public: static bool Is_(const TinyCl::Datum* const x)
    {
        if (Record* p = x->DynamicCast<Record>())
        {
            const ClassD* pClassD = p->m_classd->StaticCast<ClassD>();
            return nil != memq(CLASS_window, pClassD->m_class_precedence_list);
        }
        return false;
    } // Is_
}; // Window

CASSERT(sizeof(Layout_window) == sizeof(Window));

class Container :
    public Container_<Container, Layout_container, Peer::Container>
{
    public: static bool Is_(const Datum* const x)
    {
        if (Record* p = x->DynamicCast<Record>())
        {
            const ClassD* pClassD = p->m_classd->StaticCast<ClassD>();
            return nil != memq(
                CLASS_container,
                pClassD->m_class_precedence_list );
        }
        return false;
    } // Is_
}; // Container

CASSERT(sizeof(Layout_container) == sizeof(Container));

inline bool container_p(Val x)
    { return x->Is<Container>(); }

} // Editor

#endif //!defined(INCLUDE_editor_window_h)
