// -*- Mode: C++ -*-
//
// TinyCl - Common Utitlity Classes and Functions
// tinycl/z_util.h
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/z_util.h#10 $
//
#if !defined(INCLUDE_z_util_h)
#define INCLUDE_z_util_h

namespace Common
{

template<class Item_, class Parent_>
class DoubleLinkedList_;

class GlobalObject {};
class DummyParent {};

template<class Item_, class Parent_ = DummyParent>
class DoubleLinkedItem_
{
    friend class DoubleLinkedList_<Item_, Parent_>;

    private: Item_* m_pNext;
    private: Item_* m_pPrev;

    public: DoubleLinkedItem_() :
        m_pNext(NULL),
        m_pPrev(NULL) {}

    public: Item_* GetNext() const { return m_pNext; }
    public: Item_* GetPrev() const { return m_pPrev; }
}; // DoubleLinkedItem_


template<class Item_, class Parent_ = DummyParent>
class DoubleLinkedList_
{
    protected: typedef DoubleLinkedList_<Item_, Parent_> List_;
    private:   typedef DoubleLinkedItem_<Item_, Parent_> Cons_;

    private: Item_* m_pFirst;
    private: Item_* m_pLast;

    public: DoubleLinkedList_() :
        m_pFirst(NULL),
        m_pLast(NULL) {}

    // [A]
    public: Item_* Append(Item_* pItem)
    {
        Cons_* pCons = static_cast<Cons_*>(pItem);

        pCons->m_pNext = NULL;
        pCons->m_pPrev = m_pLast;

        if (NULL == m_pFirst)
        {
            m_pFirst = pItem;
        } // if

        if (NULL != m_pLast)
        {
            static_cast<Cons_*>(m_pLast)->m_pNext = pItem;
        } // if

        return m_pLast = pItem;
    } // Append

    // [C]
    public: int Count() const
    {
        int n = 0;
        foreach (Enum, oEnum, this)
        {
            n += 1;
        } // for
        return n;
    } // Count

    // [D]
    public: Item_* Delete(Item_* pItem)
    {
        Cons_* pCons = static_cast<Cons_*>(pItem);

        Item_* pNext = pCons->m_pNext;
        Item_* pPrev = pCons->m_pPrev;
        if (NULL == pNext)
        {
            m_pLast = pPrev;
        }
        else
        {
            static_cast<Cons_*>(pNext)->m_pPrev = pPrev;
        } // if

        if (NULL == pPrev)
        {
            m_pFirst = pNext;
        }
        else
        {
            static_cast<Cons_*>(pPrev)->m_pNext = pNext;
        } // if

        pCons->m_pNext = NULL;
        pCons->m_pPrev = NULL;

        return pItem;
    } // Delete

    public: void DeleteAll()
    {
        while (Item_* pItem = GetFirst())
        {
            Delete(pItem);
        } // while
    } // DeleteAll

    // [E]
    public: class Enum
    {
        private: Item_* m_pRunner;
        public: Enum(const List_* p) : m_pRunner(p->m_pFirst) {}
        public: bool AtEnd() const { return NULL == m_pRunner; }
        public: Item_* Get() const { return m_pRunner; }
        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = static_cast<Cons_*>(m_pRunner)->m_pNext;
        } // Next
    }; // Enum

    public: class EnumReverse
    {
        private: Item_* m_pRunner;
        public: EnumReverse(const List_* p) : m_pRunner(p->m_pLast) {}
        public: bool AtEnd() const { return NULL == m_pRunner; }
        public: Item_* Get() const { return m_pRunner; }
        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = static_cast<Cons_*>(m_pRunner)->m_pPrev;
        } // Next
    }; // EnumReverse

    // [G]
    public: Item_* GetFirst() const { return m_pFirst; }
    public: Item_* GetLast()  const { return m_pLast; }

    // [I]
    public: Item_* InsertAfter(Item_* pItem, Item_* pRefItem)
    {
        Item_* pNext = static_cast<Cons_*>(pRefItem)->m_pNext;
        if (NULL == pNext)
        {
            m_pLast = pItem;
        }
        else
        {
            static_cast<Cons_*>(pNext)->m_pPrev = pItem;
        }

        static_cast<Cons_*>(pItem)->m_pPrev    = pRefItem;
        static_cast<Cons_*>(pItem)->m_pNext    = pNext;
        static_cast<Cons_*>(pRefItem)->m_pNext = pItem;
        return pItem;
    } // InsertAfter

    public: Item_* InsertBefore(Item_* pItem, Item_* pRefItem)
    {
        Item_* pPrev = static_cast<Cons_*>(pRefItem)->m_pPrev;
        if (NULL == pPrev)
        {
            m_pFirst = pItem;
        }
        else
        {
            static_cast<Cons_*>(pPrev)->m_pNext = pItem;
        }

        static_cast<Cons_*>(pItem)->m_pPrev    = pPrev;
        static_cast<Cons_*>(pItem)->m_pNext    = pRefItem;
        static_cast<Cons_*>(pRefItem)->m_pPrev = pItem;
        return pItem;
    } // InsertBefore

    public: bool IsEmpty() const
    {
        return NULL == m_pFirst;
    } // IsEmpty

    // [P]
    public: Item_* Pop()
    {
        if (Item_* pItem = GetFirst())
        {
            return Delete(pItem);
        }
        return NULL;
    } // Pop

    public: Item_* Prepend(Item_* pItem)
    {
        Cons_* pCons = static_cast<Cons_*>(pItem);

        pCons->m_pNext = m_pFirst;
        pCons->m_pPrev = NULL;

        if (NULL == m_pLast)
        {
            m_pLast = pItem;
        } // if

        if (NULL != m_pFirst)
        {
            static_cast<Cons_*>(m_pFirst)->m_pPrev = pItem;
        } // if

        return m_pFirst = pItem;
    } // Prepend

    // [R]
    public: Item_* Replace(Item_* pNew, Item_* pOld)
    {
        InsertBefore(pNew, pOld);
        Delete(pOld);
        return pNew;
    } // Replace
}; // DoubleLinkedList_


template<class Item_, class Parent_>
class ChildList_;

template<class Item_, class Parent_>
class ChildItem_ : public DoubleLinkedItem_<Item_, Parent_>
{
    friend class ChildList_<Item_, Parent_>;

    protected: Parent_*   m_pParent;

    public: ChildItem_(Parent_* p = NULL) : m_pParent(p) {}
}; // ChildItem_


template<class Item_, class Parent_>
class ChildList_ : public DoubleLinkedList_<Item_, Parent_>
{
    private: typedef DoubleLinkedList_<Item_, Parent_> DoubleLinkedList;
    protected: typedef ChildList_<Item_, Parent_> ChildList;
    protected: typedef ChildItem_<Item_, Parent_> ChildItem;

    // [A]
    public: Item_* Append(Item_* pItem)
    {
        DoubleLinkedList::Append(pItem);

        static_cast<ChildItem*>(pItem)->m_pParent =
            static_cast<Parent_*>(this);

        return pItem;
    } // Append

    // [D]
    public: Item_* Delete(Item_* pItem)
    {
        DoubleLinkedList::Delete(pItem);
        static_cast<ChildItem*>(pItem)->m_pParent = NULL;
        return pItem;
    } // Delete

    // [I]
    public: Item_* InsertAfter(Item_* pItem, Item_* pRefItem)
    {
        DoubleLinkedList::InsertAfter(pItem, pRefItem);

        static_cast<ChildItem*>(pItem)->m_pParent = 
            static_cast<ChildItem*>(pRefItem)->m_pParent;

        return pItem;
    } // InsertAfter

    public: Item_* InsertBefore(Item_* pItem, Item_* pRefItem)
    {
        DoubleLinkedList::InsertBefore(pItem, pRefItem);

        static_cast<ChildItem*>(pItem)->m_pParent =
            static_cast<ChildItem*>(pRefItem)->m_pParent;

        return pItem;
    } // InsertBefore

    // [P]
    public: Item_* Prepend(Item_* pItem)
    {
        DoubleLinkedList::Prepend(pItem);

        static_cast<ChildItem*>(pItem)->m_pParent =
            static_cast<Parent_*>(this);

        return pItem;
    } // Prepend

    // [R]
    public: Item_* Replace(Item_* pNew, Item_* pOld)
    {
        InsertBefore(pNew, pOld);
        Delete(pOld);
        return pNew;
    } // Replace
};  // ChiildList_

/// <remark>
///   C-String enumerator.
/// </remark>
class EnumChar
{
    /// <summary>
    ///   Argument for EnumChar
    /// </summary>
    public: struct Arg
    {
        const char16*   m_pwch;
        const char16*   m_pwchEnd;

        Arg(
            const char16*   pwch,
            int             cwch ) :
                m_pwch(pwch),
                m_pwchEnd(pwch + cwch) {}
    }; // Arg

    private: const char16*   m_pwch;
    private: const char16*   m_pwchEnd;

    /// <summary>
    ///   Construct C-String enumerator.
    /// </summary>
    public: EnumChar(Arg oArg) :
            m_pwch(oArg.m_pwch),
            m_pwchEnd(oArg.m_pwchEnd) {}

    /// <summary>
    ///  Check enumereator at end.
    /// </summary>
    public: bool AtEnd() const
    {
        return m_pwch >= m_pwchEnd;
    } // AtEnd

    /// <summary>
    ///  Returns current character
    /// </summary>
    public: char16 Get() const
    {
        ASSERT(! AtEnd());
        return *m_pwch;
    } // Get

    /// <summary>
    ///  Advance current position
    /// </summary>
    public: void Next()
    {
        ASSERT(! AtEnd());
        m_pwch++;
    } // AtEnd
}; // EnumChar


template<class Node_, class ConsBase_ = GlobalObject>
class SingleLinkedList_
{
    private: typedef SingleLinkedList_<Node_, ConsBase_> List;

    public: struct Cons : public ConsBase_
    {
        Node_*  m_pNode;
        Cons*   m_pNext;

        Cons(Node_* p, Cons* q = NULL) :
            m_pNode(p),
            m_pNext(q) {}
    }; // Cons

    private: Cons*  m_pFirst;

    // ctor
    public: SingleLinkedList_() : m_pFirst(NULL) {}

    // [D]
    public: void DeleteAll()
        { m_pFirst = NULL; }

    // [E]
    public: class Enum
    {
        private: Cons* m_pRunner;

        public: Enum(const List* pList) :
            m_pRunner(pList->GetFirst()) {}

        public: bool AtEnd() const { return NULL == m_pRunner; }

        public: Node_* Get() const
        {
            ASSERT(!AtEnd());
            return m_pRunner->m_pNode;
        } // Get

        public: void Next()
        {
            ASSERT(!AtEnd());
            m_pRunner = m_pRunner->m_pNext;
        } // Next
    }; // Enum

    // [G]
    public: Cons* GetFirst() const
        { return m_pFirst; }

    // [I]
    public: bool IsEmpty() const
        { return NULL == m_pFirst; }

    // [P]
    public: int Position(Node_* pNode) const
    {
        int iPosn = 0;
        foreach (Enum, oEnum, this)
        {
            when (oEnum.Get() == pNode) return iPosn;
            iPosn += 1;
        } // for each cons
        return -1;
    } // Position

    // [R]
    public: void Reverse()
    {
        Cons* pList   = m_pFirst;
        Cons* pRunner = NULL;
        while (NULL != pList)
        {
            // (rotatef (cdr list) runner list)
            Cons* pTemp = pList->m_pNext;
            pList->m_pNext = pRunner;
            pRunner = pList;
            pList = pTemp;
        } // for
        m_pFirst = pRunner;
    } // Reverse

    // [S]
    public: Cons* SetFirst(Cons* pCons)
        { return m_pFirst = pCons; }
}; // SingleLinkedList

template<class T>
class Castable_
{
    private: typedef Castable_<T> Self;

    // [D]
    public: template<class T> T* DynamicCast() const
    {
        if (NULL == this) return NULL;
        T* p = static_cast<T*>(const_cast<Self*>(this));
        return p->Is_(T::Kind_()) ? reinterpret_cast<T*>(p) : NULL;
    } // DynamicCast

    // [G]
    public: virtual const char* GetKind() const = 0;

    // [I]
    public: template<class T> bool Is() const
    {
        if (NULL == this) return false;
        //T* p = static_cast<T*>(const_cast<Self*>(this));
        return Is_(T::Kind_());
    } // Is

    public: virtual bool Is_(const char*) const
        { return false; }

    // [S]
    public: template<class T> T* StaticCast() const
    {
        T* p = DynamicCast<T>();
        ASSERT(NULL != p);
        return p;
    } // StaticCast
}; // Castable_


template<class T, class B>
class WithCastable_ : public B
{
    protected: typedef WithCastable_<T, B> Base;

    // [G]
    public: override const char* GetKind() const
        { return T::Kind_(); }

    // [I]
    public: override bool Is_(const char* psz) const
    { 
        if (T::Kind_() == psz) return true;
        return B::Is_(psz);
    } // Is_
}; // WithCastable_

template<class Node_>
class WorkListItem_;

template<class Item_>
class WorkList_
{
    protected: typedef WorkListItem_<Item_> Cons;

    protected: Item_* m_pHead;

    // cotr
    public: WorkList_() : m_pHead(NULL) {}
    public: ~WorkList_() { MakeEmpty(); }

    // [D]
    public: void Delete(Item_* pItem)
    {
        ASSERT(NULL != pItem);
        ASSERT(pItem->IsInList());

        if (NULL == m_pHead)
        {
            // This WorkList is empty.
            CAN_NOT_HAPPEN();
        }

        if (m_pHead == pItem)
        {
            Pop();
            return;
        }

        Item_* pLast = m_pHead;
        for (;;)
        {
            ASSERT(NULL != pLast);

            Item_* pLastNext = static_cast<Cons*>(pLast)->m_pNext;
            if (pLastNext == pLast)
            {
                // We reach at end of WorkList.
                CAN_NOT_HAPPEN();
            }

            if (pLastNext == pItem)
            {
                Item_* pItemNext = static_cast<Cons*>(pItem)->m_pNext;
                if (pItemNext == pItem)
                {
                    static_cast<Cons*>(pLast)->m_pNext = pLast;
                }
                else
                {
                    static_cast<Cons*>(pLast)->m_pNext = pItemNext;
                }
                static_cast<Cons*>(pItem)->m_pNext = NULL;
                return;
            }

            pLast = pLastNext;
        } // for
    } // Delete

    // [E]
    public: class Enum
    {
        private: Item_* m_pRunner;

        public: Enum(const WorkList_<Item_>* pList) :
            m_pRunner(pList->m_pHead) {}

        public: bool   AtEnd() const { return NULL == m_pRunner; }
        public: Item_* Get()   const { return m_pRunner; }

        public: void Next()
        {
            ASSERT(! AtEnd());
            Item_* pNext = static_cast<Cons*>(m_pRunner)->m_pNext;
            if (pNext == m_pRunner)
            {
                pNext = NULL;
            }
            m_pRunner = pNext;
        } // Next
    }; // Enum

    // [G]
    public: Item_* Get() const
        { ASSERT(! IsEmpty()); return m_pHead; }

    // [I]
    public: template<typename T> Item_* Insert(Item_* pItem)
    {
        Item_** ppRunner = &m_pHead;
        for (;;)
        {
            Item_* pRunner = *ppRunner;
            if (NULL == pRunner)
            {
                static_cast<Cons*>(pItem)->m_pNext = pItem;
                break;
            }

            if (T::GreaterThan(pItem, pRunner))
            {
                static_cast<Cons*>(pItem)->m_pNext = pRunner;
                break;
            }

            ppRunner = &static_cast<Cons*>(pRunner)->m_pNext;

            if (static_cast<Cons*>(pRunner)->m_pNext == pRunner)
            {
                static_cast<Cons*>(pItem)->m_pNext = pItem;
                break;
            }
        } // for

        *ppRunner = pItem;
        return pItem;
    } // Insert

    public: bool IsEmpty() const
        { return NULL == m_pHead; }

    public: bool IsNotEmpty() const
        { return NULL != m_pHead; }

    // [M]
    public: void MakeEmpty()
    {
        while (! IsEmpty()) Pop();
    } // MakeEmpty

    // [P]
    public: Item_* Pop()
    {
        Item_* pItem = m_pHead;
        ASSERT(NULL != pItem);

        m_pHead = static_cast<Cons*>(pItem)->m_pNext;
        static_cast<Cons*>(pItem)->m_pNext = NULL;
        if (m_pHead == pItem)
        {
            m_pHead = NULL;
        }
        return pItem;
    } // Pop

    public: Item_* Push(Item_* pItem)
    {
        ASSERT(NULL != pItem);
        ASSERT(NULL == static_cast<Cons*>(pItem)->m_pNext);

        if (NULL == m_pHead)
        {
            static_cast<Cons*>(pItem)->m_pNext = pItem;
        }
        else
        {
            static_cast<Cons*>(pItem)->m_pNext = m_pHead;
        }

        return m_pHead = pItem;
    } // Push
}; // WorkList_

template<class Node_>
class WorkListItem_
{
    friend class WorkList_<Node_>;

    private: Node_* m_pNext;

    public: WorkListItem_() : m_pNext(NULL) {}

    // [I]
    public: bool IsInList() const { return NULL != m_pNext; }
}; // WorkListItem_

} // Common

char16* lstrchrW(const char16*, char16);
char16* lstrrchrW(const char16*, char16);

#endif //!defined(INCLUDE_z_util_h)
