#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler - IR Type
// tinycl_c_ir_type.cpp
//
// Copyright (C) 2007-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/ir/tinycl_c_ir_type.cpp#14 $
//
#include "../tinycl_c_defs.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

static bool checkTypeSyntax(Val form, int iMin, int iMax, char*)
{
    Val n = safe_list_length(form);
    if (nil == n) return false; // malformed
    if (ge(n, iMin) && le(n, iMax)) return true;
    return false;
} // checkTypeSyntax

static Val getTypeElement(Val form, int iNth, Val def)
{
    while (iNth > 0)
    {
        form = cdr(form);
        iNth -= 1;
    }
    return nil == form ? def : car(form);
} // getTypeElement

bool TyClass::sm_fStaticInit;
TyClass::Map* TyClass::sm_pMap;

/// <summary>
///   Constructor of TyClass.
/// </summary>
TyClass::TyClass(Val klass)
{
    m_typespec = klass;

    if (! sm_fStaticInit)
    {
        sm_pMap->Put(class_name(klass), this);
    }
} // TyClass::TyClass

// [A]
const Type* Type::And(
    const Type* const ptyA,
    const Type* const ptyB )
{
    if (tyNil == ptyA || tyNil == ptyB)
    {
        return tyNil;
    }

    if (tyT == ptyA)
    {
        return ptyB;
    }

    if (tyT == ptyB)
    {
        return ptyA;
    }

    return ptyA->ComputeAnd(ptyB);
} // Type::And

// [B]
void TyClass::BeginStaticInit(Mm* pMm)
{
    ASSERT(NULL == sm_pMap);
    sm_pMap = new(pMm) Map;
} // TyClass::BeginStaticInit

// [C]
const Type* TyClass::ComputeAnd(const Type* that) const
{
    if (Subtype::Yes == this->IsSubtypeOf(that))
    {
        return const_cast<TyClass*>(this);
    }

    if (Subtype::Yes == that->IsSubtypeOf(this))
    {
        return const_cast<Type*>(that);
    }

    if (that->Is<TyClass>())
    {
        DEBUG_FORMAT("and ~S ~S~%", this->Unparse(), that->Unparse());
        return tyNil;
    }

    return const_cast<TyClass*>(this);
} // TyClass::ComputeAnd

TyValues::Arity TyValues::ComputeArity() const
{
    Arity oArity;
    foreach (Types::Enum, oEnum, &m_oReqs)
    {
        oArity.m_iMin += 1;
        oArity.m_iMax += 1;
    }

    foreach (Types::Enum, oEnum, &m_oOpts)
    {
        oArity.m_iMax += 1;
    }

    KeyEntries::Enum oEnumKey(&m_oKeys);

    oArity.m_fRest = NULL != m_pResty || ! oEnumKey.AtEnd();

    return oArity;
} // TyValues::ComputeArity

const Type* TyClass::ComputeDiff(const Type* const pty2) const
{
    if (this == tyList && pty2 == tyNull)
    {
        return tyCons;
    }

    return const_cast<TyClass*>(this);
} // TyClass::ComputeDiff

const Type* TyOr::ComputeDiff(const Type* pty2) const
{
    WorkList_<Type> oTypes;
    uint nCount = 0;
    foreach (Types::Enum, oEnum, &m_oElts)
    {
        const Type* const pty1 = oEnum.Get();
        if (! pty1->Equal(pty2))
        {
            oTypes.Push(const_cast<Type*>(pty1));
            nCount += 1;
        }
    } // for

    switch (nCount)
    {
    case 0:
        return tyNil;

    case 1:
        return oTypes.Pop();

    default:
    {
        Val ty = list(Qor);
        Val tail = ty;
        TyOr* ptyOr = new TyOr(ty, nil);
        while (! oTypes.IsEmpty())
        {
            Type* const pty = oTypes.Pop();
            ptyOr->Append(pty);
            tail = setf_cdr(list(pty->Unparse()), tail);
        } // while
        return ptyOr;
    } // default
    } // switch nCount
} // TyOr::ComputeDiff

Val TyValues::computeTypespec() const
{
    Val const ty = list(Qvalues);
    Val tail = ty;

    foreach (Types::Enum, oEnum, &m_oReqs)
    {
        tail = setf_cdr(list(oEnum.Get()->Unparse()), tail);
    } // for

    if (! m_oOpts.IsEmpty())
    {
        tail = setf_cdr(list(QAoptional), tail);

        foreach (Types::Enum, oEnum, &m_oReqs)
        {
            tail = setf_cdr(list(oEnum.Get()->Unparse()), tail);
        } // for
    } // if optional

    if (NULL != m_pResty)
    {
        tail = setf_cdr(list(QArest), tail);
        tail = setf_cdr(list(m_pResty->Unparse()), tail);
    } // if rest

    if (! m_oKeys.IsEmpty())
    {
        tail = setf_cdr(list(QAkey), tail);

        foreach (KeyEntries::Enum, oEnum, &m_oKeys)
        {
            Val const datum = list(
                oEnum.Get()->m_keyword,
                oEnum.Get()->m_pty->Unparse() );

            tail = setf_cdr(list(datum), tail);
        } // for
    } // if key
    
    return ty;
} // TyValues::computeTypespec

bool TyInteger::Contain(const TyInteger* that) const
{
    if (QA != that->m_min)
    {
        if (QA == this->m_min) return false;
        if (lt(this->m_min, that->m_min)) return false;
    }

    if (QA != that->m_max)
    {
        if (QA == this->m_max) return false;
        if (gt(this->m_max, that->m_max)) return false;
    }

    return true;
} // TyInteger::Contain

// [D]
const Type* Type::Diff(
    const Type* const ptyA,
    const Type* const ptyB )
{
    if (tyNil == ptyA)
    {
        // nil - x = nil
        return tyNil;
    }

    if (tyNil == ptyB)
    {
        // x - nil = x
        return ptyA;
    }

    return ptyA->ComputeDiff(ptyB);
} // Type::Diff

// [E]
void TyClass::EndStaticInit(Mm* const pMm)
{
    ASSERT(! sm_fStaticInit);
    sm_fStaticInit = true;

    Environment* pEnv = VAR(Aruntime_environmentA)->StaticCast<Environment>();
    Val classtab = pEnv->m_classes;
    foreach (HashTable::Enum, oEnum, classtab)
    {
        // FIXME yosi@msn.com 2008-06-22 We must register static
        // classes only.
        Val name  = oEnum.GetKey();
        Val klass = oEnum.GetVal();
        if (NULL == sm_pMap->Get(name))
        {
            DEBUG_FORMAT("Install TyClass ~S~%", name);
            sm_pMap->Put(name, new(pMm) TyClass(klass));
        }
    } // for each entry
} // TyClass::EndStaticInit

bool TyOr::Equal(const Type* p) const
{
    const TyOr* that = p->DynamicCast<TyOr>();
    if (NULL == that)
    {
        return false;
    }

    //= <FIXME date="2008-12-26" by="yosi@msn.com">
    //=  We should handle different order case.
    //= </FIXME>

    Types::Enum oEnumThat(&that->m_oElts);
    foreach (Types::Enum, oEnumThis, &this->m_oElts)
    {
        if (oEnumThat.AtEnd())
        {
            return false;
        }

        if (! oEnumThis.Get()->Equal(oEnumThat.Get()))
        {
            return false;
        }

        oEnumThat.Next();
    } // for

    return true;
} // TyOr::Equal

bool TyValues::Equal(const Type* const p) const
{
    const TyValues* that = p->DynamicCast<TyValues>();
    if (NULL == that)
    {
        return false;
    }

    {
        Types::Enum oEnumThat(&that->m_oReqs);
        foreach (Types::Enum, oEnumThis, &this->m_oReqs)
        {
            if (oEnumThat.AtEnd())
            {
                return false;
            }

            if (! oEnumThis.Get()->Equal(oEnumThat.Get()))
            {
                return false;
            }

            oEnumThat.Next();
        } // for

        if (! oEnumThat.AtEnd())
        {
            return false;
        }
    }

    {
        Types::Enum oEnumThat(&that->m_oOpts);
        foreach (Types::Enum, oEnumThis, &this->m_oOpts)
        {
            if (oEnumThat.AtEnd())
            {
                return false;
            }

            if (! oEnumThis.Get()->Equal(oEnumThat.Get()))
            {
                return false;
            }

            oEnumThat.Next();
        } // for

        if (! oEnumThat.AtEnd())
        {
            return false;
        }
    }

    if (NULL != this->m_pResty)
    {
        if (NULL == that->m_pResty)
        {
            return false;
        }

        if (! this->m_pResty->Equal(that->m_pResty))
        {
            return false;
        }
    }

    {
        KeyEntries::Enum oEnum(&that->m_oKeys);

        foreach (KeyEntries::Enum, oEnumThis, &this->m_oKeys)
        {
            if (oEnum.AtEnd())
            {
                // This has more keys.
                return false;
            }

            const KeyEntry* const pThis = oEnumThis.Get();

            const KeyEntry* pThat = NULL;
            foreach (KeyEntries::Enum, oEnumThat, &that->m_oKeys)
            {
                if (pThis->m_keyword == oEnumThat.Get()->m_keyword)
                {
                    pThat = oEnumThat.Get();
                }
            } // for

            if (NULL == pThat)
            {
                return false;
            }

            if (! pThis->m_pty->Equal(pThat->m_pty))
            {
                return false;
            }

            oEnum.Next();
        } // for

        if (! oEnum.AtEnd())
        {
            // That has more keys.
            return false;
        }
    }

    return true;
} // TyValues::Equal

// [G]
const TyClass* TyClass::Get(Val const klass)
{
    ASSERT(NULL != sm_pMap);

    Val klass_name = class_name(klass);
    if (const TyClass* pTyClass = sm_pMap->Get(klass_name))
    {
        return pTyClass;
    }

    TyClass* pTyClass = new(Context::Get()) TyClass(klass);
    return pTyClass;
} // TyClass::Get

const TyClass* TyClass::Map::Get(Val const klass_name) const
{
    int hashCode = static_cast<int>(
        Fixnum::Decode_(klass_name->StaticCast<Symbol>()->m_hash_code) );

    const Slot* pTop   = m_prgoSlot;
    const Slot* pBtm   = m_prgoSlot + lengthof(m_prgoSlot);
    const Slot* pStart = pTop + hashCode % lengthof(m_prgoSlot);
    const Slot* pRunner = pStart;
    do
    {
        if (pRunner->m_name == klass_name) return pRunner->m_pTyClass;
        if (nil == pRunner->m_name) return NULL;
        pRunner++;
        if (pRunner >= pBtm) pRunner = pTop;
    } while (pRunner != pStart);
    CAN_NOT_HAPPEN();
} // TyClass::Map::Get

const Type* TyValues::GetNthTy(Int iNth) const
{
    foreach (Enum, oEnum, this)
    {
        if (oEnum.AtEnd())
        {
            return tyNull;
        }

        if (oEnum.IsRest())
        {
            return Type::Or(oEnum.Get(), tyNull);
        }

        if (iNth <= 0)
        {
            if (oEnum.IsOptional())
            {
                return Type::Or(oEnum.Get(), tyNull);
            }

            return oEnum.Get();
        }

        iNth -= 1;
    } // for

    return tyNull;
} // TyValues::GetPrimaryTy

// [I]
const Type* TyValues::IsScalar() const
{
    if (TypeItem* const pItem = m_oReqs.GetFirst())
    {
        if (pItem->GetNext() != NULL)
        {
            return NULL;
        }

        if (! m_oOpts.IsEmpty())
        {
            return NULL;
        }

        if (m_pResty)
        {
            return NULL;
        }

        if (! m_oKeys.IsEmpty())
        {
            return NULL;
        }

        return pItem->GetTy();
    } // if

    return NULL;
} // TyValues::IsScalar

/// <summary>
///   Returns Yes if this type is Subtype of specified type. Returns No if
///   if this type is not Subtype of specified type.
/// </summary>
Subtype::Answer TyClass::IsSubtypeOf(const Type* that) const
{
    ASSERT(NULL != that);

    if (this == that)
    {
        return Subtype::Yes;
    }

    if (TyClass* pty = that->DynamicCast<TyClass>())
    {
        return subclassp(this->GetClass(), pty->GetClass()) ?
            Subtype::Yes : Subtype::No;
    } // if class

    if (TyOr* pty = that->DynamicCast<TyOr>())
    {
        Subtype::Answer eSubtype = Subtype::No;
        foreach (TyOr::Enum, oEnum, pty)
        {
            switch (Type::IsSubtype(this, oEnum.Get()))
            {
            case Subtype::Yes:
                return Subtype::Yes;

            case Subtype::Unknown:
                eSubtype = Subtype::Unknown;
                break;
            } // switch
        }
        return eSubtype;
    } // if or

    return Subtype::Unknown;
} // TyClass::IsSubtypeOf

Subtype::Answer TyInteger::IsSubtypeOf(const Type* that) const
{
    if (this == that)
    {
        return Subtype::Yes;
    }

    if (TyClass* p = that->DynamicCast<TyClass>())
    {
        Val klass = p->GetClass();
        if (CLASS_integer == klass)
        {
            return Subtype::Yes;
        }

        if (CLASS_fixnum == klass)
        {
            TyInteger tyFixnum(
                Qfixnum,
                Qfixnum,
                Fixnum::MostNegative,
                Fixnum::MostPositive );
            return tyFixnum.Contain(this) ? Subtype::Yes : Subtype::No;
        }

        return subclassp(CLASS_integer, klass) ? Subtype::Yes : Subtype::No;
    } // if class

    if (const TyInteger* p = that->DynamicCast<TyInteger>())
    {
        return p->Contain(this) ? Subtype::Yes : Subtype::No;
    } // if integer

    // maybe that is (or ...)
    return Subtype::Unknown;
} // TyInteger::IsSubtypeOf

Subtype::Answer Type::IsSubtype(const Type* ptyA, const Type* ptyB)
{
    if (TyValues* ptyVa = ptyA->DynamicCast<TyValues>())
    {
        return ptyVa->IsSubtypeOf(ptyB);
    }

    if (TyValues* ptyVb = ptyB->DynamicCast<TyValues>())
    {
        return Subtype::Unknown;
    }

    if (tyNil == ptyA)
    {
        return Subtype::Yes;
    }

    if (tyT == ptyB)
    {
        return Subtype::Yes;
    }

    if (tyNil == ptyB)
    {
        return Subtype::No;
    }

    return ptyA->IsSubtypeOf(ptyB);
} // Type::IsSubtype

Subtype::Answer TyOr::IsSubtypeOf(const Type* const that) const
{
    if (that == tyT)
    {
        return Subtype::Yes;
    }

    if (that == tyNil)
    {
        return Subtype::No;
    }

    foreach (Types::Enum, oEnum, &m_oElts)
    {
        switch (oEnum.Get()->IsSubtypeOf(that))
        {
        case Subtype::No:
            return Subtype::No;

        case Subtype::Unknown:
            return Subtype::Unknown;
        } // switch answer
    } // for

    return Subtype::Yes;
} // TyOr::IsSubtypeOf

Subtype::Answer TyInteger::IsType(Val x) const
{
    if (! integerp(x))
    {
        return Subtype::No;
    }

    if (QA != m_min)
    {
        if (lt(x, m_min)) return Subtype::No;
    }

    if (QA != m_max)
    {
        if (gt(x, m_max)) return Subtype::No;
    }

    return Subtype::Yes;
} // TyInteger::IsType

Subtype::Answer TyOr::IsType(Val const x) const
{
    Subtype::Answer eAnswer = Subtype::No;

    foreach (Types::Enum, oEnum, &m_oElts)
    {
        switch (oEnum.Get()->IsType(x))
        {
        case Subtype::Yes:
            return Subtype::Yes;

        case Subtype::Unknown:
            eAnswer = Subtype::Unknown;
            break;
        } // switch answer
    } // for

    return eAnswer;
} // TyOr::IsType

// [N]

/// <summary>
///   Advance values element type enumerator.
/// </summary>
void TyValues::Enum::Next()
{
    ASSERT(! AtEnd());

    for (;;)
    {
        switch (m_eState)
        {
        case State_Keyword:
            m_pty = tyT;
            m_eState = State_KeywordValue;
            return;

        case State_KeywordValue:
            m_pty = tySymbol;
            m_eState = State_Keyword;
            return;

        case State_Optional:
            if (NULL != m_pRunner)
            {
                m_pRunner = m_pRunner->GetNext();
                if (NULL != m_pRunner)
                {
                    m_pty = m_pRunner->GetTy();
                    return;
                }
            }

            goto tryRestOrKeys;

        case State_Required:
            if (NULL != m_pRunner)
            {
                m_pRunner = m_pRunner->GetNext();
                if (NULL != m_pRunner)
                {
                    m_pty = m_pRunner->GetTy();
                    return;
                }
            }

            m_pRunner = m_ptyValues->m_oOpts.GetFirst();
            if (NULL != m_pRunner)
            {
                m_eState = State_Optional;

                m_pty = m_pRunner->GetTy();
                return;
            }

            goto tryRestOrKeys;

        tryRestOrKeys:
            if (m_ptyValues->m_oKeys.IsEmpty())
            {
                m_eState = State_Rest;
            }
            else
            {
                m_eState = State_KeywordValue;
            }
            break;

        case State_Rest:
            m_pty = m_ptyValues->m_pResty;
            if (NULL == m_pty)
            {
                m_eState = State_End;
            }
            return;

        case State_Start:
            m_eState = State_Required;
            m_pRunner = m_ptyValues->m_oReqs.GetFirst();
            if (NULL != m_pRunner)
            {
                m_pty = m_pRunner->GetTy();
                return;
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // swtich state
    } // for
} // TyValues::Enum::Next

// [O]
const Type* Type::Or(
    const Type* const ptyA,
    const Type* const ptyB )
{
    class Local
    {
        public: static const Type* Compute(
            const Type* const ptyA,
            const Type* const ptyB )
        {
            if (ptyA == tyNil)
            {
                return ptyB;
            }

            if (ptyB == tyNil)
            {
                return ptyA;
            }

            if (const TyValues* ptyVa = ptyA->DynamicCast<TyValues>())
            {
                if (const TyValues* ptyVb = ptyA->DynamicCast<TyValues>())
                {
                    return TyValues::Or(ptyVa, ptyVb);
                }

                return NULL;
            } // if

            if (const TyValues* ptyVb = ptyA->DynamicCast<TyValues>())
            {
                return NULL;
            } // if

            if (Subtype::Yes == ptyA->IsSubtypeOf(ptyB))
            {
                return ptyB;
            }

            if (Subtype::Yes == ptyB->IsSubtypeOf(ptyA))
            {
                return ptyA;
            }

            if (TyClass* ptyClassA = ptyA->DynamicCast<TyClass>())
            {
                return ptyClassA->ComputeOr(ptyB);
            }

            return NULL;
        } // Compute
    }; // Local

    if (ptyA->Equal(ptyB))
    {
        return ptyA;
    }

    if (const Type* ptyC = Local::Compute(ptyA, ptyB))
    {
        return ptyC;
    }

    return new TyOr(ptyA, ptyB);
} // Type::Or

const Type* TyValues::Or(
    const TyValues* const ptyVa,
    const TyValues* const ptyVb )
{
    if (! ptyVa->sameArity(ptyVb))
    {
        return new TyOr(ptyVa, ptyVb);
    }

    TyValues* ptyC = new TyValues;

    {
        Types::Enum oEnumB(&ptyVb->m_oReqs);
        foreach (Types::Enum, oEnumA, &ptyVa->m_oReqs)
        {
            ptyC->m_oReqs.Append(Type::Or(oEnumA.Get(), oEnumB.Get()));
            oEnumB.Next();
        } // for
    }

    {
        Types::Enum oEnumB(&ptyVb->m_oOpts);
        foreach (Types::Enum, oEnumA, &ptyVa->m_oOpts)
        {
            ptyC->m_oOpts.Append(Type::Or(oEnumA.Get(), oEnumB.Get()));
            oEnumB.Next();
        } // for
    }

    //= <FIXME date="2009-01-01" by="yosi@msn.com">\
    //=  Merget (values ... &key ...)
    //= </FIXME>
    if (NULL == ptyVa->m_pResty && ptyVa->m_oKeys.IsEmpty())
    {
        // No rest parameter
    }
    else
    {
        ptyC->m_pResty = tyT;
    }

    ptyC->m_typespec = ptyC->computeTypespec();

    return ptyC;
} // TyValues::Or

// [P]
/// <summary>
///   Parses type specifier (function (param-type*) value-type)
/// </summary>
const TyFunction* TyFunction::Parse(
    Val const typespec,
    Val const name )
{
    Val const paramty = cadr(typespec);
    Val const valty = caddr(typespec);

    const TyValues* const ptyParam = TyValues::Parse(cons(Qvalues, paramty));
    const Type* const pValueTy = Type::Parse(valty);
    return new TyFunction(
        name,
        list(Qfunction, paramty, valty),
        ptyParam,
        pValueTy );
} // TyFunction::Parse

/// <summary>
///   Parses type specifier (integer [min [max]])
/// </summary>
const TyInteger* TyInteger::Parse(
    Val const typespec, 
    Val const name )
{
    ASSERT(consp(typespec));
    ASSERT(Qinteger == car(typespec));

    if (! checkTypeSyntax(typespec, 1, 3, "(integer [min [max]])"))
    {
        return new TyInteger(typespec, name, zero, zero);
    }

    Val min = getTypeElement(typespec, 1, QA);
    Val max = getTypeElement(typespec, 2, QA);

    if (consp(min))
    {
        min = add(car(min), one);
    }

    if (consp(max))
    {
        max = sub(car(max), one);
    }

    return new TyInteger(typespec, name, min, max);
} // TyInteger::Parse

const Type* TyOr::Parse(Val const typespec, Val const orig)
{
    Val elts = cdr(typespec);
    if (nil == elts)
    {
        return tyNil;
    }

    if (cdr(elts) == nil)
    {
        return Type::Parse(car(elts), orig);
    }
DEBUG_FORMAT("ts=~S name=~S~%", typespec, orig);

    TyOr* const pty = new TyOr(typespec, orig);
    foreach (List::Enum, oEnum, elts)
    {
        pty->Append(const_cast<Type*>(Type::Parse(oEnum.Get())));
    } // for
    return pty;
} // TyOr::Parse

/// <summary>
///   Parse type specifier and return Type object.
/// </summary>
/// <return>A Type object</return>
const Type* Type::Parse(Val const typespec, Val const orig)
{
    if (nil == typespec)
    {
        return tyNil;
    }

    if (classp(typespec))
    {
        Val const klass = typespec;
        if (const TyClass* pTyClass = TyClass::Get(klass))
        {
            return pTyClass;
        }
        return new TyClass(klass);
    } // if class

    if (symbolp(typespec))
    {
        Val const name = typespec;

        Val const klass = find_class(name, nil, TLV(AenvironmentA));
        if (nil != klass)
        {
            return Type::Parse(klass);
        }

        Val const expander = find_type(name, nil, TLV(AenvironmentA));
        if (nil == expander)
        {
            return new TyUndef(name);
        }

        Val expansion = expander;
        if (functionp(expander))
        {
            expansion = funcall(expander, list(name));
        }

        return Parse(expansion, orig);
    } // if symbol

    if (! consp(typespec))
    {
        Context::Get()->Error("Invalid typespec: ~S", typespec);
        return tyT;
    }

    Val const kind = car(typespec);

    if (kind == Qfunction)
    {
        return TyFunction::Parse(typespec, orig);
    }

    if (kind == Qinteger)
    {
        return TyInteger::Parse(typespec, orig);
    }

    if (kind == Qor)
    {
        return TyOr::Parse(typespec, orig);
    }

    if (kind == Qvalues)
    {
        TyValues* const pValueTys = TyValues::Parse(typespec);
        if (const Type* const pty = pValueTys->IsScalar())
        {
            return pty;
        }
        return pValueTys;
    } // if values

    Val const expansion = find_type(kind, nil, TLV(AenvironmentA));
    if (nil == expansion)
    {
        return new TyUndef(typespec);
    }

    if (functionp(expansion))
    {
        return Parse(funcall(expansion, typespec), orig);
    }

    return new TyUndef(typespec);
} // Type::Parse

TyValues* TyValues::Parse(Val const spec)
{
    ASSERT(car(spec) == Qvalues);

    enum State
    {
        State_Key,
        State_Opt,
        State_Req,
        State_Rest,
        State_RestAfter,
    } eState = State_Req;

    TyValues* const pValueTys = new TyValues(spec);

    foreach (List::Enum, oEnum, cdr(spec))
    {
        Val token = oEnum.Get();
        switch (eState)
        {
        case State_Opt:
            if (QArest == token)
            {
                eState = State_Rest;
            }
            else if (QAkey == token)
            {
                eState = State_Key;
            }
            else
            {
                pValueTys->m_oOpts.Append(Type::Parse(token));
            }
            break;

        case State_Req:
            if (QAoptional == token)
            {
                eState = State_Opt;
            }
            else if (QArest == token)
            {
                eState = State_Rest;
            }
            else if (QAkey == token)
            {
                eState = State_Key;
            }
            else
            {
                pValueTys->m_oReqs.Append(Type::Parse(token));
            }
            break;

        case State_Rest:
            pValueTys->m_pResty = Type::Parse(token);
            eState = State_RestAfter;
            break;

        case State_RestAfter:
            if (QAkey == token)
            {
                eState = State_Key;
            }
            else
            {
                Context::Get()->Error("Invalid typespecifier ~S", token);
            }
            break;

        case State_Key:
            if (consp(token) &&
                symbolp(car(token)) &&
                consp(cdr(token)) &&
                cddr(token) == nil )
            {
                KeyEntry* pEntry = new KeyEntry;
                pEntry->m_keyword = car(token);
                pEntry->m_pty     = Type::Parse(cadr(token));
                pValueTys->m_oKeys.Append(pEntry);
            }
            else
            {
                Context::Get()->Error("Expect (keyword typespec): ~S",
                    token );
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch eState
    } // for

    return pValueTys;
} // parseTyValues

void TyClass::Map::Put(
    Val             const klass_name, 
    const TyClass*  const pTyClass)
{
    int hashCode = static_cast<int>(
        Fixnum::Decode_(klass_name->StaticCast<Symbol>()->m_hash_code) );

    Slot* pTop   = m_prgoSlot;
    Slot* pBtm   = m_prgoSlot + lengthof(m_prgoSlot);
    Slot* pStart = pTop + hashCode % lengthof(m_prgoSlot);
    Slot* pRunner = pStart;
    do
    {
        ASSERT(pRunner->m_name != klass_name);
        if (nil == pRunner->m_name)
        {
            pRunner->m_name = klass_name;
            pRunner->m_pTyClass = pTyClass;
            return;
        }
        pRunner++;
        if (pRunner >= pBtm) pRunner = pTop;
    } while (pRunner != pStart);
    CAN_NOT_HAPPEN();
} // TyClass::Map::Put

// [S]
bool TyValues::sameArity(const TyValues* const that) const
{
    {
        Types::Enum oEnumThat(&that->m_oReqs);
        foreach (Types::Enum, oEnumThis, &this->m_oReqs)
        {
            if (oEnumThat.AtEnd())
            {
                return false;
            }
            oEnumThat.Next();
        } // for

        if (! oEnumThat.AtEnd())
        {
            return false;
        }
    }

    {
        Types::Enum oEnumThat(&that->m_oOpts);
        foreach (Types::Enum, oEnumThis, &this->m_oOpts)
        {
            if (oEnumThat.AtEnd())
            {
                return false;
            }
            oEnumThat.Next();
        } // for

        if (! oEnumThat.AtEnd())
        {
            return false;
        }
    }

    if (NULL == this->m_pResty && this->m_oKeys.IsEmpty())
    {
        return NULL == that->m_pResty && that->m_oKeys.IsEmpty();
    }

    return NULL != that->m_pResty || ! that->m_oKeys.IsEmpty();
} // TyValues::sameArity

} // Compiler
} // TinyCl
