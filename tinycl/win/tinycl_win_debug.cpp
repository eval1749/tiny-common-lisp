#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime
// tinycl_runtime.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/win/tinycl_win_debug.cpp#3 $
//
#include "../tinycl_clos.h"

namespace TinyCl
{

namespace
{

class DebugFormatImpl
{
    public: static void Run(const char* psz, va_list args)
    {
        for (;;)
        {
            while ('~' != *psz)
            {
                if (0 == *psz) return;
                writeChar(*psz);
                psz++;
            } // while
            
            psz++;

            char chDirective = *psz++;
            switch (chDirective)
            {
            case '%':
                ::OutputDebugStringA("\r\n");
                break;

            case 'C': case 'c':
            {
                Val obj = va_arg(args, Val);
                if (Character* p = obj->DynamicCast<Character>())
                {
                    writeChar(p->ToCode());
                }
                else
                {
                    printObject(obj);
                }
                break;
            } // c

            case 'D': case 'd':
                printInt(va_arg(args, Val), 10);
                break;

            case 'S': case 's':
                printObject(va_arg(args, Val));
                break;

            case 'X': case 'x':
                printInt(va_arg(args, Val), 16);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch directive
        } // for
    } // Run

    // [P]
    private: static void printAddress(Val obj)
    {
        char sz[20];
        ::wsprintfA(sz, " %p>", obj);
        writeString(sz);
    } // printAddress

    private: static void printClass(Val obj, const char* pszMeta)
    {
        writeString("#<");
        writeString(pszMeta);
        writeChar(' ');

        Instance* p = obj->StaticCast<Instance>();

        Val storage = resolve(p->m_storage);

        Layout_class* q = reinterpret_cast<Layout_class*>(
            storage->StaticCast<Storage>() );

        printObject(q->m_name);
        writeChar('>');
    } // printClass

    private: static void printInt(Val x, int nBase)
    {
        x = resolve(x);

        if (! fixnump(x))
        {
            printObject(x);
            return;
        }

        if (zero == x)
        {
            writeChar('0');
            return;
        }

        Int n = Fixnum::Decode_(x);
        if (n < 0)
        {
            writeChar('-');
            n = -n;
        }

        printIntAux(n, nBase, 1);
    } // printInt

    private: static UInt printIntAux(UInt n, uint nBase, UInt q)
    {
        if (n >= nBase)
        {
            UInt qq = printIntAux(n / nBase, nBase, q);
            printIntAux(n % qq, nBase, 1);
            return qq;
        }

        if (n < 10)
        {
            writeChar(static_cast<char>(n + '0'));
        }
        else
        {
            writeChar(static_cast<char>(n + 'A' - 10));
        }

        return q * nBase;
    } // printIntAux

    private: static void printObject(Val x)
    {
        Val obj = resolve(x);

        if (nil == obj)
        {
            writeString("NIL");
            return;
        }

        if (fixnump(obj))
        {
            printInt(obj, 10);
            return;
        } // if fixnum

        if (Character* p = obj->DynamicCast<Character>())
        {
            writeString("#\\");
            writeChar(p->ToCode());
            return;
        } // if character

        if (Cons* p = obj->DynamicCast<Cons>())
        {
            writeChar(static_cast<char>(0x28));

            for (;;)
            {
                printObject(p->m_car);

                Val runner = resolve(p->m_cdr);
                if (nil == runner)
                {
                    writeChar(static_cast<char>(0x29));
                    return;
                }

                p = runner->DynamicCast<Cons>();
                if (NULL == p)
                {
                    writeString(" . ");
                    printObject(runner);
                    writeChar(static_cast<char>(0x29));
                    return;
                }

                writeChar(' ');
            } // for
        } // if cons

        if (Instance* p = obj->DynamicCast<Instance>())
        {
            if (CLASSD_built_in_class == p->m_classd)
            {
                printClass(obj, "Built-In-Class");
                return;
            }

            if (CLASSD_funcallable_standard_class == p->m_classd)
            {
                printClass(obj, "Funcallable-Standard-Class");
                return;
            }

            if (CLASSD_standard_class == p->m_classd)
            {
                printClass(obj, "Class");
                return;
            }

            if (CLASSD_structure_class == p->m_classd)
            {
                printClass(obj, "Structure-Class");
                return;
            }
        } // if class

        if (CodeObject* p = obj->DynamicCast<CodeObject>())
        {
            writeString("#<");

            if (CLASSD_native_code_closure == p->m_classd)
            {
                writeString("Closure");
            }
            else if (CLASSD_native_code_function == p->m_classd)
            {
                writeString("Function");
            }
            else if (CLASSD_standard_generic_function == p->m_classd)
            {
                writeString("Generic-Function");
            }
            else
            {
                printObject(
                    resolve(p->m_classd)->StaticCast<ClassD>()->
                        m_class->StaticCast<Class>()->m_name );
            }

            writeChar(' ');

            Val name = resolve(
                reinterpret_cast<Layout_native_code_function*>(p)->m_name );

            if (Storage* q = name->DynamicCast<Storage>())
            {
                name = resolve(
                    reinterpret_cast<Layout_generic_function*>(q)->m_name );
            }

            printObject(name);
            printAddress(obj);
            return;
        } // if function

        if (Package* p = obj->DynamicCast<Package>())
        {
            writeString("#<Package ");
            printObject(car(p->m_names));
            writeChar('>');
            return;
        } // if package

        if (SetfCell* p = obj->DynamicCast<SetfCell>())
        {
            writeString("#<Setf-Cell ");
            printObject(p->m_name);
            writeChar('>');
            return;
        } // if setf-cell

        if (SimpleString* p = obj->DynamicCast<SimpleString>())
        {
            writeChar(static_cast<char>(0x22));
            writeString(p->GetStart());
            writeChar(static_cast<char>(0x22));
            return;
        } // if simple-string

        if (Symbol* p = obj->DynamicCast<Symbol>())
        {
            Val pkg = resolve(p->m_package);
            if (nil == pkg)
            {
                writeString("#:");
            }
            else if (PKG_keyword == pkg)
            {
                writeChar(':');
            }

            Val name = resolve(p->m_name);
            writeString(name->StaticCast<SimpleString>()->GetStart());
            return;
        } // if symbol

        {
            writeString("#<");
            Val klass = resolve(class_of(obj));
            printObject(klass->StaticCast<Class>()->m_name);
            printAddress(obj);
        }
    } // printObject

    // [R]
    private: static Val resolve(Val x)
        { return x; }

    // [W]
    private: static void writeChar(char ch)
    {
        char sz[2];
        sz[0] = ch;
        sz[1] = 0;
        ::OutputDebugStringA(sz);
    } // writeChar

    private: static void writeChar(char16 wch)
    {
        char16 wsz[2];
        wsz[0] = wch;
        wsz[1] = 0;
        ::OutputDebugStringW(wsz);
    } // writeChar

    private: static void writeString(const char* psz)
        { ::OutputDebugStringA(psz); }

    private: static void writeString(const char16* pwsz)
        { ::OutputDebugStringW(pwsz); }
}; // DebugFormatImpl

} // namespace

void DebugFormat(const char* psz, ...)
{
    va_list args;
    va_start(args, psz);
    DebugFormatImpl::Run(psz, args);
    va_end(args);
} // DebugForamt


} // TinyCl
