#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Compiler
// compiler/tinycl_c_main.cpp
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/compiler/tinycl_c_main.cpp#23 $
//
#include "./tinycl_c_defs.h"
#include "./cg/tinycl_c_cg.h"

namespace TinyCl
{

using namespace Private;

namespace Compiler
{

#define defstatic(mp_ctype, mp_cname) \
    mp_ctype* mp_cname;

#define defstatic2(mp_ctype, mp_cname, mp_a) \
    mp_ctype* mp_cname;

#include "./tinycl_compiler.inc"

TyUnknownFunction::TyUnknownFunction() :
    TyFunction(
        nil,
        list(Qfunction, list(QArest, t), list(Qvalues, QArest, t)),
        tyValuesRestT,
        tyValuesRestT ) {}

const char*
Instruction::sm_rgpszMnemonic[Op_Limit] =
{
    "None",

    #define definstruction(mp_name, mp_format) #mp_name,
    #include "./tinycl_compiler.inc"
}; // Instruction::sm_rgpwszMnemonic

const InstructionDispatcher::ProcessIFun
InstructionDispatcher::k_rgpfnProcessIFun[Op_Limit] =
{
    NULL,

    #define definstruction(mp_name, mp_format) \
        PROCESS_I_NAME_(_ ## mp_name),

    #include "./tinycl_compiler.inc"
}; // k_rgpfnProcessIFun

// Compiler passes
#define Pass(mp_name, mp_bits) DECLPASS(mp_name);
    #include "./tinycl_compiler.inc"

typedef void (*PassFn)();

struct PassDesc
{
    PassFn  m_pfn;
}; // PassDesc

PassDesc k_rgoPassDesc[] =
{
    #define Pass(mp_name, mp_bits) PASS_ENTRY_(mp_name),
    #include "./tinycl_compiler.inc"
}; // k_rgoPassDesc

static Val make_html_encode_stream(Val base)
{
    Val const stream = Thread::Get()->AllocInstance(CLASSD_html_encode_stream);
    HtmlEncodeStream* const p = stream->StaticCast<HtmlEncodeStream>();
    p->m_mode = nil;
    p->m_stream = base;
    return stream;
} // make_html_encode_stream

// [C]

Context::Context(Val form) :
    m_cBBlocks(0),
    m_cFunctions(0),
    m_cOutputs(0),
    m_iDepth(0),
    m_iPass(0),
    m_iSubPass(0),
    m_pPass(NULL),
    m_pTarget(NULL),

    m_form(form),
    m_errors(nil),
    m_previous(TLV(c_AcontextA)),
    m_truename(nil),
    m_warnings(nil)
{
    Context* pPrevious = NULL;
    if (nil != m_previous)
    {
        pPrevious = m_previous->To<Context>();
        m_iDepth = pPrevious->m_iDepth + 1;
    }

    // Remember filename for reporting
    {
        if (Qcompile_file == TLV(c_AsituationA))
        {
            m_truename = TLV(Acompile_file_truenameA);
        }
        else if (Qload == TLV(c_AsituationA))
        {
            m_truename = TLV(Aload_truenameA);
        }
        else if (NULL != pPrevious)
        {
            m_truename = pPrevious->GetTruename();
        }
    } // truename

    TLV(c_AcontextA) = Fixnum::Encode(this);
} // Context::Context

Context::~Context()
{
    TLV(c_AcontextA) = m_previous;
} // Context::~Context

void Context::Error(const char* psz)
    { ErrorV(psz, nil); }

void Context::Error(const char* psz, Val a)
    { ErrorV(psz, list(a)); }

void Context::Error(const char* psz, Val a, Val b)
    { ErrorV(psz, list(a, b)); }

void Context::Error(const char* psz, Val a, Val b, Val c)
    { ErrorV(psz, list(a, b, c)); }

void Context::ErrorV(const char* const psz, Val const args)
{
    Val const ctrl = make_string(psz);
    format(t, "; Compiler-Error: ");
    values_list(listA(t, ctrl, args));
    formatV(Thread::Get());
    write_char(Newline);
    push(list(ctrl, args), m_errors);

    if (::IsDebuggerPresent())
    {
        __debugbreak();
    }
} // Context::Error

void Context::InternalError(
    const char* const pszFile,
    int         const iLine,
    const char* const psz )
{
    Log(1, "<div class='e'>Internal error at ~A(~D): ~A</div>",
        pszFile,
        iLine,
        psz );

    Error("Internal error at ~S(~D): ~A",
        make_string(pszFile),
        Fixnum::Encode(iLine),
        make_string(psz) );
} // Context::InternalError

void Context::Log(int, const char* psz, ...)
{
    Context* pContext = Context::Get();

    Val stream = pContext->GetPass()->GetStream();
    if (nil == stream)
    {
        return;
    }

    va_list args;
    va_start(args, psz);
    cformatv(stream, psz, args);
    va_end(args);
} // Context::Log

void Context::NoteV(Instruction*, const char* psz, Val args)
{
    write_string("; Note: ");
    Val const ctrl = make_string(psz);
    values_list(listA(t, ctrl, args));
    formatV(Thread::Get());
    write_char(Newline);
} // Context::Note

void cformat(Val stream, const char* psz, ...)
{
    va_list args;
    va_start(args, psz);
    cformatv(stream, psz, args);
    va_end(args);
} // cformat

void cformatv(Val stream, const char* psz, va_list args)
{
    char sz[100];

    while (0 != *psz)
    {
        char ch = *psz++;
        if ('~' != ch)
        {
            write_char(ch, stream);
            continue;
        }

        bool fColon = false;
        if (':' == *psz)
        {
            fColon = true;
            psz++;
        }

        switch (*psz)
        {
        case '%':
            if (fColon) write_string("<br/>", stream);
            write_char(Newline, stream);
            break;

        case 'A': case 'a':
            write_string(va_arg(args, char*), stream);
            break;

        case 'D': case 'd':
            ::wsprintfA(sz, "%ld", va_arg(args, Int));
            write_string(sz, stream);
            break;

        case 'S': case 's':
        {
            Object* p = va_arg(args, Object*);
            if (NULL == p)
            {
                write_string("<i>null</i>", stream);
            }
            else
            {
                p->HtmlPrint(stream);
            }
            break;
        } // S

        case 'X': case 'x':
            ::wsprintfA(sz, "%lx", va_arg(args, Int));
            write_string(sz, stream);
            break;

        case 'W': case 'w':
        {
            HtmlEncodeStream* const p =
                stream->StaticCast<HtmlEncodeStream>();

            p->m_mode = Khtml;

            print_object(va_arg(args, Val), stream);

            p->m_mode =nil;
            break;
        } // W

        default:
            error("Unsupported directive ~~C", Character::FromCode(*psz));
        } // switch ch

        psz++;
    } // while
} // cformatv

Val Context::Run()
{
    class GlobalMm : public Mm
    {
        public: GlobalMm() :
            Mm(::GetProcessHeap()) {}

        public: ~GlobalMm()
            { detach(); }
    }; // GlobalMm

    // Initialize static objects
    if (NULL == tyT)
    {
        GlobalMm oMm;

        #define defstatic(mp_ctype, mp_cname) \
            mp_cname = new(&oMm) mp_ctype;

        #define defstatic2(mp_ctype, mp_cname, mp_a) \
            mp_cname = new(&oMm) mp_ctype(mp_a);

        TyClass::BeginStaticInit(&oMm);

        #include "./tinycl_compiler.inc"

        TyClass::EndStaticInit(&oMm);
    } // if

    // Run passes
    for (
        const PassDesc* p = k_rgoPassDesc;
        p < k_rgoPassDesc + lengthof(k_rgoPassDesc);
        p++ )
    {
        p->m_pfn();
        if (nil != m_errors)
        {
            return nil;
        }
    } // for

    Module::EnumFunction oEnum(Context::Get()->GetModule());
    return oEnum.Get()->m_fn;
} // Context::Run

// [F]

void FunctionPass::Run()
{
    foreach (Module::EnumFunction, oEnum, Context::Get()->GetModule())
    {
        processFunction(oEnum.Get());
        unless (Context::Get()->CanContinue()) break;
    } // for each function
} // FunctionPass::Run

// [L]

void* LocalObject::operator new(size_t cb)
{
    return Context::Get()->Alloc(cb);
} // LocalObject::new

void* LocalObject::operator new(size_t cb, IMm* p)
{
    return p->Alloc(cb);
} // LocalObject::new

// [F]

void ModulePass::Run()
{
    processModule(Context::Get()->GetModule());
} // ModulePass::Run

// [P]

Pass::Pass() :
    m_pOuter(Context::Get()->GetPass()),
    m_pszName("noname"),
    m_stream(nil)
{
    if (NULL != m_pOuter)
    {
        Context::Get()->m_iSubPass += 1;
        m_stream = m_pOuter->m_stream;
    }
    else
    {
        Context::Get()->m_iPass += 1;
        Context::Get()->m_iSubPass = 0;
    }

    m_nStartAt = ::GetTickCount();

    Context::Get()->SetPass(this);
} // Pass::Pass

Pass::~Pass()
{
    uint const nEndAt = ::GetTickCount();

    DEBUG_PRINTF("%d.%d.%d %s %dms\n",
        Context::Get()->m_iDepth,
        Context::Get()->m_iPass,
        Context::Get()->m_iSubPass,
        m_pszName,
        nEndAt - m_nStartAt );

    Val stream = m_stream;
    if (NULL == m_pOuter)
    {
        CLOG(1, "<hr />");
        CLOG(1, "Elapsed time: ~Dms~%", nEndAt - m_nStartAt);

        if (Context::Get()->CanContinue())
        {
            Module* const pM = Context::Get()->GetModule();

            CLOG(1, "<hr />");
            pM->Clean();

            if (! pM->Verify())
            {
                CLOG(1, "<div class='fatal'>IR Verification failed.</div>");

                Context::Get()->Error("IR Verification failed at ~A.",
                    make_string(m_pszName) );
            }
        }
    }
    else
    {
        if (m_stream == m_pOuter->m_stream)
        {
            stream = nil;
        }
    }

    if (nil != stream)
    {
        Context::Get()->GetModule()->HtmlPrint(stream, true);
        format(stream, "</body></html>~%");
        close(stream);
    }

    Context::Get()->SetPass(m_pOuter);
} // Pass::~Pass

/// <summary>
///  Prepares compilation pass.
/// </summary>
void Pass::Prepare()
{
    m_pszName = GetName();

    DEBUG_PRINTF("%d.%d.%d %s\n",
        Context::Get()->m_iDepth,
        Context::Get()->m_iPass,
        Context::Get()->m_iSubPass,
        m_pszName );

    if (Qcompile != cdr(assq(Qdebug, TLV(c_AoptionsA))))
    {
        return;
    }

    if (Context::Get()->m_iDepth > 0)
    {
        return;
    }

    char sz[100];
    ::wsprintfA(sz, "%02d_%02d_%02d_%s.html",
        Context::Get()->m_iDepth,
        Context::Get()->m_iPass,
        Context::Get()->m_iSubPass,
        GetName() );

    StackString_<> oFilename(sz);

    Val stream = open(
        oFilename,
        Kdirection, Koutput,
        Kif_exists, Ksupersede );

    if (nil == stream)
    {
        return;
    }

    m_stream = make_html_encode_stream(stream);

    char szTitle[100];
    ::wsprintfA(szTitle, "Pass %02d_%02d_%02d %s",
        Context::Get()->m_iDepth,
        Context::Get()->m_iPass,
        Context::Get()->m_iSubPass,
        GetName() );

    StackString_<> oTitle(szTitle);

    Val title = oTitle;

    format(m_stream,
        //"<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.0//EN'>"
        "<html><head>"
        "<title>~A</title>"
        "<style type='text/css'>"
        "  body"
        "  {"
        "      font-family: Verdana;"
        "      font-size: 9pt;"
        "  }"
        "  h1"
        "  {"
        "      font-size: 120%;"
        "      margin-bottom:  3pt;"
        "      margin-top:     3pt;"
        "  }"
        "  h2"
        "  {"
        "      font-size: 110%;"
        "      margin-bottom:  3pt;"
        "      margin-top:     10pt;"
        "  }"
        "  h3"
        "  {"
        "      font-size: 100%;"
        "      margin-bottom:  3pt;"
        "      margin-top:     5pt;"
        "  }"
        "  h4"
        "  {"
        "      font-size: 100%;"
        "      margin-bottom:  3pt;"
        "      margin-top:     3pt;"
        "  }"
        "  h5"
        "  {"
        "      font-size: 110%;"
        "      margin-bottom:  3pt;"
        "      margin-top:     3pt;"
        "  }"
        "  o"
        "  {"
        "      margin-bottom:  3pt;"
        "      margin-top:     3pt;"
        "  }"
        "  table"
        "  {"
        "      border-collapse: collapse;"
        "      font-size: 100%;"
        "  }"  // table
        "  a.b"
        "  {"
        "      color: #009900;"
        "  }"
        "  a.eb"
        "  {"
        "      font-weight: bold;"
        "      color: #990000;"
        "  }"
        "  a."
        "  {"
        "      color: #CC0000;"
        "      font-family: Consolas;"
        "  }" // a.l literal
        "  a.m"
        "  {"
        "      color: #0000CC;"
        "      font-size: 120%;"
        "      font-family: Consolas;"
        "      font-variant: small-caps;"
        "  }" // a.m mnemonic
        "  a.o"
        "  {"
        "      font-family: Consolas;"
        "  }" // a.o output
        "  b.g"
        "  {"
        "      color: #009900;"
        "  }"
        "  b.r"
        "  {"
        "      color: #990000;"
        "  }"
        "  div.e"
        "  {"
        "       background:     #fffff0;"
        "       border:         2px #990000 solid;"
        "       color:          #990000;"
        "       font-size:      12pt;"
        "       margin-bottom:  3pt;"
        "       margin-top:     3pt;"
        "       padding:        5pt;"
        "  }"
        "  div.fatal"
        "  {"
        "       background: #fffff0;"
        "       border:     5px #990000 solid;"
        "       color:      #990000;"
        "       font-size: 20pt;"
        "       padding:   5pt;"
        "  }"
        "  h3.f"
        "  {"
        "      border-top: solid #000099 2px;"
        "      color:      #990000;"
        "      font-size:  140%;"
        "  }"
        "  h4.b"
        "  {"
        "      border-top: solid 1px gray;"
        "      font-size: 100%;"
        "      margin-bottom:  5pt;"
        "      margin-top:     10pt;"
        "  }"
        "  i.t"
        "  {"
        "      color: #ffa500;"
        "  }"
        "  li.b"
        "  {"
        "       font-weight: bold;"
        "  }"
        "  li.e"    // error
        "  {"
        "      color: #990000;"
        "  }"
        "  li.n"    // notice
        "  {"
        "      color: #990000;"
        "  }"
        "</style></head><body>~%"
        "<h1>~A</h1>~%"
        "<ul>"
            "<li><a href='#fall'>Functions</a></li>"
            "<li><a href='#M.Verify'>Verification</a></li>"
        "</ul>"
        "<hr/>",
        title,
        title );
} // Pass::Prepare

} // Compiler

defun(compile_form, (Val form))
{
    Val fn, errors, warnings;
    {
        Compiler::Context oContext(form);
        fn = oContext.Run();
        errors = oContext.m_errors;
        warnings = oContext.m_warnings;
    }
    return values(fn, warnings, errors);
} // compile_form

} // TinyCl
