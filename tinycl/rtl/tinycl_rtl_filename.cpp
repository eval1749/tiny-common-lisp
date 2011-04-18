#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// TinyCl - Runtime - Filename
// tinycl_rtl_filename.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/rtl/tinycl_rtl_filename.cpp#2 $
//
#include "../tinycl.h"

namespace TinyCl
{

using namespace Private;

class LogicalPathname :
    public Record_<LogicalPathname, Layout_logical_pathname>
{
    public: static Val ClassD_() { return CLASSD_logical_pathname; }
}; // LogicalPathname

// [A]
static Val adjust_case(Val host, Val thing, Val kase)
{
    class Internal
    {
        public: static Val Adjust(Val host_case, Val thing)
        {
            if (stringp(thing))
            {
                return adjust(host_case, thing);
            } // if string

            if (consp(thing))
            {
                Collector oCollector;
                foreach (List::Enum, oEnum, thing)
                {
                    Val thing = oEnum.Get();
                    oCollector.Add(Adjust(host_case, thing));
                }
                return oCollector.Get();
            } // if cons

            return thing;
        } // Adjust

        private: static Val adjust(Val host_case, Val str)
        {
            Val kase = analyze(str);

            if (Kdowncase == kase)
            {
                if (Kdowncase == host_case)
                {
                    return string_upcase(str);
                }

                return str;
            } // if downcase

            if (Kupcase == kase)
            {
                if (Kdowncase == host_case)
                {
                    return string_upcase(str);
                }

                return str;
            } // if upcase

            return str;
        } // Adjust

        private: static Val analyze(Val str)
        {
            bool fLower = true;
            bool fUpper = false;
            
            StringData oString(str);
            foreach (StringData::Enum, oEnum, &oString)
            {
                Val ch = Character::FromCode(oEnum.Get());
                Character* p = ch->StaticCast<Character>();
                if (p->IsLowerCase())
                {
                    if (fUpper) return Kmixed;
                    fLower = true;
                }
                else if (p->IsUpperCase())
                {
                    if (fLower) return Kmixed;
                    fUpper = true;
                }
            } // for each char

            if (fLower && fUpper)
            {
                return Kmixed;
            }

            if (fLower)
            {
                return Kdowncase;
            }

            if (fUpper)
            {
                return Kupcase;
            }

            return Kmixed;
        } // analyze
    }; // Internal

    if (Klocal == kase)
    {
        return thing;
    }

    if (Kcommon == kase)
    {
        BasicHost* p = host->StaticCast<BasicHost>();
        return Internal::Adjust(p->m_customary_case, thing);
    } // if common

    SignalTypeError(kase, list(Qmember, Kcommon, Klocal));
} // adjust_case

// [F]
Val find_pathname_host(Val name, Val errorp)
{
    check_type(name, string);
    foreach (List::Enum, oEnum, VAR(Apathname_hostsA))
    {
        Val host = oEnum.Get();

        Layout_basic_host* p =
            host->StaticCast<Record>()->To<Layout_basic_host>();

        if (string_eq(p->m_name, name))
        {
            return host;
        }
    } // for each host

    if (nil != errorp)
    {
        error(Qpathname_host_not_found, Kname, name);
    }
    return nil;
} // find_pathname_host

// [P]
static void NoReturn pathname_not_matched(Val wild, Val path)
{
    error(Qpathname_not_matched,
        Kpathname,  path,
        Kwildcard,  wild );
} // pathname_not_matched

namespace CommonLisp
{

// [D]
defun(directory_namestring, (Val thing))
{
    Val x = pathname(thing);
    Val s = make_string_output_stream();
    funcall(Qunparse_pathname, x, Qdirectory_namestring, s);
    return get_output_stream_string(s);
} // directory_namestring

// [E]
defun(enough_namestring, (Val thing, Val defaults))
{
    return merge_pathnames(parse_namestring(thing, nil, defaults), defaults);
} // enough_namestring

// [F]
defun(file_namestring, (Val thing))
{
    Val x = pathname(thing);
    Val s = make_string_output_stream();
    funcall(Qunparse_pathname, x, Qfile_namestring, s);
    return get_output_stream_string(s);
} // file_namestring

// [H]
defun(host_namestring, (Val thing))
{
    Val x = pathname(thing);
    Val host = x->StaticCast<Pathname>()->m_host;
    return host->StaticCast<BasicHost>()->m_name;
} // host_namestring

// [M]
defun(make_pathnameV, (Thread* pth))
{
    Val kase      = Klocal;
    Val defaults  = TLV(Adefault_pathname_defaultsA);
    Val device    = MARKER_unbound;
    Val directory = MARKER_unbound;
    Val host      = nil;
    Val name      = MARKER_unbound;
    Val type      = MARKER_unbound;
    Val version   = MARKER_unbound;

    KeyArg rgoKey[] =
    {
        KEYARG2(case, kase),
        KEYARG(defaults),
        KEYARG(device),
        KEYARG(directory),
        KEYARG(host),
        KEYARG(name),
        KEYARG(type),
        KEYARG(version),
    }; // rgoKey

    parseKeys(pth, 0, rgoKey, lengthof(rgoKey));

    defaults = pathname(defaults);

    Pathname* pDefaults = defaults->StaticCast<Pathname>();

    if (nil == host)
    {
        host = pDefaults->m_host;
    }
    else
    {
        // FIXME 2008-06-29 yosi@msn.com make-pathname with host parameter
        error("NYI: find-pathname-host");
    }

    // FIXME 2008-06-29 yosi@msn.com make-pathname case

    if (MARKER_unbound == device)
    {
        if (NULL != pDefaults)
        {
            device = pDefaults->m_device;
        }
    }

    if (MARKER_unbound == directory)
    {
        if (NULL != pDefaults)
        {
            directory = pDefaults->m_directory;
        }
    }

    if (MARKER_unbound == name)
    {
        if (NULL != pDefaults)
        {
            name = pDefaults->m_name;
        }
    }

    if (MARKER_unbound == type)
    {
        if (NULL != pDefaults)
        {
            type = pDefaults->m_type;
        }
    }

    if (MARKER_unbound == version)
    {
        if (NULL != pDefaults)
        {
            version = pDefaults->m_version;
        }
    }

    return funcall(Qmake_pathname_using_host,
        host,
        device,
        directory,
        name,
        type,
        version );
} // make_pathname

defmethod(make_pathname_using_host, logical_pathname,
    (Val host, Val device, Val directory, Val name, Val type, Val version) )
{
    Val path = Thread::Get()->AllocRecord(CLASS_logical_pathname);

    LogicalPathname* p = path->StaticCast<LogicalPathname>();
    p->m_host = host;
    p->m_device = device;
    p->m_directory = directory;
    p->m_name = name;
    p->m_type = type;
    p->m_version = version;
    return path;
} // make_pathname_using_host

defun(merge_pathnames, (Val thing, Val defaults, Val default_version))
{
    Val templ = pathname(defaults);

    Val path;
    {
        BindFrameScope_<1> oLet;
        oLet.Bind(TLV_Adefault_pathname_defaultsA, templ);
        path = pathname(thing);
    }

    Pathname* p = path->StaticCast<Pathname>();
    Pathname* q = templ->StaticCast<Pathname>();

    if (classd_of(p->m_host) != classd_of(q->m_host))
    {
        pathname_not_matched(templ, path);
    }

    Val device = p->m_device;
    if (nil == device)
    {
        device = q->m_device;
        if (nil == device)
        {
            device = p->m_host->StaticCast<BasicHost>()->m_default_device;
        }
    } // if

    Val directory = p->m_directory;
    if (nil == q->m_directory)
    {
        directory = p->m_directory;
    }
    else if (nil == p->m_directory)
    {
        directory = q->m_directory;
    }
    else if (consp(directory) && Krelative == car(directory))
    {
        Val rev = nconc(reverse(cdr(directory)), reverse(cdr(q->m_directory)));
        Val runner = rev;
        Val prev = nil;
        while (! endp(runner))
        {
            if (car(runner) == Kback)
            {
                runner = cddr(runner);
                if (nil == prev)
                {
                    rev = runner;
                }
                else
                {
                    setf_cdr(runner, prev);
                }
            }
            else
            {
                prev = runner;
                runner = cdr(runner);
            }
        } // for each elt

        directory = cons(Kabsolute, nreverse(rev));
    } // if

    Val name    = p->m_name;
    Val version = p->m_version;
    if (nil != name)
    {
        if (nil == version) version = default_version;
    }
    else
    {
        name = q->m_name;
        if (nil == version) version = q->m_version;
    }

    Val type = p->m_type;
    if (nil == type) type = q->m_type;

    return funcall(Qmake_pathname_using_host,
        p->m_host,
        device,
        directory,
        name,
        type,
        version );
} // merge_pathnames

// [N]
defun(namestring, (Val thing))
{
    Val x = pathname(thing);
    Val s = make_string_output_stream();
    funcall(Qunparse_pathname, x, Qnamestring, s);
    return get_output_stream_string(s);
} // namestring

// [P]
defun(parse_namestringV, (Thread* pth))
{
    Val thing = pth->mv_value[0];
    Val host  = pth->mv_value[1];
    Val templ = pth->mv_value[2];

    Val end          = nil;
    Val junk_allowed = nil;
    Val start        = zero;

    KeyArg rgoKey[] =
    {
        KEYARG(end),
        KEYARG(junk_allowed),
        KEYARG(start),
    }; // rgoKey

    switch (Fixnum::Decode_(pth->m_n))
    {
    case 1:
        host = nil;
        // FALLTHORUGH

    case 2:
        templ = TLV(Adefault_pathname_defaultsA);
        break;

    default:
        templ = pathname(templ);
        parseKeys(pth, 3, rgoKey, lengthof(rgoKey));
        break;
    } // switch n

    if (host->Is<BasicHost>())
    {
        // ok
    }
    else if (stringp(host))
    {
        host = find_pathname_host(host);
    }
    else if (nil == host)
    {
        if (stringp(thing))
        {
            Val colon = position(
                Character::FromCode(':'),
                thing,
                Kstart, start,
                Kend,   end );
            if (nil != colon)
            {
                host = find_pathname_host(subseq(thing, start, colon), nil);
                if (nil != host)
                {
                    start = xxadd(colon, one);
                }
            }
        } // if thing is string

        if (nil == host)
        {
            host = templ->StaticCast<Pathname>()->m_host;
        }
    } // if

    if (streamp(thing))
    {
        thing = stream_pathname(thing);
    }

    if (pathnamep(thing))
    {
        Val present = thing->StaticCast<Pathname>()->m_host;
        if (present != host)
        {
            error("~S msut be ~S.", present, host);
        }
        return values(thing, start);
    } // if thing is pathname

    if (stringp(thing))
    {
        Val string = thing;
        end = ensure_bounding_indexes(string, start, end);
        Val pn = funcall(Qparse_namestring_using_host,
            host, string, start, end );
        Val index = pth->mv_value[1];
        if (nil == junk_allowed)
        {
            if (nil == pn || index != end)
            {
                error(Qpathname_parse_error,
                    Khost,      host,
                    Kposition,  index,
                    Kstring,    string );
            }
        } // if not junk_allowed

        return values(pn, index);
    } // if thing is string

    SignalTypeError(thing, Qpathname_designator);
} // parse_namestringV

defun(pathname, (Val x))
{
    if (stringp(x))
    {
        return parse_namestring(x);
    }

    if (pathnamep(x))
    {
        return x;
    }

    if (streamp(x))
    {
        return stream_pathname(x);
    }

    SignalTypeError(x, Qpathname_designator);
} // pathname

defpred(pathnamep, (Val x))
{
    return subclassp(class_of(x), CLASS_pathname);
} // pathnamep

defun(pathname_deviceV, (Thread* pth))
{
    Val thing = pth->mv_value[0];

    Val kase = Klocal;
    KeyArg rgoKey[] =
    {
        { &kase, Kcase },
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    Pathname* p = pathname(thing)->StaticCast<Pathname>();
    return adjust_case(p->m_host, p->m_device, kase);
} // pathname_device

defun(pathname_directoryV, (Thread* pth))
{
    Val thing = pth->mv_value[0];

    Val kase = Klocal;
    KeyArg rgoKey[] =
    {
        { &kase, Kcase },
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    Pathname* p = pathname(thing)->StaticCast<Pathname>();
    return adjust_case(p->m_host, p->m_directory, kase);
} // pathname_directory

defun(pathname_hostV, (Thread* pth))
{
    Val thing = pth->mv_value[0];

    Val kase = Klocal;
    KeyArg rgoKey[] =
    {
        { &kase, Kcase },
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    Pathname* p = pathname(thing)->StaticCast<Pathname>();
    Val host = p->m_host;
    return adjust_case(host, host->StaticCast<BasicHost>()->m_name, kase);
} // pathname_host

defun(pathname_nameV, (Thread* pth))
{
    Val thing = pth->mv_value[0];

    Val kase = Klocal;
    KeyArg rgoKey[] =
    {
        { &kase, Kcase },
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    Pathname* p = pathname(thing)->StaticCast<Pathname>();
    return adjust_case(p->m_host, p->m_name, kase);
} // pathname_name

defun(pathname_typeV, (Thread* pth))
{
    Val thing = pth->mv_value[0];

    Val kase = Klocal;
    KeyArg rgoKey[] =
    {
        { &kase, Kcase },
    }; // rgoKey

    parseKeys(pth, 1, rgoKey, lengthof(rgoKey));

    Pathname* p = pathname(thing)->StaticCast<Pathname>();
    return adjust_case(p->m_host, p->m_type, kase);
} // pathname_type

defun(pathname_version, (Val x))
{
    return pathname(x)->StaticCast<Pathname>()->m_version;
} // pathname_version

// [T]
defmethod(truename, t, (Val thing))
    { return truename(pathname(thing)); }

} // CommonLisp

} // TinyCl
