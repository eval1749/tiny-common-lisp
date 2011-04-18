//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer Text
// editor/ed_buffer_text.h
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/editor/ed_buffer_text.h#4 $
//
#if !defined(INCLUDE_editor_text_h)
#define INCLUDE_editor_text_h

#include "./ed_defs.h"

namespace Editor
{

/// <remark>
///   Represents Text Buffer.
/// </remark>
class BufferText :
    public EditorObject_<BufferText, Layout_buffer>
{
    public: static Val ClassD_() { return CLASSD_buffer; }

    private: enum Constants
    {
        MinGapLength    = 1024,
        ExtensionLength = 1024,
    }; // Constants

    // ctor
    protected: BufferText();

    // [C]
    public: void CheckPosn(Posn) const;
    public: void CheckRange(Posn, Posn) const;

    // [D]
    protected: Count deleteChars(Posn, Posn);

    // [E]

    /// <remark>
    ///   Reverse character enumerator.
    /// </remark>
    public: class EnumChar
    {
        /// <remark>
        ///  Arguments for EnumChar.
        /// </remark>
        public: struct Arg
        {
            BufferText* m_pBuffer;
            Val         m_end;
            Val         m_start;

            Arg(
                BufferText* pBuffer,
                Posn        start,
                Posn        end ) :
                    m_pBuffer(pBuffer),
                    m_end(end),
                    m_start(start) {}

            Arg(
                BufferText* pBuffer,
                Int         iStart,
                Int         iEnd ) :
                    m_pBuffer(pBuffer),
                    m_end(Fixnum::Encode(iEnd)),
                    m_start(Fixnum::Encode(iStart)) {}
        }; // Arg

        private: BufferText*    m_pBuffer;
        private: Posn           m_posn;
        private: Posn           m_end;

        /// <summary>
        ///   Construct EnumChar from Arg.
        /// </summary>
        public: EnumChar(Arg oArg) :
            m_pBuffer(oArg.m_pBuffer),
            m_end(oArg.m_end),
            m_posn(oArg.m_start) {}

        /// <summary>
        ///   Checeks enumerator at end.
        /// </summary>
        /// <returns>True if enumerator at end.</returns>
        public: bool AtEnd() const
            { return xge(m_posn, m_end); }

        /// <summary>
        ///   Returns character of current position.
        /// </summary>
        /// <returns>C-chracter</returns>
        public: char16 Get() const
        {
            ASSERT(! AtEnd());
            return m_pBuffer->GetCharAt(m_posn);
        } // Get

        /// <summary>
        ///   Returns current position.
        /// </summary>
        /// <returns>Posn</returns>
        public: Posn GetPosn() const
        {
            return m_posn;
        } // GetPosn

        /// <summary>
        ///   Advance enumerator.
        /// </summary>
        public: void Next()
        {
            ASSERT(! AtEnd());
            m_posn = xxadd(m_posn, one);
        } // Next
    }; // EnumChar

    /// <remark>
    ///   Reverse character enumerator.
    /// </remark>
    public: class EnumCharRev
    {
        /// <remark>
        ///  Arguments for EnumCharRev.
        /// </remark>
        public: struct Arg
        {
            BufferText* m_pBuffer;
            Val         m_end;
            Val         m_start;

            Arg(
                BufferText* pBuffer,
                Posn        start,
                Posn        end ) :
                    m_pBuffer(pBuffer),
                    m_end(end),
                    m_start(start) {}

            Arg(
                BufferText* pBuffer,
                Int         iStart,
                Int         iEnd ) :
                    m_pBuffer(pBuffer),
                    m_end(Fixnum::Encode(iEnd)),
                    m_start(Fixnum::Encode(iStart)) {}
        }; // Arg

        private: BufferText*    m_pBuffer;
        private: Posn           m_posn;
        private: Posn           m_start;

        /// <summary>
        ///   Construct EnumCharRev from Arg.
        /// </summary>
        public: EnumCharRev(Arg& rArg) :
            m_pBuffer(rArg.m_pBuffer),
            m_posn(rArg.m_end),
            m_start(rArg.m_start)
        {
            ASSERT(le(rArg.m_start, rArg.m_end));
        } // EnumCharRev

        /// <summary>
        ///   Checeks enumerator at end.
        /// </summary>
        /// <returns>True if enumerator at end.</returns>
        public: bool AtEnd() const
            { return xlt(m_posn, m_start); }

        /// <summary>
        ///   Returns character of current position.
        /// </summary>
        /// <returns>Chracter</returns>
        public: char16 Get() const
        {
            ASSERT(! AtEnd());
            return m_pBuffer->GetCharAt(xxsub(m_posn, one));
        } // Get

        /// <summary>
        ///   Returns current position.
        /// </summary>
        /// <returns>Posn</returns>
        public: Posn GetPosn() const
        {
            return m_posn;
        } // GetPosn

        /// <summary>
        ///   Advance enumerator.
        /// </summary>
        public: void Next()
        {
            ASSERT(! AtEnd());
            m_posn = xxsub(m_posn, one);
        } // Next
    }; // EnumCharRev

    protected: void extend(Posn, Count);

    // [G]
    public: char16 GetCharAt(Posn) const;
    public: uint   GetText(Posn, char16*, uint) const;
    public: uint   GetText(Posn, Posn, char16*) const;

    // [I]
    protected: void insert(Posn, char16, size_t);
    protected: void insert(Posn, const char16*, size_t);
    public: bool    IsValidPosn(Posn) const;

    // [M]
    private: void moveGap(Posn);
}; // BufferText

CASSERT(sizeof(BufferText) == sizeof(Layout_buffer));

} // Editor

#endif //!defined(INCLUDE_editor_text_h)
