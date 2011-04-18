<?xml version="1.0"?><doc>
<members>
<member name="T:Regex.IEnvironment" decl="false" source="d:\proj\evedit2\regex\iregex.h" line="93">
<remark>
 Interface provides chracter tests and overridable implementation.
 <para>
    Implementation of methods use Windows API for character case
    related tests.
 </para>
</remark>
</member>
<member name="T:Regex.ICompileContext" decl="false" source="d:\proj\evedit2\regex\iregex.h" line="125">
<remark>
 Interface of regex compilation context.
</remark>
</member>
<member name="T:Regex.IMatchContext" decl="false" source="d:\proj\evedit2\regex\iregex.h" line="135">
<remark>
 Interface of regex match context.
</remark>
</member>
<member name="T:Regex.RegexPrivate.Scanner" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="20">
<remark>
  Scanner base class
</remark>
</member>
<member name="T:Regex.RegexPrivate.CharScanner" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="58">
<remark>
 Character scanner
</remark>
</member>
<member name="M:Regex.RegexPrivate.CharScanner.#ctor" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="69">
<summary>
  For CharScanner_ template
</summary>
</member>
<member name="M:Regex.RegexPrivate.CharScanner.computeMethod(System.Boolean,System.Boolean)" decl="false" source="d:\proj\evedit2\regex\regex_scanner.h" line="75">
<summary>
 Compute scanner method code.
</summary>
<param name="fBackwrad">Ture if backward scanner</param>
<param name="fIgnoreCase">Ture if case-insensitive scanner</param>
</member>
<member name="T:Common.EnumChar" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="318">
<remark>
  C-String enumerator.
</remark>
</member>
<member name="T:Common.EnumChar.Arg" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="323">
<summary>
  Argument for EnumChar
</summary>
</member>
<member name="M:Common.EnumChar.#ctor(Common.EnumChar.Arg)" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="341">
<summary>
  Construct C-String enumerator.
</summary>
</member>
<member name="M:Common.EnumChar.AtEnd" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="348">
<summary>
 Check enumereator at end.
</summary>
</member>
<member name="M:Common.EnumChar.Get" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="356">
<summary>
 Returns current character
</summary>
</member>
<member name="M:Common.EnumChar.Next" decl="false" source="d:\proj\evedit2\tinycl\z_util.h" line="365">
<summary>
 Advance current position
</summary>
</member>
<member name="T:Regex.RegexPrivate.Control" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="50">
<remark>
  Control opcode for control stack.
</remark>
</member>
<member name="T:Regex.RegexPrivate.Capture" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="66">
<remark>
 Represents a capture
</remark>
</member>
<member name="T:Regex.RegexPrivate.Engine" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="86">
<remark>
 Represents regex byte code interpreter.
</remark>
</member>
<member name="F:Regex.RegexPrivate.Engine.m_lLoopLimitPosn" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="99">
<summary>
  Limit source position. Used for detecting super-linear situation.
</summary>
</member>
<member name="M:Regex.RegexPrivate.Engine.isAsciiWordBoundary" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="298">
<summary>Predicate for ASCII word boundary.
  <list>
    <item>
        <term>+|foo</term>
        <description>
            Character before position isn't word character.
        </description>
    </item>
    <item>
        <term>foo|+</term>
        <description>
            Character at position isn't word character.
        </description>
    </item>
  </list>
</summary>
<returns>True if current position is ASCII word boundary</returns>
</member>
<member name="M:Regex.RegexPrivate.Engine.isEndOfLine" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="364">
<summary>
  Normal dollar($) or "\Z" match
  <list>
    <item><description>
        end of string</description></item>
    <item><description>
      before string-ending newline</description></item>
  </list>
</summary>
</member>
<member name="M:Regex.RegexPrivate.Engine.isUnicodeWordBoundary" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="396">
<summary>Predicate for Unicode word boundary.
  <list>
    <item>
        <term>+|foo</term>
        <description>
            Character before position isn't word character.
        </description>
    </item>
    <item>
        <term>foo|+</term>
        <description>
            Character at position isn't word character.
        </description>
    </item>
  </list>
</summary>
<returns>True if current position is Unicode word boundary</returns>
</member>
<member name="M:Regex.RegexPrivate.Engine.dispatch" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="845">
<summary>
  Opcode dispatcher
</summary>
<returns>
  True if engine executes Op_Success, false otherwise.
</returns>
</member>
<member name="M:Regex.RegexPrivate.Engine.execute(System.Int32!System.Runtime.CompilerServices.IsLong,System.Int32!System.Runtime.CompilerServices.IsLong)" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="2208">
<summary>
  Executes regex byte code from specified start position and sets
  captures.
</summary>
<param name="lStart">
  Position of source text where regex byte code execute.
</param>
<param name="lMatchStart">
  Position of source text where match started.
</param>
<returns>
  True is executes SUCCESS instruction, false otherwise.
</returns>
</member>
<member name="M:Regex.RegexPrivate.Engine.execute1" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="2250">
<summary>
  Executes regex byte code.
</summary>
<returns>
  True is executes SUCCESS instruction, false otherwise.
</returns>
</member>
<member name="M:Regex.RegexPrivate.RegexObj.NextMatch(Regex.IMatchContext*)" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="2385">
<summary>
  Find next match.
</summary>
<param name="pIContext">Find next match on this context</param>
</member>
<member name="M:Regex.RegexPrivate.RegexObj.StartMatch(Regex.IMatchContext*)" decl="false" source="d:\proj\evedit2\regex\regex_exec.cpp" line="2409">
<summary>
  Find the first matchmatch.
</summary>
<param name="pIContext">Find the first match on this context</param>
</member>
</members>
</doc>