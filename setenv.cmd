@if not defined _echo echo off
:////////////////////////////////////////////////////////////////////////
://
:// Build Script For Evita Common Lisp
:// build.cmd
://
:// @(#)$Id: //proj/evedit2/mainline/setenv.cmd#1 $
://
@if ""=="%_echo%" echo off
set DSTDIR=d:\bin
set SRCDIR=d:\proj\evedit2
set SOLUTION=evedit2
set Platforms=win32
set Configs=debug release
