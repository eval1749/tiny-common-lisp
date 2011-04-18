@if not defined _echo echo off
:////////////////////////////////////////////////////////////////////////
://
:// Build Script For Evita Common Lisp
:// platform/win/build/build.cmd
://
:// @(#)$Id: //proj/evedit2/mainline/build.cmd#2 $
://
call setenv.cmd

if not defined SOLUTION goto error_SOLUTION

setlocal

rem nmake -f messages.mk

goto do_%1

:do_
:do_all
    call :do_set
    echo Build %BUILD% > #build.log
    time/t >> #build.log

    rem if exist %SOLUTION%.suo del/a:h %SOLUTION%.suo

    for %%p in (%platforms%) do (
        for %%c in (%configs%) do (
            call :build %%c %%p
        )
    )

    rem call drop.cmd

    time/t >> #build.log

    type #build.log

    goto end

:do_win32
:do_x64
    if "%2"=="" (
        for %%c in (%configs%) do (
            call :build %%c %1
        )
        exit/b 0
    )
    call :build %2 %1
    exit/b 0

:do_debug
:do_release
    if "%2"=="" (
        for %%p in (%platform%) do (
            call :platform %%c %%p
        )
    )
    call :build %2 %1
    exit/b 0

:do_clean
    :// .debug = evcl
    :// .fas = clisp
    :// .fsl = lisp works
    :// .lib = clisp
    :// del/s/q _*
    if exist setbuild.exe del/q setbuild.exe
    del/s/q #*
    del/s/f/q *.plg *.ncb *.aps *.opt *.sup *.log *.debug *.fas *.lib *.fsl
    del/s/f/q *.user
    del/s/f/q _boot.image _img_object.h

    :// evcl and acl
    del/s/f/q *.fasl

    :// clisp
    del/s/f/q *.fas *.lib

    if exist %SOLUTION%.suo del/a:h %SOLUTION%.suo

    for %%x in (debug release) do (
        if exist %%x (
            del/s/q %%x
            rd/s/q %%x
        )
    )
    goto end

:do_set
    if not exist setbuild.exe (
        echo Compiling setbuild.cs
        csc /nologo setbuild.cs
    )
    .\setbuild.exe
    for /f "tokens=1-4 delims=." %%i in (build.txt) do (
        if "%%l"=="" (
            set BUILD=%%k
            goto end
        )
        set BUILD=%%k.%%l
        goto end
    )
    goto end

://////////////////////////////////////////////////////////////////////
://
:// Subroutines
://

:build
    echo Build %1^|%2...
    rem if exist %1\%2 del/s/q %1\%2 > nul
    rem devenv.exe /out %1\%2\build.log /build "%1|%2" %SOLUTION%.sln
    rem vcbuild.exe /nologo /logfile:%1\%2\build.log %SOLUTION%.sln "%1|%2"
    msbuild.ext /p:Configuration=%1 /p:Platform=%2
    if errorlevel 1 goto error
    exit/b 0


:dispatch
    if "%2"=="" (
        call :build %1 win32
        call :build %1 x64
        exit/b 0
    )
    call :build %1 %2
    exit/b 0

://////////////////////////////////////////////////////////////////////
://
:// Error Handlers
://

:error_SOLUTION
    echo Please set environment variable SOLUTION, e.g. mysolution
    goto end

:error_BUILD
    echo Please set environment variable BUILD, e.g. 2115.
    goto end

:error
    echo **********************************************************************
    echo *
    echo * ERROR !
    echo *
    echo **********************************************************************
    goto end

:end
