@if "%_echo%"=="" echo off
:// Create genesis loadup file on $(OutDir)
:// setup_genesis.cmd
:// @(#)$Id$
setlocal
set outdir=%1
if "%outdir%"=="" goto missing_outdir
cd %outdir%
set gendir=lib\genesis
if not exists %gendir% mkdir %gendir%
