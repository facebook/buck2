@echo off

set OUT=%1
shift
set dirname=%~dp1
if not exist %dirname% md %dirname%
if exist %OUT% del /f %OUT%
:loop
echo %1>>%OUT%
shift
if not "%~1"=="" goto loop
