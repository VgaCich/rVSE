@echo off
echo --Compiling level %1--
..\..\SynTex\STAsm\STAsm.exe %1.sta ..\Data\%1.stc
if "%ERRORLEVEL%" == "0" goto :EOF
echo !!Error while compiling level %1!!
set FailedLevels=%FailedLevels% %1;