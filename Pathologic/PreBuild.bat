@echo off
setlocal ENABLEEXTENSIONS
setlocal ENABLEDELAYEDEXPANSION
cd Data
set files="Data\autoexec.cfg" "Data\Preload.txt" "Data\Creds.txt" "Data\Help.txt" "Data\MenuBg.jpg"
for /d %%i in (*) do set files=!files! "Data\%%i\*.*|%%i\"
cd ..
..\rVSE\Tools\MemPak\MemPak.exe MemPak.bin !files!
brcc32 MemPak.rc
del MemPak.bin
pause