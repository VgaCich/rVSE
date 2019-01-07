@echo off
call CompileTex
echo --Creating MemPak--
..\rVSE\Tools\MemPak\MemPak.exe MemPak.pas Data\*.*
Pause