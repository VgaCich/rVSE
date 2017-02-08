@echo off
call CompileTex
call CompileLevels
echo --Creating MemPak--
..\rVSE\Tools\MemPak\MemPak.exe MemPak.bin Data\*.*
brcc32 AssaultLite.rc
brcc32 MemPak.rc
Pause