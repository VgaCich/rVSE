@echo off
echo --Compiling textures--
call ClearCache
..\SynTex\STAsm\STAsm.exe Textures.sta Data\Textures.stc
if NOT "%ERRORLEVEL%" == "0" pause