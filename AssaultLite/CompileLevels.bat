@echo off
echo --Compiling levels--
cd Levels
set FailedLevels=
for %%i in (*.sta) do call CompileLevelData %%~ni
cd ..
if "%FailedLevels%" == "" goto LevelsDataOK
echo !!Failed to compile levels:%FailedLevels%!!
pause
:LevelsDataOK