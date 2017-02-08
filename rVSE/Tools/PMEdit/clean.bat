StripReloc /b /c *.exe
del *.~*
del *.dcu
del *.ddp
del *.exe.bak
del *.cfg
del log.txt
del eXgine\*.dcu
cd ..\PMBuild
call Clean
cd ..\PMEdit