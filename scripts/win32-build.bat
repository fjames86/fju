
REM "C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\Common7\IDE\devenv.exe" "win32\win32.sln" -Build "Debug|x64" -Out out.txt
REM type out.txt
REM del out.txt 
"C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\Common7\IDE\devenv.exe" "win32\win32.sln" -Build "Release|x64" -Out out.txt
mkdir bin\
copy win32\x64\Release\*.exe bin\
copy win32\x64\Release\libfju.dll bin\

bin\fvmc.exe -I fvm\stdlib fvm\test\test.pas 
bin\fvmc.exe -o bin\nls.fvm -I fvm\stdlib nls\nls.pas
bin\fvmc.exe -o bin\log.fvm -I fvm\stdlib fvm\modules\log.pas
bin\fvmc.exe -o bin\cht.fvm -I fvm\stdlib fvm\modules\cht.pas

type out.txt 
del out.txt
