@echo off
echo Compiling Dir Browser for: Commodore 128
echo.

acme --cpu 6502 -f cbm -l db128.sym -o db128 db128.def db.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
