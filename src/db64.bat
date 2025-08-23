@echo off
echo Compiling Dir Browser for: Commodore 64
echo.

acme --cpu 6502 -f cbm -l db64.sym -o db64 db64.def db.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
