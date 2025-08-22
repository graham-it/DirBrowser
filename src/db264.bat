@echo off
echo Compiling Dir Browser for: 264 Series
echo.

acme --cpu 6502 -f cbm -l db264.sym -o db264 db264.def db.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
