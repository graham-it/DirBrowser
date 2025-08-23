@echo off
echo Compiling Dir Browser for: Commodore VIC-20 (unexpanded)
echo.

acme --cpu 6502 -f cbm -l db20.sym -o db20 db20.def db.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
