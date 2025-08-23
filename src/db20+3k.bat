@echo off
echo Compiling Dir Browser for: Commodore VIC-20 (+3k expansion)
echo.

acme --cpu 6502 -f cbm -l db20+3k.sym -o db20+3k db20+3k.def db.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
