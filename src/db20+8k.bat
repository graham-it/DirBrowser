@echo off
echo Compiling Dir Browser for: Commodore VIC-20 (+8k expansion)
echo.

acme --cpu 6502 -f cbm -l db20+8k.sym -o db20+8k db20+8k.def db.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
