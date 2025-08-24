# DirBrowser
...a lightweight and versatile program launcher for Commodore 8-bit machines!

![](/pics/db64_nos.png)

Version 1.08 (minor update)\
Released on August 14, 2025

Version 1.07 (first release)\
Released on August 14, 2025

Copyright (c) 2025 Francesco Gramignani "Graham"

https://graham-it.itch.io/dirbrowser \
https://github.com/graham-it/DirBrowser \
https://csdb.dk/release/?id=255620


## Description

Dir Browser is a lightweight and versatile program launcher for Commodore
8-bit machines that allows you to explore the contents of one or more
connected disk drives.\
Through this utility you can change the current directory, mount disk images
(or tape), and launch programs easily and intuitively.\
Designed to be used in conjunction with SD2IEC devices, it also supports
common Commodore disk drives (or compatible).


## Compatibility

Dir Browser is available for the following Commodore machines:
- C64			with "Fast Load" support
- VIC-20		with or without memory expansion
- C128			40 and 80 column mode support
- 264 Series    (C16, Plus/4, and other TED machines)

Tested disk drives:
- SD2IEC
- 1541
- 1541-II
- 1571
- 1581		(emulated on VICE)

Supported image files:
- .d64		1540, 1541, 1541-II and 1551 disk image (5.25")
- .d71		1571 double sided disk image (5.25")
- .d81		1581 disk image (3.5")
- .d41		(same as .d64)
- .m2i		earlier container for MMC2IEC
- .dnp		CMD hard disk native partition
- .tap		tape image (ITS module required)


## Features

- Automatically detects connected devices
- Cache memory to speed-up directory browsing
- Remembers cursor position on previously visited directories
- Full disk information (name, ID, and number of files)
- Displays file size (not for VIC-20)
- Fast file search (by first character)
- Optimized alphabetical sorting
- Dual port joystick support (where available)
- Fast scrolling
- On-screen active buttons
- File extension recognition to auto-mount disk and tape images


## Installation

After downloading the program archive from the provided URL, you need to
extract the files in the "exec" folder and copy them to the root folder
of the media where you want to use the program.\
If desired, you can copy only the executable files for
the machines you need:

|File name|Compatible machine|
|---------|------------------------------------------------|
|tdb64    |C64 (turbo installer, optional)|
|db64     |C64 (main program)|
|db20     |VIC-20 (without memory expansion)|
|db20+3k  |VIC-20 (with 3 KB memory expansion)|
|db20+8k  |VIC-20 (with 8 KB, or more, memory expansion)|
|db128    |C128 (executable from both 40 and 80 columns)|
|db264    |264 Series (C16, Plus/4, and other TED machines)|

The "tdb64" file is optional and (for now) is only available for the C64.
It is a self-executing file (it starts without RUN) that installs the
"Fast Load" routine and then loads the main "fb64" program with the "Turbo"
option enabled (by default), which can be disabled by the user using the
appropriate menu button.

Note:
With "Turbo" installed, you cannot reload Dir Browser via the "tdb64" file
(for example, at the end of a BASIC program). You must reload it directly
via the main "fb64" file.\
Another method is to press RUN/STOP+RESTORE to deactivate the "Fast Load"
routine, then relaunch Dir Browser from "tdb64".\
This is because the "Fast Load" routine is not compatible with self-executing
files, such as those found in various versions of GEOS and in some games, such
as GP Circuit, Hot Wheels, and Test Drive 2.


## Tips

To reduce the number of keys to press, to load Dir Browser after machine cold
start, I recommend you copying the version of the program appropriate for the
machine you use most often as the first file in the main folder (the order of
the folders is irrelevant).\
If you want to use the "Turbo" version for the C64, the first file should
be "tdb64".\
This will make it easier to launch Dir Browser by entering the following
command from BASIC: LOAD"*",8,1\
Furthermore, I recommend you organizing your programs in separate folders,
possibly by machine type.

Note:
The SD2IEC interface, while supporting long file names (since it is based on
FAT32 file system), connected to a Commodore machine, has the limitation of
showing file names limited to 16 characters (including any extensions of disk
images).\
Therefore, it is recommended to rename program files with names longer than
the standard length.\
To improve compatibility, especially with "multi-load" programs (split into
multiple files), it is recommended to remove the ".prg" extension from file
names, as it is implicit.


## Control keys

### Keys active on all machines:
- CRSR/JOY UP		Previous file
- CRSR/JOY DOWN		Next file
- CRSR/JOY LEFT		Previous page (or first line of page)
- CRSR/JOY RIGHT		Next page (or last line of page)
- HOME			Top of list
- CLR (SHIFT+HOME)	Bottom of list
- DEL (backspace)		Previous directory (or unmount a disk image)
- INST (SHIFT+DEL)	Root of disk
- "$" (dollar sign)	Reloads current directory
- RUN/STOP		Stops loading from disk (or sorting of files)
- RETURN or JOY FIRE	Start a program (LOAD"file name",8,1) open a directory, or mount a disk (or tape) image
- SHIFT+RETURN		Forces loading a program into the BASIC area (LOAD"file name",8)

### VIC-20 and C64:
- "<-" (left arrow)	Return to BASIC

### C128 and 264 Series:
- ESC			Return to BASIC
- HELP			Display program information


## Function keys

Except for VIC-20 version without memory expansion, active function keys
are visible at the bottom of the screen.

### VIC-20, C64, and C128:
- F1	DEV(nn)		Change device address (8..15)
- F2	CACHE		Cache memory for previous directories (on/off)
- F3(*)	RENEW(***)	Reload current directory
- F4(*)	JOY#(n)		Change joystick port (1/2)
- F5	SORT		Sort files in alphabetical order (on/off)
- F6(**)	QUIT		Return to BASIC
- F7	ABOUT		Display program information
- F8(**)	RESET		Reset the machine

(*) actions not available in the VIC-20 version

(**) To reduce program size, these are the only function keys active
on the VIC-20 version without memory expansion.

(***) C64 version with "fastload" module enabled:
- F5	TURBO		Fast Load (on/off)

### 264 Series:
- F1	DEV(nn)		Change device address (8..15)
- F2	RENEW		Reload current directory
- F3	SORT		Sort files in alphabetical order (on/off)
- F4	CACHE		Cache memory for previous directories (on/off)
- F5	JOY#(n)		Change joystick port (1/2)
- F6	QUIT		Returns to BASIC
- F7	RESET		Reset the machine


## Fast Load (C64 only)

The C64 version of Dir Browser can be used in conjunction with "Fast Load"
program by Thomas Tempelmann.
This software allows both Dir Browser and the programs launched from it to
load more quickly.
When Dir Browser starts, if "Fast Load" is detected in memory, the "Turbo"
button will be active, otherwise it will appear darkened.
Before launching a program, to ensure greater compatibility, "Fast Load"
can be disabled using the appropriate button.

"Fast Load" can be used in two ways:
1) by loading and running the original version of "Fast Load," which
   will be installed in memory, then manually loading and running
   Dir Browser;

2) by launching the self-executable program "tdb64", which must be located
   in the same folder as Dir Browser, with the command: LOAD"TDB64",8,1
   This will automatically install "Fast Load" and launch Dir Browser.

Note:
The Fast Load routine included in the "tdb64" file is a modified (by me)
version of Thomas Tempelmann's original program, which allows you to load
programs even from devices with addresses other than 8, provided
that any other drives are turned off or disconnected.
Furthermore, a "raster bar" effect will be visible during loading, instead
of the classic blank screen.


## Emulation on VICE

Files in the "vice" folder are compiled versions for debugging and should
not be used on a real machine.\
Specifically, the "root" command is compatible with the "VICE Virtual FS",
and the color of the frame changes during disk access to provide feedback
to the user.\
Of course, within VICE, it's not possible to mount disk (and tape) images
directly from the program.


## Credits

- nbla000
author of CBM FileBrowser v1.6 (c) 2010-2013, that simplified the use of
SD2IEC interface with Commodore machines.\
The need to provide the program with new features pushed me to analyze
the source code and create a completely new one, trying to maintain the
appearance and small size typical of the original program.

- Thomas Tempelmann
author of Fast Load v1.1 (c) 1984, a program for the C64 that allows
speeding up program loading from the 1541 disk drive, without any
hardware modifications.\
It is currently one of the few "fast loaders" supported by the SD2IEC
interface, and can be used in conjunction with Dir Browser, with the
option of disabling it in cases where there are compatibility issues
with specific software.

- Marco Baye
author of ACME 0.97 compiler, the ACME Crossassembler for Multiple
Environments (c) 1998-2020, thath allowed me to make the development
of this project faster and more efficient, working comfortably on a PC.


## Contributors

- You Dev IT (Mille e Una Avventura)
  www.youdev.it

- IGP Tech Blog
  www.isaac-garcia-peveri.com

- RetroFixer
  https://retrofixer.it


## Copyright

The program is released for public use according to legal practices.\
It may be downloaded from the URLs indicated and may not be redistributed
on other sites or installed on storage media for commercial purposes, unless
with the express consent of the author.\
The downloadable archive contains the executables, the source code, consisting
of multiple files, and this description file, available in Italian and English
language.\
Any clarifications, modifications, or proposed changes to the source code can
be submitted to the creator of the project through the provided communication
channels.

author:		Eng. Francesco Gramignani "Graham"\
email:		fra.gram@tin.it\
URLs:\
https://graham-it.itch.io/dirbrowser \
https://github.com/graham-it/DirBrowser \
https://csdb.dk/release/?id=255620

August 14, 2025 Palermo (Italy)
