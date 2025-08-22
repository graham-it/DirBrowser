;Dir Browser
;...a lightweight and versatile program launcher for Commodore machines

;version 1.07
;released on 14/8/2025

;Copyright (c) 2025 Francesco Gramignani "Graham"
;https://graham-it.itch.io/dirbrowser

;compilable with ACME 0.97
;the ACME Crossassembler for Multiple Environments
;Copyright (c) 1998-2020 Marco Baye

program_name	= "dir browser"
version		= "1.07"
program_date	= "2025"
author		= "graham"
basic_line	= 2025

!ct pet			;PETSCII text conversion table
!src "cbm_map.def"	;Commodore machines memory map
!src "db_cfg.def"	;common settings
!src "db_mem.def"	;memory allocation

;Debug mode
about_debug	= 0	;never display program info at start-up
color_debug	= 0	;change colors (foreground, background and border)
dir_debug	= 0	;border color changes when loading a new directory
img_debug	= 0	;border color changes when a disk image is mounted
turbo_debug	= 0	;detect fast load routine even if not installed
vice_debug	= 0	;root command syntax suitable for VICE Virtual FS

; --------------------
; - BASIC launcher
; --------------------

;Start of program
* = BASTXT			

prg_start
	!by <next_line,>next_line	;link address
	!by <basic_line,>basic_line	;line number

!if target = 128 {
	!by GRAPHIC_tok,CLR_tok,':'	;GRAPHIC CLR
	!tx CMD_prefix,BANK_tok,"15:"	;BANK 15
}

	!by SYS_tok			;SYS command

;M/L code entry point (decimal)
	!by '0' + ml_start DIV 1000
	!by '0' + ml_start MOD 1000 DIV 100
	!by '0' + ml_start MOD 100  DIV 10
	!by '0' + ml_start MOD 10

	!tx SUB_tok,author		;comment
	!by 0				;end of line

next_line
	!by 0,0				;end of BASIC program

; --------------------
; - Main
; --------------------

;M/L code entry point
ml_start

;Init program and system setup
	lda #0
	sta MSGFLG		;disable Kernal messages (in case of program launched with SYS)
	sta NDX			;clear keyboard buffer index

!if target = 128 | target = 264 {
	sta KYNDX		;clear function keys buffer index
}

	sta insideimage		;clear disk/tape image flag (bits 7:6)

!if joyenable {
	sta lastjoy		;clear last joystick input
}

!if target = 128 {
	tax			;set data (a = 0) and file name (x = 0) BANK for I/O operations
	jsr SETBNK

	lda #<PFKOFF		;disable function keys (as for VIC-20/C64)
	sta KEYCHK		;(then restored by CINT)

	bit MODE		;check active display
	bpl +
	jsr FAST		;set Fast Mode, if program launched from 80-columns

+	jsr SetMem		;set custom memory config (BASIC ROMs not available)
}

!if target = 264 {
	jsr SetFnKeys		;set standard function key definitions (as for VIC-20/C64)
}

;Set defaults
!if aboutenable {
	lda #(def_aboutmode != 0 & about_debug = 0) << 7
	sta aboutmode		;set program info at start-up (bit 7)
}

!if cachemem {
	lda #(def_cachemode != 0) << 7
	sta cachemode		;set cache mode (bit 7)
}

!if sortenable {
	lda #(def_sortmode != 0) << 7
	sta sortmode		;set sort mode (bit 7)
}

!if joyenable {
!if target != 20 {
!if target = 264 {
	lda #def_joyport-1	;(0 = port #1, 1 = port #2)

} else {
	lda #2-def_joyport	;(0 = port #2, 1 = port #1)
}

	sta joyport		;set joystick port
}
}

!if target = 64 {
!if fastload {
;Detect fast load routine and set default mode
	lda #(def_fastload != 0) << 7	;detected (bit 6 clear) and default mode (bit 7)
	ldy ILOAD+1			;current LOAD vector
	cpy #>NLOAD			;standard LOAD vector
	bne +
	ldy ILOAD			;lo-byte
	cpy #<NLOAD
	bne +

!if turbo_debug {
	beq +			;always detected

} else {
	lda #$40		;not detected (bit 6 set) and disabled (bit 7 clear)
}

+	sta turbomode		;set fast load flag (bits 7:6)
}
}

!if colorenable {
;Set colors
!if target = 20 {
	lda #VIC_colors		;border and background color
	sta VICCRF

} else {
	lda #border_col		;border color
	sta EXTCOL
}

!if target = 64 | target = 264 {
	lda #back_col		;background color
	sta BGCOL0
}

!if target = 128 {
	ldy #back_col		;VIC-II background color
	sty BGCOL0

	ldx #vdc_COLOR		;select VDC internal register [FORGND/BACKGND]
	jsr READREG		;get VDC foreground/background color (x, y = preserved)
	and #$f0		;keep foreground color (hi-nibble)
	ora COLOR80,y		;merge with background VDC color code translation (lo-nibble)
	jsr WRTREG		;set VDC foreground/background color
}

	lda #list_acol		;foreground color
	jsr CHROUT
}
;colorenable

	jsr CLRSCN		;clear screen

!if printbars {
;Print bars
	ldx #listtop-1		;top bar
	jsr PrintLine

	ldx #listbot+1		;bottom bar
	jsr PrintLine
}
;printbars

!if printbuttons {
;Print buttons
!if target != 20 {
!if joyenable {
	jsr JoyButton		;joystick button
}

!if fastload {
	jsr TurboButton		;turbo button

} else {
	ldx #reload_btn		;reload button
	jsr StaticButton
}
}

!if sortenable {
	jsr SortButton		;sort button
}

!if aboutenable {
	ldx #about_btn		;about button
	jsr StaticButton
}

	ldx #quit_btn		;quit button
	jsr StaticButton

	ldx #reset_btn		;reset button
	jsr StaticButton
}
;printbuttons

;Set device address
!if def_device {
	lda #def_device		;default device
	sta FADDR		;set device

} else {
	lda FADDR		;last used
	cmp #firstdev		;check device
	bcs +
	lda #firstdev		;first device
	sta FADDR		;set device
+
}

!if autodetect {
	jsr DetectDevice	;detect from current
	beq OpenDevice
	jsr DetectDevice	;restart from first
}

; --------------------
; - Open device
; --------------------

OpenDevice
!if printbuttons {
	jsr DeviceButton	;device button
}

!if rootonstart {
	jmp RootDir		;start from root
}

InitDir
!if cachemem {
	jsr ClearCache		;clear cache memory

} else {
!if rememberpos {
	lda #0			;discard prev position
	sta prevpos+1
}
}

LoadDir
	jsr GetDir		;load directory

TopDir
	jsr MoveFirstTop	;move to first entry

OldDir
!if sortenable {
	bit sortmode		;check sort mode (bit 7)
	bpl MainMenu		;leave unsorted

!if statusenable {
	jsr PleaseWait		;waiting message
}

	jsr SortList		;sort current list
}

; --------------------
; - Main menu
; --------------------

MainMenu
!if getdiskinfo {
	jsr PrintDiskInfo	;print disk info

} else {
!if statusenable {
	jsr ClearStatusBar
}
}

MenuRefresh
	jsr RedrawList		;display entries on screen

!if aboutenable {
	bit aboutmode		;check about flag (bit 7)
	bpl MenuUpdate

AboutMessage
;Display program info
	sec
	ror aboutmode		;set about flag (bit 7)

	ldy #proginfo_pos	;start column
	jsr ClearStatusPos

	lda #<proginfo_txt
	ldy #>proginfo_txt
	jsr PRNSTR
}

MenuUpdate
!if rasterbar {
;Calculate start rasterline (half values for VIC-20)
	lda #topraster >> (target = 20)		;top border
	ldx currow				;cursor row

!if listtop = 0 {
	beq +			;skip if first screen line
}

	clc
-	adc #rowheight >> (target = 20)		;row height
	dex
	bne -
+	sta startraster		;start rasterline

!if target = 128 {
	bit MODE		;check active display
	bpl MenuLoop
}
}
;rasterbar

!if target = 128 | rasterbar = 0 {
	jsr RevLine		;reverse selected
}

; --------------------
; - Menu loop
; --------------------

MenuLoop
!if rasterbar {
;Display raster bar

!if target = 20 {
!if colorenable {
	ldx #VIC_colors		;background color
	ldy #VIC_raster		;raster bar color

} else {
	lda VICCRF		;current VIC colors
	tax			;switch off

	and #$8f		;mask (bit 7 and bits 3:0)
	sta tmp
	lda TXTCOL		;current foreground color (bits 2:0)
	asl			;shift to hi-nibble (bits 6:4)
	asl
	asl
	asl
	ora tmp			;merge colors
	eor #$f0		;invert (bits 7:4)
	tay			;switch on
}

	sei			;disable interrupts
	lda startraster		;start rasterline (half value)
-	cmp RASTERH		;wait for start rasterline (high order bits)
	bne -
	sty VICCRF		;switch on

	clc			;end rasterline
	adc #rowheight >> 1	;row height (half value)
-	cmp RASTERH		;wait for end rasterline (high order bits)
	bne -
	stx VICCRF		;switch off
	cli
}
;target = 20

!if target = 64 | target = 128 {
!if colorenable {
	ldy #raster_col		;raster bar color

} else {
	lda BGCOL0		;current background color
	pha			;color off

	and #$08		;mask (bit 3)
	sta tmp
	lda TXTCOL		;current foreground color
	and #$07		;mask (bits 2:0)
	ora tmp
	eor #$0f		;invert (bits 3:0)
	tay			;color on
}

	sei			;disable interrupts
	lda startraster		;start rasterline
-	cmp RASTERL		;wait for start rasterline
	bne -
	ldx #waitraster		;wait for alignment
-	dex
	bne -
	sty BGCOL0		;switch on

	clc			;end rasterline
	adc #rowheight		;row height
-	cmp RASTERL		;wait for end rasterline
	bne -
	ldx #waitraster		;wait for alignment
-	dex
	bne -

!if colorenable {
	lda #back_col		;background color

} else {
	pla			;color off
}

	sta BGCOL0		;switch off
	cli

!if target = 128 {
;(required for keyscan to work properly)
-	bit RASTERH		;wait for scanline 256
	bpl -			;check bit 7
}
}
;target = 64 | target = 128

!if target = 264 {
!if colorenable {
	ldx #back_col		;background color
	ldy #raster_col		;raster bar color

} else {
	lda BGCOL0		;current background color
	tax			;color off

	and #$08		;mask (bit 3) and strip luminance
	sta tmp
	lda TXTCOL		;current foreground color
	and #$07		;mask (bits 2:0) and strip luminance
	ora tmp
	eor #$0f		;invert (bits 3:0)
	tay
	lda STDCOL,y		;get standard color from table
	tay			;color on
}

-	lda RASTERH		;wait for scanline 0
	lsr			;check bit 0
	bcs -			;(required to catch the right scanline)

	sei			;disable interrupts
	lda startraster		;start rasterline
-	cmp RASTERL		;wait for start rasterline
	bne -
	sty BGCOL0		;switch on

	clc			;end rasterline
	adc #rowheight		;row height
-	cmp RASTERL		;wait for end rasterline
	bne -
	stx BGCOL0		;switch off
	cli

-	lda RASTERH		;wait for scanline 256
	lsr			;check bit 0
	bcc -			;(required to speed up keyscan)
}
;target = 264
}
;rasterbar

!if joyenable {
;Get joystick input
	sei			;disable interrupts

!if target = 20 {
	lda VIAPRA		;get input from VIA #1 port A (bits 5:2)
	ora #%11000011		;mask unused bits

	inc V2DDRB		;set all bits as input for VIA #2 port B ($ff -> $00) (keyboard disabled)
	and VI2PRB		;get right direction (bit 7) and merge with previous input
	dec V2DDRB		;restore default data direction ($00 -> $ff) (keyboard re-enabled)
}

!if target = 64 | target = 128 {
	ldx joyport		;select joystick port
	beq +			;skip if port #2 (not needed)

	lda #$ff		;disable keyboard (until next keyscan)
	sta CIAPRA		;keyboard matrix columns

!if target = 128 {
	sta XSCAN		;extended keyboard matrix columns
}

+	lda CIAPRA,x		;get input from CIA #1 port A/B
	ora #%11100000		;mask unused bits
}

!if target = 264 {
	lda #$ff		;disable keyboard (until next keyscan)
	sta PIOKEY

	ldx joyport		;select joystick port
	lda JOYBIT,x		;get latch value

	sta TEDKEY		;latch joystick port
	lda TEDKEY		;get input value
}

	cli			;enable interrupts

;Check joystick input
	eor #$ff		;flip all bits
	beq GetKey		;no input

	ldx #firstdelay		;first repeat delay
	cmp lastjoy		;compare with last input
	bne +

!if rasterbar = 0 {
;Wait one frame
!if target = 264 {
-	ldx RASTERH		;wait for scanline 0
	inx			;($ff -> $00)
	beq -
-	ldx RASTERH		;wait for scanline 256
	inx			;($fe -> $ff)
	bne -

} else {
-	bit RASTERH		;wait for scanline 256
	bpl -			;check bit 7
-	bit RASTERH		;wait for scanline 0
	bmi -			;check bit 7
}
}

	dec joydelay		;delay countdown
	bne MenuLoop

	ldx #repeatdelay	;next repeat delay
+	stx joydelay		;set delay

	sta lastjoy		;update last joystick input

;Translate joystick input into keyboard input
	ldx #joytable_len
-	dex

!if target = 20 {
	asl			;check leftmost bit

} else {
	lsr			;check rightmost bit
}

	bcc -

	lda joytable,x		;get key from table
	bne MenuChoice		;(forced)

GetKey
!if target != 20 {
;Filter keyboard buffer (not needed for VIC-20)
	ldx lastjoy		;check last joystick input
	beq +			;no previous input

!if target = 64 | target = 128 {
	ldx joyport		;check joystick port
	beq +			;skip if port #2 (not needed)
}

	sta NDX			;clear keyboard buffer index

!if target = 128 | target = 264 {
	sta KYNDX		;clear function keys buffer index
}
}
;target != 20

+	sta lastjoy		;clear last joystick input
}
;joyenable

	jsr GETIN		;get keyboard input

critical_branch = * -MenuLoop + 2

!if critical_branch > 128 {
	bne MenuChoice
	jmp MenuLoop

} else {
	beq MenuLoop		;no input, waiting loop
}

; --------------------
; - Menu choice
; --------------------

MenuChoice
!if target = 128 | rasterbar = 0 | aboutenable != 0 {
	pha			;preserve input

!if target = 128 | rasterbar = 0 {
!if rasterbar {
	bit MODE		;check active display
	bpl +
}

	jsr RevLineCur		;unreverse selected
}

!if aboutenable {
+	bit aboutmode		;check about flag (bit 7)
	bpl +
	lsr aboutmode		;clear about flag

!if getdiskinfo {
	jsr PrintDiskInfo	;print disk info

} else {
!if statusenable {
	jsr ClearStatusBar
}
}
}
;aboutenable

+	pla			;restore input
}

!if color_debug {
;Change screen colors
	cmp #','
	bne +
	jsr PrevForeground
	jmp MenuRefresh

+	cmp #'.'
	bne +
	jsr NextForeground
	jmp MenuRefresh

+	cmp #'<'
	bne +
	jsr PrevBackground	;(CBM -> PrevBorder)
	jmp MenuRefresh

+	cmp #'>'
	bne +
	jsr NextBackground	;(CBM -> NextBorder)
	jmp MenuRefresh
+
}
;color_debug

;Parse user input
	ldx #keytable_len-1
-	cmp keytable,x
	beq +			;key found
	dex
	bpl -

!if searchenable {
	jsr SearchEntry		;search entry
}

	jmp MenuUpdate

;Check routine type
+	cpx #no_return		;no return required
	bcs +

;Set return address
	lda #>MenuUpdate-1
	pha
	lda #<MenuUpdate-1
	pha

;Get routine address
+	lda jumptable_hi,x
	pha
	lda jumptable_lo,x
	pha
	rts			;jump to routine

; --------------------
; - Menu routines
; --------------------

FirstEntry
	jsr MoveFirstTop
--	jmp MenuRefresh

LastEntry
	jsr MoveBotCheck	;page bottom
-	jsr MoveNext
	bne -
	beq --			;(forced)

!if devchange {
ChangeDevice
!if autodetect {
	jsr NextDevice		;detect from next
	beq +
	jsr DetectDevice	;restart from first
+

} else {
	ldx FADDR		;current device
	inx
	cpx #lastdev+1
	bcc +

	ldx #firstdev		;first device
+	stx FADDR		;set device
}

	jmp OpenDevice
}
;devchange

!if sortenable {
ChangeSort
	lda sortmode
	eor #$80		;swap sort mode (bit 7)
	sta sortmode

!if printbuttons {
	jsr SortButton		;update sort button
}

!if cachemem = 0 {
!if rememberpos {
	lda #0			;discard prev position
	sta prevpos+1
}
}

	bit sortmode		;check sort mode (bit 7)
	bpl ReloadDir		;reload current dir
	jmp TopDir		;move to first entry and sort current dir
}
;sortenable

!if cachemem {
CacheSwitch
;Enable/disable cache memory
	lda cachemode
	eor #$80		;swap flag (bit 7)
	sta cachemode
	bpl ForgetDir		;clear cache memory

!if printbuttons {
	jsr CacheButton		;update cache button
}

	jmp MenuUpdate
}
;cachemem

QuitProgram
!if target = 264 {
	jsr RestoreFnKeys	;restore function key definitions

	sei			;disable interrupts
	jsr IOINIT		;Initialize I/O Devices (restore default screen colors)
	jsr CINT		;Initialize Screen Editor (restore default cursor color and clear screen)
	jmp WARMBAS		;BASIC warm start

} else {
!if target = 128 {
	jsr MMURES		;Enable all system ROMs (BANK 15)
}

	jmp WARMST2		;Warm start routine (without RESTOR)
}

ResetMachine
!if cartsupport {
	jmp CleanReset		;clean reset

} else {
!if target = 264 {
-	lda LSTX		;wait for key release
	cmp #$40		;no key pressed
	bne -
}

	jmp (RESVEC)		;system reset
}

SelectEntry
!if addressmode {
	lda #1			;not relocated load address
	!by BIT2_opcode		;skip next two bytes

SelectBasic
	lda #0			;BASIC relocated load address
	sta loadmode		;set load address mode
}

;Highlight selected entry
!if rasterbar {
	jsr RevLine

} else {
	jsr RevLineCur
}

; --------------------
; - Check position
; --------------------

;Check if back entry "<-"
	jsr CheckFirst
	bne ++

PrevDir
;Move to previous dir or exit from disk image
!if img_debug {
!if colorenable {
!if target != 20 {
	lda #border_col		;restore border color
	sta EXTCOL
}
}
}

	lda #0			;clear disk image flag
	sta insideimage

	lda #backcmd_len	;command length
	ldx #<backcmd		;parent command
	ldy #>backcmd
	jsr SendCmd
	bcs ForgetDir		;check status

!if cachemem {
	bit cachemode		;check cache mode (bit 7)
	bpl ReloadDir		;do not pull from cache
	jsr PullDir		;restore prev dir from cache
	beq ReloadDir		;cache memory is empty

!if rememberpos {
!if sortenable {
!if printbuttons {
	jsr SortButton		;update sort button
}
}

	jmp MainMenu

} else {
	jmp TopDir
}

} else {
!if rememberpos {
	jsr GetDir		;load directory

	lda prevpos+1		;check if available
	beq +
	sta selected+1		;restore prev position
	lda prevpos
	sta selected
	lda prevrow		;restore cursor row
	sta currow
	lda #0			;discard prev position
	sta prevpos+1
	jmp OldDir

+	jmp TopDir
}
}

ReloadDir
	jmp LoadDir

!if rootentry {
;Check if root entry "//"
!if cachemem {
++	lda #tblhdr+entrylen	;offset to 2nd entry
	jsr CheckPos

} else {
++	lda selected+1
	cmp #>tbl+tblhdr+entrylen
	bne ++
	lda selected
	cmp #<tbl+tblhdr+entrylen
}

	bne ++
}

RootDir
;Move to root directory
	lda #rootcmd_len	;command length
	ldx #<rootcmd		;root command
	ldy #>rootcmd
	jsr SendCmd

ForgetDir
	jmp InitDir		;clear cache memory (or discard prev position)

; --------------------
; - Check selected
; --------------------

;Copy name of selected into DOS command
++	ldy #0
-	lda (selected),y
	beq +
	sta dosname,y
	iny
	cpy #namelen
	bne -
+	sty dosnamelen		;name length
	lda #0			;end of string
	sta dosname,y

;Check if selected is a directory
	ldy #namelen
	lda (selected),y	;check type
	beq DirSelected		;change dir

;Check if already inside a disk image
	bit insideimage		;check disk image flag (bit 7)
	bmi LoadPrg		;already inside a disk image, load program

;Search for DOS extension
	sec
	lda dosnamelen		;name length
	sbc #typelen+1
	bcc LoadPrg		;less than 4 chars, load program
	tay
	lda (selected),y	;check last 4th char
	cmp #'.'		;dot preceding extension
	bne LoadPrg		;no extension, load program

;Check if selected is a known disk/tape image
	iny			;offset to extension
	ldx #dos_ext		;DOS extensions list
	jsr ParseExt		;parse file extension
	bvs LoadPrg		;no match, load program

!if img_debug {
!if colorenable {
!if target != 20 {
	stx EXTCOL		;change border color
}
}
}

!if tapesupport {
	cpx #tap_ext		;check if TAP image
	beq MountTape		;mount tape image
}

	cpx #dnp_ext		;check if DNP native partition
	beq DirSelected		;skip flag setting to allow subfolders browsing

	lda #$80		;set disk image flag (bit 7)
	sta insideimage

DirSelected
!if cachemem {
	bit cachemode		;check cache mode (bit 7)
	bpl +
	jsr PushDir		;store current dir into cache

} else {
!if rememberpos {
	lda selected		;store current position
	sta prevpos
	lda selected+1
	sta prevpos+1
	lda currow		;store current cursor row
	sta prevrow
}
}

+	ldx #'c'		;change dir command "cd:"
	ldy #'d'
	jsr SetupCmd		;setup command (then send)

	jmp LoadDir

!if tapesupport {
MountTape
	lda #$40		;set tape image flag (bit 6)
	sta insideimage

	ldx #'x'		;mount tape command "xt:"
	ldy #'t'
	jsr SetupCmd		;setup command (then send)
}

; --------------------
; - Load program
; --------------------

LoadPrg
!if target = 128 {
	jsr MMURES		;enable all system ROMs (BANK 15)
}

!if target = 264 {
	jsr RestoreFnKeys	;restore function key definitions
}

!if fastload {
	bit turbomode		;check fast load enabled flag (bit 7)
	bmi +
	jsr RESTOR		;Restore RAM Vectors for Default I/O Routines
+
}

; --------------------
; - VIC-20 config
; --------------------

!if target = 20 {
!if vicmemcfg {
;Set cartridge mode and memory config according to file load address

!if cartsupport {
	lda #0			;clear cartridge mode
	sta cartmode
}

!if tapesupport {
	bit insideimage		;check tape image flag (bit 6)
	bvs no_config
}

!if addressmode {
	lda loadmode		;check load address mode
	beq no_config
}

;Set file name
	lda dosnamelen		;name length
	ldx #<dosname		;name address
	ldy #>dosname
	jsr SETNAM

;Set logical file parameters
	lda #filenum		;logical file number
	ldx FADDR		;device address
	ldy #0			;sec. address (read file)
	jsr SETLFS

!if statusenable {
	jsr PleaseWait		;waiting message
}

	jsr OPEN		;open file
	bcs end_config		;check status

	ldx #filenum		;set logical file as input
	jsr CHKIN

;Detect load address
	jsr CHRIN		;get lo-address
	pha
	jsr CHRIN		;get hi-address
	tay
	pla			;restore lo-address

!if cartsupport {
	bne CheckBasic

;Check cart image
	lda #$80		;cartridge mode (bit 7)

	cpy #>RAMBLK1		;block 1 ($2000-$3fff)
	beq SetCart

	cpy #>RAMBLK2		;block 2 ($4000-$5fff)
	beq SetCart

	cpy #>RAMBLK3		;block 3 ($6000-$7fff)
	beq SetCart

	cpy #>RAMBLK5		;block 5 ($a000-$bfff)
	bne end_config

	ora #$40		;autostart mode (bit 6)

SetCart
	sta cartmode		;set cartridge mode
	jmp end_config
}

CheckBasic
	cmp #1			;check lo-address
	bne end_config

	cpy #>RAMBOT_8K		;+8k (no changes)
	beq end_config

	cpy #>RAMBOT_UNEXP	;unexpanded

!if vicmemcfg = 8 {
	beq +
	cpy #>RAMBOT_3K		;+3k (block 0)
}

	bne end_config		;no changes

;Configure system memory
+	sty MEMSTR+1		;Pointer to the Start of user memory (hi-byte)

!if vicmemcfg = 8 {
	lda #>RAMTOP_UNEXP	;End of user RAM (+3k or unexpanded)
	sta MEMSIZ+1		;Pointer to the End of user memory (hi-byte)
	sta HIBASE		;Top Page of Screen Memory
}

;Initialize mem pointers
	jsr INITSK		;Initialize Screen and Keyboard
	jsr INITV		;Initialize BASIC Vectors
	jsr INITCZ		;Initialize BASIC RAM
	jsr SCRTCH		;Perform NEW

end_config
	jsr CLRCH		;restore default I/O channels
	lda #filenum		;close file
	jsr CLOSE

no_config

; --------------------
; - Cartridge image
; --------------------

!if cartsupport {
;Check cartridge mode
	bit cartmode
	bpl not_cart

;Set logical file parameters
	lda #filenum		;logical file number
	ldx FADDR		;device address
	ldy #1			;sec. address (not relocated)
	jsr SETLFS

;Load cartridge image
	lda #0			;load flag
	jsr LOAD		

	bit cartmode		;check autostart (bit 6)
	bvs CleanReset

	jmp MainMenu		;resume browser

CleanReset
;(required for some cart images like Ms. Pac-Man)
	sei
	lda #0			;reset VIA ports
	sta VIDDRB
	sta VIDDRA
	sta V2DDRB
	sta V2DDRA
	lda #$7f		;disable all interrupts
	sta VIAIER
	sta VI2IER

	jmp (RESVEC)		;system reset

not_cart
}
;cartsupport
}
;vicmemcfg
}
;target = 20

; --------------------
; - Program launcher
; --------------------

;LOAD and RUN program through BASIC prompt
	sei			;disable interrupts

!if tapesupport {
	bit insideimage		;check tape image flag (bit 6)
	bvc +
	jsr RESTOR		;Restore RAM Vectors for Default I/O Routines
+
}

!if target = 64 | target = 20 {
	jsr INITSK		;Initialize Screen and Keyboard (restore default colors and clear screen)

} else {
	jsr IOINIT		;Initialize I/O Devices (restore default screen colors)
	jsr CINT		;Initialize Screen Editor (restore default cursor color and clear screen)
}

;Fill keyboard buffer
!if target = 20 | target = 64 {
	ldx #1			;keyboard queue length
	stx NDX
	lda #RUN_char		;RUN key code
	sta KEYD

} else {
	ldx #loadrun_len	;keyboard queue length
	stx NDX
-	lda loadrun-1,x		;LOAD and RUN commands
	sta KEYD-1,x
	dex
	bne -
}

;BASIC start-up screen
!if target = 128 {
	jsr STRTMSG		;Display BASIC start-up messages (interrupts re-enabled)
	jsr SCRTCH		;perform NEW

} else {
	jsr FREMSG		;Display BASIC start-up messages, then perform NEW (interrupts re-enabled)
}

!if tapesupport {
	bit insideimage		;check tape image flag (bit 6)
	bvs BasicPrompt
}

;Print file name and parameters
	lda TBLX		;get cursor row
	pha			;preserve for later
	tax
	inx			;row below READY message
	inx
	ldy #4			;leave space for LOAD command
	clc
	jsr PLOT

	lda #quote_char		;opening quote
	jsr CHROUT

	lda #<dosname		;file name
	ldy #>dosname

!if target = 128 {
	jsr STROUT		;(keep BANK 15)

} else {
	jsr PRNSTR
}

	lda #quote_char		;closing quote
	jsr CHROUT

	lda #','		;comma
	jsr CHROUT

	lda #0			;hi-byte
	ldx FADDR		;device address

!if target = 128 {
	jsr LINPRT		;(keep BANK 15)

} else {
	jsr PRNINT
}

!if addressmode {
	lda loadmode		;check load address mode
	beq +
}

	lda #','		;comma
	jsr CHROUT

	lda #'1'		;not relocated load address
	jsr CHROUT

+	pla			;restore cursor row
	sta TBLX

BasicPrompt
	jmp READY		;Display READY message, then fall into Main BASIC Loop

; --------------------
; - Subroutines
; --------------------

RedrawList
;Display list on screen (relative to selected)
;args:
;	selected, currow
;consts:
;	list_acol, listtop, listbot
;calls:
;	MoveTopCheck, PrintEntry, MoveNext
;	CHROUT, CLRLN
;regs:
;	a, x, y = not preserved

;Clear all entries shown on screen
!if colorenable {
	lda #list_acol		;set color
	jsr CHROUT
}

	ldx #listtop		;top row
-	jsr CLRLN		;clear line
	inx
	cpx #listbot+1		;bottom row (+1)
	bne -

;Fill screen with entries
!if counterenable | searchenable {
	jsr PushSel		;preserve selected

} else {
	lda selected+1		;preserve selected
	pha
	lda selected
	pha
}

	lda currow		;preserve cursor row
	pha

	jsr MoveTopCheck	;top of page
-	jsr PrintEntry
	jsr MoveNext
	beq +			;last reached

	inc currow		;cursor down
	ldx currow
	cpx #listbot+1		;check bottom
	bcc -

+	pla			;restore cursor row
	sta currow

!if counterenable | searchenable {
PullSel
	lda tmpptr		;restore selected
	sta selected
	lda tmpptr+1
	sta selected+1
	rts

PushSel
	lda selected		;preserve selected
	sta tmpptr
	lda selected+1
	sta tmpptr+1
	rts

} else {
	pla			;restore selected
	sta selected
	pla
	sta selected+1
	rts
}

PrintEntry
;Print selected entry at current row
;args:
;	selected, currow
;uses:
;	PNTR, (TBLX), (TXTCOL), (USER)
;consts:
;	list_acol, listpos, (typepos), (sizepos)
;	ext_table, namelen, typelen, (sizelen)
;calls:
;	CHROUT, CLRLN, (PRNINT), (RightInt16)
;regs:
;	a, x, y = not preserved

!if colorenable {
	lda #list_acol		;set color
	jsr CHROUT
}

	ldx currow		;row
	ldy #listpos		;column

!if target = 128 | target = 264 {
	stx TBLX		;set row
}

	sty PNTR		;set column
	jsr CLRLN		;clear line

!if target = 20 | (target = 64 & kernalold != 0) {
;(required for VIC-20 and for C64 with Kernal ROM rev. 1 or 2)

	ldy #screencols-1
-	lda TXTCOL		;current foreground color
	sta (USER),y
	dey
	bpl -
}

;Print name
	ldy #0
-	lda (selected),y	;load char

!if list_tab = 1 {
	bne +
	lda #' '		;padding

} else {
	beq ++			;stop printing
}

+	jsr CHROUT		;print char
++	iny
	cpy #namelen
	bne -

;Print type
!if list_tab = 1 {
	inc PNTR		;move cursor right

} else {
	lda #typepos		;set column
	sta PNTR
}

	lda (selected),y	;get type (y = namelen)

!if getsize {
	pha			;push type
}

	tax
	ldy #typelen		;chars to print
-	lda ext_table,x
	jsr CHROUT
	inx
	dey
	bne -

!if getsize {
;Print size
!if list_tab = 1 {
	inc PNTR		;move cursor right

} else {
	ldy #sizepos		;set column
	sty PNTR
}

	pla			;pull type
	beq revline_rts		;directory (do not print)

	ldy #namelen+1		;get file size
	lda (selected),y	;lo-byte
	tax
	iny
	lda (selected),y	;hi-byte

!if rightalign {
	jmp RightInt16		;(then return)

} else {
	jmp PRNINT		;(then return)
}

} else {
	rts	
}
;getsize = 0

RevLine
;Reverse characters of selected entry
;args:
;	currow, PNT, (MODE)
;calls:
;	SETPNT, (RevLine80)
;consts:
;	screencols, listpos, listend
;regs:
;	a, x, y = not preserved

	ldx currow		;set cursor row
	jsr SETPNT		;set line address (then continue)

!if colorenable & colorline {
!if target = 20 | target = 64 {
	jsr SCOLOR		;synchronize color address (then continue)
}
}

RevLineCur
;Reverse characters of current screen line
!if target = 128 {
	bit MODE		;check active display
	bmi RevLine80
}

!if wholeline | rasterbar {
	ldy #screencols-1	;whole screen line

} else {
	ldy #listpos-1		;start column
}

-	lda (PNT),y
	eor #$80		;toggle reverse char (bit 7)
	sta (PNT),y

!if colorenable & colorline {
	bmi +
	lda #list_col		;not selected
	!by BIT2_opcode		;skip next two bytes

+	lda #select_col		;selected
	sta (USER),y		;set color
}

!if wholeline | rasterbar {
	dey
	bpl -

} else {
	iny
	cpy #listend+2		;end column (+1)
	bne -
}

revline_rts
	rts

!if target = 128 {
RevLine80
;Reverse VDC characters at current screen line
;consts:
;	listpos, listwidth, vdc_COUNT, (COLOR80)
;	(list_col), (sel80_col)
;calls:
;	ATTRPOS, READ80, WRITE80, WRTREG
;regs:
;	a, x, y = not preserved

	ldy #listpos-1		;start column
	jsr ATTRPOS		;set attribute memory address (y = preserved)
	jsr READ80		;get character attributes (and move to next position) (y = preserved)
	eor #$40		;toggle reverse (bit 6)

!if colorenable & colorline {
	rol			;check reverse (bit 7 <- bit 6)
	bmi +
	ldx #list_col		;not selected
	!by BIT2_opcode		;skip next two bytes

+	ldx #sel80_col		;selected
	ror			;restore reverse (bit 7 -> bit 6)
	and #$f0		;keep character attributes (hi-nibble)
	ora COLOR80,x		;merge with foreground VDC color code translation (lo-nibble)
}

	jsr WRITE80		;set character attributes (and move to next position) (y = preserved)
	jsr ATTRPOS		;restore start column (y = preserved)

	lda #listwidth+2	;line lenght
	ldx #vdc_COUNT		;select VDC internal register [WORD COUNT]
	jmp WRTREG		;block write repeat (then return)
}
;target = 128

CheckFirst
;Check if selected is first entry

!if cachemem {
	lda #tblhdr		;offset to first

CheckPos
;Check offset of selected from base address
;args:
;	selected -> current entry
;	tblbas -> table base address
;	a = offset
;return:
;	a, (x) = not preserved
;status:
;	zero set = match
;	 "   clear = no match

	clc
	adc tblbas
	tax
	lda tblbas+1
	adc #0
	cmp selected+1
	bne +
	cpx selected

} else {
	lda selected+1
	cmp #>tbl+tblhdr
	bne +
	lda selected
	cmp #<tbl+tblhdr
}

+	rts

MovePrev
;Move to previous entry
;args:
;	selected -> current entry
;consts:
;	entrylen
;calls:
;	CheckFirst
;return:
;	a, (x) = not preserved
;status:
;	zero set = first reached (no changes)
;	 "   clear = selected -> prev entry

	jsr CheckFirst
	beq +			;first reached

	sec			;prev entry
	lda selected
	sbc #entrylen
	sta selected
	lda selected+1
	sbc #0
	sta selected+1		;(not zero, normally)
+	rts

PrevEntry
;Move to previous entry and, if top reached, scroll down page
;args:
;	selected -> current entry
;	currow = cursor row
;consts:
;	listtop
;calls:
;	MovePrev, ScrollDown
;return:
;	selected -> prev entry (unaltered, if first already)
;	currow = prev row (unaltered, if top already)
;	a, x, y = not preserved

	jsr MovePrev
	beq +			;first reached (no changes)

	ldx currow		;check cursor row
	cpx #listtop
	beq ScrollDown		;top reached

	dec currow		;cursor up
+	rts

ScrollDown
;Scroll page down by one line
;uses:
;	PNT, tmpptr
;consts:
;	listtop, listbot, listend, listpos, screencols
;calls:
;	SETPNT, CopyLine, PrintEntry
;regs:
;	a, x, y = not preserved

!if target = 128 & romscroll = 0 {
	bit MODE		;check active display
	bpl +
}

!if target = 128 | (target = 264 & romscroll != 0) {
	jsr SetWindow		;set window
	jsr SCRLDN		;scroll down
	jsr FULLW		;full screen
	jmp PrintEntry		;(then return)
}

!if romscroll = 0 {
+	ldx #listbot		;bottom row
	jsr SETPNT		;set line address

-	sec			;prev line
	lda PNT
	sta tmpptr		;lower line
	sbc #screencols
	sta PNT			;upper line
	lda PNT+1
	sta tmpptr+1
	sbc #0
	sta PNT+1

	jsr CopyLine		;upper -> lower
	dex
	cpx #listtop		;top row
	bne -
	jmp PrintEntry		;(then return)

CopyLine
	ldy #listpos		;start column
-	lda (PNT),y
	sta (tmpptr),y
	iny
	cpy #listend+1		;end column (+1)
	bne -
	rts
}

PrevPage
;Check if top row, if so move to previous page of the list
;uses:
;	currow, tmp, (joydelay)
;consts:
;	listtop, listrows
;calls:
;	MoveTop, PrevEntry
;return:
;	selected -> prev page (or first entry)
;	currow = top row
;	x = not preserved

	ldx currow		;check cursor row
	cpx #listtop
	bne MoveTop		;move to top

	ldx #listrows		;page height
	stx tmp

-	jsr PrevEntry
	dec tmp			;steps left
	bne -

!if joyenable {
	ldx #1			;reduce repeat delay
	stx joydelay
}

	rts

MoveTop
;Move to top row of current page
-	jsr MovePrev
	beq +			;first reached
	dec currow		;cursor up

MoveTopCheck
	ldx currow		;check cursor row
	cpx #listtop
	bne -
+	rts

MoveFirstTop
;Move to first entry (with cursor at top row)
	lda #listtop
	sta currow		;(then continue)

MoveFirst
;Move to first entry
!if cachemem {
	clc
	lda tblbas
	adc #tblhdr
	sta selected
	lda tblbas+1
	adc #0

} else {
	lda #<tbl+tblhdr
	sta selected
	lda #>tbl+tblhdr
}

	sta selected+1
	rts

MoveNext
;args:
;	selected -> current entry
;	tbllst -> last entry
;consts:
;	entrylen
;return:
;	a = not preserved
;status:
;	zero set = last reached (no changes)
;	 "   clear = selected -> next entry

	lda selected+1		;check last
	cmp tbllst+1
	bne +
	lda selected
	cmp tbllst
	beq ++			;last reached

AddEntry
;Move to next entry without checking end of list
+	clc			;next entry
	lda selected
	adc #entrylen
	sta selected
	bcc ++			;(not zero, without carry)
	inc selected+1		;(not zero, normally)
++	rts

NextEntry
;Move to next entry and, if bottom reached, scroll up page
;args:
;	selected -> current entry
;	currow = cursor row
;consts:
;	listbot
;calls:
;	MoveNext, ScrollUp
;return:
;	selected -> next entry (unaltered, if last already)
;	currow = next row (unaltered, if bottom already)
;	a, x ,y = not preserved

	jsr MoveNext
	beq +			;last reached (no changes)

	ldx currow		;check cursor row
	cpx #listbot
	beq ScrollUp		;bottom reached

	inc currow		;cursor down
+	rts

ScrollUp
;Scroll page up by one line
;uses:
;	PNT, tmpptr
;consts:
;	listtop, listbot, listend, listpos, screencols
;calls:
;	SETPNT, CopyLine, PrintEntry
;regs:
;	a, x, y  = not preserved

!if target = 128 & romscroll = 0 {
	bit MODE		;check active display
	bpl +
}

!if target = 128 | (target = 264 & romscroll != 0) {
	jsr SetWindow		;set window
	jsr SCRLUP		;scroll up
	jsr FULLW		;full screen
	jmp PrintEntry		;(then return)
}

!if romscroll = 0 {
+	ldx #listtop		;top row
	jsr SETPNT		;set line address

-	clc			;next line
	lda PNT
	sta tmpptr		;upper line
	adc #screencols
	sta PNT			;lower line
	lda PNT+1
	sta tmpptr+1
	adc #0
	sta PNT+1

	jsr CopyLine		;lower -> upper
	inx
	cpx #listbot		;bottom row
	bne -
	jmp PrintEntry		;(then return)
}

NextPage
;Check if bottom row, if so move to next page of the list
;uses:
;	currow, tmp, (joydelay)
;consts:
;	listbot, listrows
;calls:
;	MoveBot, NextEntry
;return:
;	selected -> next page (or last entry)
;	currow = bottom row (or last of page)
;	x = not preserved

	ldx currow		;check cursor row
	cpx #listbot
	bne MoveBot		;move to bottom

	ldx #listrows		;page height
	stx tmp

-	jsr NextEntry
	dec tmp			;steps left
	bne -

!if joyenable {
	ldx #1			;reduce repeat delay
	stx joydelay
}

	rts

MoveBot
;Move to bottom row of current page
-	jsr MoveNext
	beq +			;last reached
	inc currow		;cursor down

MoveBotCheck
	ldx currow		;check cursor row
	cpx #listbot
	bne -
+	rts

!if autodetect {
DetectDevice
-	ldx #0			;clear STATUS
	stx STATUS

	jsr LISTEN		;send LISTEN command (a = device)
	jsr UNLSN		;send UNLISTEN command

	ldx STATUS		;check STATUS
	beq +			;detected (return zero)

NextDevice
	inc FADDR		;next device
	lda FADDR
	cmp #lastdev+1		;check range
	bcc -

	lda #firstdev		;restore first device
	sta FADDR		;not detected (return not zero)
+	rts
}
;autodetect

!if devchange {
!if printbuttons {
DeviceButton
	ldx #device_btn		;button offset
	jsr StaticButton

	ldx FADDR		;device address
	cpx #10			;check digits
	bcs +
	lda #'0'		;precede with zero
	jsr CHROUT

+	lda #0			;hi-byte
	jmp PRNINT		;(then return)
}
;printbuttons
}
;devchange

ParseExt
;Parse file type/extension
;args:
;	x = offset to list of file types/extensions
;	y = offset to extension into file name
;	selected -> current entry (preserved)
;consts:
;	ext_table, set_v, typelen
;uses:
;	tmp = chars counter
;return:
;	a = not preserved
;	x = offset to last checked extension
;	y = preserved
;status:
;	overflow set = no match
;	 "       clear = match found

--	clv			;clear overflow
	lda #typelen		;chars to compare
	sta tmp
-	lda (selected),y
	and #$7f		;clear shift flag (bit 7)
	cmp ext_table,x
	beq +
	bit set_v		;no match (set overflow)
+	inx
	iny
	dec tmp			;chars counter
	bne -
	dey			;restore entry offset
	dey
	dey
	bvc +			;match found (no overflow)
	lda ext_table,x		;check end of list
	bne --			;next extension
+	dex			;restore extension offset
	dex
	dex
set_v	rts			;used to set overflow (bit 6)

SetupCmd
;Setup disk command (then send)
;args:
;	x = first char of command
;	y = second char of command

	stx doscmd		;command "cd:" or "xt:"
	sty doscmd+1
	lda #':'
	sta doscmd+2

	clc
	lda #cmdlen		;command length
	adc dosnamelen		;name length

	ldx #<doscmd		;command string
	ldy #>doscmd		;(then continue)

SendCmd
;Send command to disk drive
;args:
;	a = command length
;	x:y -> command string
;	FADDR = device address
;consts:
;	filenum, cmdchan
;calls:
;	SETNAM, SETLFS, OPEN, CLOSE
;return:
;	a, x, y = not preserved
;status:
;	carry set = disk error
;	 "    clear = done

	jsr SETNAM		;set command

	lda #filenum		;logical file number
	ldx FADDR		;device address
	ldy #cmdchan		;sec. address (command channel)
	jsr SETLFS		;set logical file parameters

	jsr OPEN		;send command
	php			;preserve status

	lda #filenum		;close file
	jsr CLOSE
	plp			;restore status
	rts

GetDir
;Load directory in memory
;args:
;	FADDR = device address
;	(tblbas -> table base address)
;consts:
;	filenum, dircmd, dircmd_len, tblhdr, (tbl), (tbldata)
;	entrylen, namelen, (diskidlen), quote_char, cbm_types
;uses:
;	selected -> entry to add
;	STATUS, (FACHO), (VICCRF), (EXTCOL)
;calls:
;	SETLFS, SETNAM, OPEN, CLOSE, CHKIN, CLRCH, STOP
;	PleaseWait, FindName, ParseExt
;return:
;	tbllst -> last entry
;	a, x, y = not preserved

!if statusenable {
	jsr PleaseWait		;waiting message
}

;Clear (current) dir table
	lda #0

!if cachemem {
	ldy #tbldata		;preserve prev table data
-	sta (tblbas),y

} else {
	tay			;clear full page
-	sta tbl,y
}

	iny
	bne -

;Add back entry "<-"
	lda #left_arrow

!if cachemem {
	ldy #tblhdr
	sta (tblbas),y

} else {
	sta tbl+tblhdr
}

!if rootentry {
;Add root entry "//"
	lda #'/'

!if cachemem {
	ldy #tblhdr+entrylen+1
	sta (tblbas),y
	dey
	sta (tblbas),y

} else {
	sta tbl+tblhdr+entrylen+1
	sta tbl+tblhdr+entrylen
}
}

;Set last entry (empty list)
!if cachemem {
	clc
	tya			;(y = tblhdr+entrylen*rootentry)
	adc tblbas
	sta selected
	lda tblbas+1
	adc #0
	sta selected+1

} else {
	lda #<tbl+tblhdr+entrylen*rootentry
	sta selected
	lda #>tbl+tblhdr+entrylen*rootentry
	sta selected+1
}

;Set file name
	lda #dircmd_len		;name length
	ldx #<dircmd		;name address
	ldy #>dircmd
	jsr SETNAM

;Set logical file parameters
	lda #filenum		;logical file number
	ldx FADDR		;device address
	ldy #0			;sec. address (read file)
	jsr SETLFS

!if dir_debug {
!if target = 20 {
	inc VICCRF		;change VIC colors

} else {
	inc EXTCOL		;change border color
}
}

	jsr OPEN		;open directory
	bcs DirExit

	ldx #filenum		;set logical file as input
	jsr CHKIN

; --------------------
; - Dir Parser
; --------------------

	ldy #6 >> getsize	;bytes to skip (or pairs to read)
	jsr FindName		;(y = 0, on exit)
	bne DirExit		;not found

!if getdiskinfo {
!if cachemem {
	ldy #tbldata		;preserve prev table data
}
}

;Read until end of line
-	jsr CHRIN
	beq GetEntry		;end of line

!if getdiskinfo {
;Get disk info
!if getdiskid {
	cmp #quote_char		;closing quote
	bne +

	jsr CHRIN		;drop space after disk name
	ldy #tblhdr-diskidlen	;continue with disk id
	bne -			;(forced)
}

+	cpy #tblhdr		;drop exceeding chars
	beq +

!if cachemem {
	sta (tblbas),y

} else {
	sta tbl,y
}

	iny			;next char
}

+	lda STATUS		;check status
	beq -
	bne DirExit

GetEntry
	ldy #4 >> getsize	;bytes to skip (or pairs to read)
	jsr FindName		;(y = 0, on exit)
	bne DirExit		;not found
	jsr AddEntry		;add entry to list

;GetLine
-	jsr CHRIN
	sta (selected),y
	beq GetName		;end of line
	iny			;next char
	lda STATUS		;check status
	beq -
	bne DirExit

GetName
	tay			;reset index (a = 0)
-	lda (selected),y
	cmp #quote_char		;closing quote
	beq +
	iny
	bne -			;(forced, normally)

-	lda #0			;add padding
	sta (selected),y
	iny			;skip padding spaces
+	cpy #namelen		;check length
	bcc -

;GetType
	iny			;skip quote (or last padding space)
	iny			;skip space (or splat file mark)
	ldx #cbm_types		;CBM file types list
	jsr ParseExt		;parse file type
	txa			;offset to file type

!if getsize {
;Get file size
	ldx #3			;bytes to store
}

	ldy #namelen
-	sta (selected),y

!if getsize {
	iny
	lda FACHO-2,x		;file size (last stored pair)
	dex
	bne -
}

	jsr STOP		;check STOP key
	bne GetEntry		;continue reading

DirExit
!if dir_debug {
!if target = 20 {
	dec VICCRF		;restore VIC colors

} else {
	dec EXTCOL		;restore border color
}
}

	lda selected		;set last entry
	sta tbllst
	lda selected+1
	sta tbllst+1

	jsr CLRCH		;restore default I/O channels
	lda #filenum
	jmp CLOSE		;close file (then return)

FindName
!if getsize {
;Get pairs at the beginning of current line
--	ldx #2			;byte pair (y = pairs to read) 
-	jsr CHRIN
	sta FACHO-1,x		;store pair (in reverse order)
	dex			;next byte
	bne -
	dey			;next pair
	bne --

} else {
;Skip bytes at the beginning of current line
-	jsr CHRIN		;(y = bytes to skip)
	dey
	bne -
}

;Find quote into current line
-	jsr CHRIN
	cmp #quote_char		;opening quote
	beq +
	lda STATUS		;check status
	beq -
+	rts			;(y = 0, on exit)

!if getdiskinfo {
PrintDiskInfo
;Display disk name and id
!if colorenable {
	lda #diskinfo_acol	;set color
	jsr CHROUT
}

	ldy #diskname_pos	;cursor column
	jsr ClearStatusPos

	lda #RVS_on		;reverse
	jsr CHROUT

!if cachemem {
	ldy #tbldata
-	lda (tblbas),y		;from table header

} else {
	ldy #0
-	lda tbl,y
}

	bne +			;padding
	lda #' '

+	jsr CHROUT
	iny

!if getdiskid {
	cpy #tblhdr-diskidlen	;switch to disk id
	bne +

!if target = 20 {
	inc PNTR		;move cursor right

} else {
	lda #diskid_pos		;set column
	sta PNTR
}
}

+	cpy #tblhdr
	bne -

	lda #RVS_off		;normal

!if counterenable {
	jsr CHROUT		;(then continue)

;Print entries number
!if colorenable {
	lda #active_acol	;set color
	jsr CHROUT
}

	ldy #counter_pos-(2*rightalign)
	sty PNTR		;set column

;Calculate entries number
	jsr PushSel		;preserve selected
	jsr MoveFirst
	ldx #$ff-rootentry	;counter (-1 or -2)
	ldy #$ff		;hi-byte

-	inx			;lo-byte
	bne +
	iny			;hi-byte
+	jsr MoveNext
	bne -

	jsr PullSel		;restore selected
	tya			;hi-byte

!if rightalign {
	jmp RightInt16		;(then return)

} else {
	jmp PRNINT		;(then return)
}

} else {
	jmp CHROUT		;(then return)
}
;counterenable = 0
}
;getdiskinfo

!if statusenable {
PleaseWait
;Display waiting message
	ldy #status_pos		;start column
	jsr ClearStatusPos

	lda #<waitmsg_txt
	ldy #>waitmsg_txt
	jmp PRNSTR		;(then return)
}

!if statusenable | aboutenable | getdiskinfo {
ClearStatusPos
;Set message position
	sty PNTR		;set column

ClearStatusBar
;Clear status bar
	ldx #status_row

!if target = 128 | target = 264 {
	stx TBLX		;set row
}

	jmp CLRLN		;(then return)
}

!if sortenable {
SortList
;Sort list by name using Bubble sort algorithm
;args:
;	(tbl), (tblbas), tblhdr, tbllst, entrylen
;uses:
;	tmpptr -> current entry
;	auxptr -> previous entry
;	(lastsort -> last to sort)
;	(lastswap -> last swap)
;	(tmp = swap flag)
;regs:
;	a, y = not preserved

!if fastsort {
;Optimized sort pointers
	lda tbllst		;set last to sort
	sta lastsort
	lda tbllst+1
	sta lastsort+1
}

SortStart
!if cachemem {
	clc			;set first to sort
	lda tblbas
	adc #tblhdr+(entrylen<<rootentry)
	sta tmpptr
	lda tblbas+1
	adc #0
	sta tmpptr+1

} else {
	lda #<tbl+tblhdr+(entrylen<<rootentry)
	sta tmpptr
	lda #>tbl+tblhdr+(entrylen<<rootentry)
	sta tmpptr+1
}

	lda #0

!if fastsort {
	sta lastswap+1		;clear last swap (hi-byte)

} else {
	sta tmp			;clear swap flag
}

SortCheck
	jsr STOP		;check STOP key
	beq SortQuit

	lda tmpptr
	ldy tmpptr+1

!if fastsort {
	cpy lastsort+1		;check last to sort
	bcc SortNext
	cmp lastsort
	bcc SortNext

	lda lastswap		;update last to sort
	sta lastsort
	lda lastswap+1		;check last swap (hi-byte)
	sta lastsort+1

} else {
	cpy tbllst+1		;check last entry
	bcc SortNext
	cmp tbllst
	bcc SortNext
	lda tmp			;check swap flag
}

	bne SortStart		;restart

SortQuit
	rts

SortNext
	sta auxptr		;previous entry
	adc #entrylen
	sta tmpptr		;current entry
	sty auxptr+1
	bcc SortComp
	inc tmpptr+1

SortComp
	ldy #0			;compare entries
-	lda (tmpptr),y
	cmp (auxptr),y
	bcc SortSwap		;current < previous
	bne SortCheck		;current > previous
	iny
	cpy #entrylen
	bne -
	beq SortCheck		;current = previous (forced)

SortSwap
!if fastsort {
	lda tmpptr		;set last swap
	sta lastswap
	lda tmpptr+1
	sta lastswap+1
}

	ldy #entrylen-1		;swap entries
-	lda (auxptr),y
	pha
	lda (tmpptr),y
	sta (auxptr),y
	pla
	sta (tmpptr),y
	dey
	bpl -

!if fastsort = 0 {
	sty tmp			;set swap flag (y = $ff)
}

	bmi SortCheck		;(forced)

!if printbuttons {
SortButton
;Print sort button
	lda sortmode		;sort flag (bit 7)
	ldx #sort_btn		;button offset
	jmp ActiveButton	;(then return)
}
;printbuttons
}
;sortenable

!if searchenable {
SearchEntry
;Return next entry that starts with a specified char
;args:
;	a = char to search
;	selected -> current entry (preserved)
;uses:
;	tmp
;calls:
;	MoveFirst, MoveNext, MoveTarget
;return:
;	auxptr -> found entry
;	a, y = not preserved

	sta tmp			;char to find
	jsr PushSel		;preserve selected

	jsr SearchNext		;search from next
	bcs SearchDone		;found

	jsr MoveFirst		;restart from first
	jsr SearchThis		;search from current

SearchDone
	lda selected		;set target
	sta auxptr
	lda selected+1
	sta auxptr+1
	jsr PullSel		;restore selected

	bcs MoveTarget		;move to found entry
	rts			;not found

SearchThis
	ldy #0			;first char
	lda (selected),y
	and #$7f		;clear shift flag (bit 7)
	cmp tmp			;char to find
	beq SearchStop		;found (carry set, also)

SearchNext
	jsr MoveNext		;next entry
	bne SearchThis
	clc			;not found

SearchStop
	rts

MoveTarget
;Move to target entry
;args:
;	selected -> current entry
;	auxptr -> target entry

	lda #jumpsize		;max distance before jump
	sta tmp			;(then continue)

CheckTarget
;Check if target reached
	dec tmp
	beq JumpTarget		;too far

	lda auxptr+1		;hi-byte
	cmp selected+1
	bcc PrevTarget
	bne NextTarget

	lda auxptr		;lo-byte
	cmp selected
	bcc PrevTarget
	beq SearchStop		;(else continue)

NextTarget
;Move to forward entry
	jsr NextEntry
	jmp CheckTarget

PrevTarget
;Move to previous entry
	jsr PrevEntry
	jmp CheckTarget

JumpTarget
;Jump directly to target
	lda auxptr
	sta selected
	lda auxptr+1
	sta selected+1
	jmp RedrawList		;(then return)
}
;searchenable

!if cachemem {
PullDir
;Pull prev table from cache
	lda tblbas		;check current table pointer
	ldy tblbas+1
	cpy #>tbl
	bne +
	cmp #<tbl
	beq ++			;cache memory is empty (return zero)

+	sta tmpptr		;pointer backup
	sty tmpptr+1

	sec			;restore last entry
	sbc #entrylen
	sta tbllst
	tya			;(hi-byte)
	sbc #0
	sta tbllst+1

	ldy #tbldata-1		;restore prev table data
-	lda (tmpptr),y
	sta tblbas,y
	dey
	bpl -			;done (return not zero)
++	rts

PushDir
;Push current table into cache
	clc			;next table pointer
	lda tbllst
	adc #entrylen
	sta tmpptr
	tax			;(lo-byte)
	lda tbllst+1
	adc #0
	sta tmpptr+1

	ldy #tbldata-1		;store prev table data
-	lda tblbas,y
	sta (tmpptr),y
	dey
	bpl -

	stx tblbas		;switch to next table
	lda tmpptr+1	
	sta tblbas+1
	rts

ClearCache
;Restore table base pointer
	lda #<tbl
	sta tblbas
	lda #>tbl
	sta tblbas+1

!if printbuttons {
CacheButton
;Print cache button
	lda cachemode		;cache flag (bit 7)
	ldx #cache_btn		;button offset
	jmp ActiveButton	;(then return)

} else {
	rts
}
;printbuttons
}
;cachemem

!if printbars {
PrintLine
;Print line at specified row
;args:
;	x = row (preserved)
;consts:
;	listpos, listwidth, line_char, (bar_acol)
;calls:
;	CHROUT, PLOT
;regs:
;	a, y = not preserved

	ldy #listpos-1		;start column
	clc
	jsr PLOT		;set cursor pos

!if colorenable {
	ldy #listwidth+3	;line length (+color)
	lda #bar_acol		;set color
-	jsr CHROUT
	lda #line_char		;set char

} else {
	ldy #listwidth+2	;line length
	lda #line_char		;set char
-	jsr CHROUT
}

	dey
	bne -
	rts
}
;printbars

!if printbuttons {
StaticButton
	lda #0			;clear button flag

ActiveButton
;Check button flag and print at its predefined position 
;args:
;	a = button flag
;	    bit 7 = active
;	     "  6 = not available (fast load only)
;	x = offset to button struct (row, column, fn key, text, 0)
;consts:
;	buttontable, button_acol, active_acol
;	RVS_on, RVS_off, (shadow_acol)
;uses:
;	(TBLX)
;calls:
;	PLOT, CHROUT
;regs:
;	a, x, y = not preserved

	pha			;preserve flag
	txa			;preserve offset
	pha

	lda buttontable,x	;row
	ldy buttontable+1,x	;column
	tax
	clc
	jsr PLOT		;set cursor pos

!if target = 20 {
	dec TBLX		;prevent scrolling
}

	pla			;restore offset
	tax

!if colorenable {
	lda #button_acol	;button color
}

!if fastload {
	plp			;check not available flag (bit 6)

!if colorenable {
	php			;push back flag
	bvc +
	lda #shadow_acol	;not available (change color)

} else {
	bvs ++			;not available (do not print)
	php			;push back flag
}
}

!if colorenable {
+	jsr CHROUT		;set color
}

	lda #RVS_on		;reverse

-	jsr CHROUT
	inx
	lda buttontable+1,x	;button key
	bne -

	lda #RVS_off		;normal
	jsr CHROUT

!if colorenable {
	lda #active_acol	;active color

} else {
	lda #RVS_on		;reverse
}

	plp			;check active flag (bit 7)
	bpl +			;not active

-	jsr CHROUT
+	inx
	lda buttontable+1,x	;button text
	bne -

!if colorenable {
	rts

} else {
++	lda #RVS_off		;normal
	jmp CHROUT		;(then return)
}
}
;printbuttons

!if getsize | counterenable {
!if rightalign {
RightInt16
;Print right aligned 16-bit unsigned integer
;args:
;	a = hi-byte
;	x = lo-byte
;consts:
;	exp16, nodigits, sizelen
;uses:
;	FACHO, DECCNT, PNTR, (LCRC)
;calls:
;	FLOATC, FOUTC, PRNSTR
;regs:
;	a, x, y = not preserved

;Fixed point to floating point
!if target = 128 {
	sta LCRC		;enable BASIC ROMs (BANK 14)
}

	sta FACHO		;hi-byte
	stx FACHO+1		;lo-byte
	ldx #exp16		;exponent for 16-bit values
	sec			;not negative
	jsr FLOATC		;unsigned int -> FAC1

;Floating point to string
	ldx #nodigits+1		;set decimal point position (1 digit) 
	stx DECCNT		;(required in case of zero)
	jsr FOUTC		;FAC1 -> ASCII (result string at FBUFFR pointed by A/Y)

;Right align output
	ldx DECCNT		;decimal point position
-	cpx #nodigits+sizelen	;5 digits
	beq +
	inc PNTR		;move cursor right
	inx
	bne -			;(forced)

+	jmp PRNSTR		;(then return) (C128: custom memory config restored)
}
;rightalign
}
;getsize | counterenable

; --------------------
; - Machine specific
; --------------------

!if target != 20 {
!if joyenable {
JoySwap
;Swap joystick port
	lda joyport
	eor #$01		;swap port (bit 0)
	sta joyport

!if printbuttons {
;Print joystick button
JoyButton
	ldx #joy_btn		;button offset
	jsr StaticButton

!if target = 264 {
	lda #'1'		;port number
	clc
	adc joyport		;(0 = port #1, 1 = port #2)

} else {
	lda #'2'		;port number
	sec
	sbc joyport		;(0 = port #2, 1 = port #1)
}

	jmp CHROUT		;(then return)

} else {
	rts
}
;printbuttons
}
;joyenable
}
;target != 20

!if target = 64 {
!if fastload {
TurboSwitch
;Enable/disable fast load
	lda turbomode
	bit turbomode		;check not detected flag (bit 6)
	bvs +
	eor #$80		;swap flag (bit 7)
+	sta turbomode

!if printbuttons {
TurboButton
;Print turbo button
	lda turbomode		;fast load flag (bits 7:6)
	ldx #turbo_btn		;button offset
	jmp ActiveButton	;(then return)

} else {
	rts
}
;printbuttons
}
;fastload
}
;target = 64

!if target = 128 {
PRNINT
;Print unsigned integer in A/X
;args:
;	a = hi-byte
;	x = lo-byte
;calls:
;	LINPRT (BASIC High ROM, relies on STROUT)
;return:
;	y = printed chars

	sta LCRC		;enable BASIC ROMs (BANK 14)
	jsr LINPRT		;(then continue)

SetMem
;Set custom memory config
	lda #mmu_cfg		;custom memory config (BASIC ROMs not available)
	sta MMUCR		;MMU Configuration Register
	rts

PRNSTR
;Print string in A/Y
;args:
;	a:y -> string address
;calls:
;	STROUT (BASIC Low ROM)

	sta LCRC		;enable BASIC ROMs (BANK 14)
	jsr STROUT		;(result: zero set)
	beq SetMem		;(forced)
}
;target = 128

!if target = 128 | (target = 264 & romscroll != 0) {
SetWindow
;Setup Window boundaries
	lda #listtop		;set top/left corner
	ldx #listpos
	jsr WINTOP

	lda #listbot		;set bottom/right corner
	ldx #listend
	jmp WINBOT		;(then return)
}

!if target = 264 {
SetFnKeys
!if fnkeysave {
;Save current function key definitions
	ldx #fnkeynum*2-1
-	lda PKYBUF,x
	sta fnkeystore,x
	dex
	bpl -
}

;Set standard function key definitions (as for VIC-20/C64)
	ldx #fnkeynum-1		;number of keys to program (-1)
-	lda #1			;length
	sta PKYBUF,x
	lda PFKCHRS,x		;Table of Programmable Key character values
	sta PKYDEF,x
	dex
	bpl -
	rts

RestoreFnKeys
!if fnkeysave {
;Restore saved function key definitions
	ldx #fnkeynum*2-1
-	lda fnkeystore,x
	sta PKYBUF,x
	dex
	bpl -
	rts

} else {
;Restore default function key definitions
	ldx #PFKEND-PFKTBL-1
-	lda PFKTBL,x
	sta PKYBUF,x
	dex
	bpl -
	rts
}
}
;target = 264

; --------------------
; - Color debug
; --------------------

!if color_debug {
PrevForeground
!if target = 20 {
	ldx TXTCOL		;current foreground color
	dex			;prev color
	txa
	and #$07		;avoid multicolor mode
	sta TXTCOL
}

!if target = 64 | target = 128 {
	dec TXTCOL		;prev foreground color
}

!if target = 264 {
	ldx TXTCOL		;current foreground color
	dex			;prev color
	txa
	and #$0f		;strip luminance
	tax
	lda STDCOL,x		;get standard color from table
	sta TXTCOL
}

	rts

NextForeground
!if target = 20 {
	ldx TXTCOL		;current foreground color
	inx			;next color
	txa
	and #$07		;avoid multicolor mode
	sta TXTCOL
}

!if target = 64 | target = 128 {
	inc TXTCOL		;next foreground color
}

!if target = 264 {
	ldx TXTCOL		;current foreground color
	inx			;next color
	txa
	and #$0f		;strip luminance
	tax
	lda STDCOL,x		;get standard color from table
	sta TXTCOL
}

	rts

PrevBackground
	lda SHFLAG		;check modifier key
	and #2			;CBM key pressed
	bne PrevBorder

!if target = 20 {
	lda VICCRF		;current VIC colors
	sec
	sbc #$10		;prev background color (hi-nibble)
	sta VICCRF
}

!if target = 64 | target = 128 {
	dec BGCOL0		;prev background color
}

!if target = 264 {
	ldx BGCOL0		;current background color
	dex			;prev color
	txa
	and #$0f		;strip luminance
	tax
	lda STDCOL,x		;get standard color from table
	sta BGCOL0
}

	rts

NextBackground
	lda SHFLAG		;check modifier key
	and #2			;CBM key pressed
	bne NextBorder

!if target = 20 {
	lda VICCRF		;current VIC colors
	clc
	adc #$10		;next background color (hi-nibble)
	sta VICCRF
}

!if target = 64 | target = 128 {
	inc BGCOL0		;next background color
}

!if target = 264 {
	ldx BGCOL0		;current background color
	inx			;next color
	txa
	and #$0f		;strip luminance
	tax
	lda STDCOL,x		;get standard color from table
	sta BGCOL0
}

	rts

PrevBorder
!if target = 20 {
	lda VICCRF		;current VIC colors
	tax
	and #$f8		;mask (7:3)
	sta tmp
	dex			;prev border color
	txa
	and #$07		;mask (2:0)
	ora tmp			;merge colors
	sta VICCRF
}

!if target = 64 | target = 128 {
	dec EXTCOL		;prev border color
}

!if target = 264 {
	ldx EXTCOL		;current border color
	dex			;prev color
	txa
	and #$0f		;strip luminance
	tax
	lda STDCOL,x		;get standard color from table
	sta EXTCOL
}

	rts

NextBorder
!if target = 20 {
	lda VICCRF		;current VIC colors
	tax
	and #$f8		;mask (7:3)
	sta tmp
	inx			;next border color
	txa
	and #$07		;mask (2:0)
	ora tmp			;merge colors
	sta VICCRF
}

!if target = 64 | target = 128 {
	inc EXTCOL		;next border color
}

!if target = 264 {
	ldx EXTCOL		;current border color
	inx			;next color
	txa
	and #$0f		;strip luminance
	tax
	lda STDCOL,x		;get standard color from table
	sta EXTCOL
}

	rts
}
;color_debug

; --------------------
; - Keyboard input
; --------------------

keytable
	!by preve_key,nexte_key,prevp_key,nextp_key

!ifdef joyswap_key	!by joyswap_key
!ifdef turbo_key	!by turbo_key

;(no return routines)
no_return = * - keytable
	!by top_key,bot_key,back_key,reload_key
	!by root_key,quit_key,reset_key,load_key

!ifdef loadbas_key	!by loadbas_key		;BASIC relocated load address
!ifdef quit_key2	!by quit_key2		;(alternate)
!ifdef device_key	!by device_key
!ifdef reload_key2	!by reload_key2		;(alternate)
!ifdef sort_key		!by sort_key
!ifdef cache_key	!by cache_key
!ifdef about_key	!by about_key
!ifdef about_key2	!by about_key2		;(alternate)

keytable_len = * - keytable

; --------------------
; - Jump tables
; --------------------

;Menu routines address table (hi-byte)
jumptable_hi
	!by >PrevEntry-1,>NextEntry-1,>PrevPage-1,>NextPage-1

!ifdef joyswap_key	!by >JoySwap-1
!ifdef turbo_key	!by >TurboSwitch-1

;(no return routines)
	!by >FirstEntry-1,>LastEntry-1,>PrevDir-1,>ReloadDir-1
	!by >RootDir-1,>QuitProgram-1,>ResetMachine-1,>SelectEntry-1

!ifdef loadbas_key	!by >SelectBasic-1	;BASIC relocated load address
!ifdef quit_key2	!by >QuitProgram-1	;(alternate)
!ifdef device_key	!by >ChangeDevice-1
!ifdef reload_key2	!by >ReloadDir-1	;(alternate)
!ifdef sort_key		!by >ChangeSort-1
!ifdef cache_key	!by >CacheSwitch-1
!ifdef about_key	!by >AboutMessage-1
!ifdef about_key2	!by >AboutMessage-1	;(alternate)

;Menu routines address table (lo-byte)
jumptable_lo
	!by <PrevEntry-1,<NextEntry-1,<PrevPage-1,<NextPage-1

!ifdef joyswap_key	!by <JoySwap-1
!ifdef turbo_key	!by <TurboSwitch-1

;(no return routines)
	!by <FirstEntry-1,<LastEntry-1,<PrevDir-1,<ReloadDir-1
	!by <RootDir-1,<QuitProgram-1,<ResetMachine-1,<SelectEntry-1

!ifdef loadbas_key	!by <SelectBasic-1	;BASIC relocated load address
!ifdef quit_key2	!by <QuitProgram-1	;(alternate)
!ifdef device_key	!by <ChangeDevice-1
!ifdef reload_key2	!by <ReloadDir-1	;(alternate)
!ifdef sort_key		!by <ChangeSort-1
!ifdef cache_key	!by <CacheSwitch-1
!ifdef about_key	!by <AboutMessage-1
!ifdef about_key2	!by <AboutMessage-1	;(alternate)

; --------------------
; - User buttons
; --------------------

!if printbuttons {
buttontable
!if devchange {
device_btn = * - buttontable
	!by button_row		;row
	!by button_pos		;column
	!tx "f1",0		;key
	!tx "dev"		;text

!if colorenable {
	!by active_acol		;value color
}

	!by 0
}

!if cachemem {
cache_btn = * - buttontable
	!by button_row+1	;row
	!by button_pos		;column

!if target = 264 {
	!tx "f4"		;key

} else {
	!tx "f2"		;key
}

	!by 0
	!tx "cache",0		;text
}

!if target != 20 {
!if fastload {
turbo_btn = * - buttontable
	!by button_row		;row
	!by slot_1		;column
	!tx "f3",0		;key
	!tx "turbo",0		;text

} else {
reload_btn = * - buttontable
	!by button_row		;row
	!by slot_1		;column

!if target = 264 {
	!tx "f2"		;key

} else {
	!tx "f3"		;key
}

	!by 0
	!tx "renew",0		;text	
}

!if joyenable {
joy_btn = * - buttontable
	!by button_row+1	;row
	!by slot_1		;column

!if target = 264 {
	!tx "f5"		;key

} else {
	!tx "f4"		;key
}

	!by 0
	!tx "joy"		;text

!if colorenable {
	!by active_acol		;value color
}

	!by '#',0
}
}
;target != 20

!if sortenable {
sort_btn = * - buttontable
	!by button_row		;row
	!by slot_2		;column

!if target = 264 {
	!tx "f3"		;key

} else {
	!tx "f5"		;key
}

	!by 0
	!tx "sort",0		;text
}

quit_btn = * - buttontable
	!by button_row+1	;row
	!by slot_2		;column
	!tx "f6",0		;key
	!tx "quit",0		;text

!if aboutenable {
about_btn = * - buttontable
	!by button_row		;row
	!by slot_3		;column

!if target = 264 {
	!tx "help",0		;key (no text)

} else {
	!tx "f7",0		;key
	!tx "about"		;text
}

	!by 0
}

reset_btn = * - buttontable
	!by button_row+1	;row
	!by slot_3		;column

!if target = 264 {
	!tx "f7"		;key

} else {
	!tx "f8"		;key
}

	!by 0
	!tx "reset",0		;text
}
;printbuttons

; --------------------
; - Other tables
; --------------------

!if joyenable {
;Joystick to keyboard conversion table
joytable
!if target = 20 {
	!by preve_key,nexte_key,prevp_key,load_key,0,nextp_key
}

!if target = 64 | target = 128 {
	!by load_key,nextp_key,prevp_key,nexte_key,preve_key
}

!if target = 264 {
	!by load_key,load_key,0,0,nextp_key,prevp_key,nexte_key,preve_key
}

joytable_len = * - joytable
}

;List of file types/extensions
ext_table

;CBM file types
cbm_types = * - ext_table	;offset
	!tx "dir"		;$00
	!tx "del"		;$03
	!tx "seq"		;$06
	!tx "prg"		;$09
	!tx "usr"		;$0c
	!tx "rel"		;$0f
	!tx "cbm"		;$12
	!tx "???"		;$15 (unknown)
	!by 0			;end of list

;DOS extensions
dos_ext = * - ext_table		;offset
	!tx "d64"		;$19
	!tx "d71"		;$1c
	!tx "d81"		;$1f
	!tx "m2i"		;$22
	!tx "d41"		;$25

dnp_ext = * - ext_table
	!tx "dnp"		;$28

!if tapesupport {
tap_ext = * - ext_table
	!tx "tap"		;$2b
}

	!by 0			;end of list

;DOS commands
dircmd
	!tx '$'			;dir command

dircmd_len = * - dircmd

backcmd
	!tx "cd",left_arrow	;back command

backcmd_len = * - backcmd

rootcmd				;root command
!if vice_debug {
	!tx "cd:/"		;VICE Virtual File System

} else {
	!tx "cd//"		;SD2IEC (compliant with CMD drives)
}

rootcmd_len = * - rootcmd

!if target = 128 | target = 264 {
;LOAD and RUN commands
loadrun
	!tx "load",RETURN_char
	!tx "run",RETURN_char

loadrun_len = * - loadrun
}

; --------------------
; - Strings
; --------------------

!if statusenable {
waitmsg_txt
!if colorenable {
	!by status_acol		;color
}

	!tx "loading...",0
}
;statusenable

!if aboutenable {
proginfo_txt
!if colorenable {
	!by about_acol		;color
}

	!tx program_name
	!by ' '

!if target != 20 {
	!by 'v'
}

	!tx version


!if vicmemcfg {
!if colorenable {
	!by active_acol		;color
}

	!tx " +",'0' + vicmemcfg,"k"

} else {
	!tx " (c) ",program_date
	!tx " by ",author
}

proginfo_len = * - proginfo_txt - colorenable*(1+(vicmemcfg != 0))
proginfo_pos = (screencols - proginfo_len)/2

	!by 0
}
;aboutenable

; --------------------
; - Dir table
; --------------------

;Dir table base address
tbl

;table header
;	prev table data		6	with cachemem enabled (must be consecutive locations)
;		table pointer	2
;		selected entry	2	(optional)
;		current row	1	(optional)
;		sort mode	1	(optional)
;
;	disk info		21	with getdiskinfo enabled
;		disk name	16
;		 "   id		5	(optional)
;
;fixed entries
;	back/exit	19	"cd<-"	always present
;	root dir	19	"cd//"	(optional)
;
;table entry
;	name		16	(with zero padding)
;	type		1	offset to CBM file types list
;	size		2	lo-byte/hi-byte (optional, not available on VIC-20)

ml_size = prg_end-ml_start		;M/L code size
prg_size = prg_end-prg_start+2		;program size (+load address)

;End of program
prg_end
