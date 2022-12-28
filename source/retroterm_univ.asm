;////////////////////////////////////////////////////////////////////////////////////////////
; BBS PETSCII compatible terminal, RS232 with tcpser/BBSServer or wifi with zimodem firmware
; Supports TURBO56K v0.6 protocol at 57600 bps, using TX, RX and RTS
;////////////////////////////////////////////////////////////////////////////////////////////
; 
; Versions / corrections
; ========================
; 02-02-2020	First version
; 12-02-2021	ACIA & 56K Userport source codes unified
; 16-08-2021	Version 0.13 Released
; 25-08-2021	Started translation of all code comments to English
; 29-08-2021	Comment translation complete, fixed visual bug on split screen
; 01-09-2021	More robust exit to BASIC. Now gives compile error if _HARDTYPE_ is set
;				wrong - Exomize with SFX BASIC option.
; 03-10-2021	Split screen visual bug gone for good now(?), disabled interrupts on
;				command $A2, prevents crash if ie reconnecting to bbs with split screen
;				active.
; 04-10-2021	Command $A3 implemented, CmdParTable slightly reworked
; 20-12-2021	Command $85 implemented
; 11-04-2022	Fixed text output bug when the C64 has an old Kernal revision, Turbo56K
;				version changed to 0.6
; 12-04-2022	Command $85 tested working, further tests to find improved register write
;				sequences for specific SID play routines are needed.
;				Intro screen exits after ~2seconds.
;				Full digi-boost for 8580
;========================================================================================

 !ifndef _INTRO_{
	 _INTRO_ = 1 		;Compile with intro screen
 }


 !ifndef _HARDTYPE_{
    _HARDTYPE_ = 56     ;Hardware type to compile for:
                        ; 232 = Turbo232
                        ; 38  = Swiftlink
                        ; 56 = Userport 56K
 }

 !if _HARDTYPE_ != 56 {
	 !if _HARDTYPE_ != 232 {
		 !if _HARDTYPE_ != 38 {
			 !serious "UNKNOWN HARDWARE TYPE"
		 }
	 }

 }



;	_SYNTH_		= 1		; Uncomment to add speech synth (Microsint) hard support

; Variables
!addr {
	TXBYTE		= $C003	; Byte to be transmitted
	RXBYTE		= $C004	; Last received byte
	CMDFlags	= $C005	; Command flags
						; Bit 7 (N): 1 = Last received byte is CMDON ($ff); 0 = Normal operation
						; Bit 6 (V): 1 = Receive N bytes as parameters and wait for the command to complete; 0 = Normal operation
						; Bits 3-0: Parameter counter for bit-6
	TEMPCNT1	= $C006	; Temporal counter 1
	BYTECNT		= $C007	; 16-bit block transfer remaining bytes to receive counter
						; --$C008
	BORDERCLR	= $C009	; Current screen border color backup
	FLAGS1		= $C00A	; Status flags
						; Bit 7 (N): 1 = Command mode; 0 = Normal mode
						; Bit 6 (V): 1 = Last byte should be redirected to the voice synth; 0 = Last byte is meant for the terminal
						; Bit 4	   : 1 = Split screen enabled
						; Bit 3    : 1 = Cursor blink enabled; 0 = Cursor blink disabled
						; Bit 2    : 1 = Microsint enabled; 0 = Microsint disabled / not found 
						; Bit 1    : 1 = Terminal is starting up; 0 = Normal operation
						; Bit 0    : 1 = RUN/STOP pressed, returning to BASIC
	SPBUFFERCNT = $C00B	; Voice buffer byte counter
	PRBUFFERCNT = $C00C	; Print buffer byte counter
	STREAMFLAG	= $C00D	; SID streaming state flag 
	SPINDEXOUT	= $C00E	; Index to the voice buffer byte to send next
	SPINDEXIN	= $C00F	; Index to the first free byte in the voice buffer
	PRINDEXOUT	= $C010	; Index to the print buffer byte to output next
	PRINDEXIN	= $C011	; Index to the first free byte in the print buffer
	RXINDEXOUT	= $C012	; Index to the reception buffer byte to process next
	RXINDEXIN	= $C013	; Index to the first free byte in the reception buffer
	SPBYTE		= $C014	; Byte to be sent to the voice synth
	PRBYTE		= $C015	; Byte to be printed on screen
	TIMER1		= $C016	; Timer decremented on each interrupt call
	CRSRTIMER	= $C017	; Cursor blink timer, decremented on each interrupt call
	PRSPEED		= $C018	; Text print delay (0:no delay)
	CRSRCHAR	= $C019	; Character currently under the cursor
	STREAMBM	= $C01A	; 4 bytes containing a bitmap of used SID registers used in the current frame (streaming)
						; -- $C01B
						; -- $C01C
						; -- $C01D
	STREAMCNT	= $C01E	; Size of the SID streaming packet received
	CRSRCOLOR   = $C01F ; Color of the character under the cursor
	SNDSTATUS	= $C020	; Key beep status:
						; 0 = Normal
						; 1 = Mute
						; 128 = Newlines only
	WTOP		= $C021	; Text window top limit
	WBOTTOM		= $C022 ; Text window bottom limit
	TEMPCNT2	= $C023	; Temporal counter 2
	BLOCKPTR	= $C024	; Memory pointer for the block transfer command
						; $C025
	SPLITRST	= $C026	; Split creen raster line
	VERSION		= $C027	; 6 byte buffer for the voice synth version ($C027-$C02C)

; Constants

	SIDREG = $D400			; SID Base
	VICREG = $D000			; VIC II Base
	PLOT   = $FFF0			; PLOT Kernal routine
	CHROUT = $FFD2			; CHROUT Kernal routine
	STROUT = $AB1E			; STROUT BASIC ROM routine
	COLORMEM = $D800		; Color RAM
	BUFFER = $0801			; Dummy constant for self-modifying code
	CURRLINEPTR = $D1		; Current screen line pointer (zero page)
	CURRCOLUMN = $D3		; Current screen column (zero page)
	CURRLINE = $D6		    ; Current screen line (zero page)
	PrBuffer = ENDOFCODE	; Print buffer address
	SpBuffer = PrBuffer+256	; Speach buffer address
	T232BASE = $DE00		; Turbo232 base
	T232DATA = T232BASE+0	; Turbo232 data register
	T232STATUS = T232BASE+1	; Turbo232 status register
	T232COMMAND = T232BASE+2; Turbo232 command register
	T232CONTROL = T232BASE+3; Turbo232 control register
	T232ENSPEED = T232BASE+7; Turbo232 high speed register

; Buffers
	SIDREGS = $033C			; SID streaming register buffer 
}
	MAXCMD = $B5			; Highest command implemented


; Output file names, this will trigger a warning when compiling thru Makefile
!if _HARDTYPE_ = 232 {
	!to "rt_232_v0.14.prg", cbm
	!sl "labels_232_v0.14.txt"
}
!if _HARDTYPE_ = 38 {
    !to "rt_sl_v0.14.prg", cbm
	!sl "labels_sl_v0.14.txt"
}
!if _HARDTYPE_ = 56 {
	!to "rt_u_v0.14.prg", cbm
	!sl "labels_u_v0.14.txt"
}

* = $0801

;///// Macros /////
!macro DisKernal {	; Disable Kernal
	LDX #$35
	STX $01
}

!macro EnKernal{	; Enable Kernal
	LDA #$37
	STA $01
}

!macro _Version_{	; Insert version number string
	!src "version.asm"
}

;2021 SYS 2062
!byte	$0c, $08, $e5, $07, $9e, $20, $32, $30, $36, $32, $00, $00, $00

_START
!if _INTRO_ > 0 {
		LDA $D018
		ORA #$02
		STA $D018
		LDX screen_001	;#$01
		STX $D020
		LDX screen_001+1
		STX $D021
		DEX
-		LDA screen_001+2,X
		STA $0400,X
		LDA screen_001+2+1000,X			;color_001,X
		STA $D800,X
		LDA screen_001+$102,X
		STA $0500,X
		LDA screen_001+2+1000+$100,X	;color_001+$100,X
		STA $D900,X
		LDA screen_001+$202,X
		STA $0600,X
		LDA screen_001+2+1000+$200,X	;color_001+$200,X
		STA $DA00,X
		LDA screen_001+$302,X
		STA $0700,X
		LDA screen_001+2+1000+$300,X	;color_001+$300,X
		STA $DB00,X
		INX
		BNE -
!ifdef _SPACE_{
-		JSR $F142
		BEQ -
} else {
Delay16
		LDA #$24
		STA	DELAYTIME
		LDA	#$08		; Set CIA2 for One-shot mode
		STA	$DD0E
DlyStart2
		LDA	#$F4		; Set Timer A to $F424 (62500 us)
		STA	$DD05
		LDA	#$24
		STA	$DD04
		LDA	#$09		; Start Timer A
		STA	$DD0E
DlyLoop2
		LDA	$DD0D
		AND	#$01
		BEQ	DlyLoop2
DlyNext2
		DEC	DELAYTIME	; If 500 us have ellapsed, decrement DELAYTIME
		BNE	DlyStart2	; Loop to DlyStart2 if DELAYTIME hasnt reach 0
}
}

; Use ROM mem move
; First loop:	move second section to $E000-
; Second loop:	move first section to $C000-

		LDX #$11
.c0		LDY #$08		;<-
.c2		LDA _DATA1,X	;<-
		BEQ .c1
		STA $0058,Y
.c1		DEX				;<-
		DEY
		BPL .c2
		TXA
		PHA
		JSR $A3BB		; >>Open space in memory
		PLA
		TAX
		BPL .c0
!if _HARDTYPE_ = 56{
		JSR udetect
}
		LDA #$00
		STA $0801
		STA $0802
		JMP CODESTART

!if _HARDTYPE_ = 56{
; Detect C64 model
udetect:
	sei
	lda #$FE
	and $DC0E
	sta $DC0E
	lda #$38
	sta $DC04
	lda #$4F
	sta $DC05
	lda $0314
	sta TIRQ
	lda #<MIRQ
	sta $0314
	lda $0315
	sta TIRQ+1
	lda #>MIRQ
	sta $0315
;Wait for raster line zero
.z1	lda $D012
	bne .z1
	lda $D011
	and #$80
	bne .z1
	sta Flg		;Clear test flag
	inc $DC0E	;Start timer
	cli
.f1	ldy Flg
	beq .f1		;Wait for test flag
	lda Ras
.s1	cmp #$0B	;PAL-B/G?
	bne .n0
	;yes, copy PAL-B TurboRX routine
	INC $D020
	LDX #$08

--	LDY #$08		;<-
-	LDA _DATA2,X	;<-
	BEQ +
	STA $0058,Y
+	DEX				;<-
	DEY
	BPL -
	TXA
	PHA
	JSR $A3BB		; >>Open space in memory
	PLA
	TAX
	BPL --
;
.n0	sei
	lda TIRQ	;Back to normal
	sta $0314
	lda TIRQ+1
	sta $0315
	lda #$FE
	and $DC0E
	sta $DC0E
	lda $02A6
	beq .n1
	lda #$25
	sta $DC04
	lda #$40
	bne .n2
.n1	lda #$95
	sta $DC04
	lda #$42
.n2	sta $DC05
	inc $DC0E
	cli
	rts

MIRQ
	lda $DC0D
	cmp #$81
	bne .p1
	ldx Flg
	bne .p1
	inc Flg
	lda $D012
	sta Ras
	;inc V_BORDER
.p1	jmp $ea81

TIRQ	!byte 00,00
Flg	!byte 00
Ras	!byte 00	;PALG 11 NTSC 50 NTSCold 56 PALN 1

_DATA2		;Memory move parameters
!word	PALEND, _PALEND_
!byte	$00, $00, $00
!word	_PALSTART_
}
DELAYTIME !byte 00

;Intro screen
!if _INTRO_ > 0 {
!src "intro_sc.asm"
; Uncomment the next lines for auto update version number on the intro screen
;_tmp = *	; save PC
;* = screen_001+2+(22*40)+24	; Line 22, column 24
;+_Version_	; Overwrite version string in Intro Screen -- Triggers Warning!
;* = _tmp	; recover PC
}

_DATA1					; Memory move parameters

!word	ENDOFCODE, _ENDOFCODE_
!byte	$00, $00, $00
!word	_CODESTART_

!word	EXTRAEND, _EXTRAEND_
!byte	$00, $00, $00
!word	_EXTRACODE_

_CODESTART_
!pseudopc $C000 {
	;*= $C000

;///////////////////////////////////////////////////////////////////////////////////
; START
;///////////////////////////////////////////////////////////////////////////////////

CODESTART:
	JMP	MainPrg		; Program starts on MainPrg

!fill $2A, $00		; Space for variables
	;*= $C02E

;///////////////////////////////////////////////////////////////////////////////////
; SUBROUTINES
;///////////////////////////////////////////////////////////////////////////////////

!if _HARDTYPE_ != 56 {
;///////////////////////////////////////////////////////////////////////////////////
; SendByte, .A: byte to be sent

SendByte
	RTS			; 2


;///////////////////////////////////////////////////////////////////////////////////
; ReadByte, receives a byte from rs232 and stores it in RXBYTE
;---------------------------------------------------------------------------------------

ReadByte
	LDA	FLAGS1		; Ignore reception if the terminal is starting up
	AND	#%00000010
	BNE	CancelRX
 !if _HARDTYPE_ = 232 {
 	LDA	#$00		; 2 Timer A - Wait 86uS
 }
 !if _HARDTYPE_ = 38 {
     LDA #$00        ; 2 Timer A - Wait 130uS
 }
 	STA	$DD05		; 4
 !if _HARDTYPE_ = 232 {
 	LDA	#$56	; 2
 }
 !if _HARDTYPE_ = 38 {
     LDA #$82
 }
 	STA	$DD04		; 4
 	LDA	$DD0D		; 4 Clear interrupt bits(CIA2: NMI)
 	LDA	#$19		; 2 Force load and start Timer A
 	STA	$DD0E		; 4
EnRTS
	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, enables reception), no rx irq, rx enabled (dtr=0)
	STA	T232COMMAND
WaitRX1
	 LDA	$DD0D		; 4 Timer A not elapsed yet
	 AND	#$01		; 2
	 BEQ	WaitRX1		; 2 back to WaitRX1 and waits to disable RTS at half a byte
	;NOP
	;NOP
	;NOP
	;NOP

DisRTS
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, disables reception), no rx irq, rx enabled (dtr=0)
	STA	T232COMMAND
!if _HARDTYPE_ = 232{
	LDY #$0E		; 2
}
!if _HARDTYPE_ = 38{
	LDY #$14		; 2
}
WaitRX2
	LDA	T232STATUS	; 4 Byte received?
	AND	#%00001000	; 2
	BNE	Received	; 3 
	DEY				; 2
	BNE WaitRX2		; 3
;	LDA	$DD0D		; 4 if Timer A not elapsed
;	AND	#$01		; 2
;	BEQ	WaitRX2		; 2 back to WaitRX2
CancelRX
	LDA	#$00		; 2 Store 0 in RXBYTE
	STA	RXBYTE		; 4
	CLC			; 2 CARRY = 0 (no byte received)
	RTS			; 6
Received
	LDA	T232DATA	; 4 .A= Received byte
	STA	RXBYTE		; stored in RXBYTE
	SEC			; 2 CARRY = 1 (byte received)
	RTS			; 6
}

!if _HARDTYPE_ != 56 {
;//////////////////////////////////////////////////////////////////////////////////////////
; TurboRX, receives a byte stream and plays it as nibbles through the SID volume register
;------------------------------------------------------------------------------------------

TurboRX
	LDA	#$00		; Set keyboard matrix for direct read
	STA	$DC03		; port b ddr (input)
	LDX	#$FF
	STX	$DC02		; port a ddr (output)
	STA	$DC00

	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, enable reception), no rx irq, rx enabled (dtr=0)
	STA	T232COMMAND
TurboLoop
	TXA				; 2 
	AND	#$0F		; 2
	STA	SIDREG + 24	; 4 Write the first sample to the SID
	STA	$D020		; 4 <-
;	LDA	#$00		; 2 Set Timer A to  75 ($004B) microseconds
;	STA	$DD05		; 4
;!if _HARDTYPE_ = 232 {
;	LDA	#$4B		; 2
;}
;!if _HARDTYPE_ = 38 {
;DD04_1:
;    LDA #$60
;}
;	STA	$DD04		; 4
;	LDA	$DD0D		; 4 Clear interrupt register (CIA2: NMI)
;	LDA	#$19		; 2 Force load and start Timer A
;	STA	$DD0E		; 4 <-26 total cycles

	TXA				; 2 Prepare second sample
	ROR				; 2
	ROR				; 2
	ROR				; 2
	ROR				; 2
	AND	#$0F		; 2
	TAX				; 2	<-18 cycles until here
TRXWait1
; Wait 86 - 16 - 18 cycles for 11520Khz
; Wait 130 - 16 - 18 cycles for 7680KHz
!if _HARDTYPE_ = 232 {
	LDY #$0A	; 2
}
!if _HARDTYPE_ = 38 {
	LDY #$12
}
-	DEY			; 2
	BNE -		

;	LDA	$DD0D		; 4 If Timer A didnt elapse
;	AND	#$01		; 2
;	BEQ	TRXWait1	; 2 go back to TRXWait1 and wiat
	STX	SIDREG + 24	; 4 Write second sample
	STX	$D020		; 4
TRXWait2
-
	LDA	T232STATUS	; 4 Wait for a character
	AND	#%00001000	; 2
	BEQ	-			; 3
	LDA	T232DATA	; 4 .A = Received character
	TAX				; 2

	BIT $DC01		; 4 Key press?
	BMI +			; 3
	LDA #$FF		; Yes, send $FF
	STA T232DATA
+	BNE	TurboLoop	; 3

TurboExit
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, reception disabled), no rx irq, rx enabled (dtr=0)
	STA	T232COMMAND
	RTS
}

;///////////////////////////////////////////////////////////////////////////////////
; Main Program
;///////////////////////////////////////////////////////////////////////////////////

MainPrg
	SEI				; Disable IRQs
	LDA	#%01111111
	STA	$DC0D		; "Switch off" interrupts signals from CIA-1
	STA	$DD0D		; "Switch off" interrupts signals from CIA-2
	LDA	#$00
	STA	CMDFlags	; Reset Command flags
	LDA	$DC0D		; Clear interrupt register (CIA1: IRQ)
	LDA	$DD0D		; (CIA2: NMI)
!if _HARDTYPE_ = 56 {
!ifdef _SYNTH_{
	LDA	#<StartBit_	; Set the NMI vector to StartBit_ (microsint detection)
	STA	$FFFA		;$0318
	LDA	#>StartBit_
	STA	$FFFB		;$0319
}
}
!if _HARDTYPE_ = 56 {
	LDA	#$06		; Set PB1 (RTS) and PB2 (DTR) as outputs
	STA	$DD03
	LDA #$02
	STA	$DD01		; 4
} ;else {
;	LDA	#$04		; 2 Set RTS to 1 (and DTR to 0)
;}

	LDA	#$08		; Set Timer A mode to one-shot 
	STA	$DD0E
	LDA	#$00
	STA	$D020
	STA	$D021

	JSR InitSID

!if _HARDTYPE_ != 56{
	LDA	#%00000010	; no parity, no echo, no tx irq (rts=1), no rx irq, rx disabled (dtr=1)
	STA	T232COMMAND
!if _HARDTYPE_ = 232 {
	LDA	#%00000000	; 1 stop bit, 8 data bits, enhanced speed
}
!if _HARDTYPE_ = 38 {
    LDA #%00011111	; 1 stop bit, 8 data bits, baud rate generator, 38400 bps
}
	STA	T232CONTROL
!if _HARDTYPE_ = 232 {
	LDA	#%00000010	; 57600 bps
	STA	T232ENSPEED
}
}
	LDX	#0

	LDA	#$00		; Init the voice, print and reception buffers
	STA	SPINDEXOUT
	STA	SPINDEXIN
	STA	SPBUFFERCNT
	STA	PRINDEXOUT
	STA	PRINDEXIN
	STA	PRBUFFERCNT
	STA	RXINDEXOUT
	STA	RXINDEXIN
	STA STREAMFLAG
	STA WTOP
	STA SNDSTATUS
	LDA	#%00001010	; Initializing the terminal, DTR disable, cursor enabled
	STA	FLAGS1
	LDA	#32			; Writes an space as initial character under the cursor
	STA	CRSRCHAR
	LDA	#$01		; Init BLOCKPTR to $0801 (BASIC text area)
	STA	BLOCKPTR
	LDA	#$08
	STA	BLOCKPTR
	LDA #25
	STA WBOTTOM

!ifdef _SYNTH_ {
!if _HARDTYPE_ = 56 {
;///////////////////////////////////////////////////////////////////////////////////
; Microsint detection
;///////////////////////////////////////////////////////////////////////////////////

ChkUSint
;JMP	ConfigVIC
	LDA #$00		; Disable raster interrupt
	STA $D01A
	LDA	$D011		; Disable VIC II screen
	AND	#$EF
	STA	$D011

;	LDA	#$00		; Enables de voice synth on ReadByte
;	STA	RBDTR

	+DisKernal

	LDA	#0			; Send NUL
	JSR	SendByte
	LDA	#0			; Send NUL
	JSR	SendByte
	LDA	#27			; Send ESC
	JSR	USintDet	; Receive reply (if echo is disabled, there'll be no reply)
	LDA	#"V"		; Send "V"
	JSR	USintDet	; Receive reply, if it is not "H", continues on ConfigVIC
	LDA	RXBYTE
	CMP	#$48
	BNE	ConfigVIC
	STA	VERSION		; Stores "H" in VERSION
;	JSR	CHROUT
	LDA	#0			; Sends NUL
	JSR	USintDet
	LDA	RXBYTE		; Stores the reply in VERSION+1 ("R"|"D")
	STA	VERSION+1
;	JSR	CHROUT
	LDA	#0			; Sends NUL
	JSR	USintDet
	LDA	RXBYTE		; Stores the reply in VERSION+2 ("4"|"6"|"1")
	STA	VERSION+2
;	TAX
;	LDA	#$00
;	JSR	$BDCD
	LDA	#0			; Sends NUL
	JSR	USintDet
	LDA	RXBYTE		; Stores the reply in VERSION+3 (version 1)
	STA	VERSION+3
;	TAX
;	LDA	#$00
;	JSR	$BDCD
	LDA	#0			; Sends NUL
	JSR	USintDet
	LDA	RXBYTE		; Stores the reply in VERSION+4 (version 2)
	STA	VERSION+4
;	TAX
;	LDA	#$00
;	JSR	$BDCD
	LDA	#0			; Sends NUL
	JSR	USintDet
	LDA	RXBYTE		; Stores the reply in VERSION+5 (version 3)
	STA	VERSION+5
	LDA	#127		; Sends DEL (empties the buffer)
	JSR	SendByte
	LDA	#0			; Envia el caracter NUL
	JSR	SendByte
	LDA	FLAGS1		; Set flag enabling microsint voice messages
	ORA	#%00000100
	STA	FLAGS1
}
}

ConfigVIC
!if _HARDTYPE_ = 56 {
	+EnKernal
	LDA	#<StartBit	; Sets the NMI vector to StartBit (ReadByte)
	STA	$FFFA		;$0318
	LDA	#>StartBit
	STA $FFFB		;$0319
	LDA	#$04		; Sets RTS to 0 and DTR to 1, to enable RS232
	STA	$DD01
}
	LDA	#$10		; Enables VIC II screen
	ORA	$D011
	AND	#%01111111	; Clear most significant bit in VIC's raster register
	STA	$D011
	LDA	#251		; Set the raster line number where interrupt should occur
	STA	$D012
	LDA	#<Irq		; Set the interrupt vector to point to interrupt service routine below
	;STA $FFFE
	STA	$0314
	LDA	#>Irq
	;STA $FFFF
	STA	$0315
	LDA	#%00000001	; Enable raster interrupt signals from VIC
	STA	$D01A

	CLI
!ifdef _SYNTH_{
	LDA	FLAGS1		; If voice messages are disabled, continues on +
	AND	#%00000100
	BEQ	+
	LDX	#<VersionSp	; Pointer to VersionSp string
	;STA	SCopyLoop + 1
	LDY	#>VersionSp
	;STA	SCopyLoop + 2
	JSR	SpeakText	; and it's added to the voice buffer
}
+	LDX	#<Version	; Pointer to the Version string (RETROTERM...)
	;STA	AddTLoop + 1
	LDY	#>Version
	;STA	AddTLoop + 2
	JSR	AddText		; and it's added to the print buffer
	LDA	#1			; Speed: 1 character per frame
	STA	PRSPEED
	STA	TIMER1
	;JSR	StopCRSR
	JSR	PrintBuffer	; Process the print buffer
	;JSR	StartCRSR

	JSR WaitNoChar	; Waits for one frame and turns off the sound

	LDA	#100		; Wait 2 seconds
	STA	PRSPEED
	STA	TIMER1
	LDA	#0			; Inits the cursor timer so it starts in the ON state
	STA	CRSRTIMER
Wait1	
	JSR	BlinkCRSR	; Process the cursor blinking
	LDA	TIMER1
	BNE	Wait1
!ifdef _SYNTH_{
	LDA	FLAGS1		; If voice messages are disabled, continues on NoSpeech
	AND	#%00000100
	BNE	+
	JMP	NoSpeech
+	LDX	#<Sp00		; Pointer to the Sp00 string
	;STA	SCopyLoop + 1
	LDY	#>Sp00
	;STA	SCopyLoop + 2
	JSR	SpeakText	; and it's added to the voice buffer
	LDX	#<Msg00		; Pointer to the Msg00 string (MICROSINT DETECTADO)
	;STA	AddTLoop + 1
	LDY	#>Msg00
	;STA	AddTLoop + 2
	JSR	AddText		; and it's added to the print buffer
	LDA	#1			; Speed: 1 character per frame
	STA	PRSPEED
	STA	TIMER1
	;JSR	StopCRSR
	JSR	PrintBuffer	; Process the print buffer
	;JSR	StartCRSR

	JSR WaitNoChar	; Waits for one frame and turns off the sound

	LDA	#75			; Wait 1.5 seconds
	STA	PRSPEED
	STA	TIMER1
	LDA	#0			; Inits the cursor timer so it starts in the ON state
	STA	CRSRTIMER
Wait2	
	JSR	BlinkCRSR	; Process the cursor blinking
	LDA	TIMER1
	BNE	Wait2

	LDX	#<Sp01		; Pointer to the Sp01 string
	;STA	SCopyLoop + 1
	LDY	#>Sp01
	;STA	SCopyLoop + 2
	JSR	SpeakText	; and it's added to the voice buffer
	LDX	#<Msg01		; Pointer to the Msg01 string
	;STA	AddTLoop + 1
	LDY	#>Msg01
	;STA	AddTLoop + 2
	JSR	AddText		; and it's added to the print buffer
	LDA	#1			; Speed: 1 character per frame
	STA	PRSPEED
	STA	TIMER1
	;JSR	StopCRSR
	JSR	PrintBuffer	; Process the print buffer
	;JSR	StartCRSR

	JSR WaitNoChar	; Waits for one frame and turns off the sound

	LDA	#25			; Waits 1/2 a second
	STA	PRSPEED
	STA	TIMER1
	LDA	#0			; Inits the cursor timer so it starts in the ON state
	STA	CRSRTIMER
Wait3
	JSR	BlinkCRSR	; Process the cursor blinking
	LDA	TIMER1
	BNE	Wait3
}

NoSpeech

	+DisKernal
	SEI
	JSR SpriteSetup
	LDA $07F8
	EOR #$03
	STA $07F8
	CLI
	+EnKernal

	LDX	#<Msg06		; Pointer to the Msg06 string (credits/Turbo56K version)
	;STA	AddTLoop + 1
	LDY	#>Msg06
	;STA	AddTLoop + 2
	JSR	AddText		; and it's added to the print buffer
	LDA	#0			; Speed: max
	STA	PRSPEED
	STA	TIMER1
	;JSR	StopCRSR
	JSR	PrintBuffer	; Process the print buffer
	;JSR	StartCRSR

	;LDX	#<Msg07		; Pointer to the Msg07 string (info@pastbytes.com)
	;STA	AddTLoop + 1
	;LDY	#>Msg07
	;STA	AddTLoop + 2
	;JSR	AddText		; and it's added to the print buffer
	;LDA	#0			; Speed: max
	;STA	PRSPEED
	;STA	TIMER1
	;JSR	StopCRSR
	;JSR	PrintBuffer	; Process the print buffer
	;JSR	StartCRSR

	;LDA	#$00		; From now on there's no delay
	;STA	PRSPEED
	;STA	TIMER1
	LDA	#0			; Inits the cursor timer so it starts in the ON state
	STA	CRSRTIMER
	LDA	FLAGS1		; The terminal has finished its initialization
	AND	#%11111101
	STA	FLAGS1
PrintRX
	LDA	PRBUFFERCNT	; If the print buffer is empty, continue on Blink
	BNE +
	LDA TIMER1		; If TIMER1 = 0 turn off the print beep
	BNE +
	JSR NoChar
	BNE Blink
	;BEQ	Blink
+	LDA	TIMER1		; If we're not printing (TIMER1<>0), continue on Blink
	BNE	Blink
	;JSR	StopCRSR
	JSR NoChar
	JSR	PrintBuffer	; Process the print buffer
	;JSR	StartCRSR
	INC TIMER1
Blink
	LDA	#%00000001	; Check if we must return to BASIC
	AND	FLAGS1
	BEQ	+
	JMP	ExitPrg		; if so, continue on ExitPrg to exit the program
+	JSR	BlinkCRSR	; else, process the cursor blink
	JMP	PrintRX		; and return to PrintRX

;///////////////////////////////////////////////////////////////////////////////////
; Init the SID
;///////////////////////////////////////////////////////////////////////////////////
InitSID
	+DisKernal
	JSR RTTReset	; Reset register order for SID streaming
	+EnKernal
	LDA	#$00		; Inits the SID
	STA	SIDREG+24	; Volumen = 0
	;LDA	#$00	; Attack/decay = 0/0 on channels 1 & 2
	STA	SIDREG+5
	STA	SIDREG+12
	STA	SIDREG+13	; Sustain = 0 / Release = 0 on channels 2 (click) & 3 (Bell)
	STA	SIDREG+20
	STA SIDREG+23	; Filter disabled
	STA SIDREG+14
	LDA #$A0
	STA SIDREG+15	; Channel 3 (Bell) = ~2497 Hz
	LDA #09			; Attack/decay = 0/9 on channel 3
	STA	SIDREG+19
	LDA	#$F0		; Sustain = 15 / Release = 0 on channel 1 (beeps)
	STA	SIDREG+6
	LDA	#$10		; Triangle wave on channel 1 / gate off
	STA	SIDREG+4
	LDA	#$80		; Noise on channel 2 / gate off
	STA	SIDREG+11
	LDA	#$14		; Triangle wave on canal 3 (Bell) / ring modulation / gate off
	STA	SIDREG+18
	LDA	#$0F		; Channel 2 (click) frequency = 229,758 Hz
	STA	SIDREG+7
	STA	SIDREG+8
	;LDA	#$0F		; Volumen = 15
	STA	SIDREG+24
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Process the cursor blink
;///////////////////////////////////////////////////////////////////////////////////

BlinkCRSR
	LDA FLAGS1
	AND #%00001000
	BEQ BlinkEnd
	LDA	CRSRTIMER	; If it isn't the momento to reverse the cursor (CRSRTIMER<>0), continue on BlinkEnd
	BNE	BlinkEnd
	LDA	#20		; If CRSRTIMER got to 0, we renew it
	STA	CRSRTIMER
    LDX CRSRCOLOR
	LDY	CURRCOLUMN
	LDA	#%10000000	; and reverse the character under the cursor
	EOR	(CURRLINEPTR), Y
	STA	(CURRLINEPTR), Y
    BPL +
    LDX $0286
+   TXA
    STA ($F3),Y     ; Present color
BlinkEnd
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Disables the cursor, and restores the character under it
;///////////////////////////////////////////////////////////////////////////////////

StopCRSR
	LDY	CURRCOLUMN
	LDA	CRSRCHAR
	STA	(CURRLINEPTR), Y
    LDA CRSRCOLOR
    STA ($F3),Y
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Enables the cursor, and saves the character under it
;///////////////////////////////////////////////////////////////////////////////////

StartCRSR
	LDY	CURRCOLUMN
	LDA	(CURRLINEPTR), Y
	STA	CRSRCHAR
    LDA ($F3),Y
    STA CRSRCOLOR
	LDA	#0		; Inits the cursor timer so it starts in the ON state
	STA	CRSRTIMER
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Screen print routine, prints the full buffer
;///////////////////////////////////////////////////////////////////////////////////

PrintBuffer
	JSR StopCRSR
.pb0
	LDA	PRBUFFERCNT	; If the buffer is empty, continues on PrintEnd
	BEQ PrintEnd
	;BNE +
	;LDA #$01
	;STA TIMER1
-	;LDA TIMER1		; If it's not the time to print (TIMER1<>0), waits
	;BNE -
	;JSR NoChar		; Mutes the print beep
	;BNE	PrintEnd
+	LDA	TIMER1		; If its not the time to print (TIMER1<>0), returns to .pb0
	BNE	.pb0
	LDX	PRINDEXOUT
	;LDA	#%11000000	; Checks the flags states
	BIT	FLAGS1
    BMI .pb2        ; Command mode? -> Parse comnands
	BVS	.pb4		; Voice mode, continue on .PB4
 	LDA	PrBuffer, X
	CMP #$FF		; Check for commands
	BEQ .pb1

	CMP #$FE		; Check for command mode exit <- Captures extraneous $FE characters
	BEQ .pb2

	JSR	CharOut		;CHROUT replacement routine (Codebase)
	LDA SNDSTATUS
	BNE ++
	JSR Beep		; Print beep
++	INC	PRINDEXOUT
	DEC	PRBUFFERCNT
	LDA	PRSPEED		; Renew TIMER1 with PRSPEED
	STA	TIMER1
	JMP	.pb0		; Return to .pb0 to process the rest of the buffer
PrintEnd
;	LDA TEMPCNT2
;-	CMP TEMPCNT2
;	BEQ -
;	JSR NoChar
	JSR StartCRSR
	RTS
; // Enter command mode
.pb1
	INC PRINDEXOUT
	DEC PRBUFFERCNT	; Skip the $FF character
    LDA FLAGS1
    ORA #%10000000  ; Enters command mode
    STA FLAGS1
    BNE .pb0
; Parse commands
.pb2
	JSR NoChar		; Mutes the print beep
   	INC PRINDEXOUT
	DEC PRBUFFERCNT	; Update pointers	
    LDA PrBuffer,X
	BPL +			; Invalid command (bit7=0)
	CMP #MAXCMD+1
    BCC ++			; Is it less than or equal to the highest implemented command ($B5)?
+	LDA #$8F		; Invalid command, replace with unimplemented command ($8F)
++	AND #%01111111	; -128
	ASL
	TAY
    LDA CmdTable+1,Y
    STA .pb3+2
    LDA CmdTable,Y  ; Point the JSR to the command address (self-modifying) 
    STA .pb3+1
.pb3
    JSR CmdFE       ; Command call<<<<
	LDA #$00
	STA CMDFlags	; Command completed, reset CMDFlags
    JMP .pb0		; Return to .pb0 to process the rest of the buffer

.pb4                ; Insert byte into the voice buffer
	JSR NoChar		; Mute the print beep
    LDA PrBuffer,X
	CMP #$FF		; Check for commands
	BEQ .pb1
    INC PRINDEXOUT
    DEC PRBUFFERCNT
    JSR AddToSpBuffer
	JMP .pb0

;///////////////////////////////////////////////////////////////////////////////////
; CHROUT replacement, equivalent to the Kernal routine but no quote mode, 40
; logical columns and text window support
; Petscii -> Screen code by Mace (from Codebase64)
;///////////////////////////////////////////////////////////////////////////////////
CharOut
    CMP #$20		; if A<32 then...
	BCC ddCtrl      ; Control characters->

	CMP #$60		; if A<96 then...
	BCC dd1

	CMP #$80		; if A<128 then...
	BCC dd2

	CMP #$a0		; if A<160 then...
	BCC ddCtrl      ; Control characters->

	CMP #$c0		; if A<192 then...
	BCC dd4

	CMP #$ff		; if A<255 then...
	BCC ddRev

	LDA #$7e		; A=255, then A=126
	BNE ddEnd

dd2	AND #$5f		; if A=96..127 then strip bits 5 and 7
	BNE ddEnd

dd4	EOR #$c0		; if A=160..191 then flip bits 6 and 7
	BNE ddEnd

dd1 AND #$3f		; if A=32..95 then strip bits 6 and 7
	BPL ddEnd		; <- you could also do .byte $0c here

ddRev
	EOR #$80		; flip bit 7 (reverse on when off and vice versa)
ddEnd
    LDX $C7
    BEQ +
    ORA #$80        ; Inverse video

+   LDY CURRCOLUMN
    LDX CURRLINE
    STA (CURRLINEPTR),Y	; Write character to screen
    JSR UCRAM           ; Update Color RAM (Was $E4DA, not working with older Kernals)
    INY
    CPY #40             ; Reached right border?
    BNE cu1				; No, go update screen pointers
    LDY #00             ; Wrap to column 0
    INX
    CPX WBOTTOM         ; Pass bottom window border?
    BCC cu1            	; No, go update screen pointers
    DEX
    JSR WinScroll       ; Scroll text window

cu1 STY CURRCOLUMN
    STX CURRLINE
    JSR $E9F0           ; Set start of line (.X = line)
    JSR $EA24           ; Synchronize color RAM pointer
ce0 RTS
ddCtrl ;Control chars
   	CMP #$13			; Check for HOME
	BNE +				; No, next
	LDX WTOP
	LDY #$00
	BEQ cu1
+	CMP #$93			; Check for CLEAR
	BNE +				; No, next
    LDX WBOTTOM
-   DEX
    JSR $E9FF			; Clear Line
    CPX WTOP
    BNE -
    LDX WTOP
    LDY #$00
    BEQ cu1
+	CMP #$91			; Check for Cursor UP
	BNE +				; No, next
    LDX CURRLINE
	CPX WTOP			; Cursor at the top of the text window?
	BEQ ce0				; Yes, do nothing
    DEX
    BPL cu1+2
+	CMP #$11			; Check for Cursor DOWN
	BNE +				; No, next
	LDX CURRLINE
    INX
    CPX WBOTTOM     	; Cursor beyond the bottom of the text window?
	BNE cu1+2			; No, update pointers
    DEX
	JSR WinScroll		; Yes, scroll text window
	BNE cu1+2
+	CMP #$9D			; Check for Cursor LEFT
	BNE ++				; No, next
    LDX CURRLINE
    LDY CURRCOLUMN
    DEY
    BPL cu1         	; Cursor still on same row -> update pointers
    CPX WTOP
    BEQ +           	; Cursor is already at the text window's HOME
    DEX
    LDY #38         	; < 39-1, to be incremented to the correct value further ahead
+   INY             	; 38 to 39 if going up a row, 255 to 0 if already at HOME
    BPL cu1         	; update pointers
++  CMP #$1D        	; Check for Cursor RIGHT
    BNE +           	; No, next
    LDX CURRLINE
    LDY CURRCOLUMN
    INY
    CPY #40         	; Beyond column 40?
    BNE cu1         	; No, update pointers
    LDY #$00
    INX
    CPX WBOTTOM     	; Do we need to scroll?
    BNE cu1         	; No, update pointers
    JSR WinScroll
    DEX
    BPL cu1
+   CMP #$0D        	; Check for RETURN
    BEQ +
    CMP #$8D        	; Check for SHIFT-RETURN
    BNE ++
;+	LDA SNDSTATUS
	;BPL +
	;JSR Beep			; Play beep if SNDSTATUS = $80
+   LDX CURRLINE
    LDY #0
	STY $C7
    INX
    CPX WBOTTOM     	; Bayond the text window's bottom?
    BEQ +
    JMP cu1         	; No, move cursor
+   DEX
    JSR WinScroll   	; Yes, scroll
+   JMP cu1
++  CMP #$14        	; Check for DELETE
    BNE ++          	; No, next
    LDX CURRLINE
    LDY CURRCOLUMN
    BEQ +           	; If already on column 0, branch
    DEY
    STY CURRCOLUMN
-   INY
    LDA (CURRLINEPTR),Y
    DEY
    STA (CURRLINEPTR),Y
    INY
    LDA ($F3),Y
    DEY
    STA ($F3),Y
    INY
    CPY #39
    BNE -
    LDA #$20        	; Space
    STA (CURRLINEPTR),Y
    LDA $0286
    STA ($F3),Y
-   JMP cu1+2
+   CPX WTOP
;    BNE +           	; At HOME?
	BEQ +				; Yes, do nothing <<<<<<<<<<<<<<<<<<<<<
    ;RTS             	; Yes, do nothing
;+
    LDY #39
    STY CURRCOLUMN
    DEX
    STX CURRLINE
    JSR $E9F0       	; Update pointers
    JSR $EA24
    LDA #$20        	; Space
    STA (CURRLINEPTR),Y
    LDA $0286
    STA ($F3),Y
+   RTS
++  CMP #$94        	; Check for INSERT
    BNE ++           	; No, siguiente
    LDY #39
    LDA (CURRLINEPTR),Y
    CMP #$20        	; Space in the row's last column?
    BEQ +           
-   RTS             	; No, do nothing
+   CPY CURRCOLUMN  	; Cursor at the last column?
    BEQ -	            ; Yes, do nothing
-   DEY
    LDA (CURRLINEPTR),Y
    INY
    STA (CURRLINEPTR),Y
    DEY
    LDA ($F3),Y
    INY
    STA ($F3),Y
    DEY
    CPY CURRCOLUMN
    BNE -
    LDA #$20
    STA (CURRLINEPTR),Y
    LDA $0286
    STA ($F3),Y
    RTS
++  CMP #$12        	; Check for RVSON
    BNE +           	; No, next
    LDA #$01
    STA $C7         	; Reverse ON
    RTS
+   CMP #$92        	; Check for RVSOFF
    BNE +           	; No, next
    LDA #$00
    STA $C7         	; Reverse OFF
    RTS
+   CMP #$08        	; Check disable CBM+SHIFT
    BNE +           	; No, next
    LDA #$80
    STA $0291
    RTS
+   CMP #$09        	; Check enable CBM+SHIFT
    BNE +           	; No, next
    LDA #$00
    STA $0291
    RTS
+   CMP #$0E        	; Check switch to Uppercase/Lowercase
    BNE +
    LDA $D018
    ORA #$02
    BNE ++
+   CMP #$8E        	; Check switch to Uppercase/Graphics
    BNE +
    LDA $D018
    AND #$FD
++  STA $D018
    RTS
+	CMP #$07			; Check for BELL
	BNE +
	LDA #$14			; Mute sound
	STA SIDREG+18
	LDA #$15			; And turn it on again
	STA SIDREG+18
	RTS	
+   JSR $E8CB       	; Check colors (Kernal)
    RTS

; --- Patch for old Kernal ---
; Update color RAM
UCRAM
	LDA $0286
	STA ($F3),Y
	RTS
; -----------------------------

;///////////////////////////////////////////////////////////////////////////////////
; Text window scroll
;///////////////////////////////////////////////////////////////////////////////////
WinScroll
    TXA
    PHA
    TYA
    PHA

    LDX WBOTTOM
    DEX
    STX .ws+1
    LDX WTOP
    DEX

-   INX
    JSR $E9F0       ; Set start of line
.ws CPX #$18        ; Self modifying - Text window's bottom
    BCS +
    LDA $ECF1,X
    STA $AC
    LDA $DA,X
    JSR $E9C8       ; Move screen row
    BMI -           ; Continue until reaching WBOTTOM

+   JSR $E9FF       ; Clear screen row

    PLA
    TAY
    PLA
    TAX
    RTS

;///////////////////////////////////////////////////////////////////////////////////
; Text window scroll up ---- FUTURE FEATURE
;///////////////////////////////////////////////////////////////////////////////////
; WinScrollU
; 	TXA
; 	PHA
; 	TYA
; 	PHA
; 	LDX WUP
; 	STX .wu+1
; 	LDX WBOTTOM

; -	JSR $E9F0
; 	DEX
; 	LDA $ECF0,x
; 	STA $AC
; 	LDA $D9,x
; 	JSR $E9C8
; .wu	CPX #$00
; 	BNE -
; 	JSR $E9FF

; 	PLA
; 	TAY
; 	PLA
; 	TAX
; 	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Insert a 0 ended string into the print buffer
;///////////////////////////////////////////////////////////////////////////////////

AddText
	STX	AddTLoop + 1
	STY	AddTLoop + 2

	LDX	#$00			; Inits .X to 0
AddTLoop
	LDA	PrBuffer, X		; Reads a byte from the string
	BNE	AddTLoop1		; if 0, returns
	RTS
AddTLoop1
	JSR	AddToPrBuffer	; otherwise calls AddToPrBuffer to insert the byte into the print buffer
	INX					; Next character
	JMP	AddTLoop		; and loops

;///////////////////////////////////////////////////////////////////////////////////
; Insert a 0 ended string into the voice buffer
;///////////////////////////////////////////////////////////////////////////////////

SpeakText
	STX	SCopyLoop + 1
	STY	SCopyLoop + 2

	LDX	#$00			; Inits .X to 0
SCopyLoop
	LDA	SpBuffer, X		; Reads a byte from the string
	BNE	SCopyLoop1		; if 0, returns
	RTS
SCopyLoop1
	JSR	AddToSpBuffer	; otherwise calls AddToSpBuffer to insert the byte into the voice buffer
	INX					; Next character
	JMP	SCopyLoop		; and loops

;///////////////////////////////////////////////////////////////////////////////////
; Inserts the character in .A, to the voice buffer
;///////////////////////////////////////////////////////////////////////////////////

AddToSpBuffer
	STA	SPBYTE		; Saves the character in SPBYTE
	LDY	SPINDEXIN	; Loads .Y with SPINDEXIN
ChkSpBuffer
	LDA	SPBUFFERCNT	; If SPBUFFERCNT=255 (buffer full) waits until a space is open
	EOR	#$FF
	BEQ	ChkSpBuffer
	SEI				; Disable IRQs
	LDA	SPBYTE		; Store SPBYTE in the next free space in the voice buffer (SpBuffer)
	STA	SpBuffer, Y
	INC	SPINDEXIN	; Increment SPINDEXIN
	INC	SPBUFFERCNT	; and SPBUFFERCNT
	CLI				; Enable IRQs
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Get a character from the print buffer
;///////////////////////////////////////////////////////////////////////////////////
GetFromPrBuffer
    LDA PRBUFFERCNT
    BEQ GetFromPrBuffer ; Wait for a character in the print buffer
    LDX PRINDEXOUT
    INC PRINDEXOUT
    DEC PRBUFFERCNT     ; Update pointers
    LDA PrBuffer,X		; Get character
    RTS

;////////////////////////////////////////////////////////////////////////////////////
; Insert the character in .A, to the print buffer
;///////////////////////////////////////////////////////////////////////////////////

AddToPrBuffer
	STA	PRBYTE		; Store the character in PRBYTE
	LDY	PRINDEXIN	; Loads .Y with PRINDEXIN
ChkPrBuffer
	LDA	PRBUFFERCNT	; If PRBUFFERCNT=255 (buffer full) waits until a space is open
	EOR	#$FF
	BEQ	ChkPrBuffer
	SEI				; Disable IRQs
	LDA	PRBYTE		; Store PRBYTE in the next free space in the print buffer (PrBuffer)
	STA	PrBuffer, Y
	INC	PRINDEXIN	; Increment PRINDEXIN
	INC	PRBUFFERCNT	; and PRBUFFERCNT
	CLI				; Enable IRQs
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Wait for the voice buffer to be emptied
;///////////////////////////////////////////////////////////////////////////////////

; WaitSpeak
; 	LDA	SPBUFFERCNT	; Wait for SPBUFFERCNT = 0
; 	BNE	WaitSpeak
; 	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Wait for the print buffer to be emptied
;///////////////////////////////////////////////////////////////////////////////////

; WaitPrint
; 	LDA	PRBUFFERCNT	; Wait for PRBUFFERCNT = 0
; 	BNE	WaitPrint
; 	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Exit to BASIC
;///////////////////////////////////////////////////////////////////////////////////

ExitPrg
	SEI				; Disable IRQs
	LDA	#$10		; Triangle wave on channel 1 / gate off
	STA	SIDREG+4
!if _HARDTYPE_ != 56 {
	LDA	#%00000010	; no parity, no echo, no tx irq (rts=1), no rx irq, rx disabled (dtr=1)
	STA	T232COMMAND
}
	+EnKernal
	LDA	#%01111111
	STA	$DC0D		; "Switch off" interrupts signals from CIA-1
	STA	$DD0D		; "Switch off" interrupts signals from CIA-2
	LDA	#$31		; Restores the default IRQ vector
	STA	$0314
	LDA	#$EA
	STA	$0315
	LDA	#%00000000	; Disable raster interrupt signals from VIC
	STA	$D01A
	LDA	$DC0D		; Clear the interrupt flags (CIA1: IRQ)
	LDA	$DD0D		; (CIA2: NMI)
	LDA	#$47		; Restores the default NMI vector
	STA	$0318
	LDA	#$FE
	STA	$0319
	LDA	#32			; Setups CIA1 Timer A to generate the system IRQ
	STA	$DC04
	LDA	#78
	STA	$DC05
	LDA	#1
	STA	$DC0E
	LDA	#%10000001
	STA	$DC0D
	LDA	#0			; Empties the keyboard buffer
	STA	$C6
	STA $D010		; Clear sprites x-coord MSB
	CLI				; Enable IRQs
	;JSR $A660		; Call BASIC CLR 
	LDA $2D
	STA $31
	LDA $2E
	STA $32			; Restore BASIC array pointer 

	LDA	#<Msg05		; Print exit message
	LDY	#>Msg05
	JSR	STROUT

	LDA	#21			; Switch to text mode
	STA	$D018
	LDA	#$08		; Disable multicolor mode
	STA	$D016
	LDA #%00011011	; Text mode
	STA $D011
	JMP $E386		; Restart BASIC
	;RTS				; Exit to BASIC

;///////////////////////////////////////////////////////////////////////////////////
; Play print beep
;///////////////////////////////////////////////////////////////////////////////////
Beep:
	;LDA	PRBUFFERCNT	; If the print buffer is empty, continue on NoChar
	;BEQ	NoChar
	LDA	#$0F		; Volumen = 15 (max)
	STA	SIDREG+24
	LDA	TEMPCNT1	; If TEMPCNT1 is odd, continue on DoBeep2
	AND	#$01
	BNE	DoBeep2
	LDA	#$A4		; Channel 1 frequency = 671 Hz
	STA	SIDREG
	LDA	#$3F
	STA	SIDREG+1
	LDA	#$11		; Triangle wave on channel 1 / gate on
	STA	SIDREG+4
	BNE	DoBpEnd		; Continue on DoBpEnd
DoBeep2
	LDA	#$E5		; Channel 1 frequency = 1219 Hz
	STA	SIDREG
	LDA	#$4F
	STA	SIDREG+1
	LDA	#$11		; Triangle wave on channel 1 / gate on
	STA	SIDREG+4
	LDA	#$81		; Noise on channel 2 / gate on
	STA	SIDREG+11
DoBpEnd
	INC	TEMPCNT1
	;JMP	Speak
	RTS

WaitNoChar
	LDA TEMPCNT2
-	CMP TEMPCNT2
	BEQ -			; Wait for one frame
NoChar
	LDA	#$10		; Triangle wave on channel 1 / gate off
	STA	SIDREG+4
	RTS


;///////////////////////////////////////////////////////////////////////////////////
; Main IRQ routine
;///////////////////////////////////////////////////////////////////////////////////

Irq:

!if _HARDTYPE_ = 56 {
	+DisKernal
}
	;LDA	$D020		;<<<<<<<<<<<<<<<<
	;PHA				;<<<<<<<<<<<<<<<<

	INC	TEMPCNT2

	BNE ChkTimer1
	LDA #$00
	STA $D015			; Disable sprites

ChkTimer1
	LDA	TIMER1			; if TIMER1 != 0, decrement it
	BEQ	ChkTimer2
	DEC	TIMER1
ChkTimer2
	LDA FLAGS1
	AND #%00001000		; Check if the cursor is enabled
	BEQ EndTimers
	LDA	CRSRTIMER		; if CRSRTIMER !=0, decrement it
	BEQ	EndTimers
	DEC	CRSRTIMER
EndTimers

;///////////////////////////////////////////////////////////////////////////////////
; Receive up to 3 characters from RS232

ReadBytes
	LDA	#%00000000		; Disable raster interrupt signals from VIC
	STA	$D01A
	LDA	$D011			; Disable VIC II screen
	AND	#%01101111
	STA	$D011

	LDX	#3				; Character count
ReadBLoop
	LDA	PRBUFFERCNT		; If the print buffer is full, continue on Speak
	EOR #$FF
	BEQ	Speak

	LDA #%00001111		; Z = 0 if there's no parameters left to receive
	BIT CMDFlags		; Check command flags
	BVC +				; If we're not waiting for parameters, receive character
	BEQ Speak			; If we were waiting for parameters but all were received, do not receive more characters
						; until the command is completed
+	JSR	ReadByte		; Receive a character from RS232
	BCC	Speak  			; If none is received, continue on Speak
	LDA	RXBYTE			; Get the received character
	JSR	AddToPrBuffer	; Insert it into the print buffer
	;INC $D020		;<<<<<<<<<<<<<<<<<<<

	;LDA #%00001111		; Z = 0 if there's no parameters left to receive
	BIT CMDFlags		; Check command flags
	BMI .cmdchk			; If the last character was CMDON, check which command was received
	BVS +				; Branch if we're waiting for parameters
	LDA RXBYTE			; Not waiting for command or parameters
	BPL ++				; If bit 7 is not set, it isn't a command
	BIT FLAGS1			; If bit 7 is set, check that we are in command mode
	BMI .cmdchk			; Yes, it is a command
	EOR #$FF			; Check if the received character is CMDON
	BEQ .cmdon			;BNE ++
	CMP #$01			; Check if the character is an extraneous CMDOFF
	BNE ++
	LDA #$00			; Last received characters is an extraneous CMDOFF
	BEQ +++
.cmdon
	LDA #%10000000		; Last received character is CMDON
+++	STA CMDFlags		; Set CMDFlags
	BNE ++
+	DEC CMDFlags		; A parameter character was received, decrement counter

-
++	DEX					; Decrementa X
	BNE	ReadBLoop		; if != 0, return to ReadBLoop
	BEQ Speak			; Continue on Speak

.cmdchk					; Check the received command and set CMDFlags accordingly
	LDA RXBYTE			; Get the received character
	BPL +				; Invalido command (bit 7 unset)
	CMP #MAXCMD+1
    BCC ++				; Is it less than or equal to the highest implemented command ($B5)?
+	LDA #$8F			; Invalid command, replace with unimplemented command ($8F)
++	AND #%01111111		; -128
	TAY
	LDA CmdParTable,Y	; Parameter count
	AND #%00001111		; Clear unwanted bits
	ORA #%01000000		; Enable parameter wait
	STA CMDFlags		; Set CMDFlags
	BNE -				; Continue the loop

Speak
	LDA	SPBUFFERCNT		; If the voice buffer is empty, continue on SpeakEnd
	BEQ	SpeakEnd
!if _HARDTYPE_ = 56 {
!ifdef _SYNTH_ {
	LDA	#$00			; 2 Set RTS to 0 (and DTR to 0, enabling the voice synthesizer)
	STA	$DD01			; 4
}
}
	LDX	SPINDEXOUT
	LDA	FLAGS1			; If the voice synth is disabled, ignores the reception
	AND	#%00000100
	BEQ	+
	LDA	SpBuffer, X		; Get a character from the voice buffer
!ifdef _SYNTH_{
	JSR	SendByte		; and sends it to the voice synth
	LDA	#0
	JSR	SendByte		; and sends it to the voice synth
}
+	INC	SPINDEXOUT
	DEC	SPBUFFERCNT
SpeakEnd
!if _HARDTYPE_ = 56{
	LDA	#$04			; 2 Set RTS to 0 (and DTR to 1, enabling RS232)
	STA	$DD01			; 4
}

ChkKey
!if _HARDTYPE_ = 56{
	+EnKernal
}
	JSR	$F142			; Read the keyboard buffer
	BEQ	ExitIrq			; If there's no key pressed, continue on ExitIrq
	CMP	#$03			; Check for RUN STOP
	BNE	+
	LDA	#%00000001		; If it is STOP, setup exit to BASIC
	ORA	FLAGS1
	STA	FLAGS1
	LDA	#$FF			; STOP was pressed
	STA	$91
	JMP	ExitIrq			; and continues to ExitIrq to exit the IRQ routine
+	CMP	#$8D			; Check for SHIFT + RETURN
	BNE	+
	LDA	#$0A			; if so, convert it to LF
	BNE ++
+	CMP #$A7			; CBM+M?
	BNE +
	LDA #$FF
	EOR SNDSTATUS
	STA SNDSTATUS
	LDA #$03
	STA $D015			;Enable Sprites
	STA TEMPCNT2
	EOR $07F8
	STA $07F8
	BNE ExitIrq
	;LDA #$01
	;CMP SNDSTATUS
	;BEQ .m1
	;BCS .m0
	;ROL SNDSTATUS
	;BEQ +
.m1	;ROR SNDSTATUS
	;BNE +
.m0	;INC SNDSTATUS

+
;+	JSR	SendByte		; otherwise, send the typed character by RS232

!if _HARDTYPE_ = 56 {
++	+DisKernal
	JSR SendByte		; otherwise, send the typed character by RS232
	+EnKernal
} else {
++	STA	T232DATA		; Store the typed character on the transmit register, to be sent in the next interrupt
}

ExitIrq
	LDA	#$FF			; Clear the interrupt flags
	STA	$D019
	LDA	#%00000001		; Enable raster interrupt signals from VIC
	STA	$D01A
	LDA FLAGS1
	AND #%00010000		; Is screen split enabled?
	BEQ +
	LDA SPLITRST
	STA $D012
	LDA #<IrqB3
	STA $0314
	LDA #>IrqB3
	STA $0315
	LDA VMODE
	STA $D016			; Switch to hires or multicolor mode
	LDA UPBGC
	STA $D021			; Upper section background color
	LDA #%00011111
	STA $D018
	LDA #%00111010		;%00111011	; Enable bitmap mode
	BNE ++
+	LDA	$D011			; Enable VIC II screen
	AND	#%01111111
	ORA	#$10
++	STA	$D011

	;PLA			;<<<<<<<<<<<<<
	;STA $D020	;<<<<<<<<<<<<

	JMP	$EA31			; Jump into KERNAL's standard interrupt service routine.

;///////////////////////////////////////////////////////////////////////////////////
; COMMANDS
;///////////////////////////////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////////////////////////////
; 128: Set the transfer memory pointer, requires 2 parameter
;      bytes: destination address (low, high)

Cmd80
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	BLOCKPTR		; and store it in BLOCKPTR
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	BLOCKPTR + 1	; and store it in BLOCKPTR + 1
	RTS 

;///////////////////////////////////////////////////////////////////////////////////
; 129: Select a preset address for memory transfer, 
;	   requires 1 parameter byte: Destiny address preset

Cmd81
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	EOR #$00
	BEQ	Addr00			; If $00 go to Addr00
	CMP	#$10
	BEQ	Addr10			; If $10 go to Addr10
	CMP	#$20
	BEQ	Addr20			; If $20 go to Addr20
	LDA	#$01			; Otherwise set BLOCKPTR to $0801 (BASIC text area)
	STA	BLOCKPTR
	LDA	#$08
	STA	BLOCKPTR + 1
-	RTS 
Addr00
	LDA	#$00			; Point BLOCKPTR to $0400 (1024)
	STA	BLOCKPTR
	LDA	#$04
	STA	BLOCKPTR + 1
	RTS 
Addr10
	LDA	#$00			; Point BLOCKPTR to $2000 (8192)
	STA	BLOCKPTR
	LDA	#$20
	STA	BLOCKPTR + 1
	RTS 
Addr20
	LDA	#$00			; Point BLOCKPTR to $D800 (55296)
	STA	BLOCKPTR
	LDA	#$D8
	STA	BLOCKPTR + 1
	RTS 

;///////////////////////////////////////////////////////////////////////////////////
; 130: Transfers a byte block to memory, requires 2 parameter bytes
;      Byte count (low, high)

Cmd82
	LDA #$00
	STA $D015			; Disable sprites
	LDA	#$10			; Triangle wave on channel 1 / gate off
	STA	SIDREG+4
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	TAY;STA	BYTECNT
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	TAX;STA	BYTECNT + 1

; Check if transferring to the BASIC area
	LDA BLOCKPTR
	CMP #$01
	BNE +
	LDA BLOCKPTR + 1
	CMP #$08
	BNE +
	CLC
	TYA
	ADC BLOCKPTR
	STA $2D
	TXA
	ADC BLOCKPTR+1
	STA $2E				; Update Start of BASIC variables pointer


+	DEY
	CPY #$FF
	BNE +
	DEX
+	STY BYTECNT
	STX BYTECNT + 1
	LDA	$D020
	STA	BORDERCLR
	LDA	BLOCKPTR		; Copy BLOCKPTR pointer to C82Loop
	STA	C82Loop + 9
	LDA	BLOCKPTR + 1
	STA	C82Loop + 10
	LDA #$00			; Disable raster interrupts
	STA $D01A
	SEI					; Disable IRQs
	LDA	$DC0D			; Clear interrupt flags

!if _HARDTYPE_ = 56{
	+DisKernal
}

-	LDA $D012
	CMP #251
	BNE -				; Waits for raster line 251

	LDA	$D011			; Disables VIC II screen
	AND	#%01101111
	STA	$D011

C82Loop
-	JSR	ReadByte		; Receive a character from RS232
	BCC	-				; Nothing received, retry
	LDA	RXBYTE			; Store it in RAM
	STA	BUFFER			; (self-modifying code)
	INC	$D020
	INC	C82Loop + 9		; 6 Increment the memory pointer
	BNE	C82Next			; 2 
	INC	C82Loop + 10	; 6 
C82Next					; 1 if coming from the BNE
	LDY BYTECNT
	DEY
	CPY #$FF
	BNE C82Cont
	LDX BYTECNT+1
	DEX
	CPX #$FF
	BEQ	C82End			; 2 
	STX BYTECNT+1 
C82Cont					; 1 if coming from the BNE
	STY BYTECNT
	JMP	C82Loop
C82End
	LDA	BORDERCLR
	STA	$D020
	LDA	$DC0D			; Clear the interrupt flags ***

!if _HARDTYPE_ = 56{
	+EnKernal
}

	LDA	$D011			; Enable VIC II screen
	ORA	#%00010000
	STA	$D011
	LDA #$01
	STA $D01A			; Enable raster interrupts
	CLI					; Enable IRQs
	RTS 

;///////////////////////////////////////////////////////////////////////////////////
; 131: PCM audio streaming until receiving a NUL ($00) character

Cmd83
	LDA #$00		; Disable raster interrupts
	STA $D01A
	SEI
	;LDA #$00
	STA $D015		; Disable sprites

!if _HARDTYPE_ = 56 {
	+DisKernal
}
		; LDA #$00
		; STA SIDREG+$05	;AD 1
		; STA SIDREG+$0C	;2
		; STA SIDREG+$13	;3
		; STA SIDREG+$15	;Filter lo
		; LDA #$0F
		; STA SIDREG+$06	;SR 1
		; STA SIDREG+$0D	;2
		; STA SIDREG+$14	;3
		; LDA #$01
		; STA SIDREG+$04	;CTRL 1
		; STA SIDREG+$0B	;2
		; STA SIDREG+$12	;3
		; LDA #$10
		; STA SIDREG+$16	;Filter hi
		; LDA #%11110111
		; STA SIDREG+$17	;Filter voices/resonance
	LDA #$FF		; Pulse wave / Gate On / Test bit On <- Digi Boost
	STA SIDREG+6
	STA SIDREG+13
	STA SIDREG+20
	LDA	#$49
	STA	SIDREG+4
	STA SIDREG+11
	STA SIDREG+18
	LDA	$D020
	STA	BORDERCLR

	LDA	#%01111111
	STA	$DC0D		; "Switch off" interrupts signals from CIA-1
	STA	$DD0D		; "Switch off" interrupts signals from CIA-2

	LDX	#$88

-	LDA $D012
	CMP #251
	BNE -			; Wait for raster line 251

	LDA	$D011		; Disable VIC II screen
	AND	#%01101111
	STA	$D011

	JSR	TurboRX		; Do the thing
!if _HARDTYPE_ = 56 {
	+EnKernal
}
	CLI


	LDA	$D011		; Enable VIC II screen
	ORA	#%00010000
	STA	$D011
	LDA #$01
	STA $D01A		; Enable raster interrupts


	LDA	BORDERCLR
	STA	$D020
	;Reinit the SID
	JSR InitSID
	LDA	$DC0D		; Clear CIA1 interrupt flags

	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 132: SID streaming until receiving a 0 byte data block or interrupted by the user

Cmd84
	LDA #$00
	STA $D015		; Disable sprites
	;LDA #$00		; Disable raster interrupts
	STA $D01A
	SEI
	LDA	#$10		; Triangle wave on channel 1 / gate off
	STA	SIDREG+4

	LDA $D011
	AND #$7F
	STA $D011
	LDA #$33		; Set raster interrupt to line 51
	STA $D012

	LDA #<c84_irq1
	STA $FFFE		;$0314
	LDA #>c84_irq1
	STA $FFFF		;$0315

	LDA #$01
	STA $D01A		; Enable raster interrupts
;
	LDA $D011
	AND #%01101111	; Disable the screen
	STA $D011
;

	LDA #$00		; Setea keyboard matrix for direct access
	STA $DC03		; port b ddr (input)
	LDX #$FF
	STX $DC02		; port a ddr (output)
;	LDA #$00
	STA $DC00

-	LDA $DC01
	CMP #$FF
	BNE -			; Wait for the last key to be released

	+DisKernal
	CLI
	INC STREAMFLAG

	LDA #50			; First sync will be sent after 50 frames at the start
	STA SIDSTREAM_SYNC

	; Wait remote stop <<<<<<<<<
-	LDA STREAMFLAG
	;BNE -
	BEQ +
;!if _HARDTYPE_ != 56 {
	;JSR $F142		; Read the keyboard buffer
	LDA $DC01
	CMP #$FF
	BEQ -			; No key pressed, recheck STREAMFLAG
	LDA #$FF
	STA STREAMFLAG	; Abort stream
	;JSR	SendByte
	;STA T232DATA
	CMP #$03		; RUN/STOP?
;}
	BNE -
	LDA #$01		;exit to BASIC
	ORA FLAGS1
	STA FLAGS1
	DEC STREAMFLAG


+	LDA #$00		; Disable raster interrupts
	STA $D01A
	SEI

	LDA #$10
	ORA $D011
	AND #%01111111
	STA $D011		; Enable screen

	LDA #<Irq
	STA $0314
	LDA #>Irq		; Restore normal IRQ routine
	STA $0315

	+EnKernal

	LDA #251
	STA $D012

	LDA #$01
	STA $D01A		; Enable raster interrupts

	;Reinit the SID
	JSR InitSID

	CLI
	JMP CmdFE		; Exit command mode

SIDSTREAM_SYNC
!byte	$00


;////////////////////////////////////////////////////////////////////////////////////
; 133: Set the order the SID registers are written. 25 parameter bytes

Cmd85
	SEI
	+DisKernal
	LDY #$00
--	TYA
	PHA
-	LDA $D012
	CMP #251
	BCC -
-	JSR ReadByte
	BCC -
	PLA
	TAY
	LDA RXBYTE
	STA regtable,Y		; Store in the register table
	INY
	CPY #$19
	BNE --				; Repeat until all 25 parameters are read
	+EnKernal
	CLI
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 144: Returns to the default text mode, requires 3 parameter bytes
;      Page (not used), border color, background color

Cmd90
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	;Página - Descartado
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$D020
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$D021
	LDA	#23				; Switch to texto mode (uppercase/lowercase)
	STA	$D018
	LDA	#$08			; Disable multicolor mode
	STA	$D016
	LDA	#$1F			; Clear bit 6 (extended color mode) and 5 (bitmap mode)
	AND	$D011
	STA	$D011
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 145: Switch to bitmap hires mode, requires 2 parameter bytes
;      Page (not used), border color

Cmd91
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$D020
	LDA	#$08
	ORA	$D018
	STA	$D018
	LDA	#$08			; Disable multicolor
	STA	$D016
	; Switch to bitmap mode
	LDA #%00111011
	STA	$D011
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 146: Switch to bitmap multicolor mode, requires 3 parameter bytes
;      Page (not used), border color, background color

Cmd92
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$D020
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$D021
	LDA	$D018
	ORA	#$08
	STA	$D018
	LDA	#$10			; Set multicolor multicolor mode
	ORA	$D016
	STA	$D016
	; Switch to bitmap mode
	LDA #%00111011
	STA	$D011
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 160: Selects the screen as the output for the received characters
CmdA0
	LDA	FLAGS1
	AND	#%10111111	; Switch to screen mode, Setting FLAGS1 bit 6 to 0
	STA	FLAGS1		; and exits command mode
	JMP	CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 161: Selects the voice synthesizer as output for the received characters
CmdA1
	LDA	FLAGS1
	ORA	#%01000000	; Switch to voice mode, Setting FLAGS1 bit 6 to 1
	STA	FLAGS1		; and exits command mode
	JMP CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 162: Requests the terminal's ID and version

CmdA2
	SEI
!if _HARDTYPE_ = 56 {
	+DisKernal
}
	LDY #00
	STY $D015		; Disable Sprites
-	LDA IDString,Y
!if _HARDTYPE_ = 56 {
	JSR SendByte
} else {
	JSR SendID
}
	INY
	CPY #24
	BNE -
!if _HARDTYPE_ = 56 {
	+EnKernal
}
	CLI
	RTS             ;JMP	ExitIrq

;///////////////////////////////////////////////////////////////////////////////////
; 163: Queries terminal if a command is implemented, requires 1 parameter: Command #
;	   Returns number of parameters if command exist, or bit-7 = 1 if not.

CmdA3
	SEI
	JSR GetFromPrBuffer ; Reads a byte from the print buffer / Command #
	BPL +				; If it's not a command, replace with unimplemented command ($8F)
	CMP #MAXCMD+1
	BCC ++				; Is it less than or equal to the highest implemented command?
+	LDA #$8F			; Invalid command, replace with unimplemented command ($8F)
++	AND #%01111111		; -128
	TAY
	LDA CmdParTable,Y	; Get parameter count/Command implemented
!if _HARDTYPE_ = 56{
	+DisKernal
	JSR SendByte
	+EnKernal
} else {
	JSR SendID
}
	CLI
	RTS


;///////////////////////////////////////////////////////////////////////////////////
; 176: Sets cursor position, requires two parameter bytes: Column, row
; 	   Relative to the current text window

CmdB0
	JSR StopCRSR
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer / Column
	CMP #39				; Greater than 39?
	BCC	+				; No, save it, get row
	LDA #39				; Yes, force 39
+	PHA					; temp save
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer / Row
	CLC					; 0.13 relative position
	ADC WTOP			; 0.13 relative position
	TAX
	CPX WBOTTOM			; Greater than WBOTTOM?
	BCC +				; No, continue
	LDX WBOTTOM			; Yes, force WBOTTOM
;+	CPX WTOP			; 0.12 absolute position
;	BCS +				; 0.12 absolute position
;	LDX WTOP			; 0.12 absolute position
+	PLA
	TAY					;LDY TEMP1
	CLC
	JSR $E50A			; Set cursor position
	JSR StartCRSR
	JMP	CmdFE			; Exit command mode


;///////////////////////////////////////////////////////////////////////////////////
; 177:	Fill a row with the selected character (screen code) cursor is not moved
;		Requires 2 parameter bytes: Row, screen_code

CmdB1
	JSR StopCRSR
	JSR GetFromPrBuffer ; Reads a byte from the print buffer / Row
	CMP #$24			; Greater than 24?
	BCC +				; No, continue
	LDA #$24			; Yes, force 24
+	TAX
	LDA $ECF0,X			; Get the row address low byte from the ROM table
	STA .cb11+1			; Screen
	STA .cb12+1			; ColorRAM
	LDA $D9,X
	AND #$03
	ORA #$04			; Row address high byte
	STA .cb11+2			; Screen
	EOR #$DC
	STA .cb12+2			; ColorRAM
	
	JSR GetFromPrBuffer ; Reads a byte from the print buffer / screen_code

	TAY					; Save screen_code

	; Fill row
	LDX #39
.cb11
	STA $0400,X
	LDA $0286			; Current Color
.cb12
	STA $D800,X	
	TYA
	DEX
	BPL .cb11

	JSR StartCRSR
	
	RTS



;///////////////////////////////////////////////////////////////////////////////////
; 178: Set the cursor enable status, requires a single parameter byte

CmdB2
	LDA FLAGS1
	ORA #%00001000
	TAY
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	BNE	CrSr00			; IF not $00 continue on CrSr00
	JSR StopCRSR
	LDA FLAGS1
	AND #%11110111
	TAY
CrSr00
	STY FLAGS1
	RTS             	;JMP ExitIrq

;///////////////////////////////////////////////////////////////////////////////////
; 179: Split screen, requires 2 parameters, mode/row and background colors

UPBGC:
!byte $00				; Background color for the upper part of the split
BTBGC:
!byte $00				; Background color for the bottom part of the split
VMODE:
!byte $00				; Video mode for the upper part of the split

CmdB3
	JSR GetFromPrBuffer
	BEQ b3cancel
	BMI +
	LDX #%11001000
	BNE ++
+	LDX #%11011000
++	STX VMODE
	AND #$7F			; Remove mode bit
	STA WTOP			; Set text window
	ASL
	ASL
	ASL					; .A*8
	CLC
	ADC #$30			; +50 = Scanline-3
	STA SPLITRST
	JSR GetFromPrBuffer
	STA UPBGC
	LSR
	LSR
	LSR
	LSR
	STA BTBGC
	LDA #%00010000
	ORA FLAGS1
	STA FLAGS1
	LDY #$00
	LDX WTOP
	CLC
	JSR $E50A			; Set cursor position
	RTS
b3cancel				; Cancel split screen
	LDA #%11101111
	AND FLAGS1
	STA FLAGS1
	LDA #$00
	STA WTOP			; Set text window
	RTS

;---------------------------------
; Split screen interrupt routine

IrqB3:
	LDY BTBGC
	LDX $D012
-	CPX $D012
	BEQ -				; Synchronize jitter
	
	LDX #7				;2
-	DEX					;2
	BNE -				;3

	;NOP				;<----
	LDY BTBGC
	;NOP
	;NOP

	;LDA #$FF			;2
	ROL $D019			;4
;<- 50-55
	;NOP
	;NOP
	LDA #%00011011		;2 Text mode
	STA $D011			;4
	LDA #%11001000		;2
	STA $D016			;4 Disable multicolor
	;LDA BTBGC			;4
	STY $D021			;4 Bottom section background color
	LDA	#23				;2 Switch to text mode (uppercase/lowercase)
	STA	$D018			;4

	LDA #251			;2
	STA $D012			;4

	LDA #<Irq
	STA $0314
	LDA #>Irq
	STA $0315

	JMP $EA81


	

;///////////////////////////////////////////////////////////////////////////////////
; 181: Set text window, requires 2 parameters, top and bottom rows

CmdB5
	JSR GetFromPrBuffer
	BPL +
	LDA #$00
+	CMP #24
	BCC +
	LDA #24
+	STA WTOP
	JSR GetFromPrBuffer
	TAY
	BPL +
	LDY #$00
+	CPY #24
	BCC +
	LDY #24
+	INY
	STY WBOTTOM
	LDY #$00
	LDX WTOP
	CLC
	JSR $E50A		; Set cursor position
	RTS



;///////////////////////////////////////////////////////////////////////////////////
; 254: Exit command mode, setting FLAGS1 bit 7 to 0
CmdFE
	LDA	FLAGS1		
	AND	#%01111111
	STA	FLAGS1
    RTS

;///////////////////////////////////////////////////////////////////////////////////


IDString:
!if _HARDTYPE_ = 38 {
	!text "RTRETROTERM-SL "
	+_Version_
	!text "   "	
} else {
	!text "RTRETROTERM "
	+_Version_
	!text "      "
}
	!byte $00,$06	;Turbo56K version, subversion

!if _HARDTYPE_ != 56 {
SendID
	STA	T232DATA	; Store the character in the transmit register
	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, receive enable), no rx irq, rx enabled (dtr=0)
	STA	T232COMMAND
-	LDA	T232STATUS	; Wait for the character to be transmitted
	AND	#%00010000
	BEQ	-
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, receive disable), no rx irq, rx enabled (dtr=0)
	STA	T232COMMAND
	RTS
}

;///////////////////////////////////////////////////////////////////////////////////
; Startup text
;///////////////////////////////////////////////////////////////////////////////////

Credits

Version
	!byte $93, $99, $0E
!if _HARDTYPE_ = 232 {
	!text "retroterm turbo232 VER "
	+_Version_
	!text"    57600,8n1"
}
!if _HARDTYPE_ = 38 {
	!text "retroterm swiftlink VER "
	+_Version_
	!text "   38400,8n1"
}
!if _HARDTYPE_ = 56 {
	!text "retroterm VERSION "
	+_Version_
	!text "         57600,8n1"
}
	!byte $05, $00

!ifdef _SYNTH_{
VersionSp
;	!byte $0D
	!text "ret'roterm  version ser2opuntocat2orse,,"
	!byte $00
Msg00
	!byte $0D
	!text "microsint detectado."
	!byte $0D, $00
Msg01
	!text "mensajes de voz activados."
	!byte $0D, $00
}
Msg05
	!byte $93, $8E
	!text "EXITING TO BASIC..."
	!byte $0D
	!text "SYS 49152 TO RUN RETROTERM AGAIN"
	!byte $0D, $0D, $00
Msg06
	!byte $0D
	!text "(c)2022 pastbytes/durandal"
	!byte $0D		;, $00
Msg07
	!byte $9a
	!text "turbo56k V0.6"
	!byte $0D, $05, $00

!ifdef _SYNTH_{
Sp00
	!text "mik'rosint detektado,"
	!byte $00
Sp01
	!text "mensajesdevos,aktivados,"
	!byte $00
}
; Commands routine pointer table, unimplemented commands point to CMDOFF
CmdTable:
    !word Cmd80,Cmd81,Cmd82,Cmd83,Cmd84,Cmd85,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word Cmd90,Cmd91,Cmd92,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word CmdA0,CmdA1,CmdA2,CmdA3,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word CmdB0,CmdB1,CmdB2,CmdB3,CmdFE,CmdB5

; Command parameter number table.
; bit-7 = 1 : Parameter not implemented
CmdParTable:
	!byte $02  ,$01  ,$02  ,$00  ,$00  ,$00  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $03  ,$02  ,$03  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $00  ,$00  ,$00  ,$01  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $02  ,$02  ,$01  ,$02  ,$80  ,$02

;///////////////////////////////////////////////////////////////////////////////////
; Buffers (Reception + print + voice)
;///////////////////////////////////////////////////////////////////////////////////

ENDOFCODE
!if ENDOFCODE > $CDFF {
	!error "ERROR - Part 1 data beyond $CDFF"
}

}
_ENDOFCODE_

;///////////////////////////////////////////////////////////////////////////////////
; Second code section $E000->
;///////////////////////////////////////////////////////////////////////////////////

_EXTRACODE_
!pseudopc $E000{
EXTRACODE:

!if _HARDTYPE_ = 56 {
!ifdef _SYNTH_{
USintDet:
	STA	TXBYTE		; Store .A in TXBYTE
	LDA	$DD00		; Save a copy of PRA (port A, CIA2)
	AND	#$FB		; Set TX to 0 (start bit)
	TAX				; copy to .X
SendStart_
	STA	$DD00		; *** Start bit starts here ***
;---------------------------------------------------------------------------------------
CalcB0_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 0 to the carry flag
	BCS	Bit0Is1_	; 2 if CARRY=1 continue on Bit0Is1
Bit0Is0_
	JMP	SendB0_		; 3 if CARRY=0, continue on SendB0
Bit0Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB0_
	STA	$DD00		; 4 Write port A
					; *** bit 0 starts here ***
;---------------------------------------------------------------------------------------
CalcB1_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 1 to the carry flag
	BCS	Bit1Is1_	; 2 if CARRY=1 continue on Bit1Is1
Bit1Is0_
	JMP	SendB1_		; 3 if CARRY=0, continue on SendB1
Bit1Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB1_
	STA	$DD00		; 4 Write port A
					; *** bit 1 starts here ***
;---------------------------------------------------------------------------------------
CalcB2_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 2 to the carry flag
	BCS	Bit2Is1_	; 2 if CARRY=1 continue on Bit2Is1
Bit2Is0_
	JMP	SendB2_		; 3 if CARRY=0, continue on SendB2
Bit2Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB2_
	STA	$DD00		; 4 Write port A
					; *** bit 2 starts here ***
;---------------------------------------------------------------------------------------
CalcB3_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 3 to the carry flag
	BCS	Bit3Is1_	; 2 if CARRY=1 continue on Bit3Is1
Bit3Is0_
	JMP	SendB3_		; 3 if CARRY=0, continue on SendB3
Bit3Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB3_
	STA	$DD00		; 4 Write port A
					; *** bit 3 starts here ***
;---------------------------------------------------------------------------------------
CalcB4_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 4 to the carry flag
	BCS	Bit4Is1_	; 2 if CARRY=1 continue on Bit4Is1
Bit4Is0_
	JMP	SendB4_		; 3 if CARRY=0, continue on SendB4
Bit4Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB4_
	STA	$DD00		; 4 Write port A
					; *** bit 4 starts here ***
;---------------------------------------------------------------------------------------
CalcB5_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 5 to the carry flag
	BCS	Bit5Is1_	; 2 if CARRY=1 continue on Bit5Is1
Bit5Is0_
	JMP	SendB5_		; 3 if CARRY=0, continue on SendB5
Bit5Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB5_
	STA	$DD00		; 4 Write port A
					; *** bit 5 starts here ***
;---------------------------------------------------------------------------------------
CalcB6_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 6 to the carry flag
	BCS	Bit6Is1_	; 2 if CARRY=1 continue on Bit6Is1
Bit6Is0_
	JMP	SendB6_		; 3 if CARRY=0, continue on SendB6
Bit6Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB6_
	STA	$DD00		; 4 Write port A
					; *** bit 6 starts here ***
;---------------------------------------------------------------------------------------
CalcB7_
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXTBYTE bit 7 to the carry flag
	BCS	Bit7Is1_	; 2 if CARRY=1 continue on Bit7Is1
Bit7Is0_
	JMP	SendB7_		; 3 if CARRY=0, continue on SendB7
Bit7Is1_			; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB7_
	STA	$DD00		; 4 Write port A
					; *** bit 7 starts here ***
;---------------------------------------------------------------------------------------
CalcStop_
	TXA				; 2 Get port A value back to .A
	ORA	#$04		; 2 Set .A bit 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
SendStop_
	STA	$DD00		; 4 *** Stop bit starts here ***

;///////////////////////////////////////////////////////////////////////////////////
; USintRX subroutine, receive a byte and store it on RXBYTE
;---------------------------------------------------------------------------------------

USintRX
	LDA	#$01		; 2 Set timer A to 300 ($012C) microseconds
	STA	$DC05		; 4
	LDA	#$2C		; 2
	STA	$DC04		; 4
	LDA	$DD0D		; 4 Clear interrupt flags (CIA2: NMI)
	LDA	#$09		; 2 Start timer A
	STA	$DC0E		; 4
	LDA	#$90		; 2 Enable interrupt (NMI) by FLAG2
	STA	$DD0D		; 4
WaitStrt_
	LDA	$DC0D		; 4 If timer A hasn't elapsed
	AND	#$01		; 2
	BEQ	WaitStrt_	; 2 loop to WaitStrt_ waiting for the start bit
CancelRX_
	LDA	#$10		; 2 Disable interrupt (NMI) by FLAG2
	STA	$DD0D		; 4
	LDA	#$00		; 2 Store 0 in RXBYTE
	STA	RXBYTE		; 4
	CLC				; 2 Set CARRY to 0 (no reception)
	RTS				; 6

; NMI routine
StartBit_			; 14 Cycles getting here
	NOP
	NOP
	;SEI
	LDA	#$10		; 2 Disable interrupt (NMI) by FLAG2
	STA	$DD0D		; 4
ReadB0_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB1_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
ReadB2_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	LDA	#$00		; 2 Set RTS to 0 (and DTR to 0, enabling the voice synth)
	STA	$DD01		; 4
ReadB3_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB4_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB5_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	LDA	$DD0D		; 4 Clear the interrupt flags (CIA2: NMI)
	NOP				; 2
ReadB6_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB7_
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	PLA				; 4 Remove the NMI call from the stack
	PLA				; 4
	PLA				; 4
	SEC				; 2 Set CARRY = 1 (a character was received)
	;CLI
	RTS				; 6
}

;------------ Send Byte ----------------------------------------------------------------

SendByte
	TAX
-	LDA $D012		; Wait for a rasterline right after a badline
	AND #%00000111
	CMP #$04
	BNE -
	TXA

_SendByte			; Alternative entry point, for use with disabled screen
	STA	TXBYTE		; Store .A in TXBYTE
	LDA	$DD00		; Get PRA (puerto A, CIA2)
	AND	#$FB		; Set TX to 0 (start bit)
	TAX				; And copy to .X
SendStart
	STA	$DD00		; *** start bit starts here ***
;---------------------------------------------------------------------------------------
CalcB0
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 0 to the carry flag
	BCS	Bit0Is1		; 2 if CARRY=1 continue on Bit0Is1
Bit0Is0
	JMP	SendB0		; 3 if CARRY=0, continue on SendB0
Bit0Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB0
	STA	$DD00		; 4 Write port A
					; *** bit 0 starts here ***
;---------------------------------------------------------------------------------------
CalcB1
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 1 to the carry flag
	BCS	Bit1Is1		; 2 i CARRY=1 continue on Bit1Is1
Bit1Is0
	JMP	SendB1		; 3 if CARRY=0, continue on SendB1
Bit1Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB1
	STA	$DD00		; 4 Write port A
					; *** bit 1 starts here ***
;---------------------------------------------------------------------------------------
CalcB2
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 2 to the carry flag
	BCS	Bit2Is1		; 2 if CARRY=1 continue on Bit2Is1
Bit2Is0
	JMP	SendB2		; 3 if CARRY=0, continue on SendB2
Bit2Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB2
	STA	$DD00		; 4 Write port A
					; *** bit 2 starts here ***
;---------------------------------------------------------------------------------------
CalcB3
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 3 to the carry flag
	BCS	Bit3Is1		; 2 if CARRY=1 continue on Bit3Is1
Bit3Is0
	JMP	SendB3		; 3 if CARRY=0, continue on SendB3
Bit3Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB3
	STA	$DD00		; 4 Write port A
					; *** bit 3 starts here ***
;---------------------------------------------------------------------------------------
CalcB4
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 4 to the carry flag
	BCS	Bit4Is1		; 2 if CARRY=1 continue on Bit4Is1
Bit4Is0
	JMP	SendB4		; 3 if CARRY=0, continue on SendB4
Bit4Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB4
	STA	$DD00		; 4 Write port A
					; *** bit 4 starts here ***
;---------------------------------------------------------------------------------------
CalcB5
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 5 to the carry flag
	BCS	Bit5Is1		; 2 if CARRY=1 continue on Bit5Is1
Bit5Is0
	JMP	SendB5		; 3 if CARRY=0, continue on SendB5
Bit5Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB5
	STA	$DD00		; 4 Write port A
					; *** bit 5 starts here ***
;---------------------------------------------------------------------------------------
CalcB6
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 C, para habilitar el RS232
	BCS Bit6Is1		; 2 if CARRY=1 continue on Bit6is1
Bit6Is0
	JMP SendB6		; 3 if CARRY=0, continue on SendB6
Bit6Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB6
	STA	$DD00		; 4 Write port A
					; *** bit 6 starts here ***
;---------------------------------------------------------------------------------------
CalcB7
	TXA				; 2 Get port A value back to .A
	ROR	TXBYTE		; 6 Copy TXBYTE bit 7 to the carry flag
	BCS	Bit7Is1		; 2 if CARRY=1 continue on Bit7Is1
Bit7Is0
	JMP	SendB7		; 3 if CARRY=0, continue on SendB7
Bit7Is1				; 1 One extra clock cycle if branching here
	ORA	#$04		; 2 Set .A bit 2
SendB7
	STA	$DD00		; 4 Write port A
					; *** bit 7 starts here ***
;---------------------------------------------------------------------------------------
CalcStop
	TXA				; 2 Get port A value back to .A
	ORA	#$04		; 2 Set .A bit 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
SendStop
	STA	$DD00		; 4 *** stop bit starts here ***
;---------------------------------------------------------------------------------------
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	NOP				; 2
	
	;NOP			;<-
	;NOP
	
	RTS

;---------------------------------------------------------------------------------------

;///////////////////////////////////////////////////////////////////////////////////
; ReadByte, receive a character and store it in RXBYTE
;---------------------------------------------------------------------------------------

ReadByte
	LDA	FLAGS1		; Ignore reception if the terminal is initializing
	AND	#%00000010
	BEQ	+
	JMP	CancelRX
+	LDA	$DD0D		; 4 Clear the interrupt flags (CIA2: NMI)
	LDA	#$90		; 2 Enable interrupts (NMI) by FLAG2
	STA	$DD0D		; 4
	LDY	#$04
	LDA	#$00		; 2 Set timer A to 64 ($0040) microseconds
	STA	$DC05		; 4
	LDA	#$40		; 2
	STA	$DC04		; 4
	LDA	#$19		; 2 Force load and start timer A
	STA	$DC0E		; 4
	LDA	#$06		; 2 Set RTS to 1 (and DTR to 1, enabling RS232)
	STA	$DD01		; 4
WaitStrt1
	LDA	$DC0D		; 4 If timer A has not elapsed
	AND	#$01		; 2
	BEQ	WaitStrt1	; 2 loop to WaitStrt and wait for the start bit
	STY	$DD01		; 4 Set RTS to 0 (and keep DTR enabled)
	LDA	#$07		; 2 Set timer A to 2000 ($07D0) microseconds
	STA	$DC05		; 4
	LDA	#$D0		; 2	<<<<<
	STA	$DC04		; 4
	LDA	#$19		; 2 Force load and start timer A
	STA	$DC0E		; 4
WaitStrt2
	LDA	$DC0D		; 4 If timer A has not elapsed
	AND	#$01		; 2
	BEQ	WaitStrt2	; 2 loop to WaitStrt and wait for the start bit
CancelRX
	LDA	#$10		; 2 Disable interrupt (NMI) by FLAG2
	STA	$DD0D		; 4
	LDA	#$04		; 2 Set RTS to 0 (and keep DTR enabled)
	STA	$DD01		; 4
	LDA	#$18		; 2 Stop timer A
	STA	$DC0E		; 4
	LDA	$DC0D		; 4 Clear CIA1 interrupt flags
	LDA	#$00		; 2 Store 0 in RXBYTE
	STA	RXBYTE		; 4
	CLC				; 2 Clear the carry flag (no reception)
	RTS				; 6

ReadByteLong		; Waits a longer time for the 1st character of the frame
	;LDA	FLAGS1	; Ignore reception if the terminal is initializing
	;AND	#%00000010
	;BEQ	+
	;JMP	CancelRX
+	LDA	$DD0D		; 4 Clear the interrupt flags (CIA2: NMI)
	LDA	#$90		; 2 Enable interrupt (NMI) by FLAG2
	STA	$DD0D		; 4
	LDY	#$04
	LDA	#$40		; 2 Set timer A to 16384 ($4000) microseconds
	STA	$DC05		; 4
	LDA	#$01		; 2
	STA	$DC04		; 4
	;LDA #$D0
	;STA $DC05
	LDA	#$19		; 2 Force load and start timer A
	STA	$DC0E		; 4
	LDA	#$06		; 2 Set RTS to 1 (and DTR to 1, enabling RS232)
	STA	$DD01		; 4
	BNE WaitStrt1

; NMI routine
StartBit			;
	;NOP
	;NOP
	;SEI
	STY	$DD01		; 4 Set RTS to 0 (keep DTR enabled)
	EOR $02			; 3
	NOP				; 2
	NOP				; 2
ReadB0
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB1
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB2
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	JMP	*+3			; 3 Waste 3 clock cycles
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB3
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB4
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB5
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	JMP	*+3			; 3 Waste 3 clock cycles
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB6
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
ReadB7
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	PLA				; 4 Remove the NMI call from the stack
	PLA				; 4
	PLA				; 4
	LDA	#$18		; 2 Stop timer A
	STA	$DC0E		; 4
	LDA	$DC0D		; 4 Clear CIA1 interrupt flags
	SEC				; 2 Set the carry flag (character received)
	;CLI
	RTS				; 6


;//////////////////////////////////////////////////////////////////////////////////////////
; TurboRX, receives a byte stream and plays it as nibbles through the SID volume register
;------------------------------------------------------------------------------------------

TurboRX
	LDA	#$00		; Set the keyboard matrix for direct reading
	STA	$DC03		; port b ddr (input)
	LDX	#$FF
	STX	$DC02		; port a ddr (output)
	STA	$DC00

	LDA	#$06		; 2 Set RTS to 1 (and DTR to 1)
	STA	$DD01		; 4
TurboLoop
	LDA	#$01		; 2 Wait for the start bit
TRXWaitSt			; 1 extra cycle if coming from the BNE
	BIT	$DD01		; 4 Text port B bit 0 (RX)
	BNE	TRXWaitSt	; 2 Loop if there's no start bit yet

	TXA				; 2
	AND	#$0F		; 2
	STA	SIDREG + 24	; 4 Play nibble

	TXA				; 2
	ROR				; 2
	ROR				; 2
	ROR				; 2
	TAX				; 2

TurboB0				; 1 One extra clock cycle if branching here
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	TXA				; 2
	ROR				; 2
	TAX				; 2 
TurboB1
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	NOP				; 2
TurboB2
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	;NOP			; 2
	TXA				; 2
	AND	#$0F		; 2
	TAX				; 2 
TurboB3
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	TXA				; 2
	STA	$D020		; 4 Change border color
TurboB4
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB

	STX	SIDREG + 24	; 4 Play the nibble
TurboB5
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	NOP				; 2 
	NOP				; 2
TurboB6
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
TurboB7
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	LDA	RXBYTE		; 4 Load received character into .A
	BEQ	TurboExit	; 2/3 Exit routine if received character was $00
	TAX				; 2
	BIT	$DC01		; 4 Check keyboard
	BMI	TurboLoop	; 4/2 Loop if not cancelled

; Send cancel character
	LDA	#$01		; 2 Wait for the start bit
TRXSend
	BIT	$DD01		; 4 Check port B bit 0 (RX)
	BNE	TRXSend		; 2 if RX = 1 (no start bit) loop to TRXSend
	; Start bit
	LDA	$DD00		; 4 Read PRA (port A, CIA2)
	AND	#$FB		; 2 Set TX to 0 (start bit)
	STA	$DD00		; 4 Write port A (start bit)
	JMP	*+3			; 3 Waste 3 clock cycles
	LDA	$DD00		; 4 Read PRA wasting 4 cycles
	LDA	$DD00		; 4 Read PRA (port A, CIA2)
	ORA	#$04		; 2 Set .A bit 2
	STA	$DD00		; 4 Write port A (start bit ends here)
	; Data
	LDY	#27			; 2
TRXDelay			; Waste time to send $00
	DEY				; 2
	BNE	TRXDelay	; 2 (+1 if branching to TRXDelay)
	LDA	#$FF		; Disable keyboard reading
	STA	$DC00
	JMP	TurboLoop	; 3 Loop back to play routine
TurboExit			; Exit
	LDA	#$04		; 2 Set RTS to 0 (Keep DTR on 1)
	STA	$DD01		; 4
	RTS

}

;////////////////////////////////////////////////
; SID streaming interrupt routine
;////////////////////////////////////////////////

c84_irq1:
	;---
	PHA
	TXA
	PHA
	TYA
	PHA
	;---

	SEI

	LDX #$FF
	STX $D019			; Clear interrupt flags
	INX
	STX $D01A			; Disable raster interrupts

c84start
	INC $D020
	
	; Receive packet size
!if _HARDTYPE_ = 56 {
-	JSR ReadByteLong	; Receive a character from RS232 using a longer RTS pulse
} else {
-	JSR ReadByte
}
	BCC -	;ci1end2	; No character received, wait until the next frame
	LDA RXBYTE
	BEQ ci1exit			; Packet size = 0: exit streaming mode
	CMP #$FF
	BEQ -				; Check for sync character and retries to receive the packet size if found


	SEC
	SBC #$05			; substract register bitmap + 1
	STA STREAMCNT

	; Receive SID register bitmap
	LDX #$03
-	JSR ReadByte
	BCC -
	LDA RXBYTE
	STA STREAMBM,X		; Save
	DEX
	BPL -

	; Receive register values
	LDX STREAMCNT
	BMI +				; branch if there's no register data to receive

-	JSR ReadByte
	BCC -

	LDA RXBYTE
	STA SIDREGS,X		; Store in the buffer

	DEX
	BPL -

	; LDA #00
	; STA $D020
	; Reply with STREAMCNT
	;LDA #$05
	;CLC
	;ADC STREAMCNT
	;+	JSR	SendByte
;---------------<<<
	;BPL +
	;LDA #$02
	;STA STREAMFLAG

	;<<<< SID Stuff here
+
-	;LDA $D012			; Raster sync
	;CMP #150
	;BCS -

+	INC $D020
	LDA #$00			; Current register
	LDY STREAMCNT		; Number of register to write-1
	BMI ci1end			; Exit loop if there's no registers to write
--	LDX #$03
	CLC
-	ROR STREAMBM,X
	DEX
	BPL -
	; C contains the current register bit from the bitmap
	TAX
	BCS +				; Carry set, seach and write the SID register
	; Carry clear, skip to the next SID register, without reading value
	INX
	TXA
	BNE --				; Get the next bit from the bitmap

; Here .X = .A register/table index
; .Y register buffer index
+	PHA					; Save table index
	LDA regtable,X		; Get register from translation table
	TAX
	LDA SIDREGS,Y		; Get register value
sidwrite
	STA $D400,X			; And write it
	PLA					; Retreive table index
	TAX
	INX					; Increment the register number to be written
	TXA
	DEY					; Decrement the number of registers left to write
	BPL --
	;BMI ci1end
;---------------------

ci1end
	DEC $D020
	DEC $D020

+	DEC SIDSTREAM_SYNC
	BNE ci1end2	;+		; If it is not time to send the sync character yet, go to the irq exit
	LDA #100			; Wait a 100 frames until next sync character
	STA SIDSTREAM_SYNC

	LDA STREAMFLAG		; Send the sync character
	;CMP #$02
	;BEQ +
!if _HARDTYPE_ = 56 {
	JSR _SendByte
} else {
	STA	T232DATA
}
	JMP ci1end2			;c84start

ci1exit
	STA STREAMFLAG		; Clear the interrupt flag, exit streaming mode
	DEC $D020
ci1end2
	LDA #$01
	STA $D01A			; Enable raster interrupts
	CLI
	PLA
	TAY
	PLA
	TAX
	PLA
	RTI
	;JMP $EA31

;-----------------------------------------
;Register translation table
;Specifies write order for SID registers
regtable:
	!for i, 0, 24 {!byte i}
	;!fill 24,0

; Reset Register translation table
RTTReset:
	LDX #24
-	TXA
	STA regtable,x
	DEX
	BPL -
	RTS
;-----------------------------------------
; BEEP Sprites
SpriteSetup:
	LDX #$00	;Copy both sprites
-	LDA SPON,X
	STA $0340,X
	LDA #$FF
	STA $0380,X
	INX
	BPL -
	LDA #$00
	STA $D01C
	STA $D017
	STA $D01D
	LDA #$03
	STA $D010
	LDX #57
	STX $D001
	STX $D003
	INX
	STX $D000
	STX $D002
	LDX #$0D
	STX $D027	; Sprite 0 Cyan
	INX
	STX $07F8	; Speaker
	INX
	STX $07F9	; Background
	LDX #$0C
	STX $D028	; Sprite 1 Grey2
	RTS


SPON:
!byte $00,$00,$00,$00,$00,$00,$00,$18
!byte $00,$00,$28,$18,$00,$48,$60,$00
!byte $89,$80,$01,$08,$00,$0e,$08,$00
!byte $12,$08,$00,$12,$08,$00,$12,$09
!byte $f8,$12,$08,$00,$12,$08,$00,$0e
!byte $08,$00,$01,$08,$00,$00,$89,$80
!byte $00,$48,$60,$00,$28,$18,$00,$18
!byte $00,$00,$00,$00,$00,$00,$00,$03

SPMUTE:
!byte $00,$00,$00,$00,$00,$00,$00,$18
!byte $00,$00,$28,$00,$00,$48,$00,$00
!byte $88,$00,$01,$08,$00,$0e,$09,$04
!byte $12,$08,$88,$12,$08,$50,$12,$08
!byte $20,$12,$08,$50,$12,$08,$88,$0e
!byte $09,$04,$01,$08,$00,$00,$88,$00
!byte $00,$48,$00,$00,$28,$00,$00,$18
!byte $00,$00,$00,$00,$00,$00,$00,$03

EXTRAEND
}
_EXTRAEND_
_PALSTART_
!if _HARDTYPE_ = 56 {
!pseudopc TurboRX{
; PALB alternative TurboRX routine
PALSTART

_TurboRX
	LDA	#$00		; Set the keyboard matrix for direct read
	STA	$DC03		; port b ddr (input)
	LDX	#$FF
	STX	$DC02		; port a ddr (output)
	STA	$DC00

	LDA	#$06		; 2 RTS to 1 (DTR = 1)
	STA	$DD01		; 4
_TurboLoop
	LDA	#$01		; 2 Wait for the start bit
_TRXWaitSt			; 1 extra cycle if branching here
	BIT	$DD01		; 4 Check port B bit 0 (RX)
	BNE	_TRXWaitSt	; 2 if RX = 1 (no start bit) loop to TRXWaitSt

	TXA				; 2
	AND	#$0F		; 2
	STA	SIDREG + 24	; 4

	TXA				; 2
	ROR				; 2
	ROR				; 2
	ROR				; 2
	TAX				; 2

_TurboB0			; 1 One extra clock cycle if branching here
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	TXA				; 2
	ROR				; 2
	TAX				; 2 
_TurboB1
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	;NOP			; 2 <<<<<<<<<<<< TST PAL-B
	NOP				; 2
_TurboB2
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	;NOP			; 2
	TXA				; 2
	AND	#$0F		; 2
	TAX				; 2 
_TurboB3
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	TXA				; 2
	STA	$D020		; 4 Change the border color
_TurboB4
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB

	STX	SIDREG + 24	; 4
_TurboB5
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	;NOP			; 2 <<<<<<<<<<<<<<<<<<<<<< PAL- B TST
	;NOP			; 2 <<<<<<<<<<<<<<<<<<<<<< PAL TST
_TurboB6
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	NOP				; 2
	JMP	*+3			; 3 Waste 3 clock cycles
_TurboB7
	LDA	$DD01		; 4 Load port B into .A
	ROR				; 2 Copy bit 0 (RX) to the CARRY flag
	ROR	RXBYTE		; 6 Push the CARRY flag into RXBYTE's MSB
	LDA	RXBYTE		; 4 Load the received character into .A
	BEQ	_TurboExit	; 2/3
	TAX				; 2
	BIT	$DC01		; 4 Check keyboard
	BMI	_TurboLoop	; 4/2 Keep playing if there's no keypress

; Send cancel character
	LDA	#$01		; 2 Wait for the start bit
_TRXSend
	BIT	$DD01		; 4 Check port B bit 0 (RX)
	BNE	_TRXSend	; 2 if RX = 1 (no start bit) loop to TRXSend
	; Start bit
	LDA	$DD00		; 4 Read PRA (puerto A, CIA2)
	AND	#$FB		; 2 Set TX to 0 (start bit)
	STA	$DD00		; 4 Write port A (start bit)
	JMP	*+3			; 3 Waste 3 clock cycles
	LDA	$DD00		; 4 Read PRA waste 4 cycles
	LDA	$DD00		; 4 Read PRA (port A, CIA2)
	ORA	#$04		; 2 Set .A bit 2
	STA	$DD00		; 4 Write port A (ends start bit)
	; Data
	LDY	#26			; 2<<<<<<<<<<<<<<<<<<<<<< PAL TST change was #27
_TRXDelay			; Waste time sending $00
	DEY				; 2
	BNE	_TRXDelay	; 2 (+1 if branching to TRXDelay)
	LDA	#$FF		; Disable keyboard reading
	STA	$DC00
	JMP	TurboLoop	; 3
_TurboExit
	LDA	#$04		; 2 Set RTS to 0 (DTR = 1)
	STA	$DD01		; 4
	RTS

PALEND
}
}
_PALEND_