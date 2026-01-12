;////////////////////////////////////////////////////////////////////////////////////////////
; BBS PETSCII compatible terminal, RS232 with tcpser/BBSServer or wifi with zimodem firmware
; Supports TURBO56K v0.7 protocol at 19200 bps, using TX, RX and RTS
;////////////////////////////////////////////////////////////////////////////////////////////
; 
; Constants

	STROUT	= $9088			; BASIC String out routine
	CHROUT	= $FFD2			; Kernal CHROUT (prints a single character)
	STOP	= $FFE1			; Kernal STOP (checks STOP key)
	GETIN	= $FFE4			; Kernal GETIN
	PLOT	= $FFF0			; Kernal PLOT
	CHKIN	= $FFC6			; Kernal CHKIN
	READST	= $FFB7			; Kernal READST
	CHRIN	= $FFCF			; Kernal CHRIN
	LISTEN	= $FFB1			; Kernal LISTEN
	SECLSN	= $FF93			; Kernal SECLSN
	UNLSN	= $FFAE			; Kernal UNLSN
	SETNAM	= $FFBD			; Kernal SETNAM
	SETLFS	= $FFBA			; Kernal SETLFS
	KOPEN	= $FFC0			; Kernal OPEN
	KCLOSE	= $FFC3			; Kernal CLOSE
	CHKOUT	= $FFC9			; Kernal CHKOUT
	CLRCHN	= $FFCC			; Kernal CLRCHN

	COLORMEM = $0800		; Character Colors

	CURRLINEPTR = $C8		; Current text line pointer (zero page)
	CURRLINE = $CD
	CURRCOLUMN = $CA		; Current column (zero page)
	CURRCOLLINE = $EA		; Current text line color porinter (zero page)
	ACIABASE = $FD00		; ACIA 6551 base address
	ACIADATA = ACIABASE+0	; Registro de datos del ACIA 6551
	ACIASTATUS = ACIABASE+1	; Registro de estado del ACIA 6551
	ACIACOMMAND = ACIABASE+2; Registro de comando del ACIA 6551
	ACIACONTROL = ACIABASE+3; Registro de control del ACIA 6551
	BEEPTIME = 4		; Cantidad minima de cuadros que dura un beep de impresion
	PrBuffer = ENDOFCODE	; Buffer de impresion

	MAXCMD = $B7			; Highest command implemented

!ifndef _MAKE_{
	!to "rt_p4_v0.20.prg", cbm
    !sl "labels-p4.txt"
}


;///////////////////////////////////////////////////////////////////////////////////
; MACROSS
;///////////////////////////////////////////////////////////////////////////////////
!macro SetCursor .col, .row {
	CLC
	LDY #.col
	LDX #.row
	JSR PLOT
}

!macro StringOut .addr {
	LDA #<.addr
	LDY #>.addr
	JSR STROUT
}

!macro DisROMs {
	STA $FF3F
}

!macro EnROMs {
	STA $FF3E
}

!macro _Version_{	; Insert version number string
	!src "version.asm"
}

	*= $1001

;///////////////////////////////////////////////////////////////////////////////////
; BASIC Header
;///////////////////////////////////////////////////////////////////////////////////

	!byte $0C, $10, $E4, $07, $9E, $20, $34, $31, $31, $32, $00	; 10 SYS 4112
	!byte $00, $00

	*= $1010

;////////////////////////////////////////////////////////////////////////////////////////////
; Start (4110)
;////////////////////////////////////////////////////////////////////////////////////////////

_Start:

		LDX #$11
.c0		LDY #$08		;<-
.c2		LDA _DATA2,X	;<-
		;BEQ .c1
		STA $0058,Y
.c1		DEX				;<-
		DEY
		BPL .c2
		TXA
		PHA
		TYA
		PHA
		JSR $88C7		; >>Open space in memory
		PLA
		TAY
		PLA
		TAX
		BPL .c0
	
	LDA	#113			; White screen and border
	STA	$FF15
	STA	$FF19

	LDA	#<InitScr	; Black ink, clear screen, block C= + SHIFT, switch to lowercase
	LDY	#>InitScr
	JSR	STROUT		;PrintTxt

; Copy LogoScr to 3352 ($0D18)
	LDX	#200
-	LDA	LogoScr-1,X
	STA	$0D18-1,X
	LDA	LogoScr+200-1,X
	STA	$0D18+200-1,X
	DEX
	BNE	-
; Copy LogoClr to 2328 ($0918)
	LDX	#200
-	LDA	LogoClr-1,X
	STA	$0918-1,X
	LDA	LogoClr+200-1,X
	STA	$0918+200-1,X
	DEX
	BNE	-

; Print retrocomputacion URL

	JSR InitPalette		; Needed because exomizer scrambles the system palette table

	CLC					; Set the cursor to row 20, column 4
	LDX	#20
	LDY	#4
	JSR	PLOT
	LDA	#<RetroIntro	; Print: www.retrocomputacion.com
	LDY	#>RetroIntro
	JSR	STROUT
	LDA #$81
	STA $A5
-	LDA $A5
	BNE -

	LDA $AE
	STA load_drive

	JSR DrvDetect

	LDA $FF07
	ASL
	AND #$80
	+DisROMs
	ORA _sub0+1
	STA _sub0+1			; Set refresh rate 
	+EnROMs

	LDX #$07
-	LDA FTable,X
	STA $0567,X
	LDA #$01
	STA $055F,X
	DEX
	BPL -

	JSR earlysetup
	JMP CODESTART

;Detect present drives for devices 8-15
DrvDetect:
--      JSR chdrive
        LDA result
        BEQ ++            ; Not found? continue with next device
        LDA #IDQTY
        STA IDtmp
-       LDA IDtmp
        ASL
        TAY
        LDA IDptr,Y
        LDX IDptr+1,Y
        JSR cmpstr
        BPL +           ;If found ->
        DEC IDtmp       ;not found, try next ID string
        BPL -
        ; found or out of ID strings
+       LDA device
        SEC
        SBC #$08
        TAX
        LDA IDtmp
        STA DRVlst,X

++      INC device
        LDA #$10
        CMP device
        BNE --

		;Copy drive list
		SEI
		+DisROMs
		LDX #$07
-		LDA DRVlst,X
		STA DRIVES,X
		DEX
		BPL -
		+EnROMs
		CLI
		RTS

chdrive:
;first check if device present
        LDA #$00
        STA $90			; clear STATUS flags

        LDA device		; device number
        JSR LISTEN		; call LISTEN
        LDA #$6F		; secondary address 15 (command channel)
        JSR SECLSN		; call SECLSN (SECOND)
        JSR UNLSN		; call UNLSN
        LDA $90		; get STATUS flags
        BNE .devnp		; device not present

        LDA #cmd_end-cmd
        LDX #<cmd
        LDY #>cmd
        JSR SETNAM		; call SETNAM

        LDA #$0F		; file number 15
        LDX device       ; last used device number
        BNE +
        LDX #$08		; default to device 8
+  		LDY #$0F		; secondary address 15
        JSR SETLFS		; call SETLFS

        JSR KOPEN		; call OPEN
        BCS ++			; if carry set, the file could not be opened
        LDX #$0F		; filenumber 15
        JSR CHKIN		; CHKIN file now used as input
        LDY #$03
-		JSR READST		; call READST (read status byte)
        BNE +			; either EOF or read error
        JSR CHRIN		; call CHRIN (get a byte from file)
        DEY				; skip first 3 chars
        BNE -
-	    JSR READST		; call READST
        BNE +
        JSR CHRIN		; call CHRIN
        STA result,Y
        INY
        JMP -			; next byte
+
-       LDA #$00
        STA result,Y	; Null terminate result string
        LDA #$0F
        JSR KCLOSE		; call CLOSE

        JMP CLRCHN		; call CLRCHN
;        RTS
++
        ;... error handling for open errors ...
        LDY #$00
        BEQ -			; even if OPEN failed, the file has to be closed
.devnp
        LDY #$00
        STY result      ; 'clear' result string
        RTS

cmpstr: ;Find substring, ID string addr in .a/.x. Return .x :$00 if found, $FF otherwise
        STA $D8
        STX $D9
        LDX #$FF
        STX match
        ; Iterate ID string until finding the 1st character of the substing
        LDY #$FF
--      INY
-       INX
        LDA ($D8),Y
        BEQ ++			; If null, exit
        CMP result,X
        BNE +
        ; match
        LDA #$00
        STA match
        BEQ --			; continue with the next character in both strings
        ; no match
+       LDA #$FF
        STA match
        LDA result,X
        BEQ ++			; end of ID string?
        LDY #$00		; reset substring index
        BEQ -
++      LDX match
        RTS


device:		!byte $08
cmd:		!text "UI",$0d          ; command string
cmd_end:
match		!byte $ff               ; string matched $00 or not $ff

result:		!fill $40, $00          ; Drive response tmp string
DRVlst:		!fill $08, $ff          ; Available Drive list ($FF = not found)
; Expected ID substrings
ID1541:		!text "1541",$00		; #ID 10
ID1551:		!text "TDISK",$00		; #ID 9
ID1570:		!text "1570",$00		; #ID 8
ID1571:		!text "1571",$00		; #ID 7
ID1581:		!text "1581",$00		; #ID 6
IDCMDFD:	!text "CMD FD",$00		; #ID 5
IDSD2IEC:	!text "SD2IEC",$00		; #ID 4
IDULTI:		!text "ULTIMATE",$00	; #ID 3
IDVICEFS:	!text "VICE",$00		; #ID 2
IDVICE:		!text "VIRTUAL",$00		; #ID 1
IDPI1541:	!text "PI1541",$00		; #ID 0

IDQTY = 11-1

IDptr: !word IDPI1541,IDVICE,IDVICEFS,IDULTI, IDSD2IEC, IDCMDFD, ID1581, ID1571, ID1570, ID1551, ID1541
IDtmp: !byte IDQTY

_DATA2:
!word ENDOFCODE, _ENDOFCODE_
!byte $00, $00, $00
!word _CODESTART_

!word	ENDSHADOW, _ENDSHADOW_
!byte	$00, $00, $00
!word	_SHADOWCODE_

;///////////////////////////////////////////////////////////////////////////////////
; Retrocomputacion logo

LogoScr
	!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,225,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
	!byte 32,124,226,226,226,226,226,226,226,226,126,255,226,226,108,226,226,127,124,251,226,108,226,226,126,255,226,226,123,226,226,226,226,226,226,226,226,226,126,32
	!byte 124,226,226,226,226,226,226,226,226,226,126,97,32,32,225,226,226,226,32,225,32,225,32,32,32,97,32,32,97,226,226,226,226,226,226,226,226,226,226,126
	!byte 124,226,226,226,226,226,226,226,226,226,126,126,32,32,32,226,226,226,32,32,226,124,32,32,32,124,226,226,32,226,226,226,226,226,226,226,226,226,226,126
	!byte 32,124,226,226,226,226,226,226,226,226,126,226,226,226,124,226,226,226,124,226,226,124,226,226,126,226,226,226,126,226,226,226,226,226,226,226,226,226,126,32
	!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,225,32,32,32,32,32,32,32,32,32,124,32,32,32,32,32,32,32,32
	!byte 108,226,226,226,108,226,226,127,225,226,236,127,225,226,226,127,225,32,32,225,124,251,226,32,226,226,127,108,226,226,226,225,108,226,226,127,225,226,226,123
	!byte 225,32,32,32,225,32,32,225,225,32,97,225,225,32,32,225,225,32,32,225,32,225,32,108,226,226,251,225,32,32,32,225,225,32,32,225,225,32,32,97
	!byte 32,226,226,226,32,226,226,126,124,32,126,124,225,226,226,126,32,226,226,126,32,32,226,32,226,226,226,32,226,226,226,124,32,226,226,126,124,32,32,126
	!byte 124,226,226,226,124,226,226,226,124,226,226,226,124,124,226,226,124,226,226,226,124,226,226,124,226,226,226,124,226,226,226,124,124,226,226,226,124,226,226,126
LScrEnd

LogoClr
	!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	!byte 0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0
	!byte 103,103,103,103,103,103,103,103,103,103,103,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,0,0,103,103,103,103,103,103,103,103,103,103,103
	!byte 101,101,101,101,101,101,101,101,101,101,101,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,0,50,101,101,101,101,101,101,101,101,101,101,101
	!byte 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
	!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	!byte 70,70,70,70,70,70,70,70,70,70,70,70,0,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
LClrEnd


_CODESTART_
!pseudopc $7000 {
CODESTART:
	JMP	MainPrg		; Jump to MainPrg

	; Variables

TXBYTE			!byte $00		; Byte to be transmitted
RXBYTE			!byte $00		; Last received byte
CMDFlags		!byte $00		; Command flags
								; Bit 7 (N): 1 = Last received byte is CMDON ($ff); 0 = Normal operation
								; Bit 6 (V): 1 = Receive N bytes as parameters and wait for the command to complete; 0 = Normal operation
								; Bits 3-0: Parameter counter for bit-6
TEMPCNT1		!byte $00		; Temporal counter 1
BYTECNT			= $61			;!word $0000		; 16-bit block transfer remaining bytes to receive counter
BLOCKPTR		!word $0000		; Memory pointer for the block transfer command

BORDERCLR		!byte $00		; Current screen border color backup
FLAGS1			!byte $00		; Status flags
								; Bit 7 (N): 1 = Command mode; 0 = Normal mode
								; Bit 6 (V): 1 = Last byte should be redirected to the voice synth; 0 = Last byte is meant for the terminal
								; Bit 4	   : 1 = Split screen enabled
								; Bit 3    : 1 = Cursor blink enabled; 0 = Cursor blink disabled
								; Bit 2    : 1 = Microsint enabled; 0 = Microsint disabled / not found
								; Bit 1    : 1 = Terminal is starting up; 0 = Normal operation
								; Bit 0    : 1 = RUN/STOP pressed, returning to BASIC
; -----
PRBUFFERCNT		!byte $00		; Print buffer byte counter
PRINDEXOUT		!byte $00		; Index to the print buffer byte to output next
PRINDEXIN		!byte $00		; Index to the first free byte in the print buffer
; -----
PRBYTE			!byte $00		; |UNUSED| Byte to be printed on screen
PRSPEED			!byte $00		; Text print delay (0:no delay)

WTOP			!byte $00		; Text window top limit
WBOTTOM			!byte $19		; Text window bottom limit
TIMER1			!byte $00		; Timer decremented on each interrupt call
CRSRTIMER		!byte $00		; Cursor blink timer, decremented on each interrupt call
CRSRCHAR		!byte $00		; Character currently under the cursor
CRSRCOLOR		!byte $00		; Color of the character under the cursor
TEMPCNT2		!byte $00		; Temp counter 2
BEEPTIMER		!byte $00		; Timer que decrementa en cada interrupcion
TIMERDIV4		!byte $00		; Timer que decrementa en cada interrupcion, usado para realizar acciones cada 4 interrupciones
DELAYTIME		!byte $00		; Parametro (tiempos) de las rutinas de temporizacion
SNDSTATUS		!byte $00		; Character beep enable
FLASHSTATUS		!byte $00		; FLASHON control code enable
SPLITRST		!byte $00		; Split screen raster line

;///////////////////////////////////////////////////////////////////////////////////
; ReadByte, receive a byte, store in RXBYTE
;---------------------------------------------------------------------------------------

ReadByte
	LDA	FLAGS1		; Ignore reception if the terminal is initializing
	AND	#%00000010
	BNE	CancelRX
EnRTS
rb1l:
	LDA #$1C
	STA $FF02
rb1h:
	LDA #$00
	STA $FF03		; Wait ~30uS
	LDA #%10010000	; Clear Timer 2 IRQ
	STA $FF09

	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, enable transmision), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
	LDA #$10
WaitRX1
	BIT $FF09
	;AND #$10
	BEQ WaitRX1

DisRTS
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, disable transmision), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
	LDA	#$EE		;$D2		; 2 Set Timer A to 2000 ($06EE) microseconds
	STA	$FF02		; 4
	LDA	#$06		;$01		; 2
	STA	$FF03		; 4
	LDA	#%10010000	; 2 Clear Timer 2 IRQ bit
	STA	$FF09		; 4
WaitRX2
	LDA	ACIASTATUS	; Check if a byte has arrived
	AND	#%00001000
	BNE	Received	; 3 If the time has elapsed, (85+174 us with no reception), continue on CancelRX
	LDA	$FF09		; 4 If 85 microseconds have not yet elapsed on Timer A...
	AND	#%00010000	; 2
	BEQ	WaitRX2		; 2 Loop back to WaitRX2 to give enough for a byte to arrive
CancelRX
	LDA	#$00		; 2 Set RXBYTE to 0
	STA	RXBYTE		; 4
	CLC				; 2 Reset CARRY (no reception)
	RTS				; 6 Return
Received
	LDA	ACIADATA	; 4 Read the received byte
	STA	RXBYTE		; 4 and store it in RXBYTE
	SEC				; 2 Set CARRY (byte received)
	RTS				; 6 Return

;//////////////////////////////////////////////////////////////////////////////////////////
; TurboRX, receives a byte stream and plays it as nibbles through the TED volume register
;------------------------------------------------------------------------------------------

TurboRX
	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, enable reception), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
TurboLoop
	LDA #$FB		; Set keyboard latches
	STA $FD30
	STA $FF08
	TXA				; 2 
	AND	#$0F		; 2
	TAY
	LDA trtable,Y
	STA	$FF11		; 4 Write the first sample
	STA	$FF19		; 4 <-

	TXA				; 2 Prepare second sample
	ROR				; 2
	ROR				; 2
	ROR				; 2
	ROR				; 2
	AND	#$0F		; 2
	;TAX				; 2	<-18 cycles until here
TRXWait1
; Wait 86 - 16 - 18 cycles for 11520Khz
; Wait 130 - 16 - 18 cycles for 7680KHz
	LDY #$41
-	DEY			; 2
	BNE -		

	TAY

	LDA trtable,Y
	STA	$FF11		; 4 Write second sample
	STA	$FF19		; 4
TRXWait2
-
	LDA	ACIASTATUS	; 4 Wait for a character
	AND	#%00001000	; 2
	BEQ	-			; 3
	LDA	ACIADATA	; 4 .A = Received character
	TAX				; 2
	BEQ TurboExit

	LDA $FF08		; 4 Key press?
	CMP #$FF
	BEQ TurboLoop			; 3
	LDA #$FF		; Yes, send $FF
	STA ACIADATA
	JMP	TurboLoop	; 3

TurboExit
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, reception disabled), no rx irq, rx enabled (dtr=0)
_acomm4
	STA	ACIACOMMAND
	RTS
; 4-bit sample to 13 TED amplitudes
trtable:
    !byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$B5,$B5,$B6,$B7,$B7,$B8,$B8


;///////////////////////////////////////////////////////////////////////////////////
; Programa principal
;///////////////////////////////////////////////////////////////////////////////////

MainPrg
	JSR InitPalette
	JSR $D88B		; Clear Screen (Makes sure the Screen link table is valid from here on)
	SEI				; Disable interrupts

	LDA	#$00		; Black screen and border
	STA	$FF15
	STA	$FF19
	STA $0540		; Repeat Space, INS/DEL and cursor keys only

	LDA	#%00000010	; no parity, no echo, no tx irq (rts=1), no rx irq, rx disabled (dtr=1)
	STA	ACIACOMMAND
	LDA	#%00011111	; 1 stop bit, 8 data bits, baud rate generator, 19200 bps
;	LDA	#%00011000	; 1 stop bit, 8 data bits, baud rate generator, 1200 bps
	STA	ACIACONTROL
	LDA	#$00		; Inicializa el buffer de impresion
	STA	PRINDEXOUT
	STA	PRINDEXIN
	STA	PRBUFFERCNT
	LDA	#%00001010	; Terminal is initializing
	STA	FLAGS1
	LDA	#32			; Sets a space as initial character under the cursor
	STA	CRSRCHAR
	LDA	#4			; Init TIMERDIV4
	STA	TIMERDIV4
	LDA	#0			; Reset beep counters
	STA	TEMPCNT1
	STA	TEMPCNT2

ResetFkeys
	LDX #$07
-	LDA FTable,X
	STA $0567,X
	LDA #$01
	STA $055F,X
	DEX
	BPL -

ConfigTED
	LDA	#%00000000	; Volume = 0 (min), disable channels 1 & 2 and reset the oscilators
	STA	$FF11
	LDA #$03
	STA $FF10		; Voice 2 freq
	LDA #$9a
	STA $FF0F
	LDA	#$10		; Enable TED screen
	ORA	$FF06
	STA	$FF06
;	LDA	#%11111110	; Clear most significant bit in TED's raster register
;	AND	$FF0A
;	STA	$FF0A
	LDA	#204		; Set the raster line number where interrupt should occur
	STA	$FF0B
	LDA	#<Irq		; Set the interrupt vector to point to interrupt service routine below
	STA	$0314
	LDA	#>Irq
	STA	$0315
	LDA	#%10100010	; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A

	; Set STREND variable (needed by the PAINT ROM routine)
	LDA #$00
	STA $31
	LDA #$40
	STA $32

	CLI
	LDX	#<Version	; Carga en el buffer la cadena Version (RETROTERM...)
	;STA	AddTLoop + 1
	LDY	#>Version
	;STA	AddTLoop + 2
	JSR	AddText		; y la agrega al buffer de impresion
	LDA	#1		; Velocidad: 1 caracter por cuadro
	STA	PRSPEED
	STA	TIMER1
	;JSR	StopCRSR
	JSR	PrintBuffer	; Procesa el buffer de impresion
	;JSR	StartCRSR

	LDA	#100		; Espera 2 segundos
	STA	PRSPEED
	STA	TIMER1
	LDA	#0		; Inicializa el temporizador del cursor para que empiece encendido
	STA	CRSRTIMER
Wait1	JSR	BlinkCRSR	; Procesa el parpadeo del cursor
	LDA	TIMER1
	BNE	Wait1

	LDA	#$00		; A partir de ahora imprime sin retardo
	STA	PRSPEED
	STA	TIMER1

	LDX	#<Msg06		; Pointer to the Msg06 string (credits/Turbo56K version)
	;STA	AddTLoop + 1
	LDY	#>Msg06
	;STA	AddTLoop + 2
	JSR	AddText		; and it's added to the print buffer
	JSR	PrintBuffer	; Process the print buffer

	LDA	#0		; Inicializa el temporizador del cursor para que empiece encendido
	STA	CRSRTIMER
	LDA	FLAGS1		; Se termino de inicializar la terminal
	AND	#%11111101
	STA	FLAGS1
PrintRX
	LDA	PRBUFFERCNT	; Si no hay elementos en el buffer para imprimir, sigue en Blink
	BEQ	Blink
	LDA	TIMER1		; Si no es momento de imprimir (TIMER1<>0), sigue en Blink
	BNE	Blink
	;JSR	StopCRSR
	JSR	PrintBuffer	; Procesa el buffer de impresion
	;JSR	StartCRSR
Blink
	LDA	#%00000001	; Verifica si hay que volver al BASIC
	AND	FLAGS1
	BEQ	+
	JMP	ExitPrg		; si es asi, sigue en ExitPrg para salir del programa
+	JSR	BlinkCRSR	; sino, procesa el parpadeo del cursor
	JMP	PrintRX		; y vuelve a PrintRX

;///////////////////////////////////////////////////////////////////////////////////
; Blink the cursor
;///////////////////////////////////////////////////////////////////////////////////

BlinkCRSR
	LDA FLAGS1
	AND #%00001000
	BEQ BlinkEnd
	LDA	CRSRTIMER	; Continue to BlinkEnd if this is not the time to reverse the cursor (CRSRTIMER<>0)
	BNE	BlinkEnd
	LDA	#30			; Reset CRSRTIMER
	STA	CRSRTIMER
	LDX CRSRCOLOR
	LDY	CURRCOLUMN
	LDA	#%10000000	; reverse the character under the cursor
	EOR	(CURRLINEPTR), Y
	STA	(CURRLINEPTR), Y
	BPL +
	LDX $053B
+	TXA
	STA ($EA),Y
BlinkEnd
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Disables the cursor, restores the character under it
;///////////////////////////////////////////////////////////////////////////////////

StopCRSR
	LDY	CURRCOLUMN
	LDA	CRSRCHAR
	STA	(CURRLINEPTR), Y
	LDA CRSRCOLOR
	STA ($EA),Y
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Enable the cursor, saves the character under it
;///////////////////////////////////////////////////////////////////////////////////

StartCRSR
	LDY	CURRCOLUMN
	LDA	(CURRLINEPTR), Y
	STA	CRSRCHAR
	LDA ($EA),Y
	STA CRSRCOLOR
	LDA	#0		; Inicializa el temporizador del cursor para que empiece encendido
	STA	CRSRTIMER
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Print the entire buffer
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
	JMP StartCRSR
;	RTS
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
    BCC ++			; Is it less than or equal to the highest implemented command ($B6)?
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
	JMP .pb0

;///////////////////////////////////////////////////////////////////////////////////
; CHROUT replacement, equivalent to the Kernal routine but no quote mode, 40
; logical columns and text window support
; Petscii -> Screen code by Mace (from Codebase64)
;///////////////////////////////////////////////////////////////////////////////////
CharOut:
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
    JSR $E008           ; Update Attributes
    INY
    CPY #40             ; Reached right border?
    BNE cu1				; No, go update screen pointers
    LDY #00             ; Wrap to column 0
    INX
    CPX WBOTTOM         ; Pass bottom window border?
    BCC cu1            	; No, go update screen pointers
    DEX
	TXA
	PHA
	TYA
	PHA
    JSR $DA89       	; Scroll text window
	PLA
	TAY
	PLA
	TAX

cu1 STY CURRCOLUMN
    STX CURRLINE
    JSR $D8AA           ; Set start of line (.X = line)
    ;JSR $EA24          ; Synchronize color RAM pointer
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
    JSR $DAF7			; Clear Line
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
	TXA
	PHA
	JSR $DA89			; Yes, scroll text window
	PLA
	TAX
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
	TXA
	PHA
	TYA
	PHA
    JSR $DA89			; Yes, scroll
	PLA
	TAY
	PLA
	TAX
    DEX
    JMP cu1
+   CMP #$0D        	; Check for RETURN
    BEQ +
    CMP #$8D        	; Check for SHIFT-RETURN
    BNE ++
+   LDX CURRLINE
    LDY #0
	STY $C7
    INX
    CPX WBOTTOM     	; Beyond the text window's bottom?
    BEQ +
    JMP cu1         	; No, move cursor
+   DEX
	TXA
	PHA
	TYA
	PHA
    JSR $DA89   	; Yes, scroll
	PLA
	TAY
	PLA
	TAX
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
    LDA ($EA),Y
    DEY
    STA ($EA),Y
    INY
    CPY #39
    BNE -
    LDA #$20        	; Space
    STA (CURRLINEPTR),Y
    LDA $053B
    STA ($EA),Y
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
    JSR $D8AA       	; Update pointers
    LDA #$20        	; Space
    STA (CURRLINEPTR),Y
    LDA $053B
    STA ($EA),Y
+   RTS
++  CMP #$82			; Check for FLASH ON
	BNE++
	LDA	FLASHSTATUS
	BEQ	+				; if disabled do nothing
	RTS
+	LDA #$80
	STA $53C
	RTS
++	CMP #$84			; Check for FLASH OFF
	BNE +
	LDA #$00
	STA $53C
	RTS
+   CMP #$94        	; Check for INSERT
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
    LDA ($EA),Y
    INY
    STA ($EA),Y
    DEY
    CPY CURRCOLUMN
    BNE -
    LDA #$20
    STA (CURRLINEPTR),Y
    LDA $053B
    STA ($EA),Y
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
    STA $0547
    RTS
+   CMP #$09        	; Check enable CBM+SHIFT
    BNE +           	; No, next
    LDA #$00
    STA $0547
    RTS
+   CMP #$0E        	; Check switch to Uppercase/Lowercase
    BNE +
    LDA $FF13
    ORA #$04
    BNE ++
+   CMP #$8E        	; Check switch to Uppercase/Graphics
    BNE +
    LDA $FF13
    AND #%11111011
++  STA $FF13
    RTS
+	CMP #$07			; Check for BELL
	BNE +
	LDA #BEEPTIME
	STA BEEPTIMER
	LDA $FF11
	ORA #%00110100
	STA $FF11
	RTS	
+   JMP $DCE6       	; Check colors (Kernal)

;///////////////////////////////////////////////////////////////////////////////////
; Print beep
;///////////////////////////////////////////////////////////////////////////////////
Beep
	LDA BEEPTIMER
	BNE +
	LDA #BEEPTIME
	STA BEEPTIMER	; Reset beep timer
+	LDA	#$01		; if TEMPCNT1 is odd jump to DoBeep2
	AND	TEMPCNT1
	BNE	+
	LDA	#$1A		; Freq channel 1 = ~470 Hz
	BNE	++
+	LDA	#$45		; Freq channel 1 = ~590 Hz
++	STA	$FF0E
	LDA	$FF12
	AND	#$FC
	ORA	#$03
	STA	$FF12
	LDA	#%00010010	; Volume = 2 (Low), enable channels 1 & 2 in tone mode
	ORA $FF11
	STA	$FF11

	INC	TEMPCNT1
	RTS	;JMP	EndBeep

NoChar
	LDA	#%00010000	; Volume = 0
	STA	$FF11
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Insert a string into the print buffer
;///////////////////////////////////////////////////////////////////////////////////

AddText
	STX	AddTLoop + 1
	STY	AddTLoop + 2

	LDX	#$00
AddTLoop
	LDA	PrBuffer, X
	BNE	+
	RTS					; Exit if character is 0
+
;        SEI
	JSR	AddToPrBuffer	; Insert character into the print buffer
;        CLI
	INX
	JMP	AddTLoop		; loop

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

;///////////////////////////////////////////////////////////////////////////////////
; Insert the character in .A, to the print buffer
;///////////////////////////////////////////////////////////////////////////////////

AddToPrBuffer
	PHA
	LDY	PRINDEXIN	; Loads .Y with PRINDEXIN
-	LDA	PRBUFFERCNT	; If PRBUFFERCNT=255 (buffer full) waits until a space is open
	EOR	#$FF
	BEQ	-
	;SEI				;Disable IRQs
	PLA
	STA	PrBuffer, Y
	INC	PRINDEXIN	; Increment PRINDEXIN
	INC	PRBUFFERCNT	; and PRBUFFERCNT
	;CLI				; Enable IRQs
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Wait until the print buffer is empty
;///////////////////////////////////////////////////////////////////////////////////

WaitPrint
	LDA	PRBUFFERCNT	; Wait for PRBUFFERCNT == 0
	BNE	WaitPrint
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Return to BASIC
;///////////////////////////////////////////////////////////////////////////////////

ExitPrg
	SEI				; Disable IRQ
	JSR b3cancel2	; Cancel split screen
	JSR _txtmode	; Switch to text mode
	LDA #24
	STA $07E5
	LDA	#$0F		; Volume = 15 (max), disable channels 1 & 2
	STA	$FF11
;	LDA	#32			; Imprime un espacio
;	JSR	CHROUT
	LDA	#<ExitMsg	; Show exit message
	LDY	#>ExitMsg
	JSR	STROUT		;PrintTxt
	LDA	#%00000010	; no parity, no echo, no tx irq (rts=1), no rx irq, rx disabled (dtr=1)
	STA	ACIACOMMAND
	LDA	#$0E		; Restore default IRQ vector
	STA	$0314
	LDA	#$CE
	STA	$0315
	LDA	#161		; Set the raster line number where interrupt should occur
	STA	$FF0B
	LDA	#%10100010	; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A
	LDA	#0			; Flush keyboard buffer
	STA	$EF
	CLI				; Enable IRQ
	JMP $867E		; Returns to BASIC

;///////////////////////////////////////////////////////////////////////////////////
; Interrupt routine
;///////////////////////////////////////////////////////////////////////////////////

Irq
	PHA			; store register A in stack
	TXA
	PHA			; store register X in stack
	TYA
	PHA			; store register Y in stack


ChkTimer1
	LDA	TIMER1		; Decrement TIMER1 if !=0
	BEQ	ChkTimer2
	DEC	TIMER1
ChkTimer2
	LDA	CRSRTIMER	; Decrement CRSRTIMER if != 0
	BEQ	+
	DEC	CRSRTIMER
+

ChkBuffer
	LDA	PRBUFFERCNT
	BNE	ReadBytes

;///////////////////////////////////////////////////////////////////////////////////
; Generate the screen printing beep

DoBeep
	LDA	BEEPTIMER	; Decrement BEEPTIMER if != 0
	BEQ	ReadBytes
	DEC	BEEPTIMER
	BNE	ReadBytes

NoBeep
	LDA	#%00010000	; Volume = 8 (max), enable channels 1 & 2 in tone mode and restart the oscilators
	STA	$FF11
EndBeep

;///////////////////////////////////////////////////////////////////////////////////
; Read up to 3 bytes from the RS232 port

ReadBytes
	LDA	#%10100000	; Disable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A

	LDA FLAGS1
	AND #%00010000	; Is screen split enabled?
	BEQ+
	LDA SPLITRST
	STA $FF0B
	LDA #<IrqB3
	STA $0314
	LDA #>IrqB3
	STA $0315
	LDA VMODE
	ORA $FF07
	STA $FF07
	LDA #$18		; Attributes at $1800
	STA $FF14
	LDA $FF12
	AND #$FB		; Bitmap in RAM
	STA $FF12		; Bitmap at $2000
	LDA UPBGC
	STA $FF15		; Upper background color
	LDA #$3B		; Bitmap mode
	BNE ++

+	LDA	#$10			; Enable TED screen
	ORA	$FF06
++	STA	$FF06

	LDX	#$03			; Read up to 3 bytes from RS232
ReadBLoop
	LDA	PRBUFFERCNT		; if PRBUFFERCNT=255 (buffer full) continue to ReadBEnd
	EOR	#$FF
	BEQ	ReadBEnd

	LDA #%00001111		; Z = 0 if there's no parameters left to receive
	BIT CMDFlags		; Check command flags
	BVC +				; If we're not waiting for parameters, receive character
	BEQ ReadBEnd		; If we were waiting for parameters but all were received, do not receive more characters
						; until the command is completed
+	JSR	ReadByte		; Receive a character from RS232
	BCC	++	;ReadBEnd		; If none is received, continue on Speak
	LDA	RXBYTE			; Get the received character
	JSR	AddToPrBuffer	; Insert it into the print buffer

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
	BEQ ReadBEnd		; Continue on Speak

.cmdchk					; Check the received command and set CMDFlags accordingly
	LDA RXBYTE			; Get the received character
	BPL +				; Invalid command (bit 7 unset)
	CMP #MAXCMD+1
    BCC ++				; Is it less than or equal to the highest implemented command ($B6)?
+	LDA #$8F			; Invalid command, replace with unimplemented command ($8F)
++	AND #%01111111		; -128
	TAY
	LDA CmdParTable,Y	; Parameter count
	AND #%00001111		; Clear unwanted bits
	ORA #%01000000		; Enable parameter wait
	STA CMDFlags		; Set CMDFlags
	BNE -				; Continue the loop


; 	JSR	ReadByte	; Lee un byte desde el RS232
; 	BCC	ReadBEnd	; Si no se recibio nada, sigue en ReadBEnd
; 	LDA	RXBYTE		; Lee el byte recibido
; 	JSR	AddToPrBuffer	; Agrega el byte al buffer de impresion
; ;	JMP	ReadBNext	; sigue en ReadBNext
; ReadBNext
; 	DEX			; Decrementa X
; 	BNE	ReadBLoop	; Si no llego a 0, vuelve a ReadBLoop
ReadBEnd

ChkKey
	JSR	GETIN		; Read from the keyboard buffer
	BEQ	ExitIrq		; No key? Continue to ExitIrq
	STA	TXBYTE		; Copy the character into TXBYTE
	CMP #$03		;JSR	STOP		; Check the STOP key
	BNE	+
	LDA	#%00000001	; If STOP is pressed, set the flago to return to BASIC
	ORA	FLAGS1
	STA	FLAGS1
	LDA	#0			; Reset the STREY flag
	STA	$91
	JMP	ExitIrq		; and continue to ExitIrq
+	;LDA	TXBYTE		; Check for SHIFT + RETURN
	CMP	#$8D
	BNE	+
	LDA	#$0A		; convert it to LF
	BNE ++
+	CMP #$A7		; CBM+M
	BNE +
	LDA #$FF
	EOR SNDSTATUS
	STA SNDSTATUS
	JMP ExitIrq
+	CMP #$88		; F7?
	BNE +
	LDX $0543		; Shift keys flag
	CPX #$02		; C= pressed?
	BNE ++
	;Test terminal not in command mode
	BIT CMDFlags
	BVS ExitIrq
	LDA #>SETUP			; Modify Stack
	PHA
	LDA #<SETUP
	PHA
	LDA #$00			
	PHA
	RTI
+	CMP #$3C		; <?
	BNE	++
	LDX	$0543		; Shift keys flag
	CPX	#$02		; C= pressed?
	BNE	++
	;Toggle FLASHON support
	LDA #$FF
	EOR FLASHSTATUS
	STA FLASHSTATUS
	LDA #$00
	STA $53C		; FLASHOFF
	JMP ExitIrq


++	BIT CMDFlags
	BVS ExitIrq
	STA	ACIADATA	; Coloca la tecla leida en el registro de transmision, para ser enviada en la proxima interrupcion

ExitIrq
	;INC $FF19
	LDA	#%10000010	; Limpia todas las banderas de interrupcion, incluyendo la de interrupcion de barrido
	STA	$FF09
	LDA	#%10100010	; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A

; 	LDA FLAGS1
; 	AND #%00010000	; Is screen split enabled?
; 	BEQ+
; 	LDA SPLITRST
; 	STA $FF0B
; 	LDA #<IrqB3
; 	STA $0314
; 	LDA #>IrqB3
; 	STA $0315
; 	LDA VMODE
; 	ORA $FF07
; 	STA $FF07
; 	LDA $FF12
; 	AND #$FB		; Bitmap in RAM
; 	STA $FF12			; Bitmap at $2000
; 	LDA #$3B		; Bitmap mode
; 	BNE ++

; +	LDA	#$10		; Habilita el TED (activa la pantalla)
; 	ORA	$FF06
; ++	STA	$FF06

	LDA $FB
	PHA
	LDA #$00
	STA $FB
	PHP
	CLI
	JSR $DB11		; Scan keyboard
	PLP
	PLA
	STA $FB


	PLA
	TAY			; restore register Y from stack
	PLA
	TAX			; restore register X from stack
	PLA			; restore register A from stack
	;JMP	$CE0E		; Jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc.
	JMP $FCBE

;///////////////////////////////
; Init color codes palette
;///////////////////////////////

InitPalette:
	LDX #$0F
-	LDA Palette,X
	STA $0113,X
	DEX
	BPL -
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; SETUP SCREEN
;///////////////////////////////////////////////////////////////////////////////////

SETUP:
	+DisROMs
	JSR _SETUP
	+EnROMs
	JMP ExitIrq

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
	LDY #$00
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	;EOR #$00
	BEQ	Addr00			; If $00 go to Addr00
	CMP	#$10
	BEQ	Addr10			; If $10 go to Addr10
	CMP	#$20
	BEQ	Addr20			; If $20 go to Addr20
	INY					; Otherwise set BLOCKPTR to $1001 (BASIC text area)
	LDA	#$10
-	STY	BLOCKPTR
	STA	BLOCKPTR + 1
	RTS 
Addr00
	; Point BLOCKPTR to $0C00 (3072)
	LDA	#$0C
	BNE -
Addr10
	; Point BLOCKPTR to $2000 (8192)
	LDA	#$20
	BNE -
Addr20
	; Point BLOCKPTR to $0800 (2048)
	LDA	#$08
	BNE -

;///////////////////////////////////////////////////////////////////////////////////
; 130: Transfers a byte block to memory, requires 2 parameter bytes
;      Byte count (low, high)

Cmd82
	JSR NoChar			; Mute beep
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	TAY
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	TAX

; Check if transferring to the BASIC area
	LDA BLOCKPTR
	CMP #$01
	BNE +
	LDA BLOCKPTR + 1
	CMP #$10
	BNE +
	CLC
	TYA
	ADC BLOCKPTR
	STA $2D
	TXA
	ADC BLOCKPTR+1
	STA $2E				; Update Start of BASIC variables pointer

_Cmd82	;Alternative entry point
+	DEY
	CPY #$FF
	BNE +
	DEX
+	STY BYTECNT
	STX BYTECNT + 1
	LDA	$FF19
	STA	BORDERCLR
	LDA	BLOCKPTR		; Copy BLOCKPTR pointer to C82Loop
	STA	C82Addr + 1
	LDA	BLOCKPTR + 1
	STA	C82Addr + 2
	LDA	#%10100000		; Disable raster interrupt signals from TED
	STA	$FF0A
	SEI					; Disable IRQs

-	LDA $FF1D
	CMP #204
	BNE -				; Waits for raster line 204

	LDA	$FF06			; Disables TED screen
C82enable
	AND	#%01101111
	STA	$FF06

C82Loop
	; Timeout counter
	LDA	#$0A			; Count ~5.66 seconds
	STA TEMPCNT2		; H
	LDA #$00				; 0
	STA TEMPCNT1		; L

-	JSR	ReadByte		; Receive a character from RS232
	BCS ++				; Byte received -> +
	LDA TEMPCNT1		; Decrement counter
	BNE +
	LDA TEMPCNT2
	BEQ C82End			; Zero, exit
	DEC TEMPCNT2
+	DEC TEMPCNT1
	BCC -				; Keep receiving

;	BCC	-				; Nothing received, retry
++	LDA	RXBYTE			; Store it in RAM
C82Addr
	STA	C82Addr			; (self-modifying code) (WAS BUFFER)
C82FX
	INC	$FF19			;<<<<
	INC	C82Addr + 1		; 6 Increment the memory pointer
	BNE	C82Next			; 2 
	INC	C82Addr + 2		; 6 
C82Next					; 1 if coming from the BNE
	LDA BYTECNT			; 4
	BNE +				; 2+1
	LDA BYTECNT+1		; 4
	BEQ C82End			; 2+1
	DEC BYTECNT+1		; 6
+	DEC BYTECNT			; 6
	; LDY BYTECNT
	; DEY
	; CPY #$FF
	; BNE C82Cont
	; LDX BYTECNT+1
	; DEX
	; CPX #$FF
	; BEQ	C82End			; 2 
	; STX BYTECNT+1 
C82Cont					; 1 if coming from the BNE
;	STY BYTECNT
	LDA C82FX
	EOR #$20			; Toggle INC <-> DEC
	STA C82FX
	JMP	C82Loop
C82End
	LDA	BORDERCLR
	STA	$FF19
	;LDA	$DC0D			; Clear the interrupt flags ***

	LDA	$FF06			; Enable screen
	ORA	#%00010000
	STA	$FF06
	LDA #$02
	STA $FF0A			; Enable raster interrupts
	CLI					; Enable IRQs
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 131: PCM audio streaming until receiving a NUL ($00) character

Cmd83
	SEI

	LDA	$FF19
	STA	BORDERCLR

	LDX	#$88

-	LDA $FF1D
	CMP #251
	BNE -			; Wait for raster line 251

	LDA	$FF06		; Disable TED screen
	AND	#%01101111
	STA	$FF06

	JSR	TurboRX		; Do the thing
	CLI


	LDA	$FF06		; Enable TED screen
	ORA	#%00010000
	STA	$FF06
	LDA #$82
	STA $FF09		; Ack raster interrupts


	LDA	BORDERCLR
	STA	$FF19
	RTS

;////////////////////////////////////////////////////////////////////////////////////
; 134: Start a file transfer
Cmd86
	SEI
	+DisROMs
	JSR _Cmd86
	+EnROMs
	CLI
	JMP CmdFE			; Exit command mode


;///////////////////////////////////////////////////////////////////////////////////
; 144: Returns to the default text mode, requires 3 parameter bytes
;      Page (not used), border color, background color

Cmd90
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	;Página - Descartado
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$FF19
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$FF15
_txtmode
	LDA	#%00011011		; Switch to text mode
	STA	$FF06
	LDA #$00
	STA $83
	LDA	$FF07			; Disable multicolor mode
	AND #$48
	STA $FF07
	LDA #$08			; Attrs at $0800 Screen at $0C00
	STA $FF14
	LDA $FF13			; Charset at $D400
	AND #$03
	ORA #$D4
	STA $FF13
	LDA $FF12			; Read Charset from ROM
	ORA #$04
	STA $FF12

	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 145: Switch to bitmap hires mode, requires 2 parameter bytes
;      Page (not used), border color

Cmd91
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$FF19
	JSR _luchcopy
	; LDA #$08			; Attributes at $0800
	LDA #$18			; Attributes at $1800
	STA $FF14
	LDA $FF12
	AND #$03			;$07
	ORA #$08
	STA $FF12			; Bitmap at $2000
	LDA	$FF07			; Disable multicolor mode
	AND #$48
	STA $FF07
	; Switch to bitmap mode
	LDA #%00111011
	STA	$FF06
	STA $83
	RTS

; Copy color and luma tables from $800 to $1800 before switching to graphic modes
_luchcopy
	LDX #$00
-	LDA $0800,X	; Lumas
	STA $1800,X
	LDA $0900,X
	STA $1900,X
	LDA $0A00,X
	STA $1A00,X
	LDA $0B00,X
	STA $1B00,X
	LDA $0C00,X	; colors
	STA $1C00,X
	LDA $0D00,X
	STA $1D00,X
	LDA $0E00,X
	STA $1E00,X
	LDA $0F00,X
	STA $1F00,X
	INX
	BNE -
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 146: Switch to bitmap multicolor mode, requires 4 parameter bytes
;      Page (not used), border color, background color, multicolor 3 color

Cmd92
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$FF19
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$FF15
	JSR	GetFromPrBuffer	; Reads a byte from the print buffer
	STA	$FF16
	JSR _luchcopy
	; LDA #$08			; Attributes at $0800
	LDA #$18			; Attributes at $1800
	STA $FF14
	LDA $FF12
	AND #$03
	ORA #$08
	STA $FF12			; Bitmap at $2000
	LDA	#$10			; Set multicolor multicolor mode
	ORA	$FF07
	STA $FF07
	; Switch to bitmap mode
	LDA #%00111011
	STA	$FF06
	STA $83
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 152: Clears the graphic screen
Cmd98
	LDA FLAGS1
	AND #%00010000	; Is screen split enabled?
	BEQ +
--	LDA $83			; Current screen mode
	BEQ --			; Wait until graphic mode is enabled
+	JMP $C567	;SCNCLR
	; RTS

;///////////////////////////////////////////////////////////////////////////////////
; 153: Set Pen color
; Parameters: Pen, Color
Cmd99:
	JSR GetFromPrBuffer	; Pen
	TAY	; Save it
	JSR GetFromPrBuffer	; Color
	TAX
	TYA
	BNE +
	STX $FF15			; Pen 0
	STX UPBGC
	RTS
+	CMP #$01
	BNE +
	STX $86				; Pen 1
	RTS
+	CMP #$02
	BNE +
	STX $85				; Pen 2
	RTS
+	CMP #$03
	BNE +
	STX $FF16			; Pen 3
+	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 154: Plot point
; Parameters: Pen, X(16bit), Y(16bit)
Cmd9A:
	JSR GetFromPrBuffer ; Pen
	STA $84
	LDY #$00
-	JSR GetFromPrBuffer ; Get coordinates
	STA $02AD,Y
	INY
	CPY #$04
	BNE -
	LDA $FF06
	AND #$20
	BEQ +				; Bitmap mode?
	SEI
	JSR	$C1A5			; Plot point
	JSR	_bmack
	CLI
+	RTS

_bmack:					; Ack any pending IRQ during drawing routines
	LDA #$82
	STA $FF09			; Ack raster interrupts
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 155: Line
; Parameters: Pen, X1(16bit), Y1(16bit), X2(16bit), Y2(16bit)
Cmd9B:
	JSR GetFromPrBuffer ; Pen
	STA $84
	LDY #$00
-	JSR GetFromPrBuffer ; Get coordinates
	STA $02AD,Y
	INY
	CPY #$08
	BNE -
	LDA $FF06
	AND #$20
	BEQ +				; Bitmap mode?
	; SEI
	JSR	$C0DA			; Draw line
	; CLI
+	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 156: Box
; Parameters: Pen, X1(16bit), Y1(16bit), X2(16bit), Y2(16bit), Fill
Cmd9C:
	JSR GetFromPrBuffer	; Pen
	STA $84
	LDY #$00
	STY $02D0			; Rotation lo
	STY $02D1			; Rotation hi
-	JSR	GetFromPrBuffer
	STA	$02CC,Y			; X1, Y1
	INY
	CPY #$04
	BNE	-
	; LDY #$00
-	JSR	GetFromPrBuffer
	STA	$02D4,Y			; X2, Y2
	INY
	CPY #$08
	BNE	-
	JSR	GetFromPrBuffer	; Fill flag
	BEQ +
	LDA #$01
+	TAX
	JMP $BB02
;	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 157: Circle
; Parameters: Pen, center X(16bit), center Y(16bit), radius X(16bit), radius Y(16bit)
Cmd9D:
	JSR GetFromPrBuffer	; Pen
	STA $84
	LDY #$00
	STY $02D8			; Start angle lo
	STY $02D9			; Start angle hi
-	JSR	GetFromPrBuffer
	STA	$02CC,Y			; center X, Y. radius X, Y
	INY
	CPY #$08
	BNE	-
	LDA #$68
	STA $02DA			; End angle lo
	LDX #$01
	STX $02DB			; End angle hi
	INX
	STX	$E9				; Segment angle

	; This section adapted from Plus4 ROM
        LDY   #$00	; temp
		TYA
        JSR   $BC59	; Function evaluating with interpolation for gfx.
		LDX   #$2D
        LDY   #$2B
        JSR   $C305	; Subtract to coordinates
        BCC   +
+	    LDX   #$03
-	    LDA   $02D0,x
        STA   $02D4,x
        DEX    
        BPL   -
        LDA   #$90
        JSR   $BCD5	; ???
        LDX   #$07
-	    LDA   $02D0,x
        STA   $02DC,x
        DEX    
        BPL   -
        JSR   $BCEE	; ???
        JSR   $C37B	; Set graphic cursor
	CLC
	JMP	$C0B2	; Circle
;	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 158: Fill
; Parameters: Pen, X(16bit), Y(16bit)
Cmd9E:
	JSR GetFromPrBuffer	; Pen
	STA $84
	LDY #$00
-	JSR GetFromPrBuffer ; Get coordinates
	STA $02AD,Y
	STA $02B1,Y
	INY
	CPY #$04
	BNE -
	LDA #$00			; fill area enclosed by the pen number
	JMP $B8E7			; PAINT
;	RTS

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
	LDY #00
-	LDA IDString,Y
	JSR SendID
	INY
	CPY #24
	BNE -
	CLI
	RTS             ;JMP	ExitIrq

;///////////////////////////////////////////////////////////////////////////////////
; 163: Queries terminal if a command is implemented, requires 1 parameter: Command #
;	   Returns number of parameters if command exist, or bit-7 = 1 if not.

CmdA3
	JSR GetFromPrBuffer ; Reads a byte from the print buffer / Command #
	SEI
	BPL +				; If it's not a command, replace with unimplemented command ($8F)
	CMP #MAXCMD+1
	BCC ++				; Is it less than or equal to the highest implemented command?
+	LDA #$8F			; Invalid command, replace with unimplemented command ($8F)
++	AND #%01111111		; -128
	TAY
	LDA CmdParTable,Y	; Get parameter count/Command implemented
	JSR SendID
	CLI
	RTS

;////////////////////////////////////////////////////////////////////////////////////
; 164: Query client's setup, requires one parameter: subsystem

CmdA4
	JSR GetFromPrBuffer	; Reads byte from the print buffer / Subsystem
	SEI
	+DisROMs
	CMP #$06			; Check if valid subsystem
	BCC	.a4_0			; less than...
	BEQ .a4_0			; equal...
	LDA #$07
.a4_0
	TAY
	LDA A4offsets,Y		; A: A4array offset
	TAY
	LDA A4array,Y		; A: responte length
	TAX
	INX

.a4_1
	JSR SendID
	INY
	LDA A4array,Y
	DEX
	BNE .a4_1
	+EnROMs
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
	CLC
	ADC WTOP
	TAX
	CPX WBOTTOM			; Greater than WBOTTOM?
	BCC +				; No, continue
	LDX WBOTTOM			; Yes, force WBOTTOM
+	PLA
	TAY					;LDY TEMP1
	CLC
	JSR $D839			; Set cursor position
	JSR StartCRSR
	LDA WTOP			; Restore OS text window limits
	STA $07E6
	LDX WBOTTOM
	DEX
	STX $07E5
	JSR $DE80			; Rebuild link table
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
	LDA $D802,X			; Get the row address low byte from the ROM table
	STA .cb11+1			; Screen
	STA .cb12+1			; Attributes
	LDA $D81B,X			; Row address high byte
	STA .cb11+2			; Screen
	EOR #$04
	STA .cb12+2			; Attributes
	
	JSR GetFromPrBuffer ; Reads a byte from the print buffer / screen_code

	TAY					; Save screen_code

	; Fill row
	LDX #39
.cb11
	STA $0C00,X
	LDA $053B			; Current Color
	;ORA $053C			; Flash
.cb12
	STA $0800,X	
	TYA
	DEX
	BPL .cb11

	JMP StartCRSR
	
;	RTS

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

;---------------------------------
; Split screen interrupt routine

IrqB3:
	LDY BTBGC
	LDA #$08			; Attributes at $0800
	STA $FF14
	LDA $FF12			; Charset from ROM
	ORA #$04

	; Watch for page boundaries!
 	LDX #$0D			;2
-	DEX					;2
 	BNE -				;3
	TAX

	; NOP
	; NOP
	; NOP
	STY $FF15			;4 Bottom section background color
	LDA #%00011011		;2 Text mode
	STA $FF06			;4
	LDA $FF07			;4
	AND #%11101111		;2
	STA $FF07			;4 Disable multicolor
	STX $FF12

	LDA #204			;2
	STA $FF0B			;4

	LDA	#%10000010		; Limpia todas las banderas de interrupcion, incluyendo la de interrupcion de barrido
	STA	$FF09
	LDA	#%10100010		; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A

	LDA #<Irq
	STA $0314
	LDA #>Irq
	STA $0315

	JMP $FCBE

CmdB3
	JSR GetFromPrBuffer
	BEQ b3cancel
	TAY
 	BMI +
 	LDX #$00
 	BEQ ++
+	LDX #$10
++	STX VMODE
 	AND #$1F			; Remove mode bit
 	STA WTOP			; Set text window
	STA $88				; Limit for drawing routines
 	ASL
 	ASL
 	ASL					; .A*8
 	CLC
 	ADC #$01			; +01 = Scanline-3
 	STA SPLITRST
	JSR GetFromPrBuffer
	STA UPBGC			; This is wrong, we need 1 byte per bg color
	TAX
	TYA
	AND #$20			; Check if there's additional parameters
	BEQ +
	LDA #$42			; Get 2 more parameters
	STA CMDFlags
	JSR GetFromPrBuffer
	STA BTBGC
	JSR GetFromPrBuffer
	AND #$7F
	STA $FF16
	BPL ++
+	TXA
	LSR
	LSR
	LSR
	LSR
	STA BTBGC
++	LDA #%00010000
	ORA FLAGS1
	STA FLAGS1
	LDY #$00
	LDX WTOP
	CLC
	JSR $D839			; Set cursor position
	LDA WTOP
	STA $07E6
	LDA $FF12
	AND #$03
	ORA #$08
	STA $FF12			; Bitmap at $2000
	JSR _luchcopy		; Copy luma/colors to $1800
	; LDA #$03
	; STA ReadBLoop-1		; Limit reception to 3 characters per frame
	LDA #%01100000		; Split screen hires
	LDX VMODE
	BEQ +
	ORA #%10000000		; Split screen multi	
+	STA $83				; Graphic mode
	RTS
b3cancel				; Cancel split screen
	JSR GetFromPrBuffer
b3cancel2
	LDA #%11101111
	AND FLAGS1
	STA FLAGS1
	LDA #$00
	STA WTOP			; Set text window
	STA $07E6
	LDA #$19
	STA $88				; Max lines for drawing routines
	; LDA #$03
	; STA ReadBLoop-1
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 180: Get cursor position, returns 2 bytes, column and row. Exit CMD mode

CmdB4
	SEI
	SEC
	JSR $D839			; Get cursor position
	TXA
	SEC
	SBC WTOP
	PHA
	TYA					; Column
	SEC
	SBC WTOP
	JSR SendID
	PLA					; Row
	JSR SendID
	CLI
	LDA WTOP			; Restore text window limits
	STA $07E6
	LDX WBOTTOM
	DEX
	STX $07E5
	JMP CmdFE

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
	JSR $D839		; Set cursor position
	LDA WTOP		; Set OS text window limits
	STA $07E6
	LDX WBOTTOM
	DEX
	STX $07E5
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 182: Scroll text window, requires 1 parameter: rows to scroll, signed byte
CmdB6
	JSR GetFromPrBuffer
	BEQ ++
	BPL +
	;Scroll up
	EOR #$FF
	STA $D8
	INC $D8
-	JSR $DF04
	DEC $D8
	BNE -
	BEQ ++
+	STA $D8
-	JSR $DEF6
	DEC $D8
	BNE -

++	RTS

;///////////////////////////////////////////////////////////////////////////////////
; 183: Set Ink color, requires 1 parameter: Color index
CmdB7
	JSR GetFromPrBuffer
	AND #$7F	; Remove Flash bit
	STA $053B
	JMP CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 254: Exit command mode, setting FLAGS1 bit 7 to 0
CmdFE
	LDA	FLAGS1		
	AND	#%01111111
	STA	FLAGS1
    RTS

;///////////////////////////////////////////////////////////////////////////////////

SendID
	STA	ACIADATA	; Store the character in the transmit register
	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, receive enable), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
-	LDA	ACIASTATUS	; Wait for the character to be transmitted
	AND	#%00010000
	BEQ	-
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, receive disable), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
	RTS

; Commands routine pointer table, unimplemented commands point to CMDOFF
CmdTable:
    !word Cmd80,Cmd81,Cmd82,Cmd83,CmdFE,CmdFE,Cmd86,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word Cmd90,Cmd91,Cmd92,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,Cmd98,Cmd99,Cmd9A,Cmd9B,Cmd9C,Cmd9D,Cmd9E,CmdFE
    !word CmdA0,CmdA1,CmdA2,CmdA3,CmdA4,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word CmdB0,CmdB1,CmdB2,CmdB3,CmdB4,CmdB5,CmdB6,CmdB7

; Command parameter number table.
; bit-7 = 1 : Parameter not implemented
CmdParTable:
	!byte $02  ,$01  ,$02  ,$00  ,$80  ,$80  ,$00  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $03  ,$02  ,$04  ,$80  ,$80  ,$80  ,$80  ,$80  ,$00  ,$02  ,$05  ,$09  ,$0A  ,$09  ,$05  ,$80
	!byte $00  ,$00  ,$00  ,$01  ,$01  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $02  ,$02  ,$01  ,$02  ,$00  ,$02  ,$01  ,$01

InitScr
	!byte $93, $08, $0E, $00
RetroIntro
	!text "   rETROCOMPUTACION PRESENTS"
	!byte 13, 13, 29, 29, 29, 29, 29, 29, 29, 29
	!text "    rETROTERM pLUS/4"
	!byte 19, 0
Version
	!byte $05, $93, $99, $08, $0E
	!text "retroterm plus/4 VER "
	+_Version_
	!fill 34-(*-Version),$20		; Auto space fill
	!text "19200,8n1"
	!byte $05, $0D , $00
Msg06
	!text "(c)2026 RETROCOMPUTACION.COM"
	!byte $0D
	!byte $9A
	!text "turbo56k V0.8"
	!byte $0D, $05, $00	
ExitMsg
	!byte $93, $8E
	!text "EXITING TO BASIC..."
	!byte $0D
	!text "SYS28672 TO RESTART RETROTERM"
	!byte $0D, $0D, $00

IDString: ; ID String 22 characters long
	!text "RTRETROTERM-P4 "
	+_Version_
	!fill 22-(*-IDString),$20		; Auto space fill
	!byte $00,$08	;Turbo56K version, subversion

Palette
	!byte $00,$71,$32,$63,$34,$45,$26,$67
	!byte $48,$29,$52,$31,$41,$65,$46,$51
;$FF15 background, $FF19 border

FTable
	!byte $85,$89,$86,$8A,$87,$8B,$88,$8C

;///////////////////////////////////////////////////////////////////////////////////
; Buffer de impresion
;///////////////////////////////////////////////////////////////////////////////////

ENDOFCODE
!if ENDOFCODE > $7EFF {
	!error "ERROR - Part 1 data beyond $7EFF"
}
}
_ENDOFCODE_

;///////////////////////////////////////////////////////////////////////////////////
; Mobile code section $8000->
;///////////////////////////////////////////////////////////////////////////////////
_SHADOWCODE_
!pseudopc $8000{
SHADOWCODE:

; ------------------------------------------
; Save to disk routines
	CRC = 	$0F40	;$FD		; CRC result $FD/$FE
	CRCHI = $1900	; CRC tables
	CRCLO = $1A00
	FNAME = $0332	; Null terminated file name
	FNL = 	$03F2	; Filename length

; Copy Memory
; Source at SOURCEPTR($D8/$D9)
; Destination at DESTPTR($DA/$DB)
; Size at CSIZE($58/$59)
CSIZE		= $E8
SOURCEPTR 	= $D8
DESTPTR		= $DA

; List of detected drives. Filled at first startup
DRIVES:
!fill $08,$FF

_MemCpy
--	LDY #$00
-	LDA (SOURCEPTR),Y
	STA (DESTPTR),Y
	LDA CSIZE
	BNE +
	LDA CSIZE+1
	BEQ ++
	DEC CSIZE+1
+	DEC CSIZE	
    INY
    BNE -

    INC	SOURCEPTR+1
    INC DESTPTR+1
    BNE --
++	RTS

_Cmd86
	JSR MAKECRCTABLE

DESTADDR	= $1B00
BBUF		= $0E00

	; Copy routine to lower RAM ($1B00)
	LDX #$01
-	LDA _86data,X
	STA SOURCEPTR,X
	LDA _86data+2,X
	STA DESTPTR,X
	LDA _86data+4,X
	STA CSIZE,X
	DEX
	BPL -

	JSR _MemCpy		; Do mem copy
	LDX #$08
-	LDA DRIVES,X	; Copy list of detected drives to low RAM
	STA _drives,X
	DEX
	BPL -
	JMP bsave
_86data
	!word _bsstart,DESTADDR,_bsend-_bsstart

_bsstart:
!pseudopc $1B00{
bsave:
	+EnROMs
	LDA #$00
	STA $FF19
	STA $FF15
	JSR b3cancel2	; Cancel split screen

	CLI
	; Print message
	+StringOut bst1
; 	SEI
; -	LDA $FF1D
; 	CMP #251
; 	BNE -				; Waits for raster line 251
; 	JSR	ReadByte		; Get file type
; 	BCC-
; 	CLI
; 	LDA RXBYTE
	LDA #$46
	STA CMDFlags		; Enable reception
	JSR GetFromPrBuffer	; Get file type

	STA $02
	BMI +				; Bit 7 = 1 : PRG, else SEQ
	LDA #<bst5
	STA .ft+1			; Set file type OPEN string addr
	LDA #>bst5
	STA .ft+2
	LDY #>bst3
	LDA #<bst3
	BNE ++
+	LDA #<bst4
	STA .ft+1			; Set file type OPEN string addr
	LDA #>bst4
	STA .ft+2
	LDA #<bst2
	LDY #>bst2
++	JSR STROUT
	+StringOut bst1a
	JSR ReadString	; Get filename
	+SetCursor $0A,$01
	; Print filename
	+StringOut FNAME
	+SetCursor $20,$0B
	; Print buffer frame
	+StringOut bst6

	JSR UBlock		; Print counters
	JSR URetry

	LDX #$00
	STX _curdrv		; Find first available drive #
-	CPX #$08
	BEQ ++			; No available drive found > CANCEL
	LDA _drives,X
	BPL +			; Available drive
	INX
	BNE -

+	STX _curdrv		; Store as current drive
	JSR ddrive

-	LDA $C6			; Matrix value for last keypress
	CMP #$19		; Wait for 'Y'
	BEQ .y1
	CMP #$27		; Or 'N'
	BNE +
++	LDY #$42		; CANCEL
	BNE .bb
+	CMP #$28		; '+'
	BNE +
	JSR ndrive		; Get next drive
	JMP -
+	CMP #$2B		; '-'
	BNE -
	JSR pdrive		; Get previous drive
	JMP -

.y1	;Continue
	+SetCursor $0B,$08
	+StringOut bst9	; Print abort message

	LDY FNL			; add write string to filename
	LDX #$00
.ft	LDA bst4,X
	STA FNAME,Y
	INY
	INX
	CPX #$04
	BNE .ft
	TXA
	CLC
	ADC FNL
	;STA FNL
	JSR .bo				; Open file. Input .A = filename length. Returns flag on .Y
	;LDY	#$80		; OK Flag

; Read block
	LDA	#$00			; Sound off
	STA	$FF11
.bb
	LDA $C6				; Keymatrix
	CMP #$03			; F7?
	BNE +
	LDY #$42			; ABORT!
+	SEI
	TYA
	JSR SendID

	LDY #$03

	LDA CMDFlags
	ORA #$44
	STA CMDFlags	; Get 4 parameter bytes

	; Get 4 bytes: Block size LSB,MSB | CRC16 LSB,MSB

	CLI
-	JSR GetFromPrBuffer

	STA $D8,Y			; $FD/$FE : Size
	DEY
	BPL -

	SEI

	; If block size = 0, exit transfer
	LDY $DA
	STY .c+1		; Set CRC check counter limit
	BNE +
	LDA $DB
	BNE +
	JMP .be			; 0 length -> end transfer
+	LDA #<BBUF
	STA BLOCKPTR
	LDA	#>BBUF
	STA BLOCKPTR+1	; Destination
	LDX $DB
	JSR _Cmd82		; Receive block
	; check CRC
	LDY #$FF
	STY CRC
	STY CRC+1
	INY
-	LDA BBUF,Y
	JSR UPDCRC
	INY
.c	CPY #$00
	BNE -
	LDA $D8
	CMP CRC
	BNE .er			; CRC doesn't match, error
	LDA $D9
	CMP CRC+1
	BNE .er			; CRC doesn't match, error
	; Write to disk
	JSR UBlock
	JSR bwrite		; Write block
	;LDY #$80		; OK
	JMP .bb			; Get next block
.er
	JSR URetry
	LDY #$AA		; ERROR
	JMP .bb			; Retry

	;Open file
.bo
 	;LDA FNL	; Name length
 	LDX #<FNAME
 	LDY #>FNAME
 	JSR SETNAM	  	; call SETNAM

	LDA _curdrv		; current selected drive
	CLC
	ADC #$08
	TAX
	LDA #$02    	; file number 2
	LDY #$02      	; secondary address 2
	JSR SETLFS     	; call SETLFS
 	JSR KOPEN     	; call OPEN
	BCS .error		; if carry set, file couldnt not be opened
	LDX #$02		; filenumber 2
	JSR CHKOUT		; call CHKOUT (file 2 now used as output)
	BEQ +
	BNE .error
+	JSR CLRCHN		; CLRCHN
	LDY #$81		; OK
	RTS

.error	; Handle errors - Cancels transfer
	JSR .bc			; Close file
	LDY #$42		; CANCEL
	RTS

.bc		; Close file
	LDA #$02
	JSR KCLOSE		; CLOSE
	JMP CLRCHN		; CLRCHN
;	RTS

.be		; Transfer complete
	JSR	.bc			; Close file
	SEI
	+DisROMs
	RTS

bwrite 	; Write data
	LDY $DA			; Get counter limit
	STY .b1+1
	LDX #$02		; filenumber 2
	JSR CHKOUT		; call CHKOUT (file 2 now used as output)

	LDY #$00
-	JSR	READST		; call READST
	BNE .error			; handle error
	LDA BBUF,Y
	JSR CHROUT		; call CHROUT (write byte to file)
	INY
.b1	CPY #$00
	BNE -
	JSR CLRCHN		; CLRCHN
	LDY #$81		; OK
	RTS

; Quick CRC computation with lookup tables
UPDCRC:
	EOR CRC+1
	TAX
	LDA CRC
	EOR CRCHI,X
	STA CRC+1
	LDA CRCLO,X
	STA CRC
	RTS

;Update received block count
UBlock:
	+SetCursor $00,$06
	+StringOut bst7
	LDA bcount
	LDX bcount+1
	JSR $A45F		; Print number
	INC bcount+1
	BNE +
	INC bcount
+	RTS

bcount
	!byte	$00,$00

;Update retries count
URetry:
	+SetCursor $00,$07
	+StringOut bst8
	LDA rcount
	LDX rcount+1
	JSR $A45F		; Print number
	INC rcount+1
	BNE +
	INC rcount
+	RTS

rcount
	!byte	$00,$00

;Receive NULL terminated String
ReadString:
	LDA #$46
	STA CMDFlags


	LDY	#$00

--	INC CMDFlags			; +1 Parameter to read
	JSR GetFromPrBuffer	

	STA FNAME,Y		; Store name
	BEQ +			; Stop if $00 received
	INY
	CPY #$11
	BNE --			; Repeat until 16 characters (+ null) are read
+	
	LDA #$00
	STA FNAME,Y		; Make sure the string is NULL terminated
	STY	FNL			; String length
	LDA #$40		; stop reception
	STA CMDFlags
	RTS

; Find next available drive
ndrive:
	LDX _curdrv
-	INX
	CPX #$08
	BEQ +
	LDA _drives,X
	BMI -
	STX _curdrv
+	JMP ddrive
;	RTS

;Find previous available drive
pdrive:
	LDX _curdrv
-	DEX
	BMI +
	LDA _drives,X
	BMI -
	STX _curdrv
+	JMP ddrive
;	RTS

;Print drive number
ddrive:
	+SetCursor 11,2
	+StringOut bst10
	CLC
	LDA _curdrv
	ADC #$08
	TAX
	LDA #$00
	JSR $A45F	; Print number
	; and wait for key release
-	LDA $C6
	CMP #$40
	BNE -
	RTS

; File save text
bst1:
	!byte	$93,$8E,$99,$92	; Clear, upp/gfx, light green, rvsoff
	!text	"HOST REQUESTED TO SAVE ",$00
bst1a:
	!text 	"FILE",$0D
	!text	$05,"FILENAME:",$0D
	!text	$9E,"TO DRIVE ",$12,"+",$92,"    ",$12,"-",$0D
	!text	$11,$81,"CONTINUE? (Y/N)",$05,$00
bst2:	; Program
	!text	"PROGRAM ",$00
bst3:	; Sequential
	!text	"SEQUENTIAL ",$00
bst4:	; Write Program
	!text	",P,W"
bst5:	; Write Sequential
	!text	",S,W"
; Buffer frame
bst6:
	!byte $9E		; Yellow
	!fill $27,$AF	; 39 _
	!byte $BA
	!fill $07,$0D
	!fill $08,$1D	; cursor
	!byte $6F
	!fill $27,$B7	; 39
	!byte $05, $00	; White
; Block count
bst7:
	!text "BLOCKS:      "
	!fill $05,$9D
	!byte $00
; Retry count
bst8:
	!text "RETRIES:      "
	!fill $05,$9D
	!byte $00
; Abort text
bst9:
	!text "HOLD ",$12," F7 ",$92," TO ABORT",$00
; Drive clean spaces
bst10:
	!text "  ",$9D,$9D,$00

; Detected drives copy
_curdrv:
	!byte $00
_drives:
bsend
}
_bsend
; Generate CRC tables
MAKECRCTABLE:
	LDX #0          ; X counts from 0 to 255
BYTELOOP
	LDA #0          ; A contains the low 8 bits of the CRC-16
	STX CRC         ; and CRC contains the high 8 bits
	LDY #8          ; Y counts bits in a byte
BITLOOP  
	ASL
	ROL CRC         ; Shift CRC left
	BCC NOADD       ; Do nothing if no overflow
	EOR #$21        ; else add CRC-16 polynomial $1021
	PHA             ; Save low byte
	LDA CRC         ; Do high byte
	EOR #$10
	STA CRC
	PLA             ; Restore low byte
NOADD
    DEY
	BNE BITLOOP     ; Do next bit
	STA CRCLO,X     ; Save CRC into table, low byte
	LDA CRC         ; then high byte
	STA CRCHI,X
	INX
	BNE BYTELOOP    ; Do next byte
	RTS

;//////////////////////////
; Setup screen
;//////////////////////////
_SETUP
	JSR b3cancel2	;Cancel split screen
	LDA #<suIRQ		;Set minimal IRQ routine
	STA $0314
	LDA #>suIRQ
	STA $0315

	; Copy routine to lower RAM ($1B00)
	LDX #$01
-	LDA _sudata,X
	STA SOURCEPTR,X
	LDA _sudata+2,X
	STA DESTPTR,X
	LDA _sudata+4,X
	STA CSIZE,X
	DEX
	BPL -

	JSR _MemCpy		;Do mem copy
	+DisROMs
	LDA load_drive
	STA _load_drive
	JSR dosetup		;Call setup routine

	LDA #<Irq		;Restore main IRQ routine
	STA $0314
	LDA #>Irq
	STA $0315

	RTS

_sudata
	!word _sustart,$1B00,_suend-_sustart
;----
_sustart:
!pseudopc $1B00{
dosetup:
	+EnROMs
	LDA $FF15		;Save screen colors
	PHA
	LDA $FF19
	PHA
	LDA $053B
	PHA

_dosetup				; alternative entry point
	LDA #$32
	STA $FF15
	STA $FF19
	LDA	#%10100010		; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A
	JSR _txtmode		; Switch to text mode

	LDA #24
	STA $07E5

	LDA #<rate_offs
	STA $D8
	LDA #>rate_offs
	STA $D9

	JSR set_prefs
	JSR bck_data

	CLI
;...
	; Print message
	+StringOut sut1
	+SetCursor $00,$16
	+StringOut sut3
	LDA _load_drive
	BMI +
	+StringOut sut3_
+	LDA #$80
	STA $0540			; Repeat all keys

	JSR refresh_init	; Show modem init string
	JSR show_rate		; Show modem init baud rate

--
	+SetCursor $14,$02
	+StringOut sut2
	LDX rb1l+1
	LDA rb1h+1
	STX	_sudtmp
	STA _sudtmp+1

	JSR $A45F			; Print RTS delay

-	JSR $EBDD			; Read keyboard buffer
	BEQ -
	CMP	#$2B			; (+)
	BNE +
	INC rb1l+1
	BNE --
	INC rb1h+1
	JMP --
+	CMP	#$2D			; (-)
	BNE ++
	LDA rb1l+1
	BNE +
	DEC rb1h+1
+	DEC rb1l+1
	JMP --

++	CMP #$49			; (I)
	BNE +
	JSR edit_init
	JMP --

+	CMP #$52			; (R)
	BNE +
	JSR cycle_rates
	JMP --

+	CMP #$50			; (P)
	BNE +
	JMP phone_book

+	CMP #$87			; (F5)
	BNE ++
	LDA _load_drive
	BMI +
	JSR psave
+	JMP --

++
	CMP #$85			; (F1)
	BEQ +
	BNE --
+
dsexit:

;....
	LDA #$00
	STA $0540		; Default key repeat
	SEI
	LDX #$EA
-	LDA _sudtmp+4,X
	STA setupdata+4,X
	DEX
	BPL -
	JSR data_bck
	JSR _resp		; copy new prefs to setupdata

	LDA	#%10100000	; Disable raster interrupt signals from TED
	STA	$FF0A

	PLA
	STA $053B
	PLA
	STA $FF19		; Restore screen colors
	PLA
	STA $FF15
	JSR $D88B		; Clear screen
	+DisROMs
	RTS

; --- Minimal IRQ
suIRQ:

	PHA			; store register A in stack
	TXA
	PHA			; store register X in stack
	TYA
	PHA			; store register Y in stack

	LDA	#%10000010	; Limpia todas las banderas de interrupcion, incluyendo la de interrupcion de barrido
	STA	$FF09
	LDA	#%10100010	; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A

	LDA $FB
	PHA
	LDA #$00
	STA $FB
	PHP
	CLI
	JSR $DB11		; Scan keyboard
	PLP
	PLA
	STA $FB


	PLA
	TAY			; restore register Y from stack
	PLA
	TAX			; restore register X from stack
	PLA			; restore register A from stack
	;JMP	$CE0E		; Jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc.
	JMP $FCBE

; --- Refresh modem init string
refresh_init:
	+SetCursor $00,$06
	;Delete whole line
	LDY #40
-	LDA #$14
	JSR CHROUT
	DEY
	BNE -
	LDA #<(_sudtmp+5)
	LDY #>(_sudtmp+5)
	JSR printstring
;	+StringOut _sudtmp+5
	RTS

; --- Edit modem init string
edit_init:
	+EnROMs
	LDA #$00
	STA _sudtmp+5		; Remove the previous string
	JSR refresh_init	; Clear previous string on screen
	LDA #>_filter
	LDX #<_filter
	LDY #38
	JSR FILTERED_INPUT
	LDY #$00			; Copy new init string
-	LDA fibuffer,Y
	STA _sudtmp+5,Y
	BEQ +
	INY
	CPY #38
	BNE -
	DEY
	LDA #$00			; safeguard for long/unterminated string
	STA _sudtmp+5,Y
+	JSR refresh_init	; Refresh init string on screen
	SEI
	;+DisROMs
	RTS

; --- cycle modem init baudrates
cycle_rates:
	+EnROMs
	+SetCursor $19,$07
	LDX _sudtmp+4
	INX
	TXA
	AND #$03
	STA _sudtmp+4
	TAY
_cr1:
	LDA ($D8),Y
	CLC
	ADC #<rates
	TAX
	LDA #$00
	ADC #>rates
	TAY
	TXA
	JSR STROUT
	SEI
	;+DisROMs
	RTS

; --- display modem init baudrate
show_rate:
	+SetCursor $19,$07
	LDY _sudtmp+4
	BPL _cr1

; --- Display phone book
phone_book:
	+StringOut pbook0
	+SetCursor $02,$02
	LDA #<(_sudtmp+$2B)
	LDY #>(_sudtmp+$2B)
	JSR printstring
	+SetCursor $02,$04
	LDA #<(_sudtmp+$52)
	LDY #>(_sudtmp+$52)
	JSR printstring
	+SetCursor $02,$06
	LDA #<(_sudtmp+$79)
	LDY #>(_sudtmp+$79)
	JSR printstring
	+SetCursor $02,$08
	LDA #<(_sudtmp+$A0)
	LDY #>(_sudtmp+$A0)
	JSR printstring
	+SetCursor $02,$0A
	LDA #<(_sudtmp+$C7)
	LDY #>(_sudtmp+$C7)
	JSR printstring

	CLI
-	JSR $EBDD			; Read keyboard buffer
	BEQ -

	CMP #$5f			; <- back key
	BNE +
	JMP +++
+	CMP #$31			; 1
	BCC -				; less ->
	CMP #$36			; 6
	BCS -				; more or equal ->
	PHA
	+SetCursor $0D,$0D
	PLA
	PHA
	JSR CHROUT
	+StringOut pbook1

	PLA
	SEC
	SBC #$31			; .A = preset-1
	ASL
	TAX
	LDA pstable,X
	STA .ph0+1
	STA .ph1+1
	LDA pstable+1,X
	STA .ph0+2
	STA .ph1+2

--	JSR $EBDD			; Read keyboard buffer
	BEQ --

	CMP #$5f			; <- back key
	BNE +
	JMP phone_book
+	CMP #$45			; Edit
	BNE ++
	LDA #>_filter
	LDX #<_filter
	LDY #38
	JSR FILTERED_INPUT

	LDY #$00
-	LDA fibuffer,Y
.ph0
	STA _sudtmp+$2B,Y
	BEQ +
	INY
	CPY #38
	BNE -
	DEY
	LDA #$00			; safeguard for long/unterminated string
	BEQ .ph0
+	JMP phone_book

++	CMP #$44			; "Dial"
	BNE --
	LDA FLAGS1
	AND #$02
	BNE --				; Only dial if terminal already started up

	LDY #$00
.ph1
	LDA _sudtmp+$2B,Y
	BEQ +
	JSR SendID
	INY
	CPY #38
	BNE .ph1
+	LDA #$0D
	JSR SendID
	JMP dsexit

; !if _HARDTYPE_ = 56{
; 	+DisKernal x
; }

+++
	JSR data_bck
	JMP _dosetup

pstable:
!word _sudtmp+$2B,_sudtmp+$52,_sudtmp+$79,_sudtmp+$A0,_sudtmp+$C7


; --- Copy setupdata to _sudtmp
bck_data:
	SEI
	+DisROMs
	LDX #_sudend-setupdata+1;$EE
-	LDA setupdata-1,X
	STA _sudtmp-1,X
	DEX
	BNE -
	+EnROMs
	CLI
	RTS

; --- Copy _sudtmp to setupdata
data_bck:
	SEI
	+DisROMs
	LDX #_sudend-setupdata+1
-	LDA _sudtmp-1,X
	STA setupdata-1,X
	DEX
	BNE -
	+EnROMs
	CLI
	RTS

delfname:	!text "S0:"
pfname:		!text "RT.PREF"
pfname_n:	!text ",S,W"
pfname_e:
_load_drive: !byte $FF

; Set/Reset setupdata from in-code values
res_prefs:
	SEI
	+DisROMs
_resp:
	LDA rb1l+1		; RTS Delay
	STA setupdata
	LDA rb1h+1
	STA setupdata+1
	LDA #$00
	STA setupdata+2	; ACIA base
	STA setupdata+3	; Screen enable
	+EnROMs
	CLI
	RTS

; Set pref values from setupdata
set_prefs:
	SEI
	+DisROMs
	LDA setupdata		; RTS delay
	STA rb1l+1
	LDA setupdata+1
	STA rb1h+1
	+EnROMs
	CLI
	RTS

;======================================================================
; Adapted from codebase64, original by Schema
; Input a string and store it in fibuffer, terminated with a null byte.
; x:a is a pointer to the allowed list of characters, null-terminated.
; max # of chars in y
; returns num of chars entered in y.
;======================================================================

; Example usage
; FILTERED_TEXT
;   lda #>_filter
;   ldx #<_filter
;   ldy #38
  ;Drop through

; Main entry
FILTERED_INPUT:
	STY _fimax
	STX .ficheck+1
	STA .ficheck+2

  ;Zero characters received.
	LDA #$00
	STA _ficount
	+StringOut _cursor+1	; Print cursor

;Wait for a character.
.figet
	JSR GETIN
	BEQ .figet

	STA _filast

	CMP #$14               ;Delete
	BEQ .fidel

	CMP #$0D               ;Return
	BEQ .fidone

	;Check the allowed list of characters.
	LDX #$00
.ficheck
	LDA $FFFF,x           ;Overwritten
	BEQ .figet         ;Reached end of list (0)
	
	CMP _filast
	BEQ .fiok           ;Match found
	
	;Not end or match, keep checking
	INX
	BNE .ficheck
	
.fiok
	LDA _filast          ;Get the char back
	LDY _ficount
	STA fibuffer,y        ;Add it to string
	STA _cursor
	LDA #<_cursor
	LDY #>_cursor
	JSR printstring
	;JSR CHROUT             ;Print it

	INC _ficount           ;Next character

	;End reached?
	LDA _ficount
	CMP _fimax
	BEQ .fidone
	;Not yet.
	BNE .figet

.fidone
   LDY _ficount
   LDA #$00
   STA fibuffer,y   ;Zero-terminate
   RTS

; Delete last character.
.fidel
	;First, check if we're at the beginning.  If so, just exit.
	LDA _ficount
	BNE +
	JMP .figet

	;At least one character entered.
	;Move pointer back.
+	DEC _ficount

	;Store a zero over top of last character, just in case no other characters are entered.
	LDY _ficount
	LDA #$00
	STA fibuffer,y

	;Print the delete char
	LDA #$14
	STA _cursor
	+StringOut _cursor
	; JSR CHROUT

	;Wait for next char
	JMP .figet

; Needed because BASIC Stringout doesn't play nice with quotes
printstring:
	STA $6F
	STY $70
	LDY #$00
-	LDA ($6F),Y
	BEQ +
	TAX
	TYA
	PHA
	TXA
	JSR CharOut
	PLA
	TAY
	INY
	BNE -
+	RTS


_cursor
	!byte $A6,$A6,$9D,$00	; Hash, hash, crsr left, null
_filter
	!text " ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890.,-+!?%&'()*:",$22,$00
_fimax
	!byte $00

_filast
	!byte $00

_ficount
	!byte $00

; --- Open file
channel:	!byte $00
fdrive: 	!byte $08

; A: Name length
; X/Y: Name pointer
; channel: Secondary address
; fdrive: drive
; File number same as channel, only one file open at a time anyways
fopen:
	JSR	SETNAM	; SETNAM
	LDX fdrive
	LDY channel
	TYA			; file number
	JSR SETLFS	; SETLFS
	JMP KOPEN	; Open (Carry clear if ok)
	;RTS

; --- File close
fclose:
	LDA channel	; file number
	JSR KCLOSE	; CLOSE
	JMP CLRCHN	; CLRCHN

; --- Read error channel
; - CLOSE OTHER FILES BEFORE CALLING -
rechan:
	LDA #<errbuf
	STA $9B
	LDA #>errbuf
	STA $9C
	LDA #$0F
	STA channel
	LDA #$00
	TAX
	TAY
_rc1:
	JSR fopen
	BCS +		; error ->
	LDX channel
	JSR CHKIN	; CHKIN
	LDA #$00
-	JSR READST	; READST
	BNE +		; EOF or read error ->
	JSR CHRIN	; CHRIN
	STA ($9B),Y
	INC $9B
	BNE -
	INC $9C
	BNE -
+
	JMP fclose

; --- Save pref file
psave:
	; First update preferences data
	JSR res_prefs
	JSR set_prefs
	; then, try to delete existing file
	LDA #<errbuf
	STA $9B
	LDA #>errbuf
	STA $9C
	LDA #$0F
	STA channel
	LDA _load_drive
	STA fdrive
	LDA #pfname_n-delfname
	LDX #<delfname
	LDY #>delfname
	JSR _rc1				; Send command
	; Save data
	LDA #$02
	STA channel
	LDA _load_drive
	STA fdrive
	LDA #pfname_e-pfname
	LDX #<pfname
	LDY #>pfname
	JSR fopen
	BCS .ser				; if carry set, an open error has ocurred
	LDX channel
	JSR CHKOUT				; CHKOUT
	LDA #<_sudtmp
	STA $9B
	LDA #>_sudtmp
	STA $9C
	LDY #$00
-	JSR READST				; READST
	BNE .ser				; error ->
	LDA ($9B),Y
	JSR CHROUT				; CHROUT
	INY
	CPY #_sudend-setupdata	;45
	BNE -
.ser
	JSR fclose
	RTS


; --- Setup Texts
sut1:
	;Clear, Lower/upper, yellow
	!text $93,$0E,$9E,"    --== rETROTERM sETUP SCREEN ==--"
	!text $0D,$0D,"rts PULSE TIMING: ",$12,"+",$92,"       ",$12,"-"
	!text $0D,$0D,"mODEM ",$12,"i",$92,"NIT STRING:",$0D,$0D,$0D
	!text "iNITIAL MODEM BAUD ",$12,"r",$92,"ATE:"
	!byte $00
sut2:
	!text "     "
	!fill $05,$9D
	!byte $00
sut3:
	!text $12," p ",$92," pHONE BOOK",$0D,$0D
	!text $12," f1 ",$92," tERMINAL        ",$00
sut3_:
	!text $12," f5 ",$92," sAVE SETTINGS",$00

; Phonebook screen
pbook0:
	!text $93,$0E,$9E,$12,"               pHONE BOOK               ",$92
	!byte $60,$B2
	!fill $26,$60
	!text $12,"1",$92,$7D,$0D
	!byte $1D,$7D,$0D
	!text $12,"2",$92,$7D,$0D
	!byte $1D,$7D,$0D
	!text $12,"3",$92,$7D,$0D
	!byte $1D,$7D,$0D
	!text $12,"4",$92,$7D,$0D
	!byte $1D,$7D,$0D
	!text $12,"5",$92,$7D,$0D
	!byte $1D,$7D,$0D
	!byte $60,$B1
	!fill $26,$60
	!text $0D,"sELECT ENTRY:"
	!fill $09,$11
	!text $12," ",$5F," ",$92," back",$00

pbook1:
	!text $0D,$12,"e",$92,"DIT/",$12,"d",$92,"IAL",$0D,$00

; Baud rates
rates:
	!text "skip",$00
	!text " 300",$00
	!text "1200",$00
	!text "2400",$00
rate_offs:
	!byte 0,5,10,15

; temporal setup data
_sudtmp:


fibuffer = _sudtmp+$EE	; String input buffer
errbuf = fibuffer

suend
}
_suend

load_drive:	!byte $FF	; Drive to use for preferences file

; Setup data here
; $00: RTS timing low
; $01: RTS timing high
; $02: ACIA base address high (even if not used)
; $03: Flags:
;		bit 0: Screen enable during turbo transfers
; $04: Early startup command baudrate:
;		  0: No early startup command
;		  1:  300 bauds
;		  2: 1200 bauds
;		  3: 2400 bauds
; $05-$2A: Null terminated startup command (38 chars max). Pad with 0s
; $2B-$51: Preset 1, null terminated, 0 padded
; $52-$78: Preset 2...
; $79-$9F: Preset 3...
; $A0-$C6: Preset 4...
; $C7-$ED: Preset 5...
setupdata:
!byte $1C,$00,$DE,$00,$02
!text "ATF0B19200",$00
_sudf
!fill 43-(_sudf-setupdata),$00
_preset1
!text "ATD LU4FBU.DDNS.NET:6400",00
_p1f
!fill 39-(_p1f-_preset1),$00
_preset2
!text "ATD",$22,"SOTANOMSXBBS.ORG:6400",$22,00
_p2f
!fill 39-(_p2f-_preset2),$00
_preset3
!text "",00
_p3f
!fill 39-(_p3f-_preset3),$00
_preset4
!text "",00
_p4f
!fill 39-(_p4f-_preset4),$00
_preset5
!text "",00
_p5f
!fill 39-(_p5f-_preset5),$00
_sudend

; CmdA4 subsystems offsets

A4offsets:
!byte _sub0-A4array,_sub1-A4array,_sub2-A4array,_sub3-A4array
!byte _sub4-A4array,_sub5-A4array,_sub6-A4array,_subnull-A4array

; CmdA4 subsystems response array
A4array:
_sub0
	!byte $01,$01			; $00: Platform/Refresh rate
_sub1
	!byte $02,$28,$19		; $01: Text screen dimensions
_sub2
	!byte $02				; $02: Connection speed
	!byte $08
_sub3
	!byte $02				; $03: RAM size
	!word $0040
_sub4
	!byte $02
	!word $0000				; $04: VRAM size
_sub5
	!byte $01,$00			; $05: Graphic modes
_sub6
	!byte $02,$00,%00001001	; $06: Audio
_subnull
	!byte $00				; Entry not implemented

ENDSHADOW
}
_ENDSHADOW_

;/////////////////////////
;  Early setup code
;/////////////////////////
; Executed before starting up retroterm
; Try to load setup file, if none if found, show setup screen
; if setup file is found, setup and start retroterm
earlysetup:
	SEI
	+DisROMs
	LDA #$02	; Set startup flag bit
	STA FLAGS1
	; Copy routines to lower RAM ($0B00)
	LDX #$01
-	LDA _sudata,X
	STA SOURCEPTR,X
	LDA _sudata+2,X
	STA DESTPTR,X
	LDA _sudata+4,X
	STA CSIZE,X
	DEX
	BPL -
	JSR _MemCpy		;Do mem copy
	JSR res_prefs	; Reset preferences
	JSR bck_data	; And init _sudtmp
	+DisROMs
	LDA load_drive	; Check last used device
	CMP #$08
	BCS ++
	; retroterm was not loaded from disk
	; use first detected device
	LDX #$00
-	CPX #$08
	BNE +
	; +EnKernal a
	; JSR res_prefs	; Reset preferences
	; JSR bck_data
	BEQ .esq		; No available drive found > show setup
+	LDA DRIVES,X
	BPL +			; Available drive
	INX
	BNE -
+	TXA
++	STA load_drive
	STA _load_drive
	+EnROMs
	; CLI
	; JSR res_prefs	; Reset preferences	
	; JSR bck_data	; And init _sudtmp
	JSR loadsetup	; Get preferences from disk, if any
	BCC +			; prefs ok run retroterm
.esq
	; LDA #$00
	JSR dosetup	; prefs error, run setup, alternative entry point
+	+EnROMs
	LDA _sudtmp+4
	BEQ +			; Skip init string
	JSR _init_modem
+	RTS

; --- Load setup file
loadsetup:
	LDA #$02
	STA channel
	LDA _load_drive
	STA fdrive
	LDA #pfname_n-pfname
	LDX #<pfname
	LDY #>pfname
	JSR fopen
 	BCS .oerr		; if carry set, an open error has happened
	LDX channel		; filenumber 2
	JSR CHKIN		; call CHKIN (file 2 now used as input)

	LDA #<_sudtmp
	STA $9B
	LDA #>_sudtmp
	STA $9C

	LDY #$00

-	JSR READST		; call READST (read status byte)
	BNE .eof		; either EOF or read error
	JSR CHRIN		; call CHRIN (get a byte from file)
	STA ($9B),Y		; write byte to memory
	INC $9B
	BNE -
	INC $9C
	JMP -			; next byte

.eof
	CMP #$40		; end of file?
	BNE .oerr
	JSR fclose		; close file
	; Copy setup preferences
	SEI
	+DisROMs
	LDX #$EF
-	LDA _sudtmp-1,X
	STA setupdata-1,X
	DEX
	BNE -
	+EnROMs
	CLI
	CLC				; Prefs loaded ok
	RTS

; handle open or read errors
.oerr
	LDA $90			; STatus
	BMI ++			; Device not present?
	JSR fclose
	JSR rechan
++	SEC				; Prefs load error
	RTS

; Send init string
_init_modem:
	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, receive enable), no rx irq, rx enabled (dtr=0)
	STA ACIACOMMAND
	LDX _sudtmp+4	; get baud rate
	DEX
	LDA _bauds,x
	STA ACIACONTROL
	LDX #$00
-	LDA _sudtmp+5,x
	BEQ +
	JSR SendID
	INX
	CPX #38
	BNE -
+	LDA #$0D
	JSR SendID
	RTS

_bauds:
	!byte %00010110, %00011000, %00011010
