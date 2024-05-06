;////////////////////////////////////////////////////////////////////////////////////////////
; Retroterm, RS232 with tcpser/BBSServer or wifi with zimodem firmware
; Supports TURBO56K v0.7 protocol at 19200 bps, using TX, RX and RTS
;////////////////////////////////////////////////////////////////////////////////////////////

	ORG	&h100
	JP	Start

; Assembly options
FIRQ		EQU 0			; Set to 1 to use USART IRQs - 0 for VBLANK only


; System variables

CGTABL		EQU	&h0004		; Character set address in ROM
VDP_DR		EQU	&h0006		; VDP data read port
VDP_DW		EQU	&h0007		; VDP data write port
EXPTBL		EQU	&hFCC1		; Extended slot flags table (4 bytes)
H.INIP		EQU	&hFDC7
EXBRSA		EQU	&hFAF0		; Slot address of BIOS (main) ROM
LINL40		EQU &hF3AE		; Width For SCREEN 0 (Default = 37)
LINL32		EQU	&hF3AF		; Width For SCREEN 1 (Default = 29)
CLIKSW		EQU	&hF3DB		; CLIKSW - Key Press Click Switch (SCREEN n Writes To This Address)
							; 0 = When Key Press Click Disabled, 1 = When Key Press Click Enabled
OLDKEY		EQU	&hFBDA		; Captura anterior de las filas del teclado
NEWKEY		EQU	&hFBE5		; Ultima captura de las filas del teclado
SCNCNT		EQU	&hF3F6		; Key Scan Counter: 0 = Ready To Scan Pressed Keys
PUTPNT		EQU	&hF3F8		; Keyboard Buffer Address To Write Character
GETPNT		EQU	&hF3FA		; Keyboard Buffer Address To Read Character
KEYBUF		EQU &hFBF0		; Keyboard Buffer Address, 40 bytes
GRPNAM		EQU &hF3C7		; SCREEN 2 name table
GRPCOL		EQU &hF3C9		; SCREEN 2 color table
GRPCGP		EQU &hF3CB		; SCREEN 2 character pattern table
CSRY		EQU	&hF3DC		; Current row-position of the cursor
CSRX		EQU &hF3DD		; Current column-position of the cursor
FNKSTR		EQU &hF87F		; Function key definitions

; BIOS
BOOT		EQU	&h0000		; Tests RAM and sets RAM slot for the system
BDOS		EQU	&h0005		; Punto de entrada al BDOS
RDSLT		EQU	&h000C		; Read slot routine
CALSLT		EQU &h001C		; Inter slot call
DISSCR		EQU	&h0041		; Inhibits the screen display
ENASCR		EQU	&h0044		; Displays the screen
FILVRM		EQU &h0056		; Fill VRAM with value
LDIRVM		EQU &h005C		; Block transfer to VRAM from memory
INITXT		EQU &h006C		; Switches to SCREEN 0 (text screen with 40×24 characters)
INIT32		EQU &h006F		; Switches to SCREEN 1 (text screen with 32x24 characters)
INIGRP		EQU &h0072		; Switches to SCREEN 2 (high resolution screen with 256×192 pixels)
GICINI		EQU &h0090		; Initialises PSG and sets initial value for the PLAY statement
CHGET		EQU	&h009F		; One character input (waiting)
CHPUT		EQU &h00A2		; Displays one character
BEEP		EQU &h0060		; Generates beep
GTSTCK		EQU	&h00D5		; returns the joystick status
GTTRIG		EQU	&h00D8		; returns the trigger button status
GTPDL		EQU	&h00DE		; returns the paddle value
SNSMAT		EQU	&h0141		; reads the value of the specified line from the keyboard matrix

;	PPI
PPI.BR		EQU &hA9
PPI.CW		EQU &hAA
PPI.CR		EQU &hAA

;	i8251	USART
USARTData	EQU &h80		; Write: Tx - Read: Rx
USARTCmd	EQU	&h81		; Mode format (Async mode - Write):
							; bits 0-1 (Baud rate factor)
							;	00:	Sync mode
							;	01: x1
							;	10: x16
							;	11:	x64
							; bits 2-3 (Character length)
							;	00: 5bits
							;	01: 6bits
							;	10: 7bits
							;	11: 8bits
							; bit 4 (Parity enable): 1 - Enable
							; bit 5 (Parity generation/check)
							;	0: Odd parity
							;	1: Even parity
							; bits 6-7 (Stop bits)
							;	00: Invalid
							;	01: 1bit
							;	10: 1 1/2 bits
							;	11: 2 bits
							;
							; Command format (Write):
							; bit 0 (Transmit enable): 1 - Enable
							; bit 1 (DTR): 1 - Active
							; bit 2 (Receive enable): 1 - Enable
							; bit 3 (Send break character): 1 - forces Tx low
							; bit 4 (Error reset): 1 - Reset error flags
							; bit 5 (RTS): 1 - Active
							; bit 6 (Reset): 1 - Return 8251 to Mode format
							; bit 7 (Enter hunt mode): Sync mode only
							;
							; Status (Read):
							; bit 0 (TxReady)
							; bit 1 (RxReady)
							; bit 2 (TxEmpty)
							; bit 3 (PE): Parity error
							; bit 4 (OE): Overrun error
							; bit 5 (FE): Framing error
							; bit 6 (SYNDET/BRKDET)
							; bit 7 (DSR)


USARTIrq	EQU &h82		; Write:
							;	bit 0: 0 - Enable IRQ
							; Read:
							;	bit 0: Carrier Detect
							;	bit 1: Ring Indicator (Not on SVI738)
							;	bit 6: i8253 Counter 2 output
							;	bit 7: Clear To Send


;   i8253	Timer
CNT0		EQU &h84
CNT1		EQU &h85
CNT2		EQU &h86
CNTCTRL		EQU &h87		; bit 0 (BCD):
							;	0 - 16-bit binary mode
							;	1 - 4 decades BCD mode
							; bit2 1-3 (Mode):
							;	000 - Mode 0: IRQ on terminal count (count continues)
							;	001 - Mode 1: Programmable One-Shot
							;	x10 - Mode 2: Rate Generator - Divide by N counter
							;	x11 - Mode 3: Square Wave Rate Generator - Divide by 2N counter
							;	100 - Mode 4: Software Triggered Strobe
							;	101 - Mode 5: Hardware Triggered Strobe
							; bits 4-5 (Read/Load):
							;	00 - Counter latching
							;	01 - Read/Load MSB only
							;	10 - Read/Load LSB only
							;	11 - Read/Load LSB first, then MSB
							; bits 6-7 (Select Counter):
							;	00 - Counter 0
							;	01 - Counter 1
							;	10 - Counter 2
							;	11 - Illegal

; PSG
AYINDEX		EQU	&hA0
AYWRITE		EQU &hA1
AYREAD		EQU &hA2


; BDOS
CWRITE		EQU	2		; console output
RAWIO		EQU	6		; direct console I/O
CONOUT		EQU	9		; console output
WRITESTR	EQU	9		; output string
READSTR		EQU	10		; buffered console input
SELDSK		EQU	24		; pass disk no. in c
SETDMA		EQU	33		; pass address in bc
SETTRK		EQU	27		; pass track in reg C
SETSEC		EQU	30		; pass sector in reg c
WRITE		EQU	39		; write one CP/M sector to disk

; Constants
MAXCMD		EQU	&hB7	; Highest command implemented
SHORTRX		EQU	50		; Short wait time for ReadByte routine
LONGRX		EQU 100		; Longer wait time for ReadByte routine

; Variables
FLAGS1:		DB	&h08	; Status flags
						; Bit 7 : 1 = Command mode; 0 = Normal mode
						; Bit 6 : 1 = Last byte should be redirected to the voice synth; 0 = Last byte is meant for the terminal
						; Bit 4	: 1 = Split screen enabled
						; Bit 3 : 1 = Cursor blink enabled; 0 = Cursor blink disabled
						; Bit 2 : 1 = Microsint enabled; 0 = Microsint disabled / not found 
						; Bit 1 : 1 = Terminal is starting up; 0 = Normal operation
						; Bit 0 : 1 = CTRL-C pressed, returning to SYSTEM
CMDFlags	DB	&h00	; Command flags
						; Bit 7 : 1 = Last received byte is CMDON (&hff); 0 = Normal operation
						; Bit 6 : 1 = Receive N bytes as parameters and wait for the command to complete; 0 = Normal operation
						; Bits 3-0: Parameter counter for bit-6
VPORTR:		DB	&h00	; VDP Read port
VPORTW:		DB	&h00	; VDP Write port
ACOLORS:	DB	&hF1	; Current active Attributes (affected by RVS on)
SCOLORS:	DB	&hF1	; Current set Attributes
EDSTAT:		DB  &h00	; Screen editor status flags
						; bit 0: GRAPH  -> 1 if last character was &h01
						; bit 1: RVSON  -> 1 if reverse video is on
						; bit 2: CURSOR -> 1 if cursor display is on
						; bit 3: BLINK  -> Cursor blink state: 0 normal character display - 1 inverse character display
						; bits 4-5: Screen mode
						;			00: Text
						;			01: Bitmap (Screen 2)
						;			10: Split screen
						; bit 7: Reception Flag -> 1 if done for the frame
NTOSET:		DW	&h0000	; Name tables offset for current cursor position
;PTOSET:		DW	&h0000	; Pattern tables offset
CUPATT:		DB	&h00	; Current pattern under cursor
CUATTR:		DB	&hF1	; Current attributes under cursor
BORDER:     DB  &h01    ; Border color
PRBUFFERCNT:	DB	&h00	; Print buffer byte counter
PRINDEXIN:	DB	&h00	; Index to the first free byte in the print buffer
PRINDEXOUT:	DB	&h00	; Index to the print buffer byte to output next
RXBYTE:		DB	&h00	; Latest received byte
WTOP:		DB	&h00	; Top of the text window
WBOTTOM:	DB	&h18	; Bottom of the text window + 1
TEMPCNT1	DB	&h00	; Beep counter - Updated each time the Beep routine is called
TEMPCNT2	DB	&h00	; Frame counter - Updated each ISR
TIMER1		DB	&h00	; Text timer
PRSPEED		DB	&h00	; Text printing speed
BLOCKPTR	DW	&h0000	; Memory pointer for the block transfer command
BYTECNT		DW	&h0000	; Block transfer byte count

Start:
	LD		C,WRITESTR
	LD		DE,INTRO
	CALL	BDOS
.s0
	CALL	GetKey
	CP		0
	JR		Z,.s0

	CALL	VDPDR			; Obtiene el puerto de lectura del VDP y lo copia a VPORTR
	LD		(VPORTR),A
	CALL	VDPDW			; Obtiene el puerto de escritura del VDP y lo copia a VPORTW
	LD		(VPORTW),A

	CALL	PSGIni
	CALL	SetISR
	CALL	Setfkeys
	CALL	InitComm

    ; Switch to Screen 2
	LD      IX,INIGRP
    LD      IY,(EXPTBL-1)	; get expanded slot status to IYH
    CALL    CALSLT			; perform an inter-slot call

	; Init screen

	CALL	ClrScr

loop1:
	LD		A,(PRBUFFERCNT)
	CP		0
	JR		NZ,.l00
	LD		A,(TIMER1)
	CP		0
	JR		NZ,.l00
	CALL	NoChar
	JR	doblink
.l00
	LD		A,(TIMER1)
	CP		0
	JR		NZ,doblink
	CALL	NoChar
	CALL	PrintBuffer
	LD		HL,TIMER1
	INC		(HL)

doblink
	LD		A,(FLAGS1)
	BIT		0,A
	JR		NZ,End1			; CTRL-C pressed
	BIT		3,A
	JR		Z,loop1			; Cursor Off, go on to print buffer
	LD		A,(EDSTAT)
	AND		8				; Get just cursor blink status
	LD		B,A				; Save in B
	LD		HL,CFlag
	XOR 	(HL)
	JR		Z,loop1			; If same as before, go on to print buffer
	LD		A,B
	LD		DE,(NTOSET)
	LD		(CFlag),A
	CP		0
	JR		Z,.l0
	LD		A,(SCOLORS)		; Get current cursor attributes
	LD		(Blink),A		; temp storage
	LD		HL,Blink
	RRD						; Swap nibbles (BG<->FG) in (HL)
	JR		.l1
.l0
	LD		HL,CUATTR
.l1
	LD		B,(HL)			; B: Attributes of character under cursor
	LD		A,(CUPATT)
	LD		E,A		; DE: Pattern under cursor
	CALL	FillColor		; Update colors under cursor blink
.l2
	JR		loop1

End1:
	CALL	NoChar
	CALL	RestoreISR
	LD		B,40
	LD		A,(EXBRSA)		; slot address of SUB-ROM
	OR		A				; 0 if MSX1
	PUSH	AF
	JR		Z,TO40
	LD		B,80

TO40:
	LD		A,B
	LD      (LINL40),A		; set width into work area
	POP	    AF
    LD      IX,INITXT
    LD      IY,(EXPTBL-1)	; get expanded slot status to IYH
    CALL    CALSLT			; perform an inter-slot call
    EI						; because CALSLT do DI
	JP		BOOT			; Return to DOS

CFlag:
	DB		0				; Last cursor blink status

;/////////////
; ISR test
SetISR:
	DI
	LD		HL,(0x0039)		; Read original ISR address
	LD		(isrjp+1),HL	; And store in self modifying code
	LD		HL,newISR
	LD		(0x0039),HL		; Set new ISR
	EI
	RET

RestoreISR:
	DI
	LD		HL,(isrjp+1)
	LD		(0x0039),HL
	EI
	RET

newISR:
	DI
	PUSH	AF
	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD		HL,EDSTAT
	RES		7,(HL)		; Clear reception bit
	LD		HL,TIMER1
	LD		A,(HL)
	CP		0
	JR		Z,ChkTimer2
	DEC		(HL)		; if TIMER1 != 0, decrement it
ChkTimer2
	LD		A,(Bcount)	; Cursor blink counter
	LD		D,A
	DEC		D
	CP		0
	JP		NZ,.recb

	LD		A,(EDSTAT)
	XOR		8
	LD		(EDSTAT),A
	LD		D,15		; Reset counter
.recb:	; Receive up to 3 bytes
	LD		B,3
.rb0
	LD		A,(PRBUFFERCNT)
	XOR		&hFF
	JR		Z,.inbyte		; If input buffer full check if any char in keyb buffer
	LD		A,(CMDFlags)
	BIT		6,A				; Waiting for parameters?
	JR		Z,.rb1			; If we're not waiting for parameters, receive character
	AND		&h0F
	JR		Z,.inbyte		; If we were waiting for parameters but all were received, do not receive more characters
							; until the command is completed
.rb1
	PUSH	BC
	CALL	ReadByte
	POP		BC
	JP		NC,.rece		; if no character received, loop
	CALL	AddToPrBuffer	; otherwise add byte to print buffer

; Commented out for I8251 USART
; RTS has delay and incoming stream cannot be stopped with precision
; 

; 	LD		A,(CMDFlags)
; 	BIT		7,A
; 	JR		NZ,.cmdchk		; If the last character was CMDON, check which command was received
; 	BIT		6,A
; 	JR		NZ,.con1		; Branch if we're waiting for parameters
; 	LD		C,A
; 	LD		A,(RXBYTE)		; Not waiting for command or parameters
; 	LD		E,A				; Save RXBYTE
; 	BIT		7,A
; 	JR		Z,.rece			; If bit 7 is not set, it isn't a command
; 	LD		A,(FLAGS1)
; 	BIT		7,A				; If bit 7 is set, check that we are in command mode
; 	JR		NZ,.cmdchk		; Yes, it is a command
; 	LD		A,E				; Get RXBYTE
; 	XOR		&hFF			; Check if the received character is CMDON
; 	JR		Z,.cmdon
; 	CP		&h01			; Check if the character is an extraneous CMDOFF
; 	JR		NZ,.rece
; 	LD		A,&h00			; Last received characters is an extraneous CMDOFF
; 	JR		.con0
; .cmdon
; 	LD		A,%10000000		; Last received character is CMDON
; .con0
; 	LD		(CMDFlags),A	; Set CMDFlags
; 	JR		.rece
; .con1
; 	DEC		C				; A parameter character was received, decrement counter
; 	LD		A,C
; 	LD		(CMDFlags),A

.rece:
	DJNZ	.rb0			; if != 0, return to .rb0
	LD		HL,EDSTAT
	BIT		7,(HL)			; check if we're done for the frame
	JR		NZ,.inbyte		; Yes, Continue on .inbyte
	INC		B				; Loop 1 more time
	SET		7,(HL)			; Set receive flag
	JR		.rb0			; Loop

; Commented out for I8251 USART
; RTS has delay and incoming stream cannot be stopped with precision

; .cmdchk						; Check the received command and set CMDFlags accordingly
; 	LD		A,(RXBYTE)		; Get the received character
; 	BIT		7,A
; 	JR		Z,.cd0			; Invalid command (bit 7 unset)
; 	CP		MAXCMD+1
; 	JR		C,.cd1			; Is it less than or equal to the highest implemented command ($B6)?
; .cd0
; 	LD		A,&h8F			; Invalid command, replace with unimplemented command ($8F)
; .cd1
; 	AND		%01111111		; -128
; 	LD		E,A
; 	LD		D,0
; 	LD		HL,CmdParTable
; 	ADD		HL,DE
; 	LD		A,(HL)			; Parameter count
; 	AND		%00001111		; Clear unwanted bits
; 	OR		%01000000		; Enable parameter wait
; 	LD		(CMDFlags),A	; Set CMDFlags
; 	JR		.rece			; Continue the loop

.inbyte
	LD		HL,(GETPNT)		; Check if key(s) in buffer
	LD		A,(PUTPNT)		; lower byte of PUTPNT
	SUB		L				; Check if same
	JR		Z,.eisr
	LD		C,(HL)			; Get pressed key
	INC		HL				; Update GETPNT
	LD		A,L
	CP		&h18
	JR		NZ,.ib0
	LD		HL,KEYBUF		; Reset to start of buffer
.ib0
	LD		(GETPNT),HL		;<-/
	LD		A,C
	CP		3				; CTRL-C?
	JR		NZ,.ib1
	LD		HL,FLAGS1
	SET		0,(HL)			; Set CTRL-C flag
	JR		.eisr
.ib1
	OUT		(USARTData),A	; Send key
.eisr:
	LD		A,D
	LD		(Bcount),A
	POP		HL
	POP		DE
	POP		BC
	POP		AF
isrjp:
	JP		newISR

Blink:
	DB		&hAA
Bcount:
	DB		15

;/////////////
; Init 8251 - Partly based on SVI ROM disassembly
InitComm:
	LD		DE,(BaudTable+2)	; Set 19200 bps
	; LD		DE,(BaudTable+4)	; Set 38400 bps
	LD		C,0
	CALL	CSet				; Rx Clock
	INC		C
	CALL	CSet				; Tx Clock
	XOR		A				; Get 8251 in command mode
	OUT		(USARTCmd),A
	PUSH	AF
	POP		AF
	OUT		(USARTCmd),A
	PUSH	AF
	POP		AF
	OUT		(USARTCmd),A
	PUSH	AF
	POP		AF
	LD		A,&h40			; Reset 8251
	OUT		(USARTCmd),A
	PUSH	AF
	POP		AF
	LD		A,%01001110		; x16 8N1
	; LD		A,%01001101		; x1  8N1
	OUT		(USARTCmd),A	; Set 8251 async mode
	PUSH	AF
	POP		AF
	LD		A,&h07			; Activate Rx,Tx and DTR
	CALL	UCmd
	CALL	CharIn
IF FIRQ
	LD		A,&h00
ELSE
	LD		A,&h01
ENDIF
	OUT		(USARTIrq),A	; Disable IRQ
	RET

;/////////////
CharIn:
	IN		A,(USARTData)
	RET

UCmd:
	OUT		(USARTCmd),A
	PUSH	AF
	POP		AF
	RET

;/////////////
;PROGRAM THE DIVISION FACTOR FOR THE i8253 INTERNAL COUNTERS
;In :C =counter number 0= receive clock  1= transmit clock
;   :DE=division factor
CSet:
	PUSH	AF				;push registers
	PUSH	BC
	PUSH	DE
	LD		A,C				;pass the counter number to A
	LD		B,C				;and also to B
	ADD		A,CNT0			;add the i8253 base port (cntr 0)
	LD		C,A				;pass the counter port to C
	LD		A,B				;get the counter number
	RRCA					;pass it to
	RRCA					;bits 6 and 7
	OR		36H				;r/w LSB first,sq wave gen, bin
	OUT		(CNTCTRL),A		;i8253 mode register
	PUSH	AF
	POP		AF
	OUT		(C),E			;send the division factor
	PUSH	AF
	POP		AF
	OUT		(C),D			;to the chip
	POP		DE				;pop registers
	POP		BC
	POP		AF
	RET						;ret

;/////////////
; BaudTable

BaudTable:
	DW	&h000C				; 9600	x16
	DW	&h0006				; 19200	x16
	; DW	&h0040				; 28800 x1 UNRELIABLE
	; DW	&h0003				; 38400 x16 X	Doesn't work max x16 baudrate is around 25.5K
	; DW	&h0030				; 38400	x1 ?	UNRELIABLE
	; DW	&H0020				; 57600 x1 ?	not worth testing

;/////////////

;///////////////////////////////////////////////////////////////////////////////////
; Screen print routine, prints the full buffer
;///////////////////////////////////////////////////////////////////////////////////

PrintBuffer
.pb0
	;LD		A,(PRBUFFERCNT)	; If the buffer is empty, exit
	;CP		0
	LD		HL,PRBUFFERCNT
	LD		A,0
	CP		(HL)
	RET		Z
	;DI
	;LD		B,A
	;LD		A,(PRINDEXOUT)
	;LD		E,A
	;LD		D,HIGH(PrBuffer)
	LD		A,(FLAGS1)
	BIT		7,A
	JR		NZ,.pb2			; Command mode? -> Parse comnands
	BIT		6,A
	JR		NZ,.pb4			; Voice mode, skip character
 	;LD		A,(DE)
	CALL	GetFromPrBuffer	; Get byte from buffer
	CP		&hFF			; Check for commands
	JR		Z,.pb1

	CP		&hFE			; Check for command mode exit <- Captures extraneous $FE characters
	JR		Z,.pb0

	; EX		AF,AF'			; Save A
	; DEC		B
	; LD		A,B
	; LD		(PRBUFFERCNT),A
	; INC		E
	; LD		A,E
	; LD		(PRINDEXOUT),A
	; EX		AF,AF'			; Retrieve A
	;EI
	CALL	CharOut			; Output character to screen
	CALL	DoBeep
	LD		A,(PRSPEED)
	LD		(TIMER1),A		; Renew TIMER1 with PRSPEED
	JR		.pb0			; Return to .pb0 to process the rest of the buffer
; // Enter command mode
.pb1
	; DEC		B
	; LD		A,B
	; LD		(PRBUFFERCNT),A
	; INC		E
	; LD		A,E
	; LD		(PRINDEXOUT),A	; Skip the $FF character
	; EI
	LD		HL,FLAGS1
	SET		7,(HL)
	; LD		A,(FLAGS1)
	; OR		%10000000
	; LD		(FLAGS1),A		; Enters command mode
	JR		.pb0
; Parse commands
.pb2
	CALL	NoChar			; Mutes the print beep
	;DEC		B
	;LD		A,B
	;LD		(PRBUFFERCNT),A
	;LD		A,E
	;INC		A
	;LD		(PRINDEXOUT),A	; Update pointers
	;LD		A,(DE)			; A = Command byte
	CALL	GetFromPrBuffer	; Get byte from buffer
	BIT		7,A
	;EI
	JR		Z,.pb2_1		; Invalid command (bit7=0)
	CP		MAXCMD+1
	JR		C,.pb2_2		; Is it less than or equal to the highest implemented command?
.pb2_1
	LD		A,&h8F			; Invalid command, replace with unimplemented command ($8F)
.pb2_2
	AND		%01111111		; -128
	SLA		A				; x2 (Carry 0)
	LD		C,A
	LD		B,0
	LD		HL,CmdTable
	ADD		HL,BC
	LD		A,(HL)			; A <- Command jump address MSB
	LD		(.pb3+1),A
	INC		HL
	LD		A,(HL)			; A <- Command jump address MSB
	LD		(.pb3+2),A
.pb3
	CALL	CmdFE			; Command call<<<<
	LD		A,0
	LD		(CMDFlags),A	; Command completed, reset CMDFlags
	JR		.pb0			; Return to .pb0 to process the rest of the buffer

.pb4                ; Insert byte into the voice buffer
	CALL	NoChar		; Mute the print beep
 	;LD		A,(DE)
	CALL	GetFromPrBuffer	; Get byte from buffer
 	CP 		&hFF		; Check for commands
 	JR		z,.pb1
	; EX		AF,AF'			; Save A
	; DEC		B
	; LD		A,B
	; LD		(PRBUFFERCNT),A
	; INC		E
	; LD		A,E
	; LD		(PRINDEXOUT),A
	;EX		AF,AF'			; Retrieve A
	;EI
 	JR		.pb0

;///////////////////////////////////////////////////////////////////////////////////
; Get a character from the print buffer
;///////////////////////////////////////////////////////////////////////////////////
GetFromPrBuffer
	LD		A,0
    ;LD		A,(PRBUFFERCNT)
	LD		HL,PRBUFFERCNT
	CP		(HL)				;0
	JR		Z,GetFromPrBuffer	; Wait for a character in the print buffer
.gb0
	DI
	NOP
	DEC		(HL)				;A
	;LD		(PRBUFFERCNT),A
	LD		A,(PRINDEXOUT)
	LD		E,A
	INC		A
	LD		(PRINDEXOUT),A		; Update pointers
	LD		D,HIGH(PrBuffer)
    LD		A,(DE)				; Get character
	EI
    RET

;////////////////////////////////////////////////////////////////////////////////////
; Insert the character in A, to the print buffer
; (!) PrBuffer must be page aligned (!)
;///////////////////////////////////////////////////////////////////////////////////
AddToPrBuffer
	;EX		AF,AF'
	PUSH	AF
	LD		A,(PRINDEXIN)	; Loads E with PRINDEXIN
	LD		E,A
.apb0
	LD		A,(PRBUFFERCNT)	; If PRBUFFERCNT=255 (buffer full) waits until a space is open
	LD		C,A
	XOR		&hFF
	JR		Z,.apb0
	;DI						; Disable IRQs
	;EX		AF,AF'
	POP		AF
	LD		D,HIGH(PrBuffer)
	LD		(DE),A
	INC		E
	LD		A,E
	LD		(PRINDEXIN),A	; Increment PRINDEXIN
	INC		C
	LD		A,C
	LD		(PRBUFFERCNT),A	; and PRBUFFERCNT
	;EI				; Enable IRQs
	RET

; ChrGet:
; 	LD 		IX,CHGET
; 	LD		IY,(EXPTBL-1)
; 	CALL	CALSLT
; 	EI
; 	RET

; ChrOut:		;Output character from BIOS
; 	LD		IX,CHPUT
; 	LD		IY,(EXPTBL-1)
; 	CALL	CALSLT
; 	EI
; 	RET

PSGIni:		;Initialize PSG
	; LD		IX,GICINI
	; LD		IY,(EXPTBL-1)
	; CALL	CALSLT
	LD		C,AYINDEX
	LD		A,7
	LD		DE,&h08BF
	OUT		(C),A
	INC		C
	OUT		(C),E		; Disable all voices
	INC		A
	DEC		C
	OUT		(C),A
	INC		C
	OUT		(C),D		; Set Channel 1 volume to 8
	RET

;///////////////////////////////////////////////////////////////////////////////////
; Play print beep
;///////////////////////////////////////////////////////////////////////////////////
DoBeep:
	LD		C,AYINDEX
	LD		B,AYWRITE
	LD		HL,TEMPCNT1
	LD		A,(HL)
	INC		(HL)
	AND		&h02		; Is TEMPCNT1 odd?
	LD		DE,&h005B	; Channel 1 frequency = 1219 Hz
	JR		NZ,DoBeep2
	LD		DE,&h00A7	; Channel 1 frequency = 671 Hz
DoBeep2
	LD		A,0
	OUT		(C),A		; #R0 Channel 1 Freq LSB
	INC		C
	OUT		(C),E
	DEC		C
	INC		A
	OUT		(C),A
	INC		C
	OUT		(C),D		; #R1 Channel 1 Freq MSB
	DEC		C
	LD		A,7
	OUT		(C),A
	INC 	C
	LD		A,%10111110	; Channel 1 Tone enable
	OUT		(C),A
	RET

WaitNoChar
	LD		HL,TEMPCNT2
	LD		A,(HL)
.wnc
	CP 		(HL)
	JR		Z,.wnc	; Wait for one frame
NoChar
	LD		C,AYINDEX
	LD		A,7
	OUT		(C),A
	INC		C
	LD		A,%10111111	; Channel 1 Tone disable
	OUT		(C),A
	RET

; ---------------------------------------
;	Turbo 56K commands
; ---------------------------------------

;///////////////////////////////////////////////////////////////////////////////////
; 128: Set the transfer memory pointer, requires 2 parameter
;      bytes: destination address (low, high)

Cmd80
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	LD		(BLOCKPTR),A		; and store it in BLOCKPTR
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	LD		(BLOCKPTR+1),A	; and store it in BLOCKPTR + 1
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 129: Select a preset address for memory transfer, 
;	   requires 1 parameter byte: Destiny address preset

Cmd81
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	;EOR #$00
	CP		&h00
	JR		Z,Addr00		; If $00 go to Addr00	; Text/Name table
	CP		&h10
	JR		Z,Addr10		; If $10 go to Addr10	; Pattern table
	BIT		5,A
	JR		NZ,Addr20		; If $20 go to Addr20	; Color table
	LD		HL,&h4000		; Otherwise set BLOCKPTR to $4000 (RAM Page 1)
.c810
	LD		(BLOCKPTR),HL
	RET 
Addr00
	; Point BLOCKPTR to $5800
	LD		HL,&h5800
	JR		.c810
Addr10
	; Point BLOCKPTR to $4000
	LD		HL,&h4000
	JR		.c810
Addr20
	; Point BLOCKPTR to $5B00 (gfx color table)
	BIT		0,A
	JR		Z,.c811
	LD		HL,&h5B00
	JR		.c810
.c811
	; Point BLOCKPTR to ColorRAM0 (text color table)	<<<< change to ColorRAM0
	LD		HL,ColorRAM0
	JR		.c810

;///////////////////////////////////////////////////////////////////////////////////
; 130: Transfers a byte block to memory, requires 2 parameter bytes
;      Byte count (low, high)

Cmd82
	CALL	NoChar			; Mute beep
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	LD		C,A
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	LD		B,A


_Cmd82	;Alternative entry point
	DI					; Disable IRQs
	LD		HL,EDSTAT
	RES		7,(HL)		; Clear reception bit
	DEC		BC		;??
	LD		(BYTECNT),BC
	LD		HL,(BLOCKPTR)

	LD		A,SHORTRX
	LD		(.rts0+1),A		; Set shorter Rx timing

	; LD		A,128+7
	; OUT		(&h99),A
	; LD		A,17+128
	; OUT		(&h99),A		;Setup border color write

C82Loop
	; Timeout counter
	LD		A,&h0A			; Count ~5.66 seconds
	LD		(TEMPCNT2),A	; H
	XOR		A				; 0
	LD		(TEMPCNT1),A	; L

.c820
	PUSH	BC
	PUSH	HL
	LD		A,0
	LD		HL,PRBUFFERCNT
	CP		(HL)
	JR		Z,.c820a
;	CALL	.gb0			; There's still bytes in the buffer
	DEC		(HL)				;A
	LD		A,(PRINDEXOUT)
	LD		E,A
	INC		A
	LD		(PRINDEXOUT),A		; Update pointers
	LD		D,HIGH(PrBuffer)
    LD		A,(DE)				; Get character

	POP 	HL
	POP 	BC
	JR		.c821
.c820a
	CALL	ReadByte		; Receive a character from RS232
	POP		HL
	POP		BC
	JR		C,.c821			; Byte received -> .c821
	LD		A,(TEMPCNT1)	; Decrement counter (L)
	DEC		A
	LD		(TEMPCNT1),A
	CP		0
	JR		NZ,.c820
	LD		A,(TEMPCNT2)	; Decrement counter (H)
	DEC		A
	LD		(TEMPCNT2),A
	CP		0
	JR		Z,C82End
	JR		.c820

.c821
;	LD		A,(RXBYTE)			; Store it in RAM
C82Addr
	LD		(HL),A
	LD		A,L
	OUT		(&h99),A
	LD		A,&h80+7
	OUT		(&h99),A
C82FX
	INC		HL
C82Next
	LD		A,&hFF
	DEC		BC
	CP		C
	JR		NZ,C82Loop
	CP		B
	JR		Z,C82End	; Count wrapped to $FFFF
C82Cont
	JR		C82Loop
C82End
	; TODO:
	;	-Check destination address and screen mode and copy data
	;	 to VRAM if needed
	LD		A,LONGRX
	LD		(.rts0+1),A		; Set longer Rx timing

	LD		HL,EDSTAT
.c822		; Save any remaining received bytes into the buffer
	SET		7,(HL)		; Set receive flag
	CALL	ReadByte
	JR		NC,.c823	; if no character received, exit
	CALL	AddToPrBuffer
	JP 		.c822
.c823
	LD		A,(BORDER)
	LD		D,A	; Set Border color back
	LD		E,7
	CALL	WriteVReg
	EI					; Enable IRQs
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 131: PCM audio streaming until receiving a NUL ($00) character

Cmd83
	DI
	LD		A,0
	LD		HL,PRBUFFERCNT		; Reset Buffer
	LD		(HL),A
	INC 	HL
	LD		(HL),A
	INC 	HL
	LD		(HL),A
	CALL	TurboRX
	EI
	LD		A,(BORDER)
	LD		D,A
	LD		E,7
	CALL	WriteVReg			; Restore border color
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 144: Returns to the default text mode, requires 3 parameter bytes
;      Page (not used), border color, background color

Cmd90
	LD		A,(EDSTAT)
	AND		&hCF
	LD		(EDSTAT),A		; Set screen mode
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	;Page - Discarded
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	;Border
	LD		(BORDER),A
	LD		D,A
	LD		E,7
	CALL	WriteVReg
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	;Background - Just change current attribute for now
	AND 	&h0F			; Only BG color
.c90_1
	LD		B,A
	LD		A,(SCOLORS)		; Current colors
	AND		&hF0			; Remove BG color
	OR		B
	LD		(SCOLORS),A
	LD		(ACOLORS),A
	LD		HL,EDSTAT
	BIT		1,(HL)		; RVS On?
	JP		NZ,.ct19_0	; Yes, swap nibbles
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 145: Switch to bitmap hires mode, requires 2 parameter bytes
;      Page (not used), border color

Cmd91
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer
	LD		(BORDER),A
	LD		D,A
	LD		E,7
	CALL	WriteVReg

	; Copy bitmap to VRAM
	LD		HL,&h5800
	LD		DE,(GRPNAM)
	LD 		BC,&h0300
	CALL	CpyVRAM				; Copy NameTable

	LD		HL,&h4000
	LD		DE,(GRPCGP)
	LD 		BC,&h1800
	CALL	CpyVRAM				; Copy PatternTable

	LD		HL,&h5B00
	LD		DE,(GRPCOL)
	LD 		BC,&h1800
	CALL	CpyVRAM				; Copy ColorTable
	RET


;///////////////////////////////////////////////////////////////////////////////////
; 160: Selects the screen as the output for the received characters
CmdA0
	LD		A,(FLAGS1)
	AND		%10111111		; Switch to screen mode, Setting FLAGS1 bit 6 to 0
	LD		(FLAGS1),A		; and exits command mode
	JP		CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 161: Selects the voice synthesizer as output for the received characters
CmdA1
	LD		A,(FLAGS1)
	OR		%01000000		; Switch to voice mode, Setting FLAGS1 bit 6 to 1
	LD		(FLAGS1),A		; and exits command mode
	JP		CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 162: Requests the terminal's ID and version
CmdA2
	DI
	LD		HL,IDString
	LD		B,24
.a2_0
	LD		A,(HL)
	CALL	SendID
	INC		HL
	DJNZ	.a2_0
	EI
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 163: Queries terminal if a command is implemented, requires 1 parameter: Command #
;	   Returns number of parameters if command exist, or bit-7 = 1 if not.

CmdA3
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer / Command #
	DI
	BIT		7,A
	JR		Z,.a3_0			; If it's not a command, replace with unimplemented command ($8F)
	CP 		MAXCMD+1
	JR		C,.a3_1			; Is it less than or equal to the highest implemented command?
.a3_0
	LD		A,&h8F			; Invalid command, replace with unimplemented command ($8F)
.a3_1
	AND		%01111111		; -128
	LD		E,A
	LD		D,0
	LD		HL,CmdParTable
	ADD		HL,DE
	LD		A,(HL)			; Get parameter count/Command implemented
	CALL	SendID
	EI
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 176: Sets cursor position, requires two parameter bytes: Column, row
; 	   Relative to the current text window

CmdB0
	CALL 	ClrCrsr
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer / Column
	CP 		31				; Greater than 31?
	JR		C,.b0_0			; No, set it, get row
	LD		A,31			; Yes, force 31
.b0_0
	LD		(CSRX),A
	LD		A,(WBOTTOM)
	DEC		A
	LD		B,A
	LD		A,(WTOP)
	LD		C,A
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer / Row
	ADD		A,C
	CP		B				; Greater than WBOTTOM-1?
	JR		C,.b0_1			; No, continue
	LD		A,B			; Yes, force WBOTTOM-1
.b0_1
	LD		(CSRY),A
; 	CLC
; 	ADC WTOP
; 	TAX
; 	CPX WBOTTOM			; Greater than WBOTTOM?
; 	BCC +				; No, continue
; 	LDX WBOTTOM			; Yes, force WBOTTOM
; +	PLA
; 	TAY					;LDY TEMP1
; 	CLC
	; JSR $D839			; Set cursor position
	; JSR StartCRSR
	; LDA WTOP			; Restore OS text window limits
	; STA $07E6
	; LDX WBOTTOM
	; DEX
	; STX $07E5
	; JSR $DE80			; Rebuild link table
	CALL	.cup			; Update cursor pointers
	JP		CmdFE			; Exit command mode

;///////////////////////////////////////////////////////////////////////////////////
; 177:	Fill a row with the selected character, cursor is not moved
;		Requires 2 parameter bytes: Row, character code
;		(extended graphics mapped to 00-1F)

CmdB1
 	CALL	ClrCrsr
	LD		HL,FLAGS1
	LD		A,(HL)
	PUSH	AF
	RES		3,(HL)
	CALL	GetFromPrBuffer	; Reads a byte from the print buffer / Row
	CP 		23				; Greater than 23?
	JR		C,.b1_0			; No, continue
	LD		A,23			; Yes, force 23
.b1_0
	LD		B,A				; Save row
	CALL	GetFromPrBuffer ; Reads a byte from the print buffer / character code
	LD		C,A				; C = Char
	LD		A,(ACOLORS)
	CALL	.b1_1
	CALL	.cup			; Update cursor
	POP		AF
	LD		(FLAGS1),A
	RET

.b1_1	; Fill line -A = Color - B = ROW - C = Char
	LD		(FCOLORS),A
	LD		HL, RowTbl
	SLA		B
	LD		A,B
	ADD		A,L
	LD		L,A
	LD		E,(HL)
	INC		HL
	LD		D,(HL)			; DE: char offset
	EX		DE,HL
	LD		(DOFFS),HL		; HL: offset
	; Update ScreenCodes and ColorRAM first
	LD		B,32			; Character count
	LD		D,H
	LD		E,L				; Copy offset to DE
	LD		A,HIGH(ScreenCodes0)
	ADD 	A,H
	LD		H,A
	LD		A,HIGH(ColorRAM0)
	ADD		A,D
	LD		D,A
.b1_2
	LD		A,C
	LD		(HL),A			; Update ScreenCodes
	LD		A,(FCOLORS)
	LD		(DE),A			; Update ColorRAM
	INC		HL
	INC 	DE
	DJNZ	.b1_2			; loop
	; Update patterns
	LD		B,32			; Character count
	LD		HL,(DOFFS)
	LD		D,H
	LD		E,L
	LD		A,HIGH(VideoMatrix0)
	ADD		A,H
	LD		H,A
.b1_3
	PUSH	DE
	LD		E,(HL)			; DE Pattern
	PUSH	BC
	PUSH	HL
	LD		A,(FCOLORS)
	LD		B,A
	CALL	CpyPATT
	POP		HL
	POP		BC
	POP		DE
	INC		HL
	DJNZ	.b1_3			; loop
	RET

FCOLORS	DB	0

;///////////////////////////////////////////////////////////////////////////////////
; 178: Set the cursor enable status, requires a single parameter byte

CmdB2

	CALL	GetFromPrBuffer
	LD		HL,FLAGS1
	CP		0				; disable?
	JR		Z, .b2_0		; yes, branch
	SET		3,(HL)
	RET
.b2_0
	BIT		3,(HL)
	JR		Z,.b2_1			; if already disabled, do nothing
	RES 	3,(HL)
	CALL	ClrCrsr
.b2_1
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 179: Split screen, requires 2 parameters, mode/row and background colors
CmdB3
	CALL	ClrCrsr
	CALL	GetFromPrBuffer
	CP		0
	JR		Z,.b3cancel
	CP		24
	JR		C,.b3_0
	LD		A,23
.b3_0
	;LD		(WTOP),A
	CALL	setwtop				; Set window top
	LD		A,23
	CALL	.b5_1				; Set window bottom
	CALL	GetFromPrBuffer
	SRL		A
	SRL		A
	SRL		A
	SRL		A
	CALL	.c90_1
	;JP		.chome0				; Home cursor
	LD		A,(EDSTAT)
	AND		&hCF
	OR		&h20
	LD		(EDSTAT),A			; Set split screen mode
	; TODO: Copy bitmap segment to VRAM
	CALL	ClrScr				; Clear entire screen

	LD		HL,&h5800
	LD		DE,(GRPNAM)
	LD 		BC,(STROFF)
	CALL	CpyVRAM				; Copy Top section NameTable

	LD		BC,(STROFF)
	SLA		C
	RL		B
	SLA		C
	RL		B
	SLA		C
	RL		B
	PUSH 	BC					; STROFF*8
	LD		HL,&h4000
	LD		DE,(GRPCGP)
	CALL	CpyVRAM				; Copy Top section PatternTable

	POP		BC
	LD		HL,&h5B00
	LD		DE,(GRPCOL)
	CALL	CpyVRAM				; Copy Top section ColorTable
	RET
.b3cancel
	;LD		(WTOP),A			; WTOP = 0
	CALL	setwtop
	LD		A,23
	CALL	.b5_1				; WBOTTOM = 23
	LD		A,(EDSTAT)
	AND		&hCF
	LD		(EDSTAT),A			; Return to text mode
	CALL	GetFromPrBuffer		; Discard next parameter
	JP		.chome0				; Home cursor
	; RET

;///////////////////////////////////////////////////////////////////////////////////
; 180: Get cursor position, returns 2 bytes, column and row. Exit CMD mode

CmdB4
	DI
	LD		A,(CSRX)
	CALL	SendID
	LD		A,(WTOP)
	LD		B,A
	LD		A,(CSRY)
	SUB		B
	CALL	SendID
	EI
	JP		CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 181: Set text window, requires 2 parameters, top and bottom rows

CmdB5
	CALL	ClrCrsr
	CALL	GetFromPrBuffer
	CP		24
	JR		C,.b5_0
	LD		A,23
.b5_0
	CALL	setwtop

	; LD		(WTOP),A
	; LD		(CSRY),A
	; ; Set top row offset for scroll routine
	; LD		HL,RowTbl
	; SLA		A
	; ADD		A,L
	; LD		L,A
	; LD		E,(HL)
	; INC		HL
	; LD		D,(HL)			; DE = Top row offset
	; LD		(STROFF),DE

	CALL	GetFromPrBuffer
	CP		24
	JR		C,.b5_1
	LD		A,23
.b5_1
	; Set bottom row offset for scroll routine
	LD		B,A				;Save A
	LD		HL,RowTbl
	SLA		A
	ADD		A,L
	LD		L,A
	LD		E,(HL)
	INC		HL
	LD		D,(HL)			; DE = Bottom row offset
	; LD		HL,31
	; ADD		HL,DE
	; LD		(SBROFF),HL		; DE + 31
	LD		(SBROFF),DE

	LD		A,B
	INC		A
	LD		(WBOTTOM),A
	;LD		A,0
	XOR		A				; Zero carry
	LD		(CSRX),A

	;LD		HL,&h20
	;ADD		HL,DE			; HL = Bottom row offset + 32
	EX		DE,HL
	;SBC		HL,DE
	LD		DE,(STROFF)
	SBC		HL,DE
	LD		DE,&h20
	ADD		HL,DE
	LD		(SCOUNT),HL		; Number of characters to scroll
	
	;LD		A,FLAGS1
	; BIT		3,A				; Check if cursor is enabled
	; JR		Z,.b5_2			; No, just update cursor position
.b5_2
	JP		.cup

setwtop
	LD		(WTOP),A
	LD		(CSRY),A
	; Set top row offset for scroll routine
	LD		HL,RowTbl
	SLA		A
	ADD		A,L
	LD		L,A
	LD		E,(HL)
	INC		HL
	LD		D,(HL)			; DE = Top row offset
	LD		(STROFF),DE
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 182: Scroll text window, requires 1 parameter: rows to scroll, signed byte
CmdB6
	CALL	ClrCrsr
	CALL	GetFromPrBuffer
	CP		0
	RET		Z
	BIT		7,A
	JR		Z,.b6_1
	;Scroll up
	CPL					; ~A
	INC		A
.b6_0
	CALL	DownScroll
	DEC		A
	CALL	NZ,.b6_0
	JP		.cup
;	RET
.b6_1
	CALL	UpScroll
	DEC		A
	JR		NZ,.b6_1
	RET

;///////////////////////////////////////////////////////////////////////////////////
; 183: Set Ink color, requires 1 parameter: Color index
CmdB7
	CALL	ClrCrsr
	CALL 	GetFromPrBuffer
	CALL	HighCNib
	LD		(SCOLORS),A
	LD		(ACOLORS),A
	LD		HL,EDSTAT		; RVS On?
	BIT		1,(HL)			; RVS On?
	JR		Z,.b7_0
	CALL	.ct19_0			; Yes, swap nibbles
.b7_0
	JP 		CmdFE

;///////////////////////////////////////////////////////////////////////////////////
; 254: Exit command mode, setting FLAGS1 bit 7 to 0
CmdFE
	LD		HL,FLAGS1
	RES		7,(HL)		; Clear bit 7
	RET

;///////////////////////////////////////////////////////////////////////////////////
SendID
	EX		AF,AF'
.si0
	NOP
	NOP
	IN		A,(USARTCmd)
	AND		&h05			; leave only txready and txempty
	CP		&h05			; ready to transmit?
	JR		NZ,.si0			; wait if not
	EX		AF,AF'
	OUT		(USARTData),A
	RET

;///////////////////////////////////////////////////////////////////////////////////
; ReadByte, receive a byte, store in RXBYTE
;---------------------------------------------------------------------------------------

ReadByte
	; LDA	FLAGS1		; Si estamos inicializando la terminal, ignora la recepcion
	; AND	#%00000010
	; BNE	CancelRX
	LD		HL,EDSTAT
	BIT		7,(HL)
	JR		NZ,.rts0	; If receive flag is set then dont pulse RTS
EnRTS
rb1l:
rb1h:
	LD		A,%00100111	; RTS Enabled, Rx/Tx enabled, DTS Active
	OUT		(USARTCmd),A
.rts1
	LD		B,7
WaitRX1		; Wait ~30uS
	DJNZ	WaitRX1
DisRTS
	LD		A,%00000111	; RTS Disabled, Rx/Tx enabled, DTS Active
	OUT		(USARTCmd),A
.rts0
	LD		B,LONGRX			; Wait ~4*char time
WaitRX2
	IN		A,(USARTCmd)	; Read 8251 status
	AND		&h02			; Byte received?
	JR		NZ,Received
	DJNZ	WaitRX2
CancelRX
	XOR		A		; Write 0 to RXBYTE, clear CARRY
	RET
Received
	IN		A,(USARTData)	; Read the received byte
	LD		(RXBYTE),A
	SCF						; Set CARRY (Byte received)
	RES		7,(HL)			; Reset receive flag
	RET

;//////////////////////////////////////////////////////////////////////////////////////////
; TurboRX, receives a byte stream and plays it as nibbles through the PSG volume registers
;------------------------------------------------------------------------------------------

TurboRX
	LD		A,%00100111	; RTS Enabled, Rx/Tx enabled, DTS Active
	OUT		(USARTCmd),A
	; LD		A,128+7
	; OUT		(&h99),A
	; LD		A,17+128
	; OUT		(&h99),A		;Setup border color write
TurboLoop
	LD		D,0
	LD		A,(RXBYTE)
	AND 	&h0F


	OUT		(&h99),A
	LD		E,A
	LD		A,&h80+7
	OUT		(&h99),A

	LD		HL,Sam1
	ADD		HL,DE
	LD		B,(HL)
	LD		A,&h10
	LD		E,A
	ADD		HL,DE
	LD		C,(HL)
	LD		E,A
	ADD		HL,DE
	LD		H,(HL)
	LD		E,C			; B:Voice 1 - E:Voice 2 - H: Voice 3
	LD		C,&hA1
	LD		A,&h08
	; Write first sample
	OUT		(&hA0),A
	INC		A
	OUT		(C),B
	OUT		(&hA0),A
	INC		A
	OUT		(C),E
	OUT		(&hA0),A
	INC		A
	OUT		(C),H
	;
	; Delay HERE 933-301 cycles (632)
	;
	LD		B,&h2E
.tdelay
	DJNZ 	.tdelay
	LD		A,0
	LD		HL,RXBYTE
	RRD
	LD		A,(RXBYTE)

	OUT		(&h99),A
	LD		E,A
	LD		A,&h80+7
	OUT		(&h99),A

	LD		HL,Sam1
	ADD		HL,DE
	LD		B,(HL)
	LD		A,&h10
	LD		E,A
	ADD		HL,DE
	LD		C,(HL)
	LD		E,A
	ADD		HL,DE
	LD		H,(HL)
	LD		E,C			; B:Voice 1 - E:Voice 2 - H: Voice 3
	LD		C,&hA1
	LD		A,&h08
	; Write second sample
	OUT		(&hA0),A
	INC		A
	OUT		(C),B
	OUT		(&hA0),A
	INC		A
	OUT		(C),E
	OUT		(&hA0),A
	INC		A
	OUT		(C),H
	;
	
TRXWait2
	IN		A,(USARTCmd)	; Read 8251 status
	AND		&h02			; Byte received?
	JR		Z,TRXWait2
	IN		A,(USARTData)	; Read received bytes
	OR		A
	JP		Z,TurboExit		; Exit if 0 received

	LD		(RXBYTE),A
	IN		A,(PPI.CR)
	AND		&hF0
	ADD		A,&h07			; 7th row
	OUT		(PPI.CW),A
	IN		A,(PPI.BR)
	AND		&h10			; Check for STOP
	JP		NZ,TurboLoop
	LD		A,&hFF			; Yes, send $FF
	OUT		(USARTData),A
	JP		TurboLoop
TurboExit
	LD		A,%00000111	; RTS Disabled, Rx/Tx enabled, DTS Active
	OUT		(USARTCmd),A
	LD		HL,EDSTAT
.tex1		; Save any remaining received bytes into the buffer
	SET		7,(HL)		; Set receive flag
	CALL	ReadByte
	JR		NC,.tex2	; if no character received, exit
	CALL	AddToPrBuffer
	JR 		.tex1
.tex2
	JP		PSGIni
	; RET



;//////////////////////////////////////7
; Setup F-Keys

Setfkeys:
	LD		B,10
	LD		HL,Fkeys	;Src
.sf1
	LD		DE,(FAddr)	;Dst
.sf0
	LDI
	LD		A,(HL)
	CP		0
	JR		NZ,.sf0
	LD		(DE),A
	INC		HL
	EX		DE,HL
	
	LD		HL,(.sf1+2)
	INC		HL
	INC		HL
	LD		(.sf1+2),HL
	EX		DE,HL
	; LD		A,(.sf1+2)
	; ADD		A,2
	; LD		(.sf1+2),A
	;CP		LOW(.fae)
	;JR		NZ,.sf1
	DJNZ	.sf1
	RET


;///////////////////////////////////////////////////////////////////////////////////
; Subrutina para esperar la pulsacion de una tecla

WaitKey:
	; LD	C,RAWIO		; Lee el teclado directamente
	; LD	DE,&h0FF	; FFh: Retorna un caracter sin imprimirlo
	; CALL	BDOS
	CALL	GetKey
	CP		0			; Si no se presiono ninguna tecla (A=0), vuelve a ReadKeyP3
	JP		Z,WaitKey
	RET					; Retorna

;///////////////////////////////////////////////////////////////////////////////////
; Get key press, .A = 0 if none

GetKey:
	LD	C,RAWIO		; Lee el teclado directamente
	LD	DE,&h0FF	; FFh: Retorna un caracter sin imprimirlo
	CALL	BDOS
	RET				; Retorna


;///////////////////////////////////////////////////////////////////////////////////
; Rutinas de manejo del VDP

;Routine to get the port of data reading from the VDP for MSX-DOS1/2
;Entry: None
;Output: A = CPU port connected to the VDP reading data port #0
 
VDPDR:
	LD	HL,VDP_DR	;HL = Address to read
	LD	A,(EXPTBL)	;A = Main-ROM slot
	CALL	RDSLT
	RET

;Routine to get port of data writing to the VDP for MSX-DOS1/2.
;Entry: None
;Output: A = CPU port connected to the VDP writing data port #0
 
VDPDW:
	LD	HL,VDP_DW	;HL = Address to read
	LD	A,(EXPTBL)	;A = Main-ROM slot
	CALL	RDSLT
	RET

; Rutina para cambiar el puntero en el VDP, para lectura de VRAM
; Se requiere la direccion cargada en DE

SetVPtrR:
	LD	A,(VPORTW)		; C = VPORTW+1 (Puerto de registros del VDP)
	INC	A
	LD	C,A
	DI					; Apunta el VDP a la direccion DE en VRAM
	LD	A,E
	AND	%00111111		; (Bit 6 = 0, Lectura en VRAM)
	LD	B,A
	LD	A,D
	OUT	(C),A
	OUT	(C),B
	EI
	RET

; Rutina para cambiar el puntero en el VDP, para escritura en VRAM
; Se requiere la direccion cargada en DE

SetVPtrW:
	LD	A,(VPORTW)		; C = VPORTW+1 (Puerto de registros del VDP)
	INC	A
	LD	C,A
	DI				; Apunta el VDP a la direccion DE en VRAM (Charset)
	LD	A,D
	AND	%00111111
	OR	%01000000		; (Bit 6 = 1, Escritura en VRAM)
	LD	B,A
	LD	A,E
	OUT	(C),A
	OUT	(C),B
	EI
	RET

; Rutina para escribir un valor a un registro del VDP
; Se requiere el valor en D y el numero de registro en E

WriteVReg:
	LD	A,(VPORTW)		; C = VPORTW+1 (Puerto de registros del VDP)
	INC	A
	LD	C,A
	DI
	LD	A,E			; B = Numero de registro con el bit 7 = 1
	OR	%10000000
	LD	B,A
	LD	A,D			; A = Dato a escribir en el registro
	OUT	(C),A			; Write the data
	OUT	(C),B			; Write the register number (with the bit 7 always set)
	EI
	RET

; Fill VRAM
; A: Value to write
; HL: Starting address
; BC: Size
FillVRAM:
	LD      IX,FILVRM
    LD      IY,(EXPTBL-1)	; get expanded slot status to IYH
    CALL    CALSLT			; perform an inter-slot call
	EI
	RET

; Fill RAM
; A: Value to write
; HL: Starting address
; BC: Size
FillRAM:
	LD 		D,H
	LD 		E,L
	INC		DE
	LD		(HL),A
	DEC		BC
	LDIR
	RET

; Block transfer to VRAM from memory
; BC - Block length
; DE - Start address of VRAM
; HL - Start address of memory
CpyVRAM:
	; LD      IX,LDIRVM
    ; LD      IY,(EXPTBL-1)	; get expanded slot status to IYH
    ; CALL    CALSLT			; perform an inter-slot call
	; EI
	; RET
	PUSH	HL
	PUSH	BC
	CALL	SetVPtrW	; Point the VDP towards the destination
	POP		BC
	POP		DE
.cv0
	LD		A,(DE)
	OUT		(0x98),A	; Hardcoded VDP port
	INC		DE
	DEC 	BC
	LD		A,C
	OR		B
	JR		NZ,.cv0
	RET

; Clear Cursor
ClrCrsr:
	LD		DE,(NTOSET)
	LD		HL,ColorRAM0
	ADD 	HL,DE
	LD		B,(HL)
	; LD	A,(CUATTR)
	; LD	B,A
	LD		A,(CUPATT)
	LD		E,A
	CALL	FillColor
	LD		A,15
	LD		(Bcount),A
	LD		(CFlag),A
	LD		HL,EDSTAT
	SET		3,(HL)
	RET

; Scroll up
UpScroll:
	PUSH	AF

	; Set self modifying code start values
	LD		A,HIGH(ColorRAM1)
	LD		(.us11+1),A
	LD		A,HIGH(ScreenCodes1)
	LD		(.us12+1),A
	LD		B,0
	;LD		A,(WTOP)
	LD		DE,(STROFF)
	LD		A,HIGH(VideoMatrix0)
	LD		(.us10+1),A
	ADD		A,D
	LD		D,A
	LD		(.us04+1),DE
	LD		(.us01+1),DE
	; LD		(.us06+1),DE
	LD		A,(WTOP)
.us04
	LD		HL,VideoMatrix0	; First save upper row of window/section
	LD		DE,ScPat
	LD		C,&h20
	LDIR
	; Here HL contains the address of the next row
.us01
	LD		DE,VideoMatrix0	; DE: Current row | HL: Next row
	LD		C,A
	LD		A,(WBOTTOM)
	DEC		A
	CP		C
	JR		Z,.us06	;.us07	; ->Copy whole video matrix to vram
	LD		A,7
	AND		C
	CP		7				; Last row of VideoMatrix section?
	LD		A,C
	JR		Z,.us06			; -> Copy temp buffer to current row


.us05	; Copy next row to current row
;	LD		DE,VideoMatrix0	; DE: Current row | HL: Next row
	LD		C,&h20
	LDIR
	LD		(.us01+1),DE	; Update self modifying code
	INC	A
	JR		.us01
.us06	; Copy temp buffer to current row
	LD		HL,ScPat
	LD		C,&h20
	LDIR
	LD		(.us04+1),DE		; Update self modifying code
	LD		(.us01+1),DE
	INC		A
	LD		C,A
	LD		A,(WBOTTOM)
	DEC		A
	CP		C
	LD		A,C
	JR		NC,.us04		; NZ
.us07	; Copy updated part of VideoMatrix to VRAM
	LD		DE,VideoMatrix0
	LD		HL,(GRPNAM)
	LD		BC,(STROFF)
	ADD		HL,BC
	EX		DE,HL
	ADD		HL,BC
	;LD		DE,(GRPNAM)
	LD		BC,(SCOUNT)				;&h0300
	CALL	CpyVRAM

	; Before scrolling Screencodes and ColorRAM
	; render the lines that crossed section boundaries
	LD		B,7				; Current row = 7
	LD		IXL,0
.us08
	PUSH	BC				; Save Current row
	LD		A,(WTOP)
	CP		B
	JR		C,.us08a		; WTOP < 7
	JR		Z,.us08a		; WTOP = 7
	JR		NC,.us12a		; WTOP > 7 >>
.us08a
	LD		A,(WBOTTOM)
	DEC		A
	CP		B
	JR		C,.us13			; Yes: WBOTTOM-1 < Current row
	JR		Z,.us13			;		" 	"	 =  "		"

	LD		A,0
.us09
	; A: column
	PUSH	AF
	LD		C,A	; Save A
.us10
	LD		H,HIGH(VideoMatrix0)
	ADD		A,&hE0
	LD		L,A
	LD		A,(HL)
.us10a
	LD		D,IXL
	LD		E,A					; DE = Destination Pattern
.us11
	LD		H,HIGH(ColorRAM1)
	LD		L,C
	LD		A,(HL)
	LD		B,A					; B = Color
.us12
	LD		H,HIGH(ScreenCodes1)
	LD		L,C
	LD		A,(HL)
	LD		C,A					; C = Character to copy
	CALL 	CpyPATT
	POP		AF
	INC		A
	CP		32
	JP		NZ,.us09
.us12a
	LD		HL,.us10+1			;Update pointers
	INC		(HL)
	LD		HL,.us11+1
	INC		(HL)
	LD		HL,.us12+1
	INC		(HL)
	; LD		D,IXL
	INC 	IXl
	; LD		IXL,D
	POP		AF
	ADD		A,8
	LD		B,A
	JR		.us08
.us13	; Scroll Screencodes
	POP		BC
	LD		BC,(SCOUNT)
	LD		DE,(STROFF)
	LD		A,HIGH(ScreenCodes0)
	ADD		A,D
	LD		D,A
	LD		HL,&h0020
	ADD		HL,DE
	LDIR
; Scroll ColorRAM
	LD		BC,(SCOUNT)
	LD		DE,(STROFF)
	LD		A,HIGH(ColorRAM0)
	ADD		A,D
	LD		D,A
	LD		HL,&h0020
	ADD		HL,DE
	LDIR	
; Render WBOTTOM-1 with spaces
	LD		A,(WBOTTOM)
	DEC		A
	LD		B,A
	LD		C,32
	LD		A,(SCOLORS)
	CALL	.b1_1			; Line Fill

	POP		AF
	RET

STROFF	DW	&h0000		;WTOP row offset
SBROFF	DW	&h02E0		;WBOTTOM row offset
SCOUNT	DW	&h0300		;

; Scroll down
DownScroll:
	PUSH	AF

	; Set self modifying code start values
	LD		A,HIGH(ColorRAM0)
	LD		(.ds11+1),A
	LD		A,HIGH(ScreenCodes0)
	LD		(.ds12+1),A
	LD		B,0
	;LD		A,(WBOTTOM)
	LD		DE,(SBROFF)
	LD		HL,31
	ADD		HL,DE
	EX		DE,HL
	LD		A,HIGH(VideoMatrix1)
	LD		(.ds10+1),A
	DEC		A					; >> A = HIGH(VideoMAtrix0)
	ADD		A,D
	LD		D,A
	LD		(.ds04+1),DE
	LD		(.ds01+1),DE
	LD		A,(WBOTTOM)
	DEC		A
.ds04
	LD		HL,VideoMatrix0	; First save lower row of window/section
	LD		DE,ScPat+31
	LD		C,&h20
	LDDR
	; Here HL contains the address of the previous row + 31
.ds01
	LD		DE,VideoMatrix0	; DE: Current row + 31 | HL: Previous row +31
	LD		C,A
	LD		A,(WTOP)
	CP		C
	JR		Z,.ds06	;.us07	; ->Copy updated video matrix to vram
	LD		A,7
	AND		C
	CP		0				; First row of VideoMatrix section?
	LD		A,C
	JR		Z,.ds06			; -> Copy temp buffer to current row


.ds05	; Copy next row to current row
	; LD		DE,VideoMatrix0	; DE: Current row + 31 | HL: Previous row +31
	LD		C,&h20
	LDDR
	LD		(.ds01+1),DE	; Update self modifying code
	DEC	A
	JR		.ds01
.ds06	; Copy temp buffer to current row
	LD		HL,ScPat+31
	LD		C,&h20
	LDDR
	LD		(.ds04+1),DE		; Update self modifying code
	LD		(.ds01+1),DE
	DEC		A
	LD		C,A
	LD		A,(WTOP)
	CP		C
	LD		A,C
	JR		C,.ds04		; NZ <<<  if WTOP < current row <<<
.ds07	; Copy updated part of VideoMatrix to VRAM
	LD		DE,VideoMatrix0
	LD		HL,(GRPNAM)
	LD		BC,(STROFF)
	ADD		HL,BC
	EX		DE,HL
	ADD		HL,BC
	;LD		DE,(GRPNAM)
	LD		BC,(SCOUNT)				;&h0300
	CALL	CpyVRAM

	; Before scrolling Screencodes and ColorRAM
	; render the lines that crossed section boundaries
	LD		B,8				; Current row = 8
	LD		IXL,1
.ds08
	PUSH	BC				; Save Current row
	LD		A,(WTOP)
	CP		B
	JR		NC,.ds12a		; WTOP >= 8
	LD		A,(WBOTTOM)
	DEC		A
	CP		B
	JR		C,.ds13			; Yes: WBOTTOM-1 < Current row
	JR		Z,.ds13			;		" 	"	 =  "		" <<<???

	LD		A,&h00			; A = Column 0
.ds09
	; A: column
	PUSH	AF
.ds10
	LD		H,HIGH(VideoMatrix1)
	;ADD		A,&h00
	LD		L,A
	ADD		A,&hE0
	LD		C,A
	LD		A,(HL)
.ds10a
	LD		D,IXL
	LD		E,A					; DE = Destination Pattern
.ds11
	LD		H,HIGH(ColorRAM0)
	LD		L,C
	LD		A,(HL)
	LD		B,A					; B = Color
.ds12
	LD		H,HIGH(ScreenCodes0)
	LD		L,C
	LD		A,(HL)
	LD		C,A					; C = Character to copy
	CALL 	CpyPATT
	POP		AF
	INC		A
	CP		&h20
	JP		NZ,.ds09
.ds12a
	LD		HL,.ds10+1			;Update pointers
	INC		(HL)
	LD		HL,.ds11+1
	INC		(HL)
	LD		HL,.ds12+1
	INC		(HL)
	;LD		D,IXL
	INC 	IXl	;D
	;LD		IXL,D
	POP		AF
	ADD		A,8
	LD		B,A
	JR		.ds08
.ds13	; Scroll Screencodes
	POP		BC
	LD		DE,(SBROFF)
	LD		A,HIGH(ScreenCodes0)
	ADD		A,D
	LD		D,A
	LD		H,D
	LD		L,E
	LD		BC,&h0020
	ADD		HL,BC
	EX		DE,HL
	XOR		A
	;SBC	HL,BC
	LD		BC,(SCOUNT)
	LDDR
; Scroll ColorRAM
	LD		HL,&h0020
	LD		DE,(SBROFF)
	LD		A,HIGH(ColorRAM0)
	ADD		A,D
	LD		D,A
	LD		H,D
	LD		L,E
	LD		BC,&h0020
	ADD		HL,BC
	EX		DE,HL
	XOR		A
	;SBC		HL,BC
	LD		BC,(SCOUNT)
	LDDR	
; Render WTOP with spaces
	LD		A,(WTOP)
	LD		B,A
	LD		C,32
	LD		A,(SCOLORS)
	CALL	.b1_1			; Line Fill

	POP		AF
	RET



; Delete/Backspace
; A: Column to delete
Delete:
	; 1st get offset of character to delete
	; And replace the pattern with a space
	LD		B,A
	EX		AF,AF'		;Save column
	LD		A,31
	SUB		B
	LD		(DCOUNT),A	; Number of characters to move
	LD		IX, RowTbl
	LD		A,(CSRY)
	SLA		A
	ADD		A,IXl
	LD		IXL,A
	LD		A,(IX)
	LD		H,(IX+1)
	ADD		A,B
	LD		L,A			; HL = char offset
	LD		(DOFFS),HL
	LD		D,H
	LD		A,HIGH(VideoMatrix0)
	ADD		A,H
	LD		H,A
	LD		E,(HL)
	LD		A,E			; DE = Pattern to clear
	LD		(DPATT),A	; Save A: Pattern to move at the end of the row
	LD		A,(SCOLORS)
	LD		B,A			; Color
	LD		C,32		; -Space-
	CALL	CpyPATT		; Clear pattern
	;Move patterns in VideoMatrix
	EX		AF,AF'		; Now A: Column A': ---
	CP		31			; Deleting column 31?
	JR		Z,.del0		; Yes, no need to move data
	LD		HL,(DOFFS)
	LD		A,HIGH(VideoMatrix0)
	ADD		A,H
	LD		H,A
	LD		D,A
	LD		E,L			; DE; VideoMatrix offset
	INC		L			; HL: VideoMatrix offset + 1
	LD		A,(DCOUNT)
	LD		C,A
	LD 		B,0
	LDIR				; Move row 1 character to the left
	LD		A,(DPATT)
	DEC		HL
	LD		(HL),A		; Move deleted pattern to the end of the row
	;Move patterns in VRAM
	LD		HL,(DOFFS)
	LD		D,H
	LD		A,HIGH(VideoMatrix0)
	ADD		A,H
	LD		H,A			; HL: VideoMatrix offset
	LD		A,(GRPNAM+1)
	ADD		A,D
	LD		D,A
	LD		E,L			; DE: Nametable offset
	LD		A,(DCOUNT)
	LD		C,A
	LD		B,0
	INC		C
	CALL	CpyVRAM
	; Update ScreenCodes
.del0
	LD		HL,(DOFFS)
	LD		A,HIGH(ScreenCodes0)
	ADD		A,H
	LD		H,A
	LD		D,A
	LD		E,L			; DE; ScreenCodes offset
	INC		L			; HL: ScreenCodes offset + 1
	LD		A,(DCOUNT)
	CP		0
	JR		Z,.del1
	LD		C,A
	LD 		B,0
	LDIR				; Move line 1 character to the left
	DEC		HL
.del1
	LD		(HL),32		; Fill last character in the row with a space
	; Update ColorRAM
	LD		HL,(DOFFS)
	LD		A,HIGH(ColorRAM0)
	ADD		A,H
	LD		H,A
	LD		D,A
	LD		E,L			; DE; ColorRAM offset
	INC		L			; HL: ColorRAM offset + 1
	LD		A,(DCOUNT)
	CP		0
	JR		Z,.del2
	LD		C,A
	LD 		B,0
	LDIR				; Move line 1 character to the left
	DEC		HL
.del2
	LD		A,(SCOLORS)
	LD		(HL),A		; Change last character color
;...
	RET

DOFFS:
	DW	&h0000	; Delete character offset
DPATT:
	DB	&h00	; Delete character pattern
DCOUNT:
	DB	&h00	; Delete, number of characters to move
IOFFS:
	DB	&h00	; Lower byte - End of row for Insert 

; Insert
; A: Column to insert
Insert:
	; 1st check if the last character in the row is a space
	LD		B,A
	LD		A,31
	SUB		B
	LD		(DCOUNT),A	; Number of characters to move
	EX		AF,AF'		;Save column
	;LD		A,(CSRY)
	LD		IX, RowTbl
	LD		A,(CSRY)
	SLA		A
	ADD		A,IXl
	LD		IXL,A
	LD		A,(IX)
	LD		E,A
	LD		H,(IX+1)
	ADD		A,B
	LD		L,A			; HL = char offset
	LD		(DOFFS),HL
	LD		A,31
	ADD		A,E
	LD		(IOFFS),A
	LD		L,A
	LD		A,HIGH(ScreenCodes0)
	ADD		A,H
	LD		H,A
	LD		A,(HL)		; A = Char at column 31
	CP		32			; Space?
	RET		NZ			; if not -> do nothing, return
	; get offset of character to insert
	; And replace the pattern color (it already is a space)
	LD		A,(DOFFS+1)
	LD		D,A
	ADD		A,HIGH(VideoMatrix0)
	LD		H,A
	LD		E,(HL)
	LD		A,E			; DE = Pattern to clear
	LD		(DPATT),A	; Save A: Pattern to move at the insertion point
	LD		A,(SCOLORS)
	LD		B,A			; Color
	CALL	FillColor	; Recolor pattern
	;Move patterns in VideoMatrix
	LD		HL,(DOFFS)
	LD		A,(IOFFS)
	LD		L,A
	LD		A,HIGH(VideoMatrix0)
	ADD		A,H
	LD		H,A
	LD		D,A
	LD		E,L			; DE; VideoMatrix offset
	DEC		L			; HL: VideoMatrix offset - 1
	LD		A,(DCOUNT)
	LD		C,A
	LD 		B,0
	LDDR				; Move row 1 character to the right
	INC		HL
	LD		A,(DPATT)
	LD		(HL),A		; Move last pattern to insertion point
	;Move patterns in VRAM
	LD		HL,(DOFFS)
	LD		D,H
	LD		A,HIGH(VideoMatrix0)
	ADD		A,H
	LD		H,A			; HL: VideoMatrix offset
	LD		A,(GRPNAM+1)
	ADD		A,D
	LD		D,A
	LD		E,L			; DE: Nametable offset
	LD		A,(DCOUNT)
	LD		C,A
	LD		B,0
	INC		C
	CALL	CpyVRAM
	; Update ScreenCodes
	LD		HL,(DOFFS)
	LD		A,(IOFFS)
	LD		L,A
	LD		A,HIGH(ScreenCodes0)
	ADD		A,H
	LD		H,A
	LD		D,A
	LD		E,L			; DE; ScreenCodes offset
	DEC		L			; HL: ScreenCodes offset - 1
	LD		A,(DCOUNT)
	LD		C,A
	LD 		B,0
	LDDR				; Move line 1 character to the right
	INC 	HL
	LD		(HL),32		; Write a space at the insertion point
	; Update ColorRAM
	LD		HL,(DOFFS)
	LD		A,(IOFFS)
	LD		L,A
	LD		A,HIGH(ColorRAM0)
	ADD		A,H
	LD		H,A
	LD		D,A
	LD		E,L			; DE; ColorRAM offset
	DEC		L			; HL: ColorRAM offset - 1
	LD		A,(DCOUNT)
	LD		C,A
	LD 		B,0
	LDDR				; Move line 1 character to the left
	INC 	HL
	LD		A,(SCOLORS)
	LD		(HL),A		; Change color at insertion point
;...
	RET

; DE: Destination pattern
; B: Color
FillColor:
	PUSH	BC
	SLA		E
	RL		D		;x2
	SLA		E
	RL		D		;x4
	SLA		E
	RL		D		;x8 DE: Pattern offset
	JR		.c1

; Copy one character from RAM to VRAM
; C: Character to copy
; B: Color
; DE: Destination pattern
CpyPATT:
	PUSH	BC
	SLA		E
	RL		D		;x2
	SLA		E
	RL		D		;x4
	SLA		E
	RL		D		;x8 DE: Pattern offset
	PUSH	DE
	LD		B,0
	SLA		C
	RL		B		;x2
	SLA		C
	RL		B		;x4
	SLA		C
	RL		B		;x8 BC: Character offset
	PUSH	BC
	LD		HL,(GRPCGP)
	ADD		HL,DE		; Point the VDP towards the Pattern generator
	EX		DE,HL
	CALL	SetVPtrW
	POP		DE			; DE: Character offset
	LD		HL,CHRSET
	ADD		HL,DE
	LD		A,(VPORTW)
    LD      C,A
	LD		B,8		; 8 bytes to copy
	;OTIR
.c0
	OUTI
	NOP
	JR		NZ,.c0

	POP		DE			; DE: Pattern offset
.c1
	LD		HL,(GRPCOL)
	ADD		HL,DE		; Color table address
	EX		DE,HL
	CALL	SetVPtrW
	POP		DE			; D: Color
	LD		A,(VPORTW)
    LD      C,A
	LD		B,8		; 8 bytes to copy
.C2
	OUT		(C),D
	NOP
	DJNZ	.C2
	RET
;

; Init/Clear Screen, both VRAM and RAM tables
ClrScr:
	LD		A,0
	LD		(CSRX),A
	LD		(CSRY),A	; Cursor to home
	; Fill screencode table with spaces
	LD		A,32
	LD		HL,ScreenCodes0
	LD		BC,&h300
	CALL	FillRAM
	; Clear patterns
	XOR		A	; A = 0
	LD		HL,(GRPCGP)
	LD		BC,&h1800
	CALL	FillVRAM
	; Fill color tables with current attributes
	LD		A,(SCOLORS)
	LD		HL,ColorRAM0	;RAM
	LD		BC,&h0300
	CALL	FillRAM
	LD		A,(SCOLORS)		;Use set colors
	LD		HL,(GRPCOL)		;VRAM
	LD		BC,&h1800
	CALL	FillVRAM
	; Reinit name tables
	LD		B,HIGH(VideoMatrix2)
	LD		D,HIGH(VideoMatrix1)
	LD		H,HIGH(VideoMatrix0)
	XOR		A	; A = 0
	LD		C,A
	LD		E,C
	LD		L,E
.cs0
	LD		(BC),A
	LD		(DE),A
	LD		(HL),A
	INC		C
	INC 	E
	INC		L
	INC		A
	JR		NZ,.cs0
	LD		DE,(GRPNAM)
	LD		BC,&h0300
	LD		HL,VideoMatrix0
	CALL	CpyVRAM
	RET
;

; Clear Text window, leave name tables as is
ClrScreen2:
	LD		A,(WBOTTOM)
	LD		HL,WTOP
	LD		C,(HL)		; C = Initial row
	SUB		C				; Check if full screen
	CP		24
	JR		Z,	ClrScr		; Yes, just reinit the screen
	DI
	LD		B,A				; B = Number of rows to clear
	; LD		A,(ACOLORS)
	; EX		AF,AF'			; Save ACOLORS in A'
	LD		A,(SCOLORS)
	LD		D,A
	; LD		(ACOLORS),A
	LD		E,C
	LD		C,32			; Space
.cs20
	PUSH	DE
	PUSH	BC
	LD		B,E				; B = Row to clear
	LD		A,D				; A = Colors
	CALL	.b1_1			; Clear line
	POP		BC				; B = Number of rows
	POP		DE
	INC		E
	DJNZ	.cs20
	LD		A,B
	LD		(CSRX),A
	LD		A,(WTOP)
	LD		(CSRY),A
	;EX		AF,AF'
	;LD		(ACOLORS),A		; Restore ACOLORS	
;	JP		.cup
	EI
	RET

; Output character to screen
; In:
; A: character to print
CharOut:
	LD		HL,EDSTAT
	LD		B,(HL)		; B: Editor status
	CP		&h20		; Control code?
	JR		C,.ctrl		; yes->jump
	CP		&h7F		; DEL?
	JP		Z,.ct7f		; yes->jump
	BIT		0,B			; Test if GRAPH mode
	JR		Z,.co0		; No, skip
	CP		&h60		; Look for half triangles (0x0160 and 0x0161)
	JR		C,.co00		; No, normal extended gfx chars
	ADD		A,&h9e
	JR		.co0
.co00
	SUB		&h40		; Substract GRAPH offset
.co0	; Printable character
	RES		0,(HL)		; Reset GRAPH mode
    LD      C,A             ; C: Character
    LD      A,(ACOLORS)
    LD      B,A             ; B: color

	LD		HL,(NTOSET)		; Cursor Name Table offset
    LD      D,H

	LD		A,HIGH(ColorRAM0)	;Update color table
	ADD		A,H
	LD		H,A
	; LD		A,(CSRX)
	; LD		L,A
	LD		(HL),B

	LD		H,D
	;LD		L,(NTOSET)
	LD		A,HIGH(ScreenCodes0)	;Update RAM Screen
	ADD		A,H
	LD		H,A
	LD		(HL),C

    LD      A,(CUPATT)
    LD      E,A             ; DE: dest Pattern
    CALL    CpyPATT        ; Render character
    JP      .crsrr      ; and move cursor

.ctrl
	; Parse control codes
	BIT		0,B			; GRAPH mode?
	JR		Z,.ct00		; No, skip to standard codes
	RES		0,(HL)		; Reset GRAPH mode
	CP		&h10
	JR		NC,.ctbg	; Greater than $0F, set BG color	
	;Set FG color
	CALL	HighCNib
	; SLA		A
	; SLA		A
	; SLA		A
	; SLA		A
	; LD		B,A
	; LD		A,(SCOLORS)
	; AND		&h0F
	; OR		B
	JR		.ctsc
	;Set BG color
.ctbg
	AND		&h0F
	LD		B,A
	LD		A,(SCOLORS)
	AND		&hF0
	OR		B
.ctsc
	LD		(SCOLORS),A
	LD		(ACOLORS),A
	BIT		1,(HL)		; RVS On?
	JP		NZ,.ct19_0	; Yes, swap nibbles
	CALL	ClrCrsr
	RET

.ct00
	CP		&h00		; NULL
	RET		Z
.ct01
	CP		&h01		; GRAPH?
	JR		NZ,.ct07	; no, skip
	SET		0,(HL)		; Set GRAPH mode
	RET
.ct07
	CP		&h07		; BELL?
	JR		NZ,.ct08		; no, skip
	LD		IX,BEEP
	LD		IY,(EXPTBL-1)
	CALL	CALSLT		; Call BIOS BEEP routine
	EI
	RET
.ct08
	CP		&h08		; Backspace?
	JR		NZ,.ct0b	; no, skip
	;RET		;<<< TODO: Backspace
	LD		A,(CSRX)
	CP		0
	JP		Z,.crsrl
	DEC		A
	CALL	Delete	; Only delete if cursor not in column 0
	JP		.crsrl

.ct0b
	CP		&h0B		; Home?
	JR		NZ,.ct0c	; no, skip
.chome0
	LD		A,0
	LD		(CSRX),A
	LD		A,(WTOP)
	LD		(CSRY),A
	CALL	ClrCrsr
	JP		.cup		; update cursor pointers

.ct0c
	CP		&h0C		; CLS?
	JR		NZ,.ct0d	; no, skip
	CALL	ClrCrsr
	CALL	ClrScreen2
	JP		.cup

.ct0d
	CP		&h0D		; RETURN?
	JR		NZ,.ct12	; no, skip
	;CALL	ClrCrsr
	LD		HL,EDSTAT
	BIT		1,(HL)		; check if RVSON
	JR		Z,.ct0d0
	RES		1,(HL)		; Yes, Disable RVS
	LD		A,(SCOLORS)
	LD		(ACOLORS),A
.ct0d0
	LD		A,0
	LD		(CSRX),A
	JP		.crsrd			; move cursor down
	; LD		A,(CSRY)
	; CP		23
	; JP		Z,.crss		; scroll up (call?)
	; INC		A
	; LD		(CSRY),A
	; JR		.cup		; update cursor pointers

.ct12
	CP		&h12		; INSERT?
	JR		NZ,.ct19
	LD		A,(CSRX)
	CP		31
	RET		Z			; Do nothing if at column 31
	PUSH	AF
	CALL	ClrCrsr
	POP		AF
	CALL	Insert
	JP		.cup

.ct19
	CP		&h19		; RVS-ON? (CTRL-Y)
	JR		NZ,.ct1a
	BIT		1,(HL)		; Already On?
	RET		NZ			; Yes, return
	SET		1,(HL)
.ct19_0
	CALL	ClrCrsr
	LD		HL,ACOLORS
	LD		A,(HL)
	RRD					; Swap nibbles (BG<->FG) in (HL)
	RET
.ct1a
	CP		&h1A		; RVS-OFF (CTRL-Z)
	JR		NZ,.ct1c
	BIT		1,(HL)		; Already Off?
	RET		Z			; Yes, return
	RES		1,(HL)
	JR		.ct19_0
.ct1c
    CP      &h1C		; CRSR RIGHT
    JR      NZ,.ct1d
.crsrr ;Cursor right
	CALL	ClrCrsr
    LD      A,(CSRX)
    CP      31
    JR      Z,.cr0
    INC     A
    LD      (CSRX),A
    JR      .cup        ; update cursor pointers
.cr0
    LD      A,0
    LD      (CSRX),A
	JR		.crsrd
    ; LD      A,(CSRY)
    ; CP      23
    ; JP      Z,.crss     ; scroll up (call?)
    ; INC     A
    ; LD      (CSRY),A
    ; JR      .cup		; update cursor pointers
.ct1d
	CP		&h1D		;CRSR LEFT
	JR		NZ,.ct1e
.crsrl	;Cursor left
	CALL	ClrCrsr
	LD		A,(CSRX)
	CP		0
	JR		Z,.cl0
	DEC		A
	LD		(CSRX),A
	CALL	ClrCrsr
	JR 		.cup		; update cursor pointers
.cl0
	LD		B,A
	LD		A,(CSRY)
	CP		B
	RET		Z			; do nothing if we're at the home position
	LD		A,(CSRY)
	DEC		A
	LD		(CSRY),A
	LD		A,31
	LD		(CSRX),A
	JR		.cup		; update cursor pointers

.ct1e
	CP		&h1E		;CRSR UP
	JR		NZ,.ct1f
	CALL	ClrCrsr
	LD		A,(WTOP)
	LD		B,A
	LD		A,(CSRY)
	CP		B
	RET 	Z			; do nothing if we're at home row
	DEC		A
	LD		(CSRY),A
	JR		.cup		; update cursor pointers

.ct1f
	CP		&h1F		;CRSR DOWN
	JR		NZ,.coe
.crsrd
	CALL	ClrCrsr
	LD		A,(WBOTTOM)
	LD		B,A
	LD		A,(CSRY)
	INC		A
	CP		B
	JR		NZ,.ct1f1
	CALL	UpScroll	; scroll up (call?)
	JR		.cup
.ct1f1
	;INC		A
	LD		(CSRY),A
	JR		.cup		; update cursor pointers
.ct7f					;DELETE
	LD		A,(CSRX)
	CALL	Delete
	JR		.cup

.cup    ; update cursor pointers
    LD      IX,RowTbl
    LD      A,(CSRY)
    SLA		A
    ADD     A,IXl
    LD      IXl,A
    LD      E,(IX)
    LD      D,(IX+1)
    LD      A,(CSRX)
    ADD     A,E
    LD      E,A
    EX      DE,HL
    LD      (NTOSET),HL
    LD      D,H
    LD      A,HIGH(VideoMatrix0)
    ADD     A,D
    LD      H,A
    LD      A,(HL)
    LD      (CUPATT),A      ; Pattern under cursor
    LD      A,HIGH(ColorRAM0)
    ADD     A,D
    LD      H,A
    LD      A,(HL)          ; A: Pattern attributes
	LD		(CUATTR),A
	RET
.coe
;	RTS DEBUG CODE
; 	CP		&h06		;F4?
; 	JR		NZ,.cF5
; 	LD		HL,.rts1+1
; 	DEC		(HL)
; 	RET
; .cF5
; 	CP		&h0E		;F5
; 	RET		NZ
; 	LD		HL,.rts1+1
; 	INC		(HL)
; ------- END DEBUG CODE
	RET

; ; Set low color nibble
; LowCNib
; 	AND		&h0F
; 	LD		B,A
; 	LD		A,(ACOLORS)
; 	AND		&hF0
; 	OR		B
; 	RET

; Set high color nibble
HighCNib
	SLA		A
	SLA		A
	SLA		A
	SLA		A
	LD		B,A
	LD		A,(SCOLORS)
	AND		&h0F
	OR		B
	RET

; Strings
INTRO:
	DB	"Retroterm MSX v0.10 ALPHA"
	DB	&h0D,&h0A
	DB	"(c)2024 by Retrocomputacion.com"
	DB	&h0D,&h0A
	DB	"Press any key$"

IDString:
	DB "RTRETROTERM-M1 0.10   "	; ID String 22 characters long
	DB &h00,&h07	;Turbo56K version, subversion

; Function keys
Fkeys:
	DB	&h02,&h00
	DB	&h04,&h00
	DB	&h05,&h00
	DB	&h06,&h00
	DB	&h0E,&h00
	DB	&h0F,&h00
	DB	&h10,&h00
	DB	&h11,&h00
	DB	&h13,&h00
	DB	&h14,&h00

; Color test
	; DB	&h01,&h01,&h00
	; DB	&h01,&h02,&h00
	; DB	&h01,&h04,&h00
	; DB	&h01,&h08,&h00
	; DB	&h01,&h0F,&h00
	; DB	&h01,&h11,&h00
	; DB	&h01,&h12,&h00
	; DB	&h01,&h14,&h00
	; DB	&h01,&h18,&h00
	; DB	&h01,&h1F,&h00

FAddr:	;Pointers to the F-keys definitions - Should be safe to reuse after init
	DW	&hF87F,&hF88F,&hF89F,&hF8AF,&hF8BF,&hF8CF,&hF8DF,&hF8EF,&hF8FF,&hF90F
.fae

;Mode 2 text tables

; Align code with the next memory page
IF $ % 256 != 0
    DS 256-($%256),$00
ENDIF

; Receive buffer
PrBuffer:
	DS	256,00

; Virtual screen in ASCII codes
ScreenCodes0:
	DS 	256,32
ScreenCodes1:
	DS 	256,32
ScreenCodes2:
	DS 	256,32

; Color attributes for character
ColorRAM0:
    DS 256,&hF1
ColorRAM1:
    DS 256,&hF1
ColorRAM2:
    DS 256,&hF1

; Name table sincronized with VRAM
VideoMatrix0:
    REPT 256, ?d
    DB ?d
    ENDM
VideoMatrix1:
    REPT 256, ?d
    DB ?d
    ENDM
VideoMatrix2:
    REPT 256, ?d
    DB ?d
    ENDM


; Scroll buffer
ScPat:
	DS 32,0

; PSG Sample Table
Sam1:
	DB	&h00, &h06, &h09, &h0A, &h0B, &h0C, &h0C, &h0D, &h0D, &h0E, &h0E, &h0E, &h0E, &h0E, &h0E, &h0E
Sam2:
	DB	&h00, &h04, &h05, &h07, &h07, &h06, &h09, &h06, &h09, &h04, &h08, &h0A, &h0B, &h0C, &h08, &h0D
Sam3:
	DB	&h00, &h03, &h01, &h01, &h03, &h02, &h03, &h05, &h05, &h02, &h04, &h04, &h05, &h01, &h07, &h04


; Row offsets
RowTbl:
    DW  &h0000,&h0020,&h0040,&h0060,&h0080,&h00A0,&h00C0,&h00E0
    DW  &h0100,&h0120,&h0140,&h0160,&h0180,&h01A0,&h01C0,&h01E0
    DW  &h0200,&h0220,&h0240,&h0260,&h0280,&h02A0,&h02C0,&h02E0

; Commands routine pointer table, unimplemented commands point to CMDOFF
CmdTable:
    ; DW Cmd80,Cmd81,Cmd82,Cmd83,Cmd84,Cmd85,Cmd86,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    ; DW Cmd90,Cmd91,Cmd92,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    ; DW CmdA0,CmdA1,CmdA2,CmdA3,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    ; DW CmdB0,CmdB1,CmdB2,CmdB3,CmdB4,CmdB5,CmdB6,CmdB7
    DW Cmd80,Cmd81,Cmd82,Cmd83,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    DW Cmd90,Cmd91,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    DW CmdA0,CmdA1,CmdA2,CmdA3,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    DW CmdB0,CmdB1,CmdB2,CmdB3,CmdB4,CmdB5,CmdB6,CmdB7

; Command parameter number table.
; bit-7 = 1 : Parameter not implemented
CmdParTable:
	; DB &h02  ,&h01  ,&h02  ,&h00  ,&h00  ,&h00  ,&h00  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80
	; DB &h03  ,&h02  ,&h03  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80
	; DB &h00  ,&h00  ,&h00  ,&h01  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80
	; DB &h02  ,&h02  ,&h01  ,&h02  ,&h00  ,&h02  ,&h01  ,&h01
	DB &h02  ,&h01  ,&h02  ,&h00  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80
	DB &h03  ,&h02  ,&h83  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80
	DB &h00  ,&h00  ,&h00  ,&h01  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80  ,&h80
	DB &h02  ,&h02  ,&h01  ,&h02  ,&h00  ,&h02  ,&h01  ,&h01

; Charset
CHRSET:
	incbin "chrset.bin"	;256 caracteres