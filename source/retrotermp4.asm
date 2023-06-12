;////////////////////////////////////////////////////////////////////////////////////////////
; Micro terminal para Commodore Plus/4
;////////////////////////////////////////////////////////////////////////////////////////////
; Ultima modificacion: 7-Nov-2020
;
; Versiones / correcciones
; ========================
; 07-11-2020	Primera version.
; 26-11-2020	
; 27-11-2020	Segunda version.
;========================================================================================

; Constants

	STROUT = $9088			; BASIC String out routine
	CHROUT = $FFD2			; Kernal CHROUT (prints a single character)
	STOP = $FFE1			; Kernal STOP (checks STOP key)
	GETIN = $FFE4			; Kernal GETIN
	PLOT   = $FFF0			; Kernal PLOT
	COLORMEM = $0800		; Character Colors

	CURRLINEPTR = $C8		; Current text line pointer (zero page)
	CURRLINE = $CD
	CURRCOLUMN = $CA		; Current column (pagina cero)
	CURRCOLLINE = $EA		; Current text line color porinter (zero page)
	ACIABASE = $FD00		; ACIA 6551 base address
	ACIADATA = ACIABASE+0	; Registro de datos del ACIA 6551
	ACIASTATUS = ACIABASE+1	; Registro de estado del ACIA 6551
	ACIACOMMAND = ACIABASE+2; Registro de comando del ACIA 6551
	ACIACONTROL = ACIABASE+3; Registro de control del ACIA 6551
	BEEPTIME = 4		; Cantidad minima de cuadros que dura un beep de impresion
	PrBuffer = ENDOFCODE	; Buffer de impresion

	MAXCMD = $B7			; Highest command implemented


	!to "rt_p4_v0.10.prg", cbm
    !sl "labels-p4.txt"

	*= $1001

;///////////////////////////////////////////////////////////////////////////////////
; PROGRAMA BASIC
;///////////////////////////////////////////////////////////////////////////////////

	!byte $0C, $10, $E4, $07, $9E, $20, $34, $31, $31, $32, $00	; 10 SYS 4112
	!byte $00, $00

	*= $1010

;////////////////////////////////////////////////////////////////////////////////////////////
; Inicio (4110)
;////////////////////////////////////////////////////////////////////////////////////////////

_Start:

	LDY #$08
-	LDA _DATA2,Y
	BEQ +
	STA $0058,Y
+	DEY
	BPL -
	JSR $88C7		; >>Open space in memory

	LDA	#113		; Fondo y borde blancos
	STA	$FF15
	STA	$FF19

	LDA	#<InitScr	; Tinta negra, borra pantalla, bloquea C= + SHIFT, cambia a minusculas/mayusculas
	LDY	#>InitScr
	JSR	STROUT		;PrintTxt

; Copia LogoScr a 3352 ($0D18)
	LDX	#200
-	LDA	LogoScr-1,X
	STA	$0D18-1,X
	LDA	LogoScr+200-1,X
	STA	$0D18+200-1,X
	DEX
	BNE	-
; Copia LogoClr a 2328 ($0918)
	LDX	#200
-	LDA	LogoClr-1,X
	STA	$0918-1,X
	LDA	LogoClr+200-1,X
	STA	$0918+200-1,X
	DEX
	BNE	-

; Imprime la url de retrocomputacion

	CLC			; Coloca el cursor en la fila 20, columna 4
	LDX	#20
	LDY	#4
	JSR	PLOT
	LDA	#<RetroIntro	; Imprime: www.retrocomputacion.com
	LDY	#>RetroIntro
	JSR	STROUT		;PrintTxt
	LDA #$81
	STA $A5
-	LDA $A5
	BNE -



	JMP CODESTART

_DATA2:
!word ENDOFCODE, _ENDOFCODE_
!byte $00, $00, $00
!word _CODESTART_


;///////////////////////////////////////////////////////////////////////////////////
; Logo de retrocomputacion

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
	JMP	MainPrg		; Comienza el programa en MainPrg

	; Variables

TXBYTE			!byte $00		; Byte to be transmitted
RXBYTE			!byte $00		; Last received byte
CMDFlags		!byte $00		; Command flags
								; Bit 7 (N): 1 = Last received byte is CMDON ($ff); 0 = Normal operation
								; Bit 6 (V): 1 = Receive N bytes as parameters and wait for the command to complete; 0 = Normal operation
								; Bits 3-0: Parameter counter for bit-6
TEMPCNT1		!byte $00		; Temporal counter 1
BYTECNT			!word $0000		; 16-bit block transfer remaining bytes to receive counter
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
TEMPCNT2		!byte $00		; Contador temporal 2
BEEPTIMER		!byte $00		; Timer que decrementa en cada interrupcion
TIMERDIV4		!byte $00		; Timer que decrementa en cada interrupcion, usado para realizar acciones cada 4 interrupciones
DELAYTIME		!byte $00		; Parametro (tiempos) de las rutinas de temporizacion
SNDSTATUS		!byte $00		; Character beep enable
SPLITRST		!byte $00		; Split screen raster line

;///////////////////////////////////////////////////////////////////////////////////
; ReadByte, receive a byte, store in RXBYTE
;---------------------------------------------------------------------------------------

ReadByte
	LDA	FLAGS1		; Si estamos inicializando la terminal, ignora la recepcion
	AND	#%00000010
	BNE	CancelRX
EnRTS
	LDA #$1A
	STA $FF02
	LDA #$00
	STA $FF03		; Wait ~30uS
	LDA #%00010000	; Clear Timer 2 IRQ
	STA $FF09

	LDA	#%00001011	; no parity, no echo, no tx irq (rts=0, habilita envio), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
	LDA #$10
WaitRX1
	BIT $FF09
	;AND #$10
	BEQ WaitRX1
	; NOP
	; NOP
	; NOP
	; NOP
	; NOP
	; NOP
DisRTS
	LDA	#%00000011	; no parity, no echo, no tx irq (rts=1, deshabilita envio), no rx irq, rx enabled (dtr=0)
	STA	ACIACOMMAND
	LDA	#$D2		; 2 Coloca 466 ($01D2) microsegundos en el Timer A
	STA	$FF02		; 4
	LDA	#$01		; 2
	STA	$FF03		; 4
	LDA	#%00010000	; 2 Limpia el bit de interrupcion de Timer 2
	STA	$FF09		; 4
WaitRX2
	LDA	ACIASTATUS	; Verifica si se recibio un byte
	AND	#%00001000
	BNE	Received	; 3 Si transcurrio el tiempo, entonces pasaron 85+174 us sin recepcion, sigue en CancelRX
	LDA	$FF09		; 4 Si no transcurrieron los 85 microsegundos del Timer A
	AND	#%00010000	; 2
	BEQ	WaitRX2		; 2 vuelve a WaitRX2 para dar tiempo a que se reciba un byte
CancelRX
	LDA	#$00		; 2 Carga RXBYTE con el valor 0
	STA	RXBYTE		; 4
	CLC			; 2 Hace CARRY = 0 (no hubo recepcion)
	RTS			; 6 Retorna
Received
	LDA	ACIADATA	; 4 Lee en A el valor del byte recibido
	STA	RXBYTE		; y lo almacena en RXBYTE
	SEC			; 2 Hace CARRY = 1 (se recibio un byte)
	RTS			; 6 Retorna

;//////////////////////////////////////////////////////////////////////////////////////////
; TurboRX, receives a byte stream and plays it as nibbles through the SID volume register
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
;	LDA	#$00		; 2 Set Timer A to  75 ($004B) microseconds
;	STA	$DD05		; 4
;DD04_1:
;    LDA #$60
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
	;TAX				; 2	<-18 cycles until here
TRXWait1
; Wait 86 - 16 - 18 cycles for 11520Khz
; Wait 130 - 16 - 18 cycles for 7680KHz
	LDY #$41
-	DEY			; 2
	BNE -		

	TAY

;	LDA	$DD0D		; 4 If Timer A didnt elapse
;	AND	#$01		; 2
;	BEQ	TRXWait1	; 2 go back to TRXWait1 and wiat
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

	SEI			; Deshabilita las interrupciones

	LDA	#$00		; Fondo y borde negros
	STA	$FF15
	STA	$FF19

	LDA	#%00000010	; no parity, no echo, no tx irq (rts=1), no rx irq, rx disabled (dtr=1)
	STA	ACIACOMMAND
	LDA	#%00011111	; 1 stop bit, 8 data bits, baud rate generator, 19200 bps
;	LDA	#%00011000	; 1 stop bit, 8 data bits, baud rate generator, 1200 bps
	STA	ACIACONTROL
	LDA	#$00		; Inicializa el buffer de impresion
	STA	PRINDEXOUT
	STA	PRINDEXIN
	STA	PRBUFFERCNT
	LDA	#%00001010	; Inicializando la terminal
	STA	FLAGS1
	LDA	#32		; Pone un espacio como caracter inicial debajo del cursor
	STA	CRSRCHAR
	LDA	#4		; Inicializa TIMERDIV4
	STA	TIMERDIV4
	LDA	#0		; Pone a 0 los contadores de beeps
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
	LDA	#%00000000	; Volumen = 0 (minimo), desactiva canales 1 y 2 y reinicia los osciladores
	STA	$FF11
	LDA #$03
	STA $FF10		; Voice 2 freq
	LDA #$9a
	STA $FF0F
	LDA	#$10		; Habilita el TED (activa la pantalla)
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
; Procesa el parpadeo del cursor
;///////////////////////////////////////////////////////////////////////////////////

BlinkCRSR
	LDA FLAGS1
	AND #%00001000
	BEQ BlinkEnd
	LDA	CRSRTIMER	; Si no es momento de invertir el cursor (CRSRTIMER<>0), sigue en BlinkEnd
	BNE	BlinkEnd
	LDA	#30		; Si CRSRTIMER llego a 0, lo renovamos
	STA	CRSRTIMER
	LDX CRSRCOLOR
	LDY	CURRCOLUMN
	LDA	#%10000000	; e invertimos el caracter bajo el cursor
	EOR	(CURRLINEPTR), Y
	STA	(CURRLINEPTR), Y
	BPL +
	LDX $053B
+	TXA
	STA ($EA),Y
BlinkEnd
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Deshabilita el cursor, y restaura el caracter debajo del cursor
;///////////////////////////////////////////////////////////////////////////////////

StopCRSR
	LDY	CURRCOLUMN
	LDA	CRSRCHAR
	STA	(CURRLINEPTR), Y
	LDA CRSRCOLOR
	STA ($EA),Y
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Habilita el cursor, y resguarda el caracter debajo del cursor
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
; Rutina de impresion, imprime el buffer completo
;///////////////////////////////////////////////////////////////////////////////////

PrintBuffer
; 	LDA	PRBUFFERCNT	; Si no hay elementos en el buffer para imprimir, sigue en PrintEnd
; 	BEQ	PrintEnd
; 	LDA	TIMER1		; Si no es momento de imprimir (TIMER1<>0), vuelve a PrintBuffer
; 	BNE	PrintBuffer
; 	LDX	PRINDEXOUT
; 	LDA	PrBuffer, X
; 	JSR	CHROUT
; 	JSR Beep
; 	INC	PRINDEXOUT
; 	DEC	PRBUFFERCNT
; 	LDA	PRSPEED		; Renueva TIMER1 con el valor de PRSPEED
; 	STA	TIMER1
; 	JMP	PrintBuffer	; Vuelve a PrintBuffer para seguir imprimiendo el resto del buffer
; PrintEnd
; 	RTS

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
!ifdef _SYNTH_ {
    JSR AddToSpBuffer
}
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
;+	LDA SNDSTATUS
	;BPL +
	;JSR Beep			; Play beep if SNDSTATUS = $80
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
    ;JSR $EA24
    LDA #$20        	; Space
    STA (CURRLINEPTR),Y
    LDA $053B
    STA ($EA),Y
+   RTS
++  CMP #$82			; Check for FLASH ON
	BNE+
	LDA #$80
	STA $53C
+	CMP #$84			; Check for FLASH OFF
	BNE +
	LDA #$00
	STA $53C
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
	;LDA #$14			; Mute sound
	;STA SIDREG+18
	;LDA #$15			; And turn it on again
	;STA SIDREG+18
	RTS	
+   JSR $DCE6       	; Check colors (Kernal)
    RTS

;///////////////////////////////////////////////////////////////////////////////////
; Print beep
;///////////////////////////////////////////////////////////////////////////////////
Beep
	LDA #BEEPTIME
	STA BEEPTIMER	; Reset beep timer
	LDA	#$01		; Si TEMPCNT1 es impar sigue en DoBeep2
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
	LDA	#$01		; Si TEMPCNT1 es impar sigue en DoBeep2
	AND	TEMPCNT1
	LDA	#%00010010	; Volumen = 2 (Low), activa canales 1 y 2 en modo tono
	ORA $FF11
++	STA	$FF11

	INC	TEMPCNT1
	RTS	;JMP	EndBeep

NoChar
	LDA	#%00010000	; Volumen = 8 (maximo), activa canales 1 y 2 en modo tono y reinicia los osciladores
	STA	$FF11
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Ingresa los caracteres de la cadena al buffer de impresion
;///////////////////////////////////////////////////////////////////////////////////

AddText
	STX	AddTLoop + 1
	STY	AddTLoop + 2

	LDX	#$00		; Inicializa X a 0
AddTLoop
	LDA	PrBuffer, X	; Lee un byte de la cadena a imprimir
	BNE	AddTLoop1	; Si es 0, retorna
	RTS
AddTLoop1
;        SEI
	JSR	AddToPrBuffer	; sino llama a AddToPrBuffer para agregarlo al buffer de impresion
;        CLI
	INX			; Pasa al siguiente caracter
	JMP	AddTLoop	; y vuelve a AddTLoop

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
; Espera a que se haya impreso completo el buffer de impresion
;///////////////////////////////////////////////////////////////////////////////////

WaitPrint
	LDA	PRBUFFERCNT	; Espera hasta que PRBUFFERCNT sea 0
	BNE	WaitPrint
	RTS

;///////////////////////////////////////////////////////////////////////////////////
; Vuelve al BASIC
;///////////////////////////////////////////////////////////////////////////////////

ExitPrg
	SEI			; Deshabilita las interrupciones
	LDA	#$0F		; Volumen = 15 (maximo), desactiva canales 1 y 2
	STA	$FF11
	LDA	#32		; Imprime un espacio
	JSR	CHROUT
	LDA	#<ExitMsg	; Muestra mensaje de salida
	LDY	#>ExitMsg
	JSR	STROUT		;PrintTxt
	LDA	#%00000010	; no parity, no echo, no tx irq (rts=1), no rx irq, rx disabled (dtr=1)
	STA	ACIACOMMAND
;	LDA	#%01111111
;	STA	$DC0D		; "Switch off" interrupts signals from CIA-1
;	STA	$DD0D		; "Switch off" interrupts signals from CIA-2
	LDA	#$0E		; Restaura el vector de interrupcion por defecto
	STA	$0314
	LDA	#$CE
	STA	$0315
	LDA	#161		; Set the raster line number where interrupt should occur
	STA	$FF0B
	LDA	#%10100010	; Enable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A
	LDA	#0		; Vacia el buffer del teclado
	STA	$EF
	CLI			; Vuelve a habilitar las interrupciones
	RTS			; Retorna al BASIC

;///////////////////////////////////////////////////////////////////////////////////
; Rutina de interrupcion
;///////////////////////////////////////////////////////////////////////////////////

Irq
	PHA			; store register A in stack
	TXA
	PHA			; store register X in stack
	TYA
	PHA			; store register Y in stack

	;DEC $FF19

;	LDA	#%10000000	; Volumen = 0 (minimo), desactiva canales 1 y 2 y reinicia los osciladores
;	STA	$FF11

;	INC	TEMPCNT2

ChkTimer1
	LDA	TIMER1		; Si TIMER1<>0, hace TIMER1--
	BEQ	ChkTimer2
	DEC	TIMER1
ChkTimer2
	LDA	CRSRTIMER	; Si TIMERCRSR<>0, hace CRSRTIMER--
	BEQ	EndTimers
	DEC	CRSRTIMER
EndTimers

;	BNE	ChkBuffer
;	LDA	FLAGS1		; Indica que ya no tenemos que emitir beeps
;	AND	#%11110111
;	STA	FLAGS1

ChkBuffer
	LDA	PRBUFFERCNT	; Si no hay elementos en el buffer para imprimir, sigue en +
	BNE	ReadBytes
;	LDA	FLAGS1		; Si hay elementos, habilita los beeps de impresion
;	EOR	#%00001000
;	STA	FLAGS1

;+	LDA	#%00001000	; Chequea si hay que procesar los beeps de impresion
;	AND	FLAGS1
;	BEQ	EndBeep
	; LDA	#BEEPTIME	; Si hay beeps pendientes, renueva el temporizador de beep
	; STA	BEEPTIMER

;///////////////////////////////////////////////////////////////////////////////////
; Genera el beep de impresion en pantalla

DoBeep
	LDA	BEEPTIMER	; Si BEEPTIMER no llego a 0, lo decrementa
	BEQ	ReadBytes
	DEC	BEEPTIMER
	BNE	ReadBytes


NoBeep
	LDA	#%00010000	; Volumen = 8 (maximo), activa canales 1 y 2 en modo tono y reinicia los osciladores
	STA	$FF11
	;LDA	#0		; Pone a 0 los contadores de beep
	;STA	TEMPCNT1
	;STA	TEMPCNT2
EndBeep

;///////////////////////////////////////////////////////////////////////////////////
; Lee hasta 3 bytes del puerto RS232

ReadBytes
	LDA	#%10100000	; Disable raster interrupt signals from TED, and clear MSB in TED's raster register
	STA	$FF0A
;	LDA	#%11101111	; Deshabilita la pantalla
;	AND	$FF06
;	STA	$FF06

	LDX	#3		; Lee hasta 3 bytes del RS232
ReadBLoop
	LDA	PRBUFFERCNT	; Si PRBUFFERCNT=255 (buffer lleno) sigue en ReadBEnd
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
	JSR	GETIN		; Leemos el buffer del teclado
	BEQ	ExitIrq		; Si no hay ninguna tecla presionada, sigue en ExitIrq
	STA	TXBYTE		; sino copia el caracter a TXBYTE
	CMP #$03		;JSR	STOP		; Chequea si se presiono la tecla STOP
	BNE	+
	LDA	#%00000001	; Si es STOP, indica que hay que volver al BASIC
	ORA	FLAGS1
	STA	FLAGS1
	LDA	#0		; Pone a 0 la bandera STREY que indica que se presiono STOP
	STA	$91
	JMP	ExitIrq		; y sigue en ExitIrq para salir de la interrupcion
+	;LDA	TXBYTE		; Verifica si la tecla presionada es SHIFT + RETURN
	CMP	#$8D
	BNE	+
	LDA	#$0A		; y si es asi, la convierte en LF
	BNE ++
+	CMP #$A7			; CBM+M
	BNE ++
	LDA #$FF
	EOR SNDSTATUS
	STA SNDSTATUS
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
	LDA $FF12
	AND #$FB		; Bitmap in RAM
	STA $FF12			; Bitmap at $2000
	LDA #$3B		; Bitmap mode
	BNE ++

+	LDA	#$10		; Habilita el TED (activa la pantalla)
	ORA	$FF06
++	STA	$FF06

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

InitPalette:
	LDX #$0F
-	LDA Palette,X
	STA $0113,X
	DEX
	BPL -
	RTS

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
	LDA	$DC0D			; Clear interrupt flags

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
	LDA C82FX
	EOR #$20			; Toggle INC <-> DEC
	STA C82FX
	JMP	C82Loop
C82End
	LDA	BORDERCLR
	STA	$FF19
	LDA	$DC0D			; Clear the interrupt flags ***

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
	LDA #$02
	STA $FF09		; Ack raster interrupts


	LDA	BORDERCLR
	STA	$FF19
	RTS

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
	LDA	#%00011011		; Switch to text mode
	STA	$FF06
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
	LDA #$08			; Attributes at $0800
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
	LDA #$08			; Attributes at $0800
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
	TAY
 	BMI +
 	LDX #$00
 	BEQ ++
+	LDX #$10
++	STX VMODE
 	AND #$1F			; Remove mode bit
 	STA WTOP			; Set text window
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
	RTS
b3cancel				; Cancel split screen
	JSR GetFromPrBuffer
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
	LDA $FF12
	ORA #$04
	
 	LDX #$0F			;2
-	DEX					;2
 	BNE -				;3
	TAX

	; NOP
	; NOP
	LDA #%00011011		;2 Text mode
	STA $FF06			;4
	LDA $FF07			;4
	AND #%11101111		;2
	STA $FF07			;4 Disable multicolor
	STX $FF12
	STY $FF15			;4 Bottom section background color

;	LDA $FF12			; Charset from ROM
;	ORA #$04

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
    !word Cmd80,Cmd81,Cmd82,Cmd83,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word Cmd90,Cmd91,Cmd92,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word CmdA0,CmdA1,CmdA2,CmdA3,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE,CmdFE
    !word CmdB0,CmdB1,CmdB2,CmdB3,CmdB4,CmdB5,CmdB6,CmdB7

; Command parameter number table.
; bit-7 = 1 : Parameter not implemented
CmdParTable:
	; !byte $02  ,$01  ,$02  ,$00  ,$00  ,$00  ,$00  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	; !byte $03  ,$02  ,$03  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	; !byte $00  ,$00  ,$00  ,$01  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	; !byte $02  ,$02  ,$01  ,$02  ,$80  ,$02  ,$01
	!byte $02  ,$01  ,$02  ,$00  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $03  ,$02  ,$04  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $00  ,$00  ,$00  ,$01  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80  ,$80
	!byte $02  ,$02  ,$01  ,$02  ,$00  ,$02  ,$01  ,$01

InitScr
	!byte $90, $93, $08, $0E, $00
RetroIntro
	!text "   rETROCOMPUTACION PRESENTS"
	!byte 13, 13, 29, 29, 29, 29, 29, 29, 29, 29
	!text "    rETROTERM pLUS/4"
	!byte 19, 0
Version
	!byte $05, $93, $99, $08, $0E
;	!text "retroterm VERSION 0.5 turbo    57600,8n1"
;	!text "retroterm turbo232 VER 0.11    57600,8n1"
	!text "retroterm plus/4 VER 0.10      19200,8n1"
	!byte $05, $0D , $00
Msg06
	!text "(c)2023 RETROCOMPUTACION.COM"
	!byte $0D
	!byte $9A
	!text "turbo56k V0.7"
	!byte $0D, $05, $00	
ExitMsg
	!byte $93, $8E
	!text "EXITING TO BASIC..."
	!byte $0D
	!text "SYS28672 TO RESTART RETROTERM"
	!byte $0D, $0D, $00

IDString:
	!text "RTRETROTERM-P4 0.10   "	; ID String 22 characters long
	!byte $00,$07	;Turbo56K version, subversion

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
!if ENDOFCODE > $8EFF {
	!error "ERROR - Part 1 data beyond $8EFF"
}
}
_ENDOFCODE_