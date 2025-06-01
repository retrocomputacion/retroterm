
<div align = center>

![logo](turbo56k.png)

</div>

# Turbo56K v0.7


**Turbo56K** was created by **Jorge Castillo** as a simple protocol to provide high speed file transfer functionality to his bit-banging `57600bps` **RS232** routine for the **Commodore 64**.

Over time, the protocol has been extended to include `4-bit` **PCM** audio streaming, bitmap graphics transfer and display, **SID** and **PSG** music streaming and more.

A typical **Turbo56K** command sequence consists of a command start character ( **CMDON** : `$FF` ) followed by the command itself (a character with it's 7th bit set) and the parameters it requires.

The sequence ends with the command end character ( **CMDOFF** : `$FE` )

Some commands will exit *command mode* automatically without needing a `CMDOFF` character, but is good practice to include it anyway.

For example the following byte sequence enters command mode, sets the screen to Hires mode on page 0 with blue border and then exits command mode:

        $FF $90 $00 $06 $FE


---



## Reserved Characters

| Hex | Dec | Description
|:---:|:---:|------------
| `$FF` | `255` |Enters command node
| `$FE` | `254` |Exits command node

<br>

## Commands

<br>

### Data Transfer

| Hex | Dec | Description
|:---:|:---:|------------
| `$80` | `128` | Sets the memory pointer for the next transfer **Parameters**<br>- Destination Address : 2 bytes : low \| high
| `$81` | `129` | Selects preset address for the next transfer <br>**Parameters**<br>- Preset Number : 1 byte
| `$82` | `130` | Start a memory transfer<br>**Parameters**<br>- Transfer Size : 2 bytes : low \| high
| `$83` | `131` | Starts audio streaming until receiving a `$00` character
| `$84` | `132` | Starts chiptune streaming until receiving a data block with size `0`, or interrupted by the user
| `$85` | `133` | `New v0.6`<br><br>Sets the stream and write order of the registers for SID streaming<br>**Parameters**<br> - Stream : 25 bytes
| `$86` | `134` | `New v0.7`<br><br>Starts a file transfer (to be saved on a storage device client side)

<br>

### Graphics Mode

| Hex | Dec | Description
|:---:|:---:|------------
| `$90` | `144` | Returns to the default text mode<br>**Parameters**<br>- Page Number : 1 byte<br>- Border Color : 1 byte<br>- Background Color : 1 byte
| `$91` | `145` | Switches to hi-res bitmap mode<br>**Parameters**<br>- Page Number : 1 byte<br>- Border Color : 1 byte
| `$92` | `146` | Switches to multicolor bitmap mode <br> **Parameters**<br>- Page Number : 1 byte<br>- Border Color : 1 byte<br>- Background Color : 1 byte<br>**Only for Plus/4:**<br>- Multicolor 3 color : 1 byte

<br>

### Drawing Primitives

| Hex | Dec | Description
|:---:|:---:|------------
| `$98` | `152` | `New v0.8` Clears graphic screen
| `$99` | `153` | `New v0.8` Set pen color<br> **Parameters**<br>- Pen Number: 1 byte<br>- Color index: 1 byte
| `$9A` | `154` | `New v0.8` Plot point<br> **Parameters**<br>- Pen Number: 1 byte<br>- X coordinate: 2 bytes<br>- Y coordinate: 2 bytes
| `$9B` | `155` | `New v0.8` Line<br> **Parameters**<br>- Pen Number: 1 byte<br>- X1: 2 bytes<br>- Y1: 2 bytes<br>- X2: 2 bytes<br>- Y2: 2 bytes
| `$9C` | `156` | `New v0.8` Box<br> **Parameters**<br>- Pen Number: 1 byte<br>- X1: 2 bytes<br>- Y1: 2 bytes<br>- X2: 2 bytes<br>- Y2: 2 bytes<br>- Fill: 1 byte
| `$9D` | `157` | `New v0.8` Circle/Ellipse<br> **Parameters**<br>- Pen Number: 1 byte<br>- X: 2 bytes<br>- Y: 2 bytes<br>- rX: 2 bytes<br>- rY: 2bytes
| `$9E` | `158` | `New v0.8` Fill<br> **Parameters**<br>- Pen Number: 1 byte<br>- X: 2 bytes<br>- Y: 2 bytes

<br>

### Connection Management

| Hex | Dec | Description
|:---:|:---:|------------
| `$A0` | `160` | Selects the screen as the output for the received characters, exits command mode
| `$A1` | `161` | Selects the optional hardware voice synthesizer as the output for the received characters, exits command mode.<br><br> (*Valid only for the microsint + rs232 / Wi-Fi board*)
| `$A2` | `162` | Request terminal ID and version
| `$A3` | `163` | `New v0.6`<br><br> Query if the command passed as parameter is implemented in the terminal. If the returned value has its 7th bit clear then the value is the number of parameters required by the command.<br><br>(*Max 8 in the current Retroterm implementation*)<br><br>If the 7th bit is set the command is not implemented.
| `$A4` | `164` | `New v0.8`<br><br> Query the client's setup. The single byte parameter indicates which 'subsystem' is being queried. Client must reply with at least 1 byte indicating the reply length. Zero meaning not implemented. See below for the subsystem parameters.

<br>

### Screen Management

| Hex | Dec | Description
|:---:|:---:|------------
| `$B0` | `176` | Moves the text cursor<br>**Parameters**<br> - Column : 1 byte <br> - Row : 1 byte <br><br> Exits command mode
| `$B1` | `177` | Fills a text screen row with a given <br> character, text cursor is not moved<br>**Parameters**<br>- Screen Row : 1 byte <br>- Fill Character : 1 byte : *C64 Screen Code*
| `$B2` | `178` | Enables or disables the text cursor<br>**Parameters**<br>- Enable : 1 byte
| `$B3` | `179` | Screen split<br>**Parameters**<br>- Modes : 1 byte<br>  `Bit 0 - 4` : Split Row `1 - 24`<br>  `Bit 7` : Bitmap Graphics Mode in top section<br>    `0` : Hires<br>    `1` : Multicolor <br><br> - Background Color : 1 byte<br>  `Bit 0 - 3` : Top Section<br>  `Bit 4 - 7` : Bottom Section
| `$B4` | `180` | `New v0.7`<br><br>Get text cursor position, returns 2 characters, column and row.
| `$B5` | `181` | Set text window<br>**Parameters**<br> - Top Row : 1 byte : `0 - 23`<br> - Bottom Row : 1 byte : `1 - 24`
| `$B6` | `182` | `New v0.7`<br><br>Scroll the text window up or down x rows<br>**Parameters**<br> - Row count: 1 byte -128/+127
| `$B7` | `183` | `New v0.7`<br><br>Set ink color<br>**Parameters**<br> - Color index: 1 byte
<br>

### Preset Addresses

*For command `$81`*

#### Commodore 64 & Plus/4
| Hex | Dec | Description
|:---:|:---:|:------------
| `$00` | `0` | Text page `0`
| `$10` | `16` | Bitmap page `0`
| `$20` | `32` | Color RAM

*The current versions of **Retroterm** supports only a single text / bitmap page.*<br>*Values other than `0` for bits `0 - 3` will be ignored.*
<br>
#### MSX1

| Hex | Dec | Description
|:---:|:---:|:------------
| `$00` | `0` | Text/name table page `0`
| `$10` | `16` | Pattern table page `0`
| `$20` | `32` | Color table

*Any other value will set the address to $4000 (RAM Page 1) -Subject to changes-*

### "Subsystems"

*For command `$A4`*

##### `$00`: Platform/Refresh rate

Reply length: 2 bytes

| Position | Value
|:---:|:---
| 0 | 1
| 1 | bits 0-6: platform<br> bit 7: Refresh rate

###### Platform:

| Value | Platform
|:---:|:---
| 0 | C64
| 1 | Plus/4
| 2 | MSX
| 3 | `reserved` C128
| 4 | `reserved` VIC20
| 5 | `reserved` ZX Spectrum
| 6 | `reserved` Atari
| 7 | `reserved` Apple II
| 8 | `reserved` Amstrad
| 9 | `reserved` Amiga
| 10 | `reserved` PET
|

###### Refresh rate:

| Value | Meaning
|:---:|:---
| 0 | 50Hz
| 1 | 60Hz


##### `$01`: Text screen size

Reply length: 3 bytes

| Position | Value
|:---:|:---
| 0 | 2
| 1 | Columns
| 2 | Rows
|

##### `$02`: Connection speed

Reply length: 2 bytes

| Position | Value
|:---:|:---
| 0 | 1
| 1 | <br>0: Network<br>1: 300bps<br>2: 600bps<br>3: 1200bps<br>4: 1800bps<br>5: 2400bps<br>6: 4800bps<br>7: 9600bps<br>8: 19200bps<br>9: 28800bps<br>10: 38400bps<br>11: 57600bps<br>12: 76800bps<br>13: 115200bps
|

##### `$03`: RAM size

Reply length: 3 bytes

| Position | Value
|:---:|:---
| 0 | 2
| 1-2 | RAM size in Kilobytes (big-endian)
|

##### `$04`: VRAM size

Reply length: 3 bytes

| Position | Value
|:---:|:---
| 0 | 2
| 1-2 | VRAM size in Kilobytes (big-endian)
|

##### `$05`: Graphic modes (platform dependent)

Reply length: 2 bytes

| Position | Value
|:---:|:---
| 0 | 1
| 1 | Graphic modes available
|

###### C64:

In addition to Hires and Multicolour

| bit | Mode
|:---:|:---
| 0 | FLI
| 1 | AFLI
|
###### C128:

In addition to Hires and Multicolour

| bit | Mode
|:---:|:---
| 0 | FLI
| 1 | AFLI
| 2 | VDC
| 3 | VDCI

###### MSX:

In addition to Screen2

| bit | Mode
|:---:|:---
| 0 | Screen 3
| 1 | Screen 4
| 2 | Screen 5
| 3 | Screen 6
| 4 | Screen 7
| 5 | Screen 8
| 6 | Screen 10
| 7 | Screen 12

###### Amiga:

| Value | Chipset
|:---:|:---
| 0 | OCS
| 1 | ECS
| 2 | AGA

##### `$06`: Audio (platform dependent)

Reply length: 3 bytes

| Position | Value
|:---:|:---
| 0 | 2
| 1 | Synthesizers
| 2 | PCM

###### Synthesizers

###### -Commodore 64/128

| bit | Meaning
|:---:|:---
| 0-3 | Installed SID(s)-1
| 4 | OPL present
| 5 | microSynth present
| 6 | Magic Voice present

###### -MSX

| bit | Meaning
|:---:|:---
| 0 | MSX Audio present
| 1 | MSX Music present
| 2 | OPL3 present

###### PCM

| bit | Meaning
|:---:|:---
| 0-1 | bits per sample (1(PWM)/4/8/16)
| 2 | Channels (1/2)
| 3 | Connection speed dependent sample rate
| 4 | 11025Hz sample rate
| 5 | 16000Hz sample rate
| 6 | 22050Hz sample rate
| 7 | Delta compression
