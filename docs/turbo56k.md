
<div align = center>

![logo](turbo56k.png)

</div>

# Turbo56K v0.6


**Turbo56K** was created by **Jorge Castillo** as a simple protocol to provide high speed file transfer functionality to his bit-banging `57600bps` **RS232** routine for the **Commodore 64**.

Over time, the protocol has been extended to include `4-bit` **PCM** audio streaming, bitmap graphics transfer and display, **SID** music streaming and more.

A typical **Turbo56K** command sequence consists of a command start character ( **CMDON** : `$FF` ) followed by the command itself (a character with it's 7th bit set) and the parameters it requires.

The sequence ends with the command end character ( **CMDOFF** : `$FE` )

Some commands will exit *command mode* automatically without needing a `CMDOFF` character, but is good practice to include it anyways.

For example the following byte sequence enters command mode, sets the screen to HiRes mode on page 0 with blue border and then exits command mode:

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
| `$84` | `132` | Starts SID streaming until receiving a data block with size `0`, or interrupted by the user
| `$85` | `133` | `New v0.6`<br><br>Sets the stream and write order of the registers for SID streaming<br>**Parameters**<br> - Stream : 25 bytes
| `$86` | `134` | `New v0.7`<br><br>Starts a file transfer (to be saved on a storage device client side)

<br>

### Graphics Mode

| Hex | Dec | Description
|:---:|:---:|------------
| `$90` | `144` | Returns to the default text mode<br>**Parameters**<br>- Page Number : 1 byte<br>- Border Color : 1 byte<br>- Background Color : 1 byte
| `$91` | `145` | Switches to hi-res bitmap mode<br>**Parameters**<br>- Page Number : 1 byte<br>- Border Color : 1 byte
| `$92` | `146` | Switches to multicolor bitmap mode <br> **Parameters**<br>- Page Number : 1 byte<br>- Border Color : 1 byte<br>- Background Color : 1 byte

<br>

### Connection Management

| Hex | Dec | Description
|:---:|:---:|------------
| `$A0` | `160` | Selects the screen as the output for the received characters, exits command mode
| `$A1` | `161` | Selects the optional hardware voice synthesizer as the output for the received characters, exits command mode.<br><br> (*Valid only for the microsint + rs232 / WiFi board*)
| `$A2` | `162` | Request terminal ID and version
| `$A3` | `163` | `New v0.6`<br><br> Query if the command passed as parameter is implemented in the terminal. If the returned value has its 7th bit clear then the value is the number of parameters required by the command.<br><br>(*Max 8 in the current Retroterm implementation*)<br><br>If the 7th bit is set the command is not implemented.

<br>

### Screen Management

| Hex | Dec | Description
|:---:|:---:|------------
| `$B0` | `176` | Moves the text cursor<br>**Parameters**<br> - Column : 1 byte <br> - Row : 1 byte <br><br> Exits command mode
| `$B1` | `177` | Fills a text screen row with a given <br> character, text cursor is not moved<br>**Parameters**<br>- Screen Row : 1 byte <br>- Fill Character : 1 byte : *C64 Screen Code*
| `$B2` | `178` | Enables or disables the text cursor<br>**Parameters**<br>- Enable : 1 byte
| `$B3` | `179` | Screen split<br>**Parameters**<br>- Modes : 1 byte<br>  `Bit 0 - 4` : Split Row `1 - 24`<br>  `Bit 7` : Bitmap Graphics Mode in top section<br>    `0` : Hires<br>    `1` : Multicolor <br><br> - Background Color : 1 byte<br>  `Bit 0 - 3` : Top Section<br>  `Bit 4 - 7` : Bottom Section
| `$B4` | `180` | **RESERVED**
| `$B5` | `181` | Set text window<br>**Parameters**<br> - Top Row : 1 byte : `0 - 23`<br> - Bottom Row : 1 byte : `1 - 24`
| `$B6` | `182` | Scroll the text window up or down x rows<br>**Parameters**<br> - Row count: 1 byte -128/+127
<br>

### Preset Addresses

*For command `$81`*

| Hex | Dec | Description
|:---:|:---:|:------------
| `$00` | `0` | Text page `0`
| `$10` | `16` | Bitmap page `0`
  | `$20` | `32` | Color RAM

<br>

*The current version of **Retroterm** supports only a single text / bitmap page.*<br>*Values other than `0` for bits `0 - 3` will be ignored.*


