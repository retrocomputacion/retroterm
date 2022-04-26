
<div align = center>
<b>

```
PPPP   RRRR    OOO   TTTTT   OOO    CCCC   OOO   L    
P   P  R   R  O   O    T    O   O  C      O   O  L    
PPPP   RRRR   O   O    T    O   O  C      O   O  L    
P      R   R  O   O    T    O   O  C      O   O  L    
P      R   R   OOO     T     OOO    CCCC   OOO   LLLLL
```

</b>

<br>
<br>
</div>

# Turbo56K v0.6

<br>

**Turbo56K** was created by **Jorge Castillo** as a simple protocol to provide high speed file transfer functionality to his bit-banging `57600bps` **RS232** routine for the **C64**.

Over time, the protocol has been extended to include `4-bit` **PCM** audio streaming, bitmap graphics transfer and display, **SID** music streaming and more.

A typical **Turbo56K** command sequence consists of a command start character ( **CMDON** : `$FF` ) followed by the command itself (a character with it's 7th bit set) and the parameters it requires.

The sequence ends with the command end character ( **CMDOFF** : `$FE` )

Some commands will exit `command mode` automatically without needing a **CMDOFF** character, but is good practice to include it anyways.

<br>

---

<br>

# Retroterm v0.14
  
*Retroterms implementation of the **Turbo56K** protocol*

<br>
<br>
<div align='center'>  
## Reserved Characters

| Hex | Dec | Description
|:---:|:---:|------------
  | `$FF` | `255` |<div align='left'> Enters command node</div>
| `$FE` | `254` | <div align='left'>Exits command node

<br>
<br>

## Commands

<br>

### Data Transfer

| Hex | Dec | Description
|:---:|:---:|------------
| `$80` | `128` | <div align='left'>Sets the memory pointer for the next transfer <br> <h6>Parameters</h6> - Destination Address : 2 bytes : low \| high</div>
| `$81` | `129` | <div align='left'>Selects preset address for the next transfer <br> <h6>Parameters</h6> - Preset Number : 1 byte</div>
| `$82` | `130` | <div align='left'>Start a memory transfer <br> <h6>Parameters</h6> - Transfer Size : 2 bytes : low \| high</div>
| `$83` | `131` | <div align='left'>Starts audio streaming until receiving a `$00` character</div>
| `$84` | `132` | <div align='left'>Starts SID streaming until receiving a data <br> block with size `0`, or interrupted by the user</div>
| `$85` | `133` | <div align='left'>`Since v0.6` <br><br> Sets the stream and write order of the registers for SID streaming <br> <h6>Parameters</h6> - Stream : 25 bytes</div>

<br>

### Graphics Mode

| Hex | Dec | Description
|:---:|:---:|------------
| `$90` | `144` |<div align='left'> Returns to the default text mode <br> <h6>Parameters</h6> - Page Number : 1 byte <br> - Border Color : 1 byte <br> - Background Color : 1 byte</div>
| `$91` | `145` | <div align='left'>Switches to hi-res bitmap mode <br> <h6>Parameters</h6> - Page Number : 1 byte <br> - Border Color : 1 byte</div>
| `$92` | `146` | <div align='left'>Switches to multicolor bitmap mode <br> <h6>Parameters</h6> - Page Number : 1 byte <br> - Border Color : 1 byte <br> - Background Color : 1 byte</div>

<br>

### Connection Management

| Hex | Dec | Description
|:---:|:---:|------------
| `$A0` | `160` | <div align='left'>Selects the screen as the output for the <br> received characters, exits command mode</div>
| `$A1` | `161` | <div align='left'>Selects the optional hardware voice synthesizer <br> as the output for the received characters, exits <br> command mode. <br><br> *Valid only for the microsint + rs232 / WiFi board*</div>
| `$A2` | `162` | <div align='left'>Request terminal ID and version</div>
| `$A3` | `163` | <div align='left'>`Since v0.6` <br><br> Query if the command passed as parameter is <br> implemented in the terminal. If the returned <br> value has its 7th bit clear then the value is the <br> number of parameters required by the command. <br><br> *Max 8 in the current retroterm implementation* <br><br> If the 7th bit is set the command is not implemented.</div>

<br>

### Screen Management

| Hex | Dec | Description
|:---:|:---:|------------
| `$B0` | `176` | <div align='left'>Moves the text cursor <br> <h6>Parameters</h6> - Column : 1 byte <br> - Row : 1 byte <br><br> Exits command mode</div>
| `$B1` | `177` | <div align='left'>Fills a text screen row with a given <br> character, text cursor is not moved <br> <h6>Parameters</h6> - Screen Row : 1 byte <br> - Fill Character : 1 byte : *C64 Screen Code*</div>
| `$B2` | `178` | <div align='left'>Enables or disables the text cursor <br> <h6>Parameters</h6> - Enable : 1 byte</div>
| `$B3` | `179` | <div align='left'>Screen split <br> <h6>Parameters</h6> - Modes : 1 byte <br>  `Bit 0 - 4` : Split Row `1 - 24` <br>  `Bit 7` : Bitmap Graphics Mode in top section <br>    `0` : Hires <br>    `1` : Multicolor <br><br> - Background Color : 1 byte <br>  `Bit 0 - 3` : Top Section <br>  `Bit 4 - 7` : Bottom Section</div>
| `$B4` | `180` | <div align='left'>**RESERVED**</div>
| `$B5` | `181` | <div align='left'>Set text window <br> <h6>Parameters</h6> - Top Row : 1 byte : `0 - 23` <br> - Bottom Row : 1 byte : `1 - 24`</div>

<br>

### Preset Addresses

*For command `$81`*

| Hex | Dec | Description
|:---:|:---:|------------
| `$00` | `0` | <div align='left'>Text page `0`</div>
| `$10` | `16` | <div align='left'>Bitmap page `0`</div>
  | `$20` | `32` | <div align='left'>Color RAM</div>

<br>

*The current version of **Retroterm** supports <br>
only a single text / bitmap page.*

*Values other than `0` for bits `0 - 3` will be ignored.*

</div>

