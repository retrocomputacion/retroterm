<div align = center>

```
===============================================================================

RRRR   EEEEE  TTTTT  RRRR    RRR   TTTTT  EEEEE  RRRR   M   M
R   R  E        T    R   R  R   R    T    E      R   R  MM MM
RRRR   EEEE     T    RRRR   R   R    T    EEEE   RRRR   M M M
R  R   E        T    R  R   R   R    T    E      R  R   M   M
R   R  EEEEE    T    R   R   RRR     T    EEEEE  R   R  M   M
                                                             
                        VERSION 0.14                         
                                                 
              Jorge Castillo  &  Pablo Roldán                
               ⧼ Pastbytes ⧽      ⧼ Durandal ⧽                
                 
===============================================================================
```

<br>

## Introduction

**Retroterm** is a small **PETSCII** terminal for <br>
the `Commodore 64 / 128` ( in 64 mode ).

It implementing the **Turbo56K** protocol <br>
for high speed data transfer & streaming.

While the userports data rate is a fixed at <br>
`57600bps` , the effective amount with the <br>
screen on is `1500 / 1800bps` depending <bR>
on **PAL** / **NTSC** timings.

*The full data throughput while using the* <br>
*turbo transfer / streaming can only be* <br>
*achieved with the screen disabled.*

**Retroterm** is optimized for the use with <br>
**WiFi** modems using **Zimdem** firmware.

*It also runs on the latest **VICE** / **Z64K** emulators.*

  
<br>
<br>


## Features
  
</div>

Retroterm implements all the commands of the Turbo56K v0.6 protocol (read the
turbo56k.txt file for more information):

- less than 5KBytes in memory
    -Full duplex PETSCII color terminal
    -Turbo data transfers to a custom memory address
    -Turbo data transfers to preset memory addresses
    -11520Hz 4-bit PCM audio streming
    -1x SID music streaming
    -Bitmap display (Hires and multicolor)
    -Split screen (Hires or Multicolor and text)
    -Scrolling Text windows
    -Text line fill
    -Set cursor position





---------------------
4-1 Acknowledgements
---------------------

Beta testing:
*   Ezequiel Filgueiras
*   Thierry Kurt
*   Diego di Franceschi

Thanks to:
    WillyManilly for adding support to the Z64K emulator
