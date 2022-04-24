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

</div>




-----------------
1-1 Introduction
-----------------

Retroterm is a small (less than 5KBytes in memory) PETSCII terminal for the
Commodore 64/128(in 64 mode) implementing the Turbo56K protocol for high speed
data transfer and streaming.
Retroterm uses a fixed userport data rate of 57600bps, but efective data rate
with the screen on is equivalent 1500/1800bps depending on PAL/NTSC timings.
The full data throughput is achieved only with when disabling the screen for
turbo data transfers and streaming.

Retroterm is optimized for use with Wi-Fi modems using the Zimodem firmware,
Retroterm also runs on the latest VICE and Z64K emulators.





-------------
1-4 Features
-------------
Retroterm implements all the commands of the Turbo56K v0.6 protocol (read the
turbo56k.txt file for more information):

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