===============================================================================

         RRRR   EEEEE  TTTTT  RRRR    RRR   TTTTT  EEEEE  RRRR   M   M
         R   R  E        T    R   R  R   R    T    E      R   R  MM MM
         RRRR   EEEE     T    RRRR   R   R    T    EEEE   RRRR   M M M
         R  R   E        T    R  R   R   R    T    E      R  R   M   M
         R   R  EEEEE    T    R   R   RRR     T    EEEEE  R   R  M   M

                                VERSION 0.14

      (C)2020-2022 By Jorge Castillo(Pastbytes) & Pablo Roldán(Durandal)
===============================================================================

------------------
Table of contents
------------------
1-1 Introduction
1-2 Release history
1-3 The Turbo56K protocol
1-4 Features
1-5 Usage

2-1 TO-DO List
2-2 Known bugs

3-1 Compiling Retroterm
3-2 Modifying Retroterm/Extending Turbo56K

4-1 Acknowledgements


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

--------------------
1-2 Release history
--------------------

v0.13 (16-08-2021): First oficial release, implements Turbo56K v0.5 protocol

v0.14 (13-04-2022): -Better, more robust exit to BASIC
                    -Fixed small visual glitch when using the split screen mode
                    -Disabled interrupts on command $A2, prevents crashes if
                     (re)connecting to a BBS while the split screen mode is
                     active.
                    -Turbo56K v0.6 implemented.
                    -Fixed severe bug with text output in early Commodore 64
                     Kernal revisions.
                    -Full digi-boost for 8580 SID.
                    -Intro screen exits automaticaly after ~2 seconds.
                    -Other small bugfixes.

--------------------------
1-3 The Turbo56K protocol
--------------------------
Turbo56K was created by Jorge Castillo as a simple protocol to provide high
speed file transfer functionality to his bitbanging 57600bps RS232 routine
for the C64.
Over time, the protocol has been extended to include 4-bit PCM audio streaming,
bitmap graphics transfer and display, SID music streaming and more.

A typical Turbo56K command sequence consists of a command start character
(CMDON: $FF) followed by the command itself (a character with it's 7th bit set)
and the parameters it requires.
The sequence ends with the command end character (CMDOFF: $FE)

*Some commands will exit 'command mode' automaticaly without needing a CMDOFF
character, but is good practice to include it anyways.

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

----------
1-5 Usage
----------
Retroterm is very simple to use, most of its functionality being controlled
externally from a Turbo56K enabled BBS.
Retroterm lacks classic file transfer functions, when used to communicate with
a normal PETSCII BBS, file transfers are not available.

After LOADing and RUNning Retroterm, you can dial to your favourite BBS using
your Modem commands as usual.
To exit Retroterm, just press RUN/STOP, it will remain in memory and you can
recall it with SYS49152.
If you downloaded a program to memory from a BBS you can also RUN it.

Retroterm beeps for every received character by default, you can toggle the
sound by pressing CBM+M.

---------------
2-1 TO-DO list
---------------

    - Extend command parameter space to more than 8 parameters per command.

---------------
2-2 Known bugs
---------------

    - Connection loss during turbo transfers/streaming will cause the computer
      to crash.
    - Exiting Retroterm, recalling it with sys, exiting again and causing a
      BASIC error will crash the computer.

------------------------
3-1 Compiling Retroterm
------------------------

Retroterm is written for the ACME assembler, compiling the release version is
as simple as issuing:

    acme retroterm_univ.asm

The code uses some symbols to customize the executable, these are:

    _INTRO_: 1 = Compile with intro screen (Default 1)
    _HARDTYPE_: 56 = Compile for userport 57600bps (Default)
                232 = Compile for Turbo232 cartridge, 57600bps
                38 = Compile for Swiftlink cartridge, 38400bps
                     (PCM audio streaming at 7680Hz)
    _SPACE_: If defined the intro will wait for the a keypress
             (Default undefined)

The intro screen is a Screencode + colorRAM dump found in retro_logo.asm, this
file is generated by exporting to ACME format from Petmate (using
retrologo.petmate), with slight modifications, please check before overwriting
with your own version.

-------------------------------------------
3-2 Modifying Retroterm/Extending Turbo56K
-------------------------------------------

Retroterm is released under the MIT license (see LICENSE).
If you want to release a modified version of Retroterm which differs in 
functionality with the oficial we recommend you use a custom IDstring,
respecting the maximum 22 character length and always starting in uppercase
'RT' (PETSCII).
ie, the normal IDstring is:

IDstring:
!text "RTRETROTERM 0.14      "

but the string when compiling for the Swiftlink cartridge is:

IDstring:
!text "RTRETROTERM-SL 0.14   "

For compatibility reasons we ask you not to modify the behaviour of existing
Turbo56K commands, but you're welcomed to add new commands.
In any case the Turbo56K version bytes that follow the IDstring should remain
the correct ones for the oficial version your modified code support.

---------------------
4-1 Acknowledgements
---------------------

Beta testing:
*   Ezequiel Filgueiras
*   Thierry Kurt
*   Diego di Franceschi

Thanks to:
    WillyManilly for adding support to the Z64K emulator