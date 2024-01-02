<div align = center>

# Retroterm
### VERSION 0.20 RC

Jorge Castillo & Pablo Roldán


![Badge commodore](https://img.shields.io/badge/Commodore-64%2f128%20%26%20Plus%2f4-1E2A4E?logo=commodore&logoColor=1E2A4E&labelColor=ccc) ![GitHub all releases](https://img.shields.io/github/downloads/retrocomputacion/retroterm/total?labelColor=ccc) ![Badge license](https://img.shields.io/github/license/retrocomputacion/retroterm?labelColor=ccc)

---
</div>

## Table of contents:

1. [Introduction](#1-introduction)
   1. [Release history](#11-release-history)
   2. [The *Turbo56K* protocol](#12-the-turbo56k-protocol)
   3. [Features](#13-features)
   4. [Requirements](#14-requirements)  
2. [Usage](#2-usage)
3. [Building](#3-building-retroterm)
   1. [Symbols](#31-symbols)
   2. [Intro screen](#32-the-intro-screen)
   3. [Customizing](#33-customizing)
4. [Known bugs](#4-known-bugs)
5. [To-do](#5-to-do)
6. [Acknowledgments](#6-acknowledgments)

---
<div>

## 1 Introduction

*Retroterm* is a small, minimal *PETSCII* terminal for the *Commodore 64 / 128 (in 64 mode)* and the *Commodore Plus/4 (in development)*.

It implements the *[Turbo56k](docs/turbo56k.md)* protocol for high speed data transfer & streaming when connecting to a BBS supporting the protocol, such as [_RetroBBS_](https://github.com/retrocomputacion/retrobbs).

While the userport data rate is fixed at *57600bps* (*19200bps* for the Plus/4), the effective throughput with the screen on is *1500 / 1800bps* depending on *PAL/NTSC* timings respectively.

*The full data throughput while using the turbo transfer / streaming can only be achieved with the screen disabled.*

*Retroterm* is optimized for use with **Wi-Fi** modems using the *Zimodem* firmware.

*It also runs on the latest **VICE** / **Z64K** emulators.*

Separate *Commodore 64* versions of the executable are provided for cartridges featuring an ACIA 6551 such as *SwiftLink* (limited to **38400bps**) and *Turbo232*.</br>
The *Commodore Plus/4* version is limited to the maximum speed for the built in ACIA: **19200bps**
<br>

## 1.1 Release history

### v0.20 (02/01/2024):
- Turbo56K v0.7
- New _Commodore Plus/4_ port. _Turbo56K v0.7_ commands implemented, except for the ones regarding SID tune streaming.
- Fixed bug in command $A3 which caused the parameter to be misread, or the terminal to hang
- New command `$86` to initiate a download to disk.
- New command `$B6` to scroll the text window up or down X number of rows.
- SID streaming now better supports tunes using _hardrestart_, a special version of _SIDdump_ and updated version of _RetroBBS_ is required.
- New compile target `ultimate` compiles with timings compatible with the Swiftlink emulation in the Ultimate1541-II/II+ and Ultimate64.
- Basic configuration screen by pressing `C= + F7`, terminal screen is not preserved.
- ACIA based versions now allow selection of the interface base address ($DE00 or $DF00).
- Connected disk drives are scanned on first run, and can be selected when downloading a file with command `$86`.

### v0.14 (13/04/2022):
- Source code liberated
- Turbo56K v0.6
- Fixed small visual glitch when using the split screen mode
- Better, more robust exit to BASIC
- Disabled interrupts on command `$A2`, prevents crashes if (re)connecting to a BBS while the split screen mode is active
- Fixed severe bug affecting the text output in early Commodore 64 Kernal revisions
- Full digi-boost for 8580 SID
- Intro screen exits automatically after ~2 seconds by default
- Other small bugfixes.
### v0.13 (16/08/2021):
- Initial release
- Turbo56K v0.5


## 1.2 The Turbo56K protocol
*[Turbo56k](docs/turbo56k.md)* was created by Jorge Castillo as a simple protocol to provide high-speed file transfer functionality to his bit-banging 57600bps RS232 routine for the C64.
Over time, the protocol has been extended to include 4-bit PCM audio streaming, bitmap graphics transfer and display, SID music streaming and more.

## 1.3 Features

*Implements all commands of the __[Turbo56k](docs/turbo56k.md)__ v0.6 protocol.*

- Full duplex PETSCII color terminal
- Turbo data transfers to custom / preset memory locations
- Split Screen | Hi-res or Multicolor + Text
- 11520Hz 4 - bit PCM audio streaming
- Bitmap display | Hi-res + Multicolor
- Consumes less than 5 KB of memory
- 1x speed SID music streaming
- Scrolling Text Windows
- Set Cursor Position
- Text Line Fill

## 1.4 Requirements

- A Commodore 64, 128 or Plus/4 computer or an emulator such as VICE or Z64K
- Either an userport Wi-Fi modem with the Zimodem firmware or...
- A *SwiftLink* or *Turbo232* compatible cartridge connected to a Wi-Fi modem with the Zimodem firmware.
- ACME crossassembler for building the programs.

## 2 Usage
*Retroterm* is very simple to use, most of its functionality being controlled externally from a *[Turbo56k](docs/turbo56k.md)* enabled BBS.

*Retroterm* comes in three flavors:

- **rt_v0.20.prg** Userport version 57600bps
- **rt_sl_v0.20.prg** SwiftLink version 38400bps
- **rt_232_v0.20.prg** Turbo232 version 57600bps
- **rt_p4_v0.10.prg** Plus/4 version 19200bps

*Retroterm* lacks classic file transfer functions, when used to communicate with a normal PETSCII BBS, file transfers are not available.

After LOADing and RUNning *Retroterm*, you can dial to your favorite BBS using your Modem commands as usual.

**The modem should be setup to the correct baud rate before running _Retroterm_.**

To exit *Retroterm*, just press `RUN/STOP`, it will remain in memory, and you can recall it with `SYS49152` (Commodore 64), or `SYS28672` (Commodore Plus/4).

If you downloaded a program into memory from a BBS you can also `RUN` it.

*Retroterm* beeps for every received character by default, you can toggle the sound by pressing `CBM + M`.

## 2.1 Setup screen

A simple setup screen can be accessed by pressing `CBM + F7`, pressing `F1` will exit back to the terminal.

The first setting, common to all _Retroterm_ variants sets the **RTS** pulse width. Current values are known to work under VICE, or on real hardware with an userport modem using Zimodem firmware, and with the Ultimate Swiftlink emulation.
Use the `+` and `-` keys to adjust.

For the ACIA versions there's a couple more settings.

The first one sets the base address for the Swiftlink or Turbo232 cartridge, press `1` for $DE00 or `2` for $DF00. Switching addresses will drop any current connection.

The second setting available for ACIA versions is the ability to keep the screen visible while transferring at turbo speeds. Turbo transfers are slightly slower with the screen enabled. Press `B` to toggle.

**Important**: Upon exiting the setup screen, _Retroterm_ will default to full screen text mode. Only previous background and border colors will be restored.

## 3 Building Retroterm
*Retroterm* is written for the *ACME* cross-assembler, to compile use:

```
make
```
to compile all versions.

Or you can specify which version you want to compile, either `userport`, `swiflink` or `turbo232`.

ie:
```
make userport
```
All executables will be stored in the `build` directory.

You can also manually compile with:
```
acme retroterm_univ.asm
```
from the `source` directory.
This will compile the default userport version of *Retroterm* with an intro screen that last a couple of seconds. In this case the resulting executable will be written to the `source` directory

## 3.1 Symbols
A number of compile time symbols are defined to customize the resulting executable. Specially if you're running the compiler directly instead of using the Makefile

### `_HARDTYPE_`:
`38`: Compile for *SwiftLink/Turbo232* cartridges at **38400bps**

`56`: Compile for userport at **57600bps** -- **_Default_**

`232`: Compile for *Turbo232* cartridges at **57600bps**

`1541`: Compile for the *Ultimate 1541-II* or *Ultimate 64* Swiftlink emulation, same code as for `38` but different timing.

### `_INTRO_`:
`0`: No intro screen

`1`: Include the intro screen -- **_Default_**

### `_SPACE_`:
If defined wait for the user to press the space bar at the intro screen. Otherwise, the intro screen only last a couple of seconds.

**_Not defined by default_**

### Example:

```
acme -D_HARDTYPE_=232 -D_INTRO_=0 retroterm_univ.asm
```
Compiles _Retroterm_ for the _Turbo232_ cartridge with no intro screen.

## 3.2 The intro screen
The intro screen is a `screencode + colorRAM` dump found in `source/intro_sc.asm`

This file is generated by exporting `source/intro_sc.petmate` to ACME format from [Petmate](https://nurpax.github.io/petmate/).

## 3.3 Customizing

If you want to release a modified version of _Retroterm_ which differs in functionality from the official release we recommend you use a custom _ID string_, respecting the maximum 22 character length and always starting in uppercase `RT` (*PETSCII*)

IE, the normal ID string is:
```
IDstring:
!text "RTRETROTERM 0.20      "
```
but the string when compiling for the *SwiftLink* cartridge is:

```
IDstring:
!text "RTRETROTERM-SL 0.20   "
```

**Note: The actual version number string is sourced from the file `source/version.txt` when using the *makefile*, or from `source/version.asm` when running the compiler directly**


For compatibility reasons we ask you not to modify the behavior of existing Turbo56K commands, but you're welcomed to add new commands, or remove unwanted ones, as long as command `$A3` correctly reports the existence or not of all queried commands.

In any case the *[Turbo56k](docs/turbo56k.md)* version bytes that follow the ID string should remain the correct ones for the official version your modified code support.


## 4 Known bugs

- Losing connection while streaming data or audio will hang the program
- Exiting `Retroterm`, restarting it with `SYS49152`, exiting again and causing a BASIC error will crash the computer.

## 5 To-do
- Extend the command parameter space to support more than 8 parameters per command.
- Faster throughput when using any of the ACIA cartridges.

## 6 Acknowledgments
### Beta Testers
  
- **Ezequiel Filgueiras**
- **Thierry Kurt**
- **Diego di Franceschi**
- **ChrisKewl**

### Thanks To
  
- **Willy Manilly** for adding support to the *Z64K emulator*

- [**ElectronicsArchiver**](https://github.com/ElectronicsArchiver) for initial documentation rework
