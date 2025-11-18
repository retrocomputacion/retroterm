<div align = center>

# Retroterm
### VERSION 0.30 Beta

Jorge Castillo & Pablo Roldán


![Badge commodore](https://img.shields.io/badge/Commodore-64%2f128%20%26%20Plus%2f4-1E2A4E?logo=commodore&logoColor=1E2A4E&labelColor=ccc) ![Badge MSX1](https://img.shields.io/badge/MSX1-darkred) ![GitHub all releases](https://img.shields.io/github/downloads/retrocomputacion/retroterm/total?labelColor=ccc) ![Badge license](https://img.shields.io/github/license/retrocomputacion/retroterm?labelColor=ccc) [![Discord](https://img.shields.io/discord/625776626356977674?logo=discord&logoColor=white&label=Discord&color=blue)](https://retrocomputacion.com/thumb.php?src=e_MEDIA_IMAGE%2F2024-09%2Fdiscord.png&w=384&h=0)

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

*Retroterm* is a small, minimal *PETSCII* terminal for the *Commodore 64 / 128 (in 64 mode)*, *Commodore Plus/4* and *MSX1 (in development)*.

**ATTENTION THE MSX PORT IS CURRENTLY IN ALPHA STATE**

It implements the *[Turbo56k](docs/turbo56k.md)* protocol for high speed data transfer & streaming when connecting to a BBS supporting the protocol, such as [_RetroBBS_](https://github.com/retrocomputacion/retrobbs).

Data rate is fixed at the following speeds:

- 57600bps for Retroterm 64 for userport and Turbo232**
- 57600bps for Retroterm MSX for parallel port (very alpha state - candidate for deprecation)
- 38400bps for Retroterm 64 for Swiftlink**
- 38400bps for Retroterm MSX for parallel port (alpha state - mostly stable)
- 19200bps for Retroterm Plus/4** and Retroterm MSX for RS232**

*(**)The full data throughput while using the turbo transfer / streaming can only be achieved with the screen disabled.*

The effective throughput for text is *1500 / 1800bps* depending on *PAL/NTSC* timings respectively.

*Retroterm* is optimized for use with **Wi-Fi** modems using the *Zimodem* firmware.

*It also runs on the latest **VICE**/**Z64K** and **openMSX** emulators.*

Separate *Commodore 64* versions of the executable are provided for cartridges featuring an ACIA 6551 such as *SwiftLink* (limited to **38400bps**) and *Turbo232*.</br>
The *Commodore Plus/4* version is limited to the maximum speed for the built in ACIA: **19200bps**
<br>
The *MSX 1 RS-232* version (rt232.com) is also at this moment limited to **19200bps**, and only supports RS-232 interfaces that adhere to the MSX standard (ie: SVI-738 and HX-22 built-in ports, SVI-737 and Sony HB-232, or any other interface implemented with the i8251 + i8253 USART and Timer combo)<br>

## 1.1 Release history

### v0.30 (??/??/2025):
- Turbo56K v0.8 partial implementation
- Drawing primitives: clear screen, plot, line, box, ellipse, fill
- Commodore ports: Ability to save a configuration file to disk.
Optional modem init string with selectable 300,1200 or 2400 baud speed.
- Commodore ports: Phone book with up to 5 preset dial strings, also saved to the configuration file.
- Bugfix, all ports: No more missing filename characters when downloading to disk
- Bugfix, C64 ports: Fixed glitched sprites after downloading to disk

### v0.21 (Intermediate Github version, no oficial release):
- New shortcut key for the Plus/4 port: `CBM+,` disables/enables the FLASH-ON control code. Improves compatibility with BBSs running Centipede 128 software.
- Bugfix for the Commodore 64 ports: No more extraneous beep after streaming PCM audio.

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

*Implements all commands of the __[Turbo56k](docs/turbo56k.md)__ v0.7 protocol.*

- Full duplex PETSCII (Commodore versions)/ASCII (MSX versions) color terminal
- Turbo data transfers to custom/preset memory locations
- Split Screen | Hi-res or Multicolor+Text
- Up to 11520Hz 4-bit PCM audio streaming
- Bitmap display | Hi-res + Multicolor (Commodore) | Screen 2 (MSX)
- Uses 16K of RAM above $C000 (For the Commodore versions), the rest of the RAM is free to use as a buffer, load programs, graphics, etc.
- 1x speed SID music streaming (C64 versions)
- PSG music streaming (MSX versions)
- Scrolling Text Windows
- Set Cursor Position
- Text Line Fill
- Bidirectional scrolling
- Download files to disk
- Graphic primitives, draw onto Hi-res/Multicolor screens

## 1.4 Requirements

- A Commodore 64, 128 or Plus/4 computer, or an emulator such as VICE or Z64K
- Either an userport Wi-Fi modem with the Zimodem firmware or...
- A *SwiftLink* or *Turbo232* compatible cartridge connected to a Wi-Fi modem with the Zimodem firmware.
- A MSX1 or superior computer with 64K of RAM. With either a built-in RS-232 port or an MSX standard RS-232 interface cartridge. Or openMSX 20.0RC1
- ACME and PASMO crossassemblers for building the programs.

## 2 Usage
*Retroterm* is very simple to use, most of its functionality being controlled externally from a *[Turbo56k](docs/turbo56k.md)* enabled BBS.

*Retroterm* comes in five variants:

- **rt-u.prg** Userport version 57600bps
- **rt-sl.prg** SwiftLink version 38400bps
- **rt-232.prg** Turbo232 version 57600bps
- **rt-p4.prg** Plus/4 version 19200bps
- **rt232.com** MSX RS-232 version 19200bps
- **rt38k.com** MSX parallel port version 38400bps
- **rt56k.com** MSX parallel port version 57600bps

*Retroterm* lacks classic file transfer functions, when used to communicate with a normal PETSCII/ASCII BBS, file transfers are not available.

After LOADing and RUNning *Retroterm*, you can dial to your favorite BBS using your Modem commands as usual.

**On C64 and Plus/4 ports, _Retroterm_ will search for a configuration file on the last used drive, if found, and a modem initialization string is set, it will be sent at this moment. If no configuration file is found then the setup screen will be shown**

**The modem should be setup to the correct baud rate before running _Retroterm_.**

To exit *Retroterm*, just press `RUN/STOP` (Commodore) or `CTRL-C` (MSX), it will remain in memory, and you can recall it with `SYS49152` (Commodore 64), or `SYS28672` (Commodore Plus/4).

If you downloaded a program into memory from a BBS you can also `RUN` it (Commodore versions only).

On MSX pressing `CTRL-U` will reset the computer, if a ROM file was downloaded to RAM the system will try to execute it. ROMs that expect mirroring are not supported.

*Retroterm* beeps for every received character by default, you can toggle the sound by pressing `CBM + M` (Commodore) or `CTRL-W` (MSX).

## 2.1 Setup screen

A simple setup screen can be accessed by pressing `CBM + F7` (Commodore) or `CTRL-F5` (MSX), pressing `F1` will exit back to the terminal.

The first setting, common to all _Retroterm_ variants sets the **RTS** pulse width. Current values are known to work under VICE, or on real hardware with an userport modem using Zimodem firmware, and with the Ultimate Swiftlink emulation.
Use the `+` and `-` keys to adjust.

For the ACIA versions there's a couple more settings.

The first one sets the base address for the Swiftlink or Turbo232 cartridge, press `1` for $D700 or `2` for $DE00 or `3` for $DF00. Switching addresses will drop any current connection.

The second setting available for ACIA versions is the ability to keep the screen visible while transferring at turbo speeds. Turbo transfers are slightly slower with the screen enabled. Press `B` to toggle.

For C64 and Plus/4 ports, an optional modem initialization string can be edited by pressing `I`. The baud rate used to transmit this string can be set by pressing `R`, with current options being `SKIP`, `300`, `1200` and `2400`.
This string is only transmitted when running _Retroterm_ with `RUN`, automatically if a configuration file is found, or, if no file is found by selecting the appropriate options in the setup screen before exiting to the terminal.
Use `F5` to save the current settings to disk.

Finally pressing `P` switches to the phone book screen, here you can select one of 5 preset dial strings and either `E`dit or `D`ial it.
(Dialing only works once _Retroterm_ has been initialized properly, meaning only when accessing the setup screen from within the terminal mode)


**Important**: Upon exiting the setup screen, _Retroterm_ will default to full screen text mode. Only previous background and border colors will be restored.

## 3 Building Retroterm
The Commodore versions of *Retroterm* is written for the *ACME* cross-assembler, while *PASMO* is used for the MSX port.

Exomizer is used to pack all the Commodore ports. Find these packed versions inside the /build/packed/ subdirectory

To compile use:

```
make
```
to compile all versions.

Or you can specify which version you want to compile, either `userport`, `swiflink`, `turbo232`, `plus4`, `msx232` or `msx56k`.

ie:
```
make userport
```
All executables will be stored in the `build` directory.

You can also manually compile with:
```
acme retroterm_univ.asm
```

or:
```
pasmo -E IFACE=0 retrotermm1.asm rt232.com
pasmo -E IFACE=56 retrotermmm1.asm rt56k.com
```

from the `source` directory.</br>
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

### `IFACE`:
`0`: Compile for standard MSX RS232 ports
`56`: Compile for the MSX parallel port

### Example:

```
acme -D_HARDTYPE_=232 -D_INTRO_=0 retroterm_univ.asm
```
Compiles _Retroterm_ for the _Turbo232_ cartridge with no intro screen.

## 3.2 The intro screen
The intro screen is a `screencode + colorRAM` dump found in `source/intro_sc.asm`

This file is generated by exporting `source/intro_sc.petmate` to ACME format from [Petmate](https://nurpax.github.io/petmate/).

## 3.3 Customizing

If you want to release a modified version of _Retroterm_ which differs in functionality from the official release we recommend you use a custom _ID string_, respecting the maximum 22 character length and always starting in uppercase `RT`

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
- **Roberto Mandracchia**
- **x1pepe**

### Thanks To
  
- **Willy Manilly** for adding support to the *Z64K emulator*

- [**ElectronicsArchiver**](https://github.com/ElectronicsArchiver) for initial documentation rework
