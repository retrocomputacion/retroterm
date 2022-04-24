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