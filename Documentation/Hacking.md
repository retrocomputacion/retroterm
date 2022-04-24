
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