
<div align = center>
<b>

```
H   H   AAA    CCCC  K   K  IIIII  N   N    GGGG
H   H  A   A  C      K   K    I    NN  N   G    
HHHHH  AAAAA  C      KKKK     I    N N N   G  GG
H   H  A   A  C      K   K    I    N  NN   G   G
H   H  A   A   CCCC  K   K  IIIII  N   N    GGGG
```

</b>

<br>

***Retroterm*** *is released under the **[MIT]** license.*

<br>

</div>
  
If you want to release a modified version of **Retroterm** which <br>
differs in functionality with the official we recommend you use <br>
a custom **ID** string, respecting the maximum `22` character <br>
length and always starting in uppercase `RT` ( **PETSCII** )

IE, the normal **ID** string is:

```
IDstring:
!text "RTRETROTERM 0.14      "
```

but the string when compiling for the **Swiftlink** cartridge is:

```
IDstring:
!text "RTRETROTERM-SL 0.14   "
```

For compatibility reasons we ask you not to modify <br>
the behavior of existing **Turbo56K** commands, but <br>
you're welcomed to add new commands.

In any case the **Turbo56K** version bytes that follow <br>
the **ID** string should remain the correct ones for the <br>
official version your modified code support.


<!----------------------------------------------------------------------------->

[MIT]: ../LICENSE
