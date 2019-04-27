.486
IDEAL
MODEL small

macro Mod32 numH, numL ;RETURNS dx:ax % numH:numL in DX:AX
    Local LoopM
    Local FinishM
    Local Above
    push cx
    push bx
    
    mov cx, numH
    mov bx, numL
LoopM:
    cmp dx, cx
    jb FinishM
    ja Above
    cmp ax, bx
    jb FinishM ;if not below dx:ax >= numH:numL
Above:
    sub ax, bx
    sbb dx, cx ;32bit subtruction
    
    
    jmp LoopM
    
FinishM:
    
    pop bx
    pop cx
endm Mod32

DATASEG
CODESEG
