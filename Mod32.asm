IDEAL
MODEL small

macro Mod32 numH, numL ;RETURNS dx:ax % numH:numL in DX:AX
	Local LoopM
	Local FinishM
	push cx
	push bx
	
	mov cx, numH
	mov bx, numL
LoopM:
	cmp dx, cx
	jb FinishM
	cmp ax, bx
	jb FinishM ;if not below dx:ax >= numH:numL
	
	sub ax, bx
	sbb dx, cx ;32bit subtruction
	
	
	loop LoopM
	
FinishM:
	
	pop bx
	pop cx
endm Mod32

STACK 100h
DATASEG
CODESEG
start:
	mov ax, @data
	mov ds, ax

	mov dx, 16h
	mov ax, 0E360h
	Mod32 0Fh, 4240h
	
exit: 
	mov ax, 4c00h
	int 21h
END start