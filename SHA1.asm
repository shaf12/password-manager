IDEAL
MODEL small

;DX:AX left shift (32 bit)
MACRO ROL32
	shl dx, 1
	rcl ax, 1
	adc dx, 0

	; push bx
	; xor bx, bx
	; shl dx, 1
	; adc bl, 0
	; shl ax, 1
	; pushf
	; or ax, bx
	; xor bl, bl
	; popf
	; adc bl, 0
	; or dx, bx
	; pop bx
ENDM
;DX:AX right shift (32 bit)
MACRO ROR32
	push bx
	push cx
	
	xor bx, bx
	shr ax, 1
	rcr dx, 1
	adc bl, 0
	mov cl, 7
	shl bl, cl
	add ah, bl
	
	pop cx
	pop bx

	; push bx
	; push cx
	; xor bx, bx
	; shr dx, 1
	; adc bl, 0
	; mov cl, 7
	; shl bl, cl
	; shr ax, 1
	; pushf
	; or ah, bl
	; xor bl, bl
	; popf
	; adc bl, 0
	; mov cl, 7
	; shl bl, cl
	; or dh, bl
	; pop cx
	; pop bx
ENDM

STACK 100h
DATASEG

h0 dd 0
h1 dd 0
h2 dd 0
h3 dd 0
h4 dd 0
aVar dd 0
bVar dd 0
cVar dd 0
dVar dd 0
eVar dd 0
fVar dd 0
kVar dd 0
temp dd 0

OUTPUT_LEN = 160 ;bits
INPUT_LEN = 80
msg db 2 dup ('A','B','C','D','E'), '$'
msgHash db OUTPUT_LEN/8 dup(0), '$'
WorkBuff db 64 dup (0), 256 dup(0)
TO_448 = 448 - INPUT_LEN
CODESEG

start:
	mov ax, @data
	mov ds, ax
	
	call SHA1_Hash
	lea bx, [WorkBuff]
	lea dx, [msgHash]
	mov ah, 9
	int 21h

	
	
exit: 
	mov ax, 4c00h
	int 21h
;---------------------------------------------------------------
;Input: OUTPUT_LEN = 160, INPUT_LEN = Input msg len
;			msg = msg to hash, msg_hash OUTPUT_LEN dup
;			WorkBuff array length = 512
;			len <= 440 bit
;Output: hashed msg in msg_hash
;---------------------------------------------------------------
proc SHA1_Hash
	mov [word ptr h0], 2301h
	mov [word ptr h0+2], 6745h
	
	mov [word ptr h1], 0AB89h
	mov [word ptr h1+2], 0EFCDh
	
	mov [word ptr h2], 0DCFEh
	mov [word ptr h2+2], 98BAh
	
	mov [word ptr h3], 5476h
	mov [word ptr h3+2], 1032h
	
	mov [word ptr h4], 0E1F0h
	mov [word ptr h4+2], 0C3D2h
	

	xor si, si
	mov cx, INPUT_LEN/8
@@lCopyToOutput: ;Copy the input to the working buffer
	mov dl, [msg+si]
	mov [WorkBuff+si], dl
	inc si
	loop @@lCopyToOutput
@@AppendOne:
	mov [WorkBuff+si], 080h ;append 1 after the msg
	
	
	mov si, 1 ;starting from 2 because we've already appended 2 bytes (@@AppendOne)
	mov cx, TO_448/8 - 1 ;the amout of bytes needed to append in order to complete to 448 bit - 1 because we've already appended a byte in @@AppendOne
@@lAppendZeros:
	mov [WorkBuff + si + INPUT_LEN/8], 0 ; INPUT_LEN/8 because we are starting appending 0's after the input(and the 1 appended)
	inc si
	loop @@lAppendZeros
	
	
	xor si, si
@@lAppendLen:
	mov [word ptr WorkBuff+si+56], 0
	add si, 2
	cmp si, 6
	jne @@lAppendLen ;append len int 64-bit Big-Endian. Since len<=2^16-1 the higher 3 words are 0
	mov ax, INPUT_LEN
	
	mov [WorkBuff+62], ah
	mov [WorkBuff+63], al
	
	
	
@@ExtendTo80Words: ;extend the msg(16 dwords = 32 words = 64 bytes = 512 bits) to 80 dwords(adding 56 dwords)
	mov si, 16 ;need to be multiplied by 4 (4bytes = dword) in order to get the offset of WorkBuff
@@lExtend:
	push si ;save si value
	shl si, 1 ;2 shl = mul by 4
	shl si, 1 ;^^^^
	;***Working like in Big-Endian
	mov dx, [word ptr WorkBuff+si-12]
	mov ax, [word ptr WorkBuff+si-10]
	xchg dh, dl
	xchg ah, al
	
	mov cx, [word ptr WorkBuff+si-32]
	mov bx, [word ptr WorkBuff+si-30]
	xchg ch, cl
	xchg bh, bl
	
	xor dx, cx
	xor ax, bx
	
	mov cx, [word ptr WorkBuff+si-56] ;WorkBuff[i-14](multiplying the 14 by 4 because 14 is for dwords)
	mov bx, [word ptr WorkBuff+si-54] ;second word
	xchg ch, cl
	xchg bh, bl
	
	xor dx, cx
	xor ax, bx ;now dx:ax is	WorkBuff[i-3] xor WorkBuff[i-8] xor WorkBuff[i-14]
	
	mov cx, [word ptr WorkBuff+si-64] ;WorkBuff[i-16](multiplying the 16 by 4 because 14 is for dwords)
	mov bx, [word ptr WorkBuff+si-62] ;second word
	xchg ch, cl
	xchg bh, bl
	
	xor dx, cx
	xor ax, bx ;now dx:ax is	WorkBuff[i-3] xor WorkBuff[i-8] xor WorkBuff[i-14] xor WorkBuff[i-16]

	ROL32
	xchg dl, dh
	xchg al, ah
	mov [word ptr WorkBuff+si], dx
	mov [word ptr WorkBuff+si+2], ax
	
	pop si
	inc si
	cmp si, 80
	jne @@lExtend
	
	;initializing a,b,c,d,e
	mov dx, [word ptr h0]
	mov [word ptr aVar], dx
	mov dx, [word ptr h0+2]
	mov [word ptr aVar+2], dx
	
	mov dx, [word ptr h1]
	mov [word ptr bVar], dx
	mov dx, [word ptr h1+2]
	mov [word ptr bVar+2], dx
	
	mov dx, [word ptr h2]
	mov [word ptr cVar], dx
	mov dx, [word ptr h2+2]
	mov [word ptr cVar+2], dx
	
	mov dx, [word ptr h3]
	mov [word ptr dVar], dx
	mov dx, [word ptr h3+2]
	mov [word ptr dVar+2], dx
	
	mov dx, [word ptr h4]
	mov [word ptr eVar], dx
	mov dx, [word ptr h4+2]
	mov [word ptr eVar+2], dx
	
@@MainLoop:	
	xor si, si
@@lMain: ;iterate through all the 80 dwords(0-79)
	push si
	shl si, 1 ;2 shl = mul by 4
	shl si, 1 ;^^^^
@@if1:
	cmp si, 76 ;if 0 ≤ i ≤ 19 then
	ja @@if2
	mov dx, [word ptr bVar+2]
	mov ax, [word ptr bVar]
	mov cx,[word ptr cVar+2]
	mov bx,[word ptr cVar]
	and cx, dx ;b and c
	and bx, ax ;^^^^^
	push cx
	push bx
	
	not dx
	not ax
	mov cx,[word ptr dVar+2]
	mov bx,[word ptr dVar]
	and dx, cx ;(not b) and d
	and ax, bx ;^^^^
	
	pop bx
	pop cx
	or dx, cx ;(b and c) or ((not b) and d)
	or ax, bx ;^^^^^
	
	mov [word ptr fVar+2], dx
	mov [word ptr fVar], ax
	;0x5A827999
	mov [word ptr kVar+2], 5A82h ;5A82h
	mov [word ptr kVar], 7999h ;7999h
	jmp @@PutValues
@@if2:
	cmp si, 156 ;else if 20 ≤ i ≤ 39
	ja @@if3
	mov dx, [word ptr bVar+2]
	mov ax, [word ptr bVar]
	mov cx, [word ptr cVar+2]
	mov bx, [word ptr cVar]
	xor dx, cx ;b xor c
	xor ax, bx ;^^^
	
	mov cx, [word ptr dVar+2]
	mov bx, [word ptr dVar]
	xor dx, cx ;b xor c xor d
	xor ax, bx ;^^^^^^
	
	mov [word ptr fVar+2], dx
	mov [word ptr fVar], ax
	;0x6ED9EBA1
	mov [word ptr kVar+2], 6ED9h ;6ED9h
	mov [word ptr kVar], 0EBA1h ;0EBA1h
	jmp @@PutValues
@@if3:
	cmp si, 236 ;else if 40 ≤ i ≤ 59
	ja @@if4
	mov dx, [word ptr bVar+2]
	mov ax, [word ptr bVar]
	mov cx,[word ptr cVar+2]
	mov bx,[word ptr cVar]
	and cx, dx ;b and c
	and bx, ax ;^^^^^
	push cx
	push bx
	
	mov cx, [word ptr dVar+2]
	mov bx, [word ptr dVar]
	and dx, cx ;b and d
	and ax, bx ;^^^^^
	pop bx
	pop cx
	or dx, cx
	or ax, bx
	push dx
	push ax
	
	mov dx, [word ptr cVar+2]
	mov ax, [word ptr cVar]
	mov cx, [word ptr dVar+2]
	mov bx, [word ptr dVar]
	and cx, dx ;(c and d) 
	and bx, ax ;^^^^^
	
	pop ax
	pop dx
	or dx, cx ; (b and c) or (b and d) or (c and d) 
	or ax, bx ; ^^^^^^^^^^^^^^^^^^^^
	mov [word ptr fVar+2], dx
	mov [word ptr fVar], ax
	
	;0x8F1BBCDC
	mov [word ptr kVar+2], 8F1Bh ;8F1Bh
	mov [word ptr kVar], 0BCDCh ;0BCDCh
	
	jmp @@PutValues
;--------------------------------Mid jmp 2
@@ContinueJmpMid2:
	jmp @@lMain
;--------------------------------Mid jmp 2
@@if4: ;else if 60 ≤ i ≤ 79
	mov dx, [word ptr bVar+2]
	mov ax, [word ptr bVar]
	mov cx, [word ptr cVar+2]
	mov bx, [word ptr cVar]
	xor dx, cx ; b xor c
	xor ax, bx ; ^^^^^^
	mov cx, [word ptr dVar+2]
	mov bx, [word ptr dVar]
	xor dx, cx ; b xor c xor dVar
	xor ax, bx ; ^^^^^^^^^^^^
	
	mov [word ptr fVar+2], dx
	mov [word ptr fVar], ax
	
	;0xCA62C1D6
	mov [word ptr kVar+2], 0CA62h ;0CA62h
	mov [word ptr kVar], 0C1D6h ;0C1D6h
	
	jmp @@PutValues
@@PutValues:
	; temp = (a leftrotate 5) + f + e + k + w[i]
	mov dx, [word ptr aVar+2]
	mov ax, [word ptr aVar]
	mov cx, 5
@@lRol5:
	ROL32
	loop @@lRol5
	;(a leftrotate 5) + f
	mov cx, [word ptr fVar+2]
	mov bx, [word ptr fVar]
	add ax, bx
	adc dx, cx

	;+e
	mov cx, [word ptr eVar+2]
	mov bx, [word ptr eVar]
	add ax, bx
	adc dx, cx

	;+k
	mov cx, [word ptr kVar+2]
	mov bx, [word ptr kVar]
	add ax, bx
	adc dx, cx
	
	;+w[i](big endian!!!)
	mov cx, [word ptr WorkBuff+si]
	xchg ch, cl
	mov bx, [word ptr WorkBuff+si+2]
	xchg bh, bl
	add ax, bx
	adc dx, cx

	;temp = ^^^^ (dx:ax)
	mov [word ptr temp+2], dx
	mov [word ptr temp], ax
	;------------------------------------
	;e = d
	mov dx, [word ptr dVar+2]
	mov ax, [word ptr dVar]
	mov [word ptr eVar+2], dx
	mov [word ptr eVar], ax
	
	jmp @@D_Equ_C
	;-----------------------------Mid jmp
@@ContinueJmpMid1:
	jmp @@ContinueJmpMid2
	;-----------------------------Mid jmp
@@D_Equ_C:
	;------------------------------------
	;d = c
	mov dx, [word ptr cVar+2]
	mov ax, [word ptr cVar]
 	mov [word ptr dVar+2], dx
	mov [word ptr dVar], ax
	;------------------------------------
	;c = b leftrotate 30
	mov dx, [word ptr bVar+2]
	mov ax, [word ptr bVar]
	mov cx, 2
@@lROL30: ;ROL 30 = ROR 2
	ROR32
	loop @@lROL30
	mov [word ptr cVar+2], dx
	mov [word ptr cVar], ax	
	;------------------------------------
	;b = a
	mov dx, [word ptr aVar+2]
	mov ax, [word ptr aVar]
 	mov [word ptr bVar+2], dx
	mov [word ptr bVar], ax
	;------------------------------------
	;a = temp
	mov dx, [word ptr temp+2]
	mov ax, [word ptr temp]
	mov [word ptr aVar+2], dx
	mov [word ptr aVar], ax
	;------------------------------------
	pop si
	inc si
	cmp si, 80
	jne @@ContinueJmpMid1
	 
@@AddToGetHash:
	;Add this chunk's hash to result so far:
    ;h0 = h0 + a
    ;h1 = h1 + b 
    ;h2 = h2 + c
    ;h3 = h3 + d
    ;h4 = h4 + e
	;--------------------
	;h0 = h0 + a
	mov dx, [word ptr aVar+2]
	mov ax, [word ptr aVar]
	mov cx, [word ptr h0+2]
	mov bx, [word ptr h0]
	add ax, bx
	adc dx, cx

	mov [word ptr h0+2], dx
	mov [word ptr h0], ax
	;--------------------
    ;h1 = h1 + b
	mov dx, [word ptr bVar+2]
	mov ax, [word ptr bVar]
	mov cx, [word ptr h1+2]
	mov bx, [word ptr h1]
	add ax, bx
	adc dx, cx

	mov [word ptr h1+2], dx
	mov [word ptr h1], ax
	;--------------------
    ;h2 = h2 + c
	mov dx, [word ptr cVar+2]
	mov ax, [word ptr cVar]
	mov cx, [word ptr h2+2]
	mov bx, [word ptr h2]
	add ax, bx
	adc dx, cx

	mov [word ptr h2+2], dx
	mov [word ptr h2], ax
	;--------------------
	;h3 = h3 + d
	mov dx, [word ptr dVar+2]
	mov ax, [word ptr dVar]
	mov cx, [word ptr h3+2]
	mov bx, [word ptr h3]
	add ax, bx
	adc dx, cx

	mov [word ptr h3+2], dx
	mov [word ptr h3], ax
	;--------------------
    ;h4 = h4 + e
	mov dx, [word ptr eVar+2]
	mov ax, [word ptr eVar]
	mov cx, [word ptr h4+2]
	mov bx, [word ptr h4]
	add ax, bx
	adc dx, cx

	mov [word ptr h4+2], dx
	mov [word ptr h4], ax
	;--------------------
	;Produce hashed value
	;h0
	mov dx, [word ptr h0+2]
	mov ax, [word ptr h0]
	xchg dh, dl ; to big endian
	xchg ah, al ; ^^^^^^^^^
	mov [word ptr msgHash], dx
	mov [word ptr msgHash+2], ax
	;h1
	mov dx, [word ptr h1+2]
	mov ax, [word ptr h1]
	xchg dh, dl ; to big endian
	xchg ah, al ; ^^^^^^^^^
	mov [word ptr msgHash+4], dx
	mov [word ptr msgHash+6], ax
	;h2
	mov dx, [word ptr h2+2]
	mov ax, [word ptr h2]
	xchg dh, dl ; to big endian
	xchg ah, al ; ^^^^^^^^^
	mov [word ptr msgHash+8], dx
	mov [word ptr msgHash+10], ax
	;h3
	mov dx, [word ptr h3+2]
	mov ax, [word ptr h3]
	xchg dh, dl ; to big endian
	xchg ah, al ; ^^^^^^^^^
	mov [word ptr msgHash+12], dx
	mov [word ptr msgHash+14], ax
	;h4
	mov dx, [word ptr h4+2]
	mov ax, [word ptr h4]
	xchg dh, dl ; to big endian
	xchg ah, al ; ^^^^^^^^^
	mov [word ptr msgHash+16], dx
	mov [word ptr msgHash+18], ax

	
	ret
endp SHA1_Hash
	
	
	
	
	
	
	
	
	
	
	
END start