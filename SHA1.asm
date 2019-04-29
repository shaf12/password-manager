.486
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


include "unix.inc"
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
InputLen dw ? ;length of the input in bits
msg dw ?
CurrBlock dw ?
Blocks dw ?
msgToHash db 96 dup (?);,'E'), '$'
msgHash db OUTPUT_LEN/8 dup(0), '$'
WorkBuff db 64 dup (?), 256 dup(?)
BLOCK_SIZE = 64 ;bytes

;-------------------HMAC KEY--------------------------------------
;Key db KEY_LEN dup (?)
Key db 30h, 30h, 30h, 30h, 30h, 30h, 30h, 30h, 30h, 30h
KeyPad db BLOCK_SIZE dup(?) ;MAX PAD POSSIBLE
HKeyLen db ? ;bytes
oKeyPad db BLOCK_SIZE dup (?)
iKeyPad db BLOCK_SIZE dup (?)
oPadXor db BLOCK_SIZE dup (5Ch)
iPadXor db BLOCK_SIZE dup (36h)
HmacMsgOffset dw ? ;offset to msg
HmacMsg db 8 dup (?)
HmacMsgLen db ? ;in bytes!!!
CODESEG

; start:
    ; mov ax, @data
    ; mov ds, ax
    
    ; ; call EpochTimeDiv30
    
    ; ; lea bx, [Key]
    ; ; lea bx, [HmacMsgOffset]
    ; ; lea bx, [HKeyLen]
    
    ; ; mov [HKeyLen], 10
    ; ; mov [HmacMsgLen], 4
    ; ; lea bx, [HmacMsg]
    ; ; mov [HmacMsgOffset], bx
    ; ; call HMAC_SHA1
    ; ; lea dx, [msgHash]

    ; call GoogleAuthenticator ;num is in dx:ax
    ; shl edx, 16
    ; mov dx, ax
    ; Print32 edx
    
; exit: 
    ; mov ax, 4c00h
    ; int 21h

;---------------------------------------------------------------------------------------
;Input: Key = secret 10bytes key
;
;
;
;---------------------------------------------------------------------------------------
public GoogleAuthenticator
proc GoogleAuthenticator
    push cx
    push bx
    push si
    call EpochTimeDiv30
    xchg dh, dl ;we need big endian
    xchg ah, al
    mov [word HmacMsg], 0
    mov [word HmacMsg+2], 0
    mov [word HmacMsg+4], dx
    mov [word HmacMsg+6], ax
    mov [HmacMsgLen], 8
    mov [HKeyLen], 10
    lea bx, [HmacMsg]
    mov [HmacMsgOffset], bx
    call HMAC_SHA1 ;Now the result is in msgHash (result is big-endian)
    mov al, [msgHash+19] ;
    and al, 0Fh ;we need only the last nibble
    xor ah, ah
    mov si, ax
    mov dx, [word msgHash+si]
    mov ax, [word msgHash+si+2]
    xchg dh, dl ;back to big endian
    xchg ah, al ;back to big endian
    
    and dh, 7Fh ;removing the most significant bit(MSB)
    
    Mod32 0Fh, 4240h
    
    pop si
    pop bx
    pop cx
    ret
endp GoogleAuthenticator    
    
    
    
;------------------------------------------------------------------------
;Key length must be less than BLOCK_SIZE(64bytes for SHA-1)
;HmacMsgOffset = offset to the message
;HmacMsgLen = length of the message
;BLOCK_SIZE = 64
;Key = key
;KEY_LEN = key length
;oKeyPad db 64 dup (?)
;iKeyPad db 64 dup (?)
;------------------------------------------------------------------------
proc HMAC_SHA1
    ; cmp KEY_LEN, BLOCK_SIZE
    ; jna @@cont1
    ; xor si, si
    ; mov cx, KEY_LEN
; @@CopyFromKey:
    ; mov ax, [Key+si]
    ; mov [msg+si], ax
    ; inc si
    ; loop @@CopyFromKey
    
    ; SHA1_Hash

    ; mov cx,
; @@CopyToKey:

;@@cont1:
    cmp [HKeyLen], BLOCK_SIZE
    jnb @@cont
    xor si, si
    mov cl, BLOCK_SIZE
    sub cl, [HKeyLen]
    xor ch, ch
@@PadZeros:
    mov [KeyPad+si], 0
    inc si
    loop @@PadZeros
    
@@cont:
    xor si, si
    mov cx, 64
@@HmacCopyKey:
    mov al, [Key+si]
    xor al, 5ch
    mov [oKeyPad+si], al
    inc si
    loop @@HmacCopyKey
    xor si, si
    mov cx, 64
@@CopyFromKey:
    mov al, [Key+si]
    xor al, 36h
    mov [iKeyPad+si], al
    inc si
    loop @@CopyFromKey

    mov bl, [HKeyLen]
    xor bh, bh
    mov si, bx

    ;--------------------------------------------------------------------
    ;Preparing SHA1_Hash
    ;--------------------------------------------------------------------
    xor si, si
    mov cx, 32 ;32 words = 64 bytes
@@CopyI:
    mov ax, [word ptr iKeyPad+si]
    mov [word msgToHash+si], ax
    add si, 2
    loop @@CopyI
    
    mov bx, [HmacMsgOffset] ;the offset of the message is in HmacMsgOffset
    mov cx, [word HmacMsgLen] ;length in bytes(!)
@@CopyMsg:
    mov al, [bx]
    mov [msgToHash+si], al
    inc si
    inc bx
    loop @@CopyMsg
    
    lea bx, [msgToHash]
    mov [msg], bx
    
    mov ax, [word HmacMsgLen]
    add ax, 64 ;64byte of iKeyPad
    shl ax, 3 ;mul by 8 to convert length to bits
    mov [InputLen], ax
    
    call SHA1_Hash
    
    xor si, si
    mov cx, 32 ;32words = 64bytes
@@CopyO:
    mov ax, [word ptr oKeyPad+si]
    mov [word msgToHash+si], ax
    add si, 2
    loop @@CopyO
    
    lea bx, [msgHash]
    mov cx, 10
@@CopyFirstHash:
    mov ax, [word bx]
    mov [word msgToHash+si], ax
    add si, 2
    add bx, 2
    loop @@CopyFirstHash
    
    mov [InputLen], 84*8 ;84bytes in bits
    
    lea bx, [msgToHash]
    mov [msg], bx
    
    call SHA1_Hash
    
    ret
endp HMAC_SHA1
    
    


    
    
    
;-------------------------------------------------------------------------
;Input: OUTPUT_LEN = 160, 
;           InputLen = Input msg len in bits!!!
;           msg = offset to the msg to hash, 
;           msgHash OUTPUT_LEN/8 dup
;           WorkBuff array length = 320 bytes
;
;Output: hashed msg in msgHash
;-------------------------------------------------------------------------
proc SHA1_Hash
    cmp [InputLen], 0
    jng @@NoMoreBlocks;not greater

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
    
    mov [CurrBlock], 0
    mov ax, [InputLen]
    add ax, 7 ;appended byte(80h) --> see @@AppendOne
    ;We added 7 instead of 8 so -1 is uneeded ;dec ax ;(n-1)/k+1 --> formula to get the amount of needed blocks withous if's
    mov bx, 512 ;bits per block
    xor dx, dx
    div bx ;/k
    inc ax ;+1
    mov [Blocks], ax
    jmp @@StartBlock
    ;------------------------END PREPERATION------------------------------------
@@LastBlock:
    xor si, si
    mov bx, [CurrBlock]
    shl bx, 6
    add bx, [msg] ;in msg there is the offset of the msg to hash
    
    mov cx, [word InputLen]
    shr cx, 3
    mov ax, cx
    shr ax, 6 ;div by 64
    shl ax, 6
    sub cx, ax ;to get the remidner(last chunk length is the modulo of the full length with 64(bytes)) --> how many bytes are left to read
    push cx ;save the reminder for later
    cmp cx, 0
    je @@CxIsZero
@@lCopyToOutput: ;Copy the input to workbuff
    mov dl, [bx]
    mov [WorkBuff+si], dl
    inc bx
    inc si
    loop @@lCopyToOutput

@@CxIsZero:
    pop si ;get the reminder back (after push cx)

@@AppendOne:
    mov [WorkBuff+si], 080h ;append 1 after the msg
    

    inc si ;starting from si+1 because we've already appended 1 bytes (@@AppendOne)
    mov ax, [word ptr InputLen]  
    add ax, 8 ;we have already appended 1byte
    xor dx, dx
    mov bx, 512
    div bx
    mov bx, 448
    
    sub bx, dx ;to get how much bits we need to add in order to get len%512=448
    shr bx, 1
    shr bx, 1
    shr bx, 1
    mov cx, bx ;the amout of bytes needed to append in order to complete to len%512 = 448 bit - 1byte because we've already appended a byte in @@AppendOne
@@lAppendZeros:
    mov [WorkBuff + si], 0 ;INPUT_LEN/8 because we are starting appending 0's after the input(and the 1 appended)
    inc si
    loop @@lAppendZeros
    
    
    xor si, si
@@lAppendLen:
    mov [word ptr WorkBuff+si+56], 0
    add si, 2
    cmp si, 6
    jne @@lAppendLen ;append len int 64-bit Big-Endian. Since len<=2^16-1 the first 3 words are 0
    mov ax, [word ptr InputLen]
    
    mov [WorkBuff+62], ah
    mov [WorkBuff+63], al
    jmp @@ExtendTo80Words
;------------------------END LAST BLOCK--------------------------------------
    
@@StartBlock:
    mov ax, [CurrBlock]
    cmp ax, [Blocks] ;is another block
    jnb @@NoMoreBlocks ;no
    inc ax
    cmp ax, [Blocks] ;if there is no next block
    je @@LastBlock ;if this is the last block we need to append 1 and than append the zeros
    
    xor si, si
    mov bx, [CurrBlock]
    shl bx, 6
    add bx, [msg] ;in msg there is the offset of the msg to hash
    
    mov cx, 64 ;since it's not the last block the chunk size is 64bytes
@@CopyNotLastChunk: ;Copy the input to workbuff
    mov dl, [bx]
    mov [WorkBuff+si], dl
    inc bx
    inc si
    loop @@CopyNotLastChunk

    
    
@@ExtendTo80Words: ;extend the msg(16 dwords = 32 words = 64 bytes = 512 bits) to 80 dwords(adding 56 dwords)
    mov si, 64 ;16 need to be multiplied by 4 (4bytes = dword) in order to get the offset of WorkBuff
@@lExtend:
    ; push si ;save si value
    ; shl si, 1 ;2 shl = mul by 4
    ; shl si, 1 ;^^^^
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
    xor ax, bx ;now dx:ax is    WorkBuff[i-3] xor WorkBuff[i-8] xor WorkBuff[i-14]
    
    mov cx, [word ptr WorkBuff+si-64] ;WorkBuff[i-16](multiplying the 16 by 4 because 14 is for dwords)
    mov bx, [word ptr WorkBuff+si-62] ;second word
    xchg ch, cl
    xchg bh, bl
    
    xor dx, cx
    xor ax, bx ;now dx:ax is    WorkBuff[i-3] xor WorkBuff[i-8] xor WorkBuff[i-14] xor WorkBuff[i-16]

    ROL32
    xchg dl, dh
    xchg al, ah
    mov [word ptr WorkBuff+si], dx
    mov [word ptr WorkBuff+si+2], ax
    
    ; pop si
    ; inc si
    add si, 4
    cmp si, 320
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
    inc [CurrBlock] ;next block
    jmp @@StartBlock
@@NoMoreBlocks:
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
    
    
    
    
    
    
    
    
    
    
    
END