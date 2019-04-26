.186
IDEAL
MODEL small

MACRO perm from, to, reg, map, mapLen
    local Loop
    load from, to
    xor reg, reg
    xor ax, ax
    Loop:
        mov al, [map + reg]
        dec ax
        mov [gw_bits], ax
        call get
        mov [gw_bits], reg
        call writep
        inc reg
        cmp reg, mapLen
        jl Loop
ENDM perm

MACRO load g_add, w_add
    lea bx, [g_add]
    mov [g_address], bx
    lea bx, [w_add]
    mov [w_address], bx
ENDM load

STACK 100h
DATASEG
; ------------------------------------------
; DES DATA

    
    ip db 58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8, 57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7
    ip_1 db 40, 8, 48, 16, 56, 24, 64, 32, 39, 7, 47, 15, 55, 23, 63, 31, 38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29, 36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27, 34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41, 9, 49, 17, 57, 25
    e db 32, 1, 2, 3, 4, 5, 4, 5, 6, 7, 8, 9, 8, 9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17, 16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25, 24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32, 1
    p db 16, 7, 20, 21, 29, 12, 28, 17, 1, 15, 23, 26, 5, 18, 31, 10, 2, 8, 24, 14, 32, 27, 3, 9, 19, 13, 30, 6, 22, 11, 4, 25
    PC_1 db 57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18, 10, 2, 59, 51, 43, 35, 27, 19, 11, 3, 60, 52, 44, 36, 63, 55, 47, 39, 31, 23, 15, 7, 62, 54, 46, 38, 30, 22, 14, 6, 61, 53, 45, 37, 29, 21, 13, 5, 28, 20, 12, 4
    PC_2 db 14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10, 23, 19, 12, 4, 26, 8, 16, 7, 27, 20, 13, 2, 41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48, 44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32

    
    s1 db 14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7, 0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8, 4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0, 15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13 ;|
    s2 db 15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10, 3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5, 0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15, 13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9 ;|
    s3 db 10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8, 13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1, 13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7, 1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12 ;|
    s4 db 7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15, 13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9, 10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4, 3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14 ;|
    s5 db 2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9, 14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6, 4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14, 11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3 ;|
    s6 db 12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11, 10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8, 9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6, 4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13 ;|
    s7 db 4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1, 13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6, 1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2, 6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12 ;|
    s8 db 13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7, 1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2, 7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8, 2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11 ;|
    
    SHIFT db 1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1 ;to determine whether we need to shift once or twice(round 1 = [SHIFT+0], 2 = [SHIFT+1].... round 16 = [SHIFT+15])
    ROLmap db 28, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 56, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55

    
    DES_Key db 8 dup (11h) ;0Eh, 32h, 92h, 32h, 0EAh, 6Dh, 0Dh, 73h ; 64 bit
    
    KeyTemp1 db 7 dup(?) ; allocate 56bit(7bytes) for the permuted key
    KeyTemp2 db 7 dup(?) ; allocate 56bit(7bytes) for the permuted key

    ; SubKey1 db 6 dup (?)
    ; SubKey2 db 6 dup (?)
    ; SubKey3 db 6 dup (?)
    ; SubKey4 db 6 dup (?)
    ; SubKey5 db 6 dup (?)
    ; SubKey6 db 6 dup (?)
    ; SubKey7 db 6 dup (?)
    ; SubKey8 db 6 dup (?)
    ; SubKey9 db 6 dup (?)
    ; SubKey10 db 6 dup (?)
    ; SubKey11 db 6 dup (?)
    ; SubKey12 db 6 dup (?)
    ; SubKey13 db 6 dup (?)
    ; SubKey14 db 6 dup (?)
    ; SubKey15 db 6 dup (?)
    ; SubKey16 db 6 dup (?)
    
            db '1:'
    subKey1 db 6 dup (?)
            db '2:'
    subKey2 db 6 dup (?)
            db '3:'
    subKey3 db 6 dup (?)
            db '4:'
    subKey4 db 6 dup (?)
            db '5:'
    subKey5 db 6 dup (?)
            db '6:'
    subKey6 db 6 dup (?)
            db '7:'
    subKey7 db 6 dup (?)
            db '8:'
    subKey8 db 6 dup (?)
            db '9:'
    subKey9 db 6 dup (?)
            db 'A:'
    subKey10 db 6 dup (?)
            db 'B:'
    subKey11 db 6 dup (?)
            db 'C:'
    subKey12 db 6 dup (?)
            db 'D:'
    subKey13 db 6 dup (?)
            db 'E:'
    subKey14 db 6 dup (?)
            db 'F:'
    subKey15 db 6 dup (?)
            db 'G:'
    subKey16 db 6 dup (?)


    crntKN db ?
    
    TextBlock db 8 dup ('0');(87h,87h,87h,87h,87h,87h,87h,87h);(?) ;text
    
    MapMask1 db 1h, 2h, 4h, 8h, 10h, 20h, 40h, 80h ;masks to isolate the bits(1st - 8th) - AND with the mask
    MapMask2    db 0FEh, 0FDh, 0FBh, 0F7h, 0EFh, 0DFh, 0BFh, 07Fh ;masks to clear a bit(1st - 8th) - AND with the mask
    
    gw_bit db ? ;The bit we read/need to write
    g_address dw ?
    w_address dw ?
    gw_bits dw ? ;Num of bit
    
    isEnc db ? ; 1 = Encrypt, 0 = Decrypt
    
    OutputBlock db 8 dup (?)
    
    ;------------- f function variables -------------
    e_temp db 6 dup (?)
    s_temp2 db 4 dup (?)
    f_temp db 4 dup (?)
    ;--------------------------------------------
    
    ;------------- SBox algorithm variables -------------
    s_temp db ?
    s_line db ?
    s_col db ?
    s_idx dw ?
    ;----------------------------------------------------
    ;------------- Feistel network variables -------------
    n_var db 8 dup (?)
    n_temp db 8 dup (?)
    n_round db 0
    ;-----------------------------------------------------

    
; End DES DATA
; ------------------------------------------
CODESEG
start:
    mov ax, @data
    mov ds, ax
    
    lea ax, [TextBlock]
    lea ax, [OutputBlock]
    
    mov [isEnc], 1
    call KS
    call DES
    
    mov ax, [word OutputBlock]
    mov [word TextBlock], ax
    mov ax, [word OutputBlock+2]
    mov [word TextBlock+2], ax
    mov ax, [word OutputBlock+4]
    mov [word TextBlock+4], ax
    mov ax, [word OutputBlock+6]
    mov [word TextBlock+6], ax
    
    mov [isEnc], 0
    call KS
    call DES

exit: 
    mov ax, 4c00h
    int 21h


;------------------------------------------------
;INPUT:
;   isEnc =>  1 = Encrypt, 0 = Decrypt
;OUTPUT:
;   OutputBlock => cipher text
;------------------------------------------------
proc DES
    perm TextBlock, n_var, si, ip, 64d ;Initial permutation
    call FN ;Feistel network
    perm n_var, OutputBlock, si, ip_1, 64d ;Final permutation
    ret
endp DES
    



;------------------------------------------------------------------------------------------------
;GET / WRITE BIT PROCS
;------------------------------------------------------------------------------------------------
PROC get
    push ax
    push cx
    push si
    push di
    mov ax, [gw_bits]
    mov cl, 8
    div cl
    push ax
    cbw 
    mov si, ax ;si now stores the offset in bytes
    pop ax
    shr ax, 8
    mov di, ax ;di now stores the bit offset
    mov bx, [g_address]
    mov al, [byte ptr bx + si] 
    and al, [MapMask1 + di]
    mov cx, di
    shr al, cl
    mov [gw_bit], al
    pop di
    pop si
    pop cx
    pop ax
    ret
ENDP get

PROC writep ;writes the bit which is in gw_bit in the bit position specified by w_address (address) and gw_bits (index in bits after the address).
    push ax
    push cx
    push si
    push di
    mov ax, [gw_bits]
    mov cl, 8
    div cl
    push ax
    cbw 
    mov si, ax ;si now stores the offset in bytes
    pop ax
    shr ax, 8
    mov di, ax ;di now stores the bit offset and the number of times to shift right
    mov bx, [w_address]
    mov al, [byte ptr bx + si] 
    cmp [gw_bit], 0
    je w_zero
        or al, [MapMask1 + di]
        mov [bx + si], al
        jmp w_exit      
    w_zero:
        and al, [MapMask2 + di]
        mov [bx + si], al       
    w_exit:
        pop di
        pop si
        pop cx
        pop ax
        ret
ENDP writep



;------------------------------------------------------------------------------------------------
;                               KEY SCHEDULE PROCS AND MACROS
;------------------------------------------------------------------------------------------------
PROC DRL ;DES ROL
    pusha
    perm KeyTemp1, KeyTemp2, di, ROLmap, 56d
    mov ax, [word ptr KeyTemp2 + 0]
    mov [word ptr KeyTemp1 + 0], ax
    mov ax, [word ptr KeyTemp2 + 2]
    mov [word ptr KeyTemp1 + 2], ax
    mov ax, [word ptr KeyTemp2 + 4]
    mov [word ptr KeyTemp1 + 4], ax
    mov al, [byte ptr KeyTemp2 + 6]
    mov [byte ptr KeyTemp1 + 6], al
    popa
    ret
ENDP DRL

PROC KS
        perm DES_Key, KeyTemp1, si, PC_1, 56
    xor si, si
    KLoop:
        call DRL
        cmp [SHIFT + si], 2
        jne KCont
        call DRL
    KCont:
        lea bx, [KeyTemp1]
        mov [g_address], bx
        mov ax, si
        push cx
        mov cx, 8 ;bytes size of each sub key + 2bytes
        mul cx
        pop cx
        lea bx, [SubKey1]
        add ax, bx
        mov [w_address], ax
        xor di, di
        PC2Loop:
            xor ax, ax
            mov al, [PC_2 + di]
            dec ax
            mov [gw_bits], ax
            call get
            mov [gw_bits], di
            call writep
            inc di
            cmp di, 48
            jl PC2Loop
        inc si
        cmp si, 16
        jl KLoop
    ret
ENDP KS


;------------------------------------------------------------------------------------------------
;                                       f Function Procs and Macros
;------------------------------------------------------------------------------------------------
PROC STLP ;sbox temp loop proc. Save the current 6bit block of the sbox(after expended with e and xored with s) in w_address(s_temp)
    xor di, di
    STLoop:
        mov ax, si
        mov cx, 6
        mul cl
        add ax, di
        mov [gw_bits], ax
        call get
        mov [gw_bits], di
        call writep
        inc di
        cmp di, 6
        jne STLoop
    ret
ENDP STLP

PROC SWLP ;sbox write loop proc. Save the 4bit value(output of the s-box) in memory.
    xor di, di
    SWLoop:
        mov [gw_bits], di ;bit offset
        call get
        mov ax, si
        mov cx, 4
        mul cl
        add ax, di
        mov [gw_bits], ax
        call writep
        inc di
        cmp di, 4
        jne SWLoop
    ret
ENDP SWLP
PROC s_cidx ;Get the exact offset in the SBoxes by column and row
    xor ax, ax
    mov al, [s_col]
    mov [s_idx], ax
    mov al, [s_line]
    mov cl, 10h
    mul cl
    add [s_idx], ax
    mov ax, si
    mov cx, 40h
    mul cl
    add [s_idx], ax
    ret
ENDP s_cidx
PROC f_xor ;xors the expanded r with the subkey according to crntKN and enc boolean
        cmp [isEnc], 1
        jne f_dec
        xor ax, ax
        mov al, [crntKN]
        mov cx, 8
        mul cx
        lea bx, [SubKey1]
        add bx, ax
        jmp f_cont
    f_dec:
        xor ax, ax
        mov al, [crntKN]
        mov cx, 8
        mul cx
        lea bx, [SubKey16]
        sub bx, ax
    f_cont:
        mov ax, [bx]
        xor [word ptr e_temp], ax
        mov ax, [bx + 2]
        xor [word ptr e_temp + 2], ax
        mov ax, [bx + 4]
        xor [word ptr e_temp + 4], ax
        ret
ENDP f_xor
PROC f_func ;Performs the f function to the 32 bits the address of which is in bx using the key the number of which is specified by crntKN
    pusha
    ;------------- Expansion using E and xor -------------
        mov [g_address], bx
        lea bx, [e_temp]
        mov [w_address], bx
        xor si, si
        xor ax, ax
    ELoop:
        mov al, [E + si]
        dec ax
        mov [gw_bits], ax
        call get
        mov [gw_bits], si
        call writep
        inc si
        cmp si, 48d
        jl ELoop
        call f_xor
    ;------------- sBoxes -------------
        xor si, si ;sbox iteration number
    SLoop:
        load e_temp, s_temp
        call STLP ;sbox temp loop proc
        xor ax, ax
        mov al, [s_temp]
        push ax
        and al, 00100001b
        mov cl, 10h
        div cl
        add al, ah
        cbw
        mov [s_line], al
        pop ax
        shr al, 1
        and al, 00001111b ;get the 4 middle in the 6 bits block
        mov [s_col], al
        ;now we have the line in s_line, the column in s_col and the table number is si + 1.
        call s_cidx
        mov di, [s_idx]
        mov al, [s1 + di]
        mov [s_temp], al
        load s_temp, s_temp2
        call SWLP ;sbox write loop proc
        inc si
        cmp si, 8
        jne SLoop
    ;------------- Permutation using P -------------
        load s_temp2, f_temp
        xor si, si
        xor ax, ax
    PLoop:
        mov al, [P + si]
        dec ax
        mov [gw_bits], ax
        call get
        mov [gw_bits], si
        call writep
        inc si
        cmp si, 20h ;32d
        jl PLoop
    popa
    ret
ENDP f_func
;------------------------------------------------------------------------------------------------
;                                   DES and f Function Procs
;------------------------------------------------------------------------------------------------
PROC FN
    mov [n_round], 0
    NLoop:
        mov ax, [word ptr n_var]            ;transferring from n_var to n_temp, all 8 bytes.
        mov [word ptr n_temp], ax
        mov ax, [word ptr n_var + 2]
        mov [word ptr n_temp + 2], ax
        mov ax, [word ptr n_var + 4]
        mov [word ptr n_temp + 4], ax
        mov ax, [word ptr n_var + 6]
        mov [word ptr n_temp + 6], ax
        mov ax, [word ptr n_var]            ;transferring from r in n_var to l in n_var, 4 bytes.
        mov [word ptr n_var + 4], ax
        mov ax, [word ptr n_var + 2]
        mov [word ptr n_var + 6], ax
        lea bx, [n_temp]                    ;applying the f function to the r in n_temp.
        mov al, [n_round]
        mov [crntKN], al
        call f_func
        mov ax, [word ptr f_temp]           ;still the f function.
        mov [word ptr n_temp], ax
        mov ax, [word ptr f_temp + 2]
        mov [word ptr n_temp + 2], ax
        mov ax, [word ptr n_temp]           ;xor-ing l with r in n_temp.
        xor [word ptr n_temp + 4], ax
        mov ax, [word ptr n_temp + 2]
        xor [word ptr n_temp + 6], ax
        mov ax, [word ptr n_temp + 4]       ;moving the l of n_temp to r of n_var.
        mov [word ptr n_var], ax
        mov ax, [word ptr n_temp + 6]
        mov [word ptr n_var + 2], ax
        mov al, [n_round]
        inc al
        mov [n_round], al
        cmp [n_round], 0Fh
        jl NLoop
    mov ax, [word ptr n_var]            ;transferring from n_var to n_temp, all 8 bytes.
    mov [word ptr n_temp], ax
    mov ax, [word ptr n_var + 2]
    mov [word ptr n_temp + 2], ax
    mov ax, [word ptr n_var + 4]
    mov [word ptr n_temp + 4], ax
    mov ax, [word ptr n_var + 6]
    mov [word ptr n_temp + 6], ax
    lea bx, [n_temp]                    ;applying the f function to the r in n_temp.
    mov al, [n_round]
    mov [crntKN], al
    call f_func
    mov ax, [word ptr f_temp]           ;still the f function.
    mov [word ptr n_temp], ax
    mov ax, [word ptr f_temp + 2]
    mov [word ptr n_temp + 2], ax
    mov ax, [word ptr n_temp]           ;xor-ing l with r in n_temp.
    xor [word ptr n_temp + 4], ax
    mov ax, [word ptr n_temp + 2]
    xor [word ptr n_temp + 6], ax
    mov ax, [word ptr n_temp + 4]       ;moving the l of n_temp to l of n_var.
    mov [word ptr n_var + 4], ax
    mov ax, [word ptr n_temp + 6]
    mov [word ptr n_var + 6], ax
    ret
ENDP FN
    
END start