.486
%TITLE "Passwords Maneger"
IDEAL
MODEL small
macro Print32 number
local println, loopr

    push eax
    push edx
    push ebx
    push si
    mov cx,0
    mov eax,number
    mov ebx, 10
 loopr:  
    xor edx, edx
    div ebx         ; eax <- eax/10, edx <- eax % 10
    add dl, '0'     ; edx to ASCII
    push dx
    inc cx
    cmp eax, 0
    jnz loopr

    println:
    pop dx
    mov ah, 2h
    int 21h
    dec cx
    jnz println

    mov dl, 13d     ;Carriage Return
    mov ah, 2h
    int 21h
    mov dl, 10d     ;Line Feed
    mov ah, 2h
    int 21h

    pop si
    pop edx
    pop eax
endm Print32

MACRO SHOW_MSG MSG
	mov dx, offset MSG
	mov ah,9
	int 21h
ENDM SHOW_MSG

MACRO Getch
    mov ah, 1
    int 21h
ENDM Getch

MACRO GetDigit
    Getch
    sub al, '0' ;30h - to convert from ascii to digit
ENDM GetDigit

MACRO WaitKey
    mov ah, 7
    int 21h
ENDM WaitKey

MACRO NEW_LINE
	mov dl,13   ; CR = Caridge Return - go to row start position
	mov ah,2   
	int 21h
	mov dl,10   ;  LF = Line Feed - go down to the next line
	int 21h
ENDM NEW_LINE

MACRO ClearScr
    xor ah, ah
    mov al, 3
    int 10h
ENDM ClearScr


STACK 100h
include "Main.inc"
DATASEG

Base32EnterStart db "Please put ", '$'
Base32EnterCont db " as the secret key in your TOTP app", 10, 13, '$'
EnterCode db "Please enter the 6-digit code that you got from the TOTP app: ", '$'
WrongCode db "Wrong code, please try again ", 10, 13, '$'
SuccessMSG db "Congrats, you're in", 10, 13, '$'

PressToContMSG db "Press any key to continue . . . ", '$'

CODESEG
start:
	mov ax, @data
	mov ds, ax
	
    call Main
    
	
EXIT:
	mov ax, 4c00h
	int 21h
	
;---------------------------
; Procudures area
;---------------------------
proc Main
    call MainTOTP
    call ManageAll
    ret
endp Main



proc MainTOTP
@@ProcStart:
    ClearScr
    
    SHOW_MSG Base32EnterStart
    call PrintBase32
    SHOW_MSG Base32EnterCont

    SHOW_MSG PressToContMSG
    WaitKey
    ClearScr
    
    SHOW_MSG EnterCode
    call GetSixDigitCode
    
    call GoogleAuthenticator
    cmp cx, dx
    jne @@TryAgain

@@CheckLowOrder:
    cmp bx, ax
    jne @@TryAgain
    
    ;if we got here the code is correct
    NEW_LINE
    SHOW_MSG SuccessMSG
    
    
    ret
    
@@TryAgain:
    NEW_LINE
    SHOW_MSG WrongCode ;jne --> wrong
    SHOW_MSG PressToContMSG
    WaitKey
    jmp @@ProcStart

endp MainTOTP

proc GetSixDigitCode ;Gets the 6 digit code from the user. Output goes to cx:bx
    xor cx, cx
    xor bx, bx
    
    mov ax, 50000
    mov si, 5
@@GetLoop:
    push ax
    GetDigit
    xor ah, ah
    mov di, ax
    pop ax
    push ax
    mul di ; dx:ax = 5* (10^n) * digit
    shl ax, 1 
    rcl dx, 1 ;32bit shift left (to multiply by 2) --> x*5*2 = x*10
    
    add bx, ax
    adc cx, dx 
    pop ax
        
    xor dx, dx
    mov di, 10
    div di ;div ax by 10
    dec si
    jnz @@GetLoop
    
    GetDigit
    xor ah, ah
    add bx, ax
    adc cx, 0
    ret
endp GetSixDigitCode

END start