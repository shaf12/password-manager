.486
IDEAL
MODEL small
; Macro that show one string  on screen   must be ended with $
MACRO SHOW_MSG MSG
	mov dx, offset MSG
	mov ah,9
	int 21h
ENDM 


; Macro that show new line on screen
MACRO NEW_LINE
	mov dl,13   ; CR = Caridge Return - go to row start position
	mov ah,2   
	int 21h
	mov dl,10   ;  LF = Line Feed - go down to the next line
	int 21h
ENDM 

MACRO TAB
	mov dl,9   ; Tab
	mov ah,2   
	int 21h
ENDM 
    
    
MACRO Getch
    mov ah, 1
    int 21h
ENDM Getch

MACRO PrintNull
    xor dl, dl ;null
    mov ah, 2
    int 21h
ENDM PrintNull

; 2 constants
LOG_2_BLOCK_SIZE EQU 3  ; must be 1 or more, not above 10 
BLOCK_SIZE = 8   ; must be powerd of 2  ; 2 4 8 16 32 64 etc

PASS_MAX_LEN = 8 ;Bytes
NAME_MAX_LEN = 8 ;Bytes


STACK 100h
include "DES.inc"
DATASEG

    ErrorCreate db "Could not create file$" 
    ErrorWrite db "Could not write to file$" 
    ErrorGotoEnd db "Couldn't move to the end of the file"
    
    FileName db "pass.N&C",0
    
    FileHandle dw ?
    IsNew db 0 ;was the file already created before or right now
    
    ReadBuff db BLOCK_SIZE dup(?),'$' 
    
    WriteBuff db BLOCK_SIZE dup(?),'$' 
     
    InputBuff db "XX", 20 dup(?), "X"
     
     
    FileLength dw ?
     
    ; EncBlock db "juventu"
	; OutBlock db 8 dup(?)

    NameToPass db " --> "
    NameToPassLen = 5
    CW db 10, 13, '$'
    EnterName db 10,13, "Please enter the password name(max 8 characters): ", '$'
    EnterPass db 10, 13, "Please enter the password(max 8 characters): ", '$'
    
    AnyMoreInput db 10, 13, "Do you have anything to add (Y/N)? ", '$'
    

CODESEG

; procdesc DES_ENC
; procdesc DES_DEC
; procdesc GetOutputOffsetDES
; procdesc GetInputOffsetDES

public ManageAll
PROC ManageAll
    mov al,2
    call file_open_create
    call GotoEndFile
    
    call ManageInput
    call PrintTxtDES
    
    call file_close
    ret
ENDP ManageAll


PROC PrintTxtDES
    call get_file_len
    mov si, [FileLength]
    NEW_LINE
    call print_file_content_DES
    ret
ENDP PrintTxtDES     




PROC ManageInput
@@AnyInput:
    SHOW_MSG AnyMoreInput
    
    Getch
    cmp al, 'Y'
    je @@MoreInput
    cmp al, 'y'
    je @@MoreInput
    cmp al, 'N'
    je @@NoInput
    cmp al, 'n'
    je @@NoInput
    jmp @@AnyInput
    
@@MoreInput:
    call OneInput
    jmp @@AnyInput
    
@@NoInput:
    
    ret
ENDP ManageInput

PROC OneInput                
;-----------------------------------Name-------------------------------------------------
    SHOW_MSG EnterName
    mov dl, NAME_MAX_LEN+1
    call GetInput
    
    lea dx, [InputBuff+2]
    mov ax, si
    call PutInputDES
    call DES_ENC
    call GetOutputOffsetDES
    mov dx, bx
    mov si, 8
    call write_to_file
;-----------------------------------" --> "-------------------------------------------------
    lea dx, [NameToPass]
    mov al, NameToPassLen
    call PutInputDES
    call DES_ENC
    call GetOutputOffsetDES
    mov dx, bx
    mov si, 8
    call write_to_file
;------------------------------------Password---------------------------------------------
    SHOW_MSG EnterPass
    mov dl, PASS_MAX_LEN+1
    call GetInput
    NEW_LINE
    
    lea dx, [InputBuff+2]
    mov ax, si ;for DES len
    call PutInputDES
    call DES_ENC
    call GetOutputOffsetDES
    mov dx, bx
    mov si, 8
    call write_to_file
;-----------------------------------New Line-----------------------------------------------
    lea dx, [CW]
    mov al, 2
    call PutInputDES
    call DES_ENC
    call GetOutputOffsetDES
    mov dx, bx
    mov si, 8
    call write_to_file
    
    
    ret
ENDP OneInput


;-------------------------------------------------------------
;dx = offset to input
;si = length
;copy al bytes from dx, to the DES input buffer
;-------------------------------------------------------------
PROC PutInputDES
    pusha
    push es
    
    push ds
    pop es ;for movsb
    
    mov cx, si
    mov si, dx
    call GetInputOffsetDES
    mov di, bx
    
    rep movsb
    
    pop es
    popa
    ret
ENDP PutInputDES

;INPUT --> dl = input max
;OUTPUT --> si = output len, dx = offset to output string ("...", '$')
PROC GetInput
    push dx
    push ax
    
    mov [InputBuff], dl
    lea dx, [InputBuff]
    mov ah, 0ah
    int 21h
    mov al, [byte ptr InputBuff+1]
    xor ah, ah
    mov si, ax
    
    pop ax
    pop dx
    ret
ENDP GetInput


;=================================================
;FileHandle = file handle
;
;OUTPUT - 	AX = error code if CF set
;	                DX:AX = new pointer location if CF not set
;=================================================
proc GotoEndFile
    pusha
    mov ah, 42h
    mov al, 2
    mov bx, [FileHandle]
    xor dx, dx
    xor cx, cx
    int 21h
    jnc @@Finish ;if there was no error
@@Error:
    SHOW_MSG ErrorGotoEnd
@@Finish:
    popa
    ret
endp GotoEndFile

;================================================
; Description -  
;
; INPUT:  FileName = file path
;         AL = file attribute
;         0 = Read only, 1 = write only, 2 = both
; OUTPUT:   if success FileHandle = handle, if not ax = error code and CF is on
; Register Usage: none
;================================================                 

;Same but creates a new file if current is missing
proc file_open_create
    push ax
    push dx
    
    mov DX, offset FileName
    mov ah, 03Dh
    int 21h
    jc @@error
    mov [FileHandle], ax
    jmp @@endproc
@@error:
    xor cx, cx
    mov ah, 03Ch
    int 21h
    jc @@finishError
    mov [FileHandle], ax
    mov [IsNew], 1
    jmp @@endproc
@@finishError:
    SHOW_MSG ErrorCreate
@@endproc:
    pop dx
    pop ax
    ret
endp file_open_create



              
;================================================
; Description -  Close file
;                
; INPUT: FileHandle = file handle
; OUTPUT: if error ax = error code
; Register Usage: none
;================================================                
proc file_close
    push ax
    push bx
    
    mov bx, [FileHandle]
    mov ah, 03Eh
    int 21h
    
    pop bx
    pop ax
    
    ret 
endp file_close 




              
;================================================
; Description -  Gets file len (len has to be < 65535)
;                
; INPUT: FileHandle = handle
        
; OUTPUT:  [FileLength] = file length
; Register Usage:
;================================================                
proc get_file_len
    push ax
    push bx
    push cx
    
    mov bx, [FileHandle]
    mov al, 2
    xor cx, cx
    xor dx, dx
    
    mov ah, 042h
    int 21h
    
    mov [FileLength], ax
    
    xor al, al
    xor cx, cx
    xor dx, dx
    
    mov ah, 042h
    int 21h
    
    pop ax
    pop bx
    pop cx
    
    ret
endp get_file_len


;================================================
; Description -  write to file using handle
;                
; INPUT: FileHandle = handle
;       si = bytes to write
;       dx = offset to write buffer
; OUTPUT:  -
; Register Usage: -
;================================================                
proc write_to_file
    
    ;offset should be already in dx as input
    mov bx, [FileHandle]
    mov cx, si ;number of bytes to write
    mov ah, 040h
    int 21h
    jnc @@finishProc
    NEW_LINE
    SHOW_MSG ErrorWrite
    NEW_LINE
@@finishProc:
    ret
endp write_to_file




; Proc - print content of the file from current file position
; The proc reads blocks of BLOCK_SIZE bytes each time (except the last block)
; Input - SI how many chars to try read
; Proc assumes: 1. there is array DS:ReadBuff , size : BLOCK_SIZE  
;           2. file should already be opened and this proc will not close it
;           3.  variable FileHandle (word) contains a valid handle to the opened file 
;       
proc print_file_content_DES

    cmp si,0   ; Si = File length from start
    jz @@ProcEnd
    mov cx,si
     
    shr cx, LOG_2_BLOCK_SIZE  ; divide by  Block size  due to buffer that is BLOCK_SIZE bytes
    test si, BLOCK_SIZE -1
    jz BuufSizeMatch
    inc cx          ; for partial buffer
BuufSizeMatch:

    mov bx, [FileHandle]
ReadNextBlock:
    push cx
    push bx
    
    mov dx,offset ReadBuff
    mov cx , BLOCK_SIZE  ; read blocks of BLOCK_SIZE bytes
    mov ah,3Fh
    int 21h 
    cmp ax, 0 ; ax has the actual read bytes
    jnz PrintIt
    pop cx   ; file length was too high. So no info left in the file make unexpected exit 
    jmp @@ProcEnd
    
PrintIt:
    call GetInputOffsetDES
    xor si, si
    mov cx, ax
@@lCopyInput:
    mov dl, [ReadBuff+si]
    mov [bx+si], dl
    inc si
    loop @@lCopyInput
    call DES_DEC
	
	call GetOutputOffsetDES
	xor si, si
	mov cx, ax
@@lCopyOutput:
    mov dl,[bx+si]
    mov [ReadBuff+si], dl
    inc si
    loop @@lCopyOutput

    mov si,ax
    call print_buffer
    
    ;another option to print
    ;mov  [ReadBuff+si],'$'
    ;mov ah,9
    ;int 21h
    pop bx
    pop cx
    loop ReadNextBlock

@@ProcEnd:   
    ret
endp print_file_content_DES



; print buffer to the string bt int 21h 
; input:  si - number of bytes to print to the screen
; input: ReadBuff - the data to print
proc print_buffer
    push cx
    push dx
    
    mov cx,si
    cmp cx,0
    jz @@ret
    xor si ,si
@@Next_Byte:
    mov dl,[byte ReadBuff+si]
    cmp dl, 0
    je @@null
    mov ah,2
    int 21h
    @@null:
    inc si
    loop @@Next_Byte

@@ret:
    pop dx
    pop cx

    ret
endp print_buffer


End