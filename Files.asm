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
    
     
; 2 constants
LOG_2_BLOCK_SIZE EQU 3  ; must be 1 or more, not above 10 
BLOCK_SIZE = 8   ; must be powerd of 2  ; 2 4 8 16 32 64 etc


STACK 100h
include "DES"
DATASEG

    ErrorCreate db "Could not create file$" 
    ErrorWrite db "Could not write to file$" 
         
    FileName db "Pass.txt",0
    FileHandle dw ?
    
    ReadBuff db BLOCK_SIZE dup(?),'$' 
    
    WriteBuff db BLOCK_SIZE dup(?),'$' 
     
    InputBuff db "XX", 20 dup(?), "X"
     
     
    FileLength dw ?
     
     

CODESEG
 
start:                          
    mov ax,@data             
    mov ds,ax                

    SHOW_MSG StartMsg
    NEW_LINE
    
    mov al, 2
    call file_open_create
    mov bx, [FileHandle]
    xor dx, dx
    xor cx, cx
    mov al, 2
    mov ah, 042h
    int 21h

    
    mov [InputBuff], 20 ;read max 20 bytes from user
loopy:
    mov dx, offset InputBuff
    mov ah, 0ah
    int 21h
    push dx
    push ax
    NEW_LINE
    pop ax
    pop dx
    mov al, [byte InputBuff+1]
    xor ah, ah
    mov si, ax
    
    cmp si, 3
    jne write ;Means that its not "end" because the length > 3
    cmp [byte InputBuff+2], 'e'
    jne write
    cmp [byte InputBuff+3], 'n'
    jne write
    cmp [byte InputBuff+4], 'd'
    jne write
    jmp finishMain
write:
    add dx, 2 ;skip the "XX" of the start in InputBuff
    call write_to_file
    mov dx, offset CW
    mov si, 2
    call write_to_file
    jmp loopy
    
finishMain:
    SHOW_CHAR '<'
    call get_file_len
    mov si, [FileLength]
    call print_file_content
    SHOW_CHAR '>'
    SHOW_MSG EndMsg
    NEW_LINE
    
exit:   
    mov ax,4C00h
    int 21h
                  


                  
                  
;================================================
; Description -  
;
; INPUT:  FileName = file path
;         AL = file attribute
;         0 = Read only, 1 = write only, 2 = both
; OUTPUT:   if success FileHandle = handle, if not ax = error code and CF is on
; Register Usage: none
;================================================                 
proc file_open_no_create
    push ax
    push dx
    
    mov DX, offset FileName
    mov ah, 03Dh
    int 21h
    jc @@error
    mov [FileHandle], ax
    jmp @@finish
@@error:
    SHOW_CHAR 'E'
    SHOW_CHAR 'C'
@@finish:
    pop dx
    pop ax
    
    ret
endp file_open_no_create

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
        
; OUTPUT:  
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
@@lCopy:
    mov dl, [ReadBuff+si]
    mov [bx+si], dl
    inc si
    loop @@lCopy
    call DES_DEC
    mov si,ax
    call print_buffer
    
    ;another option to print
    ;mov  [ReadBuff+si],'$'
    ;mov ah,9
    ;int 21h

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
    mov si ,0
@@Next_Byte:
    mov dl,[byte ReadBuff+si]
    mov ah,2
    int 21h
    inc si
    loop @@Next_Byte

@@ret:
    pop dx
    pop cx

    ret
endp print_buffer



;================================================
; Description - Write on screen the value of ax (decimal)
;               the practice :  
;               Divide AX by 10 and put the Mod on stack 
;               Repeat Until AX smaller than 10 then print AX (MSB) 
;               then pop from the stack all what we kept there and show it. 
; INPUT: AX
; OUTPUT: Screen 
; Register Usage: AX  
;================================================
proc ShowAxDecimal
       push ax
       push bx
       push cx
       push dx
       jmp PositiveAx
       ; check if negative
       test ax,08000h
       jz PositiveAx
            
       ;  put '-' on the screen
       push ax
       mov dl,'-'
       mov ah,2
       int 21h
       pop ax

       neg ax ; make it positive
PositiveAx:
       mov cx,0   ; will count how many time we did push 
       mov bx,10  ; the divider
   
put_mode_to_stack:
       xor dx,dx
       div bx
       add dl,30h
       ; dl is the current LSB digit 
       ; we cant push only dl so we push all dx
       push dx    
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_mode_to_stack

       cmp ax,0
       jz pop_next  ; jump if ax was totally 0
       add al,30h  
       mov dl, al    
       mov ah, 2h
       int 21h        ; show first digit MSB
           
pop_next: 
       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
       mov dl, al
       mov ah, 2h
       int 21h        ; show all rest digits
       loop pop_next
        
       mov dl, ','
       mov ah, 2h
       int 21h
   
       pop dx
       pop cx
       pop bx
       pop ax
       
       ret
endp ShowAxDecimal

End start