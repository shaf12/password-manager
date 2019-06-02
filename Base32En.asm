.486
IDEAL
MODEL small
DATASEG

;
BASE32_TABLE db "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
SecretBit db "0000000000",'$';2 dup('A', 'B', 'C', 'D', 'E'),'$' ; 5 characters(5 bytes ascii) * 2(2 dup) = 10 bytes = 80 bits(needed secret size before base32 encoding)
INPUT_LEN = SIZE SecretBit

SecretEnc db INPUT_LEN*8/5 dup('$'),'$' ; output code buffer
;

CODESEG

;---------------------------------------------------------------
;Prints Base32 encoded SecretBit
;---------------------------------------------------------------
public PrintBase32
proc PrintBase32

    call Base32Enc
    
    lea dx, [SecretEnc]
	mov ah, 9
	int 21h

    ret
endp PrintBase32
    
;----------------------------------------------------
;SecretBit = secret code
;INPUT_LEN = SIZE SecretBit
; 
;Output code - SecretEnc
;
;***Important - Input length in bytes has to be a multiply of 5
;----------------------------------------------------
proc Base32Enc
    InByteSize = 8
    OutByteSize = 5

	xor dh, dh
	xor bx, bx
	mov cl, 8 ;Initialize 8 left bits at start(on start we have all the byte left)
	mov ch, 1 ;On start we read 1 byte (this is the bytes that have already been read counter )
	xor si, si ;In SecretBit pointer
	mov bh, [SecretBit] ;put in bh the first byte
	
	
@@ToBase32:

@@DelScannedBits:
	mov dl, cl ;Save the left bytes from the last "round"
	mov cl, 8
	sub cl, dl ;In order to know how many shifts we need to do to pop out the scanned bits we need to sub the left we have to scan from 8
	shl bx, cl ;Pop out the scanned bit
	shr bx, cl ;Reset the position back but now we won't have the scanned bits
	mov cl, dl ;Put the left bits count back to cl as we finished poping out the scanned bits
	
@@ShiftTo5Bits:
	add cl, 3 ;Increase the left bits count by 3 to get only five bytes and not 8(we want to transfer the 5 most significant bits in bx to the start of bl)
	shr bx, cl ;Shift in the way explained ^^^
	push bx
	xor bh, bh
	mov bl, [BASE32_TABLE+bx] ;Replace the value according to the base32 values table(0 = A, 1 = B ...)
	pop ax
	mov bh, ah
@@AddCharToEnc:
	push dx
	xor dl, dl
	xchg dl, dh
	mov di, dx
	mov [SecretEnc+di], bl
	pop dx
	inc dh
	
@@CheckShouldAllocate:
	;In order to know how many bits are stil "scannable", we need to
	;calculate the modulo of the count of bits that we read from the input/the prosseced bits(dh)
	mov al, 5
	mul dh ;counter of scanned 5 bits
	push bx ;save bh,bl
	mov bx, ax ;save the result
	
	mov al, 8
	mul ch
	
	div bl ;divide ax (amount of bytes read from input*8 = bits) by the amount of scanned bits
	mov cl, ah ;modulo --> the amount of bits that left from the last read byte
	
	pop bx

@@CheckEndInput:
	cmp cl, 0
	jne @@CheckProcessLeft ;if left != 0 (is bigger), means that we have left bits to process
	cmp ch, INPUT_LEN ;Check if end of the input was reached
	je @@FinalBase32
	
@@CheckProcessLeft:
	cmp cl, 0
	ja @@CheckGetFromInput
	inc si
	mov bh, [SecretBit+si]
	inc ch ;counts the bytes we have already read from input
	mov cl, 8 ;as we have 0 left from last byte(8-left = the amount of shifts on start and we need 8 shifts)
	jmp @@ToBase32
	
@@CheckGetFromInput:
	mov bh, [SecretBit+si]
	cmp cl, 5
	jae @@ToBase32
	
@@CheckCanReadOrOnlyLeft:
	cmp ch, INPUT_LEN ;Check if end was reached
	jb @@GetFromInput
	xor bl, bl
	jmp @@ToBase32 ;Since we have nothing to read now we are starting again only with the left
	
@@GetFromInput:
	inc si
	mov bl, [SecretBit+si]
	inc ch
	jmp @@ToBase32
	
@@FinalBase32:
	xor ax, ax
	mov al, dh ;Count of 5bit prosseced
	mov bl, 8
	div bl ;ah will have the 5bit prosseced count % 8
	cmp ah, 0
	je @@FinishProc

@@FillEqualsUntilMul8:
	push dx
	xor dl, dl
	xchg dl, dh
	mov dx, di
	mov [SecretEnc+di], '='
	pop dx
	xor ax, ax
	mov al, dh
	div bl
	cmp ah, 0
	je @@FinishProc
	
	inc dh
	jmp @@FillEqualsUntilMul8
	
	
@@FinishProc:	
	ret
endp Base32Enc
		
END
