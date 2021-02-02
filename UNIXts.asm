.486
IDEAL
MODEL small

macro BCDtoHEX num ;8bit, puts in dl
    push ax
    push bx
    push cx
    
    mov bl, num
    and bl, 0Fh
    mov al, num
    and al, 0F0h
    mov cl, 4
    ror al, cl
    mov dl, 0Ah
    mul dl
    add al, bl
    mov dl, al
    
    pop cx
    pop bx
    pop ax
endm BCDtoHEX

macro AxDaysToSeconds ;ax * 86400 result --> dx:ax
    local Loop1
    local Loop2
    local FinishM
    push cx
    push si
    
    xor dx, dx
    mov si, ax
    xor ax, ax
    
    mov cx, 2
Loop1:
    push cx
    mov cx, 43200
Loop2:
    add ax, si
    adc dx, 0
    loop Loop2
    pop cx
    loop Loop1
    
FinishM:
    pop si
    pop cx
endm AxDaysToSeconds

DATASEG

UNIX_TO_2021 =  1609459200
BaseTime dd UNIX_TO_2021
TIME_ZONE = 2
SEC_TIME_ZONE = 3600*TIME_ZONE

DaysInMonthLookup dw 0 ;illegal
                              dw 0 ;Days between 1/1 and 1/1
                              dw 31 ;Days between 1/1 and 1/2
                              dw 59 ;Days between 1/1 and 1/3
                              dw 90 ;31+28+31 ;Days between 1/1 and 1/4
                              dw 120 ;31+28+31+30 ;Days between 1/1 and 1/5
                              dw 151 ;31+28+31+30+31 ;Days between 1/1 and 1/6
                              dw 181 ;31+28+31+30+31+30 ;Days between 1/1 and 1/7
                              dw 212 ;31+28+31+30+31+30+31 ;Days between 1/1 and 1/8
                              dw 243 ;31+28+31+30+31+30+31+31 ;Days between 1/1 and 1/9
                              dw 273 ;31+28+31+30+31+30+31+31+30 ;Days between 1/1 and 1/10
                              dw 304 ;31+28+31+30+31+30+31+31+30+31 ;Days between 1/1 and 1/11
                              dw 334 ;31+28+31+30+31+30+31+31+30+31+30 ;Days between 1/1 and 1/12
    
LeapYears dw 2020, 2024, 2028, 2032, 2036;, 2040, 2044, 2048, 2052, 2056, 2060 ;leap years lookup table.    

CODESEG

public EpochTimeDiv30
proc EpochTimeDiv30
    push cx
    push bx
    
    mov cx, [word ptr BaseTime+2]
    mov bx, [word ptr BaseTime] ;cx:bx = UNIX_TO_2021
    call SecThisYearUTC
    add ax, bx
    adc dx, cx
    
    push ax
    
    mov ax, dx
    xor dx, dx
    mov bx, 30
    div bx ;dx = rem, ax = high order quotient
    mov cx, ax ;store high order quotient
    
    pop ax
    div bx
    
    mov dx, cx
    ;now dx:ax = quotient
    
    pop bx
    pop cx
    ret
endp EpochTimeDiv30
    
    
    
;--------------------------------------------
;Calcultate the amout of seconds since January 1st of the current year
;OUTPUT: dx = days
;--------------------------------------------
proc SecThisYear
    push cx
    push bx
    push si
    
    mov ah, 4 ;get rtc date
    int 1ah
    push dx ;save month:day(DH:DL)
    BCDtoHEX dh
    mov al, dl ;save hex month
    pop dx
    BCDtoHEX dl ;now dl = day, al = month
    
    shl al, 1 ;mul month by 2 because its in words(every word = 2bytes) 
    xor ah, ah
    mov si, ax
    mov ax, [DaysInMonthLookup+si]
    
    xor dh, dh
    dec dl ;if it's the 25th we add 24(current day isn't a full day)
    add ax, dx ;ax = days this year
    push ax ;save days
    
    mov ah, 2
    int 1ah
    
    BCDtoHEX dh
    mov dh, dl
    BCDtoHEX ch
    mov ch, dl
    BCDtoHEX cl
    mov cl, dl
    
    mov bl, dh ;seconds - current day
    xor bh, bh
    mov si, bx ;save current day's seconds in si

    mov bl, ch
    mov ax, 3600
    mul bx ;now dx:ax = current day's hours in seconds
    
    push dx ;save hours in seconds high-order word
    push ax ;low
    
    mov bl, cl
    mov al, 60
    mul bl ;now ax = current day's minutes in seconds
    
    add si, ax ;add minutes to seconds
    pop ax
    pop dx
    add ax, si
    adc dx, 0 ;add minutes and secondes to hours(all in seconds)
    
    pop cx ;days current year that we pushed earlier
    push dx
    push ax 
    mov ax, cx
    AxDaysToSeconds ;dx:ax = days current year in seconds
    
    pop bx ;minutes secondes and hours - low order word
    pop cx ;^^^^^^ - high-order word
    
    add ax, bx
    adc dx, 0
    add dx, cx
    
    pop si
    pop bx
    pop cx
    ret
endp SecThisYear

;-----------------------------------------------------------
;Returns in dx:ax the seconds that have past since the beginnig of the current year in UTC
;-----------------------------------------------------------
proc SecThisYearUTC
    call SecThisYear
    sub ax, SEC_TIME_ZONE
    sbb dx, 0 ;32 bit subtruction
    ret
endp SecThisYearUTC

end