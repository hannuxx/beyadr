; ZBOOT.ASM
; Beyadr OPERATING SYSTEM BOOT CODE
; 1998-2008: COMPILED WITH NASM 0.98 FOR WIN98 & LINUX
; 2009->   : COMPILED WITH NASM 0.98 FOR WIN98 & LINUX
; Copyright 1999-2009 (c) Hannu M. Heikkinen
; hannuxx@iki.fi
; All rights reserved.
; Some code snippets taken from Stefan Reitshamer ChiOS (bootload.asm and
; loader.asm). Please see http://www.reitshamer.com.

        org 0x7c00
        section .text
        jmp beyadr_entry

; =========================================
; Print numeric values like registers etc.
; =========================================
printnum:
        push ax
        mov al, ah        
        call printal
        pop ax
        push ax
        call printal
        pop ax
        ret
printal:
        push ax
        and al, 0xf0
        ror al, 4
        call num1
        pop ax
        push ax
        and al, 0x0f
        call num1
        pop ax
        ret
num1:
        push ax
        push bx
        add al, 0x30 ; ASCII conv
        cmp al, 0x39 ; A-F?
        jle numpr
        add al, 0x27
numpr:
        mov ah, 0x0e
        mov bx, 0x07
        int 0x10
        pop bx
        pop ax
        ret

; ==============================================
; Print strings
; ==============================================
printstr:
        push ax
        call printstr2
        pop ax
        ret
printstr2:
        lodsb
        cmp al, 0
        jz fin
        call printch
        jmp printstr2
fin:
        ret

; Print character
printch:
        push ax
        push bx
        push cx
        xor bh, bh
        mov cx, 01h
        mov ah, 0eh
        int 0x10
        pop cx
        pop bx
        pop ax
	ret

; ===========================================
; Read the sector from the floppy!!!
; read_sector(param copy_seg, param copy_offset, param LBA)
; LBA   Head    Track   Sector
; ===   ====    =====   ======
; 0x00  0       0x00    0x01
;   ...
; 0x11  0       0x00    0x12
; 0x12  1       0x00    0x01 <== Head ONE!
;   ...
; etc
; ===========================================
read_sector:
        pusha
        mov di, 3       ; try three times
read_try1:
        push ax
        ; CH = track, CL = sector,
        ; DH = head, DL = drive
        ; AH = 02h, AL = Num of sectors

        ; ch = cylinder/track, cl = sector number
        ; dh = head, dl = drive (floppy)

        mov ax, 0x0201  ; read disk (2), and sectors (1)
        int 0x13
        jnc read_away
        dec di
        pop ax
        jnz read_try1
        jmp read_fail

read_away:
        pop ax
read_fail:
        popa
        ret

; wait and give bus some time
some_wait:
        xchg ax, ax
        ret

; ===========================================================
; After setting interrupts causes floppy motor to run, if
; this is not called
; ===========================================================
kill_floppy:
        push dx
        mov dx, 0x3f2
        mov al, 0x0
        out dx, al
        pop dx
        ret

; ===========================================================
; Set the system running...
; ===========================================================
beyadr_entry:
        mov ax, 0
        mov ds, ax
        mov es, ax
        cli
        mov ss, ax
        mov sp, 0x7c00
        sti

        cld             ; Clear direction flag

        ; Initialize printing
        mov ah, 0x03
        xor bx, bx
        int 0x10

        mov si, LoadLine
        call printstr

        ; Handle floppy reads I
        ; move 2-12 sectors into ES:BX 
        mov cx, 0x0B    ; 11 sectors
        mov bx, 0x7e0
        mov es, bx              
        mov bx, 0x0
        mov ax, 2       ; sectors 2-12

read_loop:
        mov dl, 0x0     ; dl = drive (floppy)
        mov dh, 0x0     ; dh = head
        push cx
        mov ch, 0       ; ch = cylinder/track
        mov cl, al      ; cl = sector number
        call read_sector
        add bx, 0x200   ; up 512 bytes...
        add ax, 1       ; From sector 1         
        pop cx
        loop read_loop

        mov si, FirstRead
        call printstr

        ; Handle floppy reads II
        ; move 13-24 sectors into ES:BX 
        mov cx, 0x0C    ; 12 sectors
        mov bx, 0x920
        mov es, bx              
        mov bx, 0x0
        mov ax, 13       ; From sector 13-24

read_loop2:
        mov dl, 0x0     ; dl = drive (floppy)
        mov dh, 0x1     ; dh = head
        push cx
        mov ch, 0       ; ch = cylinder/track
        mov cl, al      ; cl = sector number
        call read_sector
        add bx, 0x200    ; up 512 bytes...
        add ax, 1
        pop cx
        loop read_loop2

        mov si, SecRead
        call printstr

        call kill_floppy

        mov si, JumpInfo
        call printstr
        
        ; Jump into the loader
        jmp loader_start

; Error if over here!
        mov si, NoWay
        call printstr

spin:  jmp spin

; =========================================
; Data
; =========================================
LoadLine db "Booting process...loading the Beyadr loader...",13,10
	 db 0
JumpInfo db "Jumping into the loader in 0x7E00...",13,10
	 db 0
ReadOK  db "Sector read OK...",13,10
	db 0
FailRead db "*read failed*",13,10
	db 0
NoWay db "#Error#"
	db 0
             
; Make this 512 long...
       times 510-($-$$) db 0

; Boot signature
DW 0xAA55

; ============================================================
;            Z E N D R I Q       L   O   A   D   E   R
; Copyright 1999-2009 (c) Hannu M. Heikkinen
; hannuxx@iki.fi
; All rights reserved.
; ============================================================
       times 0x200-($-$$) db 0

enable_a20:
	  mov si, enablea20
        call printstr

        cli                             ; disable interrupts

        mov bl, 0xd0                    ; read current status command
        call kbd_send_ctrl_cmd

        call kbd_read_data
        or al, 2                        ; set the a20 enable bit
        push ax

        mov bl, 0xd1                    ; write current status command
        call kbd_send_ctrl_cmd

        pop bx
        call kbd_write_data             ; write the new status

        mov bl, 0xd0                    ; read current status command
        call kbd_send_ctrl_cmd

        call kbd_read_data              ; read the current status
        and al, 2

        sti                             ; enable interrupts

        jnz a20_ok

        mov si, a20err
        call printstr

        call reboot
a20_ok:

	  mov si, msg_ok
        call printstr
        ret

reboot:
        mov si, presskeymsg
        call printstr

        mov ah, 0              ; read keypress function
        int 0x16               ; call bios keyboard services

        jmp 0xFFFF:0x0000


kbd_wait_cmd:
        in al, 0x64            ; read the controller status port
        and al, 2              ; check if the controller is ready
        jnz kbd_wait_cmd       ; to accept the next command
        ret                    ; (or piece of data)

kbd_wait_data:
        in al, 0x64            ; read the controller status port
        and al, 1              ; check if the data is ready
        jz kbd_wait_data
        ret

kbd_send_ctrl_cmd:
; input : bl = command

        call kbd_wait_cmd
        mov al, bl
        out 0x64, al           ; send the command to the control
        ret                    ; register

kbd_read_data:
; output : al = data

        call kbd_wait_data
        in al, 0x60            ; read data from intput/output port
        ret

kbd_write_data:
; input bl = data

        call kbd_wait_cmd
        mov al, bl
        out 0x60, al           ; write data to input/output port
        ret


; ============================ LOADER START ==========================
loader_start:
        mov ax, 0x0
        mov ds, ax
        mov es, ax
        cli
        mov ss, ax
        mov sp, 0x2FFF
        sti

        ; Initialize printing
        mov ah, 0x03
        xor bx, bx
        int 0x10

        mov si, InfoLine
        call printstr

        call print_regs

	call set_interrupts

        call enable_a20

        jmp go_protected

print_regs:
        mov si, _AX
        call printstr
        call printnum

        mov si, _BX
        call printstr
        mov ax, bx
        call printnum

        mov si, _CX
        call printstr
        mov ax, cx
        call printnum

        mov si, _DS
        call printstr
        mov ax, ds
        call printnum

        mov si, _ES
        call printstr
        mov ax, es
        call printnum

        mov si, LF
        call printstr

        mov si, _SS
        call printstr
        mov ax, ss
        call printnum

        mov si, _SP
        call printstr
        mov ax, sp
        call printnum

        mov si, _CS
        call printstr
        mov ax, cs
        call printnum
        mov si, LF
        call printstr

	ret

set_interrupts:
        ; Reprogram the 1st 8259 PIC
        cli
        mov al, 0x20
        out 0x20, al

        mov al, 0x11
        out 0x20, al
        ; Only reprogramming the 1st 8259...

        mov al, 0x20    ; IRQ0-IRQ7
        out 0x21, al

        mov al, 0x4
        out 0x21, al

        mov al, 0x1
        out 0x21, al

        ; Enable interrupts ==> IRQ0 is timer and IRQ1 is keyboard...
        mov al, 0xFC
        out 0x21, al

        ret

go_protected:
        mov si, SetINT
        call printstr
        call set_interrupts
        mov si, SetINT2
        call printstr

        mov si, GoingPM
        call printstr

        ; switch to protected mode
        cli
o32     lgdt [gdtr]
o32     lidt [idtr]
        mov eax, cr0
        or al, 1
        mov cr0, eax
        jmp code_selector:go_pm

[bits 32]
go_pm:
        mov ax, data_selector
        mov ds, ax
        mov es, ax
        mov ax, video_selector
        mov gs, ax

        sti ; Now hw calls our interrupt handlers...

kbd_spin_pm:
        mov al, [LastKey]
        cmp al, 0x81
        jne kbd_spin_pm

        cli
        mov ax, data_selector        ; stack segment the same as data selector
        mov ss,ax
        mov esp, 0x8000
        sti

        ; Jump into beyadr_prot.asm start
        jmp code_selector:0x8C00

error_spin:
	mov dword [gs:168], 0x1f45 

        jmp error_spin

;----------------------------------------------------------------------------
; Interrupt handlers
;----------------------------------------------------------------------------
unhand:
        push ax
        push gs

        mov ax, video_selector
        mov gs, ax
        inc byte [gs:160]

        pop gs
        pop ax

        iret

kbd:
        push ax
        push bx
        push gs

        mov ax, video_selector
        mov gs, ax
        mov ax, data_selector
        mov ds, ax
        ;inc byte [gs:164]
        in al, 0x60
        mov ah, 0x1f
        mov [gs:164],ax
        mov [LastKey], al

        ; EOI
        mov al, 0x20
        out 0x20, al

        pop gs
        pop bx
        pop ax

        iret

timer:
        push ax
        push bx
        push gs

        mov ax, video_selector
        mov gs, ax
        mov ax, data_selector
        mov ds, ax
        inc byte [gs:162]

        ; EOI
        mov al, 0x20
        out 0x20, al

        pop gs
        pop bx
        pop ax

        iret

[bits 16]
;----------------------------------------------------------------------------
; this procedure just writes a zero-terminated message to the screen
; format: word x, word y, attribute byte, string, 0
; In:	DS:SI - pointer to format string

write_msg:
        mov     ax,[si+2]               ; get Y position
        mov     di,80
	mul	di
        add     ax,[si]
        mov     di,ax
        mov     ah,[si+4]               ; get attribute byte
	add	si,5
write_loop:
        mov     al,[si]
	or	al,al			; end of string?
        jz      loop_end
	inc	si
        mov     [gs:di],ax
	inc	di
	inc	di
        jmp     write_loop
loop_end:
	ret

[bits 32]
;-----------------------------------------------------------------------
; GDT=Global Descriptor Table
; Descriptor bits and bytes:
; 00-15     segment limit, low 2 bytes
; 16-39     base addr, low 3 bytes
; 40        A=accessed
; 41        W=(0=read-only, 1=writable)
; 42        "expand down" for data, or "conforming" for code =>make it 0
; 43        E=(1 = executable, 0 = not executable)
; 44        =1
; 45-46     DPL=(descriptor privilege level): 0 = kernel
; 47        P=(present)
; 48-51     limit (bits 0-15 and 48-51 limit, adj with 55)
; 52        AVL
; 53        O
; 54        D=(1 = 32-bit code default, 0 = 16-bit code default)
; 55        G=(0=add 0xFF to bottom of limit, 1=add 0 to top of limit)
;-----------------------------------------------------------------------
gdtr:       dw gdt_end-gdt-1    ; length of GDT
            dd gdt
gdt
null_selector equ $-gdt 	; = 0
gdt0                  		; descriptor #0, the null descriptor
            dd 0
            dd 0

code_selector equ $-gdt ; = 0x08 
code_gdt              
				; 4 GB flat segment starting at 0x0000:0x0000
            dw 0xffff 		; limit bytes 1-2 (because G=1)
            dw 0x0000 		; base bytes 1-2
            db 0x00   		; base byte 3
            db 0x9a   		; bits 47-40=10011010 (writable,executable,present,DPL=0)
            db 0xcf   		; bits 55-48=11001111 (G=1,D=1,limit bits4=0xf)
            db 0x00   		; base byte 4
data_selector equ $-gdt 	; = 0x10
data_gdt
            dw 0xffff 		; limit bytes 1-2 (because G=1)
            dw 0x0000 		; base bytes 1-2
            db 0x00   		; base byte 3
            db 0x92   		; bits 47-40=10010010 (writable,!executable,present,DPL=0)
            db 0xcf   		; bits 55-48=11001111 (G=1,D=1,limit bits4=0xff)
            db 0x00   		; base byte 4
video_selector equ $-gdt 	; = 0x18
            dw 3999   		; limit bytes 1-2
            dw 0x8000 		; base bytes 1-2
            db 0x0b   		; base byte 3
            db 0x92   		; bits 47-40=10010010 (writable,!executable,present,DPL=0)
            db 0x00   		; bits 55-48=00000000
            db 0x00   		; base byte 4
gdt_end:

idtr:   dw idt_end - idt - 1    ; idt limit
        dd idt

idt:
; Unhandled interrupts  0-31
        %rep 32
        dw unhand
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0
        %endrep

; Timer interrupt       32
        dw timer
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0

; Keyboard interrupt    33
        dw kbd
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0

        %rep 14
        dw unhand       ; 34-47 are unhandled
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0
        %endrep
idt_end:

[BITS 16]
;-----------------------------------------------------------------------
; Data
LastKey  db 0x0

InfoLine db "Welcome, The Beyadr Operating System 0.01...",13,10
	 db 0
FirstRead db "Read sectors 2-12",13,10
	 db 0
SecRead db "Read sectors 13-24",13,10
	 db 0
SetINT   db "Setting interrupts...",13,10
	 db 0
SetINT2  db "Interrupts DONE...",13,10
	 db 0
GoingPM db "Press ESC to enter the Beyadr kernel...",13,10
	 db 0
GoingPM2 db "Into protected mode, no turning back...",13,10
	 db 0
LF     db 13,10,0
_AX    db " AX=0x",0
_BX    db " BX=0x",0
_CX    db " CX=0x",0
_DX    db " DX=0x",0
_DS    db " DS=0x",0
_ES    db " ES=0x",0
_SS    db " SS=0x",0
_SP    db " SP=0x",0
_CS    db " CS=0x",0
_IP    db " IP=0x",0

presskeymsg db 'Remove disk and press a key to reboot....', 13, 10, 0
enablea20   db 'enabling a20 line... ', 0
a20err      db 'ERROR in a20 line... ', 13, 10, 0
msg_ok      db 'a20 is OK... ', 13, 10, 0

; Make this 0x1000 long...
       times 0x1000-($-$$) db 0


