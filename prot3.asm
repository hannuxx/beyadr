; ZPROT.ASM
; COMPILED WITH NASM 0.98 UNDER WIN98
; Copyright (c) Hannu M. Heikkinen
; hannu.heikkinen@helsinki.fi
; hannu.m.heikkinen@vaisala.com
; All rights reserved.

global start
global _clear_screen
global _hello_from_main
extern _main

[BITS 32]
start:
        ; Loading GDT and IDT
        cli
	lgdt [gdtr_prot]
	lidt [idtr_prot]
        sti

        mov ax, data_selector
        mov ds, ax
        mov es, ax
        mov fs, ax
        mov gs, ax
        cli
        mov ss, ax
        mov esp, 0xFF00
        sti

        mov eax, video_selector
        mov gs, eax

        call _clear_screen

        ;mov esi, zprot_msg1
        ;call print_string

        ;mov esi, zprot_msg2
        ;call print_string

        ;mov esi, Reg_info
        ;call print_string

        ;call print_registers

        ;mov esi, zprot_msg3a
        ;call print_string

        call _main

after_main:

        mov esi, zprot_msg4
        call print_string

start32_spin:
        JMP start32_spin

_hello_from_main:
        push esi
        mov esi, zprot_msg3
        call print_string
        pop esi
        ret

_clear_screen:
        sub edi, edi
        mov cx, 2000
        mov ah, 0x1f  
        mov al,' '
clear_loop:
        or cx, cx
        jz clear_loop_end
        mov     [gs:edi],ax
	inc	edi
	inc	edi
        dec cx
        jmp     clear_loop
clear_loop_end:
	ret

print_string:
        sub edi, edi
	movzx	di,[esi+2]		; get Y position
	imul	edi,160
	add	di,[esi]		; add X position
        add     di,[esi]
        mov     ah,[esi+4]              ; get attribute byte
        add     esi,5
print_loop:
        mov     al,[esi]
	or	al,al			; end of string?
        jz      print_loop_end
	inc	esi
        mov     [gs:edi],ax
	inc	edi
	inc	edi
        jmp     print_loop
print_loop_end:
	ret

; =========================================
; Print numeric values like registers etc.
; =========================================
print_number:
        push eax
        push ebx

        push ebx
        ror ebx, 32
        mov al, bl
        call printal

        mov al, bh
        call printal

        pop ebx
        mov ebx, eax
        mov al, bl
        call printal

        mov al, bh
        call printal

        pop ebx
        pop eax
        ret
printal:
        push eax

        and al, 0xf0
        ror al, 4
        call num1
        pop eax
        push eax
        and al, 0x0f
        call num1

        pop eax
        ret
num1:
        push eax
        push ebx
        add al, 0x30 ; ASCII conv
        cmp al, 0x39 ; A-F?
        jle numpr
        add al, 0x27
numpr:
        mov ah, ch
        mov [gs:edi], ax
        inc edi
        inc edi
        pop ebx
        pop eax
        ret

; =========================================
; Print numeric values like registers etc.
; =========================================
print_registers:
        push eax
        push ebx
        push ecx

        mov ch, 0x1f

        mov esi, _AX
        call print_string
        call print_number

        mov esi, _BX
        call print_string
        mov eax, ebx
        call print_number

        mov esi, _CX
        call print_string
        mov eax, ecx
        call print_number

        mov esi, _DX
        call print_string
        mov eax, edx
        call print_number

        mov esi, _CS
        call print_string
        mov eax, cs
        call print_number

        mov esi, _DS
        call print_string
        mov eax, ds
        call print_number

        mov esi, _ES
        call print_string
        mov eax, es
        call print_number

        mov esi, _FS
        call print_string
        mov eax, fs
        call print_number

        mov esi, _GS
        call print_string
        mov eax, gs
        call print_number

        mov esi, _SS
        call print_string
        mov eax, ss
        call print_number

        mov esi, _SP
        call print_string
        mov eax, esp
        call print_number

        pop ecx
        pop ebx
        pop eax

        ret

;----------------------------------------------------------------------------
; Interrupt handlers
;----------------------------------------------------------------------------
unhand:
        push eax
        push gs

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax
        inc byte [gs:180]

        pop gs
        pop eax

        iret

system_call_itr:
        push eax
        push gs

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax
        inc byte [gs:184]

        pop gs
        pop eax

        iret

kbd_itr:
        push eax
        push ebx
        push ecx
        push gs

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax
        in al, 0x60
        mov ah, 0x1f
        mov ch, 0x1f

        sub edi, edi
        mov bl, 4
        movzx   di,bl           ; Y position
        mov ebx, 160
        imul    edi,ebx
	add	di,10		; add X position
        add     di,10

        call printal
	
        sub edi, edi
        mov bl, 5
        movzx   di,bl           ; Y position
        mov ebx, 160
        imul    edi,ebx
	add	di,10		; add X position
        add     di,10

        mov [LastKey_prot], al

is_a:
        cmp al, 0x9e
        jne is_b
	mov al, 'a'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_b:
        cmp al, 0xb0
        jne is_c
	mov al, 'b'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_c:
        cmp al, 0xae
        jne is_d
	mov al, 'c'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_d:
        cmp al, 0xa0
        jne is_e
	mov al, 'd'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_e:
        cmp al, 0x92
        jne is_f
	mov al, 'e'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_f:
        cmp al, 0xa1
        jne is_g
	mov al, 'f'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_g:
        cmp al, 0xa2
        jne is_h
	mov al, 'g'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_h:
        cmp al, 0xa3
        jne is_i
	mov al, 'h'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_i:
        cmp al, 0x97
        jne is_j
	mov al, 'i'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_j:
        cmp al, 0xa4
        jne is_k
	mov al, 'j'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_k:
        cmp al, 0xa5
        jne is_l
	mov al, 'k'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_l:
        cmp al, 0xa6
        jne is_m
	mov al, 'l'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_m:
        cmp al, 0xb2
        jne is_n
	mov al, 'm'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_n:
        cmp al, 0xb1
        jne is_o
	mov al, 'n'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_o:
        cmp al, 0x98
        jne is_p
	mov al, 'o'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_p:
        cmp al, 0x99
        jne is_q
	mov al, 'p'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_q:
        cmp al, 0x90
        jne is_r
	mov al, 'q'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_r:
        cmp al, 0x93
        jne is_s
	mov al, 'r'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_s:
        cmp al, 0x9f
        jne is_t
	mov al, 's'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_t:
        cmp al, 0x94
        jne is_u
	mov al, 't'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_u:
        cmp al, 0x96
        jne is_v
	mov al, 'u'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_v:
        cmp al, 0xaf
        jne is_w
	mov al, 'v'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_w:
        cmp al, 0x91
        jne is_x
	mov al, 'w'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_x:
        cmp al, 0xad
        jne is_y
	mov al, 'x'
        mov [gs:edi], al

        int 47                
        
	jmp kbd_EOI

is_y:
        cmp al, 0x95
        jne is_z
	mov al, 'y'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_z:
        cmp al, 0xac
        jne is_ao
	mov al, 'z'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_ao:
        cmp al, 0x9a
        jne is_aa
	mov al, 'å'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_aa:
        cmp al, 0xa8
        jne is_oo
	mov al, 'ä'
        mov [gs:edi], al
	
	jmp kbd_EOI

is_oo:
        cmp al, 0xa7
        jne kbd_EOI
	mov al, 'ö'
        mov [gs:edi], al

	
kbd_EOI:

        ; EOI
        mov al, 0x20
        out 0x20, al

        pop gs
	pop ecx
        pop ebx
        pop eax

        iret

timer_itr:
        push eax
        push ebx
        push gs

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax
        inc byte [gs:170]

        ; EOI
        mov al, 0x20
        out 0x20, al

        pop gs
        pop ebx
        pop eax

        iret

;----------------------------------------------------------------------------
; Global and Interrupt Descriptor Tables
;----------------------------------------------------------------------------
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
gdtr_prot:  dw gdt_end-gdt-1    ; length of GDT
            dd gdt
gdt
null_selector equ $-gdt  ; = 0
gdt0                  ; descriptor #0, the null descriptor
            dd 0
            dd 0

code_selector equ $-gdt  ; = 0x08
code_gdt              ; 4 GB flat segment starting at 0x0000:0x0000
            dw 0xffff ;limit bytes 1-2 (because G=1)
            dw 0x0000 ;base bytes 1-2
            db 0x00   ;base byte 3
            db 0x9a   ; bits 47-40=10011010 (writable,executable,present,DPL=0)
            db 0xcf   ; bits 55-48=11001111 (G=1,D=1,limit bits4=0xf)
            db 0x00   ; base byte 4
data_selector equ $-gdt  ; = 0x10
data_gdt
            dw 0xffff ;limit bytes 1-2 (because G=1)
            dw 0x0000 ;base bytes 1-2
            db 0x00   ;base byte 3
            db 0x92   ;bits 47-40=10010010 (writable,!executable,present,DPL=0)
            db 0xcf   ;bits 55-48=11001111 (G=1,D=1,limit bits4=0xff)
            db 0x00   ;base byte 4
video_selector equ $-gdt ; = 0x18
            dw 3999   ; limit bytes 1-2
            dw 0x8000 ; base bytes 1-2
            db 0x0b   ; base byte 3
            db 0x92   ;bits 47-40=10010010 (writable,!executable,present,DPL=0)
            db 0x00   ; bits 55-48=00000000
            db 0x00   ; base byte 4
stack_selector equ $-gdt ; = 0x20
            dw 0x0000 ; limit bytes 1-2
            dw 0x2000 ; base bytes 1-2
            db 0x0020 ; base byte 3
		      ; read/write stack
            db 0x96   ;bits 47-40=10010110 (writable,!executable,present,DPL=0)
            db 0x00   ;bits 55-48=00000000 (G=0,D=0,limit bits4=0x00)
            db 0x00   ;base byte 4
gdt_end

idtr_prot:  dw idt_end - idt - 1    ; idt limit
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
        dw timer_itr
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0

; Keyboard interrupt    33
        dw kbd_itr
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0

        %rep 13
        dw unhand       ; 34-46 are unhandled
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0
        %endrep

; System call interrupt 47
        dw system_call_itr
        dw code_selector
        db 0x0
        db 0x8E
        dw 0x0
idt_end:

Reg_info db 0,0,11,0, 0x1f, "Registers:",0
_AX    db 0,0,12,0,  0x1f,  "EAX=0x",0
_BX    db 0,0,13,0,  0x1f,  "EBX=0x",0
_CX    db 0,0,14,0,  0x1f,  "ECX=0x",0
_DX    db 0,0,15,0,  0x1f,  "EDX=0x",0
_DS    db 0,0,16,0, 0x1f, "DS=0x",0
_ES    db 0,0,17,0, 0x1f, "ES=0x",0
_FS    db 0,0,18,0, 0x1f, "FS=0x",0
_GS    db 0,0,19,0, 0x1f, "GS=0x",0
_SS    db 0,0,20,0, 0x1f, "SS=0x",0
_SP    db 0,0,21,0, 0x1f, "ESP=0x",0
_CS    db 0,0,22,0, 0x1f, "CS=0x",0
_IP    db 0,0,23,0, 0x1f, "IP=0x",0

zprot_msg1    db 0,0,0,0,0x1f,"Zendriq Operating System 0.0.1",0
zprot_msg2    db 0,0,1,0,0x1f,"Copyright (c) 2001 Hannu M. Heikkinen",0
zprot_msg3a   db 0,0,2,0,0x1f,"Before ZMAIN...",0
zprot_msg3    db 0,0,3,0,0x1f,"Hello World from ZMAIN!",0
zprot_msg3b   db 0,0,4,0,0x1f,"After ZMAIN...",0
zprot_msg4    db 0,0,5,0,0x1f,"All rights reserved.",0

; Keyboard interrupt data place holder
LastKey_prot  db 0x0


stack32
%rep 0x200
              db 0
%endrep
stack32_end   equ $-stack32


