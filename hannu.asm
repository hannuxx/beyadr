
; ZPROT.ASM
; COMPILED WITH NASM 0.98 UNDER WIN98
; Copyright (c) Hannu M. Heikkinen
; hannu.heikkinen@helsinki.fi
; hannu.m.heikkinen@vaisala.com
; All rights reserved.


global _start
global _clear_screen
global _print_msg1
global _print_msg2
global _sys_call

global _print_init
global _print_error
global _print_test1
global _print_test2
global _quit

extern main
; extern _handle_c
;; extern _timer_int		

	section .text
	
[BITS 32]

_start:
	; Check that load was right
	mov eax, [zm]
	cmp eax, 0x12
	je zeb_ok

spin:		
	jmp spin
zeb_ok:		
	; Loading GDT and IDT
        cli
        lgdt [gdtr_prot]
        lidt [idtr_prot]
        sti

        mov ax, data_selector
        mov ds, ax
        mov es, ax
        mov fs, ax

        cli
        ; set up stack
        mov ax, data_selector
        mov ss, ax
        mov esp, 0x8000
        sti

        mov eax, video_selector
        mov gs, eax

        call _clear_screen

        mov esi, Zendriq_msg
        call print_string

        mov esi, Reg_info
        call print_string

        call print_registers

        ; main never returns...
        mov ax, 0x1234
	call main
        mov ax, 0x5678
        call _print_msg2

start32_spin:
        inc byte [gs:320]
        JMP start32_spin

_quit:
	push ebp 
	mov ebp, esp
	mov esp, ebp
	pop ebp
        ret

_print_msg1:
        push ebp
        mov ebp, esp
        push esi
        mov esi, zprot_main
        call print_string
        pop esi
        mov esp, ebp
        pop ebp
        ret

_print_msg2:
        push ebp
        mov ebp, esp
        push esi
        mov esi, zprot_after
        call print_string
        pop esi
        mov esp, ebp
        pop ebp
        ret

_print_error:
        push esi
        mov esi, msg_error
        call print_string
        pop esi
        ret

_print_init:
        push esi
        mov esi, msg_init
        call print_string
        pop esi
        ret

_print_test1:
        push esi
        mov esi, msg_test1
        call print_string
        pop esi
        ret

_print_test2:
        push esi
        mov esi, msg_test2
        call print_string
        pop esi
        ret

_sys_call:
        push eax
        mov eax, ebx

        int 47

        pop eax
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

        rol ebx, 8
        mov al, bl
        call printal

        rol ebx, 8
        mov al, bl
        call printal

        ror ebx, 24
        mov al, bl
        call printal

        ror ebx, 8
        mov al, bl
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

        mov esi, _BX
        call print_string        
        call print_number

        mov esi, _AX
        call print_string
        mov ebx, eax
        call print_number

        mov esi, _CX
        call print_string
        mov ebx, ecx
        call print_number

        mov esi, _DX
        call print_string
        mov ebx, edx
        call print_number

        mov esi, _CS
        call print_string
        mov ebx, cs
        call print_number

        mov esi, _DS
        call print_string
        mov ebx, ds
        call print_number

        mov esi, _ES
        call print_string
        mov ebx, es
        call print_number

        mov esi, _FS
        call print_string
        mov ebx, fs
        call print_number

        mov esi, _GS
        call print_string
        mov ebx, gs
        call print_number

        mov esi, _SS
        call print_string
        mov ebx, ss
        call print_number

        mov esi, _SP
        call print_string
        mov ebx, esp
        call print_number

        pop ecx
        pop ebx
        pop eax

        ret

;----------------------------------------------------------------------------
; Interrupt handlers
;----------------------------------------------------------------------------
unhand:
        push ax

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax

        inc byte [gs:160]

        ; EOI
        mov al, 0x20
        out 0x20, al

        pop ax

        iret

system_call_itr:
	  pushad

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax

        inc byte [gs:180]
 
        ; EOI
        mov al, 0x20
        out 0x20, al

        popad

        iret

kbd_itr:
        pushad

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
        jne is_r
	mov al, 'c'
        mov [gs:edi], al


is_r:
        cmp al, 0x93
        jne kbd_EOI
	mov al, 'r'
        mov [gs:edi], al
        add eax, 0x123
        call print_registers
	
kbd_EOI:

        ; EOI
        mov al, 0x20
        out 0x20, al

        popad

        iret

timer_itr:
        pushad

        mov eax, video_selector
        mov gs, eax
        mov eax, data_selector
        mov ds, eax
        inc byte [gs:170]

        ; EOI
        mov al, 0x20
        out 0x20, al
	
        popad

        iret

;--------------
; TEXT SECTION 
;--------------
	section .data
	
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
            dw 0xffff ;limit bytes 1-2 (because G=1)
            dw 0x0000 ;base bytes 1-2
            db 0x00   ;base byte 3
            db 0x92   ;bits 47-40=10010010 (writable,!executable,present,DPL=0)
            db 0xcf   ;bits 55-48=11001111 (G=1,D=1,limit bits4=0xff)
            db 0x00   ;base byte 4
gdt_end

idtr_prot:  dw idt_end - idt - 1    ; idt limit
	    dd idt

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

Zendriq_msg    db 0,0,0,0,0x1f,"Zendriq Operating System 0.0.1",0
zprot_main     db 0,0,7,0,0x1f,"Hello from ZMAIN.C!",0
zprot_after   db 0,0,8,0,0x1f,"...after ZMAIN.C...",0

msg_error     db 0,0,9,0,0x1f,"Error................",0
msg_init      db 0,0,9,0,0x1f,"Init................",0
msg_test1     db 0,0,9,0,0x1f,"11111111.............",0
msg_test2     db 0,0,9,0,0x1f,"22222222.............",0

zm  db 0x12
	
; Keyboard interrupt data place holder
LastKey_prot  db 0x0















