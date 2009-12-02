SRC:=boot.asm prot.asm kernel.c
ELF_OBJS:=boot prot.o kernel.o
DO_BIN:=generate_bin
DO_ELF:=generate_elf
BINS:=boot prot
ELFS:=kernel

AS:=nasm
AS_ELF_FLAGS:=-f elf
CC:=gcc
CCF:=-Wall -ffreestanding -fomit-frame-pointer
LD_BIN_FLAGS:=-Ttext=0x8C00 --oformat binary -nostdlib
LD_ELF_FLAGS:=-Ttext=0x8C00 --oformat elf32-i386 -nostdlib

bin: $(DO_BIN)

elf: $(DO_ELF)

generate_bin: $(BINS)
	cat boot prot > zos

generate_elf: $(ELFS)
	cat boot kernel > zos

kernel: $(ELF_OBJS) 
	ld $(LD_BIN_FLAGS) -o kernel prot.o kernel.o 
#ld $(LD_ELF_FLAGS) -o kernel prot.o kernel.o 

boot:
	$(AS) -o boot boot.asm

prot: 
	$(AS) -o prot prot.asm

prot.o: 
	$(AS) $(AS_ELF_FLAGS) -o prot.o prot.asm

kernel.o: 
	$(CC) $(CCF) -c kernel.c -o kernel.o

clean:
	@rm -f prot prot.o kernel.o boot

veryclean:
	@rm -f zos kernel prot prot.o kernel.o boot

floppy:
	dd if=zos of=/dev/fd0

qemu:
	qemu -fda zos




