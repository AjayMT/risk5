#!/bin/sh

for prog in ./*.s
do
  riscv64-unknown-elf-as -march=rv32i $prog -o a.out.elf
  riscv64-unknown-elf-objcopy a.out.elf -O binary a.out.bin
  riscv32-unknown-elf-bin2hex -w 32 a.out.bin $(basename $prog .s).txt
  rm a.out.*
done
