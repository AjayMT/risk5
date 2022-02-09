#!/bin/sh

for prog in ./*.s
do
  riscv64-unknown-elf-as $prog
  riscv64-unknown-elf-objcopy a.out -O binary a.out.bin
  riscv32-unknown-elf-bin2hex -w 32 a.out.bin $(basename $prog .s).txt
  rm a.out a.out.bin
done
