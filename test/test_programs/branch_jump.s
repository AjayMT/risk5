main:
  addi x30, x0, 0xAA
  jal x1, skip
  addi x2, x0, 0xBE
skip:
  addi x20, x0, 0xDF
  xor x5, x5, x5
  jal x1, func
  bne x5, x0, main

func:
  addi x5, x0, 0xFC
  beq x5, x0, skip
  jalr x0, 0(x1)
