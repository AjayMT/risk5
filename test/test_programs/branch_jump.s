main:
  add x1, x0, x0
  jal x1, skip
  addi x2, x0, 0xBE
skip:
  addi x2, x0, 0xDF
  jal x1, main
