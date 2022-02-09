main:
  addi x1, x0, 13
  add x1, x1, x1
  lui x2, 0x1234
  auipc x3, 0xABCD
  slti x4, x1, 27
  addi x4, x4, -3
  addi x1, x0, 2
  sra x4, x1, x4
