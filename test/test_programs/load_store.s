main:
  lui x1, 0xDEADB
  addi x1, x1, 0x0EF
  sw x1, 3(x0)
  lw x2, 3(x0)
  addi x3, x0, 9
  lb x2, 3(x0)
  lh x2, 3(x0)
  lbu x2, 3(x0)
  lhu x2, 3(x0)
  sb x1, 0(x3)
  lw x2, 0(x3)
  sh x1, 0(x3)
  lw x2, 0(x3)
  sw x1, 0(x3)
  lw x2, 0(x3)
