open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { instruction : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    alu_op : 'a; [@bits 4]
    alu_a_src : 'a; [@bits 1]
    alu_b_src : 'a; [@bits 2]
    mem_write_enable : 'a; [@bits 1]
    writeback_sel : 'a; [@bits 2]
    writeback_enable : 'a; [@bits 1]
    next_pc_sel : 'a; [@bits 2]
    branch_cmp_op : 'a; [@bits 3]
  }
  [@@deriving sexp_of, hardcaml]
end

let alu_op_of_int = Signal.of_int ~width:4

module Alu_ops = struct
  let add = alu_op_of_int 0 (* 0000 *)

  let sub = alu_op_of_int 1 (* 0001 *)

  let and_ = alu_op_of_int 2 (* 0010 *)

  let or_ = alu_op_of_int 3 (* 0011 *)

  let xor = alu_op_of_int 4 (* 0100 *)

  let sll = alu_op_of_int 5 (* 0101 *)

  let srl = alu_op_of_int 6 (* 0110 *)

  let sra = alu_op_of_int 7 (* 0111 *)

  let slt = alu_op_of_int 8 (* 1000 *)

  let sltu = alu_op_of_int 9 (* 1001 *)

  let lui = alu_op_of_int 10 (* 1010 *)

  let aui = alu_op_of_int 11 (* 1011 *)
end

let circuit _ (input : _ I.t) =
  (*
    Source: https://github.com/riscv/riscv-opcodes/blob/master/opcodes-rv32i

    beq     bimm12hi rs1 rs2 bimm12lo 14..12=0 6..2=0x18 1..0=3
    bne     bimm12hi rs1 rs2 bimm12lo 14..12=1 6..2=0x18 1..0=3
    blt     bimm12hi rs1 rs2 bimm12lo 14..12=4 6..2=0x18 1..0=3
    bge     bimm12hi rs1 rs2 bimm12lo 14..12=5 6..2=0x18 1..0=3
    bltu    bimm12hi rs1 rs2 bimm12lo 14..12=6 6..2=0x18 1..0=3
    bgeu    bimm12hi rs1 rs2 bimm12lo 14..12=7 6..2=0x18 1..0=3

    jalr    rd rs1 imm12              14..12=0 6..2=0x19 1..0=3

    jal     rd jimm20                          6..2=0x1b 1..0=3

    lui     rd imm20 6..2=0x0D 1..0=3
    auipc   rd imm20 6..2=0x05 1..0=3

    addi    rd rs1 imm12           14..12=0 6..2=0x04 1..0=3
    slti    rd rs1 imm12           14..12=2 6..2=0x04 1..0=3
    sltiu   rd rs1 imm12           14..12=3 6..2=0x04 1..0=3
    xori    rd rs1 imm12           14..12=4 6..2=0x04 1..0=3
    ori     rd rs1 imm12           14..12=6 6..2=0x04 1..0=3
    andi    rd rs1 imm12           14..12=7 6..2=0x04 1..0=3

    add     rd rs1 rs2 31..25=0  14..12=0 6..2=0x0C 1..0=3
    sub     rd rs1 rs2 31..25=32 14..12=0 6..2=0x0C 1..0=3
    sll     rd rs1 rs2 31..25=0  14..12=1 6..2=0x0C 1..0=3
    slt     rd rs1 rs2 31..25=0  14..12=2 6..2=0x0C 1..0=3
    sltu    rd rs1 rs2 31..25=0  14..12=3 6..2=0x0C 1..0=3
    xor     rd rs1 rs2 31..25=0  14..12=4 6..2=0x0C 1..0=3
    srl     rd rs1 rs2 31..25=0  14..12=5 6..2=0x0C 1..0=3
    sra     rd rs1 rs2 31..25=32 14..12=5 6..2=0x0C 1..0=3
    or      rd rs1 rs2 31..25=0  14..12=6 6..2=0x0C 1..0=3
    and     rd rs1 rs2 31..25=0  14..12=7 6..2=0x0C 1..0=3

    lb      rd rs1       imm12 14..12=0 6..2=0x00 1..0=3
    lh      rd rs1       imm12 14..12=1 6..2=0x00 1..0=3
    lw      rd rs1       imm12 14..12=2 6..2=0x00 1..0=3
    lbu     rd rs1       imm12 14..12=4 6..2=0x00 1..0=3
    lhu     rd rs1       imm12 14..12=5 6..2=0x00 1..0=3

    sb     imm12hi rs1 rs2 imm12lo 14..12=0 6..2=0x08 1..0=3
    sh     imm12hi rs1 rs2 imm12lo 14..12=1 6..2=0x08 1..0=3
    sw     imm12hi rs1 rs2 imm12lo 14..12=2 6..2=0x08 1..0=3

    Source: https://github.com/riscv/riscv-opcodes/blob/master/opcodes-rv64i

    slli    rd rs1 31..26=0  shamt 14..12=1 6..2=0x04 1..0=3
    srli    rd rs1 31..26=0  shamt 14..12=5 6..2=0x04 1..0=3
    srai    rd rs1 31..26=16 shamt 14..12=5 6..2=0x04 1..0=3
   *)
  let opcode = input.instruction.:[(6, 0)] in
  let funct7 = input.instruction.:[(31, 25)] in
  let funct3 = input.instruction.:[(14, 12)] in
  let imm12 = input.instruction.:[(31, 20)] in

  let check_opcode x =
    opcode ==: Signal.of_int ~width:5 x @: Signal.of_string "2'd3"
  in

  let lui = check_opcode 0xD in
  let aui = check_opcode 0x5 in
  let i_type = check_opcode 0x4 in
  let r_type = check_opcode 0xC in
  let b_type = check_opcode 0x18 in
  let jalr = check_opcode 0x19 in
  let jal = check_opcode 0x1B in
  let store = check_opcode 0x8 in
  let load = check_opcode 0 in

  let zero7 = Signal.zero 7 in
  let _327 = Signal.of_string "7'd32" in
  let i3 = Signal.of_int ~width:3 in
  let addr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 0) in
  let sub = r_type &: (funct7 ==: _327) &: (funct3 ==: i3 0) in
  let sllr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 1) in
  let sltr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 2) in
  let sltur = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 3) in
  let xorr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 4) in
  let srlr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 5) in
  let srar = r_type &: (funct7 ==: _327) &: (funct3 ==: i3 5) in
  let orr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 6) in
  let andr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 7) in

  let addi = i_type &: (funct3 ==: i3 0) in
  let slli = i_type &: (funct3 ==: i3 1) in
  let slti = i_type &: (funct3 ==: i3 2) in
  let sltui = i_type &: (funct3 ==: i3 3) in
  let xori = i_type &: (funct3 ==: i3 4) in
  let srli =
    i_type &: (funct3 ==: i3 5) &: (imm12.:[(11, 5)] ==: Signal.of_string "7'd0")
  in
  let srai =
    i_type
    &: (funct3 ==: i3 5)
    &: (imm12.:[(11, 5)] ==: Signal.of_string "7'd32")
  in
  let ori = i_type &: (funct3 ==: i3 6) in
  let andi = i_type &: (funct3 ==: i3 7) in

  let next_pc_sel = (jal |: jalr) @: (b_type |: jalr) in

  (* here for consistency *)
  let add = addi |: addr |: load |: store |: jalr in
  ignore add;
  let sll = sllr |: slli in
  let srl = srlr |: srli in
  let sra = srar |: srai in
  let or_ = orr |: ori in
  let and_ = andr |: andi in
  let slt = sltr |: slti in
  let sltu = sltur |: sltui in
  let xor = xorr |: xori in

  let alu_op0 = sub |: or_ |: sll |: sra |: sltu |: aui in
  let alu_op1 = and_ |: or_ |: srl |: sra |: lui |: aui in
  let alu_op2 = xor |: sll |: srl |: sra in
  let alu_op3 = slt |: sltu |: lui |: aui in

  let alu_op = alu_op3 @: alu_op2 @: alu_op1 @: alu_op0 in
  let alu_b_src = (lui |: aui |: store) @: (i_type |: load |: store |: jalr) in

  let writeback_sel = (jal |: jalr) @: load in
  let writeback_enable = Signal.( ~: ) (store |: b_type) in

  {
    O.alu_op;
    alu_a_src = aui;
    alu_b_src;
    mem_write_enable = store;
    writeback_sel;
    writeback_enable;
    next_pc_sel;
    branch_cmp_op = funct3;
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"control" circuit input
