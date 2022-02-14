open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = {
    instruction : 'a; [@bits 32]
    clock : 'a; [@bits 1]
    writeback_data : 'a; [@bits 32]
    writeback_enable : 'a; [@bits 32]
    pc : 'a; [@bits 32]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    alu_op : 'a; [@bits 4]
    alu_a : 'a; [@bits 32]
    alu_b : 'a; [@bits 32]
    mem_write_data : 'a; [@bits 32]
    mem_write_width : 'a; [@bits 3]
    mem_write_enable : 'a; [@bits 1]
    writeback_alu_mem_pc_select : 'a; [@bits 2]
    writeback_mem_width : 'a; [@bits 3]
    writeback_enable : 'a; [@bits 1]
    branch_cmp_a : 'a; [@bits 32]
    branch_cmp_b : 'a; [@bits 32]
    branch_cmp_op : 'a; [@bits 3]
    next_pc_sel : 'a; [@bits 2]
    next_pc_branch_target : 'a; [@bits 32]
    next_pc_jal_target : 'a; [@bits 32]
    next_pc_pc_plus_4 : 'a; [@bits 32]
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

let circuit scope (input : _ I.t) =
  let regfile_input =
    {
      Regfile.I.clock = input.clock;
      (* rs1 *)
      read_reg_1 = input.instruction.:[(19, 15)];
      (* rs2 *)
      read_reg_2 = input.instruction.:[(24, 20)];
      (* rd *)
      write_reg = input.instruction.:[(11, 7)];
      write_data = input.writeback_data;
      write_enable = input.writeback_enable;
    }
  in
  let regfile = Regfile.hierarchical scope regfile_input in

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
   *)
  let opcode = input.instruction.:[(6, 0)] in
  let funct7 = input.instruction.:[(31, 25)] in
  let funct3 = input.instruction.:[(14, 12)] in
  let imm12 = input.instruction.:[(31, 20)] in
  let imm20 = input.instruction.:[(31, 12)] in
  let imm7 = input.instruction.:[(31, 25)] in
  let imm4 = input.instruction.:[(11, 7)] in
  let imm_store = imm7 @: imm4 in

  let sext_imm20 = Signal.sresize imm20 32 in
  let sext_imm12 = Signal.sresize imm12 32 in

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
  let sll = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 1) in
  let sltr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 2) in
  let sltur = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 3) in
  let xorr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 4) in
  let srl = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 5) in
  let sra = r_type &: (funct7 ==: _327) &: (funct3 ==: i3 5) in
  let orr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 6) in
  let andr = r_type &: (funct7 ==: zero7) &: (funct3 ==: i3 7) in

  let addi = i_type &: (funct3 ==: i3 0) in
  let slti = i_type &: (funct3 ==: i3 2) in
  let sltui = i_type &: (funct3 ==: i3 3) in
  let xori = i_type &: (funct3 ==: i3 4) in
  let ori = i_type &: (funct3 ==: i3 6) in
  let andi = i_type &: (funct3 ==: i3 7) in

  let jal_imm20 =
    imm20.:(19) @: imm20.:[(7, 0)] @: imm20.:(8) @: imm20.:[(18, 9)]
  in
  let jal_offset =
    Signal.sresize (Signal.log_shift Signal.sll jal_imm20 Signal.vdd) 32
  in
  let jal_target = input.pc +: jal_offset in
  let branch_imm =
    input.instruction.:(31) @: input.instruction.:(7)
    @: input.instruction.:[(30, 25)]
    @: input.instruction.:[(11, 8)]
  in
  let branch_offset =
    Signal.sresize (Signal.log_shift Signal.sll branch_imm Signal.vdd) 32
  in
  let branch_target = input.pc +: branch_offset in
  let pc_plus_4 = input.pc +: Signal.of_string "32'd4" in

  let next_pc_sel = (jal |: jalr) @: (b_type |: jalr) in

  (* here for consistency *)
  let add = addi |: addr |: load |: store |: jalr in
  ignore add;
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

  let writeback_alu_mem_pc_select = (jal |: jalr) @: load in
  let writeback_enable = Signal.( ~: ) (store |: b_type) in

  {
    O.alu_op;
    alu_a = Signal.mux aui [ regfile.read_data_1; input.pc ];
    alu_b =
      Signal.mux alu_b_src
        [
          regfile.read_data_2;
          sext_imm12;
          sext_imm20;
          Signal.sresize imm_store 32;
        ];
    mem_write_enable = store;
    mem_write_width = funct3;
    mem_write_data = regfile.read_data_2;
    writeback_alu_mem_pc_select;
    writeback_enable;
    writeback_mem_width = funct3;
    branch_cmp_op = funct3;
    branch_cmp_a = regfile.read_data_1;
    branch_cmp_b = regfile.read_data_2;
    next_pc_sel;
    next_pc_branch_target = branch_target;
    next_pc_jal_target = jal_target;
    next_pc_pc_plus_4 = pc_plus_4;
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decode" circuit input
