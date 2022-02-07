open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = {
    instruction : 'a; [@bits 32]
    clock : 'a; [@bits 1]
    writeback_data : 'a; [@bits 32]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    alu_op : 'a; [@bits 4]
    alu_a : 'a; [@bits 32]
    alu_b : 'a; [@bits 32]
  }
  [@@deriving sexp_of, hardcaml]
end

let alu_op_of_int = Signal.of_int ~width:4

module Alu_ops = struct
  let add = alu_op_of_int 0

  let sub = alu_op_of_int 1

  let and_ = alu_op_of_int 2

  let or_ = alu_op_of_int 3

  let xor = alu_op_of_int 4

  let sll = alu_op_of_int 5

  let srl = alu_op_of_int 6

  let sra = alu_op_of_int 7

  let slt = alu_op_of_int 8

  let sltu = alu_op_of_int 9

  let lui = alu_op_of_int 10

  let aui = alu_op_of_int 11
end

let circuit scope (input : _ I.t) =
  let regfile_input =
    {
      Regfile.I.clock = input.clock;
      read_reg_1 = input.instruction.:[(15, 19)];
      read_reg_2 = input.instruction.:[(20, 24)];
      write_reg = input.instruction.:[(7, 11)];
      write_data = input.writeback_data;
      write_enable = Signal.vdd;
    }
  in
  let regfile = Regfile.hierarchical scope regfile_input in

  {
    O.alu_op = Alu_ops.add;
    alu_a = regfile.read_data_1;
    alu_b = regfile.read_data_2;
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decode" circuit input
