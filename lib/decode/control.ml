module I = struct
  type 'a t = { instruction : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { alu_op : 'a [@bits 4] } [@@deriving sexp_of, hardcaml]
end

let alu_op_of_int = Hardcaml.Signal.of_int ~width:4

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

let circuit _ _ = { O.alu_op = Alu_ops.add }

let hierarchical scope input =
  let module H = Hardcaml.Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"control" circuit input
