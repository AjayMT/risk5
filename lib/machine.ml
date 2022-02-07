open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { clock : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { writeback_data : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit program scope (input : _ I.t) =
  let fetch_input = { Fetch.I.clock = input.clock } in
  let fetch = Fetch.hierarchical program scope fetch_input in

  let decode_input =
    {
      Decode.I.instruction = fetch.instruction;
      clock = input.clock;
      writeback_data = Signal.wire 32;
    }
  in
  let decode = Decode.hierarchical scope decode_input in

  let alu_input =
    { Alu.I.a = decode.alu_a; b = decode.alu_b; op = decode.alu_op }
  in
  let alu = Alu.hierarchical scope alu_input in

  decode_input.writeback_data <== alu.result;

  { O.writeback_data = alu.result }

let hierarchical program scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decode" (circuit program) input
