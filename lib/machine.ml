open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { clock : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { writeback_data : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit program scope (input : _ I.t) =
  let fetch_input =
    {
      Fetch.I.clock = input.clock;
      branch_target = Signal.wire 32;
      pc_sel = Signal.wire 1;
    }
  in
  let fetch = Fetch.hierarchical program scope fetch_input in

  let decode_input =
    {
      Decode.I.instruction = fetch.instruction;
      clock = input.clock;
      writeback_data = Signal.wire 32;
      pc = fetch.pc;
    }
  in
  let decode = Decode.hierarchical scope decode_input in

  fetch_input.branch_target <== decode.branch_target;
  fetch_input.pc_sel <== decode.pc_sel;

  let alu_input =
    { Alu.I.a = decode.alu_a; b = decode.alu_b; op = decode.alu_op }
  in
  let alu = Alu.hierarchical scope alu_input in

  let mem_input =
    {
      Memory.I.clock = input.clock;
      address = alu.result;
      write_width = decode.mem_write_width;
      write_data = decode.mem_write_data;
      write_enable = decode.mem_write_enable;
    }
  in
  let mem = Memory.hierarchical 512 scope mem_input in

  let writeback_input =
    {
      Writeback.I.alu_input = alu.result;
      mem_input = mem.read_data;
      mem_select = decode.writeback_mem;
      mem_width = decode.writeback_width;
    }
  in
  let writeback = Writeback.hierarchical scope writeback_input in

  decode_input.writeback_data <== writeback.writeback_data;

  { O.writeback_data = writeback.writeback_data }

let hierarchical program scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decode" (circuit program) input
