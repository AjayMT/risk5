open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { clock : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
end

module O = Writeback.O

let circuit program scope (input : _ I.t) =
  let fetch_input = { Fetch.I.clock = input.clock; next_pc = Signal.wire 32 } in
  let fetch = Fetch.hierarchical program scope fetch_input in

  let decode_input =
    {
      Decode.I.instruction = fetch.instruction;
      clock = input.clock;
      writeback_data = Signal.wire 32;
      writeback_enable = Signal.wire 1;
      pc = fetch.pc;
    }
  in
  let decode = Decode.hierarchical scope decode_input in

  let alu_input =
    { Alu.I.a = decode.alu_a; b = decode.alu_b; op = decode.alu_op }
  in
  let alu = Alu.hierarchical scope alu_input in

  let cmp_input =
    {
      Cmp.I.a = decode.branch_cmp_a;
      b = decode.branch_cmp_b;
      op = decode.branch_cmp_op;
    }
  in
  let cmp = Cmp.hierarchical scope cmp_input in

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

  let next_pc_input =
    {
      Next_pc.I.pc_plus_4 = decode.next_pc_pc_plus_4;
      branch_target = decode.next_pc_branch_target;
      jal_target = decode.next_pc_jal_target;
      jalr_target = alu.result.:[(31, 1)] @: Signal.gnd;
      next_pc_sel = decode.next_pc_sel;
      branch_cmp_result = cmp.result;
    }
  in
  let next_pc = Next_pc.hierarchical scope next_pc_input in

  fetch_input.next_pc <== next_pc.next_pc;

  let writeback_input =
    {
      Writeback.I.alu_data = alu.result;
      mem_data = mem.read_data;
      mem_width = decode.writeback_mem_width;
      pc_data = decode.next_pc_pc_plus_4;
      alu_mem_pc_select = decode.writeback_alu_mem_pc_select;
      enable = decode.writeback_enable;
    }
  in
  let writeback = Writeback.hierarchical scope writeback_input in

  decode_input.writeback_data <== writeback.writeback_data;
  decode_input.writeback_enable <== writeback.writeback_enable;

  writeback

let hierarchical program scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decode" (circuit program) input
