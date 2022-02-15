open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { clock : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    write_data : 'a; [@bits 32]
    write_enable : 'a; [@bits 32]
    pc : 'a; [@bits 32]
  }
  [@@deriving sexp_of, hardcaml]
end

let circuit program scope (input : _ I.t) =
  let fetch_input = { Fetch.I.clock = input.clock; next_pc = Signal.wire 32 } in
  let fetch = Fetch.hierarchical program scope fetch_input in

  let decode_input =
    { Decode.I.instruction = fetch.instruction; pc = fetch.pc }
  in
  let decode = Decode.hierarchical scope decode_input in

  let control_input = { Control.I.instruction = fetch.instruction } in
  let control = Control.hierarchical scope control_input in

  let regfile_input =
    {
      Regfile.I.read_reg_1 = fetch.instruction.:[(19, 15)];
      read_reg_2 = fetch.instruction.:[(24, 20)];
      write_reg = fetch.instruction.:[(11, 7)];
      write_data = Signal.wire 32;
      write_enable = Signal.wire 1;
      clock = input.clock;
    }
  in
  let regfile = Regfile.hierarchical scope regfile_input in

  regfile_input.write_enable <== control.writeback_enable;

  let alu_input =
    {
      Alu.I.a = Signal.mux control.alu_a_src [ regfile.read_data_1; fetch.pc ];
      b =
        Signal.mux control.alu_b_src
          [
            regfile.read_data_2;
            decode.sext_imm12;
            decode.sext_imm20;
            decode.sext_imm_store;
          ];
      op = control.alu_op;
    }
  in
  let alu = Alu.hierarchical scope alu_input in

  let cmp_input =
    {
      Cmp.I.a = regfile.read_data_1;
      b = regfile.read_data_2;
      op = control.branch_cmp_op;
    }
  in
  let cmp = Cmp.hierarchical scope cmp_input in

  let next_pc_input =
    {
      Next_pc.I.pc_plus_4 = decode.pc_plus_4;
      branch_target = decode.branch_target;
      jal_target = decode.jal_target;
      jalr_target = alu.result.:[(31, 1)] @: Signal.gnd;
      next_pc_sel = control.next_pc_sel;
      branch_cmp_result = cmp.result;
    }
  in
  let next_pc = Next_pc.hierarchical scope next_pc_input in

  fetch_input.next_pc <== next_pc.next_pc;

  let mem_input =
    {
      Memory.I.clock = input.clock;
      address = alu.result;
      write_width = decode.mem_width;
      write_data = regfile.read_data_2;
      write_enable = control.mem_write_enable;
    }
  in
  let mem = Memory.hierarchical 512 scope mem_input in

  let writeback_input =
    {
      Writeback.I.alu_data = alu.result;
      mem_data = mem.read_data;
      mem_width = decode.mem_width;
      pc_data = decode.pc_plus_4;
      alu_mem_pc_select = control.writeback_sel;
    }
  in
  let writeback = Writeback.hierarchical scope writeback_input in

  regfile_input.write_data <== writeback.write_data;

  {
    O.write_data = writeback.write_data;
    write_enable = control.writeback_enable;
    pc = fetch.pc;
  }

let hierarchical program scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"machine" (circuit program) input
