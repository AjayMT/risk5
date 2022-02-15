open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { instruction : 'a; [@bits 32] pc : 'a [@bits 32] }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    sext_imm12 : 'a; [@bits 32]
    sext_imm20 : 'a; [@bits 32]
    sext_imm_store : 'a; [@bits 32]
    pc_plus_4 : 'a; [@bits 32]
    branch_target : 'a; [@bits 32]
    jal_target : 'a; [@bits 32]
    mem_width : 'a; [@bits 3]
  }
  [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let funct3 = input.instruction.:[(14, 12)] in
  let imm12 = input.instruction.:[(31, 20)] in
  let imm20 = input.instruction.:[(31, 12)] in
  let imm7 = input.instruction.:[(31, 25)] in
  let imm4 = input.instruction.:[(11, 7)] in
  let imm_store = imm7 @: imm4 in

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

  {
    O.sext_imm12 = Signal.sresize imm12 32;
    sext_imm20 = Signal.sresize imm20 32;
    sext_imm_store = Signal.sresize imm_store 32;
    pc_plus_4;
    branch_target;
    jal_target;
    mem_width = funct3;
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decode" circuit input
