open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { clock : 'a; [@bits 1] next_pc : 'a [@bits 32] }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { instruction : 'a; [@bits 32] pc : 'a [@bits 32] }
  [@@deriving sexp_of, hardcaml]
end

let circuit program _ (input : _ I.t) =
  let pc_reg = Reg_spec.create ~clock:input.clock () in
  let pc = Signal.wire 32 in
  pc <== Signal.reg pc_reg input.next_pc ~enable:Signal.vdd;

  {
    O.instruction =
      Signal.mux (log_shift srl pc (Signal.of_int ~width:32 2)) program;
    O.pc;
  }

let hierarchical program scope input =
  let module H = Hardcaml.Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"fetch" (circuit program) input
