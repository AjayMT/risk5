open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = {
    alu_data : 'a; [@bits 32]
    mem_data : 'a; [@bits 32]
    mem_width : 'a; [@bits 3]
    pc_data : 'a; [@bits 32]
    alu_mem_pc_select : 'a; [@bits 2]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { write_data : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let mem_output =
    Signal.mux input.mem_width
      [
        Signal.sresize input.mem_data.:[(7, 0)] 32;
        Signal.sresize input.mem_data.:[(15, 0)] 32;
        input.mem_data;
        Signal.zero 32;
        Signal.uresize input.mem_data.:[(7, 0)] 32;
        Signal.uresize input.mem_data.:[(15, 0)] 32;
      ]
  in
  {
    O.write_data =
      Signal.mux input.alu_mem_pc_select
        [ input.alu_data; mem_output; input.pc_data ];
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"writeback" circuit input
