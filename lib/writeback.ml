open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = {
    alu_input : 'a; [@bits 32]
    mem_input : 'a; [@bits 32]
    mem_select : 'a; [@bits 1]
    mem_width : 'a; [@bits 3]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { writeback_data : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let mem_output =
    Signal.mux input.mem_width
      [
        Signal.sresize input.mem_input.:[(7, 0)] 32;
        Signal.sresize input.mem_input.:[(15, 0)] 32;
        input.mem_input;
        Signal.zero 32;
        Signal.uresize input.mem_input.:[(7, 0)] 32;
        Signal.uresize input.mem_input.:[(15, 0)] 32;
      ]
  in
  {
    O.writeback_data =
      Signal.mux input.mem_select [ input.alu_input; mem_output ];
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"writeback" circuit input
