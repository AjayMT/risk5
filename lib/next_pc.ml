open Hardcaml

module I = struct
  type 'a t = {
    pc_plus_4 : 'a; [@bits 32]
    branch_target : 'a; [@bits 32]
    jal_target : 'a; [@bits 32]
    jalr_target : 'a; [@bits 32]
    next_pc_sel : 'a; [@bits 2]
    branch_cmp_result : 'a; [@bits 1]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { next_pc : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let branch_target =
    Signal.mux input.branch_cmp_result [ input.pc_plus_4; input.branch_target ]
  in
  {
    O.next_pc =
      Signal.mux input.next_pc_sel
        [ input.pc_plus_4; branch_target; input.jal_target; input.jalr_target ];
  }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"next_pc" circuit input
