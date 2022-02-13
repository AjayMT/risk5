open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { a : 'a; [@bits 32] b : 'a; [@bits 32] op : 'a [@bits 3] }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { result : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let results =
    [
      input.a ==: input.b;
      input.a <>: input.b;
      (* op 2, 3 are undefined *)
      Signal.gnd;
      Signal.gnd;
      input.a <+ input.b;
      input.a >=+ input.b;
      input.a <: input.b;
      input.a >=: input.b;
    ]
  in
  { O.result = Signal.mux input.op results }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"cmp" circuit input
