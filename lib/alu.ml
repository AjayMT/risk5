open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = { a : 'a; [@bits 32] b : 'a; [@bits 32] op : 'a [@bits 4] }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { result : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let result = Always.Variable.wire ~default:(of_string "32'b0") in
  let a, b, op = (input.a, input.b, input.op) in
  let open Always in
  compile
    [
      switch op
        [
          (Decode.Alu_ops.add, [ result <-- a +: b ]);
          (Decode.Alu_ops.sub, [ result <-- a -: b ]);
          (Decode.Alu_ops.and_, [ result <-- (a &: b) ]);
          (Decode.Alu_ops.or_, [ result <-- (a |: b) ]);
          (Decode.Alu_ops.xor, [ result <-- a ^: b ]);
          (Decode.Alu_ops.sll, [ result <-- log_shift Signal.sll b a ]);
          (Decode.Alu_ops.srl, [ result <-- log_shift Signal.srl b a ]);
          (Decode.Alu_ops.sra, [ result <-- log_shift Signal.sra b a ]);
          (Decode.Alu_ops.slt, [ result <-- uresize (a <+ b) 32 ]);
          (Decode.Alu_ops.sltu, [ result <-- uresize (a <: b) 32 ]);
          (Decode.Alu_ops.lui, [ result <-- b.:[(19, 0)] @: of_string "12'h0" ]);
          ( Decode.Alu_ops.aui,
            [ result <-- (b.:[(19, 0)] @: of_string "12'h0") +: a ] );
        ];
    ];
  { O.result = Always.Variable.value result }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"alu" circuit input