open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = {
    read_reg_1 : 'a; [@bits 5]
    read_reg_2 : 'a; [@bits 5]
    write_reg : 'a; [@bits 5]
    write_data : 'a; [@bits 32]
    write_enable : 'a; [@bits 1]
    clock : 'a; [@bits 1]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { read_data_1 : 'a; [@bits 32] read_data_2 : 'a [@bits 32] }
  [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let write_port =
    {
      write_clock = input.clock;
      write_data = input.write_data;
      write_enable = input.write_enable;
      write_address = input.write_reg;
    }
  in
  let regfile =
    Signal.multiport_memory 32 ~write_ports:[| write_port |]
      ~read_addresses:[| input.read_reg_1; input.read_reg_2 |]
  in
  let rd1 =
    Signal.mux
      (input.read_reg_1 ==: Signal.zero 5)
      [ regfile.(0); Signal.zero 32 ]
  in
  let rd2 =
    Signal.mux
      (input.read_reg_2 ==: Signal.zero 5)
      [ regfile.(1); Signal.zero 32 ]
  in
  { O.read_data_1 = rd1; O.read_data_2 = rd2 }

let hierarchical scope input =
  let module H = Hardcaml.Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"regfile" circuit input
