open Hardcaml
open Hardcaml.Signal

module I = struct
  type 'a t = {
    clock : 'a; [@bits 1]
    address : 'a; [@bits 32]
    write_width : 'a; [@bits 3]
    write_data : 'a; [@bits 32]
    write_enable : 'a; [@bits 1]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { read_data : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let circuit _ (input : _ I.t) =
  let i32 = Signal.of_int ~width:32 in

  let make_write_port width range offset =
    {
      write_clock = input.clock;
      write_data = input.write_data.:[range];
      write_enable =
        input.write_enable
        &: (input.write_width >=: Signal.of_int ~width:3 width);
      write_address = input.address +: i32 offset;
    }
  in

  let write_ports =
    [|
      make_write_port 0 (7, 0) 0;
      make_write_port 1 (15, 8) 1;
      make_write_port 2 (23, 16) 2;
      make_write_port 2 (31, 24) 3;
    |]
  in

  let read_addresses =
    [|
      input.address;
      input.address +: i32 1;
      input.address +: i32 2;
      input.address +: i32 3;
    |]
  in

  let mem = Signal.multiport_memory 8 ~write_ports ~read_addresses in

  { O.read_data = mem.(3) @: mem.(2) @: mem.(1) @: mem.(0) }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory" circuit input
