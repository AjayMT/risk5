open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5__Alu.I) (Risk5__Alu.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5__Alu.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step a b op =
    inputs.a := Bits.of_string a;
    inputs.b := Bits.of_string b;
    inputs.op := Bits.of_constant @@ Signal.to_constant op;
    Cyclesim.cycle sim
  in

  let open Risk5__Control.Alu_ops in
  step "32'h1" "32'h2" add;

  waves

let%expect_test "addition" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││──────────                                                                                                                                │
    │a                 ││ 00000001                                                                                                                                 │
    │                  ││──────────                                                                                                                                │
    │                  ││──────────                                                                                                                                │
    │b                 ││ 00000002                                                                                                                                 │
    │                  ││──────────                                                                                                                                │
    │                  ││──────────                                                                                                                                │
    │op                ││ 0                                                                                                                                        │
    │                  ││──────────                                                                                                                                │
    │                  ││──────────                                                                                                                                │
    │result            ││ 00000003                                                                                                                                 │
    │                  ││──────────                                                                                                                                │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
