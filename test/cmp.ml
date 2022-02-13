open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Cmp.I) (Risk5.Cmp.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Cmp.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step a b op =
    inputs.a := Bits.of_int ~width:32 a;
    inputs.b := Bits.of_int ~width:32 b;
    inputs.op := Bits.of_int ~width:3 op;
    Cyclesim.cycle sim
  in

  step 1 1 0;
  step 1 2 0;
  step 3 4 1;
  step (-1) 1 4;
  step 2 (-1) 4;
  step (-1) 1 6;
  step 1 (-1) 6;
  step (-1) 1 7;
  step 2 (-1) 7;
  step (-1) 1 5;
  step 1 (-1) 5;

  waves

let%expect_test "cmp" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                            │
    │a                 ││ 00000001           │00000003 │FFFFFFFF │00000002 │FFFFFFFF │00000001 │FFFFFFFF │00000002 │FFFFFFFF │00000001                             │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                            │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                            │
    │b                 ││ 00000001 │00000002 │00000004 │00000001 │FFFFFFFF │00000001 │FFFFFFFF │00000001 │FFFFFFFF │00000001 │FFFFFFFF                             │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                            │
    │                  ││────────────────────┬─────────┬───────────────────┬───────────────────┬───────────────────┬───────────────────                            │
    │op                ││ 0                  │1        │4                  │6                  │7                  │5                                              │
    │                  ││────────────────────┴─────────┴───────────────────┴───────────────────┴───────────────────┴───────────────────                            │
    │result            ││──────────┐         ┌───────────────────┐                   ┌───────────────────┐                   ┌─────────                            │
    │                  ││          └─────────┘                   └───────────────────┘                   └───────────────────┘                                     │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
