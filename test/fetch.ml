open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Fetch.I) (Risk5.Fetch.O)

let program = List.map (Signal.of_int ~width:32) [ 1; 2; 3; 4 ]

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Fetch.circuit program scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step next_pc =
    inputs.next_pc := Bits.of_int ~width:32 next_pc;
    Cyclesim.cycle sim
  in

  step 0;
  step 3;
  step 1;
  step 2;

  waves

let%expect_test "fetch" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
    │                  ││──────────┬─────────┬─────────┬─────────                                                                                                  │
    │next_pc           ││ 00000000 │00000003 │00000001 │00000002                                                                                                   │
    │                  ││──────────┴─────────┴─────────┴─────────                                                                                                  │
    │                  ││────────────────────────────────────────                                                                                                  │
    │instruction       ││ 00000001                                                                                                                                 │
    │                  ││────────────────────────────────────────                                                                                                  │
    │                  ││────────────────────┬─────────┬─────────                                                                                                  │
    │pc                ││ 00000000           │00000003 │00000001                                                                                                   │
    │                  ││────────────────────┴─────────┴─────────                                                                                                  │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘|}]
