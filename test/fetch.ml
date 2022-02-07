open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Fetch.I) (Risk5.Fetch.O)

let program = List.map (Signal.of_int ~width:32) [ 1; 2; 3; 4 ]

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Fetch.circuit program scope) in
  let waves, sim = Waveform.create sim in

  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;

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
    │instruction       ││ 00000001 │00000002 │00000003 │00000004                                                                                                   │
    │                  ││──────────┴─────────┴─────────┴─────────                                                                                                  │
    │                  ││──────────┬─────────┬─────────┬─────────                                                                                                  │
    │pc                ││ 00000000 │00000004 │00000008 │0000000C                                                                                                   │
    │                  ││──────────┴─────────┴─────────┴─────────                                                                                                  │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘|}]
