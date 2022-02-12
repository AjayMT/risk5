open Hardcaml
open Hardcaml_waveterm
module Simulator =
  Cyclesim.With_interface (Risk5.Writeback.I) (Risk5.Writeback.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Writeback.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step alu_input mem_input mem_select mem_width =
    inputs.alu_input := Bits.of_string alu_input;
    inputs.mem_input := Bits.of_string mem_input;
    inputs.mem_width := Bits.of_int ~width:3 mem_width;
    inputs.mem_select := Bits.of_int ~width:1 mem_select;
    Cyclesim.cycle sim
  in

  step "32'hDEADBEEF" "32'h0" 0 0;
  step "32'h0" "32'hC0FFEE" 1 2;
  step "32'h0" "32'hFEEDFACE" 1 1;
  step "32'h0" "32'h12345678" 1 0;
  step "32'h0" "32'hFFFFFFF0" 1 2;
  step "32'h0" "32'hFFFFFFF0" 1 1;
  step "32'h0" "32'hFFFFFFF0" 1 4;
  step "32'h0" "32'hFFFFFFF0" 1 5;

  waves

let%expect_test "writeback" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││──────────┬─────────────────────────────────────────────────────────────────────                                                          │
    │alu_input         ││ DEADBEEF │00000000                                                                                                                       │
    │                  ││──────────┴─────────────────────────────────────────────────────────────────────                                                          │
    │                  ││──────────┬─────────┬─────────┬─────────┬───────────────────────────────────────                                                          │
    │mem_input         ││ 00000000 │00C0FFEE │FEEDFACE │12345678 │FFFFFFF0                                                                                         │
    │                  ││──────────┴─────────┴─────────┴─────────┴───────────────────────────────────────                                                          │
    │mem_select        ││          ┌─────────────────────────────────────────────────────────────────────                                                          │
    │                  ││──────────┘                                                                                                                               │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                          │
    │mem_width         ││ 0        │2        │1        │0        │2        │1        │4        │5                                                                  │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                          │
    │                  ││──────────┬─────────┬─────────┬─────────┬───────────────────┬─────────┬─────────                                                          │
    │writeback_data    ││ DEADBEEF │00C0FFEE │FFFFFACE │00000078 │FFFFFFF0           │000000F0 │0000FFF0                                                           │
    │                  ││──────────┴─────────┴─────────┴─────────┴───────────────────┴─────────┴─────────                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
