open Hardcaml
open Hardcaml_waveterm
module Simulator =
  Cyclesim.With_interface (Risk5.Writeback.I) (Risk5.Writeback.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Writeback.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step ~alu ~mem ~width ~pc ~sel ~enable =
    inputs.alu_data := Bits.of_string alu;
    inputs.mem_data := Bits.of_string mem;
    inputs.mem_width := Bits.of_int ~width:3 width;
    inputs.pc_data := Bits.of_string pc;
    inputs.alu_mem_pc_select := Bits.of_int ~width:2 sel;
    inputs.enable := Bits.of_int ~width:1 enable;
    Cyclesim.cycle sim
  in

  step ~alu:"32'hDEADBEEF" ~mem:"32'h0" ~pc:"32'h0" ~width:0 ~sel:0 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'h0" ~pc:"32'hEFFFEDAC" ~width:0 ~sel:2 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'hC0FFEE" ~pc:"32'h0" ~width:2 ~sel:1 ~enable:0;
  step ~alu:"32'h0" ~mem:"32'hFEEDFACE" ~pc:"32'h0" ~width:1 ~sel:1 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'h12345678" ~pc:"32'h0" ~width:0 ~sel:1 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'hFFFFFFF0" ~pc:"32'h0" ~width:2 ~sel:1 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'hFFFFFFF0" ~pc:"32'h0" ~width:1 ~sel:1 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'hFFFFFFF0" ~pc:"32'h0" ~width:4 ~sel:1 ~enable:1;
  step ~alu:"32'h0" ~mem:"32'hFFFFFFF0" ~pc:"32'h0" ~width:5 ~sel:1 ~enable:1;

  waves

let%expect_test "writeback" =
  let waves = testbench () in
  Waveform.print ~display_height:26 ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │enable            ││────────────────────┐         ┌───────────────────────────────────────────────────────────                                                │
    │                  ││                    └─────────┘                                                                                                           │
    │                  ││──────────┬───────────────────────────────────────────────────────────────────────────────                                                │
    │alu_data          ││ DEADBEEF │00000000                                                                                                                       │
    │                  ││──────────┴───────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││──────────┬─────────┬─────────────────────────────────────────────────────────────────────                                                │
    │alu_mem_pc_select ││ 0        │2        │1                                                                                                                    │
    │                  ││──────────┴─────────┴─────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────────────────┬─────────┬─────────┬─────────┬───────────────────────────────────────                                                │
    │mem_data          ││ 00000000           │00C0FFEE │FEEDFACE │12345678 │FFFFFFF0                                                                               │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴───────────────────────────────────────                                                │
    │                  ││────────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                │
    │mem_width         ││ 0                  │2        │1        │0        │2        │1        │4        │5                                                        │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                │
    │                  ││──────────┬─────────┬─────────────────────────────────────────────────────────────────────                                                │
    │pc_data           ││ 00000000 │EFFFEDAC │00000000                                                                                                             │
    │                  ││──────────┴─────────┴─────────────────────────────────────────────────────────────────────                                                │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬───────────────────┬─────────┬─────────                                                │
    │writeback_data    ││ DEADBEEF │EFFFEDAC │00C0FFEE │FFFFFACE │00000078 │FFFFFFF0           │000000F0 │0000FFF0                                                 │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴───────────────────┴─────────┴─────────                                                │
    │writeback_enable  ││────────────────────┐         ┌───────────────────────────────────────────────────────────                                                │
    │                  ││                    └─────────┘                                                                                                           │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
