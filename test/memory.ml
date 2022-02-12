open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Memory.I) (Risk5.Memory.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Memory.circuit 8 scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step ~address ~wwidth ~wdata ~wenable =
    inputs.address := Bits.of_int ~width:32 address;
    inputs.write_data := Bits.of_string wdata;
    inputs.write_enable := Bits.of_int ~width:1 wenable;
    inputs.write_width := Bits.of_int ~width:3 wwidth;
    Cyclesim.cycle sim
  in

  step ~address:0 ~wwidth:0 ~wdata:"32'h0" ~wenable:0;
  step ~address:1 ~wwidth:2 ~wdata:"32'hDEADBEEF" ~wenable:1;
  step ~address:1 ~wwidth:0 ~wdata:"32'h0" ~wenable:0;
  step ~address:2 ~wwidth:0 ~wdata:"32'h0" ~wenable:0;
  step ~address:0 ~wwidth:0 ~wdata:"32'h0" ~wenable:0;
  step ~address:3 ~wwidth:0 ~wdata:"32'h37" ~wenable:1;
  step ~address:1 ~wwidth:0 ~wdata:"32'h0" ~wenable:0;
  step ~address:2 ~wwidth:1 ~wdata:"32'h5885" ~wenable:1;
  step ~address:1 ~wwidth:0 ~wdata:"32'h0" ~wenable:0;

  waves

let%expect_test "memory" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_height:24 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
    │                  ││──────────┬───────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                │
    │address           ││ 00000000 │00000001           │00000002 │00000000 │00000003 │00000001 │00000002 │00000001                                                 │
    │                  ││──────────┴───────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                │
    │                  ││──────────┬─────────┬─────────────────────────────┬─────────┬─────────┬─────────┬─────────                                                │
    │write_data        ││ 00000000 │DEADBEEF │00000000                     │00000037 │00000000 │00005885 │00000000                                                 │
    │                  ││──────────┴─────────┴─────────────────────────────┴─────────┴─────────┴─────────┴─────────                                                │
    │write_enable      ││          ┌─────────┐                             ┌─────────┐         ┌─────────┐                                                         │
    │                  ││──────────┘         └─────────────────────────────┘         └─────────┘         └─────────                                                │
    │                  ││──────────┬─────────┬─────────────────────────────────────────────────┬─────────┬─────────                                                │
    │write_width       ││ 0        │2        │0                                                │1        │0                                                        │
    │                  ││──────────┴─────────┴─────────────────────────────────────────────────┴─────────┴─────────                                                │
    │                  ││────────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                │
    │read_data         ││ 00000000           │DEADBEEF │00DEADBE │ADBEEF00 │0000DEAD │DE37BEEF │00DE37BE │DE5885EF                                                 │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
