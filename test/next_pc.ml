open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Next_pc.I) (Risk5.Next_pc.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Next_pc.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step ~pc ~branch ~jal ~jalr ~sel ~res =
    inputs.pc_plus_4 := Bits.of_int ~width:32 pc;
    inputs.branch_target := Bits.of_int ~width:32 branch;
    inputs.jal_target := Bits.of_int ~width:32 jal;
    inputs.jalr_target := Bits.of_int ~width:32 jalr;
    inputs.next_pc_sel := Bits.of_int ~width:2 sel;
    inputs.branch_cmp_result := Bits.of_int ~width:1 res;
    Cyclesim.cycle sim
  in

  step ~pc:0 ~branch:1 ~jal:2 ~jalr:3 ~sel:0 ~res:1;
  step ~pc:0 ~branch:1 ~jal:2 ~jalr:3 ~sel:1 ~res:1;
  step ~pc:0 ~branch:1 ~jal:2 ~jalr:3 ~sel:2 ~res:1;
  step ~pc:0 ~branch:1 ~jal:2 ~jalr:3 ~sel:3 ~res:1;
  step ~pc:0 ~branch:1 ~jal:2 ~jalr:3 ~sel:1 ~res:0;

  waves

let%expect_test "cmp" =
  let waves = testbench () in
  Waveform.print ~display_height:24 ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │branch_cmp_result ││────────────────────────────────────────┐                                                                                                 │
    │                  ││                                        └─────────                                                                                        │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │branch_target     ││ 00000001                                                                                                                                 │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │jal_target        ││ 00000002                                                                                                                                 │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │jalr_target       ││ 00000003                                                                                                                                 │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────                                                                                        │
    │next_pc_sel       ││ 0        │1        │2        │3        │1                                                                                                │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────                                                                                        │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │pc_plus_4         ││ 00000000                                                                                                                                 │
    │                  ││──────────────────────────────────────────────────                                                                                        │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────                                                                                        │
    │next_pc           ││ 00000000 │00000001 │00000002 │00000003 │00000000                                                                                         │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────                                                                                        │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
