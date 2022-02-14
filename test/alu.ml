open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Alu.I) (Risk5.Alu.O)

let testbench which =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Alu.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step a b op =
    inputs.a := Bits.of_string a;
    inputs.b := Bits.of_string b;
    inputs.op := Bits.of_constant @@ Signal.to_constant op;
    Cyclesim.cycle sim
  in

  let open Risk5.Decode.Alu_ops in
  (match which with
  | `first ->
      step "32'h1" "32'h2" add;
      step "32'hFFFFFFFE" "32'h2" add;
      step "32'hF" "32'h1" sub;
      step "32'h1" "32'h0" sub;
      step "32'h0" "32'h1" sub;
      step "32'hFF" "32'h3" and_;
      step "32'hF0" "32'hF" or_;
      step "32'hF" "32'h3" xor
  | `second ->
      step "32'h3" "32'h2" sll;
      step "32'hFFFFFFFF" "32'h2" srl;
      step "32'hFFFFFFFF" "32'h2" sra;
      step "32'hFFFFFFF" "32'h3" sra;
      step "32'h1" "32'h2" slt;
      step "32'h2" "32'h2" slt;
      step "32'hFFFFFFFF" "32'h0" slt;
      step "32'h1" "32'h2" sltu;
      step "32'hFFFFFFFF" "32'h0" sltu;
      step "32'hACDEF" "32'hAAAAFFFF" lui;
      step "32'h2" "32'hAAAAFFFF" aui);

  waves

let%expect_test "first" =
  let waves = testbench `first in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                          │
    │a                 ││ 00000001 │FFFFFFFE │0000000F │00000001 │00000000 │000000FF │000000F0 │0000000F                                                           │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                          │
    │                  ││────────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                          │
    │b                 ││ 00000002           │00000001 │00000000 │00000001 │00000003 │0000000F │00000003                                                           │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                          │
    │                  ││────────────────────┬─────────────────────────────┬─────────┬─────────┬─────────                                                          │
    │op                ││ 0                  │1                            │2        │3        │4                                                                  │
    │                  ││────────────────────┴─────────────────────────────┴─────────┴─────────┴─────────                                                          │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                          │
    │result            ││ 00000003 │00000000 │0000000E │00000001 │FFFFFFFF │00000003 │000000FF │0000000C                                                           │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]

let%expect_test "second" =
  let waves = testbench `second in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││──────────┬───────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                            │
    │a                 ││ 00000003 │FFFFFFFF           │0FFFFFFF │00000001 │00000002 │FFFFFFFF │00000001 │FFFFFFFF │000ACDEF │00000002                             │
    │                  ││──────────┴───────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                            │
    │                  ││──────────────────────────────┬─────────┬───────────────────┬─────────┬─────────┬─────────┬───────────────────                            │
    │b                 ││ 00000002                     │00000003 │00000002           │00000000 │00000002 │00000000 │AAAAFFFF                                       │
    │                  ││──────────────────────────────┴─────────┴───────────────────┴─────────┴─────────┴─────────┴───────────────────                            │
    │                  ││──────────┬─────────┬───────────────────┬─────────────────────────────┬───────────────────┬─────────┬─────────                            │
    │op                ││ 5        │6        │7                  │8                            │9                  │A        │B                                    │
    │                  ││──────────┴─────────┴───────────────────┴─────────────────────────────┴───────────────────┴─────────┴─────────                            │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────────────────┬─────────┬─────────┬─────────                            │
    │result            ││ 0000000C │3FFFFFFF │FFFFFFFF │01FFFFFF │00000001 │00000000 │00000001           │00000000 │AFFFF000 │AFFFF002                             │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────────────────┴─────────┴─────────┴─────────                            │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    │                  ││                                                                                                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
