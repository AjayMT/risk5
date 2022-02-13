open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Machine.I) (Risk5.Machine.O)

let program =
  let prog_text = [%blob "test_programs/branch_jump.txt"] in
  let prog_lines =
    List.filter (fun s -> String.length s > 0)
    @@ String.split_on_char '\n' prog_text
  in
  List.map (fun line -> Signal.of_string @@ "32'h" ^ line) prog_lines

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Machine.circuit program scope) in
  let waves, sim = Waveform.create sim in

  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;

  waves

let%expect_test "branch_jump" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
       ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
       │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
       │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
       │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                            │
       │writeback_data    ││ 00000000 │00000008 │000000DF │00000014 │00000000 │00000008 │000000DF │00000014 │00000000 │00000008 │000000DF                             │
       │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                            │
       │writeback_enable  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────────                            │
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
       │                  ││                                                                                                                                          │
       │                  ││                                                                                                                                          │
       └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
