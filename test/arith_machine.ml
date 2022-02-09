open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Machine.I) (Risk5.Machine.O)

let program =
  let prog_text = [%blob "arith_machine_program/prog.txt"] in
  let prog_lines =
    List.filter (fun s -> String.length s > 0)
    @@ String.split_on_char '\n' prog_text
  in
  List.map (fun line -> Signal.of_string @@ "32'h" ^ line) prog_lines

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Machine.circuit program scope) in
  let waves, sim = Waveform.create sim in

  List.iter (fun _ -> Cyclesim.cycle sim) program;

  waves

let%expect_test "arith_machine" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:160 waves;

  [%expect
    {|
       ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
       │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
       │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
       │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────                                                          │
       │writeback_data    ││ 0000000D │0000001A │01234000 │0ABCD00C │00000001 │FFFFFFFE │00000002 │FFFFFFFF                                                           │
       │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────                                                          │
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
       │                  ││                                                                                                                                          │
       └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
