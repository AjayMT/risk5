open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Machine.I) (Risk5.Machine.O)

let program =
  let prog_text = [%blob "test_programs/arith_machine.txt"] in
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
       │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────        │
       │pc                ││ 00000000 │00000004 │00000008 │0000000C │00000010 │00000014 │00000018 │0000001C │00000020 │00000024 │00000028 │0000002C │00000030         │
       │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────        │
       │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────        │
       │write_data        ││ 0000000D │0000001A │01234000 │0ABCD00C │00000001 │FFFFFFFE │00000002 │FFFFFFFF │00000001 │00000008 │00000001 │80000000 │F8000000         │
       │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────        │
       │write_enable      ││──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────        │
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
