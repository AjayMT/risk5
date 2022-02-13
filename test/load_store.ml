open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Machine.I) (Risk5.Machine.O)

let program =
  let prog_text = [%blob "test_programs/load_store.txt"] in
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

let%expect_test "load_store_machine" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_width:172 waves;

  [%expect
    {|
       ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
       │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    │
       │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────│
       │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────│
       │writeback_data    ││ DEADB000 │DEADB0EF │00000003 │DEADB0EF │00000009 │FFFFFFEF │FFFFB0EF │000000EF │0000B0EF │00000009 │000000EF │00000009 │0000B0EF │00000009 │DEADB0EF │
       │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────│
       │writeback_enable  ││────────────────────┐         ┌───────────────────────────────────────────────────────────┐         ┌─────────┐         ┌─────────┐         ┌─────────│
       │                  ││                    └─────────┘                                                           └─────────┘         └─────────┘         └─────────┘         │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       │                  ││                                                                                                                                                      │
       └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
