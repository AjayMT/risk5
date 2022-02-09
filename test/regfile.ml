open Hardcaml
open Hardcaml_waveterm
module Simulator = Cyclesim.With_interface (Risk5.Regfile.I) (Risk5.Regfile.O)

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Regfile.circuit scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step ~read1 ~read2 ~wdata ~wenable ~wreg =
    inputs.read_reg_1 := Bits.of_string read1;
    inputs.read_reg_2 := Bits.of_string read2;
    inputs.write_data := Bits.of_string wdata;
    inputs.write_enable := if wenable then Bits.vdd else Bits.gnd;
    inputs.write_reg := Bits.of_string wreg;
    Cyclesim.cycle sim
  in

  step ~read1:"5'd0" ~read2:"5'd1" ~wdata:"32'hC0FFEE" ~wenable:true
    ~wreg:"5'd0";
  step ~read1:"5'd0" ~read2:"5'd1" ~wdata:"32'hDEADBEEF" ~wenable:false
    ~wreg:"5'd31";
  step ~read1:"5'd31" ~read2:"5'd1" ~wdata:"32'hFEEDFACE" ~wenable:true
    ~wreg:"5'd10";
  step ~read1:"5'd10" ~read2:"5'd31" ~wdata:"32'h0" ~wenable:false ~wreg:"5'd10";
  step ~read1:"5'd32" ~read2:"5'd10" ~wdata:"32'h0" ~wenable:false ~wreg:"5'd10";

  waves

let%expect_test "regfile" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 ~display_height:24 ~display_width:160 waves;

  [%expect
    {|
            ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
            │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
            │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
            │                  ││────────────────────┬─────────┬─────────┬─────────                                                                                        │
            │read_reg_1        ││ 00                 │1F       │0A       │00                                                                                               │
            │                  ││────────────────────┴─────────┴─────────┴─────────                                                                                        │
            │                  ││──────────────────────────────┬─────────┬─────────                                                                                        │
            │read_reg_2        ││ 01                           │1F       │0A                                                                                               │
            │                  ││──────────────────────────────┴─────────┴─────────                                                                                        │
            │                  ││──────────┬─────────┬─────────┬───────────────────                                                                                        │
            │write_data        ││ 00C0FFEE │DEADBEEF │FEEDFACE │00000000                                                                                                   │
            │                  ││──────────┴─────────┴─────────┴───────────────────                                                                                        │
            │write_enable      ││──────────┐         ┌─────────┐                                                                                                           │
            │                  ││          └─────────┘         └───────────────────                                                                                        │
            │                  ││──────────┬─────────┬─────────────────────────────                                                                                        │
            │write_reg         ││ 00       │1F       │0A                                                                                                                   │
            │                  ││──────────┴─────────┴─────────────────────────────                                                                                        │
            │                  ││──────────────────────────────┬─────────┬─────────                                                                                        │
            │read_data_1       ││ 00000000                     │FEEDFACE │00000000                                                                                         │
            │                  ││──────────────────────────────┴─────────┴─────────                                                                                        │
            │                  ││────────────────────────────────────────┬─────────                                                                                        │
            │read_data_2       ││ 00000000                               │FEEDFACE                                                                                         │
            │                  ││────────────────────────────────────────┴─────────                                                                                        │
            └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
