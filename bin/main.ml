open Hardcaml

let sim program steps =
  let open Hardcaml_waveterm in
  let module Simulator =
    Cyclesim.With_interface (Risk5.Machine.I) (Risk5.Machine.O)
  in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Risk5.Machine.circuit program scope) in
  let waves, sim = Waveform.create sim in

  for _ = 0 to steps - 1 do
    Cyclesim.cycle sim
  done;

  Hardcaml_waveterm_interactive.run waves

let rtl program ~vhdl ~out_file =
  let module Risk5Circuit =
    Circuit.With_interface (Risk5.Machine.I) (Risk5.Machine.O)
  in
  let scope = Scope.create () in
  let circuit =
    Risk5Circuit.create_exn ~name:"risk5" (Risk5.Machine.circuit program scope)
  in
  let output_mode = Rtl.Output_mode.To_file out_file in
  let output_hdl = if vhdl then Rtl.Language.Vhdl else Rtl.Language.Verilog in
  Rtl.output ~output_mode
    ~database:(Scope.circuit_database scope)
    output_hdl circuit

let () =
  let usage_msg =
    "Usage:\n" ^ "\trisk5 [--vhdl] <program_path> <output_path>\n"
    ^ "\trisk5 --waves <num_steps> <program_path>\n\n"
    ^ "Run a RISC-V program on the Risk5 CPU or output a Verilog/VHDL \
       specification\n"
    ^ "that includes the program data. Programs are formatted as plaintext \
       hexdump files;\n"
    ^ "see test/test_programs/ in the Risk5 source directory for examples.\n"
  in

  let vhdl = ref false in
  let waves = ref (-1) in
  let help = ref false in
  let pos_args = ref [] in

  let opt_list =
    [
      ("--vhdl", Arg.Set vhdl, "\t\tOutput VHDL instead of Verilog");
      ( "--waves",
        Arg.Set_int waves,
        "<num_steps>\tSimulate num_steps instructions and display waveforms in \
         an interactive viewer" );
      ("--help", Arg.Set help, "\t\tDisplay this list of options");
      ("-help", Arg.Set help, "\t\tDisplay this list of options");
    ]
  in

  Arg.parse opt_list (fun a -> pos_args := a :: !pos_args) usage_msg;

  (match (!help, !waves > 0, !pos_args) with
  | true, _, _ ->
      Arg.usage opt_list usage_msg;
      exit 0
  | _, true, pos_args ->
      if List.length pos_args <> 1 then (
        Arg.usage opt_list usage_msg;
        exit 1)
      else ()
  | _, _, pos_args ->
      if List.length pos_args <> 2 then (
        Arg.usage opt_list usage_msg;
        exit 1)
      else ());

  let prog_ch = open_in (List.nth !pos_args 0) in
  let prog_text = really_input_string prog_ch (in_channel_length prog_ch) in
  close_in prog_ch;
  let prog_lines =
    List.filter (fun s -> String.length s > 0)
    @@ String.split_on_char '\n' prog_text
  in
  let program =
    List.map (fun line -> Signal.of_string @@ "32'h" ^ line) prog_lines
  in

  if !waves <> -1 then sim program !waves
  else rtl program ~vhdl:!vhdl ~out_file:(List.nth !pos_args 1)
