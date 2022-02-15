open Hardcaml
module Risk5Circuit = Circuit.With_interface (Risk5.Machine.I) (Risk5.Machine.O)

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: risk5 <program> <output>\n";
    exit 1)
  else ();
  let prog_ch = open_in Sys.argv.(1) in
  let prog_text = really_input_string prog_ch (in_channel_length prog_ch) in
  close_in prog_ch;
  let prog_lines =
    List.filter (fun s -> String.length s > 0)
    @@ String.split_on_char '\n' prog_text
  in
  let program =
    List.map (fun line -> Signal.of_string @@ "32'h" ^ line) prog_lines
  in
  let scope = Scope.create () in
  let circuit =
    Risk5Circuit.create_exn ~name:"risk5" (Risk5.Machine.circuit program scope)
  in
  let output_mode = Rtl.Output_mode.To_file Sys.argv.(2) in
  Rtl.output ~output_mode
    ~database:(Scope.circuit_database scope)
    Verilog circuit
