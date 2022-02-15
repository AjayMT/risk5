
# Risk5
Risk5 is a single-stage RISC-V CPU that implements the RV32I ISA. It is written with [Hardcaml](https://github.com/janestreet/hardcaml) and can be compiled to Verilog or VHDL or simulated with a variety of backends -- see the Hardcaml README for more.

## Design
![risk5-design](https://github.com/AjayMT/risk5/raw/master/diagram/diagram.png)

Though the current implementation is not (yet) pipelined, this design describes the five "stages" of the CPU. Data is depicted in black and control signals are in blue. PDF and [tgif](http://bourbon.usc.edu/tgif/) .obj versions of the design are in the `diagram/` directory.

The modules in the `lib/` directory map more-or-less directly to the modules described in the diagram. The most notable difference is that "instruction memory" is not implemented using Hardcaml's `Ram` or `multiport_memory` modules -- the program is loaded into the compiled HDL as a list of signals from which instructions are selected using the PC register. This was done primarily to simplify simulation and testing, as constructing a `multiport_memory` module and writing program data to it proved to be difficult. The current implementation also includes an additional "CMP" module between "Decode" and "Next PC" that is used for comparison operations in branch instructions; the `branch_cmp` signal is actually coming from CMP and not the ALU. This was also done to simplify the implementation at the cost of additional hardware.

## Build and Usage
To build the OCaml source code, you will need:
- [OCaml](https://ocaml.org)
- [Dune](https://dune.build)
- [Hardcaml](https://github.com/janestreet/hardcaml)
- [Hardcaml\_waveterm](https://github.com/janestreet/hardcaml_waveterm)
- [ppx\_deriving\_hardcaml](https://github.com/janestreet/ppx_deriving_hardcaml)
- [ppx\_jane](https://github.com/janestreet/ppx_jane)
- [ppx\_expect](https://github.com/janestreet/ppx_expect)
- [ppx\_blob](https://github.com/johnwhitington/ppx_blob)

To build the verilog-generating executable, run `dune build`. To run the inline tests, run `dune test`.

The `test/test_programs/` directory contains example RISC-V programs in assembly and plaintext hexdump formats. The `build.sh` script compiles all assembly programs in the current directory. To build your own RISC-V programs, you will need the [RISC-V GNU Toolchain](https://github.com/riscv-collab/riscv-gnu-toolchain).

To produce Verilog for a given program, build the `risk5` executable with `dune build` and run it as follows:
```
risk5 <program> <output>
```

For example, `risk5 test/test_programs/load_store.txt output.v` will write Verilog that describes the CPU design and includes the program `load_store.txt` to `output.v`. VHDL generation and interactive simulation are currently unimplemented.

## Acknowledgements
The following resources were very helpful in learning how to use Hardcaml and implementing a RISC-V CPU:
- [hardcaml-mips](https://github.com/askvortsov1/hardcaml-mips) and the accompanying [blog posts](https://ceramichacker.com/blog/category/hardcaml-mips)
- The [Hardcaml Manual](https://github.com/janestreet/hardcaml/blob/master/docs/index.mdx)
- [RISC-V Specification](https://riscv.org/technical/specifications/)
- [RISC-V Opcodes](https://github.com/riscv/riscv-opcodes)
- [CSCE 513](https://passlab.github.io/CSCE513/) notes, specifically [this PDF](https://passlab.github.io/CSCE513/notes/lecture04_RISCV_ISA.pdf)
