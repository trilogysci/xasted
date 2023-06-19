# ΞAStEd
ΞAStEd pronounced as "wasted", is a 6 x 7 segment shooter game for fpga.
Designed to run on DE10-Lite, with just the hardware that comes with it.

This is the version coded in Clash a Haskell to HDL design language/converter
The generated verilog files are also included under the "verilog" folder as well as
a Makefile to build and install to the DE10-Lite boards using

## File Structure
* src - Haskell files to build the project using Clash
* verilog - a directory containing the verilog files generated from Clash
* build.sh - to run clash and build 
* DE10Lite.brd - the "board" file describing the pins for DE10-Lite
* build - a directory containing files to build with Quartus (Quartus lite)
  * Makefile - make file to build the project has the following targets
    * make (or make all) - build the .sof and .pof files
    * make program - install the .sof program on the fpga device, a quick temporary install
    * make program-pof - install the .pof program on the fpga device, installs to the boot flash

## Setup

You will need to update the Makefile to specify the quartus path QUARTUS_PATH

Build Steps:
See the build.sh file
