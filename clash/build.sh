#!/bin/bash
# run clash then build and install
# generate --vhdl or --verilog or --sytemverilog
stack run clash -- Xasted --verilog
pushd build
make
make program
popd
