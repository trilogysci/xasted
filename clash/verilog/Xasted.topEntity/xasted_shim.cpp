#include <cstdlib>

#include <verilated.h>

#include "Vxasted.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vxasted *top = new Vxasted;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

