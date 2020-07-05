# An ARM processor from scratch in VHDL

This is a multi-cycle (non-pipelined) processor written in VHDL from scratch designed to run ARM instructions. This is intended to be run on a basys board. The `.bit` files are bitstreams which can be directly loaded onto the board. `.vhd` files contain the logic of the processor. 

## Features

1. The processor is multi-cycled. This means that atomic operations like store/load and arithmetic operations happen in separate cycles. This increases the clock speed of the processor and makes it easier to develop a pipelined implementation. 
2. Processor has support for a supervisor mode and privileged instructions. 
3. There is a provision for software and hardware interrupts. 
4. The processor has memory-mapped I/O on bus. This means that it can support peripherals like a keypad and display which can be driven using software drivers. ARM code for this is included.

