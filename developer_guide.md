<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Developer guide

This requires LTO feature of avr-gcc which means you need to have
quite recent avr-gcc. We use avr-gcc (GCC) 4.7.0.

## SPI_STC optimization

Every cycle spent in SPI_STC produces 96 cycles every BLANK. Blank
occurs every 2^13 = 8192 cycles. Given clock frequency of f, we get:

    f / 8192 * 96 / f = 96/8192 = 1.17 %

So, one cycle spent in SPI_STC represents over one percent of overall
CPU cycles available on ATmega328p (or any other CPU running BLANK
every 2^13 CPU cycles).

Byte length on wire is 8 times the SPI clock divider. We use divider of 4, which results total cycles in transmit:

    cycles_per_byte = 8 * 4 = 32

In short, optimization really matters. Before starting to optimize,
the handler was consuming 41 cycles. In the far beginning, it was even
longer (haven't counted that). To avoid blocking lower priority
interrupts, we need to get lower than ̀cycles_per_byte`. 41
is bigger than that, so we are having way too slow SPI bus without
optimization and BLANK may occur before last byte.

After optimization the instruction count is 15. That is achieved by
reserving two global registers for storing the value of Z temporarily.
Using 2 registers for storing 16 bit value is faster than using stack
(2 instructions instead of 8).

If you are having problems with the assembly code, you may turn it off
by compiling the code with an extra option:

    scons --no-asm
