<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Helper tools

All of these are not very useful or have "production quality".

## Internal clock tools

To get current time on LED cube:

    runghc CubeConsole.hs -g SERIAL_DEVICE

To set your current computer time to the cube:

    runghc CubeConsole.hs -s SERIAL_DEVICE

That Haskell code has ugly dependencies which may be documented
later. In fact these tools should be implemented with some scripting
language like Python which is readily available on modern unices.
