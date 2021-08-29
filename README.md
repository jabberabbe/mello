# Mello
## Haskell memory inspection, debugging and tracing library

[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE)

> "Mello è Mello ed è bello perché è Bello: è un ladro-performer con tendenze al bardo ma senza i poteri del bardo e con tendenze criminali poco riuscite dovute al fatto che come ladro è una pippa pur essendo un ladro"
> - Mitch

Mello is a library to build memory inspection, RE and debugging tools/scripts in
Haskell. During CTF competitions, reverse engineering or low-level debugging of
complex runtime systems (e.g. Haskell/STG) it is sometimes very useful to script
the tools you are working with. I often find myself running the same GDB
commands every time the program stops at a breakpoint to examine the same
variable or memory region. Manual inspection of custom runtime representations
may become daunting and boring. Staring at a 100-lines long hexdump to inspect
heap chunks of a running program is not funny. Wouldn't be useful to let your
computer do that for you every time the debugger stops at a breakpoint?

Visualisation and automation really matter in these situations. Instead of
copying addresses back and forth between debugger sessions, IPython shells and
disassemblers, you could write a simple script to easily inspect memory of the
target process, perform calculations and operations you would manually do within
a debugger, and show you the results in a clean and customizable output format.

* **Why Haskell?** Because Haskell is a powerful, elegant and concise language.
  The same goals of this library could probably be achieved with more common
  scripting languages like Python and known to a broader audience. But I feel
  more comfortable doing _anything_ (including writing dumb command-line
  utilities) in Haskell. GHCi is also a powerful tool: you could put commonly
  used inspection tasks in a Haskell file using this library and trigger them
  from a GHCi session while keeping a GDB console in another pane for manual
  commands.

* **How does it work?** It is worth stressing that I'm not trying to implement a
  debugger in Haskell. This library is just a thin wrapper around communication
  protocols with other tools, libraries or subsystems. It could interact with
  GDB, directly use `ptrace` syscalls, read `/proc/PID/mem` or talk to radare2
  through `r2pipe`.

  The aim is to build the library on intuitive and easy-to-use APIs. Backends
  implement memory peek/poke operation and may provide additional bindings (e.g.
  breakpoints, shared library and DWARF info inspection...).

* **I like it, how do I use it?** Well, all of this still needs to be
  implemented. In the meanwhile, grab some tea and learn yourself some Haskell.
  It is very likely that I'll drop this project even before it will become usable.
  That's one of the problems with me.

* **Where does that dumb name come from?** Ask those guys who waste time playing
  D&D with me.

