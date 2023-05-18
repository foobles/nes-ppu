An emulator for the NTSC NES PPU (2c02) that provides a generic interface mimicking the
interface that exists on actual NES hardware. In addition, the interface also contains some
conveniences not available on the NES in order to make programming more ergonomic.

## Features
* `no_std` support
* Cycle-based emulation, including accurate timings for sprite processing and mapper accesses
* Emulation of all PPU registers (0x2000-0x2007)
* Support for arbitrary custom memory mappers and output formats
* Most system quirks are properly emulated:
  * Garbage nametable fetches
  * Different total cycle counts on even vs. odd frames
  * Buggy overflow flag behavior
  * Reading OAMDATA during rendering snoops on internal sprite processing state
  * Incorrect color output during forced blanking when vram address indexes palette ram
  * Etc.

## Limitations
* PPU register accesses happen "instantaneously," and do not cause the PPU to tick forwards
  despite reads/writes taking multiple cycles on real hardware
    * For truly accurate graphics, users must therefore be careful to weigh how much work is
      being done relative to when/how often they tick the PPU
* No support for PAL or Dendy PPUs
* No emulation of open bus behavior
* Mapper reads take 1 cycle to resolve, instead of 2 like on real hardware (the timings of when
  the reads start are still accurate)

## Acknowledgements
None of the work on this emulator would have been possible without the years of research that
members of the NESdev community have put into [www.nesdev.org](https://www.nesdev.org/), nor
would it have been possible without the assistance of members of the
[NESdev Discord server](https://discord.gg/pTWwBCp).