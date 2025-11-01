# Overview
This repository contains an annotated disassembly of the _Super C_ (US) NES ROM
and the build script(s) to reassemble the assembly into a byte-for-byte match of
the game.  _Super C_ is the sequel to the _Contra_ video game for the NES.  This
repo also contains supplemental documentation, scripts, and tools to further
understand the game.

For the disassembly of _Contra_ see https://github.com/vermiceli/nes-contra-us/

```
|-- docs - supplemental documentation
|   |-- images - files used in other documentation
|   |-- lua_scripts - lua scripts for mesen2
|   |-- sprite_library - extracted sprites for ease of viewing
|-- src - the source code for the game
|   |-- assets - the compressed graphics data and encoded audio for the game
|-- assets.txt - a list of assets, their offset in baserom.nes and their length
|-- build.bat - build script for Windows cmd (no PowerShell)
|-- build.ps1 - recommended build script for Windows (PowerShell)
|-- build.sh - bash build script for Linux/mac
|-- contra.cfg - memory mapping of PRG banks used when building
|-- README.md
|-- set_bytes.vbs - script used by build.bat to extract data from baserom.nes
```

# Building

## Prerequisites

* This repo does not include the assets (graphics data and audio data) necessary
for assembling the ROM. An existing copy of the game is required.  Place a copy
of the US NES version of _Super C_ with the name `baserom.nes` in the project
root folder.  The file will be used by the build script to extract the necessary
assets.  To confirm you have the correct ROM of the game, you can look at the
ROM file's hash.  The MD5 hash of `baserom.nes` ROM should be one of the
following.  The difference in ROM hashes come from the fact that there is a
single ignored bit in the iNES header that can be either set or not set and does
not impact the game.
  * `5FEF80625F484FCA06FDB58EBFF9D8BB`
  * `78F97D58AF93493976E810D02B40FA30`
* Building requires the [cc65 compiler suite](https://cc65.github.io/) to
assemble and link the 6502 assembly files.  Please install it and ensure the bin
folder is added to your path.

## Instructions
There are 3 build scripts in this repository. All of them do the same thing.  To
build the resulting .nes rom file, simply execute the appropriate build script
based on your environment.

| Environment             | Command       |
|-------------------------|---------------|
| Windows                 | `.\build.ps1` |
| Windows (no PowerShell) | `.\build.bat` |
| Mac/Unix/Linux          | `./build.sh`  |

* `build.ps1` - PowerShell script recommended for building on Windows machines.
  Some users have reported needing to run the script as admin to access the
  `baserom.nes`, although I haven't experienced this.  Additionally, on older
  versions of PowerShell, you may need to adjust the execution policy with the
  command `Set-ExecutionPolicy`.
* `build.bat` - bat script that can be used on windows machines without
  PowerShell, but requires VBScript support.
* `build.sh` - bash script to be used in unix environments, or on Windows
  environments with bash support (Git bash, WSL, etc.)

### Video Instructions
If you prefer video build instructions, @jdsilva has published a video on
YouTube called
[Contra ROM Compiling Tutorial](https://www.youtube.com/watch?v=Sa1O3S5YK5U).
While it goes over how to build _Contra_, building _Super C_ is virtually
identical.

## Documentation

Supplemental materials have been added that help explain interesting features of
the code.  Below are some of the more important documents.

* [docs/Banks.md](./docs/Banks.md) - details about how _Super C_ banks the PRG
  ROM and CHR ROM data.
* [docs/Bugs.md](./docs/Bugs.md) - bugs identified while disassembling
* [docs/Enemy Glossary.md](./docs/Enemy%20Glossary.md) - documentation on every
  enemy type in the game
* [docs/Graphics Documentation.md](./docs/Graphics%20Documentation.md) -
  documentation on how graphics are written to the PPU.
* [docs/ROM Map.md](./docs/ROM%20Map.md) - detailed description of ROM file
  contents
* [docs/Sound Documentation.md](./docs/Sound%20Documentation.md) - documentation
  on the audio engine used as well as information on all sounds from the game.

All sprites were captured and labeled for easy reference in
[docs/sprite_library/README.md](./docs/sprite_library/README.md)