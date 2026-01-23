; Contra US Disassembly - v1.3
; https://github.com/vermiceli/nes-contra-us
; the 16 byte header prepended to the actual cartridge data that describes the
; cartridge.

.segment "HEADER"

.byte $4e,$45,$53,$1a                 ; "NES"^Z
.byte $08                             ; Specifies the number of 16k prg banks.
.byte $10                             ; Specifies the number of 8k chr banks.
.byte $40                             ; Mapper 004 (MMC3), no battery, no optional PRG RAM ($6000-$7fff)
                                      ; mirroring bit here is ignored, MMC3 mapper controls mirroring
                                      ; some ROMs on the internet have bit 0 set, but bit 0 isn't used by MMC3 mappers
.byte $00
.byte $00,$00,$00,$00,$00,$00,$00,$00 ; 8 zeroes