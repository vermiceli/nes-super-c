; NES Super C Disassembly - v1.00
; https://github.com/vermiceli/nes-super-c/
; Bank E contains the 1-bit delta pulse code modulation (DPCM) sound samples.

; 8 KiB PRG ROM
.segment "BANKE"

; export for bank c
.export dpcm_sample_00
.export dpcm_sample_01
.export dpcm_sample_02
.export dpcm_sample_03
.export dpcm_sample_04
.export dpcm_sample_05
.export dpcm_sample_06
.export dpcm_sample_07
.export dpcm_sample_08
.export dpcm_sample_09
.export dpcm_sample_0a
.export dpcm_sample_0b
.export dpcm_sample_0c

; see bank c dpcm_samples
; 320 bytes (321 byte) sample
dpcm_sample_00:
    .incbin "assets/audio_data/dpcm_sample_00.bin"

; see bank c dpcm_samples
; 512 bytes (513 byte) sample
dpcm_sample_01:
    .incbin "assets/audio_data/dpcm_sample_01.bin"

; see bank c dpcm_samples
; 16 bytes (17 byte) sample
dpcm_sample_02:
    .incbin "assets/audio_data/dpcm_sample_02.bin"

; !(UNUSED)
    .byte $ff,$fe,$fe,$7e,$00,$00,$04,$21,$12,$fc,$ff,$ff,$ff,$07,$00,$00
    .byte $00,$00,$c0,$b7,$fb,$ff,$ff,$ff,$ff,$fe,$fe,$fe,$02,$00,$80,$20
    .byte $44,$21,$49,$32,$46,$19,$23,$f8,$ff,$ff,$ff,$ff,$00,$00,$00,$00

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_03:
    .incbin "assets/audio_data/dpcm_sample_03.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_04:
    .incbin "assets/audio_data/dpcm_sample_04.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_05:
    .incbin "assets/audio_data/dpcm_sample_05.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_06:
    .incbin "assets/audio_data/dpcm_sample_06.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_07:
    .incbin "assets/audio_data/dpcm_sample_07.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_08:
    .incbin "assets/audio_data/dpcm_sample_08.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_09:
    .incbin "assets/audio_data/dpcm_sample_09.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_0a:
    .incbin "assets/audio_data/dpcm_sample_0a.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_0b:
    .incbin "assets/audio_data/dpcm_sample_0b.bin"

; see bank c dpcm_samples
; 704 bytes (705 byte sample)
dpcm_sample_0c:
    .incbin "assets/audio_data/dpcm_sample_0c.bin"

; end of bank free space
; unused #$100 bytes out of #$2,000 bytes total (96.88% full)
; unused 256 bytes out of 8,192 bytes total (96.88% full)
; filled with 256 #$ff bytes by superc.cfg configuration
bank_e_unused_space: