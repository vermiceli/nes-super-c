; NES Super C Disassembly - v1.00
; https://github.com/vermiceli/nes-super-c/
; Bank D contains the rest of the non-DPCM encoded sound data.

; 8 KiB PRG ROM
.segment "BANKD"

; import from bank c
.import sound_29_slot_01

.export sound_29_slot_01_3
.export sound_29_slot_02
.export sound_29_slot_03
.export sound_2a_slot_00
.export sound_2a_slot_01
.export sound_2a_slot_02
.export sound_2a_slot_03
.export sound_2b_slot_00
.export sound_2b_slot_01
.export sound_2b_slot_02
.export sound_2b_slot_03
.export sound_2c_slot_00
.export sound_2c_slot_01
.export sound_2c_slot_02
.export sound_2c_slot_03
.export sound_2d_slot_00
.export sound_2d_slot_01
.export sound_2d_slot_02
.export sound_2d_slot_03
.export sound_2f_slot_00
.export sound_2f_slot_01
.export sound_2f_slot_02
.export sound_2f_slot_03
.export sound_30_slot_00
.export sound_30_slot_01
.export sound_30_slot_02
.export sound_30_slot_03
.export sound_31_slot_00
.export sound_31_slot_01
.export sound_31_slot_02
.export sound_31_slot_03
.export sound_33_slot_00
.export sound_33_slot_01
.export sound_33_slot_02
.export sound_33_slot_03
.export sound_34_slot_00
.export sound_34_slot_01
.export sound_34_slot_02
.export sound_34_slot_03
.export sound_35_slot_00
.export sound_35_slot_01
.export sound_35_slot_02
.export sound_35_slot_03

; sound_29_slot_01 continued
    .addr sound_29_slot_01_3
    .incbin "assets/audio_data/sound_29_slot_01_2.bin"
    .addr sound_29_slot_01

sound_29_slot_01_3:
    .incbin "assets/audio_data/sound_29_slot_01_3.bin"

sound_29_slot_02:
    .incbin "assets/audio_data/sound_29_slot_02_0.bin"
    .addr sound_29_slot_02_6
    .byte $fd
    .addr sound_29_slot_02_6
    .incbin "assets/audio_data/sound_29_slot_02_2.bin"
    .addr sound_29_slot_02_7
    .incbin "assets/audio_data/sound_29_slot_02_3.bin"
    .addr sound_29_slot_02_7
    .incbin "assets/audio_data/sound_29_slot_02_4.bin"
    .addr sound_29_slot_02_7
    .incbin "assets/audio_data/sound_29_slot_02_5.bin"
    .addr sound_29_slot_02

sound_29_slot_02_6:
    .incbin "assets/audio_data/sound_29_slot_02_6.bin"

sound_29_slot_02_7:
    .incbin "assets/audio_data/sound_29_slot_02_7.bin"

; also used by sound_2e
sound_29_slot_03:
    .incbin "assets/audio_data/sound_29_slot_03_0.bin"
    .addr sound_29_slot_03_3
    .byte $fd
    .addr sound_29_slot_03_3
    .incbin "assets/audio_data/sound_29_slot_03_2.bin"
    .addr sound_29_slot_03

sound_29_slot_03_3:
    .incbin "assets/audio_data/sound_29_slot_03_3.bin"

sound_2a_slot_00:
    .incbin "assets/audio_data/sound_2a_slot_00_0.bin"
    .addr sound_2a_slot_00_3
    .incbin "assets/audio_data/sound_2a_slot_00_1.bin"
    .addr sound_2a_slot_00_3
    .incbin "assets/audio_data/sound_2a_slot_00_2.bin"
    .addr sound_2a_slot_00

sound_2a_slot_00_3:
    .incbin "assets/audio_data/sound_2a_slot_00_3.bin"

sound_2a_slot_01:
    .incbin "assets/audio_data/sound_2a_slot_01_0.bin"
    .addr sound_2a_slot_01_3
    .incbin "assets/audio_data/sound_2a_slot_01_1.bin"
    .addr sound_2a_slot_01_3
    .incbin "assets/audio_data/sound_2a_slot_01_2.bin"
    .addr sound_2a_slot_01

sound_2a_slot_01_3:
    .incbin "assets/audio_data/sound_2a_slot_01_3.bin"

sound_2a_slot_02:
    .incbin "assets/audio_data/sound_2a_slot_02_0.bin"
    .addr sound_2a_slot_02_5
    .incbin "assets/audio_data/sound_2a_slot_02_1.bin"
    .addr sound_2a_slot_02_5
    .incbin "assets/audio_data/sound_2a_slot_02_2.bin"
    .addr sound_2a_slot_02_6
    .incbin "assets/audio_data/sound_2a_slot_02_3.bin"
    .addr sound_2a_slot_02_6
    .incbin "assets/audio_data/sound_2a_slot_02_4.bin"
    .addr sound_2a_slot_02

sound_2a_slot_02_5:
    .incbin "assets/audio_data/sound_2a_slot_02_5.bin"

sound_2a_slot_02_6:
    .incbin "assets/audio_data/sound_2a_slot_02_6.bin"

sound_2a_slot_03:
    .incbin "assets/audio_data/sound_2a_slot_03_0.bin"
    .addr sound_2a_slot_03_5
    .incbin "assets/audio_data/sound_2a_slot_03_1.bin"
    .addr sound_2a_slot_03_5
    .incbin "assets/audio_data/sound_2a_slot_03_2.bin"
    .addr sound_2a_slot_03_6
    .incbin "assets/audio_data/sound_2a_slot_03_3.bin"
    .addr sound_2a_slot_03_6
    .incbin "assets/audio_data/sound_2a_slot_03_4.bin"
    .addr sound_2a_slot_03

sound_2a_slot_03_5:
    .incbin "assets/audio_data/sound_2a_slot_03_5.bin"

sound_2a_slot_03_6:
    .incbin "assets/audio_data/sound_2a_slot_03_6.bin"

sound_2b_slot_00:
    .incbin "assets/audio_data/sound_2b_slot_00_0.bin"

sound_2b_slot_00_00:
    .incbin "assets/audio_data/sound_2b_slot_00_1.bin"
    .addr sound_2b_slot_00_4
    .incbin "assets/audio_data/sound_2b_slot_00_2.bin"
    .addr sound_2b_slot_00_4
    .incbin "assets/audio_data/sound_2b_slot_00_3.bin"
    .addr sound_2b_slot_00_00

sound_2b_slot_00_4:
    .incbin "assets/audio_data/sound_2b_slot_00_4.bin"

sound_2b_slot_01:
    .incbin "assets/audio_data/sound_2b_slot_01_0.bin"

sound_2b_slot_01_1:
    .incbin "assets/audio_data/sound_2b_slot_01_1.bin"
    .addr sound_2b_slot_01_4
    .incbin "assets/audio_data/sound_2b_slot_01_2.bin"
    .addr sound_2b_slot_01_4
    .incbin "assets/audio_data/sound_2b_slot_01_3.bin"
    .addr sound_2b_slot_01_1

sound_2b_slot_01_4:
    .incbin "assets/audio_data/sound_2b_slot_01_4.bin"

sound_2b_slot_02:
    .incbin "assets/audio_data/sound_2b_slot_02_0.bin"

sound_2b_slot_02_00:
    .incbin "assets/audio_data/sound_2b_slot_02_1.bin"
    .addr sound_2b_slot_02_4
    .incbin "assets/audio_data/sound_2b_slot_02_2.bin"
    .addr sound_2b_slot_02_4
    .incbin "assets/audio_data/sound_2b_slot_02_3.bin"
    .addr sound_2b_slot_02_00

sound_2b_slot_02_4:
    .incbin "assets/audio_data/sound_2b_slot_02_4.bin"

sound_2b_slot_03:
    .incbin "assets/audio_data/sound_2b_slot_03_0.bin"

sound_2b_slot_03_00:
    .byte $fd
    .addr sound_2b_slot_03_4
    .byte $fd
    .addr sound_2b_slot_03_4
    .incbin "assets/audio_data/sound_2b_slot_03_3.bin"
    .addr sound_2b_slot_03_00

sound_2b_slot_03_4:
    .incbin "assets/audio_data/sound_2b_slot_03_4.bin"

sound_2c_slot_00:
    .incbin "assets/audio_data/sound_2c_slot_00_0.bin"

sound_2c_slot_00_1:
    .incbin "assets/audio_data/sound_2c_slot_00_1.bin"
    .addr sound_2c_slot_00_4
    .incbin "assets/audio_data/sound_2c_slot_00_2.bin"
    .addr sound_2c_slot_00_4
    .incbin "assets/audio_data/sound_2c_slot_00_3.bin"
    .addr sound_2c_slot_00_1

sound_2c_slot_00_4:
    .incbin "assets/audio_data/sound_2c_slot_00_4.bin"

sound_2c_slot_01:
    .incbin "assets/audio_data/sound_2c_slot_01_0.bin"

sound_2c_slot_01_00:
    .incbin "assets/audio_data/sound_2c_slot_01_1.bin"
    .addr sound_2c_slot_01_4
    .incbin "assets/audio_data/sound_2c_slot_01_2.bin"
    .addr sound_2c_slot_01_4
    .incbin "assets/audio_data/sound_2c_slot_01_3.bin"
    .addr sound_2c_slot_01_00

sound_2c_slot_01_4:
    .incbin "assets/audio_data/sound_2c_slot_01_4.bin"

sound_2c_slot_02:
    .incbin "assets/audio_data/sound_2c_slot_02_0.bin"

sound_2c_slot_02_00:
    .incbin "assets/audio_data/sound_2c_slot_02_1.bin"
    .addr sound_2c_slot_02_4
    .incbin "assets/audio_data/sound_2c_slot_02_2.bin"
    .addr sound_2c_slot_02_4
    .incbin "assets/audio_data/sound_2c_slot_02_3.bin"
    .addr sound_2c_slot_02_00

sound_2c_slot_02_4:
    .incbin "assets/audio_data/sound_2c_slot_02_4.bin"

sound_2c_slot_03:
    .incbin "assets/audio_data/sound_2c_slot_03_0.bin"

sound_2c_slot_03_1:
    .incbin "assets/audio_data/sound_2c_slot_03_1.bin"
    .addr sound_2c_slot_03_4
    .incbin "assets/audio_data/sound_2c_slot_03_2.bin"
    .addr sound_2c_slot_03_4
    .incbin "assets/audio_data/sound_2c_slot_03_3.bin"
    .addr sound_2c_slot_03_1

sound_2c_slot_03_4:
    .incbin "assets/audio_data/sound_2c_slot_03_4.bin"

sound_2d_slot_00:
    .incbin "assets/audio_data/sound_2d_slot_00_0.bin"
    .addr sound_2d_slot_00_5
    .incbin "assets/audio_data/sound_2d_slot_00_1.bin"
    .addr sound_2d_slot_00_5
    .incbin "assets/audio_data/sound_2d_slot_00_2.bin"
    .addr sound_2d_slot_00_6
    .incbin "assets/audio_data/sound_2d_slot_00_3.bin"
    .addr sound_2d_slot_00_6
    .incbin "assets/audio_data/sound_2d_slot_00_4.bin"
    .addr sound_2d_slot_00

sound_2d_slot_00_5:
    .incbin "assets/audio_data/sound_2d_slot_00_5.bin"

sound_2d_slot_00_6:
    .incbin "assets/audio_data/sound_2d_slot_00_6.bin"

sound_2d_slot_01:
    .incbin "assets/audio_data/sound_2d_slot_01_0.bin"
    .addr sound_2d_slot_01_5
    .incbin "assets/audio_data/sound_2d_slot_01_1.bin"
    .addr sound_2d_slot_01_5
    .incbin "assets/audio_data/sound_2d_slot_01_2.bin"
    .addr sound_2d_slot_01_6
    .incbin "assets/audio_data/sound_2d_slot_01_3.bin"
    .addr sound_2d_slot_01_6
    .incbin "assets/audio_data/sound_2d_slot_01_4.bin"
    .addr sound_2d_slot_01

sound_2d_slot_01_5:
    .incbin "assets/audio_data/sound_2d_slot_01_5.bin"

sound_2d_slot_01_6:
    .incbin "assets/audio_data/sound_2d_slot_01_6.bin"

sound_2d_slot_02:
    .incbin "assets/audio_data/sound_2d_slot_02_0.bin"
    .addr sound_2d_slot_02_7
    .incbin "assets/audio_data/sound_2d_slot_02_1.bin"
    .addr sound_2d_slot_02_7
    .incbin "assets/audio_data/sound_2d_slot_02_2.bin"
    .addr sound_2d_slot_02_8
    .incbin "assets/audio_data/sound_2d_slot_02_3.bin"
    .addr sound_2d_slot_02_8
    .incbin "assets/audio_data/sound_2d_slot_02_4.bin"
    .addr sound_2d_slot_02_9
    .incbin "assets/audio_data/sound_2d_slot_02_5.bin"
    .addr sound_2d_slot_02_9
    .incbin "assets/audio_data/sound_2d_slot_02_6.bin"
    .addr sound_2d_slot_02

sound_2d_slot_02_7:
    .incbin "assets/audio_data/sound_2d_slot_02_7.bin"

sound_2d_slot_02_8:
    .incbin "assets/audio_data/sound_2d_slot_02_8.bin"

sound_2d_slot_02_9:
    .incbin "assets/audio_data/sound_2d_slot_02_9.bin"

sound_2d_slot_03:
    .incbin "assets/audio_data/sound_2d_slot_03_0.bin"
    .addr sound_2d_slot_03_7
    .byte $fd
    .addr sound_2d_slot_03_7
    .byte $fd
    .addr sound_2d_slot_03_7
    .byte $fd
    .addr sound_2d_slot_03_7
    .byte $fd
    .addr sound_2d_slot_03_7
    .byte $fd
    .addr sound_2d_slot_03_7
    .incbin "assets/audio_data/sound_2d_slot_03_6.bin"
    .addr sound_2d_slot_03

sound_2d_slot_03_7:
    .incbin "assets/audio_data/sound_2d_slot_03_7.bin"

sound_2f_slot_00:
    .incbin "assets/audio_data/sound_2f_slot_00.bin"
    .addr sound_2f_slot_00

sound_2f_slot_01:
    .incbin "assets/audio_data/sound_2f_slot_01.bin"
    .addr sound_2f_slot_01

sound_2f_slot_02:
    .incbin "assets/audio_data/sound_2f_slot_02.bin"
    .addr sound_2f_slot_02

sound_2f_slot_03:
    .incbin "assets/audio_data/sound_2f_slot_03_0.bin"
    .addr sound_2f_slot_03_7
    .incbin "assets/audio_data/sound_2f_slot_03_1.bin"
    .addr sound_2f_slot_03_7
    .incbin "assets/audio_data/sound_2f_slot_03_2.bin"
    .addr sound_2f_slot_03_8
    .incbin "assets/audio_data/sound_2f_slot_03_3.bin"
    .addr sound_2f_slot_03_9
    .incbin "assets/audio_data/sound_2f_slot_03_4.bin"
    .addr sound_2f_slot_03_8
    .incbin "assets/audio_data/sound_2f_slot_03_5.bin"
    .addr sound_2f_slot_03_9
    .incbin "assets/audio_data/sound_2f_slot_03_6.bin"
    .addr sound_2f_slot_03

sound_2f_slot_03_7:
    .incbin "assets/audio_data/sound_2f_slot_03_7.bin"

sound_2f_slot_03_8:
    .incbin "assets/audio_data/sound_2f_slot_03_8.bin"

sound_2f_slot_03_9:
    .incbin "assets/audio_data/sound_2f_slot_03_9.bin"

sound_30_slot_00:
    .incbin "assets/audio_data/sound_30_slot_00_0.bin"
    .addr sound_31_slot_00_1
    .byte $ff

sound_30_slot_01:
    .incbin "assets/audio_data/sound_30_slot_01_0.bin"
    .addr sound_30_slot_01_4
    .incbin "assets/audio_data/sound_30_slot_01_1.bin"
    .addr sound_30_slot_01_4
    .incbin "assets/audio_data/sound_30_slot_01_2.bin"
    .addr sound_31_slot_01_00
    .byte $ff

sound_30_slot_01_4:
    .incbin "assets/audio_data/sound_30_slot_01_4.bin"

sound_30_slot_02:
    .incbin "assets/audio_data/sound_30_slot_02_0.bin"
    .addr sound_31_slot_02_00
    .byte $ff

sound_30_slot_03:
    .incbin "assets/audio_data/sound_30_slot_03_0.bin"
    .addr sound_31_slot_03_1
    .byte $ff

sound_31_slot_00:
    .incbin "assets/audio_data/sound_31_slot_00_0.bin"

sound_31_slot_00_1:
    .incbin "assets/audio_data/sound_31_slot_00_1.bin"
    .addr sound_31_slot_01_6
    .byte $fd
    .addr sound_31_slot_00_7
    .incbin "assets/audio_data/sound_31_slot_00_3.bin"
    .addr sound_31_slot_01_6
    .byte $fd
    .addr sound_31_slot_00_7
    .incbin "assets/audio_data/sound_31_slot_00_5.bin"
    .addr sound_31_slot_00_1

sound_31_slot_00_6:
    .incbin "assets/audio_data/sound_31_slot_00_6.bin"

sound_31_slot_00_7:
    .incbin "assets/audio_data/sound_31_slot_00_7.bin"

sound_31_slot_01:
    .incbin "assets/audio_data/sound_31_slot_01_0.bin"

sound_31_slot_01_00:
    .incbin "assets/audio_data/sound_31_slot_01_1.bin"
    .addr sound_31_slot_00_6
    .byte $fd
    .addr sound_31_slot_01_7
    .incbin "assets/audio_data/sound_31_slot_01_3.bin"
    .addr sound_31_slot_00_6
    .byte $fd
    .addr sound_31_slot_01_7
    .incbin "assets/audio_data/sound_31_slot_01_5.bin"
    .addr sound_31_slot_01_00

sound_31_slot_01_6:
    .incbin "assets/audio_data/sound_31_slot_01_6.bin"

sound_31_slot_01_7:
    .incbin "assets/audio_data/sound_31_slot_01_7.bin"

sound_31_slot_02:
    .incbin "assets/audio_data/sound_31_slot_02_0.bin"

sound_31_slot_02_00:
    .incbin "assets/audio_data/sound_31_slot_02_1.bin"
    .addr sound_31_slot_02_3
    .incbin "assets/audio_data/sound_31_slot_02_2.bin"
    .addr sound_31_slot_02_3
    .incbin "assets/audio_data/sound_31_slot_02_3.bin"
    .addr sound_31_slot_02_00

sound_31_slot_02_3:
    .incbin "assets/audio_data/sound_31_slot_02_4.bin"

sound_31_slot_03:
    .incbin "assets/audio_data/sound_31_slot_03_0.bin"

sound_31_slot_03_1:
    .incbin "assets/audio_data/sound_31_slot_03_1.bin"
    .addr sound_31_slot_03_6
    .byte $fd
    .addr sound_31_slot_03_7
    .byte $fd
    .addr sound_31_slot_03_6
    .byte $fd
    .addr sound_31_slot_03_7
    .incbin "assets/audio_data/sound_31_slot_03_5.bin"
    .addr sound_31_slot_03_1

sound_31_slot_03_6:
    .incbin "assets/audio_data/sound_31_slot_03_6.bin"

sound_31_slot_03_7:
    .incbin "assets/audio_data/sound_31_slot_03_7.bin"

sound_33_slot_00:
    .incbin "assets/audio_data/sound_33_slot_00.bin"

sound_33_slot_01:
    .incbin "assets/audio_data/sound_33_slot_01.bin"

sound_33_slot_02:
    .incbin "assets/audio_data/sound_33_slot_02.bin"

sound_33_slot_03:
    .incbin "assets/audio_data/sound_33_slot_03.bin"

sound_34_slot_00:
    .incbin "assets/audio_data/sound_34_slot_00.bin"

sound_34_slot_01:
    .incbin "assets/audio_data/sound_34_slot_01.bin"

sound_34_slot_02:
    .incbin "assets/audio_data/sound_34_slot_02.bin"

sound_34_slot_03:
    .incbin "assets/audio_data/sound_34_slot_03.bin"

sound_35_slot_00:
    .incbin "assets/audio_data/sound_35_slot_00.bin"

sound_35_slot_01:
    .incbin "assets/audio_data/sound_35_slot_01.bin"

sound_35_slot_02:
    .incbin "assets/audio_data/sound_35_slot_02.bin"

sound_35_slot_03:
    .incbin "assets/audio_data/sound_35_slot_03.bin"

; unused #$21 bytes out of #$2,000 bytes total (99.60% full)
; unused 33 bytes out of 8,192 bytes total (99.60% full)
; filled with 33 #$ff bytes by superc.cfg configuration
bank_b_unused_space:

.segment "BANKD_ID"

; bank byte
; see load_sound_banks_init_sound
    .byte $3d