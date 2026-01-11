; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank 7 contains a continuation of Bank 6's Level 4 (Inner Base) supertile
; definitions.  Then, it contains Level 4's supertile palette definitions.  From
; there, Bank 7 contains Level 3 (Jungle) screen layout and the table of
; supertiles per screen for Level 3.  Finally, Bank 7 contains the logic to
; set the default palette colors for a level or scene (intro, ending credits).

; 8 KiB PRG ROM
.segment "BANK7"

.include "constants.asm"

; export for bank f
.export level_3_supertiles_screen_ptr_table
.export set_palettes
.export level_3_supertile_data
.export level_3_palette_data
.export level_4_palette_data
.export set_palettes_for_current_level
.export level_3_screen_layout_tbl

; level_4_supertile_data continued
    .byte $79,$7a,$14,$7b,$15,$7c,$7d,$7e,$7f,$80                         ; #$04 - rotating gun
    .byte $81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90 ; #$05 - rotating gun
    .byte $91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0 ; #$06 - rotating gun
    .byte $12,$75,$13,$76,$77,$78,$79,$7a,$14,$7b,$15,$7c,$7d,$7e,$7f,$80 ; #$07 - rotating gun
    .byte $81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90 ; #$08 - rotating gun
    .byte $91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0 ; #$09 - rotating gun
    .byte $12,$75,$13,$76,$77,$78,$79,$7a,$14,$7b,$15,$7c,$7d,$7e,$7f,$80 ; #$0a - rotating gun
    .byte $81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90 ; #$0b - rotating gun
    .byte $91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0 ; #$0c - rotating gun (destroyed)
    .byte $91,$82,$83,$84,$85,$90,$a3,$88,$89,$a4,$31,$8c,$9d,$8e,$8f,$80 ; #$0d - rotating gun
    .byte $42,$48,$43,$3a,$42,$57,$44,$3a,$42,$3a,$3a,$3a,$60,$d2,$61,$d2
    .byte $48,$43,$3a,$56,$57,$44,$3a,$56,$3a,$3a,$3a,$56,$61,$d2,$61,$62 ; #$0f - rotating gun
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$49,$51,$49,$51,$52,$53,$52,$53 ; #$10 - rotating gun
    .byte $46,$46,$46,$5d,$3a,$3a,$3a,$56,$46,$46,$46,$5d,$3a,$3a,$3a,$56
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$32,$32,$32,$32,$dc,$dc,$dc,$dc
    .byte $32,$d3,$3f,$3a,$dc,$d4,$3d,$3a,$34,$34,$3a,$3a,$c5,$c5,$c5,$c5
    .byte $0d,$0f,$0d,$0f,$e7,$e8,$e7,$e8,$2b,$2b,$2b,$2b,$dd,$de,$de,$df
    .byte $2b,$2b,$2b,$2b,$dd,$de,$de,$df,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$ef,$0c,$00,$3a,$f0,$0c,$00,$37,$37,$0c,$00,$3a,$f8,$0c,$00
    .byte $38,$ef,$0c,$00,$f2,$f3,$0c,$00,$39,$f4,$0c,$00,$3a,$f0,$0c,$00
    .byte $3a,$f0,$0c,$00,$f5,$f6,$0c,$00,$3b,$f7,$0c,$00,$c5,$f1,$1b,$00
    .byte $00,$d5,$33,$ea,$00,$d5,$eb,$ec,$00,$d5,$3c,$ed,$00,$d5,$3d,$3a
    .byte $00,$d5,$33,$34,$00,$d5,$3d,$3a,$00,$d5,$37,$37,$00,$d5,$3f,$3a
    .byte $00,$d5,$3d,$3a,$00,$d5,$ee,$43,$00,$d5,$3e,$44,$00,$d6,$e9,$c5
    .byte $34,$ef,$0e,$00,$37,$37,$0c,$00,$3a,$f0,$0c,$00,$c5,$f1,$1b,$00
    .byte $00,$d9,$33,$34,$00,$d5,$37,$37,$00,$d5,$3d,$3a,$00,$d6,$e9,$c5
    .byte $3a,$f8,$0c,$00,$c5,$f1,$0c,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$d5,$3f,$3a,$00,$d5,$e9,$c5,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$ef,$0c,$0f,$f5,$f6,$0c,$e8,$3b,$f7,$28,$2b,$c5,$f1,$de,$df
    .byte $0f,$d5,$33,$ea,$e8,$d5,$eb,$ec,$2b,$d7,$3c,$ed,$dd,$df,$e9,$c5
    .byte $38,$ef,$28,$2b,$f2,$f3,$de,$de,$39,$f4,$0e,$00,$c5,$f1,$1b,$00
    .byte $2b,$d7,$33,$34,$de,$df,$ee,$43,$00,$d9,$3e,$44,$00,$d6,$e9,$c5
    .byte $00,$bf,$10,$10,$00,$bf,$10,$10,$00,$bf,$10,$10,$00,$bf,$10,$10
    .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .byte $10,$10,$11,$00,$10,$10,$11,$00,$10,$10,$11,$00,$10,$10,$11,$00
    .byte $06,$b2,$07,$00,$b6,$b7,$b8,$00,$09,$ba,$00,$00,$00,$00,$00,$00
    .byte $00,$bf,$10,$10,$00,$bf,$10,$10,$00,$bf,$10,$10,$00,$bf,$10,$10
    .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .byte $10,$10,$11,$00,$10,$10,$11,$00,$10,$10,$11,$00,$10,$10,$11,$00
    .byte $00,$00,$00,$00,$00,$bc,$bd,$00,$00,$be,$16,$00,$00,$00,$00,$00
    .byte $2b,$2b,$2b,$2b,$dd,$de,$de,$df,$00,$c0,$17,$17,$00,$bf,$10,$10
    .byte $2b,$2b,$2b,$2b,$dd,$de,$de,$df,$17,$17,$17,$17,$10,$10,$10,$10
    .byte $2b,$2b,$2b,$2b,$dd,$de,$de,$df,$17,$17,$18,$00,$10,$10,$11,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$0d,$0f,$0d,$0f,$e7,$e8,$e7,$e8
    .byte $34,$ef,$2c,$2b,$3a,$3a,$22,$22,$3a,$3a,$3a,$3a,$c5,$c5,$c5,$c5
    .byte $4a,$d7,$33,$34,$22,$22,$3a,$3a,$3a,$3a,$3a,$3a,$c5,$c5,$c5,$c5
    .byte $2b,$2b,$00,$00,$de,$df,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $0d,$0f,$00,$00,$e7,$e8,$00,$00,$2b,$2b,$00,$00,$dd,$df,$00,$00
    .byte $34,$ef,$28,$2b,$3a,$3a,$de,$de,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$bf,$10,$10,$00,$bf,$10,$10,$0d,$0f,$0d,$0f,$e7,$e8,$e7,$e8
    .byte $10,$10,$10,$10,$10,$10,$10,$10,$0d,$0f,$0d,$0f,$e7,$e8,$e7,$e8
    .byte $10,$10,$11,$00,$10,$10,$11,$00,$0d,$0f,$0d,$0f,$e7,$e8,$e7,$e8
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2b,$2b,$00,$00,$dd,$df
    .byte $2b,$2b,$2b,$2b,$dd,$de,$de,$df,$0d,$0f,$0d,$0f,$e7,$e8,$e7,$e8
    .byte $34,$ef,$0c,$00,$37,$37,$0c,$00,$3a,$f0,$0c,$0f,$c5,$f1,$0c,$e8
    .byte $00,$d5,$33,$34,$00,$d5,$37,$37,$0f,$d5,$3d,$3a,$e8,$d5,$3d,$3a
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2b,$2b,$2b,$2b,$dd,$de,$de,$df
    .byte $34,$ef,$28,$2b,$37,$37,$34,$34,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $2b,$d7,$33,$34,$34,$34,$37,$37,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $2b,$2b,$00,$00,$34,$ef,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$2b,$2b,$00,$00,$dd,$de,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $4a,$4a,$00,$00,$22,$ef,$00,$00,$3a,$f0,$49,$51,$c5,$f1,$52,$53
    .byte $00,$00,$4a,$4a,$00,$00,$21,$34,$49,$51,$3d,$3a,$52,$53,$c4,$c5
    .byte $34,$ef,$0c,$17,$3a,$f0,$0c,$17,$37,$37,$0c,$17,$3a,$f8,$0c,$17
    .byte $38,$ef,$0c,$17,$f2,$f3,$0c,$17,$39,$f4,$0c,$17,$3a,$f0,$0c,$17
    .byte $3a,$f0,$0c,$17,$f5,$f6,$0c,$17,$3b,$f7,$0c,$17,$c5,$f1,$1b,$17
    .byte $17,$d5,$33,$ea,$17,$d5,$eb,$ec,$17,$d5,$3c,$ed,$17,$d5,$3d,$3a
    .byte $17,$d5,$33,$34,$17,$d5,$3d,$3a,$17,$d5,$37,$37,$17,$d5,$3f,$3a
    .byte $17,$d5,$3d,$3a,$17,$d5,$ee,$43,$17,$d5,$3e,$44,$17,$d6,$e9,$c5
    .byte $34,$ef,$0e,$17,$37,$37,$0c,$17,$3a,$f0,$0c,$17,$c5,$f1,$1b,$17
    .byte $17,$d9,$33,$34,$17,$d5,$37,$37,$17,$d5,$3d,$3a,$17,$d6,$e9,$c5
    .byte $3a,$f8,$0c,$17,$c5,$f1,$0c,$17,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $17,$d5,$3f,$3a,$17,$d5,$e9,$c5,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $38,$ef,$28,$2b,$f2,$f3,$de,$de,$39,$f4,$0e,$17,$c5,$f1,$1b,$17
    .byte $2b,$d7,$33,$34,$de,$df,$ee,$43,$17,$d9,$3e,$44,$17,$d6,$e9,$c5
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2b,$2b,$00,$00,$dd,$df,$00,$00
    .byte $34,$ef,$0c,$00,$f5,$f6,$0c,$00,$3b,$f7,$28,$2b,$c5,$f1,$de,$df
    .byte $00,$d5,$33,$ea,$00,$d5,$eb,$ec,$2b,$d7,$3c,$ed,$dd,$df,$e9,$c5
    .byte $10,$10,$11,$00,$10,$10,$11,$00,$2b,$2b,$2b,$2b,$dd,$de,$de,$df
    .byte $00,$bf,$10,$10,$00,$bf,$10,$10,$2b,$2b,$2b,$2b,$dd,$de,$de,$df
    .byte $3a,$f0,$19,$1a,$3a,$f0,$0c,$00,$3a,$f0,$0c,$00,$c5,$f1,$1b,$00
    .byte $1a,$e1,$3d,$3a,$00,$d5,$3d,$3a,$00,$d5,$3d,$3a,$00,$d6,$e9,$c5
    .byte $3a,$f0,$0c,$00,$c5,$f1,$1b,$00,$34,$ef,$19,$1a,$3a,$f0,$0c,$00
    .byte $00,$d5,$3d,$3a,$00,$d6,$e9,$c5,$1a,$e1,$33,$34,$00,$d5,$3d,$3a
    .byte $1a,$1a,$1a,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$1a,$1a,$1a,$1a,$00,$00,$00,$00
    .byte $3a,$f0,$1d,$00,$f5,$f6,$1d,$00,$3b,$f7,$27,$2b,$c5,$f1,$de,$de
    .byte $00,$e3,$3d,$3a,$00,$e3,$ee,$43,$2b,$e4,$3e,$44,$de,$de,$3d,$c5
    .byte $38,$ef,$0c,$00,$f2,$f3,$0c,$00,$39,$f4,$1c,$00,$c5,$f1,$1d,$00
    .byte $00,$d5,$33,$34,$00,$d5,$37,$37,$00,$e2,$3d,$3a,$00,$e3,$e9,$c5
    .byte $34,$ef,$1d,$00,$3a,$f0,$1d,$00,$37,$37,$1d,$00,$3a,$f8,$e5,$00
    .byte $00,$e3,$33,$ea,$00,$e3,$eb,$ec,$00,$e3,$3c,$ed,$00,$e6,$3d,$3a
    .byte $38,$ef,$28,$2b,$f2,$f3,$de,$de,$39,$f4,$1c,$00,$c5,$f1,$1d,$00
    .byte $2b,$d7,$33,$34,$de,$df,$ee,$43,$00,$e2,$3e,$44,$00,$e3,$e9,$c5
    .byte $00,$b0,$05,$b1,$00,$b3,$b4,$b5,$00,$00,$08,$b9,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$fa,$f9,$00,$a5,$1f,$00,$00,$00,$1f,$00
    .byte $00,$00,$00,$00,$f9,$f9,$fb,$00,$2b,$2b,$2b,$2b,$dd,$de,$de,$df
    .byte $00,$00,$1f,$00,$69,$00,$1f,$00,$00,$00,$1f,$00,$a5,$00,$1f,$00
    .byte $00,$00,$1f,$00,$f9,$f9,$fd,$00,$00,$00,$00,$00,$00,$a5,$00,$00
    .byte $00,$00,$1f,$00,$a5,$00,$fc,$f9,$00,$00,$00,$00,$00,$a5,$00,$00
    .byte $00,$00,$00,$00,$f9,$f9,$f9,$f9,$00,$00,$00,$00,$00,$00,$a5,$00
    .byte $00,$00,$00,$00,$f9,$f9,$fb,$00,$00,$00,$1f,$00,$a5,$00,$1f,$00
    .byte $00,$00,$1f,$00,$f9,$f9,$1f,$f9,$00,$00,$1f,$00,$00,$00,$1f,$00
    .byte $2b,$2b,$2b,$2b,$dd,$de,$de,$df,$00,$00,$1f,$00,$00,$00,$1f,$00
    .byte $17,$17,$18,$00,$10,$10,$11,$00,$10,$10,$11,$00,$10,$10,$11,$00
    .byte $00,$c0,$17,$17,$00,$bf,$10,$10,$00,$bf,$10,$10,$00,$bf,$10,$10
    .byte $00,$6a,$00,$00,$f9,$f9,$f9,$f9,$00,$6a,$00,$00,$00,$6a,$00,$00
    .byte $00,$00,$00,$a5,$00,$a5,$00,$69,$00,$00,$00,$00,$00,$64,$65,$65
    .byte $00,$6a,$00,$00,$00,$6a,$00,$00,$00,$6a,$00,$00,$65,$68,$00,$00
    .byte $00,$00,$00,$00,$00,$a5,$00,$00,$00,$00,$00,$00,$65,$66,$00,$a5
    .byte $00,$6a,$00,$00,$00,$6a,$00,$a5,$00,$6a,$00,$00,$00,$67,$65,$65
    .byte $00,$a5,$00,$00,$00,$00,$00,$69,$00,$00,$00,$00,$65,$65,$65,$65
    .byte $00,$6a,$00,$a5,$00,$6a,$69,$00,$00,$6a,$00,$a5,$00,$6a,$00,$00
    .byte $40,$40,$29,$6e,$6b,$6c,$70,$71,$30,$6d,$1e,$74,$00,$00,$00,$00
    .byte $2b,$2b,$2b,$2b,$72,$71,$72,$71,$1e,$74,$1e,$74,$00,$00,$00,$00
    .byte $2a,$6f,$40,$40,$72,$73,$6b,$6c,$1e,$74,$30,$6d,$00,$00,$00,$00
    .byte $33,$34,$34,$34,$c4,$c5,$d1,$d1,$00,$00,$01,$a1,$00,$00,$00,$00
    .byte $34,$34,$34,$c7,$d1,$d1,$c5,$c6,$20,$a2,$00,$00,$00,$00,$00,$00
    .byte $33,$c8,$35,$34,$c4,$c9,$ca,$c5,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$cb,$36,$ce,$c5,$cc,$cd,$cf,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$34,$34,$c7,$d2,$c5,$d2,$c6,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $32,$fe,$32,$32,$da,$db,$dc,$dc,$33,$e0,$34,$e0,$c4,$c5,$c5,$c5
    .byte $32,$32,$32,$32,$dc,$dc,$dc,$dc,$34,$cb,$36,$ce,$c5,$cc,$cd,$cf
    .byte $32,$fe,$32,$32,$da,$db,$dc,$dc,$33,$d0,$34,$d0,$c4,$c5,$c5,$c5
    .byte $32,$32,$32,$32,$dc,$dc,$dc,$dc,$33,$c8,$35,$c7,$c4,$c9,$ca,$c6
    .byte $00,$00,$00,$00,$00,$00,$c1,$c2,$00,$a7,$02,$a8,$00,$aa,$ab,$ac
    .byte $00,$00,$00,$00,$c3,$a6,$00,$00,$03,$a9,$04,$00,$ad,$ae,$af,$00
    .byte $3a,$f0,$0c,$00,$f5,$f6,$0c,$00,$3b,$f7,$0c,$00,$c5,$f1,$1c,$00
    .byte $00,$d5,$3d,$3a,$00,$d5,$ee,$43,$00,$d5,$3e,$44,$00,$e2,$e9,$c5
    .byte $3a,$f8,$0c,$00,$c5,$f1,$1c,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$0a,$0a,$0a,$00,$0b,$0b,$0b,$00,$0b,$0b,$0b,$00,$0b,$0b,$0b
    .byte $00,$0b,$0b,$0b,$00,$bb,$bb,$bb,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$cb,$36,$34,$c5,$cc,$cd,$c5,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$c7,$0c,$00,$c5,$c6,$d8,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $32,$32,$32,$32,$dc,$dc,$dc,$dc,$33,$d0,$34,$d0,$c4,$c5,$c5,$c5
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$32,$fe,$32,$32,$da,$dc,$dc,$dc
    .byte $41,$41,$41,$63,$54,$22,$22,$55,$42,$48,$43,$56,$42,$57,$44,$56
    .byte $42,$3a,$3a,$58,$42,$48,$43,$59,$42,$57,$44,$3a,$42,$3a,$3a,$3a
    .byte $41,$41,$41,$63,$34,$5a,$34,$55,$45,$ec,$3a,$56,$5b,$ed,$3a,$56
    .byte $42,$48,$43,$5c,$42,$57,$44,$5e,$47,$46,$46,$5f,$42,$3a,$3a,$3a
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$34,$34,$34,$c8,$d1,$d1,$d1,$c9
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$35,$c7,$33,$cb,$ca,$c6,$c4,$cc
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$36,$ce,$33,$34,$cd,$cf,$c4,$d1
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$34,$34,$34,$34,$d1,$c5,$c5,$c5
    .byte $25,$25,$3c,$ed,$25,$25,$3d,$3a,$25,$25,$3f,$3a,$25,$25,$c4,$c5
    .byte $24,$24,$33,$34,$25,$25,$37,$37,$25,$25,$3d,$3a,$25,$25,$3d,$3a
    .byte $25,$25,$3f,$3a,$4c,$4d,$c4,$c5,$26,$4e,$21,$34,$26,$4e,$3d,$3a
    .byte $26,$4e,$23,$3a,$26,$4e,$3d,$3a,$26,$4e,$23,$3a,$26,$4e,$3f,$3a
    .byte $2d,$32,$32,$32,$50,$ff,$dc,$dc,$2e,$25,$34,$d0,$00,$25,$3a,$3a
    .byte $32,$32,$32,$32,$dc,$dc,$dc,$dc,$34,$d0,$34,$d0,$3a,$3a,$3a,$3a
    .byte $32,$32,$32,$32,$dc,$dc,$dc,$dc,$34,$d0,$34,$34,$3a,$3a,$37,$37
    .byte $2f,$4e,$3d,$3a,$dc,$4f,$c4,$c5,$34,$34,$34,$34,$37,$37,$37,$37
    .byte $33,$34,$34,$34,$c4,$c5,$c5,$c5,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$cb,$36,$ce,$c5,$cc,$cd,$cf,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $3a,$3a,$33,$34,$3a,$3a,$c4,$d1,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $26,$4e,$23,$3a,$26,$4e,$3f,$3a,$26,$4e,$23,$3a,$26,$4e,$3f,$3a
    .byte $00,$00,$00,$00,$1a,$1a,$1a,$1a,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$4b,$33,$ea,$e1,$25,$eb,$ec,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$fb,$00,$fb,$00,$fd,$00,$fd,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$ff,$ff,$d3,$d4,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$d5,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$e4,$e5,$00,$00,$f1,$f2
    .byte $00,$00,$00,$00,$00,$dd,$de,$de,$e6,$e7,$ef,$f0,$f3,$f4,$f9,$fa
    .byte $d6,$d7,$d8,$d9,$de,$de,$df,$e0,$f0,$f0,$e8,$e9,$f9,$fa,$f9,$fa
    .byte $da,$db,$dc,$00,$e1,$e2,$de,$de,$ea,$f0,$f0,$f0,$f9,$fa,$f9,$fa
    .byte $00,$00,$00,$00,$de,$e3,$00,$00,$ef,$eb,$ec,$ed,$f9,$f5,$f6,$f7
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$ee,$00,$00,$00,$f8,$00,$00,$00
    .byte $00,$00,$00,$fb,$00,$00,$00,$fd,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $3a,$3a,$3a,$3a,$c5,$c5,$c5,$c5,$34,$34,$34,$34,$3a,$3a,$3a,$3a
    .byte $3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$c5,$c5,$c5,$c5
    .byte $34,$34,$34,$34,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a
    .byte $3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a

; each byte is an attribute palette byte for a single supertile (4x4 tiles)
level_4_palette_data:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f0,$f0,$f0,$00
    .byte $00,$00,$00,$00,$00,$0f,$0f,$0f,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$88,$88,$88,$22,$22,$22,$88,$22,$08,$02,$80,$20,$00
    .byte $00,$00,$0f,$0f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$cf,$fc,$0f,$ff,$3f,$3f,$cf,$ff,$cf,$c0,$ff,$ff,$3f,$ff
    .byte $33,$f3,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$fc,$f3,$00,$00,$00,$ff,$0f,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_3_screen_layout_tbl:
    .byte $16                                                             ; LEVEL_WIDTH
    .byte $01                                                             ; LEVEL_HEIGHT
    .byte $01,$02,$01,$03,$01,$04,$01,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d
    .byte $0e,$0e,$0e,$0e,$0f,$10

level_3_supertiles_screen_ptr_table:
    .addr level_3_supertiles_screen_00
    .addr level_3_supertiles_screen_01
    .addr level_3_supertiles_screen_02
    .addr level_3_supertiles_screen_03
    .addr level_3_supertiles_screen_04
    .addr level_3_supertiles_screen_05
    .addr level_3_supertiles_screen_06
    .addr level_3_supertiles_screen_07
    .addr level_3_supertiles_screen_08
    .addr level_3_supertiles_screen_09
    .addr level_3_supertiles_screen_0a
    .addr level_3_supertiles_screen_0b
    .addr level_3_supertiles_screen_0c
    .addr level_3_supertiles_screen_0d
    .addr level_3_supertiles_screen_0e
    .addr level_3_supertiles_screen_0f
    .addr level_3_supertiles_screen_10

level_3_supertiles_screen_00:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_01:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$10,$11,$12,$13,$15,$15,$15,$15,$15,$15,$15,$15
    .byte $16,$16,$16,$16,$16,$16,$16,$16,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_02:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$10,$11,$12,$13,$15,$15,$15,$42,$43,$15,$15,$15
    .byte $16,$16,$16,$16,$16,$16,$16,$16,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_03:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$10,$11,$12,$13,$15,$42,$43,$15,$15,$42,$43,$15
    .byte $16,$16,$16,$16,$16,$16,$16,$16,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_04:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$10,$3e,$12,$13,$15,$15,$15,$15,$3f,$40,$41,$15
    .byte $16,$16,$16,$16,$16,$16,$16,$16,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_05:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$10,$18,$19,$1a,$15,$15,$15,$15,$15,$1e,$1f,$1f
    .byte $16,$16,$16,$16,$22,$23,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_06:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $1c,$1a,$19,$1a,$1c,$1a,$1b,$13,$1f,$1f,$1f,$1f,$1f,$1f,$20,$21
    .byte $24,$24,$24,$24,$24,$24,$25,$26,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_07:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$18,$19,$1a,$1c,$1a,$19,$1a,$1d,$1e,$1f,$1f,$1f,$1f,$1f,$1f
    .byte $22,$23,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_08:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $1c,$1a,$19,$1a,$1c,$1a,$19,$1a,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_09:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $1c,$1a,$1b,$13,$10,$11,$12,$13,$1f,$1f,$20,$21,$15,$15,$15,$15
    .byte $24,$24,$25,$26,$16,$16,$16,$16,$00,$00,$00,$00,$00,$00,$00,$00

level_3_supertiles_screen_0a:
    .byte $14,$01,$02,$03,$14,$01,$02,$03,$04,$05,$06,$07,$04,$05,$06,$07
    .byte $08,$09,$0a,$0b,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$10,$11,$12,$13,$15,$15,$15,$15,$15,$15,$15,$15
    .byte $16,$16,$16,$16,$16,$44,$35,$35,$00,$00,$00,$00,$00,$45,$39,$39

level_3_supertiles_screen_0b:
    .byte $14,$01,$02,$34,$00,$00,$00,$00,$04,$05,$06,$38,$00,$00,$00,$00
    .byte $08,$09,$0a,$00,$00,$00,$00,$00,$0c,$0d,$0e,$00,$00,$00,$00,$00
    .byte $10,$11,$12,$00,$00,$00,$00,$00,$15,$15,$15,$27,$31,$32,$33,$3c
    .byte $17,$29,$2a,$2b,$30,$00,$36,$37,$2c,$2d,$2e,$2f,$28,$00,$3a,$3b

level_3_supertiles_screen_0c:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$3c,$3c,$3c,$3c,$3c,$3c,$3c,$3c
    .byte $3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3b,$3b,$3b,$3b,$3b,$3b,$3b,$3b

level_3_supertiles_screen_0d:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$53,$54,$53,$54,$53,$54
    .byte $57,$55,$56,$55,$56,$55,$56,$55,$58,$59,$5a,$59,$5a,$59,$5a,$59

level_3_supertiles_screen_0e:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$53,$54,$53,$54,$53,$54,$53,$54
    .byte $56,$55,$56,$55,$56,$55,$56,$55,$5a,$59,$5a,$59,$5a,$59,$5a,$59

level_3_supertiles_screen_0f:
    .byte $60,$67,$68,$69,$6a,$6b,$6c,$66,$6d,$6e,$6f,$70,$71,$72,$6e,$73
    .byte $60,$74,$74,$75,$76,$74,$74,$66,$77,$78,$79,$7a,$7b,$7a,$7c,$7d
    .byte $7e,$7f,$7f,$7f,$7f,$7f,$7f,$80,$53,$54,$53,$54,$53,$54,$53,$54
    .byte $56,$55,$56,$55,$56,$55,$56,$55,$5a,$59,$5a,$59,$5a,$59,$5a,$59

level_3_supertiles_screen_10:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $5b,$5c,$5c,$5d,$5e,$5c,$5c,$5f,$60,$61,$62,$63,$64,$65,$61,$66

level_3_supertile_data:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$6d,$0d,$6f,$4c,$4d,$6e,$70
    .byte $00,$00,$01,$47,$48,$49,$4a,$4b,$02,$4e,$03,$4f,$51,$52,$53,$54
    .byte $00,$00,$00,$00,$4c,$4d,$00,$00,$04,$50,$05,$72,$55,$56,$57,$73
    .byte $02,$4e,$03,$4f,$51,$52,$53,$54,$06,$59,$07,$5a,$5d,$5e,$5f,$60
    .byte $04,$50,$05,$58,$55,$56,$57,$5c,$08,$5b,$0f,$64,$61,$62,$63,$70
    .byte $06,$59,$07,$5a,$5d,$5e,$5f,$60,$09,$65,$0a,$66,$68,$69,$6a,$6b
    .byte $08,$5b,$0f,$74,$61,$62,$63,$70,$0b,$67,$0c,$58,$6c,$75,$76,$5c
    .byte $11,$65,$0a,$66,$68,$69,$6a,$6b,$00,$13,$13,$82,$00,$83,$15,$85
    .byte $0b,$67,$0c,$77,$6c,$75,$76,$7d,$12,$79,$10,$7a,$70,$00,$7b,$7c
    .byte $00,$78,$13,$82,$7e,$7f,$15,$86,$13,$00,$15,$86,$83,$00,$15,$86
    .byte $12,$79,$10,$7a,$70,$00,$7b,$7c,$14,$82,$14,$82,$18,$84,$17,$86
    .byte $00,$83,$15,$86,$00,$83,$15,$85,$00,$16,$15,$85,$00,$83,$19,$85
    .byte $14,$82,$14,$82,$18,$84,$17,$86,$18,$84,$17,$86,$18,$84,$17,$86
    .byte $16,$00,$15,$86,$83,$00,$19,$85,$16,$00,$19,$85,$83,$00,$19,$85
    .byte $18,$84,$17,$86,$18,$84,$17,$86,$18,$84,$17,$86,$18,$84,$17,$86
    .byte $00,$16,$19,$85,$00,$83,$19,$87,$00,$83,$19,$87,$00,$00,$88,$80
    .byte $18,$84,$17,$86,$18,$84,$17,$85,$18,$84,$17,$85,$1c,$81,$88,$81
    .byte $16,$00,$19,$85,$83,$00,$19,$87,$16,$00,$19,$87,$83,$00,$88,$80
    .byte $18,$84,$17,$85,$18,$84,$17,$85,$1c,$81,$17,$85,$00,$00,$88,$81
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$0e,$71,$01,$47,$48,$49,$4a,$4b
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$28,$28,$8b,$28,$8b,$28
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$8c,$2f,$2f,$8d,$2f,$8c,$35,$8e,$35,$35
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$8c,$2f,$2f,$8c,$2f,$8c,$8c,$2f,$8c,$2f
    .byte $1c,$81,$17,$84,$00,$00,$17,$81,$1d,$1d,$1a,$1d,$1b,$1b,$1d,$26
    .byte $16,$00,$19,$87,$83,$00,$88,$80,$1a,$1d,$1a,$1a,$96,$3a,$96,$3a
    .byte $1c,$81,$17,$85,$00,$00,$88,$81,$1d,$1d,$1a,$1d,$96,$3a,$96,$3a
    .byte $16,$00,$19,$87,$83,$00,$88,$80,$1a,$1a,$1a,$1a,$1e,$1b,$1d,$1b
    .byte $00,$83,$19,$87,$00,$83,$88,$80,$1a,$1a,$1a,$1a,$96,$3a,$96,$3a
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$28,$91,$8b,$28,$28,$91
    .byte $1b,$26,$1f,$3b,$8f,$1f,$3c,$3d,$26,$3c,$24,$3f,$43,$42,$37,$3f
    .byte $20,$3b,$20,$3b,$3e,$3d,$3e,$3d,$24,$3f,$24,$3f,$3f,$24,$3f,$24
    .byte $20,$97,$1e,$1b,$3e,$25,$97,$90,$24,$3f,$25,$1e,$3f,$3f,$42,$40
    .byte $1b,$1b,$1b,$1b,$90,$8a,$89,$8a,$29,$28,$28,$28,$29,$8c,$8b,$2f
    .byte $2f,$8b,$28,$91,$8c,$2f,$8c,$28,$2f,$8d,$2f,$8c,$35,$8e,$35,$35
    .byte $43,$42,$42,$3f,$95,$42,$37,$44,$2e,$95,$37,$37,$35,$35,$94,$94
    .byte $44,$3f,$44,$3f,$37,$44,$3f,$37,$37,$92,$37,$37,$94,$93,$94,$94
    .byte $44,$3f,$42,$40,$3f,$44,$42,$41,$37,$92,$41,$2e,$94,$93,$35,$35
    .byte $29,$28,$2f,$8c,$2f,$2f,$8c,$2f,$2f,$8d,$2f,$8c,$35,$8e,$35,$35
    .byte $1a,$1a,$00,$00,$b9,$b9,$ba,$bb,$28,$2f,$28,$29,$8b,$28,$8b,$28
    .byte $30,$a4,$2d,$00,$9c,$a0,$00,$00,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$8c,$2f,$2f,$8b,$2f,$8b,$9e,$31,$9e,$31
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$8c,$2f,$2f,$8b,$2f,$9e,$9e,$31,$9f,$00
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$9e,$31,$31,$9f,$2d,$a1,$a2,$a2,$98,$9a
    .byte $28,$28,$31,$9f,$35,$9f,$00,$a2,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $32,$a1,$32,$32,$99,$9a,$9c,$99,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $32,$a1,$32,$9b,$99,$9a,$9c,$9d,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $30,$98,$30,$9b,$9c,$9a,$9c,$9d,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $2f,$9f,$2d,$00,$9f,$a3,$00,$00,$32,$a4,$2d,$00,$9a,$a0,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2a,$2b,$2b,$a5,$a7,$a9,$a8,$a9
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2c,$a5,$2c,$a5,$a8,$a9,$a8,$a9
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2c,$a5,$2c,$a6,$a8,$a9,$a8,$aa
    .byte $00,$00,$00,$00,$4c,$4d,$00,$00,$04,$50,$05,$00,$55,$56,$57,$00
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$8c,$2f,$2f,$8c,$2f,$8c,$28,$2f,$28,$2f
    .byte $2d,$00,$33,$ad,$00,$ae,$af,$b0,$2d,$b3,$b8,$b5,$b6,$b7,$b8,$b5
    .byte $34,$34,$34,$34,$b1,$b1,$b2,$b2,$34,$34,$34,$34,$34,$34,$34,$34
    .byte $08,$5b,$00,$00,$61,$62,$45,$00,$0b,$46,$0c,$00,$6c,$00,$00,$00
    .byte $35,$8d,$2f,$28,$00,$8e,$35,$35,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $36,$b7,$b8,$b5,$ab,$b7,$b8,$b5,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $34,$34,$34,$34,$34,$34,$34,$34,$2d,$2d,$2d,$2d,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2d,$2d,$2d,$2d,$ac,$ac,$ac,$ac
    .byte $34,$34,$34,$34,$b2,$b2,$b2,$b2,$34,$34,$34,$34,$34,$34,$34,$34
    .byte $18,$84,$17,$86,$00,$00,$98,$85,$21,$99,$00,$00,$9a,$9b,$9c,$00
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$28,$a3,$8b,$28,$8b,$a7
    .byte $22,$9d,$23,$9e,$9f,$a0,$a1,$a2,$38,$a4,$39,$a5,$a8,$a9,$aa,$ab
    .byte $1a,$1b,$1b,$1b,$8f,$8a,$89,$8a,$27,$a6,$28,$28,$ac,$ad,$8b,$28
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$28,$28,$8b,$28,$8b,$bb
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$28,$28,$bc,$bd,$be,$bf
    .byte $2f,$8b,$2f,$8b,$8c,$2f,$8c,$2f,$2f,$8d,$2f,$8c,$35,$8e,$35,$8d
    .byte $2d,$00,$2d,$8e,$00,$00,$00,$00,$2d,$00,$2d,$00,$00,$00,$00,$00
    .byte $18,$84,$17,$86,$00,$00,$98,$85,$ae,$af,$00,$00,$9a,$b0,$b1,$00
    .byte $22,$b2,$b3,$9e,$9f,$a0,$a1,$a2,$38,$a4,$39,$b4,$a8,$a9,$aa,$b5
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$b6,$b7,$b8,$b9,$a8,$a9,$aa,$ba
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$c0,$c1,$8b,$28,$c5,$c6 ; #$49 - grass-covered turret frame #$00 (barely open)
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$c2,$c3,$c4,$28,$c7,$c7,$c8,$c9 ; #$4a - grass-covered turret frame #$00 (barely open)
    .byte $1b,$1b,$1b,$1b,$89,$8a,$ca,$cb,$28,$28,$d0,$d1,$8b,$d5,$00,$d6 ; #$4b - grass-covered turret frame #$01 (fully showing)
    .byte $1b,$1b,$1b,$1b,$cc,$cd,$ce,$8a,$d2,$d3,$d4,$28,$d7,$d8,$d9,$da ; #$4c - grass-covered turret frame #$01 (fully showing)
    .byte $1b,$1b,$1b,$1b,$89,$8a,$db,$dc,$28,$28,$e0,$e1,$8b,$d5,$00,$e5 ; #$4d - grass-covered turret frame #$02 (locked into position)
    .byte $1b,$1b,$1b,$1b,$dd,$de,$df,$8a,$e2,$e3,$e4,$28,$e6,$e7,$e8,$da ; #$4e - grass-covered turret frame #$02 (locked into position)
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$28,$28,$e9,$ea,$8b,$28,$ed,$ee ; #$4f - grass-covered turret frame #$03 (destroyed)
    .byte $1b,$1b,$1b,$1b,$89,$8a,$89,$8a,$eb,$eb,$ec,$28,$ef,$ef,$f0,$f1 ; #$50 - grass-covered turret frame #$03 (destroyed)
    .byte $00,$00,$00,$00,$00,$00,$00,$bc,$2d,$00,$27,$bd,$ac,$ac,$be,$bf
    .byte $28,$c3,$29,$c4,$c5,$c6,$c7,$c8,$2e,$c9,$2f,$ca,$cb,$cc,$cd,$ce
    .byte $2a,$2a,$2b,$2b,$c5,$c6,$c5,$c6,$2e,$c9,$30,$cf,$cb,$cc,$d1,$d2
    .byte $2c,$2b,$2b,$2c,$c5,$c6,$c7,$c8,$2e,$c9,$2f,$ca,$cb,$cc,$cd,$ce
    .byte $30,$cf,$31,$d0,$d1,$d2,$d3,$d4,$2f,$ca,$2e,$c9,$cd,$ce,$cb,$cc
    .byte $30,$d0,$2f,$ca,$cb,$cc,$cd,$ce,$30,$cf,$31,$d0,$d1,$d2,$d3,$d4
    .byte $34,$34,$34,$c0,$b2,$b2,$b2,$c1,$34,$34,$34,$c2,$34,$34,$34,$c2
    .byte $34,$34,$34,$c2,$34,$34,$34,$c2,$34,$34,$34,$c2,$34,$34,$34,$c2
    .byte $31,$d0,$30,$d0,$d3,$d4,$cb,$cc,$2e,$c9,$30,$cf,$cb,$cc,$d1,$d2
    .byte $2f,$ca,$2e,$c9,$cd,$ce,$cb,$cc,$31,$d0,$30,$d0,$d3,$d4,$cb,$cc
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$00,$00,$11,$12
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$20,$20,$20,$20,$26,$26,$26,$26
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$21,$22,$23,$23,$27,$28,$07,$17
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$23,$23,$24,$25,$18,$09,$29,$2a
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$03,$04,$00,$00,$13,$14,$00,$00
    .byte $00,$00,$11,$12,$00,$00,$11,$12,$00,$00,$11,$12,$00,$00,$11,$12
    .byte $26,$0c,$0d,$26,$26,$1c,$1d,$26,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $26,$0c,$0d,$26,$26,$1c,$1d,$2f,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $2b,$2c,$08,$18,$30,$31,$00,$19,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $0b,$00,$2d,$2e,$00,$10,$32,$33,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $26,$0c,$0d,$26,$34,$1c,$1d,$26,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $13,$14,$00,$00,$13,$14,$00,$00,$13,$14,$00,$00,$13,$14,$00,$00
    .byte $35,$35,$35,$35,$3c,$3c,$3c,$3c,$0e,$00,$10,$1a,$0e,$1a,$08,$19
    .byte $35,$35,$36,$37,$3c,$3c,$3d,$31,$08,$08,$08,$0b,$08,$08,$0b,$00
    .byte $38,$80,$00,$68,$80,$00,$6a,$6b,$00,$6e,$6f,$70,$00,$74,$75,$76
    .byte $69,$00,$81,$39,$6c,$6d,$00,$81,$71,$72,$73,$00,$77,$78,$79,$00
    .byte $3a,$3b,$35,$35,$32,$3e,$3c,$3c,$00,$10,$1a,$08,$1a,$08,$19,$08
    .byte $35,$35,$35,$35,$3c,$3c,$3c,$3c,$18,$08,$0b,$0f,$0b,$10,$00,$0f
    .byte $00,$00,$05,$06,$00,$00,$11,$12,$00,$00,$11,$12,$00,$00,$05,$06
    .byte $3f,$3f,$3f,$3f,$44,$44,$44,$44,$26,$26,$26,$26,$26,$0c,$0d,$26
    .byte $3f,$3f,$40,$41,$44,$44,$45,$46,$26,$26,$26,$4b,$26,$0c,$0d,$26
    .byte $82,$00,$7a,$7b,$47,$82,$00,$7e,$4c,$41,$00,$17,$4f,$50,$00,$18
    .byte $7c,$7d,$00,$83,$7f,$00,$83,$48,$00,$00,$42,$4d,$1a,$08,$51,$52
    .byte $42,$43,$3f,$3f,$49,$4a,$44,$44,$4e,$26,$26,$26,$26,$0c,$0d,$26
    .byte $15,$16,$00,$00,$13,$14,$00,$00,$13,$14,$00,$00,$15,$16,$00,$00
    .byte $26,$1c,$1d,$26,$26,$26,$26,$26,$57,$57,$57,$57,$57,$57,$57,$57
    .byte $53,$54,$08,$08,$53,$54,$07,$0b,$57,$58,$59,$59,$57,$57,$57,$57
    .byte $0b,$00,$55,$56,$07,$08,$55,$56,$59,$59,$5a,$57,$57,$57,$57,$57
    .byte $00,$00,$11,$12,$00,$00,$11,$12,$00,$00,$11,$12,$00,$5d,$5e,$12
    .byte $5b,$5b,$5b,$5b,$5c,$5c,$5c,$5c,$1e,$07,$08,$18,$1e,$00,$1a,$0a
    .byte $5b,$5b,$5b,$5b,$5c,$5c,$5c,$5c,$08,$1b,$19,$10,$0a,$08,$0a,$09
    .byte $5b,$5b,$5b,$5b,$5c,$5c,$5c,$5c,$1a,$08,$08,$18,$18,$07,$08,$0b
    .byte $5b,$5b,$5b,$5b,$5c,$5c,$5c,$5c,$1a,$0a,$0b,$10,$84,$07,$0a,$08
    .byte $5b,$5b,$5b,$5b,$5c,$5c,$5c,$5c,$0a,$08,$09,$1f,$0a,$08,$1b,$1f
    .byte $13,$14,$00,$00,$13,$14,$00,$00,$13,$14,$00,$00,$13,$5f,$60,$00
    .byte $00,$61,$62,$63,$00,$85,$86,$87,$00,$85,$86,$87,$00,$85,$86,$87
    .byte $64,$64,$64,$64,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26
    .byte $65,$66,$67,$00,$88,$89,$8a,$00,$88,$89,$8a,$00,$88,$89,$8a,$00

; each byte is an attribute palette byte for a single supertile (4x4 tiles)
level_3_palette_data:
    .byte $c0,$00,$00,$00,$00,$00,$00,$00,$50,$00,$54,$50,$55,$55,$55,$55
    .byte $55,$55,$55,$55,$00,$00,$00,$00,$c5,$f5,$f5,$35,$f5,$00,$bb,$aa
    .byte $ee,$00,$00,$bb,$aa,$ee,$00,$00,$01,$00,$00,$60,$08,$05,$05,$05
    .byte $12,$50,$50,$50,$00,$00,$ff,$ff,$00,$00,$0f,$ff,$ff,$ff,$f5,$00
    .byte $ff,$20,$88,$a2,$00,$00,$f5,$ff,$f0,$88,$a2,$c8,$b2,$c8,$b2,$88
    .byte $00,$b4,$55,$55,$55,$55,$55,$fb,$ff,$55,$55,$ff,$ff,$bf,$ef,$ff
    .byte $ff,$ff,$ff,$fb,$fe,$ff,$ff,$af,$af,$3c,$c3,$af,$af,$ff,$ff,$ff
    .byte $bc,$e3,$ff,$ff,$ff,$fb,$fe,$ff,$af,$af,$af,$af,$af,$ff,$ff,$ff
    .byte $ff,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; loads the palette colors for the current level or scene (intro scenes, and ending credits)
set_palettes_for_current_level:
    lda CURRENT_LEVEL ; load current level

; loads the default palette colors for the level or scene (intro scenes, and ending credits)
; sets universal background color and unused colors to #$0f (COLOR_BLACK_0f)
; input
;  * a - level of palettes to load. additionally #$08, and #$09 for intro, and #$0a for ending credits
set_palettes:
    sta $08  ; set specific palette to load
    lda #$0f ; initialize default color to #$0f (COLOR_BLACK_0f)
    ldy #$1f ; start at end of palette addresses, i.e. PPU address $3f1f

; palette colors all to #$0f (COLOR_BLACK_0f)
@init_palette_loop:
    sta PALETTE_CPU_BUFFER,y        ; set palette address color to black
    dey
    bpl @init_palette_loop
    lda $08                         ; set all colors to black, now load requested palette colors
    asl                             ; double since each entry is a #$02 byte memory address
    tay                             ; transfer to offset register
    lda game_level_palettes_tbl,y   ; load low byte of the address of the palettes
    sta $00
    lda game_level_palettes_tbl+1,y ; load high byte of the address of the palettes
    sta $01
    ldy #$17                        ; start to read from end of palette colors
    ldx #$1f                        ; total number of palette colors is #$1f

@palette_loop:
    txa
    and #$03                 ; see if index modulus 4 is 0, if so this is a universal background color or unused color slot
    beq @next_color          ; branch if universal background color or unused color
                             ; this will leave the default #$0f color (black) that was set from above
    lda ($00),y              ; otherwise, load the next color from the palette data
    sta PALETTE_CPU_BUFFER,x ; store in palette buffer
    dey                      ; move to next color

@next_color:
    dex
    bpl @palette_loop
    rts

; only stores the used palette colors, i.e. doesn't contain the universal background color (#$0f)
; nor does it contain the unused color slots like $3f04
game_level_palettes_tbl:
    .addr palette_indexes_level_1
    .addr palette_indexes_level_2
    .addr palette_indexes_level_3
    .addr palette_indexes_level_4
    .addr palette_indexes_level_5
    .addr palette_indexes_level_6
    .addr palette_indexes_level_7
    .addr palette_indexes_level_8
    .addr palette_indexes_intro_00
    .addr palette_indexes_intro_01
    .addr palette_indexes_end_credits

palette_indexes_level_1:
    .byte COLOR_WHITE_20,       COLOR_LT_GRAY_10,       COLOR_DARK_GRAY_00    ; background palette 0
    .byte COLOR_WHITE_20,       COLOR_LT_GRAY_10,       COLOR_DARK_GRAY_00    ; background palette 1
    .byte COLOR_LT_ORANGE_27,   COLOR_MED_RED_16,       COLOR_DARK_MAGENTA_04 ; background palette 2
    .byte COLOR_MED_RED_16,     COLOR_DARK_RED_06,      COLOR_DARK_GRAY_00    ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,       COLOR_MED_TEAL_1c,      COLOR_BLACK_0f    ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,  COLOR_MED_ORANGE_17,    COLOR_BLACK_0f    ; sprite palette 1
        .byte COLOR_WHITE_20,       COLOR_LT_RED_26,        COLOR_MED_RED_16  ; sprite palette 2
        .byte COLOR_PALE_OLIVE_38,  COLOR_MED_GREEN_1a,     COLOR_BLACK_0f    ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37, COLOR_MED_TEAL_1c,      COLOR_BLACK_0f    ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37, COLOR_MED_RED_16,       COLOR_BLACK_0f    ; sprite palette 1
        .byte COLOR_WHITE_20,       COLOR_LT_RED_26,        COLOR_MED_RED_16  ; sprite palette 2
        .byte COLOR_PALE_MAGENTA_34,COLOR_MED_BLUE_GREEN_1b,COLOR_BLACK_0f    ; sprite palette 3
    .endif

palette_indexes_level_2:
    .byte COLOR_WHITE_20,       COLOR_LT_GRAY_10,       COLOR_DARK_GRAY_00       ; background palette 0
    .byte COLOR_DARK_GRAY_00,   COLOR_LT_GRAY_10,       COLOR_DARK_TEAL_0c       ; background palette 1
    .byte COLOR_WHITE_20,       COLOR_MED_TEAL_1c,      COLOR_DARK_TEAL_0c       ; background palette 2
    .byte COLOR_DARK_RED_06,    COLOR_MED_TEAL_1c,      COLOR_DARK_TEAL_0c       ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,       COLOR_MED_TEAL_1c,      COLOR_BLACK_0f       ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,  COLOR_MED_ORANGE_17,    COLOR_BLACK_0f       ; sprite palette 1
        .byte COLOR_WHITE_20,       COLOR_LT_RED_26,        COLOR_MED_RED_16     ; sprite palette 2
        .byte COLOR_PALE_OLIVE_38,  COLOR_MED_GREEN_1a,     COLOR_BLACK_0f       ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37, COLOR_MED_BLUE_11,      COLOR_DARK_TEAL_0c   ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37, COLOR_MED_RED_16,       COLOR_DARK_ORANGE_07 ; sprite palette 1
        .byte COLOR_WHITE_20,       COLOR_LT_RED_26,        COLOR_MED_RED_16     ; sprite palette 2
        .byte COLOR_PALE_MAGENTA_34,COLOR_MED_BLUE_GREEN_1b,COLOR_BLACK_0f       ; sprite palette 3
    .endif

palette_indexes_level_3:
    .byte COLOR_LT_GREEN_2a,       COLOR_MED_GREEN_1a,  COLOR_DARK_GREEN_0a  ; background palette 0
    .byte COLOR_LT_OLIVE_28,       COLOR_MED_OLIVE_18,  COLOR_DARK_OLIVE_08  ; background palette 1
    .byte COLOR_LT_GRAY_10,        COLOR_MED_GREEN_1a,  COLOR_DARK_GREEN_0a  ; background palette 2
    .byte COLOR_WHITE_20,          COLOR_LT_GRAY_10,    COLOR_DARK_GRAY_00   ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,          COLOR_MED_TEAL_1c,   COLOR_BLACK_0f   ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,     COLOR_MED_ORANGE_17, COLOR_BLACK_0f   ; sprite palette 1
        .byte COLOR_WHITE_20,          COLOR_LT_RED_26,     COLOR_MED_RED_16 ; sprite palette 2
        .byte COLOR_PALE_MAGENTA_34,   COLOR_MED_MAGENTA_14,COLOR_BLACK_0f   ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37,    COLOR_MED_TEAL_1c,   COLOR_BLACK_0f   ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37,    COLOR_MED_RED_16,    COLOR_BLACK_0f   ; sprite palette 1
        .byte COLOR_WHITE_20,          COLOR_LT_RED_26,     COLOR_MED_RED_16 ; sprite palette 2
        .byte COLOR_PALE_BLUE_GREEN_3b,COLOR_MED_MAGENTA_14,COLOR_BLACK_0f   ; sprite palette 3
    .endif

palette_indexes_level_4:
    .byte COLOR_WHITE_20,      COLOR_MED_BLUE_GREEN_1b,COLOR_DARK_GREEN_0a ; background palette 0
    .byte COLOR_DARK_BLUE_01,  COLOR_DARK_GRAY_00,     COLOR_DARK_GREEN_0a ; background palette 1
    .byte COLOR_DARK_BLUE_01,  COLOR_MED_BLUE_GREEN_1b,COLOR_DARK_GREEN_0a ; background palette 2
    .byte COLOR_DARK_BLUE_01,  COLOR_DARK_GRAY_00,     COLOR_DARK_RED_06   ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,      COLOR_MED_TEAL_1c,      COLOR_BLACK_0f  ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38, COLOR_MED_ORANGE_17,    COLOR_BLACK_0f  ; sprite palette 1
    .else
        .byte COLOR_PALE_ORANGE_37,COLOR_MED_TEAL_1c,      COLOR_BLACK_0f  ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37,COLOR_MED_RED_16,       COLOR_BLACK_0f  ; sprite palette 1
    .endif
    .byte COLOR_WHITE_20,      COLOR_LT_RED_26,        COLOR_MED_RED_16    ; sprite palette 2
    .byte COLOR_WHITE_20,      COLOR_MED_BLUE_GREEN_1b,COLOR_DARK_GREEN_0a ; sprite palette 3

palette_indexes_level_5:
    .byte COLOR_WHITE_20,          COLOR_LT_GRAY_10,    COLOR_DARK_GRAY_00    ; background palette 0
    .byte COLOR_LT_GREEN_2a,       COLOR_MED_GREEN_1a,  COLOR_DARK_GRAY_00    ; background palette 1
    .byte COLOR_LT_TEAL_2c,        COLOR_MED_TEAL_1c,   COLOR_DARK_TEAL_0c    ; background palette 2
    .byte COLOR_WHITE_20,          COLOR_LT_GRAY_10,    COLOR_DARK_MAGENTA_04 ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,          COLOR_MED_TEAL_1c,   COLOR_BLACK_0f    ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,     COLOR_MED_ORANGE_17, COLOR_BLACK_0f    ; sprite palette 1
        .byte COLOR_WHITE_20,          COLOR_LT_RED_26,     COLOR_MED_RED_16  ; sprite palette 2
        .byte COLOR_PALE_MAGENTA_34,   COLOR_MED_MAGENTA_14,COLOR_BLACK_0f    ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37,    COLOR_MED_TEAL_1c,   COLOR_BLACK_0f    ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37,    COLOR_MED_RED_16,    COLOR_BLACK_0f    ; sprite palette 1
        .byte COLOR_WHITE_20,          COLOR_LT_RED_26,     COLOR_MED_RED_16  ; sprite palette 2
        .byte COLOR_PALE_BLUE_GREEN_3b,COLOR_MED_MAGENTA_14,COLOR_BLACK_0f    ; sprite palette 3
    .endif

palette_indexes_level_6:
    .byte COLOR_WHITE_20,          COLOR_LT_GRAY_10,         COLOR_DARK_GRAY_00         ; background palette 0
    .byte COLOR_LT_FOREST_GREEN_29,COLOR_MED_FOREST_GREEN_19,COLOR_DARK_FOREST_GREEN_09 ; background palette 1
    .byte COLOR_WHITE_20,          COLOR_LT_GRAY_10,         COLOR_DARK_RED_06          ; background palette 2
    .byte COLOR_LT_GRAY_10,        COLOR_DARK_GRAY_00,       COLOR_DARK_RED_06          ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,          COLOR_MED_TEAL_1c,        COLOR_BLACK_0f         ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,     COLOR_MED_ORANGE_17,      COLOR_BLACK_0f         ; sprite palette 1
        .byte COLOR_WHITE_20,          COLOR_LT_RED_26,          COLOR_MED_RED_16       ; sprite palette 2
        .byte COLOR_WHITE_20,          COLOR_DARK_GRAY_00,         COLOR_DARK_RED_06    ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37,    COLOR_MED_BLUE_11,        COLOR_DARK_TEAL_0c     ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37,    COLOR_MED_RED_16,         COLOR_DARK_ORANGE_07   ; sprite palette 1
        .byte COLOR_WHITE_20,          COLOR_LT_RED_26,          COLOR_MED_RED_16       ; sprite palette 2
        .byte COLOR_LT_GRAY_10,        COLOR_MED_RED_16,         COLOR_BLACK_0f         ; sprite palette 3
    .endif

palette_indexes_level_7:
    .byte COLOR_LT_TEAL_2c,     COLOR_MED_VIOLET_12, COLOR_DARK_VIOLET_02      ; background palette 0
    .byte COLOR_LT_VIOLET_22,   COLOR_MED_VIOLET_12, COLOR_DARK_VIOLET_02      ; background palette 1
    .byte COLOR_LT_ORANGE_27,   COLOR_MED_RED_16,    COLOR_DARK_RED_06         ; background palette 2
    .byte COLOR_MED_TEAL_1c,    COLOR_MED_GREEN_1a,  COLOR_DARK_GREEN_0a       ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,       COLOR_MED_TEAL_1c,   COLOR_BLACK_0f        ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,  COLOR_MED_ORANGE_17, COLOR_BLACK_0f        ; sprite palette 1
        .byte COLOR_WHITE_20,       COLOR_LT_RED_26,     COLOR_MED_RED_16      ; sprite palette 2
        .byte COLOR_PALE_MAGENTA_34,COLOR_MED_MAGENTA_14,COLOR_DARK_MAGENTA_04 ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37, COLOR_MED_TEAL_1c,   COLOR_BLACK_0f        ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37, COLOR_MED_RED_16,    COLOR_BLACK_0f        ; sprite palette 1
        .byte COLOR_WHITE_20,       COLOR_LT_RED_26,     COLOR_MED_RED_16      ; sprite palette 2
        .byte COLOR_LT_MAGENTA_24,  COLOR_MED_MAGENTA_14,COLOR_DARK_MAGENTA_04 ; sprite palette 3
    .endif

palette_indexes_level_8:
    .byte COLOR_LT_MAGENTA_24,       COLOR_MED_MAGENTA_14,COLOR_DARK_MAGENTA_04  ; background palette 0
    .byte COLOR_DARK_PURPLE_03,      COLOR_DARK_VIOLET_02,COLOR_DARK_TEAL_0c     ; background palette 1
    .byte COLOR_WHITE_20,            COLOR_LT_GRAY_10,    COLOR_DARK_GRAY_00     ; background palette 2
    .byte COLOR_DARK_FOREST_GREEN_09,COLOR_MED_OLIVE_18,  COLOR_DARK_OLIVE_08    ; background palette 3
    .ifdef Probotector
        .byte COLOR_WHITE_20,            COLOR_MED_TEAL_1c,   COLOR_BLACK_0f     ; sprite palette 0
        .byte COLOR_PALE_OLIVE_38,       COLOR_MED_ORANGE_17, COLOR_BLACK_0f     ; sprite palette 1
        .byte COLOR_WHITE_20,            COLOR_LT_RED_26,     COLOR_MED_RED_16   ; sprite palette 2
        .byte COLOR_WHITE_20,            COLOR_DARK_GRAY_00,  COLOR_DARK_TEAL_0c ; sprite palette 3
    .else
        .byte COLOR_PALE_ORANGE_37,      COLOR_MED_TEAL_1c,   COLOR_BLACK_0f     ; sprite palette 0
        .byte COLOR_PALE_ORANGE_37,      COLOR_MED_RED_16,    COLOR_BLACK_0f     ; sprite palette 1
        .byte COLOR_WHITE_20,            COLOR_LT_RED_26,     COLOR_MED_RED_16   ; sprite palette 2
        .byte COLOR_WHITE_20,            COLOR_DARK_GRAY_00,  COLOR_DARK_TEAL_0c ; sprite palette 3
    .endif

; intro screen palette ram indexes
palette_indexes_intro_00:
    .ifdef Probotector
        .byte COLOR_WHITE_20,    COLOR_WHITE_30,    COLOR_DARK_RED_06 ; background palette 0
    .else
        .byte COLOR_LT_GRAY_10,  COLOR_LT_ORANGE_27,COLOR_MED_RED_16  ; background palette 0
    .endif
    .byte COLOR_WHITE_20,    COLOR_LT_VIOLET_22,COLOR_DARK_VIOLET_02  ; background palette 1
    .byte COLOR_WHITE_20,    COLOR_LT_GRAY_10,  COLOR_DARK_GRAY_00    ; background palette 2
    .byte COLOR_LT_ORANGE_27,COLOR_MED_RED_16,  COLOR_DARK_RED_06     ; background palette 3
    .byte COLOR_MED_RED_16,  COLOR_DARK_RED_06, COLOR_BLACK_0f        ; sprite palette 0
    .byte COLOR_WHITE_20,    COLOR_MED_RED_16,  COLOR_MED_ORANGE_17   ; sprite palette 1
    .ifdef Probotector
        .byte COLOR_BLACK_0f,    COLOR_BLACK_0f,    COLOR_BLACK_0f    ; sprite palette 2
        .byte COLOR_BLACK_0f,    COLOR_BLACK_0f,    COLOR_DARK_RED_06 ; sprite palette 3
    .else
        .byte COLOR_WHITE_20,    COLOR_LT_RED_26,   COLOR_MED_RED_16  ; sprite palette 2
        .byte COLOR_WHITE_20,    COLOR_DARK_GRAY_00,COLOR_BLACK_0f    ; sprite palette 3
    .endif

; flashing animation intro screen palette ram indexes
palette_indexes_intro_01:
    .byte COLOR_BLACK_0f,COLOR_BLACK_0f,    COLOR_BLACK_0f          ; background palette 0
    .byte COLOR_WHITE_20,COLOR_LT_VIOLET_22,COLOR_DARK_VIOLET_02    ; background palette 1
    .byte COLOR_BLACK_0f,COLOR_BLACK_0f,    COLOR_BLACK_0f          ; background palette 2
    .byte COLOR_BLACK_0f,COLOR_BLACK_0f,    COLOR_BLACK_0f          ; background palette 3
    .ifdef Probotector
        .byte COLOR_MED_RED_16,  COLOR_DARK_RED_06,COLOR_BLACK_0f   ; sprite palette 0
        .byte COLOR_BLACK_0f,    COLOR_BLACK_0f,   COLOR_BLACK_0f   ; sprite palette 1
        .byte COLOR_BLACK_0f,    COLOR_BLACK_0f,   COLOR_BLACK_0f   ; sprite palette 2
        .byte COLOR_BLACK_0f,    COLOR_BLACK_0f,   COLOR_BLACK_0f   ; sprite palette 3
    .else
        .byte COLOR_BLACK_0f,COLOR_BLACK_0f,    COLOR_BLACK_0f      ; sprite palette 0
        .byte COLOR_WHITE_20,COLOR_MED_RED_16,  COLOR_MED_ORANGE_17 ; sprite palette 1
        .byte COLOR_WHITE_20,COLOR_LT_RED_26,   COLOR_MED_RED_16    ; sprite palette 2
        .byte COLOR_WHITE_20,COLOR_DARK_GRAY_00,COLOR_BLACK_0f      ; sprite palette 3
    .endif

palette_indexes_end_credits:
    .byte COLOR_WHITE_20,      COLOR_LT_ORANGE_27,COLOR_MED_RED_16      ; background palette 0
    .byte COLOR_LT_ORANGE_27,  COLOR_MED_RED_16,  COLOR_DARK_MAGENTA_04 ; background palette 1
    .byte COLOR_LT_ORANGE_27,  COLOR_MED_RED_16,  COLOR_DARK_MAGENTA_04 ; background palette 2
    .byte COLOR_LT_RED_26,     COLOR_MED_RED_16,  COLOR_DARK_PINK_05    ; background palette 3
    .byte COLOR_WHITE_20,      COLOR_LT_ORANGE_27,COLOR_BLACK_0f        ; sprite palette 0
    .byte COLOR_WHITE_20,      COLOR_MED_RED_16,  COLOR_MED_ORANGE_17   ; sprite palette 1
    .byte COLOR_WHITE_20,      COLOR_LT_RED_26,   COLOR_MED_RED_16      ; sprite palette 2
    .byte COLOR_PALE_ORANGE_37,COLOR_DARK_GRAY_00,COLOR_BLACK_0f        ; sprite palette 3

.ifdef Probotector
    ; !(UNUSED) duplicated bank f data
    ; starting at alternate_palettes_tbl offset 97 ($fa1d)
    ; until reset_vector offset 12 ($fb1c)
    ; probably a leftover artifact of the build system
    ; can be safely removed and used for other purposes
    .incbin "assets/chr_rom/unused_remnant_03.bin"
.endif

; unused #$62e bytes out of #$2,000 bytes total (80.69% full)
; unused 1,582 bytes out of 8,192 bytes total (80.69% full)
; filled with 1,582 #$ff bytes by superc.cfg configuration
bank_7_unused_space:

.segment "BANK7_ID"

; bank byte
; see load_sound_banks_init_sound
    .byte $37