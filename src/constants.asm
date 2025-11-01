; NES Super C Disassembly - v1.00
; https://github.com/vermiceli/nes-super-c/
; constants.asm contains the list of constants with meaningful names for the
; memory addresses used by the game. It also contains constants for the various
; palette colors.

.importzp ENEMY_CURRENT_SLOT              ; $10
.importzp GAME_ROUTINE_INDEX              ; $18
.importzp INTRO_SCREEN_STATE              ; $19
.importzp FRAME_COUNTER                   ; $1b
.importzp NMI_CHECK                       ; $1c
.importzp PPU_READY                       ; $1d
.importzp GRAPHICS_BUFFER_OFFSET          ; $1e
.importzp DEMO_MODE                       ; $1f
.importzp PLAYER_COUNT                    ; $20
.importzp PLAYER_INDEX                    ; $21
.importzp PLAYER_MODE                     ; $22
.importzp RANDOM_NUM                      ; $23
.importzp OAMDMA_CPU_BUFFER_OFFSET        ; $24
.importzp BANK_SELECT                     ; $25
.importzp NT_MIRRORING                    ; $26
.importzp IRQ_TYPE                        ; $27
.importzp IRQ_ROUTINE                     ; $28
.importzp IRQ_ROUTINE_ADDR                ; $29
.importzp NOOP_IRQ_FLAG                   ; $2b
.importzp CONT_END_SELECTION              ; $2c
.importzp IRQ_PTR_TBL                     ; $2d
.importzp SUPERTILE_ATTRIBUTE_OVERRIDE    ; $2f
.importzp LEVEL_SCREEN_LAYOUT             ; $30
.importzp LEVEL_SCREEN_SUPERTILES_PTR     ; $32
.importzp LEVEL_SUPERTILE_DATA_PTR        ; $34
.importzp LEVEL_SUPERTILE_PALETTE_DATA    ; $36
.importzp LEVEL_ROUTINE_INDEX             ; $38
.importzp PAUSE_STATE                     ; $39
.importzp UNUSED_PAUSE_FLAG               ; $3a
.importzp DEMO_LEVEL_END_FLAG             ; $3b
.importzp DELAY_TIME_LOW_BYTE             ; $3c
.importzp DELAY_TIME_HIGH_BYTE            ; $3d
.importzp LEVEL_ROUTINE_VAR               ; $3e
.importzp LEVEL_ROUTINE_DELAY             ; $3f
.importzp END_LEVEL_ROUTINE               ; $40
.importzp DEMO_INPUT_NUM_FRAMES           ; $41
.importzp DEMO_INPUT_TBL_INDEX            ; $43
.importzp SCANLINE_IRQ_1                  ; $45
.importzp SCANLINE_IRQ_2_DIFF             ; $46
.importzp SCANLINE_IRQ_3_DIFF             ; $47
.importzp SCANLINE_IRQ_2                  ; $48
.importzp SCANLINE_IRQ_3                  ; $49
.importzp IRQ_PPUADDR                     ; $4a
.importzp DEMO_INPUT_VAL                  ; $4c
.importzp DEMO_FIRE_DELAY_TIMER           ; $4e
.importzp CURRENT_LEVEL                   ; $50
.importzp SOUND_MENU_ROUTINE              ; $50
.importzp EXTRA_LIVES_CHEAT_NUM_CORRECT   ; $50
.importzp GAME_COMPLETED                  ; $51
.importzp ENEMY_DIFFICULTY                ; $52
.importzp PLAYER_NUM_LIVES                ; $53
.importzp SOUND_MENU_INDEX                ; $53
.importzp PLAYER_2_NUM_LIVES              ; $54
.importzp SOUND_MENU_SCROLL               ; $54
.importzp NEXT_1_UP_SCORE_MID             ; $55
.importzp NEXT_1_UP_SCORE_HI              ; $57
.importzp NUM_CONTINUES                   ; $59
.importzp SOUND_PLAYING_MEDLEY            ; $59
.importzp SOUND_MEDLEY_TIMER              ; $5a
.importzp GLOBAL_TIMER                    ; $5b
.importzp BOSS_DEFEATED_FLAGS             ; $5c
.importzp OVERHEAD_FLAG                   ; $5d
.importzp UNUSED_SOUND_MENU_FLAG          ; $5e
.importzp X_DRAW_ROUTINE                  ; $60
.importzp SCREEN_SCROLL_TYPE              ; $61
.importzp CREDITS_ROUTINE                 ; $61
.importzp X_SCROLL_SPEED                  ; $62
.importzp X_SCREEN                        ; $63
.importzp NT_CURRENT_COLUMN               ; $64
.importzp DRAW_X_SCREEN                   ; $65
.importzp PPU_WRITE_ADDR                  ; $66
.importzp HAS_MARKED_SUPERTILE_BACKUP     ; $68
.importzp DRAW_BACKUP_BTM_SUPERTILE       ; $69
.importzp SUPERTILE_DRAW_COUNT            ; $6a
.importzp X_SCROLL_DRAW_POINT             ; $6b
.importzp SCREENS_DRAWN                   ; $6c
.importzp NT_CURRENT_ROW                  ; $6d
.importzp DRAW_Y_SCREEN                   ; $6e
.importzp X_AUTOSCROLL                    ; $6f
.importzp Y_DRAW_ROUTINE                  ; $70
.importzp CREDITS_ROUTINE_DELAY           ; $70
.importzp LEVEL_Y_SCROLL_FLAGS            ; $71
.importzp CREDITS_CURRENT_LINE            ; $71
.importzp Y_SCROLL_SPEED                  ; $72
.importzp NT_ROW_SCROLL                   ; $73
.importzp CREDITS_BTM_CLOUD_SCROLL        ; $73
.importzp LEVEL_Y_SCREEN                  ; $74
.importzp CREDITS_MTN_SCROLL              ; $74
.importzp CREDITS_TOP_CLOUD_SCROLL_DELAY  ; $75
.importzp CREDITS_BTM_CLOUD_SCROLL_DELAY  ; $76
.importzp Y_SCROLL_DIR                    ; $77
.importzp CREDITS_MTN_SCROLL_DELAY        ; $77
.importzp X_SCROLL_TILE_UPDATE            ; $78
.importzp Y_LAYOUT_OFFSET                 ; $79
.importzp BACKUP_X_SCROLL                 ; $7a
.importzp BACKUP_Y_SCROLL                 ; $7b
.importzp BACKUP_PPUCTRL_SETTINGS         ; $7c
.importzp Y_SCROLL_FLAGS                  ; $7d
.importzp ELEVATOR_ENABLED                ; $7e
.importzp ELEVATOR_FRACT_VEL              ; $7f
.importzp ELEVATOR_FAST_VEL               ; $80
.importzp INTRO_ANIM_INDEX                ; $80
.importzp ELEVATOR_VEL_ACCUM              ; $81
.importzp INTRO_ANIM_DELAY                ; $81
.importzp Y_SCREEN                        ; $82
.importzp INTRO_SCREEN_PALETTE            ; $82
.importzp PPUCTRL_TILE_UPDATE             ; $83
.importzp INTRO_ANIM_X_SCROLL             ; $83
.importzp Y_AUTOSCROLL                    ; $84
.importzp STOMP_CEILING_X_SCROLL_CTRL     ; $85
.importzp INTRO_ANIM_PPUCTRL              ; $85
.importzp PAUSE_PALETTE_UPDATES           ; $86
.importzp PLAYER_HUD                      ; $87
.importzp LEVEL_HEIGHT                    ; $88
.importzp LEVEL_WIDTH                     ; $89
.importzp SCREEN_ENEMY_X_CHECK            ; $8a
.importzp SCREEN_ENEMY_Y_CHECK            ; $8b
.importzp X_TILE_ROUTINE                  ; $8c
.importzp Y_TILE_ROUTINE                  ; $8d
.importzp SCROLLING_UP_FLAG               ; $8e
.importzp INCLINE_BG_COLLISION_FLAG       ; $8f
.importzp ENEMY_GEN_COUNT                 ; $90
.importzp ENEMY_GEN_ROUTINE               ; $91
.importzp NT_SCREEN_COUNT                 ; $92
.importzp ENEMY_GEN_DELAY                 ; $93
.importzp ENEMY_GEN_CTRL                  ; $94
.importzp END_LEVEL_FADE_DELAY            ; $95
.importzp ACTIVE_PLAYERS_FLAG             ; $96
.importzp LEVEL_Y_CENTER                  ; $97
.importzp Y_AUTOSCROLL_STOP_POS           ; $98
.importzp ENEMY_GEN_NUM                   ; $99
.importzp PLAYER_Y_VEL_BG_COLLISION_ADJ   ; $9a
.importzp AUTO_MOVE_DELAY_TIMER           ; $9b
.importzp Y_AUTOSCROLL_STOP_SCREEN        ; $9c
.importzp NUM_TANKS                       ; $9f
.importzp ELEVATOR_CHECKPOINT             ; $9f
.importzp PLAYER_STATE                    ; $a0
.importzp PLAYER_Y_ACCUM                  ; $a2
.importzp PLAYER_X_ACCUM                  ; $a4
.importzp PLAYER_Y_FRACT_VELOCITY         ; $a6
.importzp PLAYER_Y_FAST_VELOCITY          ; $a8
.importzp PLAYER_X_FRACT_VELOCITY         ; $aa
.importzp PLAYER_X_FAST_VELOCITY          ; $ac
.importzp PLAYER_JUMP_STATUS              ; $ae
.importzp JUMP_INPUT                      ; $b0
.importzp PLAYER_SURFACE                  ; $b2
.importzp PLAYER_ANIMATION_FRAME_INDEX    ; $b4
.importzp PLAYER_ANIM_FRAME_TIMER         ; $b6
.importzp PLAYER_CURRENT_WEAPON           ; $b8
.importzp PLAYER_AIM_DIR                  ; $ba
.importzp PLAYER_RECOIL_TIMER             ; $bc
.importzp PLAYER_M_WEAPON_FIRE_TIME       ; $be
.importzp PLAYER_STATE_TIMER              ; $c0
.importzp PLAYER_SKIP_COLLISION           ; $c2
.importzp NEW_LIFE_INVINCIBILITY_TIMER    ; $c4
.importzp PLAYER_ACTION_STATE             ; $c6
.importzp F_WEAPON_CHARGE                 ; $c8
.importzp PLAYER_GAME_OVER_STATUS         ; $ca
.importzp PLAYER_X_POS                    ; $cc
.importzp PLAYER_Y_POS                    ; $ce
.importzp PLAYER_OVERHEAD_DIR             ; $d0
.importzp PLAYER_AUTO_MOVE_CHECKPOINT     ; $d2
.importzp INVINCIBILITY_TIMER             ; $d4
.importzp BOSS_DEFEATED_PLAYER_ANIM_SOUND ; $d6
.importzp PLAYER_APPLY_VEL                ; $d8
.importzp CUR_SOUND_ADDR                  ; $e0
.importzp SOUND_VAR_1                     ; $e2
.importzp SOUND_VAR_2                     ; $e4
.importzp SOUND_VAR_3                     ; $e5
.importzp SOUND_PART_ADDR                 ; $e6
.importzp SOUND_CODE_ADDR                 ; $e8
.importzp NUM_SOUND_PARTS                 ; $ea
.importzp CUR_SOUND_PERIOD_LOW            ; $ec
.importzp CUR_SOUND_LEN_TIMER_HIGH        ; $ed
.importzp DEMO_LEVEL                      ; $f0
.importzp CONTROLLER_STATE_DIFF           ; $f1
.importzp CONTROLLER_STATE                ; $f3
.importzp CONTROLLER_STATE_DIFF_B         ; $f5
.importzp CTRL_KNOWN_GOOD                 ; $f7
.importzp IRQ_Y_SCROLL                    ; $f9
.importzp IRQ_X_SCROLL                    ; $fa
.importzp IRQ_PPUCTRL_SETTINGS            ; $fb
.importzp Y_SCROLL                        ; $fc
.importzp X_SCROLL                        ; $fd
.importzp PPUMASK_SETTINGS                ; $fe
.importzp PPUCTRL_SETTINGS                ; $ff

.import SOUND_CMD_LENGTH                ; $0104
.import SOUND_CODE                      ; $010a
.import SOUND_LENGTH_MULTIPLIER         ; $0110
.import SOUND_FLAGS                     ; $0116
.import SOUND_CMD_REPEATS               ; $011c
.import SOUND_CMD_ADDRS_LOW             ; $0128
.import SOUND_CMD_ADDRS_HIGH            ; $012e
.import PARENT_SOUND_ADDRS_LOW          ; $0134
.import PARENT_SOUND_ADDRS_HIGH         ; $013a
.import PARENT_SOUND_ADDRS_2_LOW        ; $014c
.import PARENT_SOUND_ADDRS_2_HIGH       ; $0152
.import SOUND_LEN_TIMERS_HIGH           ; $0158
.import SOUND_CURRENT_SLOT              ; $015b
.import SOUND_PULSE_VOLUME              ; $015e
.import SOUND_CHNL_REG_OFFSET           ; $0160
.import INIT_SOUND_CODE                 ; $0161
.import SOUND_EFFECT_VOLUME             ; $0164
.import SOUND_TRIANGLE_CONFIG           ; $0166
.import SOUND_EFFECT_VOLUME_CONTINUED   ; $0167
.import SOUND_CFG_HIGH_A                ; $016a
.import PAUSE_STATE_01                  ; $016c
.import SOUND_FRAME_SKIP_COUNT          ; $016d
.import SOUND_VOLUME_ADJ2               ; $0170
.import SOUND_FRAME_SKIP                ; $0173
.import VIBRATO_CTRL                    ; $0175
.import SOUND_VAR_UNUSED                ; $0179
.import SOUND_DPCM_SAMPLE               ; $0187
.import SOUND_TIMER_ADJ                 ; $0189
.import SOUND_OCTAVE_SHIFT              ; $018e
.import SOUND_NOTE_PERIOD_OFFSET        ; $0191
.import SOUND_PERIOD                    ; $0194
.import SOUND_LEN                       ; $0197
.import SOUND_CTRL_ENVELOPE_OFFSET      ; $019a
.import SOUND_VOLUME_CHANGE_DELAY       ; $019c
.import SOUND_ENVELOPE_READ_OFFSET      ; $019e
.import SOUND_ENVELOPE_READ_LEN         ; $01a0
.import SOUND_ENVELOPE_BASE_READ_OFFSET ; $01a2
.import DECRESCENDO_END_PAUSE           ; $01a4
.import DECRESCENDO_PERCENT             ; $01a6
.import SOUND_DECRESCENDO_STEP_TIME     ; $01a8
.import SOUND_VOLUME_ADJ_TIMER          ; $01aa
.import SOUND_PULSE_PERIOD_LOW          ; $01ac
.import SOUND_PITCH_ADJ_PERIOD          ; $01ae
.import SOUND_PITCH_ADJ_TIMER           ; $01b0
.import SOUND_PERIOD_MULT               ; $01b2
.import SOUND_PITCH_CTRL                ; $01b4
.import SOUND_PITCH_OFFSET              ; $01b6
.import SOUND_PITCH_CTRL_PARENT_OFFSET  ; $01b8
.import SOUND_PITCH_READ_LEN            ; $01ba
.import SOUND_CFG_HIGH_B                ; $01bc
.import SOUND_PITCH_FLAGS               ; $01be
.import PITCH_OVERWRITE                 ; $01c0
.import SOUND_VOLUME_ADJ                ; $01c2
.import SOUND_PERIOD_LOW                ; $01c4
.import SOUND_LEN_TIMER_HIGH            ; $01cc
.import OAMDMA_CPU_BUFFER               ; $0200
.import CPU_GRAPHICS_BUFFER             ; $0300
.import GRAPHICS_BUFFER_TEMP            ; $03c0
.import SUPERTILE_PALETTE_MERGE         ; $03c8
.import PRE_IRQ_Y_SCROLL_MAX            ; $03ca
.import PRE_IRQ_X_SCROLL_MAX            ; $03cb
.import BG_BOSS_NAMETABLE               ; $03cc
.import PALETTE_CYCLE_INDEX             ; $03cd
.import PALETTE_CYCLE_INDEX_2           ; $03cf
.import COLLISION_CODE_TILE_INDICES     ; $03d0
.import PALETTE_CPU_BUFFER              ; $03e0
.import BG_COLLISION_DATA               ; $0400
.import SECOND_BG_COLLISION_DATA        ; $0480
.import SPRITES                         ; $0500
.import ENEMY_SPRITE                    ; $0508
.import PLAYER_SPRITE                   ; $0518
.import SPRITE_Y_POS                    ; $051a
.import ENEMY_Y_POS                     ; $0522
.import PLAYER_SPRITE_Y_POS             ; $0532
.import SPRITE_X_POS                    ; $0534
.import ENEMY_X_POS                     ; $053c
.import PLAYER_SPRITE_X_POS             ; $054c
.import SPRITE_ATTR                     ; $054e
.import ENEMY_SPRITE_ATTR               ; $0556
.import PLAYER_SPRITE_ATTR              ; $0566
.import PLAYER_BULLET_SPRITE_CODE       ; $0568
.import PLAYER_BULLET_Y_POS             ; $0578
.import PLAYER_BULLET_X_POS             ; $0588
.import PLAYER_BULLET_SPRITE_ATTR       ; $0598
.import PLAYER_BULLET_STATE             ; $05a8
.import PLAYER_BULLET_WEAPON_TYPE       ; $05b8
.import PLAYER_BULLET_Y_VEL_ACCUM       ; $05c8
.import PLAYER_BULLET_X_VEL_ACCUM       ; $05d8
.import PLAYER_BULLET_Y_VEL_FRACT       ; $05e8
.import PLAYER_BULLET_DMG               ; $05f8
.import PLAYER_BULLET_X_VEL_FRACT       ; $0608
.import PLAYER_BULLET_X_VEL_FAST        ; $0618
.import PLAYER_BULLET_TIMER             ; $0628
.import PLAYER_BULLET_COLLISION_CODE    ; $0638
.import PLAYER_BULLET_AIM_DIR           ; $0648
.import PLAYER_BULLET_Y_VEL_FAST        ; $0658
.import ENEMY_ROUTINE                   ; $0668
.import ENEMY_HP                        ; $0676
.import ENEMY_Y_VEL_ACCUM               ; $0684
.import ENEMY_X_VEL_ACCUM               ; $0692
.import ENEMY_Y_VELOCITY_FRACT          ; $06a0
.import ENEMY_Y_VELOCITY_FAST           ; $06ae
.import ENEMY_X_VELOCITY_FRACT          ; $06bc
.import ENEMY_X_VELOCITY_FAST           ; $06ca
.import ENEMY_TYPE                      ; $06d8
.import ENEMY_DELAY                     ; $06e6
.import ENEMY_FIRING_DELAY              ; $06f4
.import ENEMY_ANIMATION_DELAY           ; $0702
.import ENEMY_FRAME                     ; $0710
.import ENEMY_ATTRIBUTES                ; $071e
.import ENEMY_DESTROY_ATTRS             ; $072c
.import ENEMY_COLLISION_INDEX           ; $073a
.import ENEMY_VAR_1                     ; $0748
.import ENEMY_VAR_2                     ; $0756
.import ENEMY_VAR_3                     ; $0764
.import ENEMY_VAR_4                     ; $0772
.import ENEMY_VAR_5                     ; $0780
.import ENEMY_VAR_6                     ; $078e
.import ENEMY_VAR_7                     ; $079c
.import ATTRIBUTE_OVERRIDES             ; $07d0
.import HI_SCORE                        ; $07e0
.import P1_SCORE                        ; $07e3
.import P2_SCORE                        ; $07e6
.import INIT_SOUND_CODE_00              ; $07e9
.import HI_SCORE_RESET_FLAG             ; $07ea
.import CHEAT_CODE_STATUS               ; $07ec
.import PRG_BANK_CHG_X_BACKUP           ; $07ed
.import LOW_PRG_BANK                    ; $07ee
.import HIGH_PRG_BANK                   ; $07ef
.import CHR_BANKS                       ; $07f0
.import LEFT_TOP_HALF_CHR_BANK          ; $07f0
.import LEFT_BOTTOM_CHR_HALF_BANK       ; $07f1
.import RIGHT_FIRST_QTR_CHR_BANK        ; $07f2
.import RIGHT_SECOND_QTR_CHR_BANK       ; $07f3
.import RIGHT_THIRD_QTR_CHR_BANK        ; $07f4
.import RIGHT_FOURTH_QTR_CHR_BANK       ; $07f5
.import SPLIT_X_SCROLL                  ; $07f6
.import SPLIT_PPUCTRL                   ; $07f7
.import SPLIT_SCANLINE_IRQ_2            ; $07f9
.import SPLIT_SCANLINE_IRQ_3            ; $07fa
.import IRQ_HANDLER_PPUADDR             ; $07fb

; PPU (picture processing unit)
PPUCTRL       = $2000
PPUMASK       = $2001
PPUSTATUS     = $2002
OAMADDR       = $2003
PPUSCROLL     = $2005
PPUADDR       = $2006
PPUDATA       = $2007

; APU (audio processing unit)
APU_REGISTER_BASE        = $4000 ; base address for all APU registers
APU_PULSE_CONFIG         = $4000 ; config - DDLC VVVV duty (D), envelope loop / length counter halt (L), constant volume (C), volume/envelope (V)
APU_PULSE_SWEEP          = $4001 ; sweep  - EPPP NSSS enabled (E), period (P), negate (N), shift (S)
APU_PULSE_PERIOD_LOW     = $4002 ; timer  - TTTT TTTT timer low (T). Controls note frequency
APU_PULSE_LEN_TIMER_HIGH = $4003 ; length - LLLL LTTT length counter load (L), timer high (T)
APU_PULSE2_CONFIG        = $4004 ; config - DDLC VVVV duty (D), envelope loop / length counter halt (L), constant volume (C), volume/envelope (V)
APU_PULSE2_SWEEP         = $4005 ; config - DDLC VVVV duty (D), envelope loop / length counter halt (L), constant volume (C), volume/envelope (V)
APU_TRIANGLE_CONFIG      = $4008 ; config - CRRR RRRR length counter halt / linear counter control (C), linear counter load (R)
APU_NOISE_CONFIG         = $400c ; config - --LC VVVV envelope loop / length counter halt (L), constant volume (C), volume/envelope (V)
APU_DMC                  = $4010 ; APU delta modulation channel
APU_DMC_COUNTER          = $4011 ; APU delta modulation channel load counter
APU_DMC_SAMPLE_ADDR      = $4012 ; APU delta modulation channel sample address (location of sample)
APU_DMC_SAMPLE_LEN       = $4013 ; APU delta modulation channel sample length
OAMDMA                   = $4014
APU_STATUS               = $4015 ; ---D NT21 - enable DMC (D), noise (N), triangle (T), and pulse channels (2/1)
APU_FRAME_COUNT          = $4017

; controller input addresses
CONTROLLER_1 = $4016
CONTROLLER_2 = $4017

; palette color names
COLOR_DARK_GRAY_00          = $00
COLOR_DARK_BLUE_01          = $01
COLOR_DARK_VIOLET_02        = $02
COLOR_DARK_PURPLE_03        = $03
COLOR_DARK_MAGENTA_04       = $04
COLOR_DARK_PINK_05          = $05
COLOR_DARK_RED_06           = $06
COLOR_DARK_ORANGE_07        = $07
COLOR_DARK_OLIVE_08         = $08
COLOR_DARK_FOREST_GREEN_09  = $09
COLOR_DARK_GREEN_0a         = $0a
COLOR_DARK_BLUE_GREEN_0b    = $0b
COLOR_DARK_TEAL_0c          = $0c
COLOR_BLACK_0f              = $0f
COLOR_LT_GRAY_10            = $10
COLOR_MED_BLUE_11           = $11
COLOR_MED_VIOLET_12         = $12
COLOR_MED_PURPLE_13         = $13
COLOR_MED_MAGENTA_14        = $14
COLOR_MED_PINK_15           = $15 ; not used
COLOR_MED_RED_16            = $16
COLOR_MED_ORANGE_17         = $17
COLOR_MED_OLIVE_18          = $18
COLOR_MED_FOREST_GREEN_19   = $19
COLOR_MED_GREEN_1a          = $1a
COLOR_MED_BLUE_GREEN_1b     = $1b
COLOR_MED_TEAL_1c           = $1c
COLOR_BLACK_1d              = $1d ; not used
COLOR_MED_1e                = $1e ; not used
COLOR_BLACK_1f              = $1f ; not used
COLOR_WHITE_20              = $20
COLOR_LT_BLUE_21            = $21 ; not used
COLOR_LT_VIOLET_22          = $22
COLOR_LT_PURPLE_23          = $23
COLOR_LT_MAGENTA_24         = $24
COLOR_LT_PINK_25            = $25 ; not used
COLOR_LT_RED_26             = $26
COLOR_LT_ORANGE_27          = $27
COLOR_LT_OLIVE_28           = $28
COLOR_LT_FOREST_GREEN_29    = $29
COLOR_LT_GREEN_2a           = $2a
COLOR_LT_BLUE_GREEN_2b      = $2b ; not used
COLOR_LT_TEAL_2c            = $2c
COLOR_GRAY_2D               = $2d ; not used
COLOR_BLACK_2e              = $2e ; not used
COLOR_BLACK_2f              = $2f ; not used
COLOR_WHITE_30              = $30 ; not used
COLOR_PALE_BLUE_31          = $31 ; not used
COLOR_PALE_VIOLET_32        = $32 ; not used
COLOR_PALE_PURPLE_33        = $33 ; not used
COLOR_PALE_MAGENTA_34       = $34
COLOR_PALE_PINK_35          = $35 ; not used
COLOR_PALE_RED_36           = $36 ; not used
COLOR_PALE_ORANGE_37        = $37
COLOR_PALE_OLIVE_38         = $38 ; not used
COLOR_PALE_FOREST_GREEN_39  = $39 ; not used
COLOR_PALE_GREEN_3a         = $3a ; not used
COLOR_PALE_BLUE_GREEN_3b    = $3b
COLOR_PALE_TEAL_3c          = $3c
COLOR_PALE_GRAY_3d          = $3d ; not used
COLOR_BLACK_3e              = $3e ; not used
COLOR_BLACK_3f              = $3f ; not used