; NES Super C Disassembly - v1.00
; https://github.com/vermiceli/nes-super-c/
; ram.asm contains memory map of the game, specifying how the RAM is used

.segment "ZEROPAGE"

.exportzp ENEMY_CURRENT_SLOT              ; $10
.exportzp GAME_ROUTINE_INDEX              ; $18
.exportzp INTRO_SCREEN_STATE              ; $19
.exportzp FRAME_COUNTER                   ; $1b
.exportzp NMI_CHECK                       ; $1c
.exportzp PPU_READY                       ; $1d
.exportzp GRAPHICS_BUFFER_OFFSET          ; $1e
.exportzp DEMO_MODE                       ; $1f
.exportzp PLAYER_COUNT                    ; $20
.exportzp PLAYER_INDEX                    ; $21
.exportzp PLAYER_MODE                     ; $22
.exportzp RANDOM_NUM                      ; $23
.exportzp OAMDMA_CPU_BUFFER_OFFSET        ; $24
.exportzp BANK_SELECT                     ; $25
.exportzp NT_MIRRORING                    ; $26
.exportzp IRQ_TYPE                        ; $27
.exportzp IRQ_ROUTINE                     ; $28
.exportzp IRQ_ROUTINE_ADDR                ; $29
.exportzp NOOP_IRQ_FLAG                   ; $2b
.exportzp CONT_END_SELECTION              ; $2c
.exportzp IRQ_PTR_TBL                     ; $2d
.exportzp SUPERTILE_ATTRIBUTE_OVERRIDE    ; $2f
.exportzp LEVEL_SCREEN_LAYOUT             ; $30
.exportzp LEVEL_SCREEN_SUPERTILES_PTR     ; $32
.exportzp LEVEL_SUPERTILE_DATA_PTR        ; $34
.exportzp LEVEL_SUPERTILE_PALETTE_DATA    ; $36
.exportzp LEVEL_ROUTINE_INDEX             ; $38
.exportzp PAUSE_STATE                     ; $39
.exportzp UNUSED_PAUSE_FLAG               ; $3a
.exportzp DEMO_LEVEL_END_FLAG             ; $3b
.exportzp DELAY_TIME_LOW_BYTE             ; $3c
.exportzp DELAY_TIME_HIGH_BYTE            ; $3d
.exportzp LEVEL_ROUTINE_VAR               ; $3e
.exportzp LEVEL_ROUTINE_DELAY             ; $3f
.exportzp END_LEVEL_ROUTINE               ; $40
.exportzp DEMO_INPUT_NUM_FRAMES           ; $41
.exportzp DEMO_INPUT_TBL_INDEX            ; $43
.exportzp SCANLINE_IRQ_1                  ; $45
.exportzp SCANLINE_IRQ_2_DIFF             ; $46
.exportzp SCANLINE_IRQ_3_DIFF             ; $47
.exportzp SCANLINE_IRQ_2                  ; $48
.exportzp SCANLINE_IRQ_3                  ; $49
.exportzp IRQ_PPUADDR                     ; $4a
.exportzp DEMO_INPUT_VAL                  ; $4c
.exportzp DEMO_FIRE_DELAY_TIMER           ; $4e
.exportzp CURRENT_LEVEL                   ; $50
.exportzp SOUND_MENU_ROUTINE              ; $50
.exportzp EXTRA_LIVES_CHEAT_NUM_CORRECT   ; $50
.exportzp GAME_COMPLETED                  ; $51
.exportzp ENEMY_DIFFICULTY                ; $52
.exportzp PLAYER_NUM_LIVES                ; $53
.exportzp SOUND_MENU_INDEX                ; $53
.exportzp PLAYER_2_NUM_LIVES              ; $54
.exportzp SOUND_MENU_SCROLL               ; $54
.exportzp NEXT_1_UP_SCORE_MID             ; $55
.exportzp NEXT_1_UP_SCORE_HI              ; $57
.exportzp NUM_CONTINUES                   ; $59
.exportzp SOUND_PLAYING_MEDLEY            ; $59
.exportzp SOUND_MEDLEY_TIMER              ; $5a
.exportzp GLOBAL_TIMER                    ; $5b
.exportzp BOSS_DEFEATED_FLAGS             ; $5c
.exportzp OVERHEAD_FLAG                   ; $5d
.exportzp UNUSED_SOUND_MENU_FLAG          ; $5e
.exportzp X_DRAW_ROUTINE                  ; $60
.exportzp SCREEN_SCROLL_TYPE              ; $61
.exportzp CREDITS_ROUTINE                 ; $61
.exportzp X_SCROLL_SPEED                  ; $62
.exportzp X_SCREEN                        ; $63
.exportzp NT_CURRENT_COLUMN               ; $64
.exportzp DRAW_X_SCREEN                   ; $65
.exportzp PPU_WRITE_ADDR                  ; $66
.exportzp HAS_MARKED_SUPERTILE_BACKUP     ; $68
.exportzp DRAW_BACKUP_BTM_SUPERTILE       ; $69
.exportzp SUPERTILE_DRAW_COUNT            ; $6a
.exportzp X_SCROLL_DRAW_POINT             ; $6b
.exportzp SCREENS_DRAWN                   ; $6c
.exportzp NT_CURRENT_ROW                  ; $6d
.exportzp DRAW_Y_SCREEN                   ; $6e
.exportzp X_AUTOSCROLL                    ; $6f
.exportzp Y_DRAW_ROUTINE                  ; $70
.exportzp CREDITS_ROUTINE_DELAY           ; $70
.exportzp LEVEL_Y_SCROLL_FLAGS            ; $71
.exportzp CREDITS_CURRENT_LINE            ; $71
.exportzp Y_SCROLL_SPEED                  ; $72
.exportzp NT_ROW_SCROLL                   ; $73
.exportzp CREDITS_BTM_CLOUD_SCROLL        ; $73
.exportzp LEVEL_Y_SCREEN                  ; $74
.exportzp CREDITS_MTN_SCROLL              ; $74
.exportzp CREDITS_TOP_CLOUD_SCROLL_DELAY  ; $75
.exportzp CREDITS_BTM_CLOUD_SCROLL_DELAY  ; $76
.exportzp Y_SCROLL_DIR                    ; $77
.exportzp CREDITS_MTN_SCROLL_DELAY        ; $77
.exportzp X_SCROLL_TILE_UPDATE            ; $78
.exportzp Y_LAYOUT_OFFSET                 ; $79
.exportzp BACKUP_X_SCROLL                 ; $7a
.exportzp BACKUP_Y_SCROLL                 ; $7b
.exportzp BACKUP_PPUCTRL_SETTINGS         ; $7c
.exportzp Y_SCROLL_FLAGS                  ; $7d
.exportzp ELEVATOR_ENABLED                ; $7e
.exportzp ELEVATOR_FRACT_VEL              ; $7f
.exportzp ELEVATOR_FAST_VEL               ; $80
.exportzp INTRO_ANIM_INDEX                ; $80
.exportzp ELEVATOR_VEL_ACCUM              ; $81
.exportzp INTRO_ANIM_DELAY                ; $81
.exportzp Y_SCREEN                        ; $82
.exportzp INTRO_SCREEN_PALETTE            ; $82
.exportzp PPUCTRL_TILE_UPDATE             ; $83
.exportzp INTRO_ANIM_X_SCROLL             ; $83
.exportzp Y_AUTOSCROLL                    ; $84
.exportzp STOMP_CEILING_X_SCROLL_CTRL     ; $85
.exportzp INTRO_ANIM_PPUCTRL              ; $85
.exportzp PAUSE_PALETTE_UPDATES           ; $86
.exportzp PLAYER_HUD                      ; $87
.exportzp LEVEL_HEIGHT                    ; $88
.exportzp LEVEL_WIDTH                     ; $89
.exportzp SCREEN_ENEMY_X_CHECK            ; $8a
.exportzp SCREEN_ENEMY_Y_CHECK            ; $8b
.exportzp X_TILE_ROUTINE                  ; $8c
.exportzp Y_TILE_ROUTINE                  ; $8d
.exportzp SCROLLING_UP_FLAG               ; $8e
.exportzp INCLINE_BG_COLLISION_FLAG       ; $8f
.exportzp ENEMY_GEN_COUNT                 ; $90
.exportzp ENEMY_GEN_ROUTINE               ; $91
.exportzp NT_SCREEN_COUNT                 ; $92
.exportzp ENEMY_GEN_DELAY                 ; $93
.exportzp ENEMY_GEN_CTRL                  ; $94
.exportzp END_LEVEL_FADE_DELAY            ; $95
.exportzp ACTIVE_PLAYERS_FLAG             ; $96
.exportzp LEVEL_Y_CENTER                  ; $97
.exportzp Y_AUTOSCROLL_STOP_POS           ; $98
.exportzp ENEMY_GEN_NUM                   ; $99
.exportzp PLAYER_Y_VEL_BG_COLLISION_ADJ   ; $9a
.exportzp AUTO_MOVE_DELAY_TIMER           ; $9b
.exportzp Y_AUTOSCROLL_STOP_SCREEN        ; $9c
.exportzp NUM_TANKS                       ; $9f
.exportzp ELEVATOR_CHECKPOINT             ; $9f
.exportzp PLAYER_STATE                    ; $a0
.exportzp PLAYER_Y_ACCUM                  ; $a2
.exportzp PLAYER_X_ACCUM                  ; $a4
.exportzp PLAYER_Y_FRACT_VELOCITY         ; $a6
.exportzp PLAYER_Y_FAST_VELOCITY          ; $a8
.exportzp PLAYER_X_FRACT_VELOCITY         ; $aa
.exportzp PLAYER_X_FAST_VELOCITY          ; $ac
.exportzp PLAYER_JUMP_STATUS              ; $ae
.exportzp JUMP_INPUT                      ; $b0
.exportzp PLAYER_SURFACE                  ; $b2
.exportzp PLAYER_ANIMATION_FRAME_INDEX    ; $b4
.exportzp PLAYER_ANIM_FRAME_TIMER         ; $b6
.exportzp PLAYER_CURRENT_WEAPON           ; $b8
.exportzp PLAYER_AIM_DIR                  ; $ba
.exportzp PLAYER_RECOIL_TIMER             ; $bc
.exportzp PLAYER_M_WEAPON_FIRE_TIME       ; $be
.exportzp PLAYER_STATE_TIMER              ; $c0
.exportzp PLAYER_SKIP_COLLISION           ; $c2
.exportzp NEW_LIFE_INVINCIBILITY_TIMER    ; $c4
.exportzp PLAYER_ACTION_STATE             ; $c6
.exportzp F_WEAPON_CHARGE                 ; $c8
.exportzp PLAYER_GAME_OVER_STATUS         ; $ca
.exportzp PLAYER_X_POS                    ; $cc
.exportzp PLAYER_Y_POS                    ; $ce
.exportzp PLAYER_OVERHEAD_DIR             ; $d0
.exportzp PLAYER_AUTO_MOVE_CHECKPOINT     ; $d2
.exportzp INVINCIBILITY_TIMER             ; $d4
.exportzp BOSS_DEFEATED_PLAYER_ANIM_SOUND ; $d6
.exportzp PLAYER_APPLY_VEL                ; $d8
.exportzp CUR_SOUND_ADDR                  ; $e0
.exportzp SOUND_VAR_1                     ; $e2
.exportzp SOUND_VAR_2                     ; $e4
.exportzp SOUND_VAR_3                     ; $e5
.exportzp SOUND_PART_ADDR                 ; $e6
.exportzp SOUND_CODE_ADDR                 ; $e8
.exportzp NUM_SOUND_PARTS                 ; $ea
.exportzp CUR_SOUND_PERIOD_LOW            ; $ec
.exportzp CUR_SOUND_LEN_TIMER_HIGH        ; $ed
.exportzp DEMO_LEVEL                      ; $f0
.exportzp CONTROLLER_STATE_DIFF           ; $f1
.exportzp CONTROLLER_STATE                ; $f3
.exportzp CONTROLLER_STATE_DIFF_B         ; $f5
.exportzp CTRL_KNOWN_GOOD                 ; $f7
.exportzp IRQ_Y_SCROLL                    ; $f9
.exportzp IRQ_X_SCROLL                    ; $fa
.exportzp IRQ_PPUCTRL_SETTINGS            ; $fb
.exportzp Y_SCROLL                        ; $fc
.exportzp X_SCROLL                        ; $fd
.exportzp PPUMASK_SETTINGS                ; $fe
.exportzp PPUCTRL_SETTINGS                ; $ff

; first #$10 bytes are used for various use cases
.res 16

; $10 - specifies the current enemy slot index that is being executed
ENEMY_CURRENT_SLOT:
    .res 1

; used for various use cases
.res 7

; $18 - which part of the game routine to execute
; see game_routine_pointer_table
;  * #$00 - intro screen
;  * #$01 - demo
;  * #$02 - player mode selected, initialize level 1, advance game routine
;  * #$03 - set level_routine_00, advance routine
;  * #$04 - play the level
;  * #$05 - hidden sound menu
GAME_ROUTINE_INDEX:
    .res 1

; $19 - intro screen state
;  * #$01 - scrolling or in demo
;  * #$02 - showing player select
;  * #$03 - made intro player selection
INTRO_SCREEN_STATE:
    .res 1

; $1a - !(UNUSED)
.res 1

; $1b - the frame counter loops from #$00 to #$ff increments once per frame
; identical to GLOBAL_TIMER, except FRAME_COUNTER continues to count when the
; game is paused, whereas GLOBAL_TIMER does not
FRAME_COUNTER:
    .res 1

; $1c - track if nmi occurred during game loop
; set to #$01 at start of nmi and #$00 at end of game logic
; if during next nmi this value is still #$01, then the game knows there was
; lag and the previous frame didn't execute fully
; bit 7 is used as a check if nmi was interrupted when setting sound variables
NMI_CHECK:
    .res 1

; $1d - #$00 when at least 5 executions of nmi_start have happened since last
; configure_PPU call
PPU_READY:
    .res 1

; $1e - current write offset into CPU_GRAPHICS_BUFFER
GRAPHICS_BUFFER_OFFSET:
    .res 1

; $1f - whether or not in demo
;  * #$00 = not in demo
;  * #$01 = in demo
DEMO_MODE:
    .res 1

; $20 - 1 less than number of players playing, see PLAYER_MODE
;  * #$00 - 1 player game
;  * #$01 - 2 player game
PLAYER_COUNT:
    .res 1

; $21 - player index, used when calculating player score, foreground collisions
PLAYER_INDEX:
    .res 1

; $22 - 1 less than number of players playing, see PLAYER_COUNT
;  * #$00 - 1 player game
;  * #$01 - 2 player game
PLAYER_MODE:
    .res 1

; $23 - random number, randomized in forever_loop
RANDOM_NUM:
    .res 1

; $24 - OAMDMA_CPU_BUFFER write offset
OAMDMA_CPU_BUFFER_OFFSET:
    .res 1

; $25 - sometimes the last MMC3 bank select command is saved
; it is used after a scanline interrupt to set the command with the CPU
; I don't think this is necessary, as all bank data writes are preceded by a
; bank select command
BANK_SELECT:
    .res 1

; $26 - nametable mirroring
; * #$00 - vertical mirroring
;  A B
;  A B
; * #$01 - horizontal mirroring
;  A A
;  B B
NT_MIRRORING:
    .res 1

; $27 - which set of routines to run during a scanline interrupt
; index into irq_handler_ptr_tbl
IRQ_TYPE:
    .res 1

; $28 - specific irq handler routine offset
; index into irq_handler_xx_ptr_tbl
IRQ_ROUTINE:
    .res 1

; $29 - two byte address to the specific interrupt handler (irq_handler_xx_yy)
IRQ_ROUTINE_ADDR:
    .res 2

; $2b - whether or not to set the IRQ to the noop IRQ routine during NMI
NOOP_IRQ_FLAG:
    .res 1

; $2c - on the game over screen, whether "CONTINUE" or "END" is selected
;  * #$00 - "CONTINUE" is selected
;  * #$01 - "END" is selected
CONT_END_SELECTION:
    .res 1

; $2d - the pointer table containing routines used during an scanline interrupt
; points to specific irq_handler_xx_ptr_tbl table
IRQ_PTR_TBL:
    .res 2

; $2f - whether or not to enable override of supertile palette colors
; enabled when door (enemy type #$0d) has been destroyed. This enemy determines
; whether or not overwrite_attribute_byte is called as part of palette loading.
; door appears on level 2 and level 6, but this only affects level 2
SUPERTILE_ATTRIBUTE_OVERRIDE:
    .res 1

; $30 - pointer to level-specific level_x_screen_layout_tbl
; that table stores the structure of the level in terms of how the nametable
; screens are laid out horizontally and vertically.  Then after the width and
; height bytes, each entry represents a screen and is a pointer to
; level_x_supertiles_screen_xx, which specifies the supretiles for the screen
LEVEL_SCREEN_LAYOUT:
    .res 2

; $32 - 2-byte address containing which supertiles to use for each screen of the
; level (level_x_supertiles_screen_ptr_table)
LEVEL_SCREEN_SUPERTILES_PTR:
    .res 2

; $34 - 2-byte pointer to supertile data, which defines pattern table tiles of
; the supertiles that are used to make level blocks
; see level_x_supertile_data
LEVEL_SUPERTILE_DATA_PTR:
    .res 2

; $36 - 2-byte pointer address to the palettes used for each supertile, each
; byte describes the 4 palettes for a single supertile
; see level_x_palette_data
LEVEL_SUPERTILE_PALETTE_DATA:
    .res 2

; $38 - the index into level_routine_ptr_tbl of the routine to run
LEVEL_ROUTINE_INDEX:
    .res 1

; $39 - whether or not the game is paused
;  * #$00 - not paused
;  * #$01 - paused
PAUSE_STATE:
    .res 1

; $3a - !(UNUSED) only ever read, never written to
UNUSED_PAUSE_FLAG:
    .res 1

; $3b - whether or not demo (attract mode) for the level is complete and new
; demo level should play
DEMO_LEVEL_END_FLAG:
    .res 1

; $3c - the high byte of the delay
DELAY_TIME_LOW_BYTE:
    .res 1

; $3d - the high byte of the delay
DELAY_TIME_HIGH_BYTE:
    .res 1

; $3e - a generic variable for use within a level routine, e.g. level_routine_02
; or level_routine_04 for controller end level routine
LEVEL_ROUTINE_VAR:
    .res 1

; $3f - a delay used in some level routines
; namely level_routine_01 level_routine_04, and level_routine_06
LEVEL_ROUTINE_DELAY:
    .res 1

; $40 - end of level routine index.  Used for end of level logic
END_LEVEL_ROUTINE:
    .res 1

; $41 - determine how many even-numbered frames to continue pressing the button
; specified in DEMO_INPUT_VAL for demo
DEMO_INPUT_NUM_FRAMES:
    .res 2

; $43 - when in demo, stores the offset into specific demo_input_lvl_xx_px table
DEMO_INPUT_TBL_INDEX:
    .res 2

; $45 - the scanline where the 1st irq interrupt triggers
; scanline starts at 0 at the top and goes to #$ff at the bottom
; #$ff means no scanline irq
SCANLINE_IRQ_1:
    .res 1

; $46 - the number of scanlines after SCANLINE_IRQ_1 to run a 2nd scanline irq
; #$ff means no 2nd scanline irq
SCANLINE_IRQ_2_DIFF:
    .res 1

; $47 - the number of scanlines after SCANLINE_IRQ_2 to run a 3rd scanline irq
; #$ff means no 3rd scanline irq
SCANLINE_IRQ_3_DIFF:
    .res 1

; $48 - the actual scanline number where the 2nd irq occurs
; #$ff means no 2nd scanline irq
SCANLINE_IRQ_2:
    .res 1

; $49 - the actual scanline number where the 3rd irq occurs
; #$ff means no 3rd scanline irq
SCANLINE_IRQ_3:
    .res 1

; $4a - set to indicate a PPU address to set during a scanline interrupt.
; used only by some scanline interrupt handlers
; value copied into IRQ_HANDLER_PPUADDR for use by the interrupt handler
IRQ_PPUADDR:
    .res 2

; $4c - the controller input pressed during a demo. One byte for each player
DEMO_INPUT_VAL:
    .res 2

; $4e - the number of frames since beginning of demo for level
; used to delay #$e0 frames before firing
DEMO_FIRE_DELAY_TIMER:
    .res 1

; $4f - !(UNUSED)
.res 1

; $50 - the current level #$00-#$07
; set to #$08 after defeating final boss, then reset to #$00 before credits
CURRENT_LEVEL:

; $50 - the current routine used when initializing the sound menu UI
SOUND_MENU_ROUTINE:

; $50 - 10 extra lives cheat check
EXTRA_LIVES_CHEAT_NUM_CORRECT:
    .res 1

; $51 - whether or not the game has previously been completed
;  * 0 - not previously completed
;  * 1 - previously completed
GAME_COMPLETED:
    .res 1

; $52 - a value to adjust game difficulty.  Values are #$00, #$01, or #$02
; based on number of players who have the S weapon
;  * adjusts ENEMY_HP
;  * adjusts how quickly enemies are generated by subtracting
;    #$04 * ENEMY_DIFFICULTY from the initial generation delay
ENEMY_DIFFICULTY:
    .res 1

; $53 - player number of lives, #$00 is last life
;  * on game over stays #$00, but P1_GAME_OVER_STATUS becomes #$01
;  * maximum number of lives that can be properly displayed in level screen
;    ("REST") is #$63 (99 in decimal)
; can be thought of as the number of medals shown in the player HUD
PLAYER_NUM_LIVES:

; $53 - in the sound menu, used to keep track of selected sound to play
SOUND_MENU_INDEX:
    .res 1

; $54 - player 2 number of lives, #$00 is last life
;  * on game over stays #$00, but P1_GAME_OVER_STATUS becomes #$01
PLAYER_2_NUM_LIVES:

; $54 - sound menu scroll
SOUND_MENU_SCROLL:
    .res 1

; $55 - binary-coded decimal value of the 10,000s and 1,000s place of the score
; to achieve to get an extra life
; e.g. 42 would be xx42xx0
; $56 is for player 2
NEXT_1_UP_SCORE_MID:
    .res 2

; $57 - binary-coded decimal value of the 1,000,000s and 100,000s place of the
; score to achieve to get an extra life
; e.g. 42 would be 42xxxx0
; $58 is for player 2
NEXT_1_UP_SCORE_HI:
    .res 2

; $59 - number of continues available until game restarts from beginning
NUM_CONTINUES:

; $59 - flag that the MEDOLEY (medley) is playing from the sound menu
SOUND_PLAYING_MEDLEY:
    .res 1

; $5a - duration timer for playing sound medley
SOUND_MEDLEY_TIMER:
    .res 1

; $5b - loops from #$00 to #$ff increments once per frame
; identical to FRAME_COUNTER, except GLOBAL_TIMER stops counting when the game
; is paused
GLOBAL_TIMER:
    .res 1

; $5c - 2 bits of byte are used as boss is defeated
;  * bit 7 is set when level boss is removed
;  * bit 6 is set when playing end of level tune (sound_33/sound_34)
;  * bit 0 is set when boss is defeated
BOSS_DEFEATED_FLAGS:
    .res 1

; $5d - whether or not the level is an overhead level (Level 2 and Level 6)
;  * #$00 side view
;  * #$01 overhead view
OVERHEAD_FLAG:
    .res 1

; $5e - !(UNUSED)
UNUSED_SOUND_MENU_FLAG:
    .res 2

; $60 - run_draw_routines routine index
;  * #$00 - do nothing
;  * #$01 - advance to #$02
;  * #$02 - start new column of supertiles
;  * #$03 - draw more supertiles to current column
X_DRAW_ROUTINE:
    .res 1

; $61 - screen scroll type, e.g. level 1 is horizontal scroll
; * #$00 - horizontal (level 1, 3, 4, and 8)
; * #$01 - vertical or overhead (level 2, 5, 6, 7)
; * #$01 - various boss screens (helicopter, robot spider, fortress wall,
;   crypto-crustacean boss)
; * #$02 - final level mid level change
; * #$02 - tank boss, chandelier boss, jagger froid, final boss
SCREEN_SCROLL_TYPE:

; during the end game credits, used to control which logic for scrolling credits
; (see scroll_credits_routine_ptr_tbl)
CREDITS_ROUTINE:
    .res 1

; $62 - how many pixels to scroll the screen this frame (horizontally)
; usually based on player velocity, but is set during auto-scroll moments
; note that this is not the scroll distance within the screen
X_SCROLL_SPEED:
    .res 1

; $63 - number of horizontal nametable screens scrolled
; starts at 0
X_SCREEN:
    .res 1

; $64 - represents the current column offset into the current nametable when
; writing nametable data to the graphics buffer
; increments up to #$1e (maybe #$20)
NT_CURRENT_COLUMN:
    .res 1

; $65 - the screen number of the current level currently drawing (how many
; screens into the level). Starts at 1
DRAW_X_SCREEN:
    .res 1

; $66 - 2-byte PPU address used when drawing supertiles based on scroll
PPU_WRITE_ADDR:
    .res 2

; $68 - used when drawing supertiles based on scroll
HAS_MARKED_SUPERTILE_BACKUP:
    .res 1

; $69 - used when drawing supertiles based on scroll
; when both this and SUPERTILE_DRAW_COUNT are non-zero, SUPERTILE_PALETTE_MERGE
; has high nibble set
; laser chandelier
DRAW_BACKUP_BTM_SUPERTILE:
    .res 1

; $6a - the number of supertiles remaining to draw, starts at #$07 an decrements
; down to #$00. This is one entire column of the nametable.
; unused and hard-coded to #$ff for level 2, but level 4 uses it
SUPERTILE_DRAW_COUNT:
    .res 1

; $6b - when X_SCROLL is greater than this number, the next column of supertiles
; is drawn
X_SCROLL_DRAW_POINT:
    .res 1

; $6c - number of screens drawn for level (0 indexed)
SCREENS_DRAWN:
    .res 1

; $6d - supertile base offset to load
; increments up to #$40
; set when scrolling horizontally
NT_CURRENT_ROW:
    .res 1

; $6e - set from LEVEL_Y_SCREEN
DRAW_Y_SCREEN:
    .res 1

; $6f - horizontal auto-scroll mode.  There are 3 modes
;  * #$00 (mode 0): no auto-scroll (disabled)
;  * #$01 (mode 1): scroll horizontally to the right until PPU is scrolled to
;    show the next nametable, which means revealed entire screen to right, e.g.
;    level 4 when revealing the base of elevator
;  * #$02 (mode 2): scroll horizontally while any player on rightmost 37.5% of
;    screen (side-levels only). For example, this is used in level 1, 4, and 5
;    for the ending animation
X_AUTOSCROLL:
    .res 1

; $70 - y_scroll_draw_routine routine index
Y_DRAW_ROUTINE:

; $70 - for ending credits, used to delay before advancing to the next
; CREDITS_ROUTINE
CREDITS_ROUTINE_DELAY:
    .res 1

; $71 - initial value for level (see level_y_scroll_flags_tbl)
; can be changed like after falling from helicopter in level 1
; bit 7
;  * 0 = can scroll up if ELEVATOR_ENABLED is disabled (0)
;  * 1 = cannot scroll up
; bit 6
;  * 0 = can scroll down if ELEVATOR_ENABLED is disabled (0)
;  * 1 = cannot scroll down
LEVEL_Y_SCROLL_FLAGS:

; $71 - when showing end credits, used to keep track of next credit line to draw
CREDITS_CURRENT_LINE:
    .res 1

; $72 - how many pixels to scroll the screen this frame (vertically)
; this frame based on player velocity. Can be negative
; note that this is not the scroll distance within the screen
; negative values scroll up, positive values scroll down
Y_SCROLL_SPEED:
    .res 1

; $73 - the nametable row number of the top of the visible screen
; used for drawing, compare to Y_SCROLL, which stores PPU vertical scroll
; When wraps LEVEL_Y_SCREEN is updated
; also, horizontal scroll of bottom layer of clouds during end credits
; on level 2:
;  * starts at #$00, after first scroll initialized to #$1c
;    decrements by 2 as scroll up
NT_ROW_SCROLL:

; $73 - scroll value used during credits for the bottom of the cloud
CREDITS_BTM_CLOUD_SCROLL:
    .res 1

; $74 - vertical screen index, similar to y axis on a 2-d cardinal plane
; similar to Y_SCREEN
LEVEL_Y_SCREEN:

; $74 - scroll value used during credits for the mountain
CREDITS_MTN_SCROLL:
    .res 1

; $75 - used to time horizontal scroll of the cloud tops in the end credits
CREDITS_TOP_CLOUD_SCROLL_DELAY:
    .res 1

; $76 - scroll value used to delay scroll of the bottom of the clouds
CREDITS_BTM_CLOUD_SCROLL_DELAY:
    .res 1

; $77 - 0 when vertically scrolling up, 1 when vertically scrolling down
Y_SCROLL_DIR:

; $77 - used to time horizontal scroll of the mountains in the end credits
CREDITS_MTN_SCROLL_DELAY:
    .res 1

; $78 - used when drawing supertiles based on scroll
X_SCROLL_TILE_UPDATE:
    .res 1

; $79 - index into level_x_screen_layout_tbl for vertical draw routines
Y_LAYOUT_OFFSET:
    .res 1

; $7a - a backup of the X_SCROLL, used by the stomping ceiling
BACKUP_X_SCROLL:
    .res 1

; $7b - a backup of the Y_SCROLL, used by the stomping ceiling
BACKUP_Y_SCROLL:
    .res 1

; $7c - a backup of the PPUCTRL_SETTINGS, used by the stomping ceiling
BACKUP_PPUCTRL_SETTINGS:
    .res 1

; $7d - related to LEVEL_Y_SCROLL_FLAGS
; see level_y_scroll_flags_tbl
Y_SCROLL_FLAGS:
    .res 1

; $7e - whether or not the elevator autoscroll is enabled
; bit 7 - 1 = enabled, 0 = disabled/stopped
; !(OBS) it seems there may have been logic for bit 6 and bit 0
; bit 0 would have enabled a variable speed depending on p1 and p2 game over and
; jump status, when enabled, the elevator goes way too fast
; bit 6 seems to be kept when doing logic, but never read nor set
ELEVATOR_ENABLED:
    .res 1

; $7f - elevator scroll fractional Y speed. Scrolls Y when elevator enabled
; combined with ELEVATOR_FAST_VEL, used to set the elevator speed to -0.25
ELEVATOR_FRACT_VEL:
    .res 1

; $80 - elevator scroll fast Y speed. Scrolls Y when elevator enabled
; combined with ELEVATOR_FRACT_VEL, used to set the elevator speed to -0.25
ELEVATOR_FAST_VEL:

; $80 - for intro, index into intro_animation_ptr_tbl
INTRO_ANIM_INDEX:
    .res 1

; $81 - elevator fractional velocity accumulator
INTRO_ANIM_DELAY:

; $81 - intro animation delay timer
ELEVATOR_VEL_ACCUM:
    .res 1

; $82 - vertical screen index, similar to y axis on a 2-d cardinal plane
; changes as the nametable scroll wraps vertically
;  * level 1 - decrements from #$02 to #$00 as player climbs the hills
;  * level 2 - starts at #$0c and decrements down as player traverses up
;  * level 3 - stays at #$00 the entire time
;  * level 4 - starts at #$0c and decrements down as player climbs up
;  * level 5 - starts at #$06 decrements to #$00 as you climb the floating
;    platforms then increments from #$00 to #$02
;  * level 6 starts at #$0c and decrements down (#$01 for final screen)
;  * level 7 starts at #$0c and increments as player goes down
;  * level 8 starts at #$03, decrements to #$00 as you move through level
;    upward, increments back to #$02 as you go downward similar to
;    LEVEL_Y_SCREEN
Y_SCREEN:

; $82 - intro screen intro_palette_tbl offset for animation reveal of logo
INTRO_SCREEN_PALETTE:
    .res 1

; $83 - stores PPUCTRL_SETTINGS for vertical scroll (y_scroll_draw_routine_01)
PPUCTRL_TILE_UPDATE:

; $83 - intro animation scroll
INTRO_ANIM_X_SCROLL:
    .res 1

; $84 - vertical auto-scroll mode. Used in conjunction with
; Y_AUTOSCROLL_STOP_SCREEN andY_AUTOSCROLL_STOP_POS to auto-scroll to a position
; on a specific screen.
;  * #$00 (mode 0): vertical auto-scroll (up or down) until checkpoint
;  * #$01 (mode 1): vertically auto-scroll up to checkpoint while player(s) in
;    top 37.5% of screen. As used in the game, this is only for level 2 overhead
;    ending animation.
Y_AUTOSCROLL:
    .res 1

; $85 - intro animation PPUCTRL for use in flashing SUPER while scrolling
INTRO_ANIM_PPUCTRL:

; $85 - used to correctly animate the stomping ceiling on level 8
; prevents pre-irq X scroll and nametable change to show correct bg tiles
;  * #$00 - allow X scroll and allow moving to next horizontal nametable
;  * #$01 - prevent scrolling into next nametable
;  * #$80 - prevent X scroll
STOMP_CEILING_X_SCROLL_CTRL:
    .res 1

; $86 - controls whether or not to write the palette to the graphics buffer, by
; allowing/preventing copying palette bytes from PALETTE_CPU_BUFFER to
; CPU_GRAPHICS_BUFFER
;  * 0 - enable
;  * 1 - disable
PAUSE_PALETTE_UPDATES:
    .res 1

; $87 - controls whether do display HUD
;  * 0 - no hud
;  * 1 - display hud
PLAYER_HUD:
    .res 1

; $88 - the number of screens high the level has
; all levels are defined as a rectangle of screens LEVEL_WIDTH * LEVEL_HEIGHT
LEVEL_HEIGHT:
    .res 1

; $89 - the number of horizontal screens the level has
; all levels are defined as a rectangle of screens LEVEL_WIDTH * LEVEL_HEIGHT
LEVEL_WIDTH:
    .res 1

; $8a - used to determine when to generate level enemies based on X_SCROLL
SCREEN_ENEMY_X_CHECK:
    .res 1

; $8b - used to determine when to generate level enemies based on Y_SCROLL
SCREEN_ENEMY_Y_CHECK:
    .res 1

; $8c - index into level-specific tile routine based on horizontal location
; within level
; used to control when updating tiles banks and palettes as well as any
; auto-scroll
X_TILE_ROUTINE:
    .res 1

; $8d - index into level-specific tile routine based on vertical location within
; level
; used to control when updating tiles banks and palettes as well as any
; auto-scroll
Y_TILE_ROUTINE:
    .res 1

; $8e - used when calculating if an enemy is off screen vertically
;  * #$ff - scrolling up
;  * #$00 - not scrolling (or scrolling down)
SCROLLING_UP_FLAG:
    .res 1

; $8f - whether or not a background collision with an incline is counted as a
; collision. Set to #$00 on Level 5 (The Cliff) and Level 8 (The Final Stage).
; When disabled, bullets pass through inclines and players can jump up through
; inclines.
;  * #$00 - skip bullet background collision check for inclines
;  * #$01 - test bullet background collisions with inclines, e.g. bullets stop
;    when collide with incline
INCLINE_BG_COLLISION_FLAG:
    .res 1

; $90 - number of randomly generated enemies for the current screen
; if the player doesn't move to the next screen for a while, eventually the game
; will start generating enemies even when the player is close to the edge.
; used to make it harder for a player to stay in one screen and rack up points
ENEMY_GEN_COUNT:
    .res 1

; $91 - used as part of generating a random soldier or ladybug
; enemy_gen_routine_ptr_tbl offset
ENEMY_GEN_ROUTINE:
    .res 1

; $92 - amount of horizontal or vertical nametable screens scrolled
; X_SCREEN or Y_SCREEN depending on level
NT_SCREEN_COUNT:
    .res 1

; $93 - delay timer for generating random soldiers or ladybug enemies
ENEMY_GEN_DELAY:
    .res 1

; $94 - controls various aspects of random enemy generation
; see level_enemy_gen_ctrl_tbl
; bit 7: 1 = overhead level, 0 = not overhead level
; bit 6: 1 = pause random enemy generation
; bit 1: 0 = create random overhead soldiers, 1 = create random alien ladybugs
; bit 0: 1 = vertical scroll level (levels 2, 5, 6, and 7)
;   * vertical scrolling influences enemy generation timer on odd frames
;   * when 1, control byte is controlled by X_SCREEN
;   * when 0, control byte is controlled by Y_SCREEN
ENEMY_GEN_CTRL:
    .res 1

; $95 - the number of frames between subsequent fading of end of level when
; fading to black
END_LEVEL_FADE_DELAY:
    .res 1

; $96 - player active/game over statuses in a single byte
;  * #$00 - player 1 not in game over, player 2 in game over (or 1 player game)
;  * #$01 - player 1 in game over, player 2 not in game over
;  * #$02 - 2 player game and neither player in game over
;  * #$ff - both players in game over
ACTIVE_PLAYERS_FLAG:
    .res 1

; $97 - y point at level in which to cause scroll
LEVEL_Y_CENTER:
    .res 1

; $98 - specifies vertical position on screen to stop any vertical auto-scroll
; also specifies vertical scroll direction in vertical auto-scroll mode 0
; compared against Y_SCROLL
; used by krytpo-crustacean and laser chandelier
;  * bits 1-7: Y_SCROLL to compare against
;  * bit 0: 0 = up, 1 = down
Y_AUTOSCROLL_STOP_POS:
    .res 1

; $99 - used when creating multiple random enemies to set the number of enemies
; to generate in quick succession
ENEMY_GEN_NUM:
    .res 1

; $9a - used to adjust Y velocity with player collides with the stomping ceiling
PLAYER_Y_VEL_BG_COLLISION_ADJ:
    .res 1

; $9b - timer used to delay auto-move at the end of the level
AUTO_MOVE_DELAY_TIMER:
    .res 1

; $9c - when Y auto-scroll enabled, can be used to specify which screen of the
; level to stop on.  Compared against Y_SCREEN when non-negative
;  * when positive, the screen where the stop checkpoint exist
;  * when negative, always scroll in the determined direction until current
;    screen's stop position
Y_AUTOSCROLL_STOP_SCREEN:
    .res 1

; $9d - !(UNUSED)
.res 2

; $9f - level 2 - the number of tanks on screen. Used to prevent scroll until
; tank(s) is/are destroyed
NUM_TANKS:

; $9f - level 4 - used to time changing pattern table tiles for elevator so that
; the rack-mounted turret (enemy type = #$25) is hidden 'behind' elevator set
; near start of second elevator moving
ELEVATOR_CHECKPOINT:
    .res 1

; $a0 - player's state (see player_state_routine_ptr_tbl)
; $a1 is for p2, if p2 not playing, set to #$00
;  * #$00
;  * #$01 - used for brief time between state #$05 and #$02
;  * #$02 - normal alive state
;  * #$03 - hit by enemy
;  * #$04 - falling back after hit
;  * #$05 - dead on ground
;  * #$06 - level 1 helicopter delay drop animation
;  * #$07 - level 1 dropping into level
;  * #$08 - end of level auto movement complete
PLAYER_STATE:
    .res 2

; $a2 - variable to keep track of fractional velocity. PLAYER_Y_FRACT_VELOCITY
; is added to PLAYER_Y_ACCUM every frame.  Whenever PLAYER_Y_ACCUM wraps past
; #$ff, the carry flag is set which then adds 1 additional y unit when adding
; PLAYER_Y_FRACT_VELOCITY to the player position.
; this value isn't cleared between jumps
; another term for this variable is PLAYER_JUMP_COEFFICIENT
; $a3 - player 2
PLAYER_Y_ACCUM:
    .res 2

; $a4 - variable to keep track of fractional velocity. PLAYER_X_FRACT_VELOCITY
; is added to PLAYER_X_ACCUM every frame.  Whenever PLAYER_X_ACCUM wraps past
; 255, the carry flag is set which then adds 1 additional x unit when adding
; PLAYER_X_FRACT_VELOCITY to the player position.
; $a5 - player 2
PLAYER_X_ACCUM:
    .res 2

; $a6 - player attribute
PLAYER_Y_FRACT_VELOCITY:
    .res 2

; $a8 - player Y velocity integer portion
; positive goes down, negative goes up
PLAYER_Y_FAST_VELOCITY:
.res 2

; $aa - player's fractional X velocity
; $ab - player 2
PLAYER_X_FRACT_VELOCITY:
    .res 2

; $ac - player X velocity integer portion
; positive goes right, negative goes left
; $ad - player 2
PLAYER_X_FAST_VELOCITY:
    .res 2

; $ae - player's jump status
;  * #$00 = not jumping
;  * #$40 = fall off edge
;  * #$80 = player jump, or player hit and falling back
; $af - player 2
PLAYER_JUMP_STATUS:
    .res 2

; $b0 - stores player input while jumping
; read for left and right d-pad data for determining X velocity
; similarly, also used when falling off ledge to keep track of controller input
; when walked off ledge
; !(OBS) not sure why CONTROLLER_STATE wasn't used
; $b1 - player 2
JUMP_INPUT:
    .res 2

; $b2 - what kind of surface the player is on (not used in overhead levels)
;  * #$01 - player on floating platform that can be dropped down from
;  * #$02 - player on land / solid wall collision
;  * #$03 - collapsible ground on level 7 (eggshells)
;  * #$04 - player in water
;  * #$05
;  * #$06 - player on positive incline (start)
;  * #$07 - player on positive incline (second)
;  * #$08 - player on positive incline (most of incline)
;  * #$09 - negative incline (second)
;  * #$0a - negative incline (most of the time)
;  * #$0b - negative incline (start)
; $b3 - player 2
PLAYER_SURFACE:
    .res 2

; $b4 - which frame of the player animation. Depends on player state.
; For example, if player is running on side-view levels, this cycles from
; #$00 to #$05 (inclusively)
; on overhead levels, this cycles from #$00 to #$03 (inclusively)
; $b5 - player 2
PLAYER_ANIMATION_FRAME_INDEX:
    .res 2

; $b6 - value that is incremented every frame when player is walking,
; used to wait time animations before incrementing PLAYER_ANIMATION_FRAME_INDEX
; for animating player
; $b7 - player 2
PLAYER_ANIM_FRAME_TIMER:
    .res 2

; $b8 - what weapon player has
;  * #$00 - Regular
;  * #$01 - Machine
;  * #$02 - Spray
;  * #$03 - Laser
;  * #$04 - Flame
; bit 7 is set for rapid fire (R weapon item)
; $b9 - player 2
PLAYER_CURRENT_WEAPON:
    .res 2

; $ba - which direction the player is aiming
;  * #$00 up facing right
;  * #$01 up-right
;  * #$02 right
;  * #$03 right-down
;  * #$05 crouching facing right
;  * #$06 jumping aiming down
;  * etc.
; there are #$02 up and #$02 down values depending on facing direction
; see player_aim_dir_tbl for reference
; $bb - player 2
PLAYER_AIM_DIR:
    .res 2

; $bc - how many frames to be pushed back/down from recoil
; #$05 frames when stationary, #$11 when walking or in water
; $bd - player 2
PLAYER_RECOIL_TIMER:
    .res 2

; $be - timer for delay between machine gun firing when holding down b
; 6 frames between bullets
; $bf - player 2
PLAYER_M_WEAPON_FIRE_TIME:
    .res 2

; $c0 - a timer used within a player state
;  * in player_state_routine_06 used to delay animation before player drops from
;    helicopter in level 1
;  * in player_state_routine_07 used to time velocity adjustments when dropping
;    from helicopter in level 1
;  * set to 1 when player dies, then to #$60 as a countdown for how long to lay
;    on ground before re-spawning
; $c1 - player 2
PLAYER_STATE_TIMER:
    .res 2

; $c2 - whether or not to skip testing player sprite collision
;  * #$01 - player in water aiming down, or after player death and temporarily
;    invincible while lying on ground
;  * #$00 - otherwise
; $c3 - player 2
PLAYER_SKIP_COLLISION:
    .res 2

; $c4 - timer for invincibility after dying
; while the player is invincible, he does not kill enemies when colliding
; (unlike INVINCIBILITY_TIMER)
; $c5 - player 2
NEW_LIFE_INVINCIBILITY_TIMER:
    .res 2

; $c6 - player action state
;  * #$00 = p2 when not active
;  * #$1f = jumping
;  * #$3f = crouching
;  * #$5f = crouching on incline
;  * #$7f = in water
;  * #$bf = overhead level
;  * #$ff = normal
; $c7 - player 2
PLAYER_ACTION_STATE:
    .res 2

; $c8 - times how long b button is pressed for F weapon, charges up to #$20
; $c9 - player 2
F_WEAPON_CHARGE:
    .res 2

; $ca - game over status
; when in 1 player, p2 will be in game over
;  * #$00 - not game over
;  * #$01 - game over
; $cb - player 2
PLAYER_GAME_OVER_STATUS:
    .res 2

; $cc - calculated player X position
; not used to place player sprite, see PLAYER_SPRITE_X_POS
; copy of PLAYER_SPRITE_X_POS
; $cd - player 2
PLAYER_X_POS:
    .res 2

; $ce - calculated player Y position
; not used to place player sprite, see PLAYER_SPRITE_Y_POS
; $cf - player 2
PLAYER_Y_POS:
    .res 2

; $d0 - player overhead facing direction. #$00 up to #$07 inclusive
;  * #$00 - up
;  * #$01 - up-right
;  * #$02 - right
;  * #$03 - down-right
;  * #$04 - down
;  * #$05 - down-left
;  * #$06 - left
;  * #$07 - up-left
; $d1 - player 2
PLAYER_OVERHEAD_DIR:
    .res 2

; $d2 - end of level auto movement first of 2 checkpoints
;  * #$00 - not reached
;  * #$01 = reached
; $d3 - player 2
PLAYER_AUTO_MOVE_CHECKPOINT:
    .res 2

; $d4 - timer for player invincibility (b (barrier) weapon)
; decreases every 8 frames
; $d5 - player 2
INVINCIBILITY_TIMER:
    .res 2

; $d6 - keeps track of which sound to play for player auto-move animation after
; defeating the level boss
;  * #$00 - no sound
;  * #$07 - sound_07 (footstep)
; $d7 - player 2
BOSS_DEFEATED_PLAYER_ANIM_SOUND:
    .res 2

; $d8 - when calculating X and Y scroll, used to know if should apply scroll or
; apply velocity to player position when player past center of screen.
; set twice per frame (for X and Y logic)
;  * 0 = should scroll when player is past the center of the screen instead of
;    applying velocity.
;  * 1 = 2 player game and player cannot be the cause of scroll.  For X scroll
;    this means the player is on the left of the other player.  The player can
;    be the cause of blocking scroll, e.g. the are too far to the left
;    preventing a right scroll
; $d9 - player 2
PLAYER_APPLY_VEL:
    .res 2

; $db - !(UNUSED)
.res 6

; $e0 - 2-byte sound address used when processing a particular sound slot
; loaded from SOUND_CMD_ADDRS_LOW and SOUND_CMD_ADDRS_HIGH when processing
CUR_SOUND_ADDR:
    .res 2

; $e2 - temporary variable used for sound processing
;  * sometimes single byte sound variable used when processing
;  * sometimes 2-byte address to sound_cmd_routine_xx
;  * sometimes sound code read offset
SOUND_VAR_1:
    .res 2

; $e4 - temporary variable used for sound processing
; * used when bringing down pitch by an octave
SOUND_VAR_2:
    .res 1

; $e5 - temporary variable used for sound processing
;  * used when calculating pulse channel volume decrescendo
SOUND_VAR_3:
    .res 1

; $e6 - 2-byte memory address to sound part, e.g. sound_xx_slot_xx
SOUND_PART_ADDR:
    .res 2

; $e8 - 2-byte memory address to start of sound code, e.g. sound_xx
SOUND_CODE_ADDR:
    .res 2

; $ea - 1 less than the number of sound parts in the sound code, i.e. how many
; slots the sound code has
NUM_SOUND_PARTS:
    .res 1

; $eb - !(UNUSED)
.res 1

; $ec - controls channel period/timer (pitch/frequency)
;  * pulse channel registers $4002 and $4006 - period/timer low bits
;  * triangle channel register $400a - period/timer low bits
;  * noise channel register $400e - noise period (noise mode bit 7 always 0)
CUR_SOUND_PERIOD_LOW:
    .res 1

; $ed - controls high bits of period/timer
;  * pulse channel registers $4003 and $4007 - period/timer 3 high bits
;    length counter load is always 1 (bit 3 set)
;  * triangle channel register $400a - period/timer low bits
;  * noise channel register $400e - noise period (noise mode bit 7 always 0)
CUR_SOUND_LEN_TIMER_HIGH:
    .res 1

; $ee - !(UNUSED)
.res 2

; $f0 - the current level being demonstrated during the demo
DEMO_LEVEL:
    .res 1

; $f1 - stores the difference between the controller input between reads.
; useful for events that should only trigger on first button press
; $f2 - player 2
CONTROLLER_STATE_DIFF:
    .res 2

; $f3 - stores the currently-pressed buttons for the controller input
;  * bit 7 - A
;  * bit 6 - B
;  * bit 5 - select
;  * bit 4 - start
;  * bit 3 - up
;  * bit 2 - down
;  * bit 1 - left
;  * bit 0 - right
; $f4 - player 2
CONTROLLER_STATE:
    .res 2

; $f5 - stores the difference between the controller input between reads.
; same as CONTROLLER_STATE_DIFF, not sure why it's duplicated
; $f6 - player 2
CONTROLLER_STATE_DIFF_B:
    .res 2

; $f7 - used in input-reading code to know the last known valid read of
; controller input (similar to CONTROLLER_STATE)
; $f8 - player 2
CTRL_KNOWN_GOOD:
    .res 2

; $f9 - vertical scroll for use after 1st IRQ, but before 3rd IRQ.
; Supports having different vertical scroll values for different IRQs
; There are 2 vertical scrolls
;  * before 1st IRQ (Y_SCROLL)
;  * after 1st IRQ, but before 3rd IRQ (IRQ_Y_SCROLL)
IRQ_Y_SCROLL:
    .res 1

; $fa - horizontal scroll to use after the 1st IRQ, but before the 3rd IRQ
; used to have different horizontal scroll before IRQ (X_SCROLL) and after IRQ
; (IRQ_X_SCROLL)
IRQ_X_SCROLL:
    .res 1

; $fb - PPUCTRL_SETTINGS for after the 1st IRQ, but before the 3rd IRQ
; used to have different PPU settings before IRQ (PPUCTRL_SETTINGS) and after
; IRQ (IRQ_PPUCTRL_SETTINGS)
; excludes bit 0
IRQ_PPUCTRL_SETTINGS:
    .res 1

; $fc - the number of pixels scrolled vertically on the nametables (vertical
; part of PPUSCROLL)
; #$00-#$ef, where #$00 is top of nametables and #$ef is showing bottom
; nametable (max vertical scroll before wrapping)
; super c doesn't wrap vertically for nametables, instead it sets scroll to #$00
; when reach bottom
Y_SCROLL:
    .res 1

; $fd -  the number of pixels scrolled horizontally on the nametables
; (horizontal part of PPUSCROLL)
; goes from #$00-#$ff for each screen (256 pixels)
; for horizontal levels, this is how many pixels scrolled to the right
X_SCROLL:
    .res 1

; $fe - used to store value of PPUMASK before writing to PPU
PPUMASK_SETTINGS:
    .res 1

; $ff - used to set PPUCTRL value for next frame
PPUCTRL_SETTINGS:
    .res 1

.segment "RAM"

.export SOUND_CMD_LENGTH                ; $0104
.export SOUND_CODE                      ; $010a
.export SOUND_LENGTH_MULTIPLIER         ; $0110
.export SOUND_FLAGS                     ; $0116
.export SOUND_CMD_REPEATS               ; $011c
.export SOUND_CMD_ADDRS_LOW             ; $0128
.export SOUND_CMD_ADDRS_HIGH            ; $012e
.export PARENT_SOUND_ADDRS_LOW          ; $0134
.export PARENT_SOUND_ADDRS_HIGH         ; $013a
.export PARENT_SOUND_ADDRS_2_LOW        ; $014c
.export PARENT_SOUND_ADDRS_2_HIGH       ; $0152
.export SOUND_LEN_TIMERS_HIGH           ; $0158
.export SOUND_CURRENT_SLOT              ; $015b
.export SOUND_PULSE_VOLUME              ; $015e
.export SOUND_CHNL_REG_OFFSET           ; $0160
.export INIT_SOUND_CODE                 ; $0161
.export SOUND_EFFECT_VOLUME             ; $0164
.export SOUND_TRIANGLE_CONFIG           ; $0166
.export SOUND_EFFECT_VOLUME_CONTINUED   ; $0167
.export SOUND_CFG_HIGH_A                ; $016a
.export PAUSE_STATE_01                  ; $016c
.export SOUND_FRAME_SKIP_COUNT          ; $016d
.export SOUND_VOLUME_ADJ2               ; $0170
.export SOUND_FRAME_SKIP                ; $0173
.export VIBRATO_CTRL                    ; $0175
.export SOUND_VAR_UNUSED                ; $0179
.export SOUND_DPCM_SAMPLE               ; $0187
.export SOUND_TIMER_ADJ                 ; $0189
.export SOUND_OCTAVE_SHIFT              ; $018e
.export SOUND_NOTE_PERIOD_OFFSET        ; $0191
.export SOUND_PERIOD                    ; $0194
.export SOUND_LEN                       ; $0197
.export SOUND_CTRL_ENVELOPE_OFFSET      ; $019a
.export SOUND_VOLUME_CHANGE_DELAY       ; $019c
.export SOUND_ENVELOPE_READ_OFFSET      ; $019e
.export SOUND_ENVELOPE_READ_LEN         ; $01a0
.export SOUND_ENVELOPE_BASE_READ_OFFSET ; $01a2
.export DECRESCENDO_END_PAUSE           ; $01a4
.export DECRESCENDO_PERCENT             ; $01a6
.export SOUND_DECRESCENDO_STEP_TIME     ; $01a8
.export SOUND_VOLUME_ADJ_TIMER          ; $01aa
.export SOUND_PULSE_PERIOD_LOW          ; $01ac
.export SOUND_PITCH_ADJ_PERIOD          ; $01ae
.export SOUND_PITCH_ADJ_TIMER           ; $01b0
.export SOUND_PERIOD_MULT               ; $01b2
.export SOUND_PITCH_CTRL                ; $01b4
.export SOUND_PITCH_OFFSET              ; $01b6
.export SOUND_PITCH_CTRL_PARENT_OFFSET  ; $01b8
.export SOUND_PITCH_READ_LEN            ; $01ba
.export SOUND_CFG_HIGH_B                ; $01bc
.export SOUND_PITCH_FLAGS               ; $01be
.export PITCH_OVERWRITE                 ; $01c0
.export SOUND_VOLUME_ADJ                ; $01c2
.export SOUND_PERIOD_LOW                ; $01c4
.export SOUND_LEN_TIMER_HIGH            ; $01cc
.export OAMDMA_CPU_BUFFER               ; $0200
.export CPU_GRAPHICS_BUFFER             ; $0300
.export GRAPHICS_BUFFER_TEMP            ; $03c0
.export SUPERTILE_PALETTE_MERGE         ; $03c8
.export PRE_IRQ_Y_SCROLL_MAX            ; $03ca
.export PRE_IRQ_X_SCROLL_MAX            ; $03cb
.export BG_BOSS_NAMETABLE               ; $03cc
.export PALETTE_CYCLE_INDEX             ; $03cd
.export PALETTE_CYCLE_INDEX_2           ; $03cf
.export COLLISION_CODE_TILE_INDICES     ; $03d0
.export PALETTE_CPU_BUFFER              ; $03e0
.export BG_COLLISION_DATA               ; $0400
.export SECOND_BG_COLLISION_DATA        ; $0480
.export SPRITES                         ; $0500
.export ENEMY_SPRITE                    ; $0508
.export PLAYER_SPRITE                   ; $0518
.export SPRITE_Y_POS                    ; $051a
.export ENEMY_Y_POS                     ; $0522
.export PLAYER_SPRITE_Y_POS             ; $0532
.export SPRITE_X_POS                    ; $0534
.export ENEMY_X_POS                     ; $053c
.export PLAYER_SPRITE_X_POS             ; $054c
.export SPRITE_ATTR                     ; $054e
.export ENEMY_SPRITE_ATTR               ; $0556
.export PLAYER_SPRITE_ATTR              ; $0566
.export PLAYER_BULLET_SPRITE_CODE       ; $0568
.export PLAYER_BULLET_Y_POS             ; $0578
.export PLAYER_BULLET_X_POS             ; $0588
.export PLAYER_BULLET_SPRITE_ATTR       ; $0598
.export PLAYER_BULLET_STATE             ; $05a8
.export PLAYER_BULLET_WEAPON_TYPE       ; $05b8
.export PLAYER_BULLET_Y_VEL_ACCUM       ; $05c8
.export PLAYER_BULLET_X_VEL_ACCUM       ; $05d8
.export PLAYER_BULLET_Y_VEL_FRACT       ; $05e8
.export PLAYER_BULLET_DMG               ; $05f8
.export PLAYER_BULLET_X_VEL_FRACT       ; $0608
.export PLAYER_BULLET_X_VEL_FAST        ; $0618
.export PLAYER_BULLET_TIMER             ; $0628
.export PLAYER_BULLET_COLLISION_CODE    ; $0638
.export PLAYER_BULLET_AIM_DIR           ; $0648
.export PLAYER_BULLET_Y_VEL_FAST        ; $0658
.export ENEMY_ROUTINE                   ; $0668
.export ENEMY_HP                        ; $0676
.export ENEMY_Y_VEL_ACCUM               ; $0684
.export ENEMY_X_VEL_ACCUM               ; $0692
.export ENEMY_Y_VELOCITY_FRACT          ; $06a0
.export ENEMY_Y_VELOCITY_FAST           ; $06ae
.export ENEMY_X_VELOCITY_FRACT          ; $06bc
.export ENEMY_X_VELOCITY_FAST           ; $06ca
.export ENEMY_TYPE                      ; $06d8
.export ENEMY_DELAY                     ; $06e6
.export ENEMY_FIRING_DELAY              ; $06f4
.export ENEMY_ANIMATION_DELAY           ; $0702
.export ENEMY_FRAME                     ; $0710
.export ENEMY_ATTRIBUTES                ; $071e
.export ENEMY_DESTROY_ATTRS             ; $072c
.export ENEMY_COLLISION_INDEX           ; $073a
.export ENEMY_VAR_1                     ; $0748
.export ENEMY_VAR_2                     ; $0756
.export ENEMY_VAR_3                     ; $0764
.export ENEMY_VAR_4                     ; $0772
.export ENEMY_VAR_5                     ; $0780
.export ENEMY_VAR_6                     ; $078e
.export ENEMY_VAR_7                     ; $079c
.export ATTRIBUTE_OVERRIDES             ; $07d0
.export HI_SCORE                        ; $07e0
.export P1_SCORE                        ; $07e3
.export P2_SCORE                        ; $07e6
.export INIT_SOUND_CODE_00              ; $07e9
.export HI_SCORE_RESET_FLAG             ; $07ea
.export CHEAT_CODE_STATUS               ; $07ec
.export PRG_BANK_CHG_X_BACKUP           ; $07ed
.export LOW_PRG_BANK                    ; $07ee
.export HIGH_PRG_BANK                   ; $07ef
.export CHR_BANKS                       ; $07f0
.export LEFT_TOP_HALF_CHR_BANK          ; $07f0
.export LEFT_BOTTOM_CHR_HALF_BANK       ; $07f1
.export RIGHT_FIRST_QTR_CHR_BANK        ; $07f2
.export RIGHT_SECOND_QTR_CHR_BANK       ; $07f3
.export RIGHT_THIRD_QTR_CHR_BANK        ; $07f4
.export RIGHT_FOURTH_QTR_CHR_BANK       ; $07f5
.export SPLIT_X_SCROLL                  ; $07f6
.export SPLIT_PPUCTRL                   ; $07f7
.export SPLIT_SCANLINE_IRQ_2            ; $07f9
.export SPLIT_SCANLINE_IRQ_3            ; $07fa
.export IRQ_HANDLER_PPUADDR             ; $07fb

; $0100 - !(UNUSED)
.res 4

; $0104 - how many video frames the sound command should last for, i.e. the time
; to wait before reading next sound commands
; #$06 bytes, one for each sound slot
SOUND_CMD_LENGTH:
    .res 6

; $010a - the sound code for the sound slot
; a sound code is the number representing a logical sound to play, either a
; sound effect or background music.  A single sound code can be composed of
; multiple slots playing simultaneously.
; #$06 bytes, one for each sound slot
SOUND_CODE:
    .res 6

; $0110 - value used when determining how many video frames to wait before
; reading next sound command
; #$06 bytes, one for each sound slot
SOUND_LENGTH_MULTIPLIER:
    .res 6

; $0116 - various flags encoded in the bits used when processing sound
;  * bit 0
;    * 1 - sound effect command
;    * 0 - background music (bgm) command
; #$06 bytes, one for each sound slot
SOUND_FLAGS:
    .res 6

; $011c - the number of times to repeat the processing sound command
; #$06 bytes, one for each sound slot
SOUND_CMD_REPEATS:
    .res 6

; $0122 - !(UNUSED)
.res 6

; $0128 - low byte of address pointing to sound command
; #$06 bytes, one for each sound slot
SOUND_CMD_ADDRS_LOW:
    .res 6

; $012e - high byte of address pointing to sound command
; #$06 bytes, one for each sound slot
SOUND_CMD_ADDRS_HIGH:
    .res 6

; $0134 - parent sound command low byte
; #$06 bytes, one for each sound slot
PARENT_SOUND_ADDRS_LOW:
    .res 6

; $013a - parent sound command high byte
; #$06 bytes, one for each sound slot
PARENT_SOUND_ADDRS_HIGH:
    .res 6

; $0140 - !(UNUSED)
.res 6

; $0146 - !(UNUSED)
.res 6

; $014c - parent sound command low byte
; #$06 bytes, one for each sound slot
PARENT_SOUND_ADDRS_2_LOW:
    .res 6

; $0152 - parent sound command high byte
; #$06 bytes, one for each sound slot
PARENT_SOUND_ADDRS_2_HIGH:
    .res 6

; $0158 - stores the length counter load bits (l)
; slots 4 and 5 are stored in SOUND_LEN_TIMERS_HIGH_CONTINUED
; slot 3 does not use this variable.  SOUND_CURRENT_SLOT is there instead
; see also SOUND_LEN, and CUR_SOUND_LEN_TIMER_HIGH
SOUND_LEN_TIMERS_HIGH:
    .res 3

; $015b - the current sound slot [#$00-#$05], slots are in priority order
; (highest priority to lowest priority)
;  * #$00 = pulse 1 channel
;  * #$01 = pulse 2 channel
;  * #$02 = triangle channel
;  * #$03 = noise and dmc channel
;  * #$04 = pulse 1
;  * #$05 = noise channel
SOUND_CURRENT_SLOT:
    .res 1

; $015c - length of sound for slots 4 and slot 5 (never directly referenced)
SOUND_LEN_TIMERS_HIGH_CONTINUED:
    .res 2

; $015e - the pulse channel volume before modified
; #$02 bytes, one for each pulse channel
SOUND_PULSE_VOLUME:
    .res 2

; $0160 - APU sound channel configuration register offset
;  * #$00 for first pulse channel
;  * #$04 for second
;  * #$08 for triangle
;  * #$0c for noise
SOUND_CHNL_REG_OFFSET:
    .res 1

; $0161 - the sound code to load, e.g. #$12 = sound_12 (HARETSU)
; sound codes greater than #$37 are dmc sounds
INIT_SOUND_CODE:
    .res 3

; $0164 - constant volume for pulse and noise channels
; not used for noise channel.  SOUND_TRIANGLE_CONFIG is there instead
; slots 4 and 5 are stored in SOUND_EFFECT_VOLUME_CONTINUED
SOUND_EFFECT_VOLUME:
    .res 2

; $0166 - used when calculating triangle config
SOUND_TRIANGLE_CONFIG:
    .res 1

; $0167 - volume for slots 3, 4, and 5 (never directly referenced)
SOUND_EFFECT_VOLUME_CONTINUED:
    .res 3

; $016a - the value to merge with the high nibble before storing in APU channel
; config register, compare to SOUND_CFG_HIGH_B
; which config is used is basd on VIBRATO_CTRL flag bit 5
; #$06 bytes, one for each sound slot
SOUND_CFG_HIGH_A:
    .res 2

; $016c - whether or not the game is paused, used for sound logic
PAUSE_STATE_01:
    .res 1

; $016d - specify number of frames before skipping processing sound for a single
; frame.  Used only by sound_32.  See SOUND_FRAME_SKIP
SOUND_FRAME_SKIP_COUNT:
    .res 1

; continuation of SOUND_CFG_HIGH_A
.res 2

; $0170 - used on pulse or triangle channel
SOUND_VOLUME_ADJ2:
    .res 3

; $0173 - current counter value for use with SOUND_FRAME_SKIP_COUNT
; when counted up to SOUND_FRAME_SKIP_COUNT, sound processing will skip a single
; frame.
SOUND_FRAME_SKIP:
    .res 1

; $0174 - continuation of SOUND_VOLUME_ADJ2
.res 1

; $0175 - various sound flags that adjust pitch and tone
;  * bit 0
;  * bit 4 - when non-zero uses SOUND_PITCH_ADJ_TIMER (see check_pitch_adjust)
;  * bit 5 - 0 = SOUND_CFG_HIGH_A, 1 = use SOUND_CFG_HIGH_B
;  * bit 7
VIBRATO_CTRL:
    .res 5

; $017a - !(UNUSED) only ever written to for $017d (for slot 4) (e.g. sound_06)
SOUND_VAR_UNUSED:
    .res 13

; $0187 - used to know which sound sample code to play
SOUND_DPCM_SAMPLE:
    .res 1

; $0188 - !(UNUSED)
.res 1

; $0189 - used on pulse or triangle channel
;  * triangle linear counter reload value (fine grain duration before silencing)
;  * SOUND_TIMER_ADJ+3 is for noise channel (linear counter reload value)
SOUND_TIMER_ADJ:
    .res 5

; $018e - how many octaves to shift the sound pitch
SOUND_OCTAVE_SHIFT:
    .res 3

; $0191 - pulse 1, pulse 2, or triangle
SOUND_NOTE_PERIOD_OFFSET:
    .res 3

; $0194 - stores sound channel period byte
SOUND_PERIOD:
    .res 3

; $0197 - loaded into CUR_SOUND_LEN_TIMER_HIGH for the current slot
SOUND_LEN:
    .res 3

; $019a - sound_envelope_ptr_tbl offset
SOUND_CTRL_ENVELOPE_OFFSET:
    .res 2

; $019c - used to time pulse volume delay
SOUND_VOLUME_CHANGE_DELAY:
    .res 2

; $019e - sound_envelope_xx read offset
SOUND_ENVELOPE_READ_OFFSET:
    .res 2

; $01a0 - pulse length to restart envelope reading
; not effectively used with the audio in game
SOUND_ENVELOPE_READ_LEN:
    .res 2

; $01a2 - parent pulse envelope read offset
SOUND_ENVELOPE_BASE_READ_OFFSET:
    .res 2

; $01a4 - number of video frames before end of sound command in which the
; pulse decrescendo will resume
DECRESCENDO_END_PAUSE:
    .res 2

; $01a6 - used when calculating DECRESCENDO_END_PAUSE for pulse slots
DECRESCENDO_PERCENT:
    .res 2

; $01a8 - used with SOUND_VOLUME_ADJ_TIMER
SOUND_DECRESCENDO_STEP_TIME:
    .res 2

; $01aa - when decremented to 0, @lower_volume is called
SOUND_VOLUME_ADJ_TIMER:
    .res 2

; $01ac - backup of CUR_SOUND_PERIOD_LOW stored between frames
; ultimately written to APU_PULSE_PERIOD_LOW
SOUND_PULSE_PERIOD_LOW:
    .res 2

; $01ae - used when setting SOUND_PITCH_ADJ_TIMER when VIBRATO_CTRL bit 4 set
SOUND_PITCH_ADJ_PERIOD:
    .res 2

; $01b0 - used for pulse 1 or pulse 2 for calculating pulse length and period
SOUND_PITCH_ADJ_TIMER:
    .res 2

; $01b2 - used when calculating period in calc_period_adjust
SOUND_PERIOD_MULT:
    .res 2

; $01b4 - used when reading sound_pitch_ctrl_ptr_tbl
SOUND_PITCH_CTRL:
    .res 2

; $01b6 - sound_pitch_ctrl_xx offset
SOUND_PITCH_OFFSET:
    .res 2

; $01b8 - sound_pitch_ctrl_xx offset to go after reading SOUND_PITCH_READ_LEN
SOUND_PITCH_CTRL_PARENT_OFFSET:
    .res 2

; $01ba - the amount of bytes to read in sound_pitch_ctrl_xx
SOUND_PITCH_READ_LEN:
    .res 2

; $01bc - the value to merge with the high nibble before storing in APU channel
; config register, compare to SOUND_CFG_HIGH_A
; which config is used is basd on VIBRATO_CTRL flag bit 5
SOUND_CFG_HIGH_B:
    .res 2

; $01be - used for pulse 1 or pulse 2
;  * bit 4 - double SOUND_VAR_1
;  * bit 6 - whether or not to overwrite other channel's pitches
;    see overwrite_pitches
;  * bit 7
SOUND_PITCH_FLAGS:
    .res 2

; $01c0 - references sound slots to overwrite the pitch of
; stores values 0, 1, 2, or 3
PITCH_OVERWRITE:
    .res 2

; $01c2 - amount to lower volume by, maximum value of #$0f
SOUND_VOLUME_ADJ:
    .res 2

; $01c4 - #$08 timer bytes (low bits), for use in changing pitch quickly
; low 4 bytes are for pulse 1
; high 4 bytes are for pulse 2
SOUND_PERIOD_LOW:
    .res 8

; $01cc - #$08 timer bytes (high bits), for use in changing pitch quickly
; low 4 bytes are for pulse 1
; high 4 bytes are for pulse 2
SOUND_LEN_TIMER_HIGH:
    .res 8

; $01d4 - reserved for stack
; technically stack is $0100 to $01ff but Super C assumes stack will never
; underflow into the memory used for sound processing
.res 44

; $0200-$02ff OAMDMA (sprite) read data for direct memory access (DMA) writes to
; the PPU.  read once per frame.  Populated by load_sprites_to_oam_buffer
OAMDMA_CPU_BUFFER:
    .res 256

; $0300 - used to store data that will be then moved to the PPU later on.
; repeating structure
; byte 0 - specifies PPUCTRL and format of following bytes
;  * ppuctrl_tbl-1 offset, specifies PPUCTRL (including VRAM increment)
;  * when byte 0 is #$00 (exit), then then done writing graphics buffer to PPU,
;    do nothing
;  * when byte 0 is #$03 (repeat mode), then byte 3 is repetition count and byte
;    4 is the byte to repeatedly write to PPU
;  * when byte 0 is #$06 or #$07 (block mode), then byte 3 is length, and bytes
;    4 to (byte 4 + length) are written to PPU
;    when that data is written to the PPU, then the next two bytes are a new PPU
;    address, and the process loops until #$ff is met
;  * when byte 0 is any other number (flush mode), then bytes 3 onward are
;    written to PPU until #$ff is met
; byte 1 and byte 2 are PPU addresses high byte then low byte respectively
; this entire structure is looped until the first byte is #$00
CPU_GRAPHICS_BUFFER:
    .res 192

; $03c0 - some sort of backup for pattern tiles, copied from CPU_GRAPHICS_BUFFER
; and put back
GRAPHICS_BUFFER_TEMP:
    .res 8

; $03c8 - high nibble set when SUPERTILE_DRAW_COUNT is non-zero
SUPERTILE_PALETTE_MERGE:
    .res 1

; $03c9 - !(UNUSED)
    .res 1

; $03ca - the starting scroll from which the enemy Y position is subtracted.
; For example, when krypto-crustacean is at the top of the screen, the Y scroll
; is #$8c, which is this value.  As the boss swoops down (ENEMY_X_POS
; increases), the Y scroll will be scrolled up
; in general used for enemies built from bg tiles so that the X and Y pos can
; be simulated with scrolling
; krypto-crustacean, helicopter core, fortress wall core, stomping ceiling
PRE_IRQ_Y_SCROLL_MAX:
    .res 1

; $03cb - the starting scroll from which the enemy X position is subtracted.
; As ENEMY_X_POS increases, the scroll decreases
; in general used for enemies built from bg tiles so that the X and Y pos can
; be simulated with scrolling
; krypto-crustacean, helicopter core, fortress wall core, stomping ceiling
PRE_IRQ_X_SCROLL_MAX:
    .res 1

; $03cc - nametable bits (bits 0 and 1 of PPUCTRL)
BG_BOSS_NAMETABLE:
    .res 1

; $03cd - a timer used when cycling palette colors in a background palette
;  * level 3 - boss screen flashing red (background palette 0 for level 3)
;  * level 4 - (background palette 3 color 3)
;  * level 5 - cliff clouds color cycling
;  * level 6 - cycle floor color form blue to red for boss reveal
;  * level 8 - cycle the background palette
PALETTE_CYCLE_INDEX:
    .res 1

; $03ce - !(UNUSED)
.res 1

; $03cf - second control for additional palette color cycling
; level 5
;  * bit 6 clear - controls flashing red of pill box sensor (enemy type #$04)
;    and rotating guns (enemy type #$04)
;  * bit 7 set - boss screen for krypto-crustacean
; level 6 - red color cycle for side skull eyes
PALETTE_CYCLE_INDEX_2:
    .res 1

; $03d0 - each entry is an index into the pattern table tiles, specifies
; collision codes for the level, e.g. COLLISION_CODE_TILE_INDICES+2 is one more
; than the maximum tile index for collision code 2
COLLISION_CODE_TILE_INDICES:
    .res 16

; $03e0 - the CPU memory address of the palettes eventually loaded into the PPU
; $3f00 to $3f1f.  First loaded into graphics CPU_GRAPHICS_BUFFER, but then
; loaded into PPU
PALETTE_CPU_BUFFER:
    .res 32

; $0400 - map of collision types for each of the supertiles for both nametables
; each nibble is 16 pixels wide (2 nametable tiles, 1/4 of a super-tile)
; each byte is 32 pixels wide (4 nametable tiles, 1/2 of a super-tile)
; every #$08 bytes moves back/forward 2 nametable rows
; every #$10 bytes moves up/down one super-tile
BG_COLLISION_DATA:
    .res 128

; $0480 - collision table used for the "B" nametables.
; used when nametable vertical mirroring for the right nametables (AB|AB)
; used when nametable horizontal mirroring for the bottom nametables (AA|BB)
SECOND_BG_COLLISION_DATA:
    .res 128

; $0500 - sprites for player bullets (and intro screen cursor/logo sprite)
SPRITES:
    .res 8

; $0508 - reserved for each enemy's sprite code
ENEMY_SPRITE:
    .res 14

; $0516 - !(UNUSED) probably reserved room for more enemies, max enemies is 14
.res 2

; $0518 - player sprites, references player_sprite_ptr_tbl
; $0519 - player 2
PLAYER_SPRITE:
    .res 2

; $051a - Y position on screen of general sprites
SPRITE_Y_POS:
    .res 8

; $0522 - Y position on screen of each enemy sprite.
; starts at #$00 for top of screen goes down as value increases
ENEMY_Y_POS:
    .res 14

; $0530 - !(UNUSED) probably reserved room for more enemies, max enemies is 14
; only ever read from
.res 2

; $0532 - player Y position
; starts at #$00 for top of screen goes down as value increases
; $0533 - player 2
PLAYER_SPRITE_Y_POS:
    .res 2

; $0534 - general sprite X position
SPRITE_X_POS:
    .res 8

; $053c - X position on screen of each enemy sprite
ENEMY_X_POS:
    .res 14

; $054a - !(UNUSED) probably reserved room for more enemies, max enemies is 14
; only ever read from
.res 2

; $054c - player X position used for drawing sprite
; copy of $cc PLAYER_X_POS, which is in the zero page and used for calculating
; player location
; $054d - player 2
PLAYER_SPRITE_X_POS:
    .res 2

; $054e - sprite attribute, specifies palette, vertical flip, horizontal flip
; and whether to adjust Y position
;  * bit 0 and 1 - sprite palette
;  * bit 2 - whether to add #$01 to sprite Y position, used for recoil effect
;    firing weapon
;  * bit 5 - bg priority
;  * bit 6 - whether to flip the sprite horizontally
;  * bit 7 - whether to flip the sprite vertically
; each byte is the player bullet sprite attributes
SPRITE_ATTR:
    .res 8

; $0556 - enemy sprite attribute. See specification above for SPRITE_ATTR
ENEMY_SPRITE_ATTR:
    .res 14

; $0564 - !(UNUSED) probably reserved room for more enemies, max enemies is 14
.res 2

; $0566 - player sprite attribute, specifies palette, vertical flip, horizontal
; flip and whether to adjust Y position
;  * bit 0 and 1 - sprite palette
;  * bit 2 - whether to add #$01 to sprite Y position, used for recoil effect
;    firing weapon
;  * bit 5 - bg priority
;  * bit 6 - whether to flip the sprite horizontally
;  * bit 7 - whether to flip the sprite vertically
; examples: player being electrocuted or invincible (flashes various colors)
PLAYER_SPRITE_ATTR:
    .res 2

; $0568 - the sprite codes to load for the bullet, eventually copied into
; CPU_SPRITE_BUFFER
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_SPRITE_CODE:
    .res 16

; $0578 - the bullet's sprite Y position
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_Y_POS:
    .res 16

; $0588 - the bullet's sprite X position
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_X_POS:
    .res 16

; $0598 - the sprite attributes for the bullet (see SPRITE_ATTR for details)
; used for L bullets for flipping the angled sprites depending on direction
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_SPRITE_ATTR:
    .res 16

; $05a8 - the state of the player bullet
;  * 0 - unused
;  * 1 - normal
;  * 2 - destroyed
;  * 3 - just created laser
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_STATE:
    .res 16

; $05b8 - the type of weapon and the player that created the bullet,
; see PLAYER_CURRENT_WEAPON for values
;  * bit 7 - rapid fire flag
;  * bit 6 - set for player 2, clear for player 1
; low nibble is weapon type
;  * #$00 - Regular
;  * #$01 - Machine (M)
;  * #$02 - Spray (S)
;  * #$03 - Laser (L)
;  * #$04 - Flame (F)
;  * #$05 - charged Flame (F)
;  * #$06 - charged child Flame (F)
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_WEAPON_TYPE:
    .res 16

; $05c8 - an accumulator to keep track of PLAYER_BULLET_Y_VEL_FRACT being added
; to itself have elapsed before adding 1 to PLAYER_BULLET_Y_POS
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_Y_VEL_ACCUM:
    .res 16

; $05d8 - an accumulator to keep track of PLAYER_BULLET_X_VEL_FRACT being added
; to itself have elapsed before adding 1 to PLAYER_BULLET_X_POS
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_X_VEL_ACCUM:
    .res 16

; $05e8 - fractional Y velocity
; added to itself until overflows, which increments PLAYER_BULLET_Y_VEL_FAST
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_Y_VEL_FRACT:
    .res 16

; $05f8 - one less than how much to affect enemy's HP on collision,
; e.g. 0 takes 1 HP per hit, 4 takes 5 HP per hit
; all weapons are 0, except 5 which is charged F weapon (flame)
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_DMG:
    .res 16

; $0608 - fractional X velocity
; added to itself until overflows, which increments PLAYER_BULLET_X_VEL_FAST
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_X_VEL_FRACT:
    .res 16

; $0618 - player bullet X velocity integer portion
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_X_VEL_FAST:
    .res 16

; $0628 - counts up as bullet travels, used for timing and controlling max
; distance traveled
;  * for S outdoor bullets, used to determine the size (scale) of the bullet
;  * for L when just created, counts down as a delay for spacing out lasers,
;    then is used like other bullets for distance
; * for destroyed player bullets, becomes a timer initialized to #$06
;   controlling how long before the circular explosion sprite is removed
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_TIMER:
    .res 16

; $0638 - bullet base collision offset into collision_box_tbl
; always #$9f, or #$00 (no bullet)
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_COLLISION_CODE:
    .res 16

; $0648 - aim direction for bullet
; only used by L weapon sprite index, indexes into laser_bullet_sprite_tbl
;  * #$00 - right
;  * #$01 - down right
;  * #$02 - down
;  * #$03 - down left
;  * #$04 - left
;  * #$05 - up left
;  * #$06 - up
;  * #$07 - up right
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_AIM_DIR:
    .res 16

; $0658 - player bullet Y velocity integer portion
; first 8 bytes are for player 1, second 8 bytes for player 2
PLAYER_BULLET_Y_VEL_FAST:
    .res 16

; $0668 - index to routine number for enemy
; subtract 1 to get real routine, since all offsets are off by 1
; (...routine_ptr_tbl - 2)
; ex: for flying capsule, setting ENEMY_ROUTINE to #$01 causes
; flying_capsule_routine_00 to run the next frame
ENEMY_ROUTINE:
    .res 14

; $0676 - the HP of the enemy
ENEMY_HP:
    .res 14

; $0684 - an accumulator to keep track of ENEMY_Y_VELOCITY_FRACT being added to
; itself have elapsed before adding 1 to ENEMY_Y_POS
; can be thought of as a variable to hold the accumulated fractional velocity
; until an overflow occurs
ENEMY_Y_VEL_ACCUM:
    .res 14

; $0692 - an accumulator to keep track of ENEMY_X_VELOCITY_FRACT being added to
; itself have elapsed before adding 1 to ENEMY_X_POS
; can be thought of as a variable to hold the accumulated fractional velocity
; until an overflow occurs
ENEMY_X_VEL_ACCUM:
    .res 14

; $06a0 - enemy fractional Y velocity
; added to itself until overflows, which increments ENEMY_Y_VELOCITY_FAST
ENEMY_Y_VELOCITY_FRACT:
    .res 14

; $06ae
ENEMY_Y_VELOCITY_FAST:
    .res 14

; $06bc - enemy fractional X velocity
; added to itself until overflows, which increments ENEMY_X_VELOCITY_FAST
ENEMY_X_VELOCITY_FRACT:
    .res 14

; $06ca - enemy X velocity integer portion
ENEMY_X_VELOCITY_FAST:
    .res 14

; $06d8 - type of the enemy
ENEMY_TYPE:
    .res 14

; $06e6 - used differently by different enemies for various delay logic
ENEMY_DELAY:
    .res 14

; $06f4 - used by enemies to time firing projectiles
ENEMY_FIRING_DELAY:
    .res 14

; $0702 - used by enemies to delay various animation routines
ENEMY_ANIMATION_DELAY:
    .res 14

; $0710 - typically stores index into enemy animation frame
; many, but not all, enemies used shared method to set sprite based on
; ENEMY_FRAME (see set_enemy_animation_sprite)
; many enemies, e.g. sniper set the sprite themselves, but still set
; ENEMY_FRAME to an index that then looks up the sprite code
ENEMY_FRAME:
    .res 14

; $071e - enemy type-specific attributes that define how an enemy behaves and/or
; looks
ENEMY_ATTRIBUTES:
    .res 14

; $072c - loaded from enemy_prop_ptr_tbl
;  * bit 0
;    * 0 - test player-enemy collision
;    * 1 - disable player-enemy collision test
;  * bit 1
;    * 0 - spike explosion animation (4 frames)
;    * 1 - circular explosion animation (3 frames)
;  * bits 2345 specify sound code to play when destroyed, index into
;    enemy_destroyed_sound_tbl
;  * bit 6 - when set, set delay of #$01 when destroyed
;    (see set_destroyed_enemy_routine)
;  * bit 7
;    * 0 - enable bullet collision
;    * 1 - allow bullets to travel through enemy, e.g. weapon item
ENEMY_DESTROY_ATTRS:
    .res 14

; $073a - used along with PLAYER_STATE to determine offset into
; collision_box_tbl (always multiple of #$04)
ENEMY_COLLISION_INDEX:
    .res 14

; $0748 - generic variable for use by enemies for any use
ENEMY_VAR_1:
    .res 14

; $0756 - generic variable for use by enemies for any use
ENEMY_VAR_2:
    .res 14

; $0764 - generic variable for use by enemies for any use
ENEMY_VAR_3:
    .res 14

; $0772 - generic variable for use by enemies for any use
ENEMY_VAR_4:
    .res 14

; $0780 - generic variable for use by enemies for any use
ENEMY_VAR_5:
    .res 14

; $078e - generic variable for use by enemies for any use
ENEMY_VAR_6:
    .res 14

; $079c - generic variable for use by enemies for any use
ENEMY_VAR_7:
    .res 14

; $7aa - !(UNUSED)
.res 38

; 07d0 - post door destroyed attribute bytes overrides
; each of the #$0a bytes are specific to an single supertile index to overwrite
; (see attribute_overrides_tbl)
; $89,$8d,$ac,$af,$c1,$c2,$ec,$ed,$ee,$ef
; when byte is populated, supertile index at position in list is replaced with
; specified attribute byte
ATTRIBUTE_OVERRIDES:
    .res 10

; $07da - !(UNUSED)
.res 6

; $07e0 - binary-coded decimal of high score, e.g. 0047580 = #$58 #$47 #$00
HI_SCORE:
    .res 3

; $07e3 - binary-coded decimal of p1 score, e.g. 0066080 = #$08 #$66 #$00
P1_SCORE:
    .res 3

; $07e6 - binary-coded decimal of p2 score, e.g. 0047580 = #$58 #$47 #$00
P2_SCORE:
    .res 3

; $07e9 - the sound code to load
; sound codes greater than or equal to #$37 are dmc sounds
INIT_SOUND_CODE_00:
    .res 1

; $07ea - 2-byte signature that is checked during reset to keep hi score
; if these 2 bytes are set to #$e3 and #$b1, then a reset will not overwrite the
; hi score with 0020000, if they are not #$e3 and #$b1, then initialize the hi
; score.  This allows soft resets (reset button) to keep hi score.
HI_SCORE_RESET_FLAG:
    .res 2

; $07ec - cheat code status
;  * 0 - cheat code not entered
;  * 1 - 10 extra lives cheat code enteredsuccessfully
CHEAT_CODE_STATUS:
    .res 1

; $07ed - backup of x register when changing PRG banks
PRG_BANK_CHG_X_BACKUP:
    .res 1

; $07ee - bank number of PRG ROM bank at $8000-$9fff
LOW_PRG_BANK:
    .res 1

; $07ef - bank number of PRG ROM bank at $a000-$bfff
HIGH_PRG_BANK:
    .res 1

; $07f0 - bank number of 2 KiB CHR bank at PPU $0000-$07ff
; top half of left pattern table
CHR_BANKS:
LEFT_TOP_HALF_CHR_BANK:
    .res 1

; $07f1 - bank number of 2 KiB CHR bank at PPU $0800-$0fff
; bottom half of left pattern table
LEFT_BOTTOM_CHR_HALF_BANK:
    .res 1

; $07f2 - bank number of 1 KiB CHR bank at PPU $1000-$13ff
; first quarter of right pattern table
RIGHT_FIRST_QTR_CHR_BANK:
    .res 1

; $07f3 - bank number of 1 KiB CHR bank at PPU $1400-$17ff
; second quarter of right pattern table
RIGHT_SECOND_QTR_CHR_BANK:
    .res 1

; $07f4 - bank number of 1 KiB CHR bank at PPU $1800-$1bff
; third quarter of right pattern table
RIGHT_THIRD_QTR_CHR_BANK:
    .res 1

; $07f5 - bank number of 1 KiB CHR bank at PPU $1c00-$1fff
; last quarter of right pattern table
RIGHT_FOURTH_QTR_CHR_BANK:
    .res 1

; $07f6 - the PPU horizontal scroll x to use after an interrupt
; updating scroll during rendering does not update vertical scroll
; because changes to PPU t register will be ignored at the end of the line
; however, the second write does clear the write toggle
; for more information, see split horizontal scroll
; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
SPLIT_X_SCROLL:
    .res 1

; $07f7 - the PPUCTRL to use after an interrupt
SPLIT_PPUCTRL:
    .res 1

; $07f8 - !(UNUSED)
.res 1

; $07f9 - used after first scanline interrupt to set the 2nd scanline interrupt
; it is the number of scanlines until next IRQ
SPLIT_SCANLINE_IRQ_2:
    .res 1

; $07fa - another SCANLINE_IRQ_1 that can be set during an irq
SPLIT_SCANLINE_IRQ_3:
    .res 1

; $07fb - the PPU address low byte to use after an interrupt
IRQ_HANDLER_PPUADDR:
    .res 2