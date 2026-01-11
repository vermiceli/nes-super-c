; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank 1 continues from bank 0 with automated player movement data for the demo
; (attract) levels.  Bank 1 then contains automated player movement logic for
; end of level animation.  There is also extra-lives cheat code input check, an
; unused level select feature.  After that there is logic for running the intro
; screen as well as the ending credits.
; Then bank 1 contains sound_32 and sound_36 data. Finally, bank 1 ends with
; level 2 graphic data: screen layout, screen supertile, and screen supertile
; palette definitions.

; 8 KiB PRG ROM
.segment "BANK1"

.include "constants.asm"

; import from bank 0
.import write_graphic_data_to_ppu

; import from bank f
.import run_routine_from_tbl_below
.import increment_game_routine
.import init_player_for_level_a
.import clear_mid_high_ram
.import set_intro_screen_bg_sprite
.import write_palette_to_graphics_buffer
.import set_menu_pattern_tiles
.import write_bg_palette_to_graphics_buffer

; export for bank c
.export sound_32_slot_00
.export sound_32_slot_01
.export sound_32_slot_02
.export sound_32_slot_03
.export sound_36_slot_00
.export sound_36_slot_01
.export sound_36_slot_02
.export sound_36_slot_03

; export for bank f
.export demo_input_lvl_02_p2
.export run_end_level_anim_routine
.export extra_lives_cheat_input_check
.export level_select
.export run_intro_animation_routine
.export run_scroll_credits_routine
.export level_2_supertiles_screen_ptr_table
.export level_2_supertile_data
.export level_2_palette_data
.export level_2_screen_layout_tbl

; continuation of demo_input_lvl_02_p1
    .byte $09,$13 ; up right for #13 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $04,$0e ; down for #0e even frames
    .byte $01,$04 ; right for #04 even frames
    .byte $09,$20 ; up right for #20 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $04,$22 ; down for #22 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $09,$20 ; up right for #20 even frames
    .byte $01,$03 ; right for #03 even frames
    .byte $09,$07 ; up right for #07 even frames
    .byte $08,$15 ; up for #15 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $01,$10 ; right for #10 even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $02,$05 ; left for #05 even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $04,$01 ; down for #01 even frame
    .byte $01,$0b ; right for #0b even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $0a,$06 ; up left for #06 even frames
    .byte $02,$01 ; left for #01 even frame
    .byte $0a,$06 ; up left for #06 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $09,$02 ; up right for #02 even frames
    .byte $01,$21 ; right for #21 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $08,$09 ; up for #09 even frames
    .byte $09,$10 ; up right for #10 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $04,$1a ; down for #1a even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $89,$04 ; up right and a for #04 even frames
    .byte $88,$01 ; up and a for #01 even frame
    .byte $8a,$05 ; up left and a for #05 even frames
    .byte $82,$02 ; left and a for #02 even frames
    .byte $88,$01 ; up and a for #01 even frame
    .byte $81,$01 ; right and a for #01 even frame
    .byte $01,$07 ; right for #07 even frames
    .byte $09,$0f ; up right for #0f even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $09,$05 ; up right for #05 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $09,$09 ; up right for #09 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $09,$0d ; up right for #0d even frames
    .byte $08,$06 ; up for #06 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $08,$0c ; up for #0c even frames
    .byte $09,$03 ; up right for #03 even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $09,$0b ; up right for #0b even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $04,$1b ; down for #1b even frames
    .byte $84,$01 ; down and a for #01 even frame
    .byte $88,$01 ; up and a for #01 even frame
    .byte $89,$07 ; up right and a for #07 even frames
    .byte $81,$01 ; right and a for #01 even frame
    .byte $01,$05 ; right for #05 even frames
    .byte $09,$05 ; up right for #05 even frames
    .byte $08,$08 ; up for #08 even frames
    .byte $09,$03 ; up right for #03 even frames
    .byte $01,$0b ; right for #0b even frames
    .byte $09,$04 ; up right for #04 even frames
    .byte $08,$06 ; up for #06 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $09,$1e ; up right for #1e even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $ff,$ff ; end input#$ff frames

demo_input_lvl_02_p2:
    .byte $00,$57 ; no input for #57 even frames
    .byte $02,$09 ; left for #09 even frames
    .byte $0a,$03 ; up left for #03 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $01,$5d ; right for #5d even frames
    .byte $02,$04 ; left for #04 even frames
    .byte $00,$05 ; no input for #05 even frames
    .byte $01,$0c ; right for #0c even frames
    .byte $81,$03 ; right and a for #03 even frames
    .byte $89,$02 ; up right and a for #02 even frames
    .byte $09,$03 ; up right for #03 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $0a,$05 ; up left for #05 even frames
    .byte $02,$06 ; left for #06 even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $05,$08 ; down and right for #08 even frames
    .byte $01,$06 ; right for #06 even frames
    .byte $04,$01 ; down for #01 even frame
    .byte $00,$05 ; no input for #05 even frames
    .byte $01,$05 ; right for #05 even frames
    .byte $02,$04 ; left for #04 even frames
    .byte $00,$07 ; no input for #07 even frames
    .byte $01,$39 ; right for #39 even frames
    .byte $09,$09 ; up right for #09 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $02,$0d ; left for #0d even frames
    .byte $0a,$04 ; up left for #04 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $81,$03 ; right and a for #03 even frames
    .byte $01,$14 ; right for #14 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $0a,$08 ; up left for #08 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $01,$29 ; right for #29 even frames
    .byte $09,$14 ; up right for #14 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $09,$06 ; up right for #06 even frames
    .byte $01,$09 ; right for #09 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $0a,$02 ; up left for #02 even frames
    .byte $02,$0d ; left for #0d even frames
    .byte $0a,$01 ; up left for #01 even frame
    .byte $08,$01 ; up for #01 even frame
    .byte $88,$02 ; up and a for #02 even frames
    .byte $89,$02 ; up right and a for #02 even frames
    .byte $81,$01 ; right and a for #01 even frame
    .byte $01,$0d ; right for #0d even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$05 ; up for #05 even frames
    .byte $01,$07 ; right for #07 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $0a,$03 ; up left for #03 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $09,$06 ; up right for #06 even frames
    .byte $08,$05 ; up for #05 even frames
    .byte $09,$06 ; up right for #06 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $88,$01 ; up and a for #01 even frame
    .byte $8a,$05 ; up left and a for #05 even frames
    .byte $0a,$07 ; up left for #07 even frames
    .byte $02,$11 ; left for #11 even frames
    .byte $8a,$02 ; up left and a for #02 even frames
    .byte $88,$01 ; up and a for #01 even frame
    .byte $89,$01 ; up right and a for #01 even frame
    .byte $09,$02 ; up right for #02 even frames
    .byte $01,$07 ; right for #07 even frames
    .byte $05,$05 ; down and right for #05 even frames
    .byte $04,$02 ; down for #02 even frames
    .byte $02,$06 ; left for #06 even frames
    .byte $04,$01 ; down for #01 even frame
    .byte $01,$30 ; right for #30 even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $00,$01 ; no input for #01 even frame
    .byte $02,$03 ; left for #03 even frames
    .byte $00,$02 ; no input for #02 even frames
    .byte $01,$0c ; right for #0c even frames
    .byte $81,$02 ; right and a for #02 even frames
    .byte $89,$01 ; up right and a for #01 even frame
    .byte $88,$01 ; up and a for #01 even frame
    .byte $08,$01 ; up for #01 even frame
    .byte $0a,$04 ; up left for #04 even frames
    .byte $02,$09 ; left for #09 even frames
    .byte $06,$01 ; down left for #01 even frame
    .byte $04,$02 ; down for #02 even frames
    .byte $05,$01 ; down and right for #01 even frame
    .byte $01,$0a ; right for #0a even frames
    .byte $00,$0c ; no input for #0c even frames
    .byte $01,$10 ; right for #10 even frames
    .byte $09,$13 ; up right for #13 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $0a,$06 ; up left for #06 even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $01,$1a ; right for #1a even frames
    .byte $09,$04 ; up right for #04 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $0a,$01 ; up left for #01 even frame
    .byte $02,$02 ; left for #02 even frames
    .byte $00,$0d ; no input for #0d even frames
    .byte $80,$03 ; a for #03 even frames
    .byte $81,$02 ; right and a for #02 even frames
    .byte $01,$18 ; right for #18 even frames
    .byte $00,$02 ; no input for #02 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $05,$02 ; down and right for #02 even frames
    .byte $04,$09 ; down for #09 even frames
    .byte $05,$02 ; down and right for #02 even frames
    .byte $01,$1f ; right for #1f even frames
    .byte $02,$05 ; left for #05 even frames
    .byte $00,$0d ; no input for #0d even frames
    .byte $ff,$ff ; end input

; sets auto-scroll and auto-movement for end of level
run_end_level_anim_routine:
    lda END_LEVEL_ROUTINE
    jsr run_routine_from_tbl_below

end_level_anim_routine_ptr_tbl:
    .addr end_level_anim_routine_00 ; initialize scroll and auto move logic for end of level, advance routine
    .addr end_level_anim_routine_01 ; runs end level auto move logic

end_level_anim_routine_00:
    jsr run_init_end_level_anim ; run end level animation
    inc END_LEVEL_ROUTINE
    rts

; initializes scroll and auto move logic for end of level
run_init_end_level_anim:
    lda CURRENT_LEVEL              ; load current level
    jsr run_routine_from_tbl_below

init_end_level_anim_ptr_tbl:
    .addr init_end_level_anim_00 ; level 1 Fort Firestorm
    .addr init_end_level_anim_01 ; level 2 First Base
    .addr init_end_level_anim_02 ; level 3 Jungle
    .addr init_end_level_anim_00 ; level 4 Inner Base
    .addr init_end_level_anim_00 ; level 5 The Cliff
    .addr init_end_level_anim_05 ; level 6 Entry to HQ
    .addr init_end_level_anim_02 ; level 7 Headquarters
    .addr init_end_level_anim_07 ; level 8 The Final Stage

; level 1 Fort Firestorm
; level 4 Inner Base
; level 5 The Cliff
init_end_level_anim_00:
    lda #$02               ; X auto-scroll mode 2
    sta X_AUTOSCROLL       ; X auto-scroll while any player in right 37.5% of screen
    lda #$00
    sta SCREEN_SCROLL_TYPE ; set horizontal scrolling
    rts

; level 2 First Base
init_end_level_anim_01:
    lda #$01                 ; Y auto-scroll mode 1
    sta Y_AUTOSCROLL         ; Y auto-scroll up to checkpoint while any player in top 37.5% of screen
    lda #$40
    sta LEVEL_Y_SCROLL_FLAGS ; set cannot scroll down
    sta Y_SCROLL_FLAGS
    rts

; level 3 Jungle
; level 7 Headquarters
init_end_level_anim_02:
    .ifdef Probotector
        lda #$48                          ; delay for AUTO_MOVE_DELAY_TIMER per active player
        bne init_end_level_set_move_delay ; always branch
    .else
        lda #$90
        sta AUTO_MOVE_DELAY_TIMER
        rts
    .endif

; level 6 Entry to HQ
init_end_level_anim_05:
    ldy #$1f
    lda #$00 ; clearing bg collisions behind boss

@loop:
    sta SECOND_BG_COLLISION_DATA,y ; related to @get_collision_val
    dey
    bpl @loop
    rts

; level 8 The Final Stage
init_end_level_anim_07:
    .ifdef Probotector
        lda #$60       ; delay for AUTO_MOVE_DELAY_TIMER per active player

    ; calculate and set AUTO_MOVE_DELAY_TIMER based on players' game over status
    ; input
    ;  * a - amount to add to AUTO_MOVE_DELAY_TIMER for each non game over player
    init_end_level_set_move_delay:
        sta $00                     ; set initial auto-move delay
                                    ; doubled for 2 player when both not in game over
        lda #$00
        sta AUTO_MOVE_DELAY_TIMER   ; clear auto-move delay timer
        lda PLAYER_GAME_OVER_STATUS ; load p1 game over status (0 = not game over, 1 = game over)
        bne @add_p2_delay           ; branch if p1 game over to set delay based on p2 game over status
        jsr @add_delay              ; p1 not in game over, add $00 to delay auto-move delay timer

    @add_p2_delay:
        lda PLAYER_GAME_OVER_STATUS+1
        bne @exit                     ; exit if p2 game over

    @add_delay:
        lda $00
        clc
        adc AUTO_MOVE_DELAY_TIMER
        sta AUTO_MOVE_DELAY_TIMER

    @exit:
        rts
    .else
        lda #$c0
        sta AUTO_MOVE_DELAY_TIMER
        rts
    .endif

; runs end level auto move logic
end_level_anim_routine_01:
    ldx #$01 ; start with player 2

@player_loop:
    lda PLAYER_GAME_OVER_STATUS,x ; load player game over status (0 = not game over, 1 = game over)
    bne @continue
    jsr run_end_level_auto_move

@continue:
    dex
    bpl @player_loop
    rts

run_end_level_auto_move:
    lda CURRENT_LEVEL              ; load current level
    jsr run_routine_from_tbl_below

end_level_auto_move_tbl:
    .addr end_level_auto_move_lvl_01    ; level 1 Fort Firestorm
    .addr end_level_auto_move_lvl_02    ; level 2 First Base
    .addr end_level_auto_move_lvl_03_07 ; level 3 Jungle
    .addr end_level_auto_move_lvl_04    ; level 4 Inner Base
    .addr end_level_auto_move_lvl_05    ; level 5 The Cliff
    .addr end_level_auto_move_lvl_06    ; level 6 Entry to HQ
    .addr end_level_auto_move_lvl_03_07 ; level 7 Headquarters
    .addr end_level_auto_move_lvl_08    ; level 8 The Final Stage

; level 1, set checkpoint, and bg priority point, move player right jumping to get out of pit
end_level_auto_move_lvl_01:
    lda #$90
    sta $00                         ; set first X position checkpoint
    lda #$b8
    sta $01                         ; set X position where to set player sprite bg priority
    jmp check_auto_move_checkpoints ; move player right, setting checkpoint flag and sprite bg priority as appropriate
                                    ; jumping to avoid bg collisions (like pit on level 1)

end_level_auto_move_lvl_06:
    ldy #$00                          ; no input
    lda Y_AUTOSCROLL
    bne end_level_auto_move_exit      ; exit if not in mode 1
    lda PLAYER_SPRITE_Y_POS,x         ; mode 1 - auto-scroll to checkpoint while player at top
    cmp #$08
    bcc auto_move_complete_far        ; branch if Y position at top to mark end of level auto movement animation complete
    cmp #$50
    bcs @overhead_auto_move_input     ; branch if player below vertical center
    lda #$01
    sta PLAYER_AUTO_MOVE_CHECKPOINT,x ; set reached 1st checkpoint

@overhead_auto_move_input:
    jmp overhead_auto_move_input

end_level_auto_move_lvl_02:
    ldy #$00
    lda Y_AUTOSCROLL
    bne end_level_auto_move_exit
    lda PLAYER_SPRITE_Y_POS,x
    cmp #$08                          ; seeing if player got to top of screen
    bcc auto_move_complete_far        ; branch if Y position at top to mark end of level auto movement animation complete
    cmp #$40
    bcs overhead_auto_move_input      ; branch if player at bottom 25% of screen
    lda #$01                          ; player in the middle ~75% of screen
    sta PLAYER_AUTO_MOVE_CHECKPOINT,x ; set first checkpoint
    lda PLAYER_SPRITE_ATTR,x
    ora #$20
    sta PLAYER_SPRITE_ATTR,x          ; set bg priority (player behind background)

overhead_auto_move_input:
    ldy #$08                                  ; up d-pad input
    lda PLAYER_SPRITE_X_POS,x
    cmp overhead_auto_move_x_checkpoint_tbl,x
    beq end_level_auto_move_exit              ; branch if at X checkpoint to move up
    ldy #$09                                  ; up and right d-pad
    bcc end_level_auto_move_exit              ; branch if left of X checkpoint to move up and right
    ldy #$0a                                  ; right of X checkpoint, move left and up

end_level_auto_move_exit:
    sty CONTROLLER_STATE,x
    rts

auto_move_complete_far:
    jmp auto_move_complete ; mark end of level auto movement animation complete

overhead_auto_move_x_checkpoint_tbl:
    .byte $90,$70

end_level_auto_move_lvl_03_07:
    lda AUTO_MOVE_DELAY_TIMER
    beq @continue             ; branch if delay timer elapsed
                              ; to walk off screen to the right
    .ifdef Probotector
        lda FRAME_COUNTER
        lsr
        bcs @exit             ; decrement every other frame
    .endif
    dec AUTO_MOVE_DELAY_TIMER ; delay not elapsed

@exit:
    rts

@continue:
    lda #$ff
    sta $00                       ; set X position of the first checkpoint to far right
    sta $01                       ; set X position to enable background priority for player sprite
    jmp level_end_auto_move_right ; move player right setting checkpoint flag and sprite bg priority as appropriate

end_level_auto_move_lvl_04:
    lda #$c8
    sta $00                         ; set X position of the first checkpoint to far right
    sta $01                         ; set X position to enable background priority for player sprite
    jmp check_auto_move_checkpoints ; move player right, setting checkpoint flag and sprite bg priority as appropriate
                                    ; jumping to avoid bg collisions (like pit on level 1)

end_level_auto_move_lvl_05:
    lda #$90
    sta $00                         ; set X position of the first checkpoint to far right
    lda #$a8
    sta $01                         ; set X position to enable background priority for player sprite
    jmp check_auto_move_checkpoints ; move player right, setting checkpoint flag and sprite bg priority as appropriate
                                    ; jumping to avoid bg collisions (like pit on level 1)

end_level_auto_move_lvl_08:
    lda #$00
    sta CONTROLLER_STATE,x    ; clear controller input
    lda FRAME_COUNTER         ; load frame counter
    .ifdef Probotector
        and #$03
        bne @exit             ; exit every 3 out of 4 frames
    .else
        lsr
        bcs @exit             ; exit if odd frame
    .endif
    lda AUTO_MOVE_DELAY_TIMER ; even frame, decrement auto-move delay timer
    beq @exit
    dec AUTO_MOVE_DELAY_TIMER ; decrement auto-move delay timer
    bne @exit                 ; exit if timer not elapsed
    lda #$01                  ; timer elapsed
    sta LEVEL_ROUTINE_DELAY

@exit:
    rts

; moves player right, setting checkpoint flag and sprite bg priority as appropriate
; jumps to avoid bg collisions (like pit on level 1)
; input
;  * $00 - the X position of the first checkpoint, i.e. the place where
;    PLAYER_AUTO_MOVE_CHECKPOINT is set
;  * $01 - the X position to enable background priority for sprite
;  * x - player index (0 = p1, 1 = p2)
check_auto_move_checkpoints:
    lda #$00
    ldy X_AUTOSCROLL              ; load auto-scroll mode
    bne auto_move_exit            ; branch if X_AUTOSCROLL is enabled, do not move player while auto-scrolling
    lda PLAYER_X_POS,x            ; no auto-scroll, see if encountered collision
                                  ; side_level_apply_x_vel_or_scroll sets player X position to #$00 when encountering a bg collision
    bne level_end_auto_move_right ; branch if not obscured by bg collision
                                  ; moves player right setting checkpoint flag and sprite bg priority as appropriate
    lda #$80                      ; player encountered bg collision, jump
    sta CONTROLLER_STATE_DIFF,x   ; press the a button (jump)

; move the player right setting checkpoint flag and player sprite bg priority as appropriate
; input
;  * $00 - the X position of the first checkpoint, i.e. the place where
;    PLAYER_AUTO_MOVE_CHECKPOINT is set
;  * $01 - the X position to enable background priority for player sprite
;  * x - player index (0 = p1, 1 = p2)
level_end_auto_move_right:
    lda PLAYER_SPRITE_X_POS,x
    cmp #$e8
    bcs auto_move_complete            ; branch if far to the right
    cmp $00                           ; compare to first checkpoint X position
    bcc @check_bg_priority
    ldy #$01                          ; passed first checkpoint, set flag
    sty PLAYER_AUTO_MOVE_CHECKPOINT,x

@check_bg_priority:
    cmp $01                  ; see if at bg priority trigger point
    bcc @move_right          ; branch if not to continue walk right without bg priority
    lda PLAYER_SPRITE_ATTR,x ; reached background trigger point
    ora #$20
    sta PLAYER_SPRITE_ATTR,x ; set bg priority (player behind background)

@move_right:
    lda #$01 ; d-pad right arrow

auto_move_exit:
    sta CONTROLLER_STATE,x
    rts

; mark end of level auto movement animation complete
; input
;  * x - player index (0 = p1, 1 = p2)
auto_move_complete:
    lda #$08
    sta PLAYER_STATE,x
    rts

; checks for 10 extra lives cheat, i.e. right left down up a b
extra_lives_cheat_input_check:
    ldy EXTRA_LIVES_CHEAT_NUM_CORRECT ; load number of correct inputs to 10 extra lives cheat
    bmi @exit                         ; exit if failed inputting
    lda CONTROLLER_STATE_DIFF         ; load controller input
    and #$cf                          ; see which non select, start buttons are pressed
    beq @exit                         ; exit if none of A, B, up, down, left, nor right are pressed
    cmp extra_lives_code_tbl,y        ; compare input to next valid input for 10 lives cheat
    bne @mark_failure                 ; branch if incorrect to mark failure
    iny                               ; successfully entered next input, increment code offset
    sty EXTRA_LIVES_CHEAT_NUM_CORRECT
    lda extra_lives_code_tbl,y        ; load next correct input to see if player entered all input
    bne @exit                         ; exit if more of the cheat to enter
    lda #$01
    sta CHEAT_CODE_STATUS             ; successfully entered full cheat code, mark success

@mark_failure:
    lda #$ff
    sta EXTRA_LIVES_CHEAT_NUM_CORRECT ; mark failure entering code
                                      ; can retry by restarting or waiting for demo and pressing start again

@exit:
    rts

; right left down up a b
extra_lives_code_tbl:
    .byte $01,$02,$04,$08,$80,$40,$00

; (!UNUSED) level select menu
; show a level select menu flashing the level number
; read controller input and update level appropriately
level_select:
    jsr handle_level_select_input
    lda FRAME_COUNTER             ; load frame counter
    lsr
    lsr
    lsr
    lsr
    lda #$00                      ; randomly flash level number every 8 frames
    bcc @write_level_num
    lda CURRENT_LEVEL             ; load current level
    clc                           ; clear carry in preparation for addition
    adc #$02

; write level number to screen buffer
@write_level_num:
    ldx GRAPHICS_BUFFER_OFFSET
    sta CPU_GRAPHICS_BUFFER-2,x
    rts

; down goes to next level, up goes to previous level
handle_level_select_input:
    lda CONTROLLER_STATE_DIFF_B
    and #$0c                    ; check for up down d-pad input
    beq @check_for_start        ; branch if neither up nor down was pressed
    cmp #$08                    ; see if up d-pad pressed
    bne @down_pressed           ; branch if down pressed
    lda CURRENT_LEVEL           ; up pressed, load selected level
    beq @check_for_start        ; branch if level 1 and up pressed to do nothing
    dec CURRENT_LEVEL           ; up pressed, go to previous level
    bpl @check_for_start        ; always branch

@down_pressed:
    lda CURRENT_LEVEL    ; load current level
    cmp #$07
    beq @check_for_start ; branch if on last level
    inc CURRENT_LEVEL    ; down pressed, move to next level

@check_for_start:
    lda CONTROLLER_STATE_DIFF_B
    and #$10
    beq @exit                   ; branch if start button not pressed
    lda CURRENT_LEVEL           ; start button pressed, get ready to move to next game routine
    pha                         ; push level number to stack
    jsr clear_mid_high_ram
    pla                         ; pull level number from stack
    jsr init_player_for_level_a ; initialize level a, player state, number of lives, player count, player mode, etc.
    jmp increment_game_routine

@exit:
    rts

; output
;  * carry flag - set when last routine complete
run_intro_animation_routine:
  lda INTRO_ANIM_INDEX
  jsr run_routine_from_tbl_below

intro_animation_ptr_tbl:
    .addr intro_animation_00 ; initialize variables, set mirroring
    .addr intro_animation_01 ; flash left and right SUPER
    .addr intro_animation_02 ; animate revealing of C
    .addr intro_animation_03 ; wait for delay and then reveal text, e.g. PLAY SELECT, copyright, etc.

; initialize variables, set mirroring
intro_animation_00:
    ldy #$0f
    lda #$00

; clear intro animation bytes [$8f-$80]
@loop:
    sta INTRO_ANIM_INDEX,y
    dey
    bpl @loop
    lda #$00                      ; vertical mirroring
    sta NT_MIRRORING              ; set nametable mirroring (0: vertical; 1: horizontal)
    lda #$a9
    sta PPUCTRL_SETTINGS          ; enable nmi for vertical blank, 8x16 sprites, base nametable $2400
    sta INTRO_ANIM_PPUCTRL        ; set initial PPUCTRL for SUPER flashing scroll
    sta PAUSE_PALETTE_UPDATES     ; pause palette updates
    lda #$00
    sta X_SCROLL                  ; set PPU horizontal scroll to no scroll
    sta Y_SCROLL                  ; set PPU vertical scroll to top of nametables (no scroll)
    ldx #$02
    jsr write_graphic_data_to_ppu ; write graphic_data_01 (intro screen nametable tiles) to PPU

intro_animation_adv_routine:
    inc INTRO_ANIM_INDEX
    clc
    rts

; flash left and right SUPER
intro_animation_01:
    lda FRAME_COUNTER               ; load frame counter
    and #$01
    tax
    lda intro_anim_scroll_dir_tbl,x
    clc                             ; clear carry in preparation for addition
    adc INTRO_ANIM_X_SCROLL,x
    sta INTRO_ANIM_X_SCROLL,x
    ror
    eor intro_anim_scroll_dir_tbl,x
    bpl @continue
    lda INTRO_ANIM_PPUCTRL,x
    eor #$01
    sta INTRO_ANIM_PPUCTRL,x

@continue:
    lda INTRO_ANIM_PPUCTRL,x
    sta PPUCTRL_SETTINGS           ; set base nametable to either $2000 or $2400
    lda INTRO_ANIM_X_SCROLL,x
    sta X_SCROLL                   ; set PPU horizontal scroll
    bne intro_animation_exit
    jsr set_intro_screen_bg_sprite ; set the large sprite for the intro screen
    .ifdef Probotector
        lda #$38
    .else
        lda #$40
    .endif

intro_animation_set_delay_adv_routine:
    sta INTRO_ANIM_DELAY
    jmp intro_animation_adv_routine

intro_animation_exit:
    clc
    rts

; either scroll to the right or left by 2 pixels when animating SUPER
intro_anim_scroll_dir_tbl:
    .byte $02,$fe

; animate revealing of C
intro_animation_02:
    dec INTRO_ANIM_DELAY     ; wait for delay to elapse
                             ; before going to next animation frame
    bne intro_animation_exit
    lda #$08
    sta INTRO_ANIM_DELAY
    lda INTRO_SCREEN_PALETTE
    asl
    adc INTRO_SCREEN_PALETTE ; multiply by 3 since each entry is 3 palette bytes
    tay                      ; transfer to offset register
    ldx #$00

@palette_loop:
    lda intro_palette_tbl,y
    .ifdef Probotector
        sta PALETTE_CPU_BUFFER+1,x
        lda intro_palette_tbl2,y
        sta PALETTE_CPU_BUFFER+29,x
    .else
        sta PALETTE_CPU_BUFFER+13,x
        lda intro_palette_tbl2,y
        sta PALETTE_CPU_BUFFER+17,x
    .endif
    iny
    inx
    cpx #$03
    bcc @palette_loop
    jsr write_palette_to_graphics_buffer
    inc INTRO_SCREEN_PALETTE
    lda INTRO_SCREEN_PALETTE
    cmp #$03
    bcc intro_animation_exit
    .ifdef Probotector
        lda #$60                              ; finished animating appearing of logo
    .else
        lda #$70                              ; finished animating appearing of logo
    .endif
    jmp intro_animation_set_delay_adv_routine

intro_palette_tbl:
    .ifdef Probotector
        .byte COLOR_DARK_GRAY_00,COLOR_LT_GRAY_10,COLOR_BLACK_0f
        .byte COLOR_LT_GRAY_10,  COLOR_WHITE_20,  COLOR_DARK_RED_06
        .byte COLOR_WHITE_20,    COLOR_WHITE_30,  COLOR_DARK_RED_06
    .else
        .byte COLOR_DARK_ORANGE_07,COLOR_DARK_RED_06,COLOR_DARK_RED_06
        .byte COLOR_MED_ORANGE_17, COLOR_MED_RED_16, COLOR_DARK_RED_06
        .byte COLOR_LT_ORANGE_27,  COLOR_MED_RED_16, COLOR_DARK_RED_06
    .endif

intro_palette_tbl2:
    .ifdef Probotector
        .byte COLOR_BLACK_0f,COLOR_BLACK_0f,COLOR_BLACK_0f
        .byte COLOR_BLACK_0f,COLOR_BLACK_0f,COLOR_DARK_RED_06
        .byte COLOR_BLACK_0f,COLOR_BLACK_0f,COLOR_DARK_RED_06
    .else
        .byte COLOR_DARK_RED_06,COLOR_DARK_RED_06,COLOR_BLACK_0f
        .byte COLOR_MED_RED_16, COLOR_DARK_RED_06,COLOR_BLACK_0f
        .byte COLOR_MED_RED_16, COLOR_DARK_RED_06,COLOR_BLACK_0f
    .endif

; wait for delay and then reveal text, e.g. PLAY SELECT, copyright, etc.
; when done set carry indicating intro animation is complete
intro_animation_03:
    dec INTRO_ANIM_DELAY
    bne intro_animation_exit
    sec                      ; indicate all intro animation routines are completed
    rts

run_scroll_credits_routine:
    lda CREDITS_ROUTINE            ; load end credits routine
    jsr run_routine_from_tbl_below

scroll_credits_routine_ptr_tbl:
    .addr scroll_credits_routine_00
    .addr scroll_credits_routine_01
    .addr scroll_credits_routine_02
    .addr scroll_credits_routine_03
    .addr scroll_credits_routine_04

scroll_credits_routine_00:
    ldy #$0f
    lda #$00

; initialize scroll credits routine variables
; clears $70 to $7f inclusively
@loop:
    sta CREDITS_ROUTINE_DELAY,y
    dey
    bpl @loop
    jsr set_menu_pattern_tiles     ; set the tiles used between levels to show high score, level name, etc
    lda #$18                       ; set memory for changing tiles during interrupt (irq_handler_03_03)
    sta LEFT_BOTTOM_CHR_HALF_BANK  ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda #$69
    sta RIGHT_FOURTH_QTR_CHR_BANK  ; set bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    lda #$03
    sta IRQ_TYPE                   ; set irq routine type to irq_handler_03_ptr_tbl
                                   ; end credits
    jsr end_credits_set_irq_scroll
    lda #$01
    sta NT_MIRRORING               ; set horizontal nametable mirroring
    .ifdef Probotector
        lda #$01                   ; set delay before advancing routine
    .else
        lda #$02                   ; set delay before advancing routine
    .endif

scroll_credits_set_delay_adv:
    sta CREDITS_ROUTINE_DELAY

scroll_credits_adv_routine:
    inc CREDITS_ROUTINE ; increment scroll credits routine

scroll_credits_routine_exit:
    rts

scroll_credits_routine_01:
    jsr end_credits_set_irq_scroll
    jsr credits_scroll_scenery      ; scroll ending credits clouds and mountain (parallax scrolling)
    dec CREDITS_ROUTINE_DELAY
    bne scroll_credits_routine_exit
    lda #$00
    sta CREDITS_CURRENT_LINE
    sta Y_SCROLL_SPEED
    jmp scroll_credits_adv_routine

scroll_credits_routine_02:
    jsr end_credits_set_irq_scroll
    jsr credits_scroll_scenery         ; scroll ending credits clouds and mountain (parallax scrolling)
    jsr run_credits_helicopter_routine
    jsr end_credits_draw_line
    bcc @exit
    lda #$20
    jmp scroll_credits_set_delay_adv

@exit:
    rts

scroll_credits_routine_03:
    jsr end_credits_set_irq_scroll
    jsr credits_scroll_scenery         ; scroll ending credits clouds and mountain (parallax scrolling)
    jsr run_credits_helicopter_routine
    dec CREDITS_ROUTINE_DELAY
    bne scroll_credits_exit
    jmp scroll_credits_adv_routine

scroll_credits_routine_04:
    jsr end_credits_set_irq_scroll
    jsr credits_scroll_scenery         ; scroll ending credits clouds and mountain (parallax scrolling)
    jsr run_credits_helicopter_routine
    lda CONTROLLER_STATE_DIFF
    and #$10
    beq scroll_credits_exit            ; exit if start button not pressed
    inc X_DRAW_ROUTINE                 ; start button pressed

scroll_credits_exit:
    rts

run_credits_helicopter_routine:
    lda ENEMY_ROUTINE              ; load ending credits helicopter routine
    jsr run_routine_from_tbl_below

credits_helicopter_routine_ptr_tbl:
    .addr credits_helicopter_routine_00 ; set X position, advance routine
    .addr credits_helicopter_routine_01 ; set helicopter Y pos and sprite based on frame counter

; set X position, advance routine
credits_helicopter_routine_00:
    lda #$48
    sta ENEMY_X_POS   ; set helicopter X position
    inc ENEMY_ROUTINE
    rts

; set helicopter Y pos and sprite based on frame counter
credits_helicopter_routine_01:
    lda FRAME_COUNTER ; load frame counter
    lsr               ; change Y position every frame
    lda #$38          ; assume even frame, use #$38 for Y position
    bcc @continue     ; branch if even frame
    lda #$39          ; odd frame, use #$39 for Y position

@continue:
    sta ENEMY_Y_POS      ; set Y position
    lda FRAME_COUNTER    ; load frame counter
    lsr
    lsr                  ; change sprite rotors every 2 frames
    lda #$ba             ; sprite_ba (end credits helicopter)
    bcc @set_sprite_exit
    lda #$bb             ; sprite_bb (end credits helicopter)

@set_sprite_exit:
    sta ENEMY_SPRITE      ; set helicopter sprite
    lda #$03
    sta ENEMY_SPRITE_ATTR ; set helicopter palette
    rts

; scroll ending credits clouds and mountain (parallax scrolling)
credits_scroll_scenery:
    inc CREDITS_TOP_CLOUD_SCROLL_DELAY ; increment delay counter for top layer of clouds horizontal scroll
    lda CREDITS_TOP_CLOUD_SCROLL_DELAY ; load top layer of clouds horizontal scroll delay counter
    cmp #$04
    bcc @btm_cloud_scroll              ; continue to bottom clouds if delay hasn't elapsed
    lda #$00                           ; #$04 frames have elapsed, reset scroll delay counter
    sta CREDITS_TOP_CLOUD_SCROLL_DELAY
    inc X_SCROLL                       ; scroll top layer of clouds horizontally

@btm_cloud_scroll:
    inc CREDITS_BTM_CLOUD_SCROLL_DELAY ; increment delay counter for bottom layer of clouds horizontal scroll
    lda CREDITS_BTM_CLOUD_SCROLL_DELAY ; load bottom layer of clouds horizontal scroll delay counter
    cmp #$08
    bcc @mountain_scroll               ; continue to mountains if delay hasn't elapsed
    lda #$00                           ; #$08 frames have elapsed, reset scroll delay counter
    sta CREDITS_BTM_CLOUD_SCROLL_DELAY
    inc CREDITS_BTM_CLOUD_SCROLL       ; scroll bottom layer of clouds horizontally

; increment vertical scroll every 8 frames
@mountain_scroll:
    inc CREDITS_MTN_SCROLL_DELAY ; increment delay counter for mountains horizontal scroll
    lda CREDITS_MTN_SCROLL_DELAY ; load mountain horizontal scroll delay counter
    cmp #$16
    bcc @exit                    ; exit if delay hasn't elapsed
    lda #$00                     ; #$16 frames have elapsed, reset scroll delay counter
    sta CREDITS_MTN_SCROLL_DELAY
    inc CREDITS_MTN_SCROLL       ; scroll mountains horizontally

@exit:
    rts

end_credits_draw_line:
    .ifdef Probotector
        inc X_SCROLL_TILE_UPDATE
        lda X_SCROLL_TILE_UPDATE
        cmp #$03
        bcs @reset
        jmp credits_scroll_exit

    @reset:
        lda #$00
        sta X_SCROLL_TILE_UPDATE
    .else
        lda FRAME_COUNTER        ; load frame counter
        and #$03
        beq @irq_y_scroll        ; continue every 4 frames
        jmp credits_scroll_exit  ; exit if not 4th frame
    .endif

@irq_y_scroll:
    lda IRQ_Y_SCROLL
    clc
    adc #$01
    cmp #$f0
    bcc @continue
    adc #$0f

@continue:
    sta IRQ_Y_SCROLL
    and #$07
    bne credits_scroll_exit
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                   ; byte 0 = #$06 (block mode)
                               ; byte 1 and 2 will be PPU address
                               ; byte 3 is length, and bytes 4 to (byte 4 + length) are written to PPU
    sta CPU_GRAPHICS_BUFFER,x
    inx                        ; increment graphics buffer write offset
    lda IRQ_Y_SCROLL
    clc
    adc #$60
    bcs @handle_overflow
    cmp #$f0
    bcc @write_blank_line

@handle_overflow:
    adc #$0f

@write_blank_line:
    and #$f8
    sta $00
    lda #$09
    asl $00
    rol
    asl $00
    rol
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address high byte
    inx                       ; increment graphics buffer write offset
    lda $00
    clc
    adc #$08
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address low byte
    inx                       ; increment graphics buffer write offset
    lda #$10                  ; writing #$10 graphics bytes
    sta CPU_GRAPHICS_BUFFER,x ; write block size
    inx                       ; increment graphics buffer write offset
    lda #$00
    ldy #$10                  ; writing #$10 black tiles

; loop that writes the #$10 black tiles
; this is an empty row in the credits with no visible text
; if text is on the row, it'll overwrite the blank tile that was written
@blank_tile_loop:
    sta CPU_GRAPHICS_BUFFER,x
    inx                            ; increment graphics buffer write offset
    dey                            ; decrement loop counter
    bne @blank_tile_loop
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x      ; write end of data byte
    inx                            ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET     ; set new graphics buffer write offset
    inc Y_SCROLL_SPEED
    ldy CREDITS_CURRENT_LINE       ; load next credit to draw
    lda end_credits_text_tbl,y
    cmp Y_SCROLL_SPEED
    bne credits_scroll_exit        ; exit if not yet at scroll position for current line
    iny                            ; at draw position
    lda end_credits_text_tbl,y
    bmi credits_set_konami_palette ; when negative, update palette for konami logo
    txa                            ; transfer graphics buffer write offset to a
    sec
    sbc #$11                       ; go back to the beginning of the line
                                   ; this line was just cleared, now the letters will be written
    clc
    adc end_credits_text_tbl,y     ; add starting write position within the line
    tax                            ; set graphics buffer write offset
    iny                            ; increment credit table read offset
    lda end_credits_text_tbl,y     ; load number of letters in credit line
    sta $00                        ; set length of line
    iny                            ; increment credit table read offset

; write the contents of the credits line
@credits_character_loop:
    lda end_credits_text_tbl,y  ; load letter tile
    sta CPU_GRAPHICS_BUFFER,x   ; write credits letter
    inx                         ; increment graphics buffer write offset
    iny                         ; increment credit table read offset
    dec $00                     ; decrement loop counter
    bne @credits_character_loop
    sty CREDITS_CURRENT_LINE

credits_scroll_exit:
    clc
    rts

credits_set_konami_palette:
    cmp #$fe
    bne @exit
    lda #$10                                ; set palette for konami logo
    sta PALETTE_CPU_BUFFER+1
    jsr write_bg_palette_to_graphics_buffer
    inc CREDITS_CURRENT_LINE
    inc CREDITS_CURRENT_LINE
    clc
    rts

@exit:
    sec
    rts

; byte 0 - vertical scroll draw position
; byte 1 - starting write position from left of line
;          when negative, draw konami logo
; byte 2 - length of text
end_credits_text_tbl:
    .ifdef Probotector
        .byte $04,$05,$05
    .else
        .byte $02,$05,$05
    .endif
        .byte $1d,$1e,$0b,$10,$10                                             ; STAFF
    .ifdef Probotector
        .byte $0f,$03,$0a
    .else
        .byte $0a,$03,$0a
    .endif
    .byte $1a,$1c,$19,$11,$1c,$0b,$17,$17,$0f,$1c                             ; PROGRAMMER
    .ifdef Probotector
        .byte $13,$03,$09
    .else
        .byte $0e,$03,$09
    .endif
    .byte $1d,$26,$1f,$17,$0f,$24,$0b,$15,$13                                 ; S.UMEZAKI
    .ifdef Probotector
        .byte $1e,$01,$0e
    .else
        .byte $16,$01,$0e
    .endif
    .byte $11,$1c,$0b,$1a,$12,$13,$0d,$00,$0e,$0f,$1d,$13,$11,$18             ; GRAPHIC DESIGN
    .ifdef Probotector
        .byte $22,$04,$08
    .else
        .byte $1a,$04,$08
    .endif
    .byte $1d,$26,$17,$1f,$1c,$0b,$15,$13                                     ; S.MURAKI
    .ifdef Probotector
        .byte $2d,$02,$0c
    .else
        .byte $22,$02,$0c
    .endif
    .byte $1d,$19,$1f,$18,$0e,$00,$0e,$0f,$1d,$13,$11,$18                     ; SOUND DESIGN
    .ifdef Probotector
        .byte $31,$03,$09
    .else
        .byte $26,$03,$09
    .endif
    .byte $12,$26,$17,$0b,$0f,$24,$0b,$21,$0b                                 ; H.MAEZAWA
    ; !(OBS) Y.SAKAKURA is only credited in Probotector release
    .ifdef Probotector
        .byte $33,$03,$0a
        .byte $23,$26,$1d,$0b,$15,$0b,$15,$1f,$1c,$0b                         ; Y.SAKAKURA
        .byte $3e,$01,$0e
    .else
        .byte $2e,$01,$0e
    .endif
    .byte $1d,$1a,$0f,$0d,$13,$0b,$16,$00,$1e,$12,$0b,$18,$15,$1d             ; SPECIAL THANKS
    .ifdef Probotector
        .byte $42,$03,$0a
    .else
        .byte $32,$03,$0a
    .endif
    .byte $15,$26,$1d,$12,$13,$17,$19,$13,$0e,$0f                             ; K.SHIMOIDE
    .ifdef Probotector
        .byte $44,$02,$0c
    .else
        .byte $35,$02,$0c
    .endif
    .byte $1d,$1f,$1a,$0f,$1c,$00,$0d,$00,$1e,$0f,$0b,$17                     ; SUPER C TEAM
    .ifdef Probotector
        .byte $4f,$02,$0b
    .else
        .byte $3d,$02,$0b
    .endif
    .byte $0e,$13,$1c,$0f,$0d,$1e,$0f,$0e,$00,$0c,$23                         ; DIRECTED BY
    .ifdef Probotector
        .byte $53,$02,$0c
    .else
        .byte $41,$02,$0c
    .endif
    .byte $1f,$17,$0f,$0d,$12,$0b,$18,$00,$1e,$0f,$0b,$17                     ; UMECHAN TEAM
    .ifdef Probotector
        .byte $5f,$03,$0c
        .byte $cb,$cc,$cd,$ce,$cf,$f6,$f7,$f8,$cf,$f9,$fa,$fb
        .byte $60,$02,$0e
        .byte $30,$db,$dc,$dd,$de,$df,$eb,$ec,$ed,$df,$ee,$ef,$fc,$79,$67,$ff
    .else
        .byte $50,$fe,$51
        .byte $03,$0b,$2e,$2f,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$52,$02,$0c ; konami logo
        .byte $30,$31,$32,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$53,$02,$02,$33
        .byte $34,$59,$ff
    .endif

; called from various scroll credits routines
end_credits_set_irq_scroll:
    lda #$00
    sta IRQ_X_SCROLL        ; initialize X_SCROLL used after the 1st IRQ, but before the 3rd IRQ
    .ifdef Probotector
        lda #$2b
    .else
        lda #$38
    .endif
    sta SCANLINE_IRQ_1      ; set first scanline IRQ (irq_handler_03_00)
    lda IRQ_Y_SCROLL
    and #$07
    sta SCANLINE_IRQ_3_DIFF
    lda #$27
    sec
    sbc SCANLINE_IRQ_3_DIFF
    sta SCANLINE_IRQ_2_DIFF ; set second scanline IRQ (irq_handler_03_01)
    inc SCANLINE_IRQ_3_DIFF
    inc SCANLINE_IRQ_3_DIFF
    inc SCANLINE_IRQ_3_DIFF
    ldy #$0b
    sty IRQ_PPUADDR+1
    lda IRQ_Y_SCROLL
    and #$f8
    asl
    rol IRQ_PPUADDR+1
    asl
    rol IRQ_PPUADDR+1
    sta IRQ_PPUADDR         ; read by init_scanline_irq
    rts

sound_32_slot_00:
    .incbin "assets/audio_data/sound_32_slot_00_0.bin"

sound_32_slot_00_1:
    .incbin "assets/audio_data/sound_32_slot_00_1.bin"
    .addr sound_32_slot_00_1

sound_32_slot_01:
    .incbin "assets/audio_data/sound_32_slot_01_0.bin"

sound_32_slot_01_1:
    .incbin "assets/audio_data/sound_32_slot_01_1.bin"
    .addr sound_32_slot_01_1

sound_32_slot_02:
    .incbin "assets/audio_data/sound_32_slot_02_0.bin"

sound_32_slot_02_1:
    .incbin "assets/audio_data/sound_32_slot_02_1.bin"
    .addr sound_32_slot_02_1

sound_32_slot_03:
    .incbin "assets/audio_data/sound_32_slot_03_0.bin"

sound_32_slot_03_1:
    .incbin "assets/audio_data/sound_32_slot_03_1.bin"
    .addr sound_32_slot_03_1

sound_36_slot_00:
    .incbin "assets/audio_data/sound_36_slot_00.bin"

sound_36_slot_01:
    .incbin "assets/audio_data/sound_36_slot_01.bin"

sound_36_slot_02:
    .incbin "assets/audio_data/sound_36_slot_02.bin"

sound_36_slot_03:
    .incbin "assets/audio_data/sound_36_slot_03_0.bin"
    .addr sound_36_slot_03_4
    .byte $fd
    .addr sound_36_slot_03_4
    .byte $fd
    .addr sound_36_slot_03_4
    .incbin "assets/audio_data/sound_36_slot_03_3.bin"

sound_36_slot_03_4:
    .incbin "assets/audio_data/sound_36_slot_03_4.bin"

level_2_screen_layout_tbl:
    .ifdef Probotector
        .byte $04             ; LEVEL_WIDTH
        .byte $0d             ; LEVEL_HEIGHT
        .byte $0c,$0c,$0c,$0c
        .byte $0f,$0f,$0b,$0d
        .byte $0a,$0e,$0e,$0e
        .byte $09,$09,$09,$09
        .byte $08,$08,$08,$08
        .byte $07,$07,$07,$07
        .byte $06,$06,$06,$06
        .byte $05,$05,$05,$05
        .byte $04,$04,$04,$04
        .byte $03,$03,$03,$03
        .byte $02,$02,$02,$02
        .byte $01,$01,$01,$01
        .byte $00,$00,$00,$00

    level_2_supertiles_screen_ptr_table:
        .addr level_2_supertiles_screen_01
        .addr level_2_supertiles_screen_02
        .addr level_2_supertiles_screen_03
        .addr level_2_supertiles_screen_04
        .addr level_2_supertiles_screen_05
        .addr level_2_supertiles_screen_06
        .addr level_2_supertiles_screen_07
        .addr level_2_supertiles_screen_08
        .addr level_2_supertiles_screen_09
        .addr level_2_supertiles_screen_0a
        .addr level_2_supertiles_screen_0b
        .addr level_2_supertiles_screen_0c
        .addr level_2_supertiles_screen_0d
        .addr level_2_supertiles_screen_0e
        .addr level_2_supertiles_screen_0f
        .addr level_2_supertiles_screen_10
    .else
        .byte $02                          ; LEVEL_WIDTH
        .byte $0d                          ; LEVEL_HEIGHT
        .byte $0d,$00
        .byte $0c,$0e
        .byte $0b,$00
        .byte $0a,$00
        .byte $09,$00
        .byte $08,$00
        .byte $07,$00
        .byte $06,$00
        .byte $05,$00
        .byte $04,$00
        .byte $03,$00
        .byte $02,$00
        .byte $01,$0f

    level_2_supertiles_screen_ptr_table:
        .addr level_2_supertiles_screen_00
        .addr level_2_supertiles_screen_01
        .addr level_2_supertiles_screen_02
        .addr level_2_supertiles_screen_03
        .addr level_2_supertiles_screen_04
        .addr level_2_supertiles_screen_05
        .addr level_2_supertiles_screen_06
        .addr level_2_supertiles_screen_07
        .addr level_2_supertiles_screen_08
        .addr level_2_supertiles_screen_09
        .addr level_2_supertiles_screen_0a
        .addr level_2_supertiles_screen_0b
        .addr level_2_supertiles_screen_0c
        .addr level_2_supertiles_screen_0d
        .addr level_2_supertiles_screen_0e
    .endif

level_2_supertiles_screen_00:
    .ifdef Superc
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .endif

level_2_supertiles_screen_01:
    .byte $01,$01,$02,$1f,$10,$11,$12,$12,$02,$01,$01,$1f,$14,$15,$16,$0b
    .byte $01,$01,$01,$1f,$18,$19,$1a,$08,$01,$01,$02,$1f,$1c,$1d,$1e,$0f
    .byte $0c,$01,$01,$01,$06,$07,$07,$00,$04,$05,$01,$02,$01,$06,$07,$00
    .byte $08,$09,$0a,$01,$01,$02,$06,$03,$08,$0d,$0e,$01,$01,$01,$01,$06

level_2_supertiles_screen_02:
    .byte $31,$32,$30,$30,$30,$30,$30,$23,$20,$21,$22,$22,$22,$36,$3e,$33
    .byte $24,$25,$26,$27,$34,$35,$3e,$30,$28,$29,$2a,$2b,$38,$39,$30,$23
    .byte $2c,$28,$2e,$2f,$3c,$3d,$37,$33,$13,$17,$1b,$3a,$3a,$3a,$3b,$37
    .byte $3a,$3a,$3a,$3a,$3a,$3a,$3a,$3b,$30,$30,$30,$30,$22,$22,$22,$22

level_2_supertiles_screen_03:
    .byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

level_2_supertiles_screen_04:
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$41,$42,$43,$44,$40,$40
    .byte $40,$40,$45,$46,$47,$48,$40,$40,$40,$40,$49,$4a,$4a,$4b,$40,$40

level_2_supertiles_screen_05:
    .byte $5c,$54,$55,$40,$40,$56,$57,$51,$5d,$58,$59,$40,$40,$5a,$5b,$52
    .byte $4c,$4d,$4e,$4f,$40,$5e,$5f,$4c,$64,$64,$64,$64,$4f,$63,$64,$64
    .byte $64,$64,$64,$64,$64,$4f,$63,$64,$41,$42,$43,$44,$41,$42,$43,$44
    .byte $45,$46,$47,$48,$45,$46,$47,$48,$49,$4a,$4a,$4b,$49,$4a,$4a,$4b

level_2_supertiles_screen_06:
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $61,$60,$40,$40,$40,$40,$62,$61,$6f,$50,$40,$40,$40,$40,$53,$6f

level_2_supertiles_screen_07:
    .byte $40,$40,$90,$91,$6b,$6f,$6f,$6f,$40,$40,$94,$95,$78,$79,$80,$79
    .byte $40,$40,$40,$40,$7c,$7d,$81,$82,$40,$40,$40,$40,$7e,$7f,$4c,$5f
    .byte $90,$91,$40,$40,$63,$64,$64,$64,$94,$95,$40,$40,$40,$63,$64,$64
    .byte $40,$40,$40,$40,$40,$40,$63,$64,$40,$40,$40,$40,$40,$40,$40,$40

level_2_supertiles_screen_08:
    .byte $5c,$54,$55,$40,$40,$56,$57,$51,$5d,$58,$59,$40,$40,$5a,$5b,$52
    .byte $4c,$4d,$4e,$4f,$40,$5e,$5f,$4c,$64,$64,$64,$64,$4f,$63,$64,$64
    .byte $64,$64,$64,$64,$64,$4f,$63,$64,$40,$40,$40,$40,$70,$71,$71,$71
    .byte $40,$40,$40,$40,$6e,$6f,$6f,$6f,$40,$40,$40,$40,$70,$71,$71,$71

level_2_supertiles_screen_09:
    .byte $a4,$a4,$72,$c7,$c7,$73,$a4,$a4,$a4,$a4,$a4,$72,$73,$a4,$a4,$a4
    .byte $a4,$a4,$a4,$72,$73,$a4,$a4,$a4,$a4,$a4,$a4,$72,$73,$a4,$a4,$a4
    .byte $a4,$a4,$a4,$72,$73,$a4,$a4,$a4,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $61,$60,$40,$40,$40,$40,$62,$61,$6f,$50,$40,$40,$40,$40,$53,$6f

level_2_supertiles_screen_0a:
    .byte $90,$91,$40,$40,$40,$40,$90,$91,$94,$95,$40,$84,$85,$40,$94,$95
    .byte $76,$76,$76,$74,$75,$83,$77,$77,$7a,$7a,$7a,$74,$75,$ab,$7b,$7b
    .byte $a4,$a4,$a4,$74,$75,$a4,$a4,$a4,$a4,$a4,$9c,$9d,$9e,$9f,$a4,$a4
    .byte $a4,$a4,$99,$9b,$9b,$9a,$a4,$a4,$a4,$a4,$99,$9b,$9b,$75,$a4,$a4

level_2_supertiles_screen_0b:
    .byte $86,$87,$88,$89,$ac,$ad,$ae,$86,$8a,$8b,$8c,$8d,$af,$b0,$b1,$b2
    .byte $8e,$8f,$a0,$c1,$c2,$b3,$b4,$b5,$8e,$a1,$a2,$64,$64,$b6,$b7,$ae
    .byte $a3,$a1,$a2,$64,$40,$b8,$b9,$ba,$a5,$a6,$a2,$64,$40,$b8,$bb,$bc
    .byte $a7,$a7,$a8,$64,$40,$bd,$be,$bf,$a9,$a9,$aa,$64,$40,$c0,$a9,$a9

level_2_supertiles_screen_0c:
    .byte $d2,$d2,$d3,$40,$40,$d1,$d2,$d2,$d0,$d0,$f0,$e2,$e3,$f1,$d0,$d0
    .byte $d0,$d0,$e4,$e5,$e6,$e7,$d0,$d0,$d0,$d0,$e8,$e9,$ea,$eb,$d0,$d0
    .byte $d0,$d0,$ec,$ed,$ee,$ef,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
    .byte $ca,$c8,$d5,$ce,$ce,$ce,$c9,$ca,$86,$cc,$cb,$64,$64,$cf,$cd,$86

level_2_supertiles_screen_0d:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$86,$86,$86,$86,$86,$86,$86,$86
    .byte $86,$86,$86,$86,$86,$86,$86,$86,$86,$d8,$d9,$d6,$d6,$da,$db,$86
    .byte $d7,$dc,$dd,$00,$00,$de,$df,$d7,$d2,$d2,$e0,$64,$64,$e1,$d2,$d2

level_2_supertiles_screen_0e:
    .byte $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$f0,$e2,$e3,$f1,$d0,$d0
    .byte $d0,$d0,$e4,$e5,$e6,$e7,$d0,$d0,$d0,$d0,$e8,$e9,$ea,$eb,$d0,$d0
    .byte $d0,$d0,$ec,$ed,$ee,$ef,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
    .byte $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0

    .ifdef Probotector
        level_2_supertiles_screen_0f:
            .byte $86,$87,$88,$c3,$c4,$ad,$ae,$86,$8a,$8b,$8c,$6d,$c6,$b0,$b1,$b2
            .byte $8e,$8f,$a0,$6c,$c5,$b3,$b4,$b5,$8e,$a1,$a2,$64,$64,$b6,$b7,$ae
            .byte $a3,$a1,$a2,$64,$40,$b8,$b9,$ba,$a5,$a6,$a2,$64,$40,$b8,$bb,$bc
            .byte $a7,$a7,$a8,$64,$40,$bd,$be,$bf,$a9,$a9,$aa,$64,$40,$c0,$a9,$a9

        level_2_supertiles_screen_10:
            .byte $d2,$d2,$d3,$40,$40,$d1,$d2,$d2,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
            .byte $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
            .byte $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
            .byte $ca,$c8,$d5,$ce,$ce,$ce,$c9,$ca,$86,$cc,$cb,$64,$64,$cf,$cd,$86
    .endif

level_2_supertile_data:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $05,$06,$06,$5d,$07,$08,$5e,$5f,$07,$5e,$08,$5f,$60,$65,$65,$55
    .byte $05,$66,$14,$5d,$67,$68,$69,$6a,$15,$6b,$16,$6c,$60,$6d,$6e,$55
    .byte $0f,$18,$00,$00,$10,$00,$00,$00,$10,$00,$00,$00,$71,$00,$00,$00
    .byte $79,$82,$3f,$00,$83,$83,$37,$82,$40,$40,$49,$37,$40,$40,$40,$49
    .byte $0f,$12,$12,$5d,$00,$11,$5e,$5f,$00,$00,$11,$5f,$82,$00,$00,$55
    .byte $17,$18,$18,$00,$07,$19,$00,$00,$07,$5e,$19,$00,$60,$65,$65,$6f
    .byte $0f,$18,$18,$00,$10,$00,$00,$00,$10,$00,$00,$00,$71,$00,$00,$00
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $49,$00,$00,$55,$4a,$82,$00,$00,$4a,$37,$00,$00,$40,$37,$82,$00
    .byte $0f,$06,$06,$5d,$11,$08,$5e,$5f,$00,$5e,$08,$5f,$00,$11,$65,$55
    .byte $50,$50,$50,$50,$40,$40,$40,$40,$40,$40,$40,$40,$e4,$40,$40,$40
    .byte $05,$06,$06,$5d,$07,$08,$5e,$5f,$07,$5e,$08,$5f,$55,$55,$65,$55
    .byte $40,$49,$79,$00,$40,$40,$37,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$06,$5d,$00,$00,$5e,$5f,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $4f,$e4,$40,$40,$4d,$4f,$40,$40,$35,$4e,$4c,$4c,$00,$00,$35,$4e
    .byte $4d,$e5,$33,$e5,$36,$40,$40,$40,$36,$40,$40,$40,$34,$4c,$40,$40
    .byte $33,$e5,$33,$e5,$40,$40,$40,$40,$40,$40,$40,$40,$4c,$e4,$40,$40
    .byte $33,$e5,$33,$e5,$40,$40,$40,$40,$40,$40,$40,$40,$4c,$40,$40,$40
    .byte $41,$41,$41,$41,$eb,$eb,$eb,$eb,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $34,$e7,$37,$e7,$e8,$35,$e6,$e6,$3a,$38,$39,$39,$3a,$41,$41,$41
    .byte $37,$e7,$37,$e7,$35,$35,$e6,$35,$38,$39,$39,$38,$41,$41,$41,$41
    .byte $37,$7b,$50,$50,$4e,$4e,$40,$40,$3f,$7a,$40,$40,$7e,$35,$4f,$e4
    .byte $41,$41,$47,$ef,$eb,$eb,$eb,$d6,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $3a,$41,$41,$41,$3b,$41,$41,$41,$3b,$41,$41,$41,$3b,$41,$41,$41
    .byte $41,$41,$41,$3d,$41,$41,$41,$e9,$41,$41,$3d,$3e,$41,$41,$e9,$41
    .byte $30,$00,$4e,$32,$81,$00,$7a,$4c,$31,$00,$35,$4d,$41,$7f,$00,$4f
    .byte $41,$41,$48,$90,$eb,$eb,$eb,$91,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $3c,$41,$41,$41,$3c,$41,$41,$41,$3c,$41,$41,$41,$ea,$eb,$eb,$eb
    .byte $41,$3d,$3e,$41,$41,$e9,$41,$41,$3d,$3e,$41,$41,$ec,$eb,$eb,$eb
    .byte $41,$80,$3f,$4e,$41,$81,$00,$35,$41,$31,$3f,$00,$eb,$eb,$7d,$00
    .byte $05,$06,$06,$5d,$07,$08,$5e,$55,$07,$5e,$08,$55,$60,$65,$65,$55
    .byte $40,$40,$4a,$37,$40,$40,$40,$37,$42,$43,$42,$43,$40,$40,$40,$40
    .byte $00,$00,$08,$5f,$82,$00,$11,$55,$42,$ed,$4b,$43,$40,$ee,$84,$40
    .byte $07,$5e,$08,$5f,$65,$65,$65,$55,$43,$42,$43,$42,$40,$40,$40,$40
    .byte $07,$5e,$08,$5f,$60,$65,$65,$55,$05,$66,$14,$5d,$67,$68,$69,$6a
    .byte $40,$40,$40,$40,$40,$40,$4c,$e4,$44,$45,$44,$44,$41,$41,$41,$41
    .byte $40,$ee,$5c,$85,$40,$ee,$d9,$84,$44,$d7,$46,$5c,$41,$41,$d8,$d9
    .byte $40,$40,$40,$40,$4c,$40,$40,$40,$51,$45,$45,$44,$8a,$52,$41,$41
    .byte $40,$40,$40,$40,$40,$40,$40,$4c,$44,$45,$44,$8b,$41,$41,$41,$8c
    .byte $41,$41,$41,$41,$ef,$41,$41,$41,$47,$ef,$41,$41,$41,$47,$ef,$41
    .byte $41,$41,$48,$46,$41,$41,$41,$d8,$41,$41,$41,$48,$41,$41,$41,$41
    .byte $53,$54,$52,$41,$d9,$8a,$00,$52,$46,$53,$54,$00,$d8,$d9,$8a,$00
    .byte $41,$41,$41,$8c,$41,$41,$41,$8c,$52,$41,$41,$8c,$00,$52,$41,$8c
    .byte $41,$41,$47,$ef,$41,$41,$41,$47,$41,$41,$41,$41,$41,$41,$41,$41
    .byte $41,$41,$41,$41,$ef,$41,$41,$41,$47,$ef,$41,$41,$41,$47,$ef,$41
    .byte $48,$46,$53,$54,$41,$d8,$d9,$8a,$41,$48,$46,$8e,$41,$41,$d8,$8f
    .byte $3f,$00,$52,$8c,$00,$00,$00,$8d,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $07,$5e,$08,$5f,$60,$65,$65,$55,$05,$06,$06,$5d,$07,$08,$5e,$5f
    .byte $3f,$00,$11,$5d,$37,$82,$00,$11,$40,$49,$79,$00,$40,$40,$49,$82
    .byte $07,$5e,$08,$5f,$55,$65,$65,$55,$11,$12,$06,$5d,$00,$11,$5e,$55
    .byte $15,$6b,$16,$6c,$60,$6d,$6e,$55,$05,$06,$06,$5d,$07,$08,$5e,$5f
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$44,$45,$45,$45,$41,$41,$41,$41
    .byte $40,$40,$40,$88,$40,$40,$86,$89,$44,$45,$57,$72,$41,$41,$73,$75
    .byte $07,$5e,$08,$5f,$65,$65,$65,$55,$43,$42,$42,$87,$40,$40,$40,$88
    .byte $07,$5e,$08,$5f,$60,$65,$65,$55,$0f,$12,$06,$5d,$00,$11,$5e,$5f
    .byte $41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41
    .byte $57,$57,$56,$5f,$57,$73,$74,$55,$57,$56,$12,$5d,$73,$75,$5e,$5f
    .byte $10,$00,$00,$00,$71,$00,$00,$00,$0f,$18,$18,$00,$00,$00,$00,$00
    .byte $00,$00,$11,$5f,$00,$00,$00,$11,$0f,$18,$18,$00,$10,$00,$00,$00
    .byte $41,$41,$41,$41,$eb,$eb,$eb,$eb,$0f,$18,$18,$00,$10,$00,$00,$00
    .byte $7c,$00,$11,$5f,$76,$00,$00,$77,$0f,$18,$18,$00,$10,$00,$00,$00
    .byte $08,$5e,$08,$5f,$65,$65,$65,$55,$08,$06,$06,$5d,$08,$08,$5e,$5f
    .byte $13,$13,$13,$13,$78,$78,$78,$78,$07,$5e,$08,$5f,$60,$65,$65,$55
    .byte $01,$02,$02,$61,$62,$63,$63,$64,$01,$02,$02,$61,$62,$63,$63,$64
    .byte $01,$02,$2a,$e3,$62,$63,$a7,$a8,$01,$02,$1b,$1c,$62,$63,$ae,$af
    .byte $2b,$2a,$1a,$be,$a8,$a9,$bf,$c0,$1c,$ac,$23,$c3,$af,$c5,$c6,$c7
    .byte $22,$a6,$2a,$e3,$c1,$c2,$aa,$a8,$24,$c4,$1d,$1c,$c8,$c9,$ca,$af
    .byte $2b,$2a,$02,$61,$a8,$ab,$63,$64,$1c,$ad,$03,$61,$af,$b0,$00,$64
    .byte $01,$02,$1e,$1f,$62,$63,$b2,$b3,$01,$02,$20,$21,$62,$63,$b8,$b9
    .byte $1f,$cb,$25,$cc,$b3,$b4,$ce,$cf,$21,$21,$28,$d2,$ba,$bb,$ba,$bb ; #$46 - overhead tank extended cannon
    .byte $26,$cd,$27,$1f,$d0,$d1,$b5,$b3,$29,$d3,$21,$21,$ba,$bb,$ba,$bb ; #$47 - overhead tank extended cannon
    .byte $1f,$b1,$03,$61,$b3,$b6,$00,$64,$21,$b7,$03,$61,$bc,$bd,$00,$64
    .byte $01,$02,$09,$00,$62,$63,$63,$64,$01,$02,$02,$61,$62,$63,$63,$64
    .byte $00,$00,$00,$00,$62,$63,$63,$64,$01,$02,$02,$61,$62,$63,$63,$64
    .byte $00,$03,$03,$61,$62,$63,$63,$64,$01,$02,$02,$61,$62,$63,$63,$64
    .byte $41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$eb,$eb,$eb,$eb
    .byte $47,$ef,$41,$41,$41,$47,$ef,$41,$41,$41,$47,$ef,$eb,$eb,$eb,$d6
    .byte $41,$48,$46,$f2,$41,$41,$d8,$f3,$41,$41,$48,$f4,$eb,$eb,$eb,$f5
    .byte $0e,$02,$02,$61,$0c,$f6,$63,$64,$0b,$03,$04,$61,$0c,$00,$00,$e2
    .byte $40,$ee,$5c,$59,$40,$ee,$d9,$f0,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $44,$45,$44,$45,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$e1
    .byte $41,$41,$e1,$fd,$41,$e1,$fd,$41,$e1,$fd,$41,$41,$fd,$41,$41,$41
    .byte $5a,$da,$de,$40,$f7,$f9,$de,$40,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $45,$d7,$46,$5c,$41,$41,$d8,$d9,$41,$41,$48,$46,$41,$41,$41,$d8
    .byte $58,$02,$02,$61,$f1,$63,$63,$64,$5c,$58,$02,$61,$d9,$f1,$63,$64
    .byte $01,$02,$d5,$5b,$62,$63,$63,$f8,$01,$02,$5b,$da,$62,$63,$f8,$f9
    .byte $da,$dc,$df,$45,$f9,$fc,$41,$41,$dc,$e0,$41,$41,$fc,$41,$41,$41
    .byte $41,$41,$41,$48,$41,$41,$41,$41,$41,$41,$41,$41,$ef,$41,$41,$41
    .byte $46,$5c,$58,$61,$d8,$d9,$f1,$64,$48,$46,$5c,$59,$41,$d8,$d9,$f0
    .byte $01,$5b,$da,$dc,$62,$f8,$f9,$fc,$5a,$da,$dc,$e0,$f7,$f9,$fc,$41
    .byte $e0,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$e1
    .byte $45,$44,$45,$44,$41,$41,$41,$41,$41,$41,$41,$41,$ef,$41,$41,$41
    .byte $47,$ef,$41,$41,$41,$47,$ef,$41,$41,$41,$47,$ef,$41,$41,$41,$47
    .byte $db,$dc,$e0,$41,$fa,$fc,$41,$41,$fb,$e0,$41,$41,$fe,$eb,$eb,$eb
    .byte $41,$41,$e1,$fd,$41,$e1,$fd,$41,$e1,$fd,$41,$41,$ff,$eb,$eb,$eb
    .byte $01,$02,$02,$61,$62,$63,$63,$64,$43,$ed,$58,$61,$40,$ee,$f1,$64
    .byte $01,$02,$02,$61,$62,$63,$63,$64,$43,$42,$43,$42,$40,$40,$40,$40
    .byte $01,$02,$02,$61,$62,$63,$63,$64,$d4,$5b,$dd,$42,$62,$f8,$de,$40
    .byte $0d,$03,$03,$03,$62,$09,$00,$00,$01,$02,$0a,$03,$62,$63,$63,$09
    .byte $0b,$03,$03,$03,$0c,$00,$00,$00,$0b,$03,$03,$03,$0c,$00,$00,$00
    .byte $2b,$2a,$2a,$e3,$a8,$a8,$a8,$a8,$1c,$1c,$9a,$9b,$af,$af,$9e,$9f ; #$65 - overhead tank soldier destroyed top left
    .byte $2b,$2a,$2a,$e3,$a8,$a8,$a8,$a8,$9c,$9d,$1c,$1c,$a0,$a1,$af,$af ; #$66 - overhead tank soldier destroyed top right
    .byte $1f,$1f,$a2,$a3,$b3,$b3,$b3,$b3,$21,$21,$21,$21,$ba,$bb,$ba,$bb ; #$67 - overhead tank soldier destroyed bottom left
    .byte $a4,$a5,$1f,$1f,$b3,$b3,$b3,$b3,$21,$21,$21,$21,$ba,$bb,$ba,$bb ; #$68 - overhead tank soldier destroyed bottom right
    .byte $1f,$cb,$25,$cc,$b3,$b4,$ce,$cf,$21,$21,$2c,$2d,$ba,$bb,$ba,$bb ; #$69 - overhead tank solder retracted cannon
    .byte $26,$cd,$27,$1f,$d0,$d1,$b5,$b3,$2e,$2f,$21,$21,$ba,$bb,$ba,$bb ; #$6a - overhead tank solder extended cannon
    .byte $51,$40,$40,$40,$51,$40,$40,$40,$36,$40,$40,$40,$34,$4c,$40,$40
    .byte $0b,$03,$03,$03,$0c,$00,$00,$00,$70,$03,$03,$03,$71,$00,$00,$00
    .byte $70,$03,$03,$03,$71,$00,$00,$00,$72,$03,$03,$03,$00,$00,$00,$00
    .byte $51,$40,$40,$40,$51,$40,$40,$40,$51,$40,$40,$40,$36,$40,$40,$40
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$4c,$e4,$40,$40
    .byte $80,$e5,$e5,$e5,$51,$40,$40,$40,$51,$40,$40,$40,$51,$40,$40,$40
    .byte $e5,$e5,$e5,$e5,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $05,$06,$06,$06,$40,$40,$40,$40,$07,$40,$07,$40,$52,$53,$53,$53
    .byte $06,$06,$06,$c2,$40,$40,$40,$c3,$07,$40,$07,$c3,$53,$53,$53,$c4
    .byte $07,$40,$07,$40,$52,$53,$53,$53,$05,$06,$06,$06,$40,$40,$40,$40
    .byte $07,$40,$07,$c3,$53,$53,$53,$c4,$06,$06,$06,$c2,$40,$40,$40,$c3
    .byte $24,$24,$24,$24,$7d,$7d,$7d,$7d,$21,$20,$1f,$83,$83,$83,$20,$22
    .byte $24,$24,$24,$24,$7d,$7d,$7d,$7d,$1b,$85,$1c,$81,$1d,$1c,$1b,$1b
    .byte $34,$e7,$37,$e7,$e8,$35,$e6,$e6,$3a,$38,$39,$39,$3a,$41,$41,$41
    .byte $37,$e7,$37,$e7,$35,$35,$e6,$35,$38,$39,$39,$38,$41,$41,$41,$41
    .byte $22,$22,$20,$84,$84,$23,$22,$84,$23,$84,$23,$00,$00,$00,$00,$23
    .byte $1e,$1c,$1d,$1d,$1e,$82,$82,$1e,$30,$82,$1e,$82,$82,$00,$00,$00
    .byte $3a,$41,$41,$41,$3b,$41,$41,$41,$3b,$41,$41,$41,$3b,$41,$41,$41
    .byte $41,$41,$41,$3d,$41,$41,$41,$e9,$41,$41,$3d,$3e,$41,$41,$e9,$41
    .byte $3c,$41,$41,$41,$3c,$41,$41,$41,$3c,$41,$41,$41,$ea,$eb,$eb,$eb
    .byte $41,$3d,$3e,$41,$41,$e9,$41,$41,$3d,$3e,$41,$41,$ec,$eb,$eb,$eb
    .byte $37,$e7,$37,$e7,$35,$35,$e6,$35,$3f,$00,$39,$38,$e9,$41,$41,$41
    .byte $3e,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41
    .byte $41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$e1
    .byte $25,$24,$24,$24,$00,$54,$7d,$7d,$30,$00,$1d,$1c,$00,$00,$82,$1d
    .byte $01,$02,$02,$61,$62,$63,$63,$64,$05,$06,$06,$06,$40,$40,$40,$40
    .byte $01,$02,$02,$61,$62,$63,$63,$64,$06,$06,$06,$c2,$40,$40,$40,$c3
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $40,$40,$40,$7b,$40,$40,$40,$7b,$40,$40,$40,$7b,$40,$40,$40,$7b
    .byte $41,$49,$3f,$00,$65,$65,$65,$65,$68,$41,$41,$41,$6c,$6a,$41,$41
    .byte $3f,$00,$3f,$00,$65,$65,$65,$6b,$41,$41,$41,$6d,$41,$41,$41,$6d
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$69,$68,$40,$40,$69,$6c
    .byte $40,$40,$40,$7b,$40,$40,$40,$7b,$41,$41,$41,$4a,$6a,$41,$41,$26
    .byte $41,$49,$6a,$41,$41,$6c,$00,$6a,$41,$41,$49,$00,$41,$41,$6c,$00
    .byte $41,$41,$41,$6d,$41,$41,$41,$6d,$6a,$41,$41,$6d,$00,$6a,$41,$6d
    .byte $40,$40,$69,$41,$40,$40,$69,$41,$40,$40,$69,$41,$40,$40,$69,$41
    .byte $49,$6a,$41,$41,$6c,$00,$6a,$41,$41,$49,$3f,$6a,$41,$76,$00,$00
    .byte $1a,$c1,$10,$10,$12,$87,$88,$89,$12,$87,$13,$8d,$12,$87,$8f,$be
    .byte $10,$10,$11,$86,$8a,$8b,$15,$15,$14,$8e,$15,$15,$bf,$91,$15,$15
    .byte $88,$89,$8a,$8b,$9d,$9e,$14,$8e,$9f,$a0,$90,$91,$a1,$a2,$93,$94 ; #$92 - overhead rotating turret left (9 o'clock)
    .byte $88,$89,$8a,$8b,$a3,$a4,$14,$8e,$a5,$a6,$90,$91,$a7,$a8,$93,$94 ; #$93 - overhead rotating turret down left (7:30 o'clock)
    .byte $12,$87,$16,$c0,$12,$95,$96,$96,$12,$98,$18,$19,$99,$9a,$9a,$9b
    .byte $17,$94,$15,$15,$96,$96,$97,$15,$19,$18,$2e,$15,$9b,$9a,$9a,$9c
    .byte $88,$89,$8a,$8b,$8c,$8d,$a9,$aa,$8f,$14,$ab,$ad,$16,$92,$ac,$ae ; #$96 - overhead rotating turret down right (4:30 o'clock)
    .byte $88,$89,$8a,$8b,$8c,$8d,$af,$b0,$8f,$14,$14,$b1,$16,$92,$93,$94 ; #$97 - overhead rotating turret right (3 o'clock)
    .byte $88,$b2,$b3,$b4,$b5,$00,$b6,$b7,$b8,$b9,$ba,$bb,$16,$bc,$bd,$94
    .byte $07,$40,$07,$40,$52,$53,$53,$53,$05,$06,$06,$06,$40,$40,$40,$40
    .byte $07,$40,$07,$c3,$53,$53,$53,$c4,$06,$06,$06,$c2,$40,$40,$40,$c3
    .byte $07,$40,$07,$40,$53,$53,$53,$53,$06,$06,$06,$06,$40,$40,$40,$40
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$05,$06,$06,$06,$40,$40,$40,$40
    .byte $07,$40,$07,$40,$52,$53,$53,$53,$06,$06,$06,$06,$40,$40,$40,$40
    .byte $07,$40,$07,$c3,$53,$53,$53,$c4,$06,$06,$06,$06,$40,$40,$40,$40
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$06,$06,$06,$c2,$40,$40,$40,$c3
    .byte $4a,$41,$41,$49,$26,$41,$41,$76,$41,$4a,$41,$7a,$2d,$26,$41,$7a
    .byte $41,$7a,$32,$41,$41,$7a,$41,$41,$41,$7a,$32,$41,$41,$7a,$41,$41
    .byte $32,$69,$32,$7a,$41,$69,$41,$7a,$32,$69,$32,$7a,$41,$69,$41,$7a
    .byte $40,$40,$69,$41,$40,$40,$69,$41,$40,$40,$69,$41,$24,$24,$24,$4a
    .byte $30,$00,$30,$00,$00,$00,$00,$00,$30,$00,$30,$00,$00,$00,$00,$00
    .byte $41,$41,$41,$26,$41,$41,$41,$41,$32,$41,$32,$41,$7d,$7d,$7d,$7d
    .byte $41,$7a,$32,$41,$4a,$7a,$41,$41,$26,$7a,$32,$41,$7d,$7e,$41,$41
    .byte $32,$41,$32,$41,$41,$41,$41,$41,$31,$40,$31,$40,$24,$24,$24,$24
    .byte $32,$69,$32,$7a,$41,$69,$41,$7a,$31,$69,$32,$7a,$24,$24,$4a,$7a
    .byte $32,$41,$32,$41,$7d,$7d,$7d,$7d,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $32,$41,$26,$7a,$7d,$7d,$7d,$7e,$e8,$e9,$ea,$eb,$f8,$f9,$fa,$fb
    .byte $30,$00,$30,$1e,$00,$00,$82,$00,$30,$00,$30,$82,$00,$00,$00,$00
    .byte $3f,$00,$3f,$00,$65,$65,$65,$65,$41,$41,$41,$41,$4b,$41,$41,$41
    .byte $3f,$00,$5f,$40,$65,$65,$65,$6b,$41,$41,$41,$6e,$41,$41,$41,$6f
    .byte $40,$2a,$40,$40,$40,$2a,$40,$40,$40,$2a,$40,$40,$40,$2a,$40,$40
    .byte $4b,$41,$41,$41,$4b,$41,$41,$41,$41,$41,$41,$41,$4b,$41,$41,$41
    .byte $41,$41,$4d,$50,$41,$41,$74,$75,$41,$4d,$4e,$40,$41,$74,$78,$40
    .byte $40,$2a,$40,$40,$40,$2a,$40,$40,$4f,$41,$41,$41,$67,$41,$41,$41
    .byte $40,$40,$40,$40,$40,$40,$40,$40,$4d,$2a,$40,$40,$6f,$2a,$40,$40
    .byte $4d,$41,$50,$4f,$79,$41,$75,$67,$56,$4e,$4f,$41,$56,$78,$67,$41
    .byte $41,$41,$41,$4d,$41,$41,$41,$74,$41,$41,$4d,$4e,$41,$41,$79,$78
    .byte $50,$2a,$40,$40,$75,$2a,$40,$40,$40,$2a,$40,$40,$40,$2a,$40,$40
    .byte $27,$50,$28,$2f,$56,$75,$2a,$2b,$29,$40,$2a,$40,$7c,$40,$2a,$40
    .byte $2f,$2f,$56,$50,$41,$41,$56,$75,$2b,$41,$57,$40,$40,$2b,$7c,$40
    .byte $2c,$40,$2a,$40,$2c,$40,$2a,$40,$2c,$40,$2a,$40,$2c,$40,$2a,$40
    .byte $31,$40,$66,$40,$40,$40,$2c,$40,$31,$40,$66,$40,$40,$40,$2c,$40
    .byte $40,$2a,$40,$40,$40,$2a,$40,$40,$40,$2a,$40,$40,$4f,$24,$24,$24
    .byte $31,$40,$66,$40,$40,$40,$2c,$4f,$31,$40,$2c,$67,$40,$40,$7f,$7d
    .byte $67,$41,$41,$41,$41,$41,$41,$41,$32,$41,$32,$41,$7d,$7d,$7d,$7d
    .byte $2c,$40,$2a,$40,$2c,$40,$2a,$40,$2c,$40,$2a,$40,$2c,$4f,$24,$24
    .byte $31,$40,$2b,$41,$40,$40,$40,$2b,$31,$40,$31,$40,$24,$24,$24,$24
    .byte $32,$41,$32,$41,$41,$41,$41,$41,$31,$40,$31,$40,$24,$24,$24,$24
    .byte $2c,$67,$32,$41,$7f,$7d,$7d,$7d,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $30,$00,$2d,$6d,$00,$00,$00,$77,$0b,$03,$03,$03,$0c,$00,$00,$00
    .byte $32,$41,$32,$41,$41,$41,$41,$41,$0b,$03,$03,$03,$0c,$00,$00,$00
    .byte $00,$00,$03,$03,$00,$00,$00,$00,$0b,$03,$03,$03,$0c,$00,$00,$00
    .byte $0b,$03,$55,$40,$0c,$00,$73,$40,$0b,$03,$03,$03,$0c,$00,$00,$00
    .byte $0b,$03,$03,$03,$0c,$00,$00,$00,$0b,$03,$03,$73,$0c,$00,$00,$55
    .byte $0b,$03,$03,$55,$0c,$00,$00,$55,$0b,$03,$03,$73,$0c,$00,$73,$40
    .byte $06,$06,$06,$06,$40,$40,$40,$40,$07,$40,$07,$40,$53,$53,$53,$53
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$40,$40,$69,$49,$40,$40,$69,$6c
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$5f,$2a,$40,$40,$60,$2a,$40,$40
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$40,$40,$40,$40,$40,$40,$40,$40
    .byte $49,$00,$03,$03,$6c,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $40,$40,$69,$41,$40,$40,$69,$41,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $40,$2a,$40,$40,$40,$2a,$40,$40,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$0b,$03,$03,$03,$0c,$00,$00,$00
    .byte $0b,$03,$3f,$5f,$0c,$00,$00,$60,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
    .byte $66,$40,$67,$41,$66,$4f,$41,$41,$66,$67,$41,$41,$7f,$7d,$7d,$7d
    .byte $41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$7d,$7d,$7d,$7d
    .byte $41,$26,$41,$7a,$41,$41,$4a,$7a,$41,$41,$80,$7a,$7d,$7d,$7d,$7e
    .byte $88,$89,$8a,$8b,$13,$8d,$14,$8e,$8f,$be,$bf,$91,$16,$c0,$17,$94 ; #$d4 - overhead rotating turret aiming down (6 o'clock)
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$03,$03,$03,$03,$00,$00,$00,$00
    .byte $0f,$32,$0f,$32,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $40,$40,$40,$40,$24,$24,$24,$24,$41,$41,$41,$41,$41,$41,$41,$41
    .byte $40,$40,$69,$68,$40,$40,$69,$6c,$40,$40,$69,$41,$40,$40,$69,$41
    .byte $0f,$32,$0f,$32,$00,$00,$00,$00,$49,$00,$00,$00,$5d,$00,$00,$00
    .byte $0f,$32,$0f,$32,$00,$00,$00,$00,$00,$00,$3f,$5f,$00,$00,$00,$60
    .byte $6e,$2a,$40,$40,$60,$2a,$40,$40,$40,$2a,$40,$40,$40,$2a,$40,$40
    .byte $40,$40,$69,$41,$24,$24,$24,$4a,$41,$41,$41,$26,$41,$41,$41,$41
    .byte $41,$49,$00,$00,$41,$5d,$00,$00,$41,$41,$49,$00,$4a,$41,$5d,$00
    .byte $00,$00,$5f,$40,$00,$00,$60,$40,$3f,$5f,$40,$40,$00,$60,$40,$4f
    .byte $40,$2a,$40,$40,$4f,$24,$24,$24,$67,$41,$41,$41,$41,$41,$41,$41
    .byte $5e,$41,$41,$49,$41,$4a,$41,$76,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $5f,$40,$40,$67,$81,$40,$4f,$41,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $08,$08,$08,$08,$08,$08,$82,$83,$86,$87,$88,$89,$8f,$90,$91,$92
    .byte $08,$08,$08,$08,$84,$85,$08,$08,$8a,$8b,$8c,$86,$93,$94,$95,$96
    .byte $08,$08,$98,$99,$08,$08,$a3,$a4,$08,$08,$ad,$ae,$08,$08,$b4,$b5
    .byte $9a,$9b,$9c,$9d,$a5,$a6,$a7,$00,$af,$b0,$b1,$b2,$b6,$b5,$b7,$b8
    .byte $9e,$9f,$a0,$9a,$00,$a8,$a9,$a5,$b2,$b1,$ae,$af,$b8,$b9,$b5,$b6
    .byte $a1,$a2,$ac,$08,$aa,$ab,$00,$08,$b0,$b3,$00,$08,$b5,$ba,$00,$08
    .byte $08,$08,$bb,$a7,$08,$08,$c1,$b1,$08,$08,$c6,$c7,$08,$08,$cd,$ce
    .byte $bc,$a8,$bd,$be,$00,$b1,$c2,$c3,$c8,$c7,$c9,$ca,$cf,$d0,$d1,$d2
    .byte $be,$bf,$a7,$bc,$c3,$c4,$b1,$00,$ca,$cb,$c7,$c8,$d2,$d1,$d0,$cf
    .byte $a8,$c0,$00,$08,$b1,$c5,$00,$08,$c7,$cc,$00,$08,$d3,$d4,$00,$08
    .byte $08,$08,$d5,$d6,$08,$08,$de,$df,$08,$08,$00,$00,$08,$08,$08,$08
    .byte $f3,$d8,$d9,$da,$e0,$e0,$e1,$e2,$00,$00,$00,$00,$08,$08,$08,$08
    .byte $da,$d9,$db,$f3,$e2,$e1,$e0,$e0,$00,$00,$00,$00,$08,$08,$08,$08
    .byte $dc,$dd,$00,$08,$e3,$e4,$00,$08,$00,$00,$00,$08,$08,$08,$08,$08
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$8d,$8e
    .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$8e,$97,$08,$08
    .byte $88,$89,$8a,$8b,$eb,$ec,$ed,$ee,$00,$ef,$ef,$f0,$ea,$00,$00,$f1
    .byte $ad,$e5,$e6,$e7,$b4,$e8,$e9,$e8,$bb,$ea,$00,$f1,$c1,$f2,$00,$f2
    .byte $e5,$e6,$e7,$b3,$e8,$e9,$e8,$ba,$ea,$00,$f1,$c0,$f2,$00,$f2,$c5

; each byte is an attribute palette byte for a single supertile (4x4 tiles)
level_2_palette_data:
    .byte $00,$aa,$be,$aa,$00,$8a,$aa,$aa,$00,$08,$aa,$00,$aa,$00,$aa,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$aa
    .byte $00,$08,$0a,$ea,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $aa,$08,$aa,$ab,$00,$40,$0a,$aa,$00,$95,$aa,$aa,$a0,$a9,$aa,$aa
    .byte $55,$d1,$f0,$f0,$74,$dd,$ff,$ff,$77,$55,$55,$55,$aa,$aa,$aa,$55
    .byte $56,$aa,$aa,$a9,$aa,$55,$55,$aa,$aa,$66,$99,$aa,$aa,$aa,$aa,$aa
    .byte $65,$a5,$95,$55,$55,$f0,$f0,$ff,$ff,$ff,$ff,$aa,$55,$55,$aa,$aa
    .byte $aa,$aa,$aa,$aa,$aa,$aa,$0a,$0a,$aa,$aa,$00,$00,$aa,$aa,$aa,$aa
    .byte $aa,$aa,$aa,$0a,$a5,$a5,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$aa,$aa,$aa,$aa,$aa,$aa,$aa
    .byte $aa,$aa,$aa,$aa,$00,$aa,$aa,$aa,$aa,$aa,$aa,$00,$aa,$aa,$aa,$aa
    .byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
    .byte $fe,$5a,$5a,$55,$55,$55,$55,$aa,$a5,$a5,$a5,$56,$aa,$aa,$55,$59
    .byte $55,$aa,$aa,$aa,$ff,$55,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
    .byte $fa,$fa,$35,$c5,$19,$8a,$2a,$46,$99,$aa,$aa,$66,$59,$5a,$5a,$56
    .byte $55,$55,$a0,$a0,$a0

; !(UNUSED)
bank_1_unused:
    .byte $00,$00,$00,$00,$00,$00,$00

; unused #$74 bytes out of #$2,000 bytes total (98.58% full)
; unused 116 bytes out of 8,192 bytes total (98.58% full)
; filled with 116 #$ff bytes by superc.cfg configuration
bank_1_unused_space:

.segment "BANK1_ID"

; bank byte
; see load_sound_banks_init_sound
    .byte $31