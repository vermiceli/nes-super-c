; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank 0 deals mostly with player routines.  This includes managing player
; movement, player sprite and palettes, player background collision boxes, and
; player weapon and bullets.
; Additionally, bank 0 contains level random enemy generation, code to write
; both text and graphics to CPU memory as well as transferring text and graphics
; from CPU memory to the PPU.
; Finally, bank 0 contains logic to handle automated player movement for the
; demos.

; 8 KiB PRG ROM
.segment "BANK0"

.include "constants.asm"

; import from bank 1
.import demo_input_lvl_02_p2

; import from bank 3
.import get_bg_collision
.import get_bg_collision_code
.import check_bg_collision_at_y
.import get_bg_collision_y_range
.import is_on_ground_water
.import overhead_player_get_bg_collision

; import from bank f
.import run_routine_from_tbl_below
.import two_byte_add
.import play_game_over_sound
.import kill_player
.import clear_bullet_sprite
.import play_sound
.import get_landing_y_pos
.import bg_collision_test_tbl
.import enable_nmi_for_vblank
.import disable_nmi_set_ppumask
.import create_enemy_slot_0_to_3
.import set_enemy_collision_and_type

; export for bank f
.export run_player_state_routines
.export write_text_to_mem
.export handle_player_bullets
.export cp_bullet_to_sprite_buffers
.export create_f_child_flames_or_destroy
.export enemy_gen_routine
.export init_level_vars
.export set_level_scroll_screen
.export run_demo_input

; export for multiple banks
.export write_graphic_data_to_ppu ; bank 1 and bank f

.byte $30 ; bank byte

run_player_state_routines:
    lda #$00
    sta Y_SCROLL_SPEED          ; stop any vertical scroll for this frame
    sta X_SCROLL_SPEED          ; stop any horizontal scroll for this frame
    lda #$00
    ldy PLAYER_GAME_OVER_STATUS ; load p1 game over status (0 = not game over, 1 = game over)
    bne @p2_check               ; branch if p1 is in game over
    ora #$01                    ; player 1 not in game over, set bit 0

@p2_check:
    ldy PLAYER_GAME_OVER_STATUS+1 ; load p2 game over status (0 = not game over, 1 = game over)
    bne @continue                 ; branch if player 2 in game over (or 1 player game)
    ora #$02                      ; player 2 not in game over, set bit 1

; set ACTIVE_PLAYERS_FLAG, set any autoscroll, check which player may be causing any scroll
@continue:
    tay                           ; transfer player game over statuses to y
    dey
    sty ACTIVE_PLAYERS_FLAG       ; set player active/game over statuses
                                  ; #$00 - player 1 not in game over, player 2 in game over (or 1 player game)
                                  ; #$01 - player 1 in game over, player 2 not in game over
                                  ; #$02 - 2 player game and neither player in game over
                                  ; #$ff - both players in game over
    jsr handle_y_autoscroll       ; enable/disable vertical auto-scroll based on Y_AUTOSCROLL
                                  ; based on variables, will auto-scroll vertically up/down until certain checkpoint
    jsr handle_x_autoscroll       ; enable/disable right horizontal auto-scroll based on X_AUTOSCROLL
    ldx #$00                      ; start with player 1 for running state routines
    ldy #$01
    lda PLAYER_X_POS
    cmp PLAYER_X_POS+1            ; compare player 1 X position to player 2 X position
                                  ; if in a 1 player game, player 2 X position will be #$00
    bcc @p1_left_of_p2            ; branch if player 2 is farther to the right than player 1
    bne @run_player_state_routine ; branch if player 1 is farther to the right than player 2
    lda CONTROLLER_STATE+1        ; players at same X spot, see if player two pressing d-pad right
    ora JUMP_INPUT+1              ; or jumping while pressing right
    and #$01                      ; just looking at d-pad right button input
    beq @run_player_state_routine ; branch if player 2 is not pressing right

; 2 player game and either player 2 to the right of player 1, or players at same spot and player 2 pressing d-pad right
@p1_left_of_p2:
    inx ; set rightmost player index to p2
    dey ; set leftmost player index to p1

@run_player_state_routine:
    stx $10                                ; set player index (rightmost player)
    sty $11                                ; set other player index (leftmost player) (used in is_player_initialized)
    jsr run_scroll_player_state_routine
    lda $10
    ldx $11
    stx $10                                ; set player index (leftmost player)
    sta $11                                ; set other player index (rightmost player) (used in is_player_initialized)
    jsr run_no_scroll_player_state_routine
    ldx #$00                               ; initialize player index for lower player
    ldy #$01                               ; initialize player index for higher player
    lda LEVEL_Y_SCROLL_FLAGS
    bmi @invert_y_scroll_order             ; branch if cannot scroll up
    lda PLAYER_Y_POS
    cmp PLAYER_Y_POS+1
    bcc @run_y_scroll_routine              ; branch if player 1 is above player 2
    bne @set_p2_first                      ; branch if player 1 is below player 2
    lda OVERHEAD_FLAG                      ; players same height, load overhead flag (0 = side view, 1 = overhead view)
    beq @side_view                         ; branch if side view
    lda CONTROLLER_STATE+1                 ; overhead level and players at same height
                                           ; load player 2 controller input
    and #$08                               ; strip to just up d-pad input
    beq @run_y_scroll_routine              ; branch if p2 up d-pad not pressed
    bne @set_p2_first                      ; p2 pressing up, always branch

; players at same height and side view level
@side_view:
    lda PLAYER_JUMP_STATUS+1
    bpl @run_y_scroll_routine    ; branch if p2 is jumping or hit and falling back
    lda PLAYER_Y_FAST_VELOCITY+1
    bpl @run_y_scroll_routine    ; branch if p2 is moving down
    bmi @set_p2_first            ; always branch to just have p2 to be considered below p1

; cannot scroll up
; inverts the order run_player_y_scroll_routine is run for players
@invert_y_scroll_order:
    lda PLAYER_Y_POS+1        ; load player 2 Y position
    cmp PLAYER_Y_POS          ; compare to player 1 Y position
    bcc @run_y_scroll_routine ; branch if player 2 is above player 1
                              ; or player 2 in game over, or 1 player game
    bne @set_p2_first         ; branch if player 2 is below player 1
    bit PLAYER_JUMP_STATUS+1  ; players at same height, see if p2 is falling off ledge
    bvc @run_y_scroll_routine ; branch if player 2 is not falling off ledge

; either p2 below p1, or same height and p2 is falling falling off ledge
@set_p2_first:
    inx ; set player that cannot block scroll to p2
    dey ; set player that can block scroll to p1

; apply Y velocity or Y scroll for player(s)
; for 2 player games, one player can prevent Y scroll if would be scrolled off screen
@run_y_scroll_routine:
    stx $10                                   ; set current player index
    sty $11                                   ; set other player index
    jsr run_scroll_player_y_scroll_routine    ; apply Y velocity with different checks depending on PLAYER_STATE,x
    lda $10
    ldx $11
    stx $10                                   ; set other player index
    sta $11                                   ; set current player index
    jsr run_no_scroll_player_y_scroll_routine ; apply Y velocity with different checks depending on PLAYER_STATE,x
    jsr set_elevator_vel                      ; calculate level 4 (Base Area 3) Y scroll speed when elevator enabled
    ldy #$00                                  ; initialize ENEMY_DIFFICULTY
    ldx #$01                                  ; initialize player index

; calculate ENEMY_DIFFICULTY based on number of active players with S weapon
; check if need to transfer life from game over player
; check if should play game over sound
@calc_enemy_difficulty:
    lda PLAYER_STATE,x
    cmp #$02                    ; compare to normal state
    bne @next_player            ; branch if current player not in normal state
    lda PLAYER_CURRENT_WEAPON,x ; 0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F
    and #$0f                    ; strip rapid fire flag
    cmp #$02                    ; compare to S weapon
    bne @next_player            ; move to next player if current player doesn't have S weapon
    iny                         ; increment number of players in normal state that have the S weapon

@next_player:
    dex
    bpl @calc_enemy_difficulty    ; branch if 2 player game
    sty ENEMY_DIFFICULTY          ; set enemy difficulty
    jsr check_transfer_life       ; for 2 player games, see if should transfer a life to a game-over player
    lda PLAYER_GAME_OVER_STATUS   ; load p1 game over status (0 = not game over, 1 = game over)
    and PLAYER_GAME_OVER_STATUS+1 ; load p2 game over status (0 = not game over, 1 = game over)
    beq @exit                     ; exit if no player in game over
    jmp play_game_over_sound      ; both players (or only player) are in game over, play game over sound
                                  ; and set level routine to level_routine_05

@exit:
    rts

; determines and sets the player sprite and attribute
; decrements PLAYER_RECOIL_TIMER if non-zero
; input
;  * x - player index (0 = p1, 1 = p2)
calc_and_set_player_sprite:
    lda #$ff
    sta PLAYER_ACTION_STATE,x   ; initialize player action state to normal state
    lda #$00
    sta PLAYER_SKIP_COLLISION,x ; enable player sprite collision testing
    lda PLAYER_RECOIL_TIMER,x
    beq @det_surface
    dec PLAYER_RECOIL_TIMER,x   ; decrement weapon recoil timer

@det_surface:
    lda PLAYER_SURFACE,x        ; load the kind of surface the player is standing on
    cmp #$04                    ; see if player in water
    bne @not_in_water           ; branch if not in water
    lda #$7f                    ; player in water
    sta PLAYER_ACTION_STATE,x   ; set player action state to indicate in water
    ldy #$00                    ; default sprite for in water (player_sprite_25)
    lda CONTROLLER_STATE,x      ; load controller input
    and #$04                    ; see if down button pressed
    beq @determine_water_sprite ; branch if down button not pressed
    ldy #$02                    ; down pressed in while in water
                                ; use player_sprite_27 (splash)

@determine_water_sprite:
    tya                             ; transfer player sprite index to a
    lsr                             ; neat optimization to re-use sprite index to know collision
    sta PLAYER_SKIP_COLLISION,x     ; disable player sprite collision when crouching in water, otherwise keep collision
    lda PLAYER_RECOIL_TIMER,x
    beq @set_player_in_water_sprite ; branch if no reoil timer
    ldy #$04                        ; player recoil timer not elapsed
                                    ; use player_sprite_29 (holding weapon out)
    lda CONTROLLER_STATE,x          ; load controller input
    and #$08
    beq @set_player_in_water_sprite ; branch if not pressing up
    iny                             ; pressing up and recoil timer not elapsed in water
                                    ; use player_sprite_2a (player aiming up)
    lda CONTROLLER_STATE,x          ; load controller input
    and #$03
    bne @set_player_in_water_sprite ; branch if not pressing left nor right
    iny                             ; pressing up-right or up-left
                                    ; use player_sprite_2b (player aiming angled up)

@set_player_in_water_sprite:
    lda player_sprite_in_water_tbl,y
    jmp set_player_sprite            ; set player sprite and sprite attribute (accounting for invincibility)

; player not in water
@not_in_water:
    lda PLAYER_JUMP_STATUS,x
    asl
    bcc @not_jumping         ; branch if player not jumping
    jmp @jumping             ; jumping, branch

@not_jumping:
    asl                     ; see if got hit while falling off ledge
    bcc @on_ground          ; branch if on ground (not jumping, nor walked off ledge)
    lda #$01                ; player falling off ledge
                            ; player_sprite_01 (player with feet slightly apart)
    ldy PLAYER_STATE,x
    cpy #$02                ; see if in normal player state
    beq @set_falling_sprite
    lda #$05                ; player not in normal state
                            ; (coming down helicopter cord in level 1)
                            ; use player_sprite_05 (feet mostly together)

@set_falling_sprite:
    jmp set_player_sprite ; set player sprite and sprite attribute (accounting for invincibility)

@on_ground:
    lda CONTROLLER_STATE,x             ; load controller input
    and #$03
    beq @stationary                    ; branch if not pressing left nor right on d-pad
    inc PLAYER_ANIM_FRAME_TIMER,x      ; moving in a direction, increment animation timer
    lda PLAYER_ANIM_FRAME_TIMER,x
    cmp #$06
    bcc @check_boss
    lda #$00                           ; animation timer maxed out, reset
    sta PLAYER_ANIM_FRAME_TIMER,x
    inc PLAYER_ANIMATION_FRAME_INDEX,x ; move to next animation frame
    ldy PLAYER_ANIMATION_FRAME_INDEX,x
    cpy #$06
    bcc @check_boss
    ldy #$00                           ; finished animating all frames, start over
    beq @set_frame_continue            ; always branch

@stationary:
    lda #$00                      ; reset animation frame timer
    sta PLAYER_ANIM_FRAME_TIMER,x
    ldy #$02

; input
;  * y - animation frame index
@set_frame_continue:
    sty PLAYER_ANIMATION_FRAME_INDEX,x

@check_boss:
    ldy PLAYER_ANIMATION_FRAME_INDEX,x
    lda BOSS_DEFEATED_FLAGS
    beq @not_boss                             ; branch if boss not defeated
    lda boss_defeated_player_anim_sound_tbl,y ; boss defeated, load footstep (or silence)
    cmp BOSS_DEFEATED_PLAYER_ANIM_SOUND,x     ; see if should play player step sound for animation
    beq @not_boss                             ; branch if value matches what is expected for animation
    sta BOSS_DEFEATED_PLAYER_ANIM_SOUND,x     ; set new value for sound
                                              ; either #$00 (no sound) or #$07 sound_07 (footstep)
    tay                                       ; transfer sound code to y
    beq @not_boss                             ; branch if 0, to not play any sound
    jsr play_sound                            ; play sound_07 (footstep)

@not_boss:
    lda #$00
    sta $00
    lda CONTROLLER_STATE,x     ; load controller input
    lsr
    lsr
    lsr
    ldy #$0c
    bcs @down_up               ; branch if pressing down
    lsr
    lda #$02
    sta $00
    ldy #$06
    bcs @down_up               ; branch if pressing up
    ldy #$12
    lda PLAYER_RECOIL_TIMER,x
    beq @no_recoil
    lda CONTROLLER_STATE,x     ; load controller input
    and #$03
    bne @set_sprite_from_index ; branch pressing left or right
                               ; to set player sprite based on y and current PLAYER_ANIMATION_FRAME_INDEX
    lda #$0b
    bne @set_player_sprite_2   ; set player sprite to player_sprite_0b

@no_recoil:
    lda CONTROLLER_STATE,x     ; load controller input
    and #$03
    beq @set_sprite_from_index ; set player sprite based on y and current PLAYER_ANIMATION_FRAME_INDEX
    ldy #$00
    beq @set_sprite_from_index ; set player sprite based on y and current PLAYER_ANIMATION_FRAME_INDEX

; d-pad down or up is pressed
@down_up:
    lda CONTROLLER_STATE,x     ; load controller input
    and #$03
    bne @set_sprite_from_index ; set player sprite based on y and current PLAYER_ANIMATION_FRAME_INDEX
    lda $00
    bne @get_recoil_set_sprite
    lda PLAYER_SURFACE,x       ; load the kind of surface the player is standing on
    cmp #$06                   ; see if player is on an inclined surface
    ldy #$3f                   ; player action state for prone on flat surface
    bcc @set_action_state      ; branch if player is not on an inclined surface
    ldy #$5f                   ; player action state for crouching on inclined surface
    lda #$04
    sta $00                    ; aiming down on incline, set sprite index to crouching/kneeling

@set_action_state:
    sty PLAYER_ACTION_STATE,x ; set player action state to indicate crouching
                              ; either on flat surface (#$5f) or inclined surface (#$3f)

@get_recoil_set_sprite:
    lda PLAYER_RECOIL_TIMER,x
    beq @set_sprite
    inc $00                   ; increment sprite when have recoil

@set_sprite:
    ldy $00                          ; load player sprite index for prone, crouching, or aiming up
    lda player_sprite_prone_up_tbl,y ; load player sprite (prone, crouching, or aiming up)
    jmp set_player_sprite            ; set player sprite and sprite attribute (accounting for invincibility)

; sets player sprite based on y and current PLAYER_ANIMATION_FRAME_INDEX
; input
;  * y - base player_sprite_tbl index to specify which player sprite to load
@set_sprite_from_index:
    tya                                ; transfer sprite index to a
    clc                                ; clear carry in preparation for addition
    adc PLAYER_ANIMATION_FRAME_INDEX,x ; add current animation frame index
    tay                                ; transfer resulting sprite code offset to offset register
    lda player_sprite_tbl,y            ; load player sprite based on animation frame

@set_player_sprite_2:
    jmp set_player_sprite ; set player sprite and sprite attribute (accounting for invincibility)

@jumping:
    lda #$1f                           ; jumping
    sta PLAYER_ACTION_STATE,x          ; set player action state to indicate jumping
    inc PLAYER_ANIM_FRAME_TIMER,x
    lda PLAYER_ANIM_FRAME_TIMER,x
    cmp #$05
    bcc @set_jump_sprite
    lda #$00
    sta PLAYER_ANIM_FRAME_TIMER,x
    inc PLAYER_ANIMATION_FRAME_INDEX,x
    lda PLAYER_ANIMATION_FRAME_INDEX,x
    cmp #$04
    bcc @set_jump_sprite
    lda #$00
    sta PLAYER_ANIMATION_FRAME_INDEX,x

@set_jump_sprite:
    lda PLAYER_ANIMATION_FRAME_INDEX,x ; load curled jumping sprite index
    clc                                ; clear carry in preparation for addition
    adc #$1a                           ; add base index to curled jumping sprites

; set player sprite and sprite attribute
; if invincible after just spawning will flash invisible every other frame
; in invincible due to B weapon, will cycle palette every 2 frames
; input
;  * a - player sprite to use
;  * x - player index (0 = p1, 1 = p2)
set_player_sprite:
    tay                                ; transfer player sprite to y
    lda NEW_LIFE_INVINCIBILITY_TIMER,x
    beq @continue                      ; branch if didn't re-spawn to skip blinking effect
    lda FRAME_COUNTER                  ; load frame counter
    lsr
    bcc @continue                      ; skip blinking for even frames
    ldy #$00                           ; odd frame on a player that just spawned
                                       ; create blinking effect by hiding the player every other frame
                                       ; sprite 0 is invisible

@continue:
    tya                             ; transfer player sprite code to a
    sta PLAYER_SPRITE,x             ; set player sprite
    lda player_sprite_palette_tbl,x
    ora PLAYER_SPRITE_ATTR,x        ; merge with existing sprite attribute
    ldy INVINCIBILITY_TIMER,x       ; see if player in invincible, i.e. grabbed B weapon
    beq @set_attr_exit              ; branch if player not invincible
    lda FRAME_COUNTER               ; player invincible, load frame counter
    lsr
    lsr
    lda PLAYER_SPRITE_ATTR,x        ; load current player sprite attribute
    and #$fc                        ; changing palette every 2 frames, strip palette bits
    bcc @set_attr_exit              ; branch if frame 0 or 1 in cycle to use palette 0
    ora #$01                        ; frame 2 or 3 in cycle, use palette 1

@set_attr_exit:
    sta PLAYER_SPRITE_ATTR,x ; set player sprite palette
    rts

player_sprite_palette_tbl:
    .byte $00 ; player 1
    .byte $01 ; player 2

; footstep sound code to play for specific animation frame after defeating boss
; indexes based off of PLAYER_ANIMATION_FRAME_INDEX
; sound_07
boss_defeated_player_anim_sound_tbl:
    .byte $07,$00,$00,$07,$00,$00

; player_sprite_01, player_sprite_02, player_sprite_03, player_sprite_04
; player_sprite_05, player_sprite_06, player_sprite_07, player_sprite_08
; player_sprite_09, player_sprite_0a, player_sprite_0c, player_sprite_0d
; player_sprite_0e, player_sprite_10, player_sprite_11, player_sprite_12
; player_sprite_13, player_sprite_14, player_sprite_15
player_sprite_tbl:
    .byte $01,$02,$03,$04,$02,$05,$0c,$0d,$0e,$0f,$0d,$10,$11,$12,$13,$14
    .byte $12,$15,$06,$07,$08,$09,$07,$0a

; player_sprite_16, player_sprite_17, player_sprite_18, player_sprite_19
; player_sprite_23, player_sprite_24
player_sprite_prone_up_tbl:
    .byte $16,$17 ; prone, prone w/recoil
    .byte $18,$19 ; aiming up, aiming up w/recoil
    .byte $23,$24 ; crouch/kneel, crouch/kneel w/recoil

; player_sprite_25, player_sprite_26, player_sprite_27, player_sprite_28
; player_sprite_29, player_sprite_2a, player_sprite_2b
; !(OBS) indexes #$01 and #$03 are unused and the sprites are identical to
; indexes #$02 and #$04 respectively, perhaps originally they would be have been
; different per player index
player_sprite_in_water_tbl:
    .byte $25 ; default player in water (holding weapon close)
    .byte $26 ; unused (splash)
    .byte $27 ; player crouching in water (splash)
    .byte $28 ; unused (holding weapon out)
    .byte $29 ; recoil effect active (holding weapon out)
    .byte $2a ; pressing up with active recoil (player aiming up)
    .byte $2b ; pressing up-right/up-left (angled aiming up)

; sets PLAYER_AIM_DIR,x based on player input (and surface)
; input
;  * x - player index (0 = p1, 1 = p2)
set_player_aim_dir:
    ldy #$00
    lda PLAYER_SPRITE_ATTR,x
    asl
    bpl @player_input        ; branch if facing right
    iny                      ; facing left

@player_input:
    sty $00                ; 0 = facing right, 1 = facing left
    lda CONTROLLER_STATE,x ; load controller input
    and #$0f               ; strip to just d-pad input
    cmp #$0b               ; see if valid input
    bcc @handle_down       ; branch if valid input
    lda #$00               ; invalid input, strip to no input

@handle_down:
    tay                      ; transfer input to offset register
    cpy #$04
    bne @set_player_aim_dir  ; branch if player is not pressing down
    ldy #$07                 ; player pressing down
    lda PLAYER_JUMP_STATUS,x
    bmi @set_player_aim_dir  ; branch if player is jumping or falling back after hit
    ldy #$03                 ; player pressing down, and not jumping
    lda PLAYER_SURFACE,x     ; load the kind of surface the player is standing on
    cmp #$06                 ; see if player is on an inclined surface
    bcs @set_player_aim_dir  ; branch if player is on an inclined surface
    ldy #$04                 ; player is pressing down on a flat surface

@set_player_aim_dir:
    tya                      ; transfer controller d-pad input to a
    asl                      ; double since each entry is 2 bytes
    adc $00                  ; add whether facing left (0 = facing right, 1 = facing left)
    tay                      ; transfer to offset register
    lda player_aim_dir_tbl,y
    sta PLAYER_AIM_DIR,x
    rts

; byte 0 = facing right
; byte 1 = facing left
player_aim_dir_tbl:
    .byte $02,$0a ; no input
    .byte $02,$0a ; pressing right
    .byte $02,$0a ; pressing left
    .byte $04,$08 ; player pressing down on inclined surface
    .byte $05,$07 ; player pressing down on flat surface
    .byte $03,$09 ; player pressing right down
    .byte $03,$09 ; player pressing left down
    .byte $06,$06 ; player pressing down
    .byte $00,$0c ; player pressing up
    .byte $01,$0b ; player pressing up right
    .byte $01,$0b ; player pressing up left

; tries where player last was, then to the left 5 tiles, then to the right
; until a place where the player can land is found
; input
;  * x - player index (0 = p1, 1 = p2)
; output
;  * carry - set when bg collision found, clear otherwise
;  * a - collision code
;  * PLAYER_SPRITE_X_POS,x - set to X position to drop in at
find_player_drop_in_x_pos:
    lda #$00
    sta $15                            ; initialize $15, will be set to initial X position in next line method
    jsr find_bg_collision_below_player ; find bg collision below player position
    bcc @exit_found_bg                 ; exit success if bg collision found below player (land beneath player)
    lda X_SCROLL                       ; location not found, test to the right
                                       ; load PPU horizontal scroll
    and #$0f
    sta $00
    lda #$28
    sec                                ; set carry flag in preparation for subtraction
    sbc $00                            ; move to the left 5 tiles
    sta PLAYER_SPRITE_X_POS,x          ; test #$28 pixels to left of previous test point

; test left point, then keep moving right until a position is found
@loop:
    jsr find_bg_collision_below_player ; find bg collision at or below test X position
    bcc @exit_found_bg                 ; branch if drop in position found
    lda PLAYER_SPRITE_X_POS,x          ; location still not found
    clc                                ; clear carry in preparation for addition
    adc #$10
    sta PLAYER_SPRITE_X_POS,x          ; add #$10 to test X position
    cmp #$e0                           ; see if at far right edge
    bcc @loop                          ; branch to test next point if not too far right
    lda $15                            ; no suitable position found, just use original player X position
    sta PLAYER_SPRITE_X_POS,x
    bne @exit_found_bg                 ; pretty much always branch unless initial X position was #$00 (doesn't seem possible)
    clc                                ; couldn't find X position, retry again next frame in player_state_routine_01
    rts

@exit_found_bg:
    sec
    rts

; find Y position for given X position where there is a bg collision
; first tries player x's position, then tries ($15, player Y pos + #$20)
; input
;  * x - player index (0 = p1, 1 = p2)
;  * $15 - when 0, will be set to PLAYER_SPRITE_X_POS,x for use by find_player_drop_in_x_pos
;    to know initial, calculated X spawn location when no suitable X position found
; output
;  * a - bg collision code at (PLAYER_SPRITE_X_POS,$16)
;  * $15 - when zero is passed in for $15, this method sets $15 to PLAYER_SPRITE_X_POS,x
;    to ensure that if no suitable position found, the level spawn X position is used.
;    if non-zero, the value isn't changed to preserve initial X position
;  * $16 - when carry set, contains Y position of bg collision
;  * carry - clear when position found below player, set when no appropriate position found
find_bg_collision_below_player:
    ldy PLAYER_SPRITE_Y_POS,x
    lda PLAYER_SPRITE_X_POS,x
    jsr get_bg_collision        ; get background collision code for position (a,y)
    tay                         ; transfer bg collision code to y
    lda bg_collision_test_tbl,y ; see if collision should count
    bne exit_carry_set          ; exit with carry set if player x's position is a bg collision
    lda $15                     ; didn't find collision, see if need to backup calculated X spawn location
    bne @continue               ; branch if already backed up
    lda PLAYER_SPRITE_X_POS,x   ; first time being called, backup initial X spawn location
    sta $15                     ; in case no possible position found to use initial X spawn location anyway

@continue:
    lda PLAYER_SPRITE_Y_POS,x
    clc                       ; clear carry in preparation for addition
    adc #$20
    sta $16                   ; set Y test position to PLAYER_SPRITE_Y_POS,x + #$20

; go down the screen vertically until at bottom or found bg collision
@loop:
    ldy $16                   ; load Y position to test
    lda PLAYER_SPRITE_X_POS,x ; load X position to test
    jsr get_bg_collision      ; get background collision code for position (a,y)
    bne @exit_found           ; exit if collision code at test location
    lda $16                   ; no collision found, load Y position to test
    clc                       ; clear carry in preparation for addition
    adc #$10                  ; move down #$10 pixels
    sta $16                   ; set new Y position to test
    cmp #$e0                  ; see if at bottom of screen
    bcc @loop                 ; branch if can test new X,Y position
    rts                       ; exit with carry set as no position was found

@exit_found:
    clc
    rts

; find bg collision below (PLAYER_SPRITE_X_POS, a) for player x
; input
;  * a - Y position
;  * x - player index (0 = p1, 1 = p2)
; output
;  * carry flag - clear bg collision beneath position, set when no bg collision beneath position
find_bg_collision_below_a:
    tay                       ; store Y position in y for bg collision detection
    lsr
    lsr
    lsr
    lsr
    sta $16                   ; store high nibble of Y position in $16
    lda PLAYER_SPRITE_X_POS,x
    jsr get_bg_collision      ; get background collision code for position (a,y)
    bne @exit_found
    ldy $04

@bg_collision_check_loop:
    inc $16       ; move down one row
    lda $16
    cmp #$0e
    bcs @exit     ; exit (carry set, no collision found) if at bottom of screen
    tya           ; transfer Y position to a
    and #$80
    sta $17
    tya
    and #$7f
    clc           ; clear carry in preparation for addition
    adc #$08
    cmp #$78
    bcc @continue
    adc #$07

@continue:
    and #$7f
    ora $17
    tay
    jsr check_bg_collision_at_y  ; check bg collision at BG_COLLISION_DATA,y
    beq @bg_collision_check_loop

@exit_found:
    clc

@exit:
    rts

exit_carry_set:
    sec
    rts

; check if player clipped into a bg collision box (stuck), if so kill the player
; every #$10 frames, if boss isn't defeated, check bg collision
;  * on side-view levels, if collision is #$02 (floor/ground), kill the player
;  * on overhead levels, if collision code is non-zero, kill the player
kill_player_if_clipped:
    lda FRAME_COUNTER         ; load frame counter
    and #$0f
    bne @exit
    lda BOSS_DEFEATED_FLAGS
    bne @exit
    ldy PLAYER_SPRITE_Y_POS,x
    lda PLAYER_SPRITE_X_POS,x
    jsr get_bg_collision      ; get background collision code for position (a,y)
    tay                       ; transfer collision code to y
    beq @exit                 ; exit if no bg collision
    lda OVERHEAD_FLAG         ; (0 = side view, 1 = overhead view)
    bne @kill_player
    cpy #$02
    bne @exit                 ; exit if collision code is #$02 (floor/ground)

@kill_player:
    jmp kill_player ; play player death sound and update player state for player x

@exit:
    rts

; find X position for player x to span for overhead levels
; input
;  * x - player index (0 = p1, 1 = p2)
oh_find_spawn_x_pos:
    jsr get_player_bg_collision ; determine background collision code for position player x's position
    beq @exit                   ; exit if no background collision
    lda #$18                    ; player x encountered background collision, find new starting X position
    sta PLAYER_SPRITE_X_POS,x   ; starting at the left side of screen

@find_x_pos_loop:
    jsr get_player_bg_collision ; see if new position has background collision
    beq exit_carry_set          ; exit with carry set if position found
    lda PLAYER_SPRITE_X_POS,x   ; another bg collision, move to next position
    clc                         ; clear carry in preparation for addition
    adc #$10
    sta PLAYER_SPRITE_X_POS,x
    cmp #$f0
    bcc @find_x_pos_loop        ; loop if another X position available to test
    lda #$38                    ; searched all positions, just use X = #$38
    sta PLAYER_SPRITE_X_POS,x

@exit:
    rts

; determines background collision code for position player x's position
; input
;  * x - player index (0 = p1, 1 = p2)
; output
;  * a - background collision code
;  * y - bg collision offset, i.e. BG_COLLISION_DATA offset (same as $04)
;  * zero flag - set when no collision, clear otherwise
get_player_bg_collision:
    ldy PLAYER_SPRITE_Y_POS,x
    lda PLAYER_SPRITE_X_POS,x
    jmp get_bg_collision

; enable/disable vertical auto-scroll based on Y_AUTOSCROLL
; when appropriate, will auto-scroll vertically up/down until certain checkpoint
; input
;  * Y_AUTOSCROLL - what type of auto-scroll logic to apply
;    * 0 = scroll vertically up or down to checkpoint
;    * 1 = scroll vertically up to checkpoint if player(s) in top portion of screen
;  * Y_AUTOSCROLL_STOP_SCREEN
;    * when positive, the screen where the stop checkpoint exist
;    * when negative, always scroll in the determined direction until current screen's stop position
;  * Y_AUTOSCROLL_STOP_POS - specifies what vertical scroll value to stop scrolling
;  * $00 - the vertical scroll position to stop scrolling if mode 1 and Y_AUTOSCROLL_STOP_SCREEN is non-negative
;    !(OBS) in this game, when using mode 1, Y_AUTOSCROLL_STOP_POS is always negative so $00 is never passed in.
handle_y_autoscroll:
    lda Y_AUTOSCROLL ; 0 = scroll vertically (optionally until checkpoint)
                     ; 1 = scroll vertically up (optionally to checkpoint) if player(s) in top portion of screen
    beq @mode_0      ; branch if mode 0
    ldy #$00         ; mode 1, scrolling up when player in top portion, e.g. level 2 after boss defeated
                     ; testing to see if should scroll up
                     ; initialize player in top of screen count to #$00
    ldx #$01         ; initialize player index to player 2

@player_loop:
    lda PLAYER_GAME_OVER_STATUS,x ; load player game over status (0 = not game over, 1 = game over)
    bne @next_player              ; move to next player if current player is in game over (or 1 player game)
    lda PLAYER_SPRITE_Y_POS,x     ; load player Y position
    cmp #$60                      ; compare to top 37.5% of the screen
    bcs @next_player              ; move to next player if current player not in top 37.5% of screen
    iny                           ; increment number of players in top part of screen

@next_player:
    dex                     ; decrement player index
    bpl @player_loop        ; loop if now on player 1
    tya                     ; finished looping through players,
                            ; transfer number of players in top portion of screen to a
    bne @test_scroll_up     ; branch if at least one player is in top portion
                            ; to scroll up 1 pixel this frame unless already scrolled to checkpoint
    beq @disable_autoscroll ; no players in top portion of screen, no scroll needed (always branch)

; Y_AUTOSCROLL is zero
; see if scrolled to scroll position, and if not, determine scroll direction and scroll
; mode 0 is used for every time Y auto-scroll is used, except level 2
@mode_0:
    lda Y_AUTOSCROLL_STOP_POS ; load what position to stop scrolling for the screen
    beq @exit                 ; exit if no auto-scroll stop position
    lda Y_AUTOSCROLL_STOP_POS ; !(OBS), no need to re-load the value
    and #$fe                  ; strip out the direction bit (bit 0)
    sta $00                   ; set vertical scroll compare value
    cmp Y_SCROLL              ; compare to PPU vertical scroll
    beq @disable_autoscroll   ; branch if scrolled to position to stop auto-scroll
    lda Y_AUTOSCROLL_STOP_POS ; values are different, re-load full to determine direction
    lsr                       ; see if should scroll down
    bcs @test_scroll_down     ; branch to test if should scroll down

; set Y_SCROLL_SPEED to #$ff (scroll up) if not at checkpoint
@test_scroll_up:
    lda Y_AUTOSCROLL_STOP_SCREEN ; load screen at which to stop auto-scroll (if value is positive)
    bmi @set_scroll_up_1_exit    ; branch if negative to set Y_SCROLL_SPEED to scroll up 1
    cmp Y_SCREEN                 ; compare to number of vertical nametable screens scrolled
    bne @set_scroll_up_1_exit    ; branch if not on stop screen to set Y_SCROLL_SPEED to scroll up 1
    lda $00                      ; on screen where auto-scroll should stop, load auto-scroll stop position
    cmp Y_SCROLL                 ; compare to PPU vertical scroll
    bcs @disable_autoscroll      ; branch to stop auto-scroll if reached stop position

@set_scroll_up_1_exit:
    lda #$ff
    bne @set_y_scroll_exit ; always branch to set an upward scroll of 1

; set Y_SCROLL_SPEED to #$01 (scroll down) if not at checkpoint
@test_scroll_down:
    lda Y_AUTOSCROLL_STOP_SCREEN ; load screen at which to stop auto-scroll (if value is positive)
    bmi @set_scroll_down_1_exit  ; branch if negative to set Y_SCROLL_SPEED to scroll down 1
    cmp Y_SCREEN                 ; compare to number of vertical nametable screens scrolled
    bne @set_scroll_down_1_exit  ; branch if not on stop screen to set Y_SCROLL_SPEED to scroll down 1
    lda $00                      ; load auto-scroll stop position
    cmp Y_SCROLL                 ; compare to PPU vertical scroll
    bcc @disable_autoscroll      ; branch to stop auto-scroll if reached stop position

@set_scroll_down_1_exit:
    lda #$01

@set_y_scroll_exit:
    sta Y_SCROLL_SPEED ; set how much to scroll the screen vertically this frame

@exit:
    rts

@disable_autoscroll:
    ldy #$00
    sty Y_AUTOSCROLL_STOP_POS    ; disable mode 0 auto-scroll
    sty Y_AUTOSCROLL             ; disable mode 1
    dey
    sty Y_AUTOSCROLL_STOP_SCREEN ; disable auto-scroll stop screen
    rts

; enable/disable horizontal auto-scroll based on X_AUTOSCROLL
; input
;  * X_AUTOSCROLL - what type of auto-scroll logic to apply
;    * 0 = no auto-scroll (disabled)
;    * 1 = scroll horizontally to the right until PPU is scrolled to show
;      the next nametable, which means revealed entire screen to right
;      e.g. level 4 when revealing the base of elevator
;    * 2 = scroll horizontally while any player on rightmost 37.5% of screen
;      (side-levels only). For example, this is used in level 1, 4, and 5 for
;      the ending animation
handle_x_autoscroll:
    ldx X_AUTOSCROLL        ; load any horizontal scroll control value
    beq @exit               ; branch if horizontal auto-scroll is disabled
    dex
    beq @mode_1             ;  branch if X_AUTOSCROLL is mode #$01
                            ; which scrolls horizontally until next screen is revealed
    dex
    bne @exit               ; exit if X_AUTOSCROLL isn't #$02
    lda SCREEN_SCROLL_TYPE  ; mode 2
                            ; load screen scroll type (0 = horizontal, 1 = vertical/overhead)
    bne @clear_x_autoscroll ; branch if vertical level (not supported for mode #$02)
    tay                     ; horizontal level
                            ; initialize number of players on right edge of screen to #$00
    ldx #$01                ; initialize player index to player 2

@player_loop:
    lda PLAYER_GAME_OVER_STATUS,x ; load player game over status (0 = not game over, 1 = game over)
    bne @next_player              ; move to next player if current player is in game over (or 1 player game)
    lda PLAYER_SPRITE_X_POS,x     ; load player X position
    cmp #$a0                      ; compare to rightmost 37.5% of screen
    bcc @next_player              ; move to next player if current player not in rightmost 37.5% of screen
    iny                           ; increment number of players in rightmost part of screen

@next_player:
    dex                     ; decrement player index
    bpl @player_loop        ; loop if now on player 1
    tya                     ; finished looping through players,
                            ; transfer number of players in rightmost portion of screen to a
    bne @set_scroll_exit    ; branch if right-most part of screen has at least one player to set X auto-scroll
    beq @clear_x_autoscroll ; no players on right edge, always branch branch to stop X auto-scroll

; scroll horizontally until X_SCROLL overflows,
; i.e. scroll to the right until next nametable is shown
@mode_1:
    lda X_SCROLL         ; load PPU horizontal scroll
    bne @set_scroll_exit ; branch to continue set auto-scrolling
                         ; if haven't rolled over the X_SCROLL
                         ; i.e. haven't revealed all of next nametable

@clear_x_autoscroll:
    lda #$00
    sta X_AUTOSCROLL ; stop auto-scroll
    beq @exit        ; always exit

@set_scroll_exit:
    lda #$01
    sta X_SCROLL_SPEED ; how much to scroll horizontally the screen this frame (#00 - no scroll)

@exit:
    rts

; calculate level 4 (Base Area 3) Y scroll speed when elevator enabled
; contains !(UNUSED) logic to vary rate of speed of elevator based on players jumping and game over state
set_elevator_vel:
    lda ELEVATOR_ENABLED        ; see if elevator enabled
                                ; enabled in level_4_y_tile_routine_03
                                ; when player passes Y_SCREEN #$08, Y_SCROLL #$40 (see level_4_y_tile_routine_pts_tbl)
    beq elevator_speed_exit     ; exit if elevator is not enabled/moving
    and #$c0                    ; !(OBS) only bit 7 is ever used, but this line keeps bit 6 and 7
    sta $00                     ; set elevator enabled value
    eor LEVEL_Y_SCROLL_FLAGS    ; checking to see if bit 7 is set (cannot scroll up)
    and $00                     ; !(OBS) elevator is always enabled here, just looking to see if can scroll up
    beq elevator_speed_exit     ; exit if cannot scroll up or elevator disabled
    lda ELEVATOR_VEL_ACCUM      ; elevator is enabled and can scroll up
    clc                         ; clear carry in preparation for addition, apply velocity to Y scroll
    adc ELEVATOR_FRACT_VEL
    sta ELEVATOR_VEL_ACCUM      ; ELEVATOR_VEL_ACCUM = PLAYER_Y_FRACT_VELOCITY + ELEVATOR_VEL_ACCUM
    lda #$00
    adc ELEVATOR_FAST_VEL       ; adding any overflow
    sta Y_SCROLL_SPEED          ; set Y scroll speed
    lda ELEVATOR_ENABLED
    lsr
    bcc elevator_speed_exit     ; always exits since ELEVATOR_ENABLED is always only #$80 or #$00
    lda PLAYER_GAME_OVER_STATUS ; !(UNUSED)
                                ; load p1 game over status (0 = not game over, 1 = game over)
    bne @check_p2_status        ; branch to check p2 status if p1 in game over
    lda PLAYER_JUMP_STATUS      ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    beq @speed_up               ; player 1 not in game over and not jumping, speed up elevator

; p1 in game over, or p1 jumping
@check_p2_status:
    lda PLAYER_GAME_OVER_STATUS+1 ; load p2 game over status (0 = not game over, 1 = game over, or 1 player game)
    bne @slow_down
    lda PLAYER_JUMP_STATUS+1      ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    bne @slow_down

; speed up elevator speed
; p1 not in game over and not jumping
; or
; p1 in game over and p2 not jumping
@speed_up:
    lda ELEVATOR_FRACT_VEL
    sbc #$09
    sta ELEVATOR_FRACT_VEL  ; speed up elevator by 0.3125
    lda ELEVATOR_FAST_VEL
    sbc #$00
    sta ELEVATOR_FAST_VEL   ; subtract any underflow
    cmp #$fd
    bcs elevator_speed_exit ; branch if not yet at fastest speed (-3)
    lda #$fd                ; !(OBS) if this code were enabled
    sta ELEVATOR_FAST_VEL   ; -3 is way faster than normal speed of -0.25
    lda #$00
    sta ELEVATOR_FRACT_VEL  ; set elevator speed to -3
    rts

; slow down elevator until it stops
; p1 and p2 in game over
; or
; p1 in game over and p2 jumping
@slow_down:
    lda ELEVATOR_FRACT_VEL
    ora ELEVATOR_FAST_VEL
    beq elevator_speed_exit ; exit if elevator is stationary
    lda ELEVATOR_FRACT_VEL
    clc                     ; clear carry in preparation for addition
    adc #$08
    sta ELEVATOR_FRACT_VEL
    lda ELEVATOR_FAST_VEL
    adc #$00
    sta ELEVATOR_FAST_VEL   ; slow down by 0.3125
    bmi elevator_speed_exit ; exit if elevator is still moving up
    lda #$00                ; elevator slowed down too much, set stationary
    sta ELEVATOR_FRACT_VEL
    sta ELEVATOR_FAST_VEL

elevator_speed_exit:
    rts

; for 2 player games, if a player is in game over state, and player presses 'A',
; then a life is taken from the other player has at least 2 lives
check_transfer_life:
    lda PLAYER_COUNT        ; 0 = 1 player game, 1 = 2 player game
    beq elevator_speed_exit ; exit if single player game
    ldx #$01                ; initialize player index for loop

@loop:
    lda PLAYER_GAME_OVER_STATUS,x ; load player game over status (0 = not game over, 1 = game over)
    bne @player_game_over         ; branch if found game over player
    dex                           ; move to next player
    bpl @loop                     ; branch if moved to player 1
    bmi elevator_speed_exit       ; looped through players and didn't find game over player, exit

@player_game_over:
    txa                     ; transfer game over player index to a
    eor #$01                ; determine other player index
    tay                     ; transfer non-game over player index to y
    lda PLAYER_NUM_LIVES,y  ; load the number of lives for the non-game over player
    cmp #$02                ; see if have 3 lives or more
    bcs @continue           ; branch if player has 3 lives or more
    lda PLAYER_STATE,y      ; non-game over player has 2 lives or less
    cmp #$02                ; see if non-game over player in normal state
    bne elevator_speed_exit ; exit if non-game over player is not in normal state

; non-game over player has at least 3 lives or is normal state
; !(OBS) there are some unnecessary checks here that just happened
@continue:
    lda CONTROLLER_STATE_DIFF,x ; load game-over player controller input
    and #$80                    ; see if 'A' button is pressed
    beq elevator_speed_exit     ; exit if not pressed
    txa                         ; game over player pressed 'A', transfer game over player index to a
    eor #$01                    ; determine other player index
    tay                         ; transfer non-game over player index to y
    lda PLAYER_NUM_LIVES,y      ; load the number of lives for the non-game over player
    beq elevator_speed_exit     ; exit if non-game over player in on their last life
    cmp #$01                    ; see if have 2 lives or more
    bne @begin_swap_life        ; branch if non-game over player has more than 2 lives
    lda PLAYER_STATE,y          ; non-game over player only has 2 lives, load non-game over player state
    cmp #$02                    ; see if non-game over player in normal state
    bne elevator_speed_exit     ; exit if non-game over player is not in normal state

; take one life away from non-game over player and give it to the player in game over state
@begin_swap_life:
    lda PLAYER_NUM_LIVES,y ; load the number of lives for the non-game over player
    sec                    ; set carry flag in preparation for subtraction
    sbc #$01               ; subtract life from non-game over player
    cmp #$ff
    bne @swap_life         ; continue if non-game over player still has another life to take
                           ; should always happen, since it was just confirmed that non-game over player
                           ; has at least 2 lives a few lines above
    lda #$00               ; if somehow non-game over player was on last life,
                           ; just give free life to game over player without taking from non-game over player
                           ; just checked above, so shouldn't happen

@swap_life:
    sta PLAYER_NUM_LIVES,y   ; set new number of lives for non-game over player
    lda #$01
    sta PLAYER_NUM_LIVES,x   ; give 1 life to game-over player
    jmp player_new_life_init ; set default weapon, new life invincibility timer, and set player state to #$01

; input
;  * $11 - player index
; output
;  * carry flag - set when player state greater than #$02, clear otherwise
;  * y - player tested for initialization
is_player_initialized:
    ldy $11                       ; load other player index (0 = p1, 1 = p2)
    lda PLAYER_GAME_OVER_STATUS,y ; load player game over status (0 = not game over, 1 = game over)
    bne @exit
    lda PLAYER_STATE,y            ; load other player state
    cmp #$02                      ; see to initialized state
    rts

@exit:
    clc
    rts

; decrements new life invincibility timer every frame
; decrements invincibility timer every 4 frames
; input
;  * x - player index (0 = p1, 1 = p2)
decrement_invincibility_timers:
    lda NEW_LIFE_INVINCIBILITY_TIMER,x
    beq @dec_invincibility_timer
    dec NEW_LIFE_INVINCIBILITY_TIMER,x ; decrement timer every frame if non-zero

@dec_invincibility_timer:
    lda INVINCIBILITY_TIMER,x
    beq @exit                 ; exit if no invincibility
    lda GLOBAL_TIMER          ; load global timer
    and #$03
    bne @exit                 ; only decrement every 4 frames
    dec INVINCIBILITY_TIMER,x ; decrement invincibility timer every 4 frames

@exit:
    rts

; !(UNUSED)
init_weapon_pos_player_attrs:
    lda #$00
    sta PLAYER_CURRENT_WEAPON,x
    sta PLAYER_SPRITE_Y_POS,x
    sta PLAYER_SPRITE_X_POS,x
    sta NEW_LIFE_INVINCIBILITY_TIMER,x

; initializes player variables to #$00
; input
;  * x - player index (0 = p1, 1 = p2)
init_player_attributes:
    lda #$00
    sta PLAYER_SPRITE,x
    sta PLAYER_SPRITE_ATTR,x
    sta PLAYER_Y_ACCUM,x
    sta PLAYER_X_ACCUM,x
    sta PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_FAST_VELOCITY,x
    sta PLAYER_X_FRACT_VELOCITY,x
    sta PLAYER_X_FAST_VELOCITY,x
    sta PLAYER_JUMP_STATUS,x
    sta JUMP_INPUT,x
    sta PLAYER_SURFACE,x
    sta PLAYER_ANIMATION_FRAME_INDEX,x
    sta PLAYER_ANIM_FRAME_TIMER,x
    sta PLAYER_AIM_DIR,x
    sta PLAYER_RECOIL_TIMER,x
    sta PLAYER_M_WEAPON_FIRE_TIME,x
    sta PLAYER_STATE_TIMER,x
    sta PLAYER_SKIP_COLLISION,x
    sta PLAYER_ACTION_STATE,x
    sta F_WEAPON_CHARGE,x
    sta PLAYER_GAME_OVER_STATUS,x
    sta PLAYER_X_POS,x
    sta PLAYER_Y_POS,x
    sta PLAYER_AUTO_MOVE_CHECKPOINT,x
    sta PLAYER_OVERHEAD_DIR,x
    sta INVINCIBILITY_TIMER,x
    sta BOSS_DEFEATED_PLAYER_ANIM_SOUND,x
    sta PLAYER_APPLY_VEL,x
    rts

; run player state routine for player
; either 1 player game, or the player is to the right of the the other player
; this player would be the cause of horizontal scroll if scrolling occurs
; input
;  * x - player index (0 = p1, 1 = p2)
;  * $10 - current (rightmost) player index (0 = p1, 1 = p2)
;  * $11 - other (leftmost) player index (0 = p1, 1 = p2) (see is_player_initialized)
run_scroll_player_state_routine:
    lda #$00                     ; indicate player would be the cause of any horizontal scroll
    beq run_player_state_routine ; always branch

; run player state routine for player
; 2 player game and this player is to the left of the other player
; this player would not be the cause of any horizontal scroll,
; but they could prevent scrolling if they are too far to the left
; input
;  * x - player index (0 = p1, 1 = p2)
;  * $10 - current (leftmost) player index (0 = p1, 1 = p2)
;  * $11 - other (rightmost) player index (0 = p1, 1 = p2) (see is_player_initialized)
run_no_scroll_player_state_routine:
    lda #$01 ; indicate that player isn't the cause of any horizontal scroll

; compare to run_player_y_scroll_routine
; input
;  * a - PLAYER_APPLY_VEL,x value
;    * 0 = player is to right of other player (or 1 player game), should scroll
;      when past horizontal center
;    * 1 = player is to the left of the other player, should apply velocity to
;      to player instead of scrolling when past horizontal center
;  * x - player index (0 = p1, 1 = p2)
;  * $10 - current player index (0 = p1, 1 = p2)
;  * $11 - other player index (0 = p1, 1 = p2) (see is_player_initialized)
run_player_state_routine:
    sta PLAYER_APPLY_VEL,x                ; set whether player should cause scroll or
                                          ; whether player should have velocity applied
                                          ; when player past horizontal center of screen
    lda PLAYER_SPRITE_X_POS,x
    sta PLAYER_X_POS,x
    lda OVERHEAD_FLAG                     ; (0 = side view, 1 = overhead view)
    beq @run_routine
    jmp run_overhead_player_state_routine

@run_routine:
    lda PLAYER_STATE,x
    jsr run_routine_from_tbl_below

player_state_routine_ptr_tbl:
    .addr player_state_routine_00 ; set game over state, disable player-enemy collisions, set player position, set invisible
    .addr player_state_routine_01 ; find drop in point, used for brief time between state #$05 and #$02
    .addr player_state_routine_02 ; normal alive state, handle weapon fire and movement, set player sprite and aim dir
    .addr player_state_routine_03 ; player death, initiated from kill_player, initiate fall backwards animation
    .addr player_state_routine_04 ; player death, animate falling back, set X velocity, apply gravity, check for landing
    .addr player_state_routine_05 ; player dead on ground
    .addr player_state_routine_06 ; level 1 helicopter drop animation delay
    .addr player_state_routine_07 ; level 1 dropping into level
    .addr player_state_routine_08

; set game over state, disable player-enemy collisions, set player position, set invisible
player_state_routine_00:
    lda #$01
    sta PLAYER_GAME_OVER_STATUS,x ; set player in game over
    sta PLAYER_SKIP_COLLISION,x   ; disable player-enemy collision testing
    lda #$ff                      ; set player Y position to bottom of screen
    bit LEVEL_Y_SCROLL_FLAGS
    bpl @set_player_y_pos         ; branch if can scroll up
    lda #$00                      ; scrolling down, set player position to top of screen

@set_player_y_pos:
    sta PLAYER_Y_POS,x ; set game over player position to top or bottom of screen

player_state_routine_08:
    lda #$f0
    sta PLAYER_SPRITE_Y_POS,x ; set position to bottom of screen
    lda #$00
    sta PLAYER_SPRITE,x       ; set invisible
    sta PLAYER_X_POS,x        ; set position to leftmost part of screen
    rts

; find drop in point, used for brief time between state #$05 and #$02
; compare oh_p_state_routine_01
player_state_routine_01:
    jsr init_player_attributes    ; initialize player variables to #$00
    lda #$60
    sta PLAYER_SPRITE_Y_POS,x     ; set player Y position
    lda CURRENT_LEVEL             ; load current level
    asl                           ; double since each entry in lvl_spawn_x_pos_tbl is #$02 bytes (1 per player)
    sta $00
    txa                           ; transfer player index to a
    clc                           ; clear carry in preparation for addition
    adc $00                       ; add player index to lvl_spawn_x_pos_tbl offset
    tay                           ; transfer to offset register
    lda X_SCROLL                  ; load PPU horizontal scroll
    and #$0f
    sta $00                       ; store to later be used to offset scroll when setting spawn position
    lda lvl_spawn_x_pos_tbl,y
    sec                           ; set carry flag in preparation for subtraction
    sbc $00                       ; subtract low nibble from X scroll from spawn X position
    sta PLAYER_SPRITE_X_POS,x     ; set spawn X position
    jsr find_player_drop_in_x_pos ; find position for player to drop in
    bcc player_state_routine_exit ; exit if no collision found at spawn position
                                  ; practically this can never happen, game will fall back to level spawn X position
                                  ; when no suitable position found

; called for player death or when level 1 helicopter drop delay has elapsed
; initialize player attributes, decrement number of lives, move to next player state routine
init_player_attr_adv_player_state:
    jsr init_player_attributes ; initialize player variables to #$00
    dec PLAYER_NUM_LIVES,x     ; decrement player number of lives

inc_player_state_exit:
    inc PLAYER_STATE,x ; move to player_state_routine_02
                       ; or player_state_routine_07 for level 1 intro player helicopter drop animation

player_state_routine_exit:
    rts

; byte 0 is player 1 initial X spawn position
; byte 1 is player 2 initial X spawn position
lvl_spawn_x_pos_tbl:
    .byte $48,$30 ; level 1 Fort Firestorm
    .byte $48,$30 ; level 2 First Base
    .byte $48,$30 ; level 3 Jungle
    .byte $48,$30 ; level 4 Inner Base
    .byte $3c,$24 ; level 5 The Cliff
    .byte $48,$30 ; level 6 Entry to HQ
    .byte $48,$30 ; level 7 Headquarters
    .byte $48,$30 ; level 8 The Final Stage

; normal alive state
; handle weapon fire and movement, apply gravity, set player sprite, set aim dir, check if clipped
; compare oh_p_state_routine_02
player_state_routine_02:
    jsr handle_weapon_fire         ; determines if should create player bullets, and creates them if needed
    jsr player_handle_movement     ; handles player input that changes direction
                                   ; decrement invincibility timers, handle jump, apply gravity, calc and set X and Y velocities
    jsr calc_and_set_player_sprite ; determine and set the player sprite and attribute
    jsr set_player_aim_dir         ; set PLAYER_AIM_DIR,x based on player input (and surface)
    jmp kill_player_if_clipped     ; test if a player has clipped into a bg collision
                                   ; if so, kill the player

; !(UNUSED)
player_state_routine_02_exit:
    rts

; player hit, initiate fall back animation by setting player velocity, and animation timer
; initiated from kill_player
; compare oh_p_state_routine_03
player_state_routine_03:
    lda #$80
    sta PLAYER_JUMP_STATUS,x             ; set falling backwards
    lda #$c0
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda #$fd
    sta PLAYER_Y_FAST_VELOCITY,x         ; set Y velocity to -2.25
    lda #$00
    sta PLAYER_ANIMATION_FRAME_INDEX,x
    lda #$06
    sta PLAYER_ANIM_FRAME_TIMER,x
    jsr set_player_death_sprite          ; set player death sprite based on player animation frame index
                                         ; sprite attribute (palette and facing direction) is set as well
    jsr side_level_apply_x_vel_or_scroll ; apply player X velocity, or scroll screen based on player X velocity and other conditions
    jmp inc_player_state_exit            ; advance to player_state_routine_04

; player death, animate falling back, set X velocity, apply gravity, check for landing on ground or bottom of screen
; compare oh_p_state_routine_04
player_state_routine_04:
    ldy #$02                 ; assume facing right
    lda PLAYER_SPRITE_ATTR,x ; load facing direction when collided with enemy
    asl
    bpl @player_x_vel        ; branch if bit 6 of PLAYER_SPRITE_ATTR,x is clear, indicating facing right
    ldy #$00                 ; facing left

@player_x_vel:
    lda player_death_fall_x_vel_tbl,y   ; load X fractional velocity
    sta PLAYER_X_FRACT_VELOCITY,x
    lda player_death_fall_x_vel_tbl+1,y ; load X fast velocity
    sta PLAYER_X_FAST_VELOCITY,x
    lda PLAYER_SPRITE_Y_POS,x
    cmp #$f0
    bcc @player_y_vel                   ; branch if not at bottom of screen
    lda #$00                            ; dead player fell to bottom of screen
    jsr set_player_sprite_far           ; set player sprite and sprite attribute (accounting for invincibility)
    jmp clear_player_vel_adv_state      ; clear player velocity, set state timer, and advance player state

@player_y_vel:
    lda PLAYER_Y_FRACT_VELOCITY,x
    clc                           ; clear carry in preparation for addition
    adc #$20
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda PLAYER_Y_FAST_VELOCITY,x
    adc #$00
    sta PLAYER_Y_FAST_VELOCITY,x  ; apply gravity (0.125)
    bmi @continue                 ; branch if player is falling down (after initial rise)
    cmp #$04
    bcc @continue                 ; branch if not falling too fast
    lda #$00                      ; falling really fast
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda #$04
    sta PLAYER_Y_FAST_VELOCITY,x  ; cap max fall Y velocity to -4

@continue:
    jsr animate_player_death             ; animate player frame index up to frame #$05, then stay at frame #$05
    jsr side_level_apply_x_vel_or_scroll ; apply player X velocity, or scroll screen based on player X velocity and other conditions
    lda PLAYER_SURFACE,x                 ; load the kind of surface the player is standing on
    beq player_state_routine_exit2       ; exit if not on surface

; clear player velocity, set state timer, and advance player state
; input
;  * x - player offset (0 = p1, 1 = p2)
clear_player_vel_adv_state:
    lda #$00
    sta PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_FAST_VELOCITY,x
    sta PLAYER_X_FRACT_VELOCITY,x
    sta PLAYER_X_FAST_VELOCITY,x
    lda #$60
    sta PLAYER_STATE_TIMER,x      ; set player death lying on ground timer
    jmp inc_player_state_exit

player_state_routine_exit2:
    rts

; animate player frame index (every 6 frames) up to frame #$05, then stay at frame #$05
; input
;  * x - player offset (0 = p1, 1 = p2)
animate_player_death:
    dec PLAYER_ANIM_FRAME_TIMER,x      ; decrement player animation timer
    bne player_state_routine_exit2     ; exit if timer not elapsed
    lda #$06                           ; timer elapsed, move up to next frame in animation
    sta PLAYER_ANIM_FRAME_TIMER,x      ; reset animation timer
    inc PLAYER_ANIMATION_FRAME_INDEX,x
    lda PLAYER_ANIMATION_FRAME_INDEX,x
    cmp #$05                           ; see if past last frame
    bcc @continue                      ; set animation index if not at max
    lda #$05                           ; stay at last frame (sprite_22 - player dead on ground)

@continue:
    sta PLAYER_ANIMATION_FRAME_INDEX,x

; sets player death sprite based on player animation frame index
; input
;  * x - player index (0 = p1, 1 = p2)
set_player_death_sprite:
    ldy PLAYER_ANIMATION_FRAME_INDEX,x
    lda player_death_sprites_tbl,y

; sets player sprite and attribute (palette portion only)
; sets player palette based on player index
; flashes player invisible if just revived
; input
;  * a - sprite code
set_player_sprite_far:
    jmp set_player_sprite ; set player sprite and sprite attribute (accounting for invincibility)

player_death_fall_x_vel_tbl:
    .byte $40,$01 ;  1.25 (player facing left when killed)
    .byte $c0,$fe ; -1.25 (player facing right when killed)

player_death_sprites_tbl:
    .byte $1e ; player_sprite_1e
    .byte $1f ; player_sprite_1f
    .byte $20 ; player_sprite_20
    .byte $21 ; player_sprite_21
    .byte $1e ; player_sprite_1e
    .byte $22 ; player_sprite_22

; dead on ground
; compare oh_p_state_routine_05
player_state_routine_05:
    jsr animate_player_death ; animate player frame index up to frame #$05, then stay at frame #$05

; clear velocity, wait for lying on ground timer to elapse, decrement number of lives
; initialize player for new life
wait_for_spawn_delay:
    lda #$00
    sta PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_FAST_VELOCITY,x
    sta PLAYER_X_FRACT_VELOCITY,x
    sta PLAYER_X_FAST_VELOCITY,x
    jsr side_level_apply_x_vel_or_scroll ; apply player X velocity, or scroll screen based on player X velocity and other conditions
    dec PLAYER_STATE_TIMER,x             ; decrement player death lying on ground timer
    bne player_state_routine_exit2
    lda #$00
    ldy PLAYER_NUM_LIVES,x
    beq set_player_state_clear_timer     ; branch if no more lives to set player state to 0 and clear PLAYER_STATE_TIMER,x timer

; init player's weapon to default, set new life invincibility timer,
; and set player state to #$01
; input
;  * x - player index (0 = p1, 1 = p2)
player_new_life_init:
    lda #$00
    sta PLAYER_CURRENT_WEAPON,x        ; set default weapon
    lda #$80
    sta NEW_LIFE_INVINCIBILITY_TIMER,x ; set new life invincibility to #$80
    lda #$01

; sets player state to a and clears PLAYER_STATE_TIMER,x timer
; input
;  * a - new player state
;  * x - player index (0 = p1, 1 = p2)
set_player_state_clear_timer:
    sta PLAYER_STATE,x
    lda #$00
    sta PLAYER_STATE_TIMER,x ; clear player death lying on ground timer

player_state_routine_exit3:
    rts

; level 1 helicopter drop animation delay
player_state_routine_06:
    dec PLAYER_STATE_TIMER,x              ; decrement delay before player begins drop
    bne player_state_routine_exit3        ; exit if delay hasn't elapsed
    jsr init_player_attr_adv_player_state ; initialize player attributes, decrement number of lives
                                          ; and set player state to player_state_routine_07
    lda #$40
    sta PLAYER_SPRITE_ATTR,x              ; flip player sprite horizontally
    lda #$34
    sta PLAYER_STATE_TIMER,x              ; set timer for end reaching end of rope
    rts

; level 1 dropping into level
player_state_routine_07:
    lda #$00
    sta CONTROLLER_STATE,x         ; prevent any controller input, while animating level 1 drop in
    jsr player_handle_movement     ; handles player input that changes direction
                                   ; decrement invincibility timers, handle jump, apply gravity, calc and set X and Y velocities
    jsr calc_and_set_player_sprite ; determine and set the player sprite and attribute
    dec PLAYER_STATE_TIMER,x       ; decrement drop from helicopter timer (p2 is longer)
    beq @end_of_rope               ; branch if timer elapsed indicating player has reached end of rope
                                   ; and will now 'jump' to the right
    lda PLAYER_Y_FAST_VELOCITY,x   ; still sliding down rope
    bmi @continue
    cmp #$03
    bcc @continue
    lda #$03
    sta PLAYER_Y_FAST_VELOCITY,x   ; cap maximum player Y velocity to #$03

@continue:
    lda PLAYER_STATE_TIMER,x  ; load player death lying on ground timer
    cmp #$24
    bcs @exit                 ; exit if still waiting to drop from helicopter
    lda #$73                  ; about to start sliding down rope
    sta PLAYER_SPRITE_X_POS,x ; set player X position to #$73
    lda PLAYER_SPRITE_ATTR,x
    and #$bf
    sta PLAYER_SPRITE_ATTR,x  ; strip horizontal sprite flip bit

@exit:
    rts

@end_of_rope:
    lda #$01                         ; right d-pad
    sta JUMP_INPUT,x                 ; have player being moving right during drop down
    txa                              ; transfer player offset to a
    asl                              ; double since each entry is #$02 bytes
    tay                              ; transfer value (0 or 2) to offset register
    lda @drop_part_2_y_vel_tbl,y     ; load player x's fractional velocity
    sta PLAYER_Y_FRACT_VELOCITY,x    ; store player x's fractional velocity
    lda @drop_part_2_y_vel_tbl+1,y   ; load player x's fast velocity
    sta PLAYER_Y_FAST_VELOCITY,x     ; store player x's fast velocity
    lda #$02                         ; player state 2 (normal alive state)
    bne set_player_state_clear_timer ; always branch to set state to player_state_routine_02

@drop_part_2_y_vel_tbl:
    .byte $40,$ff ; player 1 (-0.75)
    .byte $00,$00 ; player 2 (0)

; handles player input that changes direction
;  * prevent moving left/right when ducking in water
;  * decrement invincibility timers (new life and B weapon timers)
;  * initiate jump velocity if 'A' button pressed
;  * fall through ledge if 'A' pressed and crouched and appropriate
;  * apply gravity
;  * calc and set X velocity based on d-pad and surface
;  * calc and set Y velocity based on d-pad if in the air
; only runs on side-view levels
; input
;  * x - player index (0 = p1, 1 = p2)
player_handle_movement:
    lda PLAYER_SURFACE,x   ; load the kind of surface the player is standing on
    cmp #$04               ; see if player in water
    bne @continue
    lda CONTROLLER_STATE,x ; player is in water
    and #$04               ; down d-pad input
    beq @continue          ; branch if player isn't pressing down
    lda CONTROLLER_STATE,x ; player pressing down, load controller input
    and #$fc
    sta CONTROLLER_STATE,x ; strip left and right
                           ; to prevent moving while crouched in water

@continue:
    jsr decrement_invincibility_timers ; decrement new life invincibility and/or B weapon invincibility
    lda PLAYER_JUMP_STATUS,x           ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    bne @apply_gravity
    lda CONTROLLER_STATE_DIFF,x
    and #$80
    bne @jump_button                   ; branch if 'A' button pressed (jump button)
    jmp @no_jump_set_player_x_vel      ; calculate and set player X velocity based on d-pad and surface

; player is jumping, or falling through floor (down and jump)
@jump_button:
    lda CONTROLLER_STATE,x        ; load controller input
    and #$0f                      ; strip to just d-pad input
    cmp #$04                      ; see if down button pressed
    bne @set_jump_vel_continue    ; branch if down button not pressed
    lda PLAYER_SURFACE,x          ; down button is pressed
                                  ; load the kind of surface the player is jumping from (with down button pressed)
    cmp #$01                      ; see if from a floating (flat) platform
    bne @set_jump_vel_continue    ; branch if not from a floating (flat) platform
    bit LEVEL_Y_SCROLL_FLAGS      ; on a floating (flat) platform (example base of elevator)
                                  ; see if should check for platform to fall through on
    bvc @fall_through_ledge       ; branch if bit 6 clear, indicating can scroll down (skips platform check)
                                  ; !(OBS) I'm not sure this specific condition ever happens
    lda PLAYER_SPRITE_Y_POS,x     ; cannot scroll down, need to confirm platform exists to fall through on
    clc                           ; clear carry in preparation for addition
    adc #$15                      ; add #$15 to player Y position
    clc                           ; clear carry in preparation for addition
    adc #$10                      ; !(OBS) could be optimized to a single adc #$25
    jsr find_bg_collision_below_a ; check for bg collision beneath player
    bcs @set_jump_vel_continue    ; branch if no platform beneath player to jump instead of fall through

; falling through floating ledge
@fall_through_ledge:
    lda PLAYER_SPRITE_Y_POS,x
    clc
    adc #$10
    sta PLAYER_SPRITE_Y_POS,x     ; add #$10 to Y position
    jsr set_fall_from_ledge_input ; set jump status to indicate falling from ledge
                                  ; and set JUMP_INPUT,x based on controller input
    jmp @apply_gravity

; can't fall through ledge
@set_jump_vel_continue:
    ldy #$00                 ; default jump velocity index (-4.0625)
    lda PLAYER_SURFACE,x     ; load the kind of surface the player is standing on
    cmp #$04                 ; see if player in water
    bne @set_init_jump_y_vel ; branch if player not in water
    ldy #$02                 ; player in water, set Y fractional velocity index
                             ; (-3)

; executed once during start of jump
@set_init_jump_y_vel:
    lda player_jump_initial_y_vel_tbl,y   ; load initial jump Y fractional velocity
    sta PLAYER_Y_FRACT_VELOCITY,x         ; set initial jump Y fractional velocity
    lda player_jump_initial_y_vel_tbl+1,y ; load initial jump Y fast velocity
    sta PLAYER_Y_FAST_VELOCITY,x          ; set initial jump Y fast velocity
    lda #$00
    sta PLAYER_ANIMATION_FRAME_INDEX,x    ; set initial frame of the jump animation
    lda #$80
    sta PLAYER_JUMP_STATUS,x              ; set player jumping flag
    lda CONTROLLER_STATE,x                ; load controller input
    sta JUMP_INPUT,x                      ; set input during initial jump

@apply_gravity:
    lda PLAYER_Y_FRACT_VELOCITY,x
    clc                           ; clear carry in preparation for addition
    adc #$23
    sta PLAYER_Y_FRACT_VELOCITY,x ; apply gravity
    lda PLAYER_Y_FAST_VELOCITY,x
    adc #$00
    sta PLAYER_Y_FAST_VELOCITY,x  ; add any overflow from fractional velocity
    bmi @continue_jump            ; branch if moving up
    cmp #$05
    bcc @continue_jump            ; branch if falling velocity is less than #$05
    lda #$05                      ; player falling very fast, cap falling velocity at #$05
    sta PLAYER_Y_FAST_VELOCITY,x  ; set max falling velocity to #$05
    lda #$00
    sta PLAYER_Y_FRACT_VELOCITY,x ; clear Y fractional velocity

@continue_jump:
    lda CONTROLLER_STATE,x ; load controller input
    and #$03               ; d pad left and right
    beq @set_jump_input    ; branch if not pressing left nor right
    sta JUMP_INPUT,x       ; store left-right d-pad input

@set_jump_input:
    lda #$04                    ; set right direction X velocity index
    sta $08                     ; maps to X velocity of 1
    lda #$08                    ; set left direction X velocity index
    sta $09                     ; maps to X velocity of -1
    lda JUMP_INPUT,x            ; load d-pad input
    jsr set_player_x_vel        ; set player's X velocity based on d-pad input
    jmp @set_player_sprite_attr

@no_jump_set_player_x_vel:
    jsr calc_player_x_vel_index   ; determine left and right velocity indexes for set_player_x_vel
    lda CONTROLLER_STATE,x        ; load controller input
    jsr set_player_x_vel          ; set player's X velocity based on d-pad input
    lda player_jump_vel_tbl+2,y
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda player_jump_vel_tbl+3,y
    sta PLAYER_Y_FAST_VELOCITY,x

; sets player sprite attribute
; input
;  * x - player index (0 = p1, 1 = p2)
;  * $13 - controller input
@set_player_sprite_attr:
    ldy #$fc             ; initial sprite attribute
    lda $13              ; load controller input
    and #$03             ; strip to just d-pad left and right input
    beq @face_right      ; branch if neither left nor right is pressed
    ldy #$3c             ; sprite attribute without sprite flip nor palette
    lsr                  ; push d-pad right button bit to carry
    lda #$40             ; assume only left button pressed, flip sprite horizontally (facing left)
    bcc @set_sprite_attr ; branch if left button pressed

@face_right:
    lda #$00 ; do not flip sprite (facing right)

@set_sprite_attr:
    sta $08                  ; set sprite horizontal flip bit
    tya                      ; transfer new sprite attribute to a
    and PLAYER_SPRITE_ATTR,x
    ora $08                  ; merge sprite horizontal flip bit
    sta PLAYER_SPRITE_ATTR,x ; set sprite attribute including horizontal flip

; applies X velocity for player, or scrolls right based on conditions
; input
;  * x - player index (0 = p1, 1 = p2)
;  * $11 - other player index (on 1 player game, will be 1)
side_level_apply_x_vel_or_scroll:
    lda PLAYER_AUTO_MOVE_CHECKPOINT,x ; end of level auto movement, check if passed first trigger point
    bne @adjust_vel_apply_scroll      ; branch if passed the first trigger point
    lda #$0c                          ; not yet at trigger point
    sta $0f                           ; for use in get_bg_collision_y_range
    lda #$04                          ; adjust player sprite collision Y test point by #$04
    ldy PLAYER_JUMP_STATUS            ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    beq @continue                     ; branch if not jumping, nor falling off ledge
    lda #$08                          ; player falling off edge or jumping
                                      ; adjust player sprite collision Y test point by #$08

@continue:
    clc                           ; clear carry in preparation for addition
    adc PLAYER_SPRITE_Y_POS,x     ; add either #$04 or #$08 to player Y position depending on if player is jumping
    tay                           ; transfer Y collision test point to y
    lda PLAYER_X_FAST_VELOCITY,x
    ora PLAYER_X_FRACT_VELOCITY,x
    beq @adjust_vel_apply_scroll  ; branch if player is not moving horizontally
    lda PLAYER_X_FAST_VELOCITY,x  ; player moving horizontally, load X fast velocity
    asl
    lda #$0a                      ; look to the right by #$0a pixels for a background collision
    bcc @check_collision_continue ; branch if moving to the right
    lda #$f6                      ; moving left, look to the left by #$0a pixels for a background collision

@check_collision_continue:
    clc                           ; clear carry in preparation for addition
    adc PLAYER_SPRITE_X_POS,x     ; add or subtract #$0a to player collision X test point
    jsr get_bg_collision_y_range  ; get bg collision in front of player
    bcc @adjust_vel_apply_scroll  ; branch if no background collision
                                  ; to adjust velocity for scroll and apply velocity if appropriate
    lda #$00                      ; background collision, stop horizontal velocity
    sta PLAYER_X_FRACT_VELOCITY,x
    sta PLAYER_X_FAST_VELOCITY,x
    sta PLAYER_X_POS,x            ; check_auto_move_checkpoints will detect this value is #$00 and jump to avoid obstacle
                                  ; !(OBS) this is subsequently set back to correct value in run_player_state_routine

@adjust_vel_apply_scroll:
    lda PLAYER_X_FAST_VELOCITY,x  ; load player X fast velocity
    sec                           ; set carry flag in preparation for subtraction
    sbc X_SCROLL_SPEED            ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    sta PLAYER_X_FAST_VELOCITY,x  ; update velocity adjusted for scroll
    ldy PLAYER_SPRITE_X_POS,x
    ora PLAYER_X_FRACT_VELOCITY,x ; merge fractional X velocity with fast X velocity
    beq @exit                     ; exit if player is not moving
    lda PLAYER_X_FAST_VELOCITY,x  ; load player X velocity
    bpl @moving_right             ; branch if moving right
    cpy #$14                      ; moving left, compare player X position to 7.8125%
    bcc @exit                     ; exit if moving left and at far left edge of screen
    lda PLAYER_SPRITE_Y_POS,x     ; load player Y position
    cmp #$16                      ; compare against top 8.59375% vertically
    bcs @apply_x_vel              ; branch if in main portion of screen
    lda PLAYER_SURFACE,x          ; load the kind of surface the player is standing on
    cmp #$09
    bcs @exit                     ; exit if moving left at top portion of screen and on a negative incline
    bcc @apply_x_vel              ; always branch to apply player X velocity

; player is moving to the right
@moving_right:
    tya                           ; transfer player X position to a
    ldy ACTIVE_PLAYERS_FLAG       ; load player active/game over statuses
    cmp #$ec                      ; compare player X position to 92.1875% of screen
    bcs @exit                     ; exit if moving right and at far edge of screen
    cmp side_level_center_x_tbl,y ; compare to horizontal center position depending on number of active players
    bcc @apply_x_vel              ; branch to apply velocity if not past 'center' of screen
                                  ; for 2 player game, the center is not 50%, but rather 59.375%
    lda SCREEN_SCROLL_TYPE        ; player at or past center, load scroll type (0 = horizontal, 1 = vertical/overhead)
    bne @apply_x_vel_if_should    ; branch if vertical scrolling to apply X velocity if not at very top on an inclined surface
    lda PLAYER_APPLY_VEL,x        ; horizontally scroll section of level
                                  ; see if 2 player game and current player is to the left of other player
                                  ; this would mean the current player cannot be the cause of any scroll
    bne @apply_x_vel              ; branch to apply X velocity if player to the left of other player (can't cause scroll)
    jsr is_player_initialized     ; set carry if other player ($11) is state #$02 or greater (active)
    bcc @apply_scroll_exit        ; branch if other player is not initialized to scroll right
    lda PLAYER_SPRITE_X_POS,y     ; other active player is initialized and to the left, see if that player is blocking scroll
                                  ; load that player's X position
    cmp #$14                      ; compare player X position to 7.8125%
    bcc @exit                     ; branch if at far left of screen
                                  ; do not scroll if leftmost player at left edge (even though rightmost player is causing a scroll)
                                  ; i.e. left player is holding right player back

; apply scroll instead of applying player X velocity
@apply_scroll_exit:
    lda PLAYER_X_ACCUM,x          ; load current accumulated PLAYER_X_ACCUM total
    clc                           ; clear carry in preparation for addition
    adc PLAYER_X_FRACT_VELOCITY,x ; a = PLAYER_X_FRACT_VELOCITY + PLAYER_X_ACCUM
    sta PLAYER_X_ACCUM,x          ; add another PLAYER_X_FRACT_VELOCITY to accumulator
    lda #$00
    adc PLAYER_X_FAST_VELOCITY,x  ; add player's X fast velocity with any fractional X velocity
    sta X_SCROLL_SPEED            ; set number of pixels to scroll horizontally this frame to match player's velocity
    jmp @exit

; apply X velocity if in main portion of screen or not on a floating inclined surface
@apply_x_vel_if_should:
    lda PLAYER_SPRITE_Y_POS,x ; load player Y position
    cmp #$16                  ; compare against top 8.59375% vertically
    bcs @apply_x_vel          ; branch if in main portion of screen
    lda PLAYER_SURFACE,x      ; player in top portion of screen, load the kind of surface the player is standing on
    cmp #$06                  ; see if player is on an inclined surface
    bcc @apply_x_vel          ; branch if player is not on an inclined surface
    cmp #$09                  ; see if player is on a floating inclined surface
    bcc @exit                 ; exit if player is on an inclined surface that isn't floating
                              ; applying no scroll nor any X velocity

; input
;  * x - player index (0 = p1, 1 = p2)
; output
;  * y - player X position
@apply_x_vel:
    lda PLAYER_X_ACCUM,x          ; load current accumulated PLAYER_X_ACCUM total
    clc                           ; clear carry in preparation for addition
    adc PLAYER_X_FRACT_VELOCITY,x ; a = PLAYER_X_FRACT_VELOCITY + PLAYER_X_ACCUM
    sta PLAYER_X_ACCUM,x          ; add another PLAYER_X_FRACT_VELOCITY to accumulator
    lda PLAYER_SPRITE_X_POS,x     ; load player's X position
    adc PLAYER_X_FAST_VELOCITY,x  ; add fast X velocity
                                  ; along with an additional 1 unit if PLAYER_X_ACCUM rolled over
    sta PLAYER_SPRITE_X_POS,x     ; set new player X position
    ldy PLAYER_X_POS,x            ; load calculated X position of player in y
    beq @exit
    sta PLAYER_X_POS,x            ; store calculated X position of player

@exit:
    rts

side_level_center_x_tbl:
    .byte $80 ; player 1 not in game over, player 2 in game over (or 1 player game)
    .byte $80 ; player 1 in game over, player 2 not in game over
    .byte $98 ; 2 player game and neither player in game over

player_jump_initial_y_vel_tbl:
    .byte $f0,$fb ; player not in water (-4.0625)
    .byte $00,$fd ; jumping from water (-3)

; sets player's X velocity based on d-pad input
; calculates index into player_jump_vel_tbl for Y velocity
; input
;  * a - controller input
;  * x - player index (0 = p1, 1 = p2)
;  * $08 - right direction velocity index into player_jump_vel_tbl, e.g. #$04
;  * $09 - left direction velocity index into player_jump_vel_tbl, e.g. #$08
; output
;  * y - index into player_jump_vel_tbl for Y velocity
;  * $13 - controller input
set_player_x_vel:
    sta $13             ; store controller input in $13
    ldy $08             ; load right direction X velocity offset
    lsr                 ; push bit 0 of controller input into carry
    bcs @set_x_velocity ; branch if right d-pad is pressed
    ldy $09             ; right isn't pressed, load $09 instead
    lsr
    bcs @set_x_velocity ; branch if left d-pad is pressed
    ldy #$00            ; neither right nor left pressed, use 0 for no X velocity

@set_x_velocity:
    lda player_jump_vel_tbl,y
    sta PLAYER_X_FRACT_VELOCITY,x
    lda player_jump_vel_tbl+1,y
    sta PLAYER_X_FAST_VELOCITY,x
    rts

; player X and Y velocity lookup table
player_jump_vel_tbl:
    .byte $00,$00 ; #$00:  0
    .byte $00,$00 ; #$02:  0
    .byte $00,$01 ; #$04:  1 (pressing right on d-pad)
    .byte $00,$00 ; #$06:  0
    .byte $00,$ff ; #$08: -1 (pressing left on d-pad)
    .byte $00,$00 ; #$0a:  0
    .byte $e0,$00 ; #$0c:  0.875  (incline)
    .byte $70,$00 ; #$0e:  0.4375 (unused)
    .byte $20,$ff ; #$10: -0.875  (incline)
    .byte $70,$00 ; #$12:  0.4375 (unused)
    .byte $e0,$00 ; #$14:  0.875  (incline)
    .byte $90,$ff ; #$16: -0.4375 (unused)
    .byte $20,$ff ; #$18: -0.875  (incline)
    .byte $90,$ff ; #$1a: -0.4375 (unused)

; determines the right ($08) and left ($09) velocity offsets based on surface
; used for setting the X velocity (see set_player_x_vel)
; input
;  * x - player index (0 = p1, 1 = p2)
calc_player_x_vel_index:
    ldy PLAYER_SURFACE,x ; load the kind of surface the player is standing on
    cpy #$06             ; see if player is on an inclined surface
    bcs @incline         ; branch if player is on an inclined surface
    lda #$04             ; player on flat surface, #$04 maps to 1 X velocity
    sta $08              ; set right direction velocity (maps to 1)
    lda #$08             ; player on flat surface
    sta $09              ; set left direction velocity (maps to -1)
    rts

@incline:
    lda incline_x_vel_index_tbl-6,y
    sta $08                         ; set right direction X velocity index

@left_dir:
    lda incline_x_vel_index_tbl,y ; load X velocity index
    sta $09                       ; set left direction X velocity index
    rts

; indexes into player_jump_vel_tbl
; !(HUH) seems like the values will always end up the same
; either -.875 for left and .875 for right
; or     -.875 for left and .875 for right
incline_x_vel_index_tbl:
    .byte $14,$14,$14,$0c,$0c,$0c ; right direction
    .byte $10,$10,$10,$18,$18,$18 ; left direction

; applies Y velocity with different checks depending on PLAYER_STATE,x
; this player cannot block Y scrolling
; the player is either below the other player and level scrolls up, or the
; player is above the other player and level scrolls down
; similar to run_scroll_player_state_routine
; input
;  * x - player index (0 = p1, 1 = p2)
;  * $10 - current (can't block Y scroll) player index (0 = p1, 1 = p2)
;  * $11 - other (could block Y scroll) player index (0 = p1, 1 = p2) (see is_player_initialized)
run_scroll_player_y_scroll_routine:
    lda #$00
    beq run_player_y_scroll_routine ; always branch to apply Y velocity with different checks depending on player state

; applies Y velocity with different checks depending on PLAYER_STATE,x
;  * x - player index (0 = p1, 1 = p2)
;  * $10 - current player index (0 = p1, 1 = p2)
;  * $11 - other player index (0 = p1, 1 = p2) (see is_player_initialized)
run_no_scroll_player_y_scroll_routine:
    lda #$01

; apply Y velocity with different checks depending on PLAYER_STATE,x
; input
;  * a - PLAYER_APPLY_VEL,x value
;    * 0 = player would be the cause of any Y scroll (or 1 player game)
;    * 1 = player would not be cause of any Y scroll
;  * x - player index (0 = p1, 1 = p2)
run_player_y_scroll_routine:
    sta PLAYER_APPLY_VEL,x                   ; set whether player should cause scroll or
                                             ; whether player should have velocity applied
                                             ; when player past vertical center of screen
    lda PLAYER_SPRITE_Y_POS,x
    sta PLAYER_Y_POS,x                       ; copy Y position to other variable
    lda OVERHEAD_FLAG                        ; (0 = side view, 1 = overhead view)
    beq @run_routine                         ; branch if side view level
    jmp run_overhead_player_y_scroll_routine

; apply Y velocity with different checks depending on player state
@run_routine:
    lda PLAYER_STATE,x
    jsr run_routine_from_tbl_below

player_y_scroll_routine_ptr_tbl:
    .addr player_y_scroll_routine_00 ; do nothing
    .addr player_y_scroll_routine_00 ; do nothing
    .addr player_y_scroll_routine_02 ; apply player's Y velocity, killing player if at bottom of screen, land player on ground/water if applicable
    .addr player_y_scroll_routine_03 ; apply player's Y velocity, land player on ground/water if applicable
    .addr player_y_scroll_routine_03 ; apply player's Y velocity, land player on ground/water if applicable
    .addr player_y_scroll_routine_05 ; hide player if at bottom of screen, otherwise apply player's Y velocity, land player on ground/water if applicable
    .addr player_y_scroll_routine_00 ; do nothing
    .addr player_y_scroll_routine_07 ; clear controller input, apply player's Y velocity, killing player if at bottom of screen, land player on ground/water if applicable
    .addr player_y_scroll_routine_00 ; do nothing

; clear controller input, and apply player's Y velocity accounting for bg collisions,
; killing player if at bottom of screen, land player on ground/water if applicable
player_y_scroll_routine_07:
  lda #$00
  sta CONTROLLER_STATE,x ; clear controller input

; apply player's Y velocity accounting for bg collisions, killing player
; if at bottom of screen, land player on ground/water if applicable
player_y_scroll_routine_02:
    jsr apply_y_vel_check_bottom ; apply velocity or scroll, kill player if at bottom of screen
    jmp check_landing            ; see if landing on ground/water, if so play sound (if ground) and stop falling

; apply player's Y velocity accounting for bg collisions
; land player on ground/water if applicable
player_y_scroll_routine_03:
    jsr apply_y_vel   ; handle player's Y velocity by applying scroll or updating Y position
                      ; accounts for bg collision
    jmp check_landing ; see if landing on ground/water, if so play sound (if ground) and stop falling

; hide player if at bottom of screen
player_y_scroll_routine_05:
    lda PLAYER_SPRITE_Y_POS,x
    cmp #$e0                       ; compare to bottom 12.5% of screen
    bcc player_y_scroll_routine_03 ; continue if not at bottom of screen
    lda #$00                       ; at bottom of screen, make player invisible
    sta PLAYER_SPRITE,x

player_y_scroll_routine_00:
    rts

; handles player's Y velocity by applying scroll or updating Y position
; accounts for bg collision
; kills player if at bottom of screen
; input
;  * x - player index (0 = p1, 1 = p2)
apply_y_vel_check_bottom:
    lda PLAYER_SPRITE_Y_POS,x ; load player x Y position
    cmp #$e0                  ; see if at bottom of screen
    bcc apply_y_vel           ; branch if not at bottom of screen
    jmp kill_player           ; player landed on bottom of screen, kill player
                              ; play player death sound and update player state for player x

; handles player's Y velocity by applying scroll or updating Y position
; accounts for bg collision
; input
;  * x - player index (0 = p1, 1 = p2)
apply_y_vel:
    lda PLAYER_Y_FAST_VELOCITY,x  ; load player's fast velocity
    sec                           ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED            ; subtract any vertical scrolling this frame
    sta $14                       ; set player's fast velocity adjusted for vertical scroll
    ora PLAYER_Y_FRACT_VELOCITY,x ; see if there is any fractional Y velocity
    bne @continue                 ; branch if Y fractional velocity or fast velocity after accounting for scroll
    jmp @exit                     ; no vertical movement, exit

@continue:
    lda PLAYER_SPRITE_Y_POS,x         ; re-load player Y position
    ldy $14                           ; load any Y velocity (after accounting for vertical scroll)
    bpl @moving_down                  ; branch if moving down
    jsr get_player_bg_collision_y     ; moving up, determine background collision at point (PLAYER_SPRITE_X_POS, y)
    beq @moving_up
    lda PLAYER_SPRITE_Y_POS,x         ; collided with background during jump, load player Y position
    clc                               ; clear carry in preparation for addition
    adc PLAYER_Y_VEL_BG_COLLISION_ADJ ; add any background collision adjustment, e.g. stomping ceiling pushing player down
                                      ; if no background collision adjustment, simply clears any vertical upward motion
    sta PLAYER_SPRITE_Y_POS,x         ; set player Y position to be affected by background collision
    lda #$00                          ; setting player to top of screen to prevent player from causing down scroll
    sta PLAYER_Y_POS,x                ; used in @invert_y_scroll_order comparison
                                      ; !(OBS) this is subsequently set back to correct value in the next frame
                                      ; in run_player_y_scroll_routine
    beq @exit                         ; always exit

; moving upward with no background collision
@moving_up:
    lda PLAYER_SPRITE_Y_POS,x     ; load player Y position
    cmp #$12
    bcc @exit                     ; exit if already at the very top of the screen
    cmp #$60
    bcs @apply_y_vel              ; branch if player in bottom 62.5% of screen
    lda PLAYER_APPLY_VEL,x        ; player in top 37.5% of screen, see if player is cause of Y scroll
    bne @apply_y_vel              ; branch if player is not the cause of scroll to simply apply player Y velocity
    lda LEVEL_Y_SCROLL_FLAGS      ; player is cause of Y scroll, see current allowable Y scroll direction
    bmi @apply_y_vel              ; branch if cannot scroll up to apply player Y velocity
    lda ELEVATOR_ENABLED          ; can scroll up, see if level 4 (Base Area 3) elevator is enabled/moving
    bne @apply_y_vel              ; branch if elevator is enabled/moving to apply player Y velocity
    lda PLAYER_Y_ACCUM,x          ; not applying Y velocity to player, but rather scrolling vertically
    clc                           ; clear carry in preparation for addition
    adc PLAYER_Y_FRACT_VELOCITY,x ; a = PLAYER_Y_FRACT_VELOCITY + PLAYER_Y_ACCUM
    sta PLAYER_Y_ACCUM,x          ; add another PLAYER_Y_FRACT_VELOCITY to accumulator
    lda #$00                      ; adding any overflow
    adc $14                       ; add any fast Y velocity (adjusted for any vertical scroll)
    sta Y_SCROLL_SPEED            ; set new vertical scroll
    jmp @exit

; input
;  * a - player x's Y position
;  * x - player index (0 = p1, 1 = p2)
@moving_down:
    ldy PLAYER_Y_VEL_BG_COLLISION_ADJ
    beq @apply_y_vel_or_scroll        ; branch to apply Y velocity to player or scroll if not being moved by bg
                                      ; for example, the stomping ceiling
    jsr get_player_bg_collision_y     ; being moved by background, determine background collision at point (PLAYER_SPRITE_X_POS, y)
    beq @apply_player_y_vel_or_scroll ; branch if no bg collision
    lda $14                           ; collided with background, load current Y fast velocity adjusted for vertical scroll
    clc
    adc PLAYER_Y_VEL_BG_COLLISION_ADJ ; add to existing Y fast velocity adjusted for vertical scroll
    sta $14                           ; set new fast velocity adjusted for vertical scroll

@apply_player_y_vel_or_scroll:
    lda PLAYER_SPRITE_Y_POS,x ; load player's Y position

@apply_y_vel_or_scroll:
    cmp LEVEL_Y_CENTER        ; see if at vertical 'center' of screen
                              ; if past center, Y velocity is applied to scroll instead of player
    bcc @apply_y_vel          ; branch to apply velocity to player if not past center
    ldy Y_SCROLL_SPEED        ; load number of pixels to Y scroll this frame
    bne @apply_y_vel          ; branch to apply velocity to player if past center, but already scrolling
    bit LEVEL_Y_SCROLL_FLAGS
    bvs @apply_y_vel          ; branch to apply velocity if cannot scroll down (bit 6 set)
    lda ELEVATOR_ENABLED      ; can scroll down if elevator disabled, see if elevator enabled
    bne @apply_y_vel          ; branch to apply velocity if elevator enabled
    jsr is_player_initialized ; can scroll down, and elevator is disabled (or no elevator)
                              ; see if other player is active
                              ; set carry if other player is state #$02 or greater (active)
    bcc @apply_y_scroll       ; branch to apply Y scroll if other player is not initialized
    lda PLAYER_SPRITE_Y_POS,y ; other player initialized, see if should block Y scrolling
    cmp #$22
    bcc @apply_y_vel          ; apply Y velocity if other player is in top 13.28125% of screen
                              ; this prevents scroll down if other player is at top of screen

@apply_y_scroll:
    lda PLAYER_Y_ACCUM,x          ; load current accumulated PLAYER_Y_ACCUM total
    clc                           ; clear carry in preparation for addition
    adc PLAYER_Y_FRACT_VELOCITY,x ; a = PLAYER_Y_FRACT_VELOCITY + PLAYER_Y_ACCUM
    sta PLAYER_Y_ACCUM,x          ; add another PLAYER_Y_FRACT_VELOCITY to accumulator
    lda #$00                      ; add any overflow
    adc $14                       ; add any fast Y velocity (adjusted for any vertical scroll)
    sta Y_SCROLL_SPEED            ; set number of vertical pixels to scroll vertically this frame to match player's velocity
    jmp @exit                     ; exit

@apply_y_vel:
    lda PLAYER_Y_ACCUM,x          ; load current accumulated PLAYER_Y_ACCUM total
    clc                           ; clear carry in preparation for addition
    adc PLAYER_Y_FRACT_VELOCITY,x ; a = PLAYER_Y_FRACT_VELOCITY + PLAYER_Y_ACCUM
    sta PLAYER_Y_ACCUM,x          ; add another PLAYER_Y_FRACT_VELOCITY to accumulator
    lda PLAYER_SPRITE_Y_POS,x     ; load player's Y position
    adc $14                       ; add fast velocity adjustment
    sta PLAYER_SPRITE_Y_POS,x     ; set new player Y position
    sta PLAYER_Y_POS,x            ; set new player Y position copy in zero page

@exit:
    rts

; determine background collision at point (PLAYER_SPRITE_X_POS, y)
; input
;  * x - player index (0 = p1, 1 = p2)
;  * a - Y position to test (#$0a is subtracted before testing)
get_player_bg_collision_y:
    clc                           ; clear carry in preparation for addition
    adc #$f6                      ; subtract #$0a from y test position
    tay                           ; transfer Y position to test to y
    lda PLAYER_SPRITE_X_POS,x     ; load player's X position to test
    jsr get_bg_collision_code     ; determine background collision at point (a,y)
    cmp #$06                      ; compare to the first incline collision
    bcc @get_bg_collision         ; branch if not a collision with an incline
    lda #$02                      ; collision with incline, use floor/ground collision
    ldy INCLINE_BG_COLLISION_FLAG ; load whether or not bullets should collide with inclines
    bne @get_bg_collision         ; branch if testing bg collisions with inclines
    lda #$00                      ; not testing bg collisions with inclines

@get_bg_collision:
    tay                           ; #$00 for not testing bg collisions with inclines, #$02 otherwise
    lda player_bg_collision_tbl,y
    rts

player_bg_collision_tbl:
    .byte $00 ; no collision
    .byte $00 ; floating platform
    .byte $01 ; floor/incline
    .byte $01 ; collapsible ground (eggshells)
    .byte $01 ; water
    .byte $00

check_landing_exit:
    rts

; checks if player is falling and if so looks to see if colliding with bg
; if landing, clears Y velocity, plays landing sound and sets player Y position
; input
;  * x - player index (0 = p1, 1 = p2)
check_landing:
    lda #$00
    sta PLAYER_SURFACE,x         ; clear the kind of surface the player is standing on
    lda PLAYER_JUMP_STATUS,x     ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    bpl @continue                ; branch if not jumping or if falling from ledge
    lda PLAYER_Y_FAST_VELOCITY,x ; player in jump state, see if rising or falling
    bmi check_landing_exit
    beq check_landing_exit       ; exit if not falling

; falling, test bg collision at player's feet
@continue:
    lda #$15
    sta $07                   ; store amount below player to test when testing bg collision
                              ; this value is also used when colliding to adjust landing Y position
    clc                       ; clear carry in preparation for addition
    adc PLAYER_SPRITE_Y_POS,x
    tay
    lda PLAYER_SPRITE_X_POS,x
    clc                       ; clear carry in preparation for addition
    adc X_SCROLL_SPEED        ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    jsr get_bg_collision_code ; determine background collision at point (a,y)
    sta PLAYER_SURFACE,x      ; set the kind of surface the player is standing on
    sta $09                   ; store bg collision code for use in get_landing_y_pos
    bne handle_landing        ; branch if collided with background
    lda PLAYER_JUMP_STATUS,x  ; no background collision, not landing
                              ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    bne check_landing_exit    ; branch to exit if already marked as falling off ledge or if jumped off ledge

; sets jump status to indicate falling from ledge
; sets jump input to match current controller input
set_fall_from_ledge_input:
    lda #$40
    sta PLAYER_JUMP_STATUS,x ; set falling off edge
    lda CONTROLLER_STATE,x   ; load controller input
    sta JUMP_INPUT,x         ; set jump input when starting to fall
    rts

handle_landing:
    lda PLAYER_JUMP_STATUS,x ; #$00 = no jump, #$40 = fall off edge, #$80 = player jump
    beq @continue            ; branch if not jumping
    lda PLAYER_SURFACE,x     ; jumping or falling off ledge, load the kind of surface the player is standing on
    cmp #$04                 ; see if player in water
    beq @continue            ; branch if player in water (don't play sound when landing in water)
    lda #$06                 ; sound_06 (SUTA) - player landing on ground
    jsr play_sound           ; play sound_06 (player landing on ground)

; landed on ground - clear player y velocity
@continue:
    lda #$00
    sta PLAYER_JUMP_STATUS,x      ; set jump status to no jump
    sta JUMP_INPUT,x              ; clear jump input
    sta PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_FAST_VELOCITY,x  ; clear player Y velocity
    lda PLAYER_SURFACE,x          ; load the kind of surface the player is standing on
    cmp #$04                      ; see if player in water
    bne @set_player_y_pos
    lda GLOBAL_TIMER              ; player in water
    and #$10                      ; adjust Y position every #$10 frames
    beq @set_player_y_pos
    dec $07                       ; periodically adjust final Y position to create a "bobbing" effect in water
                                  ; for #$10 frames will be #$15, then #$14, repeat

@set_player_y_pos:
    jsr get_landing_y_pos     ; determine Y position from bg collision
    sta PLAYER_SPRITE_Y_POS,x
    rts

run_overhead_player_state_routine:
    lda PLAYER_STATE,x
    jsr run_routine_from_tbl_below

oh_p_state_routine_ptr_tbl:
    .addr player_state_routine_00 ; set game over state, disable player-enemy collisions, set player position, set invisible
    .addr oh_p_state_routine_01   ; called once initialize level, then executed once between state #$05 and #$02 (player spawning)
    .addr oh_p_state_routine_02   ; normal alive state
    .addr oh_p_state_routine_03   ; player death, initiated from kill_player
    .addr oh_p_state_routine_04   ; player death
    .addr oh_p_state_routine_05   ; player dead on ground
    .addr oh_p_state_routine_06   ; state is only valid for level 1, do nothing
    .addr oh_p_state_routine_06   ; state is only valid for level 1, do nothing
    .addr player_state_routine_08

; called once for initial player spawn and after each death
; finds spawn position for player with no background collision
; compare player_state_routine_01
oh_p_state_routine_01:
    jsr init_player_attr_adv_player_state   ; initialize player attributes, decrement number of lives
                                            ; and set player state to oh_p_state_routine_02
    lda #$b0
    sta PLAYER_SPRITE_Y_POS,x               ; set initial Y position
    lda oh_level_initial_player_x_pos_tbl,x ; load initial X position
    sta PLAYER_SPRITE_X_POS,x
    jmp oh_find_spawn_x_pos                 ; test for bg collisions, starting at current player position
                                            ; move right until find appropriate spawn X location

oh_level_initial_player_x_pos_tbl:
    .byte $98 ; player 1
    .byte $68 ; player 2

; normal alive state
; handle weapon fire and movement, set player sprite, set aim dir, check if clipped
; compare player_state_routine_02
oh_p_state_routine_02:
    jsr handle_weapon_fire             ; determines if should create player bullets, and creates them if needed
    jsr oh_set_sprite_attr_apply_x_vel ; set player sprite attribute based on d-pad, applies player's X velocity
    jsr overhead_animate_player        ; calc correct animation frame index, and set player sprite and sprite palette
    jsr overhead_set_aim_dir           ; calc and set PLAYER_AIM_DIR from PLAYER_OVERHEAD_DIR
    jmp kill_player_if_clipped         ; test if a player has clipped into a bg collision
                                       ; if so, kill the player

; player state 6 and 7 are only valid for level 1
; compare player_state_routine_06, player_state_routine_07
oh_p_state_routine_06:
    rts

; player hit, initiate fall back animation by setting player velocity, and animation timer
; compare player_state_routine_03
oh_p_state_routine_03:
    lda #$80
    sta PLAYER_JUMP_STATUS,x           ; set falling backwards
    lda #$00
    sta PLAYER_X_FRACT_VELOCITY,x
    sta PLAYER_X_FAST_VELOCITY,x       ; clear X velocity
    lda #$c0
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda #$fd
    sta PLAYER_Y_FAST_VELOCITY,x       ; set Y velocity to -2.25
    lda #$0c
    sta PLAYER_ANIM_FRAME_TIMER,x
    lda #$00
    sta PLAYER_ANIMATION_FRAME_INDEX,x
    jsr oh_set_player_death_sprite     ; set player death sprite based on player animation frame index
    jsr oh_level_apply_x_vel           ; apply player X velocity, checking for bg collisions
    inc PLAYER_STATE,x                 ; move to oh_p_state_routine_04

oh_p_state_routine_exit:
    rts

; player death, animate falling back
; compare player_state_routine_04
oh_p_state_routine_04:
    jsr oh_anim_player_death
    lda PLAYER_Y_FAST_VELOCITY,x
    bmi @apply_gravity             ; continue if still rising
    cmp #$03
    bcc @apply_gravity             ; branch if not falling down too fast (not at stop speed)
    jmp clear_player_vel_adv_state ; should stop, clear player velocity, set state timer, and advance player state

@apply_gravity:
    lda PLAYER_Y_FRACT_VELOCITY,x
    clc                           ; clear carry in preparation for addition
    adc #$20
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda PLAYER_Y_FAST_VELOCITY,x
    adc #$00
    sta PLAYER_Y_FAST_VELOCITY,x  ; apply gravity (0.125)
    jmp oh_level_apply_x_vel      ; apply player X velocity, checking for bg collisions

; dead on ground
; compare player_state_routine_05
oh_p_state_routine_05:
    jsr oh_anim_player_death
    jmp wait_for_spawn_delay ; wait for lying on ground timer to elapse, decrement number of lives
                             ; then initialize player for new life

; decrements animation delay timer, and if elapsed, move to next animation frame
; animates through
oh_anim_player_death:
    dec PLAYER_ANIM_FRAME_TIMER,x
    bne oh_p_state_routine_exit
    lda #$08
    sta PLAYER_ANIM_FRAME_TIMER,x
    inc PLAYER_ANIMATION_FRAME_INDEX,x
    lda PLAYER_ANIMATION_FRAME_INDEX,x
    cmp #$02
    bcc @continue
    lda #$02

@continue:
    sta PLAYER_ANIMATION_FRAME_INDEX,x

; input
;  * a - overhead sprite index for player death (#$00 to #$03 inclusively)
oh_set_player_death_sprite:
    clc                            ; clear carry in preparation for addition
    adc #$3b                       ; add base overhead player death sprite offset
                                   ; (player_sprite_3b, player_sprite_3c, and player_sprite_3d)
    jmp overhead_set_player_sprite ; set player sprite to a and sets palette based on player index

; sets player sprite attribute, and player overhead dir based on d-pad
; then applies player's X velocity
; input
;  * x - player index (0 = p1, 1 = p2)
oh_set_sprite_attr_apply_x_vel:
    jsr decrement_invincibility_timers ; decrement new life invincibility and/or B weapon invincibility
    lda CONTROLLER_STATE,x             ; load controller input
    and #$0f                           ; strip to just d-pad input
    sta $13                            ; store d-pad input in $13
    cmp #$0b                           ; see if both up and down are pressed (invalid/impossible input)
    bcc @get_dir                       ; branch if valid input
    lda #$00                           ; if both up and down were detected don't update input

@get_dir:
    tay
    beq @continue                         ; branch if no input (or invalid input)
    lda overhead_player_input_dir_tbl-1,y ; load direction based on input value, e.g. 0 = up, 1 = up-right, 2 = right, etc.
    sta PLAYER_OVERHEAD_DIR,x             ; set facing direction, e.g. 0 = up, 1 = up-right, 2 = right, etc.

@continue:
    lda #$08
    ldy $13                        ; load controller input
    beq @set_vel_apply_x_vel
    ldy PLAYER_OVERHEAD_DIR,x      ; load facing direction, e.g. 0 = up, 1 = up-right, 2 = right, etc.
    lda overhead_player_attr_tbl,y ; load sprite attribute based on direction (what to flip horizontally)
    sta $08
    lda PLAYER_SPRITE_ATTR,x
    and #$3c                       ; strip any sprite flipping attribute and any palette overwrite
    ora $08                        ; merge with any horizontal flip bits
    sta PLAYER_SPRITE_ATTR,x       ; set sprite attribute
    tya                            ; transfer PLAYER_OVERHEAD_DIR back to a

@set_vel_apply_x_vel:
    asl
    asl                             ; quadruple since each entry is #$04 bytes
    tay                             ; transfer to offset register
    lda overhead_player_vel_tbl,y
    sta PLAYER_X_FRACT_VELOCITY,x
    lda overhead_player_vel_tbl+1,y
    sta PLAYER_X_FAST_VELOCITY,x
    lda overhead_player_vel_tbl+2,y
    sta PLAYER_Y_FRACT_VELOCITY,x
    lda overhead_player_vel_tbl+3,y
    sta PLAYER_Y_FAST_VELOCITY,x

; applies player X velocity, checking for bg collisions, and screen edge
; used in player state #$02, #$03, and #$04 for overhead levels (Level 2 and Level 6)
oh_level_apply_x_vel:
    lda IRQ_Y_SCROLL              ; load vertical scroll value used after first interrupt, but before 3rd interrupt
    cmp #$ff
    bne @apply_x_vel_if_nonzero   ; branch to apply X velocity if not at jagger froid boss
    lda SCANLINE_IRQ_1            ; there is a vertical scroll of #$ff after the first interrupt
                                  ; this is only when jagger froid is visible on Level 6 (Entry to HQ)
                                  ; load scanline where interrupt will occur
    sec                           ; set carry flag in preparation for subtraction
    sbc #$10                      ; look #$10 rows above scanline
    cmp PLAYER_SPRITE_Y_POS,x     ; compare the scanline row to the player's Y position
    bcs @apply_x_vel_if_nonzero   ; branch to apply X velocity if player is below scanline
                                  ; i.e. they are after the first interrupt
                                  ; for jagger froid, this is when the player is below the serpent spawn caves
    ldy PLAYER_SPRITE_X_POS,x     ; player before first interrupt, load player X position
    lda PLAYER_X_FAST_VELOCITY,x  ; load player X fast velocity
    bpl @pre_irq_check_right_edge ; branch if moving right
    cpy #$28                      ; moving left, compare player X position to 15.625% of screen to
                                  ; prevent walking into jagger froid left cave
    bcc @exit                     ; don't apply velocity if on left edge
    bcs @apply_x_vel              ; not near left edge and moving left
                                  ; branch to apply X velocity

; prevent walking into jagger froid right cave
@pre_irq_check_right_edge:
    cpy #$d8         ; moving right, compare player X position to 84.375% of screen
    bcs @exit        ; don't apply velocity if on right edge
    bcc @apply_x_vel ; not near right edge and moving right
                     ; branch to apply X velocity

@apply_x_vel_if_nonzero:
    lda #$12
    sta $0f                       ; used for call to get_bg_collision_y_range
    lda PLAYER_SPRITE_Y_POS,x
    clc                           ; clear carry in preparation for addition
    adc #$0f                      ; search for background collision #$0f below player Y position
    tay                           ; set Y position to test for get_bg_collision_y_range
    lda PLAYER_X_FAST_VELOCITY,x
    ora PLAYER_X_FRACT_VELOCITY,x
    beq @exit                     ; exit if no horizontal velocity
    lda PLAYER_X_FAST_VELOCITY,x
    asl
    lda #$07                      ; look to the right by #$07 pixels for a background collision
    bcc @apply_x_vel_if_should    ; branch if moving right
    lda #$f8                      ; moving left, look to the left by #$07 pixels for a background collision

; apply X velocity if not at extreme edge of screen
; if encounter bg collision, set X velocity to #$00
@apply_x_vel_if_should:
    clc                           ; clear carry in preparation for addition
    adc PLAYER_SPRITE_X_POS,x
    jsr get_bg_collision_y_range
    bcc @apply_x_vel_if_not_edge  ; branch if no background collision
    lda #$00                      ; background collision, stop horizontal velocity
    sta PLAYER_X_FRACT_VELOCITY,x
    sta PLAYER_X_FAST_VELOCITY,x
    beq @exit

@apply_x_vel_if_not_edge:
    ldy PLAYER_SPRITE_X_POS,x
    lda PLAYER_X_FAST_VELOCITY,x
    bpl @apply_x_vel_if_not_edge_right ; branch to apply X velocity if current x vel is positive
    cpy #$14                           ; X velocity is negative, compare to extreme left
    bcc @exit
    bcs @apply_x_vel

@apply_x_vel_if_not_edge_right:
    cpy #$ec  ; compare player X position to #$ec
    bcs @exit

; input
;  * x - player index (0 = p1, 1 = p2)
; does not set PLAYER_X_POS
; PLAYER_X_POS is updated in run_player_state_routine
@apply_x_vel:
    lda PLAYER_X_ACCUM,x          ; load current accumulated PLAYER_X_ACCUM total
    clc                           ; clear carry in preparation for addition
    adc PLAYER_X_FRACT_VELOCITY,x ; a = PLAYER_X_FRACT_VELOCITY + PLAYER_X_ACCUM
    sta PLAYER_X_ACCUM,x          ; add another PLAYER_X_FRACT_VELOCITY to accumulator
    lda PLAYER_SPRITE_X_POS,x     ; load current player X position
    adc PLAYER_X_FAST_VELOCITY,x  ; add player's X fast velocity with any fractional X velocity
    sta PLAYER_SPRITE_X_POS,x     ; set new player X position

@exit:
    rts

; converts from d-pad input to direction, .e.g up = 0, up-right = 1, right = 2, etc.
overhead_player_input_dir_tbl:
    .byte $02 ; offset 1: 2 = right
    .byte $06 ; offset 2: 6 = left
    .byte $00 ; offset 3: 0 = left right (invalid)
    .byte $04 ; offset 4: 4 = down
    .byte $03 ; offset 5: 3 = down right
    .byte $05 ; offset 6: 5 = down left
    .byte $00 ; offset 7: 0 = left right down (invalid)
    .byte $00 ; offset 8: 0 = up
    .byte $01 ; offset 9: 1 = up-right
    .byte $07 ; offset #$0a: 7 = left up

; horizontal sprite flip flag
overhead_player_attr_tbl:
    .byte $00 ; #$00 - up (no flip)
    .byte $40 ; #$01 - up-right (horizontal flip)
    .byte $40 ; #$02 - right (horizontal flip)
    .byte $40 ; #$03 - down-right (horizontal flip)
    .byte $00 ; #$04 - down (no flip)
    .byte $00 ; #$05 - down-left (no flip)
    .byte $00 ; #$06 - left (no flip)
    .byte $00 ; #$07 - up-left (no flip)

; each row is a different PLAYER_OVERHEAD_DIR
overhead_player_vel_tbl:
    .byte $00,$00,$00,$ff ; x vel =  0.00, y vel = -1.00, PLAYER_OVERHEAD_DIR = #$00 - up
    .byte $b5,$00,$4b,$ff ; x vel =  0.71, y vel = -0.71, PLAYER_OVERHEAD_DIR = #$01 - up-right
    .byte $00,$01,$00,$00 ; x vel =  1.00, y vel =  0.00, PLAYER_OVERHEAD_DIR = #$02 - right
    .byte $b5,$00,$b5,$00 ; x vel =  0.71, y vel =  0.71, PLAYER_OVERHEAD_DIR = #$03 - down-right
    .byte $00,$00,$00,$01 ; x vel =  0.00, y vel =  1.00, PLAYER_OVERHEAD_DIR = #$04 - down
    .byte $4b,$ff,$b5,$00 ; x vel = -0.71, y vel =  0.71, PLAYER_OVERHEAD_DIR = #$05 - down-left
    .byte $00,$ff,$00,$00 ; x vel = -1.00, y vel =  0.00, PLAYER_OVERHEAD_DIR = #$06 - left
    .byte $4b,$ff,$4b,$ff ; x vel = -0.71, y vel = -0.71, PLAYER_OVERHEAD_DIR = #$07 - up-left
    .byte $00,$00,$00,$00 ; #$08 - , x vel =  0.00, y vel =  0.00 (unused)

; determine correct animation frame index, draw player sprite, and player sprite attribute
overhead_animate_player:
    lda #$bf                               ; overhead level action state
                                           ; only one action state for overhead levels
    sta PLAYER_ACTION_STATE,x              ; set action state to overhead
    lda CONTROLLER_STATE,x
    and #$0f
    beq @no_d_pad_input                    ; branch if no d-pad input is detected
    inc PLAYER_ANIM_FRAME_TIMER,x          ; player pressing a direction (walking), increment player animation timer
    lda PLAYER_ANIM_FRAME_TIMER,x
    cmp #$09                               ; see if have walked for #$09 frames and need to update sprite to next animation
    bcc overhead_set_player_sprite_for_dir ; branch if no need to adjust animation frame,
                                           ; to set player sprite and palette based on aiming direction and animation frame index
    lda #$00                               ; need to update animation frame index
    sta PLAYER_ANIM_FRAME_TIMER,x          ; reset animation frame timer
    inc PLAYER_ANIMATION_FRAME_INDEX,x     ; move to next frame index in the animation
    ldy PLAYER_ANIMATION_FRAME_INDEX,x
    cpy #$04                               ; check to see if finished animation and need to loop back to first animation frame
    bcc overhead_set_player_sprite_for_dir ; branch if animation not elapsed
                                           ; to set player sprite and palette based on aiming direction and animation frame index
    ldy #$00                               ; finished animation loop, reset animation frame index
    beq @set_frame_and_sprite              ; always branch to set frame index, player sprite, and sprite attribute

@no_d_pad_input:
    lda #$00
    sta PLAYER_ANIM_FRAME_TIMER,x ; player not moving, reset animation timer
    ldy #$01                      ; and use first animation frame index

@set_frame_and_sprite:
    sty PLAYER_ANIMATION_FRAME_INDEX,x

; sets player sprite and attribute based on aiming direction and animation frame index
; sets player palette based on player index
; flashes player invisible if just revived
overhead_set_player_sprite_for_dir:
    ldy PLAYER_OVERHEAD_DIR,x                ; load facing direction, e.g. 0 = up, 1 = up-right, 2 = right, etc.
    lda player_overhead_dir_sprite_ptr_tbl,y ; load index into base set of sprites for direction
    clc
    adc PLAYER_ANIMATION_FRAME_INDEX,x       ; add current frame of the animation to get specific sprite offset
    tay                                      ; transfer sprite offset to y
    lda player_overhead_sprite_tbl,y         ; load sprite code based on direction and animation frame index

; sets player sprite and attribute (palette portion only)
; sets player palette based on player index
; flashes player invisible if just revived
; input
;  * a - sprite code
overhead_set_player_sprite:
    tay                                ; transfer sprite code to y
    lda NEW_LIFE_INVINCIBILITY_TIMER,x ; see if player in invincible
    beq @set_sprite_and_attr           ; continue if not invincible
    lda FRAME_COUNTER                  ; invincible, load frame counter
    lsr
    bcc @set_sprite_and_attr           ; continue if even frame
    ldy #$00                           ; odd frame, hide player for frame

@set_sprite_and_attr:
    tya                                      ; transfer sprite code to a
    sta PLAYER_SPRITE,x                      ; set player sprite based on direction and animation frame
    lda PLAYER_SPRITE_ATTR,x                 ; load current sprite attributes
    ora overhead_player_sprite_palette_tbl,x ; merge palette override based on player
    sta PLAYER_SPRITE_ATTR,x                 ; set palette based on player index
    rts

overhead_player_sprite_palette_tbl:
    .byte $00 ; player 1
    .byte $01 ; player 2

; references player_overhead_sprite_tbl to specify specific sprite for player direction
player_overhead_dir_sprite_ptr_tbl:
    .byte $10,$08,$0c,$04 ; up, up-right, right, right-down
    .byte $00,$04,$0c,$08 ; down, down-left, left, up-left

player_overhead_sprite_tbl:
    .byte $2c,$2d,$2e,$2d ; #$00 - down
    .byte $2f,$30,$31,$30 ; #$04 - down-right, down-left
    .byte $32,$33,$34,$33 ; #$08 - up-right, up-left
    .byte $35,$36,$37,$36 ; #$0c - right, left
    .byte $38,$39,$3a,$39 ; #$10 - up

; convert PLAYER_OVERHEAD_DIR to generic aim dir and set PLAYER_AIM_DIR
overhead_set_aim_dir:
    ldy PLAYER_OVERHEAD_DIR,x         ; load facing direction, e.g. 0 = up, 1 = up-right, 2 = right, etc.
    lda overhead_dir_to_aim_dir_tbl,y
    sta PLAYER_AIM_DIR,x              ; set aim direction from overhead aim direction
    rts

overhead_dir_to_aim_dir_tbl:
    .byte $00 ; #$00 - up
    .byte $01 ; #$01 - up-right
    .byte $02 ; #$02 - right
    .byte $03 ; #$03 - down-right
    .byte $06 ; #$04 - down
    .byte $09 ; #$05 - down-left
    .byte $0a ; #$06 - left
    .byte $0b ; #$07 - up-left

run_overhead_player_y_scroll_routine:
    lda PLAYER_STATE,x
    jsr run_routine_from_tbl_below

overhead_player_y_scroll_routine_ptr_tbl:
    .addr overhead_player_y_scroll_routine_00 ; do nothing
    .addr overhead_player_y_scroll_routine_00 ; do nothing
    .addr overhead_player_y_scroll_routine_02 ; #$02 - normal alive state
    .addr overhead_player_y_scroll_routine_03 ; #$03 - hit by enemy
    .addr overhead_player_y_scroll_routine_03 ; #$04 - falling back after hit
    .addr overhead_player_y_scroll_routine_05 ; #$05 - dead on ground
    .addr overhead_player_y_scroll_routine_00 ; do nothing
    .addr overhead_player_y_scroll_routine_00 ; do nothing
    .addr overhead_player_y_scroll_routine_00 ; do nothing

; hit by enemy
; apply Y velocity/scroll unless at bottom, then advance routine
overhead_player_y_scroll_routine_03:
    jsr overhead_player_y_scroll_routine_02 ; check for collision, otherwise, apply Y velocity or Y scroll if moving
    lda PLAYER_Y_FAST_VELOCITY,x
    bmi overhead_player_y_scroll_routine_00 ; branch if moving up
    lda PLAYER_Y_POS,x                      ; moving down
    cmp #$ff                                ; see if at bottom
    bne overhead_player_y_scroll_routine_00 ; exit if not at bottom
    jmp clear_player_vel_adv_state          ; at bottom
                                            ; clear player velocity, set state timer, and advance player state

; dead on ground
overhead_player_y_scroll_routine_05:
    jmp overhead_player_y_scroll_routine_02 ; check for collision, otherwise, apply Y velocity or Y scroll if moving

overhead_player_y_scroll_routine_00:
    rts

; normal alive state
; check for collision in front of player, otherwise, apply Y velocity or Y scroll if moving
overhead_player_y_scroll_routine_02:
    lda #$0c
    sta $0f                           ; set how close to the left tile to consider bg collisions there
                                      ; used in overhead_player_get_bg_collision
    lda PLAYER_Y_FAST_VELOCITY,x
    ora PLAYER_Y_FRACT_VELOCITY,x
    beq @no_collision                 ; branch to continue if no Y velocity
    lda PLAYER_AUTO_MOVE_CHECKPOINT,x ; (0 = not reached, 1 = reached)
    bne @no_collision                 ; branch to continue if reached auto-move checkpoint
    lda PLAYER_Y_FAST_VELOCITY,x      ;  ; haven't reached checkpoint
    asl
    lda #$10                          ; assume moving down, use #$10
    bcc @get_bg_collision             ; branch if player is moving down
    lda #$f9                          ; player moving up, use -7

@get_bg_collision:
    clc
    adc PLAYER_SPRITE_Y_POS,x
    tay
    lda PLAYER_SPRITE_X_POS,x
    clc
    adc #$05                             ; checking for collision in front of player
    jsr overhead_player_get_bg_collision ; check for bg collision at (a,y) as well as two tiles to the left if necessary
                                         ; checks at (PLAYER_SPRITE_X_POS+#$05, PLAYER_SPRITE_Y_POS - #$07) when player moving up
                                         ; checks at (PLAYER_SPRITE_X_POS+#$05, PLAYER_SPRITE_Y_POS + #$10) when player moving down
                                         ; this checks to the players left and right for a collision to see if collision in front of player
    bcc @no_collision                    ; branch to continue if not background collision
    lda #$00                             ; background collision, stop Y velocity
    sta PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_FAST_VELOCITY,x
    lda #$ff
    sta PLAYER_Y_POS,x

@no_collision:
    lda PLAYER_Y_FAST_VELOCITY,x
    sec
    sbc Y_SCROLL_SPEED
    sta $14                       ; set amount to scroll vertically this frame offset by player Y velocity
                                  ; e.g. scroll up 1 pixel this frame, but player Y velocity is also -1
                                  ; so player doesn't move on the screen
    ora PLAYER_Y_FRACT_VELOCITY,x
    beq @exit                     ; exit if no Y velocity when accounting for scroll
    lda PLAYER_SPRITE_Y_POS,x
    ldy $14                       ; load any player Y movement accounting for existing Y scroll
    bpl @apply_if_not_at_bottom   ; branch if player moving down to apply player Y velocity if not at bottom
    bit BOSS_DEFEATED_FLAGS       ; player moving up
    bvs @check_y_scroll           ; branch if playing end of level tune
    cmp #$12                      ; not playing end of level tune, see if player at top 7% of screen
    bcc @exit                     ; exit if player at the top of the screen to not apply Y velocity

; either player should move up, or screen should scroll up
@check_y_scroll:
    ldy ACTIVE_PLAYERS_FLAG           ; load player active/game over statuses
    cmp overhead_level_center_y_tbl,y ; compare to player Y center of screen
                                      ; scroll position depends on which player(s) is/are active
    bcs @apply_y_vel                  ; branch to apply velocity if not past 'center' of screen
    lda PLAYER_APPLY_VEL,x            ; in part of screen that can cause scroll
                                      ; see if player is cause of the need for horizontal scroll
    bne @apply_y_vel                  ; branch if player is not cause of scroll
    lda LEVEL_Y_SCROLL_FLAGS          ; player can cause Y scroll, see current allowable Y scroll direction
    bmi @apply_y_vel                  ; branch if cannot scroll up to apply player Y velocity
    jsr is_player_initialized         ; player can cause scroll, and can scroll up
                                      ; set carry if other player is state #$02 or greater (active)
    bcc @calc_y_frame_scroll          ; branch if other player is not initialized to scroll frame up
    lda PLAYER_SPRITE_Y_POS,y         ; other player active, see if should block scrolling
    cmp #$d8                          ; prevent scrolling if other player is in bottom 15.625% of screen
    bcs @exit

; calculate Y_SCROLL_SPEED and apply fractional velocity
@calc_y_frame_scroll:
    lda PLAYER_Y_ACCUM,x
    clc
    adc PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_ACCUM,x
    lda #$00                      ; including any fractional Y velocity overflow
    adc $14                       ; add amount to scroll vertically this frame offset by player Y velocity
    sta Y_SCROLL_SPEED            ; set actual amount to scroll vertically this frame
    jmp @exit

@apply_if_not_at_bottom:
    cmp #$d8  ; compare player Y position to bottom edge of the overhead level screen
    bcs @exit ; exit to not apply down velocity if at the bottom of the screen

@apply_y_vel:
    lda PLAYER_Y_ACCUM,x
    clc
    adc PLAYER_Y_FRACT_VELOCITY,x
    sta PLAYER_Y_ACCUM,x
    lda PLAYER_SPRITE_Y_POS,x
    adc $14                       ; add amount to scroll vertically this frame offset by player Y velocity
    sta PLAYER_SPRITE_Y_POS,x
    ldy PLAYER_Y_POS,x
    cpy #$ff
    beq @exit
    sta PLAYER_Y_POS,x            ; set new Y position

@exit:
    rts

; !(UNUSED)
bank_0_unused_00:
    .byte $00,$00,$01,$01,$01,$00

overhead_level_center_y_tbl:
    .byte $80 ; ACTIVE_PLAYERS_FLAG = #$00 - player 1 not in game over, player 2 in game over (or 1 player game)
    .byte $80 ; ACTIVE_PLAYERS_FLAG = #$01 - player 1 in game over, player 2 not in game over
    .byte $70 ; ACTIVE_PLAYERS_FLAG = #$01 - 2 player game and neither player in game over

; determines if should create player bullets, and creates them if needed
handle_weapon_fire:
    lda PLAYER_SURFACE,x   ; load the kind of surface the player is standing on
    cmp #$04               ; see if player in water
    bne @eval_weapon       ; continue if not in water
    lda CONTROLLER_STATE,x ; player in water, load controller input
    and #$04               ; see if pressing down in water
    bne @exit              ; exit if pressing down in water, can't fire while under water

@eval_weapon:
    lda PLAYER_CURRENT_WEAPON,x  ; get current player's weapon
    sta $0f                      ; store weapon (with rapid fire flag)
    and #$0f                     ; remove rapid fire flag
    sta $0e                      ; store weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F)
    tay                          ; transfer weapon to y
    cpy #$01                     ; see if is machine gun (M)
    beq @machine_gun             ; branch if machine gun
    cpy #$03                     ; see if is laser (L)
    bne @non_machine_gun         ; branch if not laser, nor machine gun, i.e. regular, spray, or flame
    lda CONTROLLER_STATE,x       ; player has laser weapon, load controller input
    asl
    asl
    bcc @non_machine_gun         ; branch if b button (fire) not pressed
    txa                          ; laser and b button (fire) pressed transfer player index to a
    beq @laser                   ; branch if player 1
    lda bullet_start_index_tbl,y ; player 2, load start index based on weapon

; player with laser and b button pressed
@laser:
    tay      ; transfer bullet search start index to y
    lda #$05 ; total number of slots to search
    sta $0d  ; store total number of slots to search in $0d

@count_lasers_loop:
    lda PLAYER_BULLET_STATE,y ; (0 = unused, 1 = normal, 2 = destroyed)
    bne @non_machine_gun      ; branch if slot in use, e.g. already laser on screen
    iny                       ; slot not used, increment number of lasers
    dec $0d                   ; decrement number of slots to search
    bne @count_lasers_loop    ; loop if more slots to search
    beq @create_player_bullet ; finished looping, always branch

@machine_gun:
    lda CONTROLLER_STATE,x          ; load controller input
    asl
    asl
    lda #$00
    bcc @check_m_delay              ; branch if b button (fire) not pressed
    dec PLAYER_M_WEAPON_FIRE_TIME,x ; decrement delay before firing next bullet
    bpl @exit                       ; exit if delay not elapsed
    lda #$06                        ; timer elapsed, create new bullet

@check_m_delay:
    sta PLAYER_M_WEAPON_FIRE_TIME,x
    bcs @create_player_bullet

@exit:
    rts

; non-machine weapon (M), i.e. regular, S, or F
; if laser, it's because another laser already on screen
@non_machine_gun:
    lda CONTROLLER_STATE_DIFF,x
    asl
    asl
    ldy $0e                     ; load weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F)
    bcs @check_l_continue       ; branch if b button (fire) pressed
    cpy #$04                    ; b button not pressed
    bne @exit                   ; exit if not f weapon and b button not pressed, e.g. don't create any bullet
    lda CONTROLLER_STATE,x      ; f weapon and b button not pressed, load controller input
    and #$40
    beq @handle_f_charge        ; branch if b button (fire) not pressed
    lda F_WEAPON_CHARGE,x       ; b button (fire) pressed with F weapon, load how much F weapon charged up
    cmp #$20                    ; see if fully charged
    bcs @exit                   ; exit if fully charged
    inc F_WEAPON_CHARGE,x       ; not fully charged, increment charge duration
    bcc @exit                   ; not fully charged, always exit

; F weapon and b (fire) not pressed, check if fully charged
@handle_f_charge:
    lda F_WEAPON_CHARGE,x
    cmp #$20
    bcc restore_enemy_slot_exit ; exit if f weapon not fully charged
    lda $0f                     ; f weapon fully charged and fire no longer pressed
                                ; load weapon with rapid flag
    and #$80
    ora #$05
    sta $0f                     ; set weapon to charged F weapon (with or without rapid flag)

@check_l_continue:
    cpy #$03                     ; see if laser (L)
    bne @reset_f_charge          ; branch if not laser to continue creating bullet
    txa                          ; laser, and laser already on screen
                                 ; need to replace the existing lasers with new ones
                                 ; only 5 lasers on screen at once
                                 ; transfer player index to a
    beq @clear_existing_lasers   ; brach if player 1
    lda bullet_start_index_tbl,y ; player 2, start at offset #$0a to clear p2 lasers

; clear any existing lasers on screen, player is firing new lasers
@clear_existing_lasers:
    tay      ; transfer start index to offset register
    lda #$05 ; clearing 5 lasers
    sta $0d  ; store number of l lasers to clear
    lda #$00 ; marks bullet slot as unused

@clear_laser_loop:
    sta PLAYER_BULLET_STATE,y ; mark slot unused
    iny                       ; increment bullet slot offset
    dec $0d                   ; decrement number of l bullets cleared
    bne @clear_laser_loop     ; loop if still need more l bullets

@reset_f_charge:
    lda #$00
    sta F_WEAPON_CHARGE,x

@create_player_bullet:
    ldy #$11                 ; player in water recoil timer
    lda PLAYER_SURFACE,x     ; load the kind of surface the player is standing on
    cmp #$04                 ; see if player in water
    beq @set_recoil_continue ; branch if player in water
    lda CONTROLLER_STATE,x   ; player not in water, load controller input
    and #$03
    bne @set_recoil_continue ; branch if left or right d-pad were pressed
    ldy #$05                 ; not in water recoil timer

@set_recoil_continue:
    tya                       ; transfer recoil timer to a
    sta PLAYER_RECOIL_TIMER,x ; set recoil effect timer
                              ; either #$11 (walking or in water) or #$05 (on land stationary)
    lda #$00
    sta $00                   ; initialize bullet creation count
    lda PLAYER_AIM_DIR,x
    sta $02
    lda PLAYER_SPRITE_Y_POS,x
    sta $03
    lda PLAYER_SPRITE_X_POS,x
    sta $04
    lda $0e                   ; load weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F, 5 = charged F)
    cmp #$02                  ; see if S weapon
    clc
    beq @set_bullet_owner     ; branch if S weapon
    lda PLAYER_SURFACE,x      ; load the kind of surface the player is standing on
    cmp #$06                  ; see if player on inclined surface

@set_bullet_owner:
    rol
    and #$01
    sta $05  ; set incline flag (0 = not on incline (or S weapon), 1 = on incline)
    txa      ; transfer player index to a
    ror
    ror
    ror
    and #$40
    sta $12  ; bit 6 clear for p1, set for p2

@bullet_creation_loop:
    ldx ENEMY_CURRENT_SLOT         ; load current enemy index
                                   ; (0 = start at slot #$00, 1 = use value specified by y)
    ldy $0e                        ; load weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F, 5 = charged F)
    jsr find_bullet_slot
    bcs restore_enemy_slot_exit    ; exit if unable to find a free bullet slot
    jsr init_player_bullet         ; found bullet slot
    ldy $0e                        ; load weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F, 5 = charged F)
    lda $00                        ; load number of bullets created
    inc $00                        ; increment number of bullets created
    cmp num_bullets_per_fire_tbl,y ; compare to how many should be created per fire
    bcc @bullet_creation_loop      ; branch if more bullets to create (L or S)

restore_enemy_slot_exit:
    ldx ENEMY_CURRENT_SLOT ; load current enemy index
    rts

; input
;  * y - start/stop indexes
;    #$00 - start at slot #$03 and search #$0a slots
;    #$01 - start at slot #$05 and search #$0a slots
;    #$02 - start at slot #$09 and search #$06 slots
;    #$03 - start at slot #$04 and search #$0a slots
;    #$04 - start at slot #$01 and search #$0a slots
;    #$05 - start at slot #$00 and search #$0a slots
;    #$06 - start at slot #$09 and search #$06 slots
;  * x - whether or not to use start index from y
;    (0 = start at slot #$00, 1 = use value specified by y)
; output
;  * x - bullet slot index of empty bullet slot
;  * carry flag - set when unable to find a slot, clear otherwise
find_bullet_slot:
    txa                          ; transfer whether using filtered slots to a
    beq @continue                ; branch if not using limited bullet slots
    lda bullet_start_index_tbl,y ; load start bullet slot index for search

@continue:
    tax      ; transfer start bullet slot to search to x
    lda #$00 ; initialize number of bullets searched
    sta $01  ; store current bullet count

@find_bullet_loop:
    lda PLAYER_BULLET_STATE,x  ; (0 = unused, 1 = normal, 2 = destroyed)
    beq @exit_clear            ; exit if found bullet slot to use
    inx                        ; move to next bullet
    lda $01                    ; load number of bullets searched
    cmp bullet_end_count_tbl,y ; load number of slots to search based on y
    bcs @exit_set_carry        ; if all slots to search were searched
                               ; exit with carry flag set (no slot found)
    inc $01                    ; increment number of bullets searched
    bcc @find_bullet_loop      ; always loop to next bullet slot

@exit_clear:
    clc ; mark bullet slot found

@exit_set_carry:
    rts

; one less than the number of bullets to create based on weapon type
num_bullets_per_fire_tbl:
    .byte $00 ; #$00 - regular
    .byte $00 ; #$01 - machine (M)
    .byte $04 ; #$02 - spray (S)
    .byte $04 ; #$03 - laser (L)
    .byte $00 ; #$04 - flame (F)
    .byte $00 ; #$05 - charged flame

bullet_end_count_tbl:
    .byte $03,$05,$09,$04,$01,$00,$09

bullet_start_index_tbl:
    .byte $0a ; #$00 - regular
    .byte $0a ; #$01 - machine (M)
    .byte $06 ; #$02 - spray (S)
    .byte $0a ; #$03 - laser (L)
    .byte $0a ; #$04 - flame (F)
    .byte $0a ; #$05 - charged flame
    .byte $06 ; #$06 - child charged flame

; input
;  * $00 - S bullet index (which bullet of S fire)
;  * $02 - PLAYER_AIM_DIR,x
;  * $03 - bullet Y position
;  * $04 - bullet X position
;  * $05 - incline flag (0 = not on incline (or S weapon), 1 = on incline)
;  * $0f - weapon with rapid flag
;  * $12 - bullet owner byte (bit 6)
init_player_bullet:
    ldy $02                     ; load PLAYER_AIM_DIR
    lda bullet_aim_dir_tbl,y
    sta PLAYER_BULLET_AIM_DIR,x ; set aim dir for bullet (used only by l weapon)
    tya
    asl
    asl                         ; quadruple since each entry is #$04 bytes
    ldy $05                     ; load incline flag (0 = not on incline (or S weapon), 1 = on incline)
    beq @continue               ; branch if not on an incline
    clc
    adc #$34                    ; move to bottom half of bullet_pos_dir_tbl

@continue:
    tay                             ; transfer to offset register
    lda $03                         ; load Y position
    clc
    adc bullet_pos_dir_tbl,y
    sta PLAYER_BULLET_Y_POS,x       ; set bullet Y position
    lda $04                         ; load X position
    clc
    adc bullet_pos_dir_tbl+1,y
    sta PLAYER_BULLET_X_POS,x       ; set bullet X position
    lda bullet_pos_dir_tbl+3,y
    sta PLAYER_BULLET_SPRITE_ATTR,x ; set bullet sprite attribute
    lda bullet_pos_dir_tbl+2,y
    sta $0a                         ; set bullet direction offset (bullet_velocity_xx)
    lda $0e                         ; load weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F)
    cmp #$02                        ; compare to S weapon
    bne @calc_speed_code
    ldy $00                         ; S weapon, load bullet index (which bullet of S fire)
    lda s_bullet_dir_adj_tbl,y      ; adjust direction based on which of the S bullets this is
    clc
    adc $0a
    and #$7f                        ; strip bit 7
    sta $0a                         ; set bullet direction offset (bullet_velocity_xx)

@calc_speed_code:
    lda #$00
    ldy $0e                          ; load weapon (0 = Regular, 1 = M, 2 = S, 3 = L, 4 = F)
    cpy #$03                         ; compare to laser
    beq @init_bullet_vel_sprite      ; branch if laser
    lda $0f                          ; load weapon with rapid flag
    asl                              ; transfer rapid fire bit to carry
    lda weapon_type_speed_code_tbl,y
    bcc @check_max                   ; branch if no rapid fire
    adc #$00                         ; add 1

@check_max:
    cmp #$02                    ; see if passed max value
    bcc @init_bullet_vel_sprite
    lda #$02                    ; over max, set max value

@init_bullet_vel_sprite:
    ldy $05                               ; load incline flag (0 = not on incline (or S weapon), 1 = on incline)
    beq init_bullet_vel_sprite_play_sound ; branch if on incline
    clc
    adc #$03                              ; on incline, adjust speed code to

; initializes the player's bullet velocity, sprite, damage, and plays firing sound
; input
;  * a - bullet speed code
;  * $0a - bullet direction offset (bullet_velocity_xx)
;  * $0f - weapon type with rapid flag
;  * $12 - player index of player that created the bullet
;  * $01 - number of bullets created (used for spacing laser)
init_bullet_vel_sprite_play_sound:
    asl                               ; double bullet speed code
    tay                               ; transfer to offset register
    lda bullet_velocity_ptr_tbl,y
    sta $08
    lda bullet_velocity_ptr_tbl+1,y
    sta $09
    ldy $0a                           ; load direction offset
    lda ($08),y                       ; load bullet_velocity_xx Y fractional velocity
    sta PLAYER_BULLET_Y_VEL_FRACT,x   ; set bullet fractional velocity
    iny                               ; increment read offset
    lda ($08),y                       ; load bullet_velocity_xx Y fast velocity
    sta PLAYER_BULLET_Y_VEL_FAST,x    ; set bullet fast velocity
    iny                               ; increment read offset
    lda ($08),y                       ; load bullet_velocity_xx X fractional velocity
    sta PLAYER_BULLET_X_VEL_FRACT,x   ; set bullet X fractional velocity
    iny                               ; increment read offset
    lda ($08),y                       ; load bullet_velocity_xx X fast velocity
    sta PLAYER_BULLET_X_VEL_FAST,x    ; load bullet_velocity_xx X fast velocity
    lda #$00
    sta PLAYER_BULLET_TIMER,x
    ldy #$01
    lda $0f                           ; load player weapon type with rapid flag
    ora $12                           ; merge with player that created bullet flag (bit 6)
    sta PLAYER_BULLET_WEAPON_TYPE,x   ; set weapon type and player that created the bullet
    and #$0f
    cmp #$03
    bne @continue                     ; branch if not laser
    ldy PLAYER_BULLET_AIM_DIR,x       ; bullet was created by laser weapon
    lda laser_initial_dist_base_tbl,y ; load which row from laser_initial_dist_tbl to use
    clc
    adc $01                           ; add number of bullets created (see find_bullet_slot)
    tay
    lda laser_initial_dist_tbl,y      ; load delay to space L bullets
    sta PLAYER_BULLET_TIMER,x         ; set delay before L is created
    ldy #$03

@continue:
    tya
    sta PLAYER_BULLET_STATE,x          ; (0 = unused, 1 = normal, 2 = destroyed, 3 = just created laser)
    lda PLAYER_BULLET_WEAPON_TYPE,x    ; load weapon type and player that created the bullet
    and #$0f                           ; strip rapid fire flag
    tay                                ; transfer to offset register
    lda player_bullet_sprite_tbl,y
    sta PLAYER_BULLET_SPRITE_CODE,x    ; set bullet sprite
    lda player_bullet_dmg_tbl,y
    sta PLAYER_BULLET_DMG,x            ; load damage given when collide with enemy
    lda player_bullet_fire_sound_tbl,y
    beq @exit                          ; exit if no sound to play
    jmp play_sound                     ; play weapon fire sound

@exit:
    rts

; indexes into laser_initial_dist_tbl
laser_initial_dist_base_tbl:
    .byte $00 ; PLAYER_BULLET_AIM_DIR = #$00 - right
    .byte $05 ; PLAYER_BULLET_AIM_DIR = #$01 - down right
    .byte $00 ; PLAYER_BULLET_AIM_DIR = #$02 - down
    .byte $05 ; PLAYER_BULLET_AIM_DIR = #$03 - down left
    .byte $00 ; PLAYER_BULLET_AIM_DIR = #$04 - left
    .byte $05 ; PLAYER_BULLET_AIM_DIR = #$05 - up left
    .byte $00 ; PLAYER_BULLET_AIM_DIR = #$06 - up
    .byte $05 ; PLAYER_BULLET_AIM_DIR = #$07 - up right

laser_initial_dist_tbl:
    .byte $01,$03,$05,$07,$09 ; right, down, left, and up
    .byte $01,$03,$05,$07,$09 ; diagonal directions

; based on aim direction
; only used by for indexing into laser_bullet_sprite_tbl
bullet_aim_dir_tbl:
    .byte $06 ; sprite_15
    .byte $07 ; sprite_17
    .byte $00 ; sprite_16
    .byte $01 ; sprite_18
    .byte $00 ; sprite_16
    .byte $00 ; sprite_16
    .byte $02 ; sprite_15
    .byte $04 ; sprite_16
    .byte $04 ; sprite_16
    .byte $03 ; sprite_18
    .byte $04 ; sprite_16
    .byte $05 ; sprite_17
    .byte $06 ; sprite_15

; (0 - Regular, 1 - Machine, 2 - Spray, 3 - Laser, 4 - Flame, 5 - Charged Flame)
; sprite_05, sprite_19, sprite_06, sprite_00, sprite_19, sprite_03
player_bullet_sprite_tbl:
    .byte $05,$19,$06,$00,$19,$19,$03

; (0 - Regular, 1 - Machine, 2 - Spray, 3 - Laser, 4 - Flame, 5 - Charged Flame)
; all weapons deal 1 damage, except charged flame, which deals 5 damage
player_bullet_dmg_tbl:
    .byte $00,$00,$00,$00,$00,$04,$00

; (0 - Regular, 1 - Machine, 2 - Spray, 3 - Laser, 4 - Flame, 5 - Charged Flame)
; sound_0b, sound_0c, sound_0e, sound_0d, sound_0f
player_bullet_fire_sound_tbl:
    .byte $0b,$0c,$0e,$0d,$0f,$0f,$0f

s_bullet_dir_adj_tbl:
    .byte $00,$fc,$04,$f8,$08

weapon_type_speed_code_tbl:
    .byte $00,$00,$00,$00,$01,$02

bullet_velocity_ptr_tbl:
    .addr bullet_velocity_00 ; 1x
    .addr bullet_velocity_01 ; 1.375x
    .addr bullet_velocity_02 ; 1.75x
    .addr bullet_velocity_03 ; 1x on incline
    .addr bullet_velocity_04 ; 1.375x on incline
    .addr bullet_velocity_05 ; 1.75x on incline

; the specified time in the comment refers to the position of the hour hand at that time
bullet_velocity_00:
    .byte $00,$00,$00,$04 ; y vel =  0.00, x vel =  4.00 (3:00 o'clock)
    .byte $c4,$00,$ec,$03 ; y vel =  0.77, x vel =  3.92 (3:22 o'clock)
    .byte $84,$01,$b0,$03 ; y vel =  1.52, x vel =  3.69 (3:45 o'clock)
    .byte $38,$02,$50,$03 ; y vel =  2.22, x vel =  3.31 (4:08 o'clock)
    .byte $d4,$02,$d4,$02 ; y vel =  2.83, x vel =  2.83 (4:30 o'clock)
    .byte $50,$03,$38,$02 ; y vel =  3.31, x vel =  2.22 (4:52 o'clock)
    .byte $b0,$03,$84,$01 ; y vel =  3.69, x vel =  1.52 (5:15 o'clock)
    .byte $ec,$03,$c4,$00 ; y vel =  3.92, x vel =  0.77 (5:38 o'clock)
    .byte $00,$04,$00,$00 ; y vel =  4.00, x vel =  0.00 (6:00 o'clock)
    .byte $ec,$03,$3c,$ff ; y vel =  3.92, x vel = -0.77 (6:22 o'clock)
    .byte $b0,$03,$7c,$fe ; y vel =  3.69, x vel = -1.52 (6:45 o'clock)
    .byte $50,$03,$c8,$fd ; y vel =  3.31, x vel = -2.22 (7:08 o'clock)
    .byte $d4,$02,$2c,$fd ; y vel =  2.83, x vel = -2.83 (7:30 o'clock)
    .byte $38,$02,$b0,$fc ; y vel =  2.22, x vel = -3.31 (7:52 o'clock)
    .byte $84,$01,$50,$fc ; y vel =  1.52, x vel = -3.69 (8:15 o'clock)
    .byte $c4,$00,$14,$fc ; y vel =  0.77, x vel = -3.92 (8:38 o'clock)
    .byte $00,$00,$00,$fc ; y vel =  0.00, x vel = -4.00 (9:00 o'clock)
    .byte $3c,$ff,$14,$fc ; y vel = -0.77, x vel = -3.92 (9:22 o'clock)
    .byte $7c,$fe,$50,$fc ; y vel = -1.52, x vel = -3.69 (9:45 o'clock)
    .byte $c8,$fd,$b0,$fc ; y vel = -2.22, x vel = -3.31 (10:08 o'clock)
    .byte $2c,$fd,$2c,$fd ; y vel = -2.83, x vel = -2.83 (10:30 o'clock)
    .byte $b0,$fc,$c8,$fd ; y vel = -3.31, x vel = -2.22 (10:52 o'clock)
    .byte $50,$fc,$7c,$fe ; y vel = -3.69, x vel = -1.52 (11:15 o'clock)
    .byte $14,$fc,$3c,$ff ; y vel = -3.92, x vel = -0.77 (11:38 o'clock)
    .byte $00,$fc,$00,$00 ; y vel = -4.00, x vel =  0.00 (12:00 o'clock)
    .byte $14,$fc,$c4,$00 ; y vel = -3.92, x vel =  0.77 (12:22 o'clock)
    .byte $50,$fc,$84,$01 ; y vel = -3.69, x vel =  1.52 (12:45 o'clock)
    .byte $b0,$fc,$38,$02 ; y vel = -3.31, x vel =  2.22 (1:08 o'clock)
    .byte $2c,$fd,$d4,$02 ; y vel = -2.83, x vel =  2.83 (1:30 o'clock)
    .byte $c8,$fd,$50,$03 ; y vel = -2.22, x vel =  3.31 (1:52 o'clock)
    .byte $7c,$fe,$b0,$03 ; y vel = -1.52, x vel =  3.69 (2:15 o'clock)
    .byte $3c,$ff,$ec,$03 ; y vel = -0.77, x vel =  3.92 (2:38 o'clock)

; 1.375x
bullet_velocity_01:
    .byte $00,$00,$80,$05 ; y vel =  0.00, x vel =  5.50 (3:00 o'clock)
    .byte $0d,$01,$64,$05 ; y vel =  1.05, x vel =  5.39 (3:22 o'clock)
    .byte $15,$02,$12,$05 ; y vel =  2.08, x vel =  5.07 (3:45 o'clock)
    .byte $0d,$03,$8e,$04 ; y vel =  3.05, x vel =  4.55 (4:08 o'clock)
    .byte $e3,$03,$e3,$03 ; y vel =  3.89, x vel =  3.89 (4:30 o'clock)
    .byte $8e,$04,$0d,$03 ; y vel =  4.55, x vel =  3.05 (4:52 o'clock)
    .byte $12,$05,$15,$02 ; y vel =  5.07, x vel =  2.08 (5:15 o'clock)
    .byte $64,$05,$0d,$01 ; y vel =  5.39, x vel =  1.05 (5:38 o'clock)
    .byte $80,$05,$00,$00 ; y vel =  5.50, x vel =  0.00 (6:00 o'clock)
    .byte $64,$05,$f3,$fe ; y vel =  5.39, x vel = -1.05 (6:22 o'clock)
    .byte $12,$05,$eb,$fd ; y vel =  5.07, x vel = -2.08 (6:45 o'clock)
    .byte $8e,$04,$f3,$fc ; y vel =  4.55, x vel = -3.05 (7:08 o'clock)
    .byte $e3,$03,$1d,$fc ; y vel =  3.89, x vel = -3.89 (7:30 o'clock)
    .byte $0d,$03,$72,$fb ; y vel =  3.05, x vel = -4.55 (7:52 o'clock)
    .byte $15,$02,$ee,$fa ; y vel =  2.08, x vel = -5.07 (8:15 o'clock)
    .byte $0d,$01,$9c,$fa ; y vel =  1.05, x vel = -5.39 (8:38 o'clock)
    .byte $00,$00,$80,$fa ; y vel =  0.00, x vel = -5.50 (9:00 o'clock)
    .byte $f3,$fe,$9c,$fa ; y vel = -1.05, x vel = -5.39 (9:22 o'clock)
    .byte $eb,$fd,$ee,$fa ; y vel = -2.08, x vel = -5.07 (9:45 o'clock)
    .byte $f3,$fc,$72,$fb ; y vel = -3.05, x vel = -4.55 (10:08 o'clock)
    .byte $1d,$fc,$1d,$fc ; y vel = -3.89, x vel = -3.89 (10:30 o'clock)
    .byte $72,$fb,$f3,$fc ; y vel = -4.55, x vel = -3.05 (10:52 o'clock)
    .byte $ee,$fa,$eb,$fd ; y vel = -5.07, x vel = -2.08 (11:15 o'clock)
    .byte $9c,$fa,$f3,$fe ; y vel = -5.39, x vel = -1.05 (11:38 o'clock)
    .byte $80,$fa,$00,$00 ; y vel = -5.50, x vel =  0.00 (12:00 o'clock)
    .byte $9c,$fa,$0d,$01 ; y vel = -5.39, x vel =  1.05 (12:22 o'clock)
    .byte $ee,$fa,$15,$02 ; y vel = -5.07, x vel =  2.08 (12:45 o'clock)
    .byte $72,$fb,$0d,$03 ; y vel = -4.55, x vel =  3.05 (1:08 o'clock)
    .byte $1d,$fc,$e3,$03 ; y vel = -3.89, x vel =  3.89 (1:30 o'clock)
    .byte $f3,$fc,$8e,$04 ; y vel = -3.05, x vel =  4.55 (1:52 o'clock)
    .byte $eb,$fd,$12,$05 ; y vel = -2.08, x vel =  5.07 (2:15 o'clock)
    .byte $f3,$fe,$64,$05 ; y vel = -1.05, x vel =  5.39 (2:38 o'clock)

; 1.75x
bullet_velocity_02:
    .byte $00,$00,$00,$07 ; y vel =  0.00, x vel =  7.00 (3:00 o'clock)
    .byte $57,$01,$dd,$06 ; y vel =  1.34, x vel =  6.86 (3:22 o'clock)
    .byte $a7,$02,$74,$06 ; y vel =  2.65, x vel =  6.45 (3:45 o'clock)
    .byte $e2,$03,$cc,$05 ; y vel =  3.88, x vel =  5.80 (4:08 o'clock)
    .byte $f3,$04,$f3,$04 ; y vel =  4.95, x vel =  4.95 (4:30 o'clock)
    .byte $cc,$05,$e2,$03 ; y vel =  5.80, x vel =  3.88 (4:52 o'clock)
    .byte $74,$06,$a7,$02 ; y vel =  6.45, x vel =  2.65 (5:15 o'clock)
    .byte $dd,$06,$57,$01 ; y vel =  6.86, x vel =  1.34 (5:38 o'clock)
    .byte $00,$07,$00,$00 ; y vel =  7.00, x vel =  0.00 (6:00 o'clock)
    .byte $dd,$06,$a9,$fe ; y vel =  6.86, x vel = -1.34 (6:22 o'clock)
    .byte $74,$06,$59,$fd ; y vel =  6.45, x vel = -2.65 (6:45 o'clock)
    .byte $cc,$05,$1e,$fc ; y vel =  5.80, x vel = -3.88 (7:08 o'clock)
    .byte $f3,$04,$0d,$fb ; y vel =  4.95, x vel = -4.95 (7:30 o'clock)
    .byte $e2,$03,$34,$fa ; y vel =  3.88, x vel = -5.80 (7:52 o'clock)
    .byte $a7,$02,$8c,$f9 ; y vel =  2.65, x vel = -6.45 (8:15 o'clock)
    .byte $57,$01,$23,$f9 ; y vel =  1.34, x vel = -6.86 (8:38 o'clock)
    .byte $00,$00,$00,$f9 ; y vel =  0.00, x vel = -7.00 (9:00 o'clock)
    .byte $a9,$fe,$23,$f9 ; y vel = -1.34, x vel = -6.86 (9:22 o'clock)
    .byte $59,$fd,$8c,$f9 ; y vel = -2.65, x vel = -6.45 (9:45 o'clock)
    .byte $1e,$fc,$34,$fa ; y vel = -3.88, x vel = -5.80 (10:08 o'clock)
    .byte $0d,$fb,$0d,$fb ; y vel = -4.95, x vel = -4.95 (10:30 o'clock)
    .byte $34,$fa,$1e,$fc ; y vel = -5.80, x vel = -3.88 (10:52 o'clock)
    .byte $8c,$f9,$59,$fd ; y vel = -6.45, x vel = -2.65 (11:15 o'clock)
    .byte $23,$f9,$a9,$fe ; y vel = -6.86, x vel = -1.34 (11:38 o'clock)
    .byte $00,$f9,$00,$00 ; y vel = -7.00, x vel =  0.00 (12:00 o'clock)
    .byte $23,$f9,$57,$01 ; y vel = -6.86, x vel =  1.34 (12:22 o'clock)
    .byte $8c,$f9,$a7,$02 ; y vel = -6.45, x vel =  2.65 (12:45 o'clock)
    .byte $34,$fa,$e2,$03 ; y vel = -5.80, x vel =  3.88 (1:08 o'clock)
    .byte $0d,$fb,$f3,$04 ; y vel = -4.95, x vel =  4.95 (1:30 o'clock)
    .byte $1e,$fc,$cc,$05 ; y vel = -3.88, x vel =  5.80 (1:52 o'clock)
    .byte $59,$fd,$74,$06 ; y vel = -2.65, x vel =  6.45 (2:15 o'clock)
    .byte $a9,$fe,$dd,$06 ; y vel = -1.34, x vel =  6.86 (2:38 o'clock)

; 1x incline
bullet_velocity_03:
    .byte $00,$00,$00,$04 ; y vel =  0.00, x vel =  4.00 (3:00 o'clock)
    .byte $d0,$01,$78,$03 ; y vel =  1.81, x vel =  3.47 (3:55 o'clock)
    .byte $00,$04,$00,$00 ; y vel =  4.00, x vel =  0.00 (6:00 o'clock)
    .byte $d0,$01,$88,$fc ; y vel =  1.81, x vel = -3.47 (8:05 o'clock)
    .byte $00,$00,$00,$fc ; y vel =  0.00, x vel = -4.00 (9:00 o'clock)
    .byte $30,$fe,$88,$fc ; y vel = -1.81, x vel = -3.47 (9:55 o'clock)
    .byte $00,$fc,$00,$00 ; y vel = -4.00, x vel =  0.00 (12:00 o'clock)
    .byte $30,$fe,$78,$03 ; y vel = -1.81, x vel =  3.47 (2:05 o'clock)

; 1.375x incline
bullet_velocity_04:
    .byte $00,$00,$80,$05 ; y vel =  0.00, x vel =  5.50 (3:00 o'clock)
    .byte $7e,$02,$c5,$04 ; y vel =  2.49, x vel =  4.77 (3:55 o'clock)
    .byte $80,$05,$00,$00 ; y vel =  5.50, x vel =  0.00 (6:00 o'clock)
    .byte $7e,$02,$3b,$fb ; y vel =  2.49, x vel = -4.77 (8:05 o'clock)
    .byte $00,$00,$80,$fa ; y vel =  0.00, x vel = -5.50 (9:00 o'clock)
    .byte $82,$fd,$3b,$fb ; y vel = -2.49, x vel = -4.77 (9:55 o'clock)
    .byte $80,$fa,$00,$00 ; y vel = -5.50, x vel =  0.00 (12:00 o'clock)
    .byte $82,$fd,$c5,$04 ; y vel = -2.49, x vel =  4.77 (2:05 o'clock)

; 1.75x incline
bullet_velocity_05:
    .byte $00,$00,$00,$07 ; y vel =  0.00, x vel =  7.00 (3:00 o'clock)
    .byte $2c,$03,$12,$06 ; y vel =  3.17, x vel =  6.07 (3:55 o'clock)
    .byte $00,$07,$00,$00 ; y vel =  7.00, x vel =  0.00 (6:00 o'clock)
    .byte $2c,$03,$ee,$f9 ; y vel =  3.17, x vel = -6.07 (8:05 o'clock)
    .byte $00,$00,$00,$f9 ; y vel =  0.00, x vel = -7.00 (9:00 o'clock)
    .byte $d4,$fc,$ee,$f9 ; y vel = -3.17, x vel = -6.07 (9:55 o'clock)
    .byte $00,$f9,$00,$00 ; y vel = -7.00, x vel =  0.00 (12:00 o'clock)
    .byte $d4,$fc,$12,$06 ; y vel = -3.17, x vel =  6.07 (2:05 o'clock)
    .byte $00,$21,$42,$61 ; y vel = 33.00, x vel = 97.26 (3:37 o'clock)
    .byte $7f,$9b,$b5,$cb ; y vel = -100.50, x vel = -52.29 (11:05 o'clock)
    .byte $dd,$ec,$f7,$fd ; y vel = -19.14, x vel = -2.04 (11:48 o'clock)
    .byte $ff,$fd,$f7,$ec ; y vel = -2.00, x vel = -19.04 (9:12 o'clock)
    .byte $dd,$cb,$b5,$9b ; y vel = -52.14, x vel = -100.29 (9:55 o'clock)
    .byte $80,$61,$42,$21 ; y vel = 97.50, x vel = 33.26 (5:22 o'clock)

; Y position adjust, X position adjust, offset to bullet_velocity_xx, sprite attribute
; first half is for when player fired bullet not on incline
bullet_pos_dir_tbl:
    .byte $f0,$02,$60,$02
    .byte $f3,$06,$70,$02
    .byte $fe,$08,$00,$02
    .byte $03,$08,$10,$02
    .byte $06,$08,$00,$02
    .byte $0b,$08,$00,$02
    .byte $04,$00,$20,$02
    .byte $0b,$f8,$40,$42 ; flip horizontally
    .byte $06,$f8,$40,$42 ; flip horizontally
    .byte $03,$f8,$30,$42 ; flip horizontally
    .byte $fe,$f8,$40,$42 ; flip horizontally
    .byte $f3,$fa,$50,$42 ; flip horizontally
    .byte $f0,$fe,$60,$42 ; flip horizontally

; on incline
    .byte $f0,$02,$18,$02
    .byte $f5,$06,$1c,$02
    .byte $fe,$08,$00,$02
    .byte $03,$08,$04,$02
    .byte $06,$08,$00,$02
    .byte $0b,$08,$00,$02
    .byte $04,$00,$08,$02
    .byte $0b,$f8,$10,$42 ; flip horizontally
    .byte $06,$f8,$10,$42 ; flip horizontally
    .byte $03,$f8,$0c,$42 ; flip horizontally
    .byte $fe,$f8,$10,$42 ; flip horizontally
    .byte $f5,$fa,$14,$42 ; flip horizontally
    .byte $f0,$fe,$18,$42 ; flip horizontally

; clear destroyed bullets, copy bullet data to general sprite buffers
; copies half of all bullets, alternating every frame which half
handle_player_bullets:
    ldx #$0f

@loop:
    jsr clear_bullet_if_destroyed ; set $13 to weapon type, then destroy the bullet
    dex
    bpl @loop

; copies values from bullet-specific buffers to the general sprite buffers
; copies half of all bullets, alternating every frame which half
cp_bullet_to_sprite_buffers:
    ldy #$07          ; initialize sprite buffer offset
    ldx #$0f          ; start at first bullet
    lda FRAME_COUNTER ; load frame counter (only half of bullets are drawn per frame)
    lsr
    bcs @loop         ; branch if starting at #$0f and going down by 2 every loop
    dex               ; start at #$0e and go down by 2 every loop

@loop:
    lda PLAYER_BULLET_SPRITE_CODE,x
    sta SPRITES,y                   ; copy sprite to sprite buffer
    lda PLAYER_BULLET_SPRITE_ATTR,x
    sta SPRITE_ATTR,y               ; copy sprite attribute to sprite attribute buffer
    lda PLAYER_BULLET_Y_POS,x
    sta SPRITE_Y_POS,y              ; copy y location to sprite y location buffer
    lda PLAYER_BULLET_X_POS,x
    sta SPRITE_X_POS,y              ; copy x location to sprite x location buffer
    dex
    dex                             ; move to next bullet (skips a slot every loop)
                                    ; only half of bullets drawn every other frame
    dey                             ; decrement sprite buffer write index
    bpl @loop
    rts

; input
;  * x - player bullet index
; output
;  * $13 - weapon type (e.g. #$01 - Machine, #$02 - Spray)
clear_bullet_if_destroyed:
    lda PLAYER_BULLET_WEAPON_TYPE,x ; load weapon type and player that created the bullet
    and #$0f                        ; strip rapid fire flag and bullet owner flags
    sta $13                         ; store weapon type
    ldy PLAYER_BULLET_STATE,x       ; (0 = unused, 1 = normal, 2 = destroyed, 3 = just created laser)
    bne @continue                   ; branch if not unused

@set_skip_collision:
    lda #$00
    sta PLAYER_BULLET_COLLISION_CODE,x
    rts

; ensure lasers are created after certain distance from each other
@just_created_laser:
    jsr bullet_apply_y_scroll ; adjust bullet Y position based on Y_SCROLL_SPEED
    dec PLAYER_BULLET_TIMER,x
    bne @set_skip_collision
    lda #$01                  ; set laser to normal state
    sta PLAYER_BULLET_STATE,x ; (0 = unused, 1 = normal, 2 = destroyed, 3 = just created laser)
    bne @set_skip_collision   ; always branch

; either destroyed state, or just created laser
@next_state:
    dey
    bne @just_created_laser         ; branch if state was 3 (just created laser)
    jsr bullet_apply_y_scroll       ; destroyed bullet, adjust bullet Y position based Y_SCROLL_SPEED
    lda #$04
    sta PLAYER_BULLET_SPRITE_CODE,x ; sprite_04 (circular explosion)
    dec PLAYER_BULLET_TIMER,x       ; decrement timer controlling when explosion sprite removed
                                    ; this is no longer the distance traveled, was set to #$06 in set_bullet_destroyed_state
    bne @set_skip_collision         ; branch if timer not elapsed

; clears player bullet's state, sprite, sprite attr and collision code
@clear_bullet_sprite:
    jmp clear_bullet_sprite

@continue:
    dey                                  ; decrement PLAYER_BULLET_STATE (normal goes to #$00)
    bne @next_state                      ; branch if in destroyed state
    inc PLAYER_BULLET_TIMER,x            ; normal bullet, increment distance traveled
    lda OVERHEAD_FLAG                    ; (0 = side view, 1 = overhead view)
    beq @apply_vel                       ; branch if side view level to apply velocity
    lda PLAYER_BULLET_TIMER,x            ; overhead view level, bullets only travel #$1a
    cmp #$1a                             ; for overhead level, F weapon splits at this distance
                                         ; and other weapon bullets are destroyed
    bcs create_f_child_flames_or_destroy ; branch if distance traveled is at least #$1a
                                         ; to create child flames for F weapon or destroy if not F weapon

@apply_vel:
    jsr set_l_f_bullet_sprite          ; set the player bullet sprite for L, F, and charged F weapons
    lda #$9f                           ; set base collision_box_tbl offset
    sta PLAYER_BULLET_COLLISION_CODE,x ; set bullet collision box code
    lda PLAYER_BULLET_Y_VEL_ACCUM,x    ; load accumulated fractional Y velocity
    clc                                ; clear carry in preparation for addition
    adc PLAYER_BULLET_Y_VEL_FRACT,x    ; add to fractional Y velocity
    sta PLAYER_BULLET_Y_VEL_ACCUM,x    ; update accumulated Y fractional velocity
    lda PLAYER_BULLET_Y_POS,x          ; load bullet Y position
    adc PLAYER_BULLET_Y_VEL_FAST,x     ; add fast Y velocity (and any overflow from fractional)
    sec                                ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED                 ; account for vertical scrolling this frame
    sta PLAYER_BULLET_Y_POS,x          ; update bullet Y position
    cmp #$f0                           ; see if bullet is in the bottom 6.25% of screen
    bcs @clear_bullet_sprite           ; branch if in bottom of screen to hide the bullet
    lda PLAYER_BULLET_X_VEL_ACCUM,x    ; load accumulated fractional X velocity
    adc PLAYER_BULLET_X_VEL_FRACT,x    ; add to fractional X velocity
    sta PLAYER_BULLET_X_VEL_ACCUM,x    ; update accumulated X fractional velocity
    lda PLAYER_BULLET_X_POS,x          ; load bullet X position
    adc PLAYER_BULLET_X_VEL_FAST,x     ; add fast X velocity (and any overflow from fractional)
    sta PLAYER_BULLET_X_POS,x          ; update bullet X position
    cmp #$08                           ; see if in top 3.125% of screen
    bcc @clear_bullet_sprite           ; branch if in top of screen to hide the bullet
    ldy PLAYER_BULLET_TIMER,x
    dey
    beq bullet_state_exit              ; branch if bullet just created
    lda $13                            ; bullet not just fired, load weapon type
    cmp #$06                           ; see if child flame from charged F weapon
    beq clear_bullet_if_traveled_far   ; branch if child flame to clear bullet if traveled more than #$0a
    lda PLAYER_BULLET_Y_POS,x
    cmp #$0c
    bcc bullet_state_exit              ; exit if bullet is too far to the left
    sbc #$04
    tay
    lda PLAYER_BULLET_X_POS,x
    jsr get_bg_collision_code          ; determine background collision at point (a,y)
    cmp #$06
    bcc @handle_bg_collision           ; branch if water, floor, or solid collision
    ldy INCLINE_BG_COLLISION_FLAG      ; see if incline bg collisions should be tested
                                       ; level 5 and level 8 do not have bullet-incline bg collisions
    bne @handle_bg_collision           ; branch to continue if inclines should count as bg collisions
    lda #$00                           ; use collision code 0 when colliding with inclines
                                       ; this prevents bullet-incline bg collisions
                                       ; so bullets travel through inclines

@handle_bg_collision:
    tay                                  ; transfer collision code to y
    lda bg_collision_test_tbl,y          ; load whether to consider collision
    beq bullet_state_exit                ; branch if bg collision should be ignored
    cpy #$03                             ; see if bullet collided with collapsible floor on level 7 (eggshells)
    bne create_f_child_flames_or_destroy ; branch if not collapsible floor
                                         ; to create child flames for F weapon or destroy
    jsr bullet_eggshell_collision        ; collided with collapsible floor on level 7 (eggshells)

; creates either 4 or 8 child flames for F weapon or charged F weapon respectively
; if not F weapon (nor charged F weapon), destroy the bullet
; input
;  * x - bullet offset
create_f_child_flames_or_destroy:
    ldy #$03                        ; assume non-charged F weapon and create #$04 child flames
    lda PLAYER_BULLET_WEAPON_TYPE,x ; load weapon type and player that created the bullet
    and #$0f                        ; strip rapid fire and bullet owner flags
    cmp #$04                        ; see if F (flame) weapon
    beq create_flame_split          ; branch if flame to create 4 child flames
    ldy #$07                        ; bullet isn't F weapon
                                    ; create #$08 child flames for a charged F weapon
    cmp #$05                        ; see if charged F weapon (flame)
    beq create_flame_split          ; branch if charged weapon to create #$08 child flames

; input
;  * x - bullet offset
set_bullet_destroyed_state:
    lda #$06
    sta PLAYER_BULLET_TIMER,x          ; set timer before circular explosion sprite removed (sprite_04)
    lda #$02                           ; set bullet destroyed state
    sta PLAYER_BULLET_STATE,x          ; (0 = unused, 1 = normal, 2 = destroyed)
    lda #$00
    sta PLAYER_BULLET_COLLISION_CODE,x ; clear collision code table offset, disable collisions

bullet_state_exit:
    rts

; if player bullet has traveled more than #$0a, then clear sprite, and collision code
; used only for child flame created by charged F weapon
; input
;  * x - player bullet offset
clear_bullet_if_traveled_far:
    lda PLAYER_BULLET_TIMER,x
    cmp #$0a
    bcc bullet_state_exit
    jmp clear_bullet_sprite   ; clear player bullet's state, sprite, sprite attr and collision code

; adjust bullet Y position based on Y_SCROLL_SPEED
; input
;  * x - player bullet offset
bullet_apply_y_scroll:
    lda PLAYER_BULLET_Y_POS,x ; load player bullet Y position
    sec                       ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED        ; subtract how much to scroll the screen vertically this frame
    sta PLAYER_BULLET_Y_POS,x ; set new Y position
    rts

; flame weapon collision with enemy, or traveled #$1a distance
; split into 4 smaller flame balls
; input
;  * x - bullet offset
;  * y = number of children flames to spawn
; output
;  * $10 - !(BUG) accidentally overwrites ENEMY_CURRENT_SLOT and doesn't restore it
create_flame_split:
    stx $10 ; store flame bullet offset
    sty $00 ; store number of children flames to create

@child_flame_loop:
    ldx $10                         ; load parent flame bullet offset
    lda PLAYER_BULLET_WEAPON_TYPE,x ; load weapon type and player that created the bullet
    and #$40                        ; see which player fired the bullet
    sta $12                         ; store bullet owner
    lda #$06
    sta $0f                         ; used to set child flame as F weapon (see init_bullet_vel_sprite_play_sound)
    rol
    rol
    rol
    and #$01                        ; !(HUH) I think this is always #$00, carry is always clear in this loop
    tax                             ; start searching from bullet slot #$00
    ldy #$06                        ; start at slot #$00 and search #$06 slots
    jsr find_bullet_slot            ; find a bullet slot for the child flame
    bcs @set_bullet_destroyed_state ; branch if unable to find a free bullet slot
                                    ; to destroy bullet without creating child flame
    jsr @init_child_flame           ; create child flame bullet
    dec $00                         ; decrement number of child flames remaining to create
    bpl @child_flame_loop           ; branch to loop to next child flame if more to create

; destroy main initial flame
@set_bullet_destroyed_state:
    ldx $10                        ; load parent flame bullet offset
    jmp set_bullet_destroyed_state ; destroy bullet x

; create child flame
@init_child_flame:
    ldy $10                               ; load parent flame bullet offset
    lda PLAYER_BULLET_Y_POS,y
    sta $0c                               ; set main flame's Y position
    lda PLAYER_BULLET_X_POS,y
    sta $0d                               ; load main flame's X position
    ldy $00                               ; load child flame index
    lda child_flame_dir_offset_tbl,y
    sta $0a                               ; set bullet direction offset (bullet_velocity_xx)
    lda $0c
    sta PLAYER_BULLET_Y_POS,x             ; set child flame initial Y position
    lda $0d
    sta PLAYER_BULLET_X_POS,x             ; set child flame initial X position
    lda #$02
    sta PLAYER_BULLET_SPRITE_ATTR,x
    lda #$00                              ; bullet speed code (bullet_velocity_00)
    jmp init_bullet_vel_sprite_play_sound ; initialize the player's bullet velocity, sprite, damage, and plays firing sound

; specifies offsets into bullet_velocity_ptr_tbl
child_flame_dir_offset_tbl:
    .byte $30,$50,$10,$70,$20,$60,$40,$00

; sets the player bullet sprite for L, F, and charged F weapons
; input
;  * $13 - weapon type (e.g. #$01 - Machine, #$02 - Spray)
set_l_f_bullet_sprite:
    lda $13                        ; load weapon type
    cmp #$03
    bcc p_bullet_sprite_routine_00 ; branch to exit if not L, F, nor charged F weapon bullet
    jsr run_routine_from_tbl_below ; weapon is laser (L) or flame (F), run routine

p_bullet_sprite_routine_ptr_tbl:
    .addr p_bullet_sprite_routine_00 ; noop
    .addr p_bullet_sprite_routine_00 ; noop
    .addr p_bullet_sprite_routine_00 ; noop
    .addr p_bullet_sprite_routine_03 ; L weapon - sets sprite based on PLAYER_BULLET_AIM_DIR
    .addr p_bullet_sprite_routine_04 ; F weapon - set sprite based on PLAYER_BULLET_TIMER
    .addr p_bullet_sprite_routine_05 ; charged F weapon - set sprite based on PLAYER_BULLET_TIMER
    .addr p_bullet_sprite_routine_00 ; noop

p_bullet_sprite_routine_00:
    rts

; L weapon - sets sprite based on PLAYER_BULLET_AIM_DIR
p_bullet_sprite_routine_03:
    ldy PLAYER_BULLET_AIM_DIR,x
    lda laser_bullet_sprite_tbl,y
    sta PLAYER_BULLET_SPRITE_CODE,x
    rts

; sprite_15, sprite_16, sprite_17, sprite_18
laser_bullet_sprite_tbl:
    .byte $16,$18,$15,$18,$16,$17,$15,$17

; F weapon - set sprite based on distance traveled
p_bullet_sprite_routine_04:
    lda #$19                         ; sprite_19
    ldy PLAYER_BULLET_TIMER,x
    cpy #$04
    bcc p_bullet_sprite_routine_exit ; branch if distance is less than #$04
                                     ; to set sprite to sprite_19
    lda #$03                         ; sprite_03
    bcs p_bullet_sprite_routine_exit ; always branch to set sprite to sprite_03

; charged F weapon - set sprite based on PLAYER_BULLET_TIMER
p_bullet_sprite_routine_05:
    lda #$19
    ldy PLAYER_BULLET_TIMER,x
    cpy #$04
    bcc p_bullet_sprite_routine_exit ; branch if distance is less than #$04
                                     ; to set sprite to sprite_19
    lda #$03                         ; sprite_03
    cpy #$0c
    bcc p_bullet_sprite_routine_exit ; branch if distance is less than #$0c
                                     ; to set sprite to sprite_03
    lda #$1a                         ; distance more than #$0c, set to sprite_1a

p_bullet_sprite_routine_exit:
    sta PLAYER_BULLET_SPRITE_CODE,x
    rts

; bullet collided with collapsible floor on level (eggshell)
; input
;  * x - player bullet offset
bullet_eggshell_collision:
    lda PLAYER_BULLET_X_POS,x
    lsr
    lsr
    lsr
    sta $00                   ; set PPU address low byte
    lda #$08
    sta $01
    lda PLAYER_BULLET_Y_POS,x
    clc                       ; clear carry in preparation for addition
    adc #$fc
    clc                       ; clear carry in preparation for addition
    adc Y_SCROLL              ; add PPU vertical scroll
    bcs @handle_overflow
    cmp #$f0
    bcc @continue

@handle_overflow:
    adc #$0f

@continue:
    and #$f0
    asl
    rol $01
    asl
    rol $01                     ; PPU address high byte
    ora $00
    and #$fe
    sta $00
    stx $10                     ; backup player bullet offset
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$40
    bcs @exit                   ; exit if graphics buffer full
    lda #$06                    ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x   ; set block mode
    inx                         ; increment graphics buffer write offset
    lda $04
    eor #$80
    tay
    jsr check_bg_collision_at_y ; check bg collision at BG_COLLISION_DATA,y
    asl
    asl
    tay
    lda #$02
    sta $02                     ; set bg tile count

@replace_bg_tile:
    lda $01
    sta CPU_GRAPHICS_BUFFER,x         ; set PPU address high byte
    inx                               ; increment graphics buffer write offset
    lda $00                           ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x         ; set PPU address low byte
    inx                               ; increment graphics buffer write offset
    lda #$02
    sta CPU_GRAPHICS_BUFFER,x         ; writing 2 bytes to PPU
    inx                               ; increment graphics buffer write offset
    lda eggshell_tile_tbl-4,y
    sta CPU_GRAPHICS_BUFFER,x
    iny                               ; increment tile
    inx                               ; increment graphics buffer write offset
    lda eggshell_tile_tbl-4,y
    sta CPU_GRAPHICS_BUFFER,x
    iny
    inx                               ; increment graphics buffer write offset
    lda $00
    clc                               ; clear carry in preparation for addition
    adc #$20                          ; move down one row
    sta $00                           ; set next ppu address low byte
    dec $02                           ; decrement number of bg tiles to replace
    bne @replace_bg_tile              ; branch if more tiles to replace
    lda #$ff                          ; replaced all tiles, write end of block flag
    sta CPU_GRAPHICS_BUFFER,x
    inx                               ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET        ; update offset
    jsr set_bg_col_destroyed_eggshell ; set bg collision for destroyed eggshell

@exit:
    ldx $10 ; restore player bullet offset
    rts

eggshell_tile_tbl:
    .byte $00,$00 ; replace both tiles with black (destroyed)
    .byte $00,$00 ; replace both tiles with black (destroyed)
    .byte $6e,$6f ; really weakened egg shell top (2nd hit)
    .byte $70,$71 ; really weakened egg shell bottom (2nd hit)
    .byte $6a,$6b ; weakened egg shell top (1st hit)
    .byte $6c,$6d ; weakened egg shell bottom (1st hit)

; set bg collision for destroyed eggshell
; input
;  * $04 - used for BG_COLLISION_DATA offset
;  * $03 - eggshell bg collision table offset
set_bg_col_destroyed_eggshell:
    lda $04
    eor #$80
    tax
    lda $03
    and #$01
    tay                               ; set eggshell_bg_collision_tbl offset
    lda BG_COLLISION_DATA,x
    sec                               ; set carry flag in preparation for subtraction
    sbc eggshell_bg_collision_tbl,y
    sta BG_COLLISION_DATA,x
    and eggshell_bg_collision_tbl+2,y
    bne @exit
    lda BG_COLLISION_DATA,x
    ora eggshell_bg_collision_tbl+6,y
    sta BG_COLLISION_DATA,x
    ldx $04
    lda BG_COLLISION_DATA,x
    and eggshell_bg_collision_tbl+4,y
    sta BG_COLLISION_DATA,x

@exit:
    rts

eggshell_bg_collision_tbl:
    .byte $10,$01
    .byte $f0,$0f
    .byte $0f,$f0
    .byte $30,$03

enemy_gen_routine_exit:
    rts

; run timer logic for ENEMY_GEN_DELAY and if delay elapsed, generate random enemy
enemy_gen_routine:
    lda BOSS_DEFEATED_FLAGS
    bne enemy_gen_routine_exit ; exit if defeated boss
    bit ENEMY_GEN_CTRL
    bvs enemy_gen_routine_exit ; exit if random enemy generation is paused
    lda ENEMY_GEN_CTRL
    lsr                        ; push whether or not vertical level into carry
    lda X_SCREEN               ; assume not vertical level
    bcc @continue              ; branch if not vertical level
    lda Y_SCREEN               ; vertical level

@continue:
    sta $16                ; either X_SCREEN, or Y_SCREEN
                           ; store screen number
    cmp NT_SCREEN_COUNT    ; compare to currently known screen number
    beq @enemy_gen_routine ; branch if player hasn't changed nametable screens
    sta NT_SCREEN_COUNT    ; player moved to next screen, reset ENEMY_GEN_COUNT
    lda #$00
    sta ENEMY_GEN_COUNT    ; moved to next screen, reset enemy generation counter

@enemy_gen_routine:
    lda ENEMY_GEN_ROUTINE          ; enemy_gen_routine_ptr_tbl offset
    jsr run_routine_from_tbl_below

enemy_gen_routine_ptr_tbl:
    .addr enemy_gen_routine_00 ; initialize ENEMY_GEN_DELAY
    .addr enemy_gen_routine_01 ; decrement delay, if elapsed, create enemy

; determines initial enemy generation delay based on
;  * whether the game has been completed
;  * the current ENEMY_DIFFICULTY
enemy_gen_routine_00:
    lda CURRENT_LEVEL               ; !(BUG) a is immediately overwritten
                                    ; this should probably have been ldy CURRENT_LEVEL
    lda level_enemy_gen_delay_tbl,y ; y is actually PRG bank number, e.g. #$33 and not current level
                                    ; this causes the delay to always be #$80
    beq @exit
    sta ENEMY_GEN_DELAY             ; reset enemy generation delay
    ldy GAME_COMPLETED              ; load whether or not the game has been completed
    beq @adj_for_difficulty         ; branch if haven't beaten the game yet
    cpy #$03                        ; player(s) have beaten the game and are playing again
    bcc @adj_for_game_completed     ; should always branch because GAME_COMPLETED is only every #$00 or #$01
    ldy #$03                        ; unlike Contra, it doesn't count the number of times you've beaten the game

; game has been beaten at least once, subtract #$10 * GAME_COMPLETED
@adj_for_game_completed:
    lda #$10
    jsr @set_enemy_gen_delay ; subtract #$10 * GAME_COMPLETED from ENEMY_GEN_DELAY

; adjust initial enemy generation delay based on ENEMY_DIFFICULTY
@adj_for_difficulty:
    ldy ENEMY_DIFFICULTY     ; load current difficulty
    beq @adv_routine         ; branch to not adjust initial enemy generation delay
    lda #$04                 ; difficulty is non-zero, subtract #$04 * ENEMY_DIFFICULTY from ENEMY_GEN_DELAY
    jsr @set_enemy_gen_delay ; subtract #$04 * ENEMY_DIFFICULTY from ENEMY_GEN_DELAY

@adv_routine:
    inc ENEMY_GEN_ROUTINE ; increment enemy_gen_routine_ptr_tbl routine

@exit:
    rts

; subtracts a * y from ENEMY_GEN_DELAY
; input
;  * a - amount to subtract each loop
;  * y - number of loops
@set_enemy_gen_delay:
    sta $08             ; set amount to subtract per loop in $08
    lda ENEMY_GEN_DELAY ; load initialized delay

@loop:
    dey                 ; decrement loop counter
    bmi @exit           ; exit if finished subtracting
    sec                 ; set carry flag in preparation for subtraction
    sbc $08             ; subtract specified amount
    bcc @exit           ; exit if overflow
    sta ENEMY_GEN_DELAY ; update delay to lower value
    bcs @loop           ; always branch to continue loop

; if delay not elapsed, decrements ENEMY_GEN_DELAY based on scroll
;  * #$01 per frame when not scrolling, or even frame
;  * #$02 per frame when scrolling (odd frames only)
; if delay elapsed, create soldiers or ladybugs
; input
;  * $16 - number of nametable screens scrolled, used as offset into enemy_gen_lvl_0x
enemy_gen_routine_01:
    lda FRAME_COUNTER  ; load frame counter
    lsr
    bcc @continue      ; branch if even frame to subtract 1 and continue logic
    lda ENEMY_GEN_CTRL ; odd frame, check if scrolling
    ldy #$02           ; when scrolling, subtract #$02 from ENEMY_GEN_DELAY
    lsr                ; push whether or not to adjust enemy generation based on vertical scroll
    lda X_SCROLL_SPEED ; see if scrolling horizontally
    bcc @set_scrolling
    lda Y_SCROLL_SPEED ; see if scrolling vertically

@set_scrolling:
    sta $17                 ; store scrolling flag (1 = scrolling, 0 = not scrolling)
    bne @set_delay_continue ; branch if scrolling to subtract #$02 and continue

@continue:
    ldy #$01 ; subtract #$01 from ENEMY_GEN_DELAY

@set_delay_continue:
    sty $08             ; set amount to subtract this frame
                        ; either #$01 or #$02 depending on scrolling (and odd frame) or not
    lda ENEMY_GEN_DELAY
    beq @delay_elapsed  ; branch if delay elapsed
    sec
    sbc $08             ; subtract either #$01 or #$02 depending on scroll
    bcs @set_delay_exit ; branch if delay hasn't elapsed to set delay and exit
    lda #$00

@set_delay_exit:
    sta ENEMY_GEN_DELAY
    rts

; ENEMY_GEN_DELAY elapsed
; input
;  * $16 - number of nametable screens scrolled, used as offset into enemy_gen_lvl_0x
@delay_elapsed:
    lda CURRENT_LEVEL   ; load current level
    asl                 ; double since each entry is a #$02 byte address
    asl                 ; double since every level has #$02 addresses
    tay                 ; transfer to offset register
    lda ENEMY_GEN_CTRL  ; load various control flags for enemy generation
    lsr                 ; push whether or not to adjust enemy generation based on vertical scroll
    bcc @read_ctrl_byte ; branch if not vertical level (level 2, 5, 6, or 7)
    iny                 ; vertical level (level 2, 5, 6, or 7)
                        ; !(OBS) in the table, every entry is duplicated, at one point developers
                        ; probably had more control codes, but decided to simplify
    iny                 ; if vertical level, use second address for level in the table

@read_ctrl_byte:
    lda enemy_gen_lvl_ptr_tbl,y
    sta $08
    lda enemy_gen_lvl_ptr_tbl+1,y
    sta $09
    ldy $16                         ; either X_SCREEN, or Y_SCREEN
    lda ($08),y                     ; load enemy generation control byte for the current screen
    cmp #$ff                        ; see if enemies should generate for current screen
    bne @find_pos                   ; branch if enemies should be generated for screen
    jmp init_enemy_gen_routine_exit ; exit if enemies don't generate for the current screen

@find_pos:
    sty $0f                         ; store enemy_gen_lvl_0x offset in $0f
    jsr find_gen_enemy_pos
    bcs @found_pos                  ; branch if found enemy position
    jmp init_enemy_gen_routine_exit ; exit if unable to find position

@found_pos:
    lda ENEMY_GEN_NUM               ; load number of enemies to generate in quick succession
    bne @gen_enemy                  ; branch if more enemies to generate (skip enemy creation prevention checks)
    ldy $0f                         ; load specific control byte offset
    lda ($08),y
    bpl @check_odds_gen_enemy
    lda RANDOM_NUM                  ; control byte was positive, load random number
    and #$02
    bne init_enemy_gen_routine_exit ; 50/50 chance to not generate enemy
    beq @gen_enemy                  ; always branch, 50/50 chance to continue to create enemy

@check_odds_gen_enemy:
    lda ($08),y
    asl
    bpl @gen_enemy                  ; always create enemy if bit 7 is 0 and bit 6 is 0
    lda RANDOM_NUM                  ; bit 6 set, load random number
    and #$0c
    bne init_enemy_gen_routine_exit ; 75% chance to exit without generating enemy

@gen_enemy:
    lda ENEMY_GEN_CTRL                     ; load random enemy generation control
    bpl @side_view_lvl_gen_enemy           ; branch if not an overhead level
    jmp create_overhead_soldier_or_ladybug ; overhead level jump to create enemy
                                           ; either overhead soldier or alien ladybug

@side_view_lvl_gen_enemy:
    jsr gen_enemy_player_edge_check ; check if player is close to the edge of the screen where enemy is being generated
    bcs init_enemy_gen_routine_exit ; exit without generating enemy if player near enemy being generated
    lda ENEMY_GEN_NUM               ; load number of enemies to generate in quick succession
    beq @check_scroll_gen
    jmp gen_enemy_short_delay       ; at least one more soldier to enemy, generate it
                                    ; generate a soldier, set generation delay to #$10 frames

@check_scroll_gen:
    lda $17                    ; load if scrolling vertically or horizontally
    beq @gen_soldier           ; branch if not scrolling
    lda RANDOM_NUM             ; load random number
    and #$03                   ; when scrolling, generate 3 soldiers 3/4 of the time
    bne @gen_soldier           ; branch 1/4 of the time to generate one soldier
    jmp gen_3_soldiers_quickly ; 3/4 of the time, 3 soldiers will be generated in quick succession

@gen_soldier:
    jsr create_enemy_slot_0_to_3     ; create an enemy of type y in between slot 0 and slot 3 inclusively
    bne init_enemy_gen_routine_exit  ; exit if unable to create enemy
    ldy #$03                         ; enemy type #$03 (soldier)
    jsr set_enemy_collision_and_type ; set the collision box type, and enemy type based on y
    ldy $0f                          ; load enemy generation configuration byte offset
    lda ($08),y                      ; load enemy generation configuration byte
    and #$0f                         ; strip to just the row within gen_soldier_enemy_attr_tbl to use
    asl
    asl                              ; quadruple since every row in gen_soldier_enemy_attr_tbl has #$0 entries
    sta $0a                          ; store base row
    lda FRAME_COUNTER                ; load frame counter
    lsr                              ; to use for randomly selecting within the row
    lsr
    lsr
    lsr
    and #$03                         ; get random number between #$00 and #$03
    clc
    adc $0a                          ; randomly select from the row
    tay                              ; transfer to offset register
    lda gen_soldier_enemy_attr_tbl,y ; load soldier attributes
    sta ENEMY_ATTRIBUTES,x           ; set soldier enemy attributes
                                     ; soldier direction (bit 0 of attributes) set a few lines down

; input
;  * $10 - enemy Y position
;  * $11 - enemy X position
set_enemy_pos_init_gen_routine_exit:
    lda $10                               ; load enemy Y position
    sec                                   ; set carry flag in preparation for subtraction
    sbc #$12                              ; subtract #$12
    sta ENEMY_Y_POS,x                     ; set created enemy's Y position
    lda $11                               ; load created enemy's X position
    sta ENEMY_X_POS,x                     ; set created enemy's X position
    bmi inc_enemy_count_init_routine_exit ; branch if enemy is on right side of screen
                                          ; soldier already set to walk left
    inc ENEMY_ATTRIBUTES,x                ; soldier coming from left, set walk direction to be to the right

; increments number of enemies generated for current screen,
; resets ENEMY_GEN_ROUTINE and then exits
inc_enemy_count_init_routine_exit:
    inc ENEMY_GEN_COUNT ; increment number of enemies generated for the current screen

init_enemy_gen_routine_exit:
    lda #$00
    sta ENEMY_GEN_ROUTINE ; set routine to initialize ENEMY_GEN_DELAY
    rts

; create either an overhead soldier or an alien ladybug, depending on $94
create_overhead_soldier_or_ladybug:
    jsr create_enemy_slot_0_to_3
    bne init_enemy_gen_routine_exit ; branch if enemy slot not found
    ldy #$63                        ; initialized enemy slot, enemy type #$63 (overhead soldier)
    lda ENEMY_GEN_CTRL
    lsr
    lsr
    bcc @continue                   ; branch if creating overhead soldier
    ldy #$24                        ; bit 1 set, enemy type #$24 (alien ladybug)

@continue:
    jsr set_enemy_collision_and_type       ; set the collision box type, and enemy type based on y
    ldy $0f
    lda ($08),y
    and #$0f
    asl
    asl
    sta $0a
    lda RANDOM_NUM                         ; load random number
    lsr
    lsr
    and #$03
    clc
    adc $0a
    tay                                    ; load random attributes
    lda gen_overhead_enemy_attr_tbl,y
    sta ENEMY_ATTRIBUTES,x
    lda $11
    sta ENEMY_X_POS,x
    lda $10
    sta ENEMY_Y_POS,x
    bpl @inc_enemy_count_init_routine_exit ; branch if enemy is on top half of screen
    inc ENEMY_ATTRIBUTES,x

@inc_enemy_count_init_routine_exit:
    jmp inc_enemy_count_init_routine_exit ; increment number of enemies generated for current screen
                                          ; reset ENEMY_GEN_ROUTINE and exit

gen_3_soldiers_quickly:
    lda #$03
    sta ENEMY_GEN_NUM ; generate #$03 soldiers in quick succession

; generates a soldier, then sets the generation delay to #$10 frames
; input
;  * ENEMY_GEN_NUM - the number of soldiers to generate in quick succession
gen_enemy_short_delay:
    jsr create_enemy_slot_0_to_3
    bne init_enemy_gen_routine_exit
    ldy #$03                                ; enemy type #$03 (soldier)
    jsr set_enemy_collision_and_type        ; set the collision box type, and enemy type based on y
    jsr set_enemy_pos_init_gen_routine_exit ; set soldier position, and ENEMY_GEN_ROUTINE to #$00
    inc ENEMY_GEN_ROUTINE                   ; set ENEMY_GEN_ROUTINE back to #$01
    lda #$10
    sta ENEMY_GEN_DELAY
    dec ENEMY_GEN_NUM                       ; decrement number of soldiers to generate in quick succession
    beq init_enemy_gen_routine_exit         ; branch if finished generating soldiers
                                            ; to set ENEMY_GEN_ROUTINE to #$00 and exit
    rts

; check if player is close to edge of the screen where enemy will be generated
; and if so, prevent random enemy generation unless game has been beaten
; or if player has stayed on screen for a long time
; input
;  * $11 - enemy X position
; output
;  * carry - set when player close to edge where enemy will be generated, clear otherwise
gen_enemy_player_edge_check:
    lda ENEMY_GEN_COUNT ; see how many enemies have been generated for the current screen
    cmp #$1e            ; see if player(s) have stayed on the same screen for a while
                        ; and #$1e enemies have been generated
    bcs @exit_clear     ; branch top skip edge check if player has been on screen for a while
                        ; to discourage player from racking up points attacking generated enemies
    lda GAME_COMPLETED  ; load whether or not the game has been completed
    bne @exit_clear     ; exit if game has been beaten to allow enemies to be generated
                        ; even when player close to edge
    ldy #$00            ; player hasn't beaten game, assume enemy on left side of screen
    lda $11             ; load enemy X position of enemy to generate
    bpl @continue       ; brach if on left side of screen
    iny                 ; enemy on right side of screen, increment y

@continue:
    ldx #$01 ; player index loop index

@loop:
    lda PLAYER_GAME_OVER_STATUS,x ; load player game over status (0 = not game over, 1 = game over)
    bne @next
    lda PLAYER_SPRITE_X_POS,x
    cmp enemy_gen_edge_tbl,y      ; compare player position to either left edge point or right edge point
    ror                           ; rotate player X position, pulling carry into bit 7
    eor enemy_gen_edge_tbl,y
    bpl @exit_set                 ; branch if player near edge
                                  ; on left side of screen and left of #$38 X position
                                  ; or if player on right side of screen and right of #$c8 X position

@next:
    dex       ; move to next player index
    bpl @loop ; branch if p1 hasn't been tested

@exit_clear:
    clc
    rts ; exit with no player near edge flag

@exit_set:
    sec
    rts ; exit with player near edge flag set

enemy_gen_edge_tbl:
    .byte $38,$c8

; find position to generate enemy
;  * for side-view levels
;    * X position position is randomly select from left and right edges
;    * Y position can be random or from enemy_gen_y_pos_ctrl_tbl depending on level
; input
;  * $0f - enemy_gen_lvl_0x offset
; output
;  * $10 - Y position
;  * $11 - X position
;  * carry - clear when unable to find position, set when found
find_gen_enemy_pos:
    lda ENEMY_GEN_CTRL              ; load enemy generation configuration byte
                                    ; bit 7 is set for overhead levels
    bpl @side_view                  ; branch if side-view level
    jmp overhead_find_gen_enemy_pos ; overhead level

@side_view:
    lda FRAME_COUNTER              ; load frame counter
    adc RANDOM_NUM
    sta RANDOM_NUM                 ; randomize random number
    lsr
    and #$0f                       ; get a random number between #$00 and #$0f inclusively
    tay                            ; transfer to offset register
    lda enemy_gen_x_pos_tbl,y      ; load initial X position (either right or left edge)
    sta $11                        ; set X position
    ldy CURRENT_LEVEL              ; load current level
    lda enemy_gen_y_pos_ctrl_tbl,y ; load whether to randomize y location or use
    beq @continue                  ; branch if not using random Y position
    lda RANDOM_NUM                 ; using random y location, load random number
    lsr
    lsr
    lsr
    lsr
    and #$03                       ; get random number between #$00 and #$03 inclusively

@continue:
    sta $13                         ; some number between #$00 and #$03 inclusively
    tay
    lsr
    lda enemy_gen_y_pos_start_tbl,y ; assume using start position from table
    bcc @adc_scroll                 ; branch if bit 0 of $13 is clear
    lda PLAYER_SPRITE_Y_POS         ; bit 0 of $13 was set, start looking from player Y position

@adc_scroll:
    and #$f0
    tay
    lda Y_SCROLL ; load PPU vertical scroll
    and #$0f
    sta $14      ; set $14 to low nibble of Y_SCROLL
    tya          ; restore a to high nibble of y starting position
    sec
    sbc $14      ; subtract number of pixels scrolled vertically
    clc
    adc #$08     ; add #$08 to the result
    sta $10      ; set start Y position for finding place for enemy
    lda #$0f
    sta $12      ; loop #$0f times

@loop:
    ldy $10                   ; load Y position
    lda $11                   ; load X position
    jsr side_view_find_ground ; check to see if there is space above point (a,y) for enemy
    bcs enemy_gen_exit_set    ; exit with carry set if found ground at ($11, $10) position

@next:
    lda $13
    lsr
    lsr
    lda #$10   ; searching downward by #$10 pixels
    bcc @adj_y
    lda #$ef   ; searching upward by #$10 pixels

@adj_y:
    adc $10   ; add or subtract #$10 from Y position
    sta $10   ; set Y position
    cmp #$f0  ; compare to bottom of screen
    bcs @next ; branch to loop around if past bottom of screen
    dec $12   ; decrement loop counter
    bne @loop ; branch if more locations to search

enemy_gen_exit_clear:
    clc ; exit indicating unable to find appropriate position

enemy_gen_exit_set:
    rts

; checks to see if there is #$20 of space above point (a,y)
; input
;  * a - X position
;  * y - Y position
;  * $10 - Y position
;  * $11 - X position
; output
;  * carry - clear when no ground position found, set when found
side_view_find_ground:
    jsr get_bg_collision_code ; determine background collision at point (a,y)
    beq enemy_gen_exit_clear  ; exit with carry clear if no bg collision at test point
    lda $10                   ; found sound to stand on, now need to look up for where that starts
                              ; load enemy Y position
    sec
    sbc #$10                  ; move up #$10 pixels
    tay                       ; transfer to y for is_on_ground_water
    lda $11                   ; load enemy X position for is_on_ground_water
    jsr is_on_ground_water    ; see if (a,y) is on ground, water, or incline
    bcs enemy_gen_exit_clear  ; branch if on ground marking no appropriate location found
    lda $10                   ; continue checking if appropriate location
                              ; move up #$10 more pixels and left/right #$10 pixels
                              ; load enemy Y position
    sec
    sbc #$10                  ; move up #$10 more pixels
    tay                       ; transfer to y for is_on_ground_water
    lda $11                   ; load enemy X position for is_on_ground_water
    asl
    lda #$10                  ; assume moving right
    bcc @continue
    lda #$ef                  ; enemy X position on right side of screen, move left #$10 (carry is set)

@continue:
    adc $11                  ; add or subtract #$10 from player X position
    jsr is_on_ground_water   ; see if (a,y) is on ground, water, or incline
    bcs enemy_gen_exit_clear ; branch if on ground marking no appropriate position found
    sec                      ; found empty collision code above bg collision, mark appropriate location found
    rts

; either left edge or right edge of screen
enemy_gen_x_pos_tbl:
    .byte $f8,$08,$f8,$f8,$08,$f8,$08,$f8,$08,$f8,$08,$f8,$f8,$08,$08,$f8

; generated enemy Y position y starting points, will look for ground below
; negative numbers use player Y position as starting point
enemy_gen_y_pos_start_tbl:
    .byte $10,$80,$e0,$80

; bit 1 specifies whether to search downward or upwards (always 0)
; bit 0 specifies whether to use random control value, i.e. whether or not to
; to search upward or downward
enemy_gen_y_pos_ctrl_tbl:
    .byte $00 ; level 1
    .byte $01 ; level 2
    .byte $00 ; level 3
    .byte $01 ; level 4
    .byte $01 ; level 5
    .byte $01 ; level 6
    .byte $01 ; level 7
    .byte $01 ; level 8

; finds an enemy spawn position for overhead level
; input
;  * $0f - enemy_gen_lvl_0x offset
;  * ($08) - enemy_gen_lvl_0x
; output
;  * carry - clear when unable to find spawn position, set when found
overhead_find_gen_enemy_pos:
    ldy $0f           ; load enemy_gen_lvl_0x offset
    lda ($08),y       ; load enemy generation byte
    ldy #$10
    and #$20          ; strip enemy generation byte to just bit 5
    beq @continue
    lda RANDOM_NUM    ; bit 5 is set, load random number
    adc FRAME_COUNTER
    lsr
    lsr
    bcs @continue
    ldy #$e0

@continue:
    sty $10                        ; set Y position to test (either #$10, or #$e0)
    lda RANDOM_NUM                 ; load random number
    and #$07
    tay
    lda oh_enemy_spawn_x_pos_tbl,y ; load random X spawn position
    sta $11                        ; set X position to test
    sta $12                        ; set X position for
    lda RANDOM_NUM                 ; load random number
    lsr
    and #$01
    sta $13                        ; random number 0 or 1
                                   ; test to the left or right of current position

@find_no_bg_collision_loop:
    ldy $10              ; load Y position to test
    lda $11              ; load X position to test
    jsr get_bg_collision ; get background collision code for position (a,y)
    beq @check_top       ; branch if no collision
    lda $13              ; collision at random spawn location, add/subtract #$10 randomly
    lsr                  ; choosing X to be either #$10 or #$ef randomly
    lda #$10
    bcc @adjust_x_pos    ; branch randomly to
    lda #$ef

@adjust_x_pos:
    adc $11                        ; add/subtract #$10 to initial X position
    sta $11                        ; set new test X position
    cmp $12
    bne @find_no_bg_collision_loop ; branch if not at initial X position
    clc                            ; ended up
    rts

@check_top:
    lda $10         ; load Y position
    cmp #$10
    bne @set_y_exit
    lda #$08        ; at top of screen, set to #$08 instead of #$10

@set_y_exit:
    sta $10 ; set Y position
    sec
    rts

oh_enemy_spawn_x_pos_tbl:
    .byte $28,$48,$68,$88,$a8,$c8,$e8,$78

; offset by level, value is stored in ENEMY_GEN_DELAY
level_enemy_gen_delay_tbl:
    .byte $7b,$53,$53,$53,$53,$53,$53,$53

; 2 per level
enemy_gen_lvl_ptr_tbl:
    .addr enemy_gen_lvl_01
    .addr enemy_gen_lvl_01
    .addr enemy_gen_lvl_02
    .addr enemy_gen_lvl_02
    .addr enemy_gen_lvl_03
    .addr enemy_gen_lvl_03
    .addr enemy_gen_lvl_04
    .addr enemy_gen_lvl_04
    .addr enemy_gen_lvl_05
    .addr enemy_gen_lvl_05
    .addr enemy_gen_lvl_06
    .addr enemy_gen_lvl_06
    .addr enemy_gen_lvl_07
    .addr enemy_gen_lvl_07
    .addr enemy_gen_lvl_08
    .addr enemy_gen_lvl_08

; when bit 7 set 50/50 generate soldier(s) or ladybug(s)
; when bit 7 clear and bit 6 clear, always generate
; when bit 7 clear and bit 6 set, 75% chance create enemy
; low nibble - row within gen_soldier_enemy_attr_tbl to use
; #$ff means do not generate enemies for screen
enemy_gen_lvl_01:
    .byte $ff,$80,$80,$80,$00,$00,$80,$80,$00,$00,$00,$80,$80,$ff

enemy_gen_lvl_02:
    .byte $ff,$ff,$ff,$22,$82,$81,$22,$02,$a1,$81,$02,$80,$80,$ff

enemy_gen_lvl_03:
    .byte $80,$00,$80,$81,$82,$40,$80,$80,$80,$80,$80,$80,$80,$40,$ff,$40
    .byte $00,$81,$01,$ff,$ff,$ff

enemy_gen_lvl_04:
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

enemy_gen_lvl_05:
    .byte $00,$00,$40,$00,$00,$00,$00,$ff

enemy_gen_lvl_06:
    .byte $ff,$ff,$02,$21,$ff,$a0,$22,$81,$02,$83,$02,$01,$00,$ff,$ff,$ff

enemy_gen_lvl_07:
    .byte $ff,$00,$80,$00,$00,$00,$80,$ff,$ff,$ff,$ff,$ff,$ff

enemy_gen_lvl_08:
    .byte $ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff

gen_soldier_enemy_attr_tbl:
    .byte $00,$00,$00,$00
    .byte $00,$00,$00,$08
    .byte $00,$00,$08,$08
    .byte $00,$08,$08,$08
    .byte $08,$08,$08,$08
    .byte $00,$00,$00,$10
    .byte $00,$00,$08,$10

gen_overhead_enemy_attr_tbl:
    .byte $00,$00,$00,$00
    .byte $00,$00,$00,$02
    .byte $00,$00,$02,$02
    .byte $00,$02,$02,$02
    .byte $02,$02,$02,$02

; write pattern tile (text) or palette information (color) to CPU offset CPU_GRAPHICS_BUFFER
; this is used when GRAPHICS_BUFFER_MODE is #$00, which defines the CPU_GRAPHICS_BUFFER format for text and palette data
; input
;  * a - first six bits are index into the short_text_pointer_table
;        when bit 7 set, write all blank characters instead of actual characters
;        (used for flashing effect)
write_text_to_mem:
    tay                                ; backup index into short_text_pointer_table
    lda #$02                           ; a = #$02
    sta $03                            ; used when bit 7 of a is set, indicating to clear text
                                       ; since the first #$02 bytes are PPU address,
                                       ; $03 is used to prevent overwriting PPU address with #$00
    lda #$01                           ; a = #$01 (next #$02 bytes are PPU address)
    jsr write_a_to_cpu_graphics_buffer ; write #$01 to CPU_GRAPHICS_BUFFER,x
    tya                                ; restore the index into short_text_pointer_table to print
    sta $02                            ; store index into short_text_pointer_table $02
    asl                                ; double index, since short_text_pointer_table is 2 bytes per label address
    tay                                ; transfer index to y register
    lda short_text_pointer_table,y     ; read the low-byte memory address from pointer table
    sta $00                            ; store low byte of short_text_pointer_table address
    lda short_text_pointer_table+1,y   ; high byte of pointer table
    sta $01                            ; store high byte of short_text_pointer_table address
    ldx GRAPHICS_BUFFER_OFFSET         ; set X to store the next offset of CPU_GRAPHICS_BUFFER
    ldy #$00                           ; initialize character offset into string to 0

; read until #fe, #$fd, or #$ff and store CPU memory starting at CPU_GRAPHICS_BUFFER
@write_char_to_cpu_mem:
    lda ($00),y                 ; read character from string
    iny                         ; increment character offset
    cmp #$ff                    ; #$ff signifies end of string, the #$ff isn't stored in CPU memory
    beq set_x_to_offset_exit    ; if #ff, the string has been completely loaded, restore x to GRAPHICS_BUFFER_OFFSET and exit
    cmp #$fe                    ; like #$ff, #$fe is end of string, but #fe causes #$ff to be stored in CPU memory at end of string
    beq @write_ff_to_cpu_memory ; store #$ff in CPU memory at the end of the string
    cmp #$fd                    ; #fd specifies next two bytes are the PPU address, i.e. changing location on screen
    beq @handle_fd              ; branch if next two bytes are a new PPU address
    sta CPU_GRAPHICS_BUFFER,x   ; store character in CPU graphics buffer
    lda $02                     ; load the index into text string table into A, i.e. which string to print
    bpl @next_char              ; branch if text ins't being blanked (part of flashing animation)
    lda $03                     ; not writing characters, writing blanks to hide text
                                ; see if already written PPU address (#$02 bytes)
    bne @dec_blank_delay        ; branch if writing PPU address to CPU graphics buffer
    sta CPU_GRAPHICS_BUFFER,x   ; write #$00 character to cpu memory to blank the text for flashing animation
    beq @next_char              ; continue to next character to read

@dec_blank_delay:
    dec $03 ; decrement delay to allow writing PPU address before writing all #$00s for text

@next_char:
    inx                        ; move to next cpu graphics buffer write offset
    bne @write_char_to_cpu_mem ; loop to read next character

; #$fe encountered (end of string)
@handle_fd:
    jsr @write_ff_to_cpu_memory
    lda #$02                                ; set to #$02 to skip blanking (zeroing) the PPU address when writing to CPU memory
    sta $03                                 ; reset delay before blanking text (flashing animation)
    lda #$01                                ; vram_address_increment offset (#$01 = write across)
    jsr write_a_to_graphics_buffer_adv_exit
    bne @write_char_to_cpu_mem

@write_ff_to_cpu_memory:
    lda #$ff                                ; set the zero flag so next line jumps and #$ff is stored in CPU memory
    bne write_a_to_graphics_buffer_adv_exit ; always branch to write #$ff to CPU graphics buffer and exit
    lda #$ff                                ; unreachable code !(UNUSED)

; input
;  * a - cpu graphics buffer byte to store
write_a_to_cpu_graphics_buffer:
    ldx GRAPHICS_BUFFER_OFFSET

write_a_to_graphics_buffer_adv_exit:
    sta CPU_GRAPHICS_BUFFER,x
    inx

set_x_to_offset_exit:
    stx GRAPHICS_BUFFER_OFFSET
    rts

; pointer table for text strings
short_text_pointer_table:
    .addr text_1_player     ; 1 PLAYER
    .addr text_2_players    ; 2 PLAYERS
    .addr text_area         ; AREA
    .addr text_1p_score     ; 1P SCORE
    .addr text_2p_score     ; 2P SCORE
    .addr text_hi_score     ; HI SCORE
    .addr text_rest         ; REST
    .addr text_rest2        ; REST
    .addr text_1_player     ; 1 PLAYER
    .addr text_game_over    ; GAME OVER
    .addr text_continue_end ; CONTINUE END
    .addr text_1_player     ; 1 PLAYER
    .addr text_area_1       ; AREA 1

; first two bytes are PPU address, followed by text
; $fe is the end of text string
; the two bytes after $fd specify the PPU address
; 1 PLAYER
text_1_player:
    .byte $22,$a6
    .byte $02,$00,$1a,$16,$0b,$23,$0f,$1c,$fe

; 2 PLAYERS
text_2_players:
    .byte $22,$b2
    .byte $03,$00,$1a,$16,$0b,$23,$0f,$1c,$1d,$fe

; 1P SCORE
text_1p_score:
    .byte $21,$03
    .byte $02,$1a,$00,$1d,$0d,$19,$1c,$0f
    .byte $fd
    .byte $21,$44
    .byte $00,$00,$00,$00,$00,$00,$01,$fe

; 2P SCORE
text_2p_score:
    .byte $21,$15,$03,$1a,$00,$1d,$0d,$19,$1c,$0f
    .byte $fd
    .byte $21,$56
    .byte $00,$00,$00,$00,$00,$00,$01,$fe

; HI SCORE
text_hi_score:
    .byte $20,$8c
    .byte $12,$13,$00,$1d,$0d,$19,$1c,$0f,$fd
    .byte $20,$cd
    .byte $00,$00,$00,$00,$00,$00,$01,$fe

; REST
text_rest:
    .byte $21,$84
    .byte $1c,$0f,$1d,$1e,$00,$00,$00,$fe

; REST
text_rest2:
    .byte $21,$96
    .byte $1c,$0f,$1d,$1e,$00,$00,$00,$fe

; AREA
text_area:
    .byte $22,$4d
    .byte $0b,$1c,$0f,$0b,$00,$00,$fe

; GAME OVER
text_game_over:
    .byte $22,$4c
    .byte $11,$0b,$17,$0f,$00,$19,$20,$0f,$1c,$fe

; CONTINUE END
text_continue_end:
    .byte $22,$4c
    .byte $0d,$19,$18,$1e,$13,$18,$1f,$0f,$00
    .byte $fd
    .byte $22,$ac
    .byte $0f,$18,$0e,$fe

; AREA 1
text_area_1:
    .byte $22,$4d
    .byte $0b,$1c,$0f,$0b,$00,$02,$fe

; write graphics data from graphic_data_ptr_tbl directly to PPU without using
; CPU graphics buffer.
; input
;  * x - graphic_data_ptr_tbl offset (already multiplied by 2, e.g. 2 loads graphic_data_01)
write_graphic_data_to_ppu:
    lda #$04
    sta PPU_READY
    lda graphic_data_ptr_tbl,x   ; load graphic data address low byte
    sta $00
    lda graphic_data_ptr_tbl+1,x ; load graphic data address high byte
    sta $01
    jsr disable_nmi_set_ppumask  ; disables nmi at vertical blanking start, sets PPUMASK and sets PPUADDR to $0000
    lda #$00
    sta X_SCROLL                 ; set PPU horizontal scroll to no scroll
    sta Y_SCROLL                 ; set PPU vertical scroll to top of nametables (no scroll)

; reads 2-bytes of memory starting at address $00,$01, which is a specific graphic_data
; and sets the PPUADDR to that address (PPU write address)
; then begins decompressing the graphic data and starts writing to PPU
begin_ppu_graphics_block_write:
    lda PPUSTATUS    ; reset PPU latch to prep for writing
    ldy #$01
    lda ($00),y      ; read PPU write high byte
    sta PPUADDR      ; set PPU write high byte
    dey              ; move to low byte
    lda ($00),y      ; read PPU write low byte
    sta PPUADDR      ; set PPU write low byte
    ldx #$00
    lda #$02
    jsr two_byte_add ; update read offset to skip past PPU address

; reads the next graphics byte compression sequence and writes it to the PPU
; multiple times depending on the number of repetitions specified
write_graphic_data_sequences_to_ppu:
    ldy #$00                       ; set offset so next line reads number of repetitions
    lda ($00),y                    ; read the byte of the graphic data
    cmp #$ff                       ; see if we are at the end of the graphic data
    beq end_graphic_code           ; loaded entire graphics data, restore previously loaded bank, re-init PPU
    cmp #$7f                       ; used to specify the PPU write address should change to the address specified in the next 2 bytes)
    beq change_ppu_write_address
    tay                            ; store command code byte in y
    bpl write_graphic_byte_a_times ; branch if byte is < #$7f to write the next byte multiple times (RLE-command)
    and #$7f                       ; byte has bit 7 set (negative), code is writing a string of bytes
                                   ; clear bit 7 to get number of bytes to write from compressed data
    sta $02                        ; store positive portion in $02, this is the number of bytes to write to PPU
    ldy #$01                       ; skip past size byte, prepare to read next n graphic bytes

; writes the next n bytes of the compressed graphic data to the PPU, starting at offset Y
; input
;  * $02 - the number of bytes to write (n)
;  * y - the graphic data read offset
write_next_n_sequence_bytes:
    lda ($00),y                           ; read graphic byte
    sta PPUDATA                           ; write graphic byte to PPU
    cpy $02                               ; see if written all n repetitions
    beq advance_graphic_read_addr_n_bytes ; written all n bytes, update base graphic read address
    iny                                   ; have not written $02 times, write next byte
    bne write_next_n_sequence_bytes       ; loop $02 times

; advances the address of the current graphic byte offset by n bytes
; where n is the value in $02 + #$1
; the #$01 is necessary to skip passed the command byte
advance_graphic_read_addr_n_bytes:
    lda #$01 ; advancing graphic byte read address by 1 (size of repetition string)
    clc      ; clear carry in preparation for addition
    adc $02  ; skip over the bytes just written the PPU
             ; a now has the number of bytes to skip

advance_ppu_write_addr:
    jsr two_byte_add                        ; update read offset to skip past PPU address
    jmp write_graphic_data_sequences_to_ppu

; write the next graphic byte to the PPU A times ($02)
write_graphic_byte_a_times:
    ldy #$01    ; offset to read graphic data byte
    sta $02     ; store number of repetitions of graphic byte to $02
    lda ($00),y ; load the graphic byte into a
    ldy $02     ; load the number of repetitions into y

; writes the value of a to PPU repeatedly y times
write_a_to_ppu_y_times:
    sta PPUDATA                ; write PPU value address to PPU
    dey                        ; decrement counter
    bne write_a_to_ppu_y_times ; RLE loop Y times
    lda #$02                   ; prepare to read and write next graphic data byte
    bne advance_ppu_write_addr ; continue updating PPU with data

; changes the PPU address where the graphic data bytes are written to
change_ppu_write_address:
    lda #$01                           ; specifies to increment graphic read address by 1 byte
    jsr two_byte_add                   ; increment 2-byte graphic read address at $00 by 1 byte
    jmp begin_ppu_graphics_block_write ; start writing graphics block to PPU

end_graphic_code:
    jmp enable_nmi_for_vblank

graphic_data_ptr_tbl:
    .addr blank_nametables ; blank out entire nametable
    .addr graphic_data_01  ; intro screen nametable tiles
    .addr graphic_data_02

; PPU address $2000 (nametable)
; clears nametable $2000 and $2c00 (with mirroring, this clears all nametables)
graphic_data_00:
blank_nametables:
    .byte $00,$20
    .byte $78,$00,$78,$00,$78,$00,$78,$00,$78,$00,$78,$00,$78,$00,$78,$00
    .byte $40,$00,$7f
    .byte $00,$2c
    .byte $78,$00,$78,$00,$78,$00,$78,$00,$78,$00,$78,$00,$78,$00,$78,$00
    .byte $40,$00
    .byte $ff

; intro screen nametable tiles
graphic_data_01:
    .ifdef Probotector
        .byte $00,$20,$6a,$00,$8c,$cb,$cc,$cd,$ce,$cf,$f6,$f7,$f8,$cf,$f9,$fa
        .byte $fb,$13,$00,$8e,$30,$db,$dc,$dd,$de,$df,$eb,$ec,$ed,$df,$ee,$ef
        .byte $84,$79,$3e,$00,$82,$fd,$fe,$11,$00,$8f,$59,$5a,$96,$97,$98,$99
        .byte $9a,$9b,$9c,$fc,$c6,$c7,$c8,$c9,$c3,$11,$00,$8f,$a4,$a5,$a6,$a7
        .byte $a8,$a9,$aa,$ab,$ac,$ad,$d6,$d7,$d8,$d9,$da,$11,$00,$91,$b4,$b5
        .byte $b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$e6,$e7,$e8,$e9,$ea,$00,$a3,$0e
        .byte $00,$93,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e
        .byte $3f,$a0,$a1,$a2,$c4,$0c,$00,$94,$40,$41,$42,$43,$44,$45,$46,$47
        .byte $48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$b0,$b1,$b2,$b3,$0d,$00,$95,$51
        .byte $52,$53,$54,$55,$46,$57,$58,$52,$53,$5b,$5c,$5d,$5e,$5f,$c0,$c1
        .byte $c2,$50,$c4,$b3,$07,$00,$99,$9d,$9e,$9f,$60,$61,$62,$63,$64,$65
        .byte $66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$d0,$d1,$d2,$d3,$d4,$d5
        .byte $08,$00,$98,$ae,$af,$70,$71,$72,$73,$74,$75,$76,$77,$78,$69,$7a
        .byte $7b,$7c,$7d,$7e,$7f,$e0,$e1,$e2,$e3,$e4,$e5,$08,$00,$9a,$be,$bf
        .byte $80,$81,$82,$83,$74,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        .byte $f0,$f1,$f2,$f3,$f4,$f5,$2e,$2f,$08,$00,$88,$90,$91,$92,$93,$94
        .byte $95,$b2,$b3,$19,$00,$82,$56,$b2,$41,$00,$8b,$1a,$16,$0b,$23,$00
        .byte $1d,$0f,$16,$0f,$0d,$1e,$31,$00,$88,$02,$00,$1a,$16,$0b,$23,$0f
        .byte $1c,$04,$00,$89,$03,$00,$1a,$16,$0b,$23,$0f,$1c,$1d,$4e,$00,$8d
        .byte $1e,$17,$00,$0b,$18,$0e,$00,$27,$00,$02,$0a,$0a,$03,$12,$00,$8f
        .byte $15,$19,$18,$0b,$17,$13,$00,$0d,$19,$26,$ca,$16,$1e,$0e,$26,$0f
        .byte $00,$95,$16,$13,$0d,$0f,$18,$1d,$0f,$0e,$00,$0c,$23,$00,$18,$13
        .byte $18,$1e,$0f,$18,$0e,$19,$26,$6f,$00,$04,$50,$04,$00,$04,$05,$2a
        .byte $00,$ff
    .else
        .byte $00,$20,$6b,$00,$8b,$2e,$2f,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d
        .byte $14,$00,$8c,$30,$31,$32,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$14
        .byte $00,$82,$33,$34,$27,$00,$88,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$17
        .byte $00,$8a,$4f,$50,$51,$52,$53,$54,$55,$55,$56,$57,$15,$00,$83,$58
        .byte $59,$5a,$04,$00,$84,$5b,$5c,$5d,$5e,$14,$00,$84,$5f,$60,$61,$62
        .byte $06,$00,$81,$72,$0b,$00,$93,$73,$74,$75,$76,$77,$78,$79,$7a,$7b
        .byte $7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$0d,$00,$93,$86,$87,$88
        .byte $89,$8a,$8b,$8c,$8d,$8e,$8f,$00,$90,$91,$92,$93,$94,$95,$96,$97
        .byte $0d,$00,$93,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4
        .byte $a5,$a6,$a7,$a8,$a9,$aa,$0d,$00,$93,$ab,$ac,$ad,$9f,$ae,$af,$b0
        .byte $9f,$b1,$b2,$00,$b3,$00,$b4,$b5,$b6,$b7,$b8,$97,$0d,$00,$97,$b9
        .byte $ba,$bb,$bc,$bd,$be,$bb,$bf,$c0,$c1,$00,$c2,$c3,$c4,$c5,$c6,$c7
        .byte $c8,$c9,$00,$63,$64,$65,$1d,$00,$82,$66,$67,$16,$00,$8b,$68,$69
        .byte $6a,$6b,$6c,$6d,$6e,$6f,$00,$70,$71,$19,$00,$81,$72,$33,$00,$8b
        .byte $1a,$16,$0b,$23,$00,$1d,$0f,$16,$0f,$0d,$1e,$31,$00,$88,$02,$00
        .byte $1a,$16,$0b,$23,$0f,$1c,$04,$00,$89,$03,$00,$1a,$16,$0b,$23,$0f
        .byte $1c,$1d,$2e,$00,$8d,$1e,$17,$00,$0b,$18,$0e,$00,$27,$00,$02,$0a
        .byte $0a,$01,$0e,$00,$98,$15,$19,$18,$0b,$17,$13,$00,$13,$18,$0e,$1f
        .byte $1d,$1e,$1c,$23,$00,$0d,$19,$26,$ca,$16,$1e,$0e,$26,$0e,$00,$8b
        .byte $16,$13,$0d,$0f,$18,$1d,$0f,$0e,$00,$0c,$23,$0f,$00,$98,$18,$13
        .byte $18,$1e,$0f,$18,$0e,$19,$00,$19,$10,$00,$0b,$17,$0f,$1c,$13,$0d
        .byte $0b,$00,$13,$18,$0d,$26,$70,$00,$03,$f0,$8b,$00,$00,$40,$50,$50
        .byte $5f,$5f,$1f,$00,$00,$44,$04,$55,$83,$d1,$30,$00,$03,$a0,$85,$ac
        .byte $af,$af,$03,$00,$06,$aa,$82,$00,$00,$06,$aa,$09,$00,$ff
    .endif

; end of game credits mountains and clouds nametable data
graphic_data_02:
    .byte $00,$20,$61,$8b,$90,$9d,$8b,$8b,$9e,$9f,$a0,$9e,$a0,$8b,$a1,$a2
    .byte $a2,$a3,$a4,$a5,$a6,$03,$8b,$88,$9d,$8b,$8b,$8c,$8b,$9f,$9e,$a0
    .byte $05,$8b,$88,$a1,$a2,$a3,$a4,$a5,$a4,$a5,$a6,$04,$8b,$94,$9d,$8b
    .byte $8b,$9e,$9e,$a0,$8b,$a1,$a7,$a5,$a2,$a3,$a4,$a5,$a6,$a1,$a5,$8b
    .byte $8b,$a0,$03,$8b,$83,$9d,$8b,$8b,$03,$9e,$89,$a0,$8b,$8b,$a1,$a2
    .byte $a3,$a4,$a5,$a6,$04,$8b,$83,$9e,$9e,$a0,$06,$8b,$81,$a6,$03,$8b
    .byte $87,$a1,$a7,$a5,$a2,$a3,$a5,$a6,$07,$8b,$8e,$a1,$a2,$a5,$a2,$a5
    .byte $a5,$a6,$a6,$8b,$a1,$a2,$a2,$a3,$a5,$0d,$8b,$86,$9d,$8b,$8c,$8b
    .byte $9e,$a0,$06,$8b,$82,$9e,$a0,$07,$8b,$84,$9d,$8b,$8c,$a0,$05,$8b
    .byte $88,$9e,$8b,$a1,$a2,$a2,$a3,$a5,$a6,$03,$8b,$88,$a1,$a2,$a5,$a5
    .byte $a6,$8b,$9e,$a0,$04,$8b,$8b,$a1,$a2,$a5,$a6,$8b,$8b,$a1,$a2,$a3
    .byte $a5,$a5,$0d,$8b,$86,$a1,$a2,$a5,$a6,$8b,$8b,$20,$8d,$05,$8e,$82
    .byte $8f,$90,$04,$8e,$82,$8f,$90,$13,$8e,$91,$91,$92,$93,$92,$94,$8b
    .byte $95,$93,$92,$96,$94,$8b,$95,$93,$92,$96,$91,$06,$97,$82,$92,$91
    .byte $06,$97,$a0,$92,$00,$00,$a8,$a9,$00,$8b,$00,$a8,$a9,$00,$00,$8b
    .byte $00,$a8,$a9,$00,$95,$98,$99,$9a,$99,$9b,$9c,$8b,$95,$98,$99,$9a
    .byte $99,$9b,$9c,$03,$8b,$82,$00,$00,$03,$8b,$82,$00,$00,$04,$8b,$87
    .byte $00,$00,$8b,$00,$8b,$8b,$00,$05,$8b,$82,$a8,$a9,$16,$8b,$82,$00
    .byte $00,$7e,$8b,$7e,$8b,$7e,$8b,$53,$8b,$04,$fa,$8c,$ba,$fa,$fa,$ba
    .byte $ef,$ff,$ff,$aa,$ea,$ff,$bf,$ab,$08,$5a,$08,$05,$20,$00,$ff

init_level_vars:
    lda #$00
    .ifdef Superc
        sta SUPERTILE_ATTRIBUTE_OVERRIDE ; clear door destroyed flag
        ldx #$09

    @loop:
        sta ATTRIBUTE_OVERRIDES,x          ; clear any supertile attribute overrides
        dex
        bpl @loop
    .endif
    sta NT_MIRRORING                       ; set nametable mirroring (0: vertical; 1: horizontal)
    sta X_SCROLL                           ; initialize PPU horizontal scroll
    sta BACKUP_X_SCROLL
    .ifdef Probotector
        lda #$ff
    .endif
    sta Y_SCROLL                           ; initialize PPU vertical scroll
    sta BACKUP_Y_SCROLL
    lda PPUCTRL_SETTINGS
    and #$fc
    sta PPUCTRL_SETTINGS
    sta BACKUP_PPUCTRL_SETTINGS
    lda #$ff
    sta Y_AUTOSCROLL_STOP_SCREEN
    ldy CURRENT_LEVEL                      ; load current level
    lda #$00
    sta X_SCREEN                           ; set leftmost screen
    lda level_y_screen_start_tbl,y
    sta LEVEL_Y_SCREEN                     ; set Y screen start
    sta Y_SCREEN
    lda level_screen_scroll_tbl,y          ; load screen scroll type (0 = horizontal, 1 = vertical/overhead)
    sta SCREEN_SCROLL_TYPE
    lda level_y_scroll_flags_tbl,y         ; load initial level allowable scroll directions
    sta LEVEL_Y_SCROLL_FLAGS
    sta Y_SCROLL_FLAGS
    lda level_incline_bg_collision_tbl,y   ; 1 = bg collision with an incline, 0 = no collision
    sta INCLINE_BG_COLLISION_FLAG
    lda level_overhead_flag_tbl,y
    sta OVERHEAD_FLAG                      ; 0 = side view, 1 = overhead view
    lda level_y_center_tbl,y               ; load Y point at level in which to cause scroll
    sta LEVEL_Y_CENTER
    lda level_enemy_gen_ctrl_tbl,y
    sta ENEMY_GEN_CTRL
    lda level_palette_cycle_enable_tbl,y
    sta PALETTE_CYCLE_INDEX                ; set whether or not palette cycling is enabled at level start
    lda level_palette_cycle_2_enable_tbl,y ; enabled for every level, only used in level 5 and level 6
    sta PALETTE_CYCLE_INDEX_2              ; set whether or not 2nd palette cycling is enabled at level start
    rts

level_y_screen_start_tbl:
    .byte $02,$0c,$00,$0c,$06,$0c,$00,$03

; SCREEN_SCROLL_TYPE
level_screen_scroll_tbl:
    .byte $00,$01,$00,$00,$01,$01,$01,$00

; LEVEL_Y_SCROLL_FLAGS
level_y_scroll_flags_tbl:
    .byte $80,$40,$c0,$c0,$40,$40,$80,$c0

; whether or not a background collision with an incline is counted as a
; collision. Set to #$00 on Level 5 (The Cliff) and Level 8 (The Final Stage).
; When disabled, bullets pass through inclines and players can jump up through
; inclines.
level_incline_bg_collision_tbl:
    .byte $01,$01,$01,$01,$00,$01,$01,$00

; level 2, and level 6 are overhead levels
level_overhead_flag_tbl:
    .byte $00,$01,$00,$00,$00,$01,$00,$00

; Y point at level in which to cause scroll
level_y_center_tbl:
    .byte $80,$d0,$a0,$a0,$80,$d0,$80,$a0

; see ENEMY_GEN_CTRL
level_enemy_gen_ctrl_tbl:
    .byte $00,$81,$00,$00,$01,$83,$01,$00

; #$ff palette cycling disabled at level start, #$00 enabled at level start
level_palette_cycle_enable_tbl:
    .byte $ff,$ff,$ff,$ff,$00,$ff,$ff,$ff

; #$ff palette cycling disabled at level start, #$00 enabled at level start
level_palette_cycle_2_enable_tbl:
    .byte $00,$00,$00,$00,$00,$00,$00,$00

set_level_scroll_screen:
    jsr handle_y_scroll
    ldx SCREEN_SCROLL_TYPE ; 0 = horizontal, 1 = vertical/overhead
    beq @apply_x_scroll    ; branch if horizontal scroll
    dex
    beq @exit              ; exit if vertical/overhead
    bne @continue          ; always branch, some boss screen

@apply_x_scroll:
    ldx STOMP_CEILING_X_SCROLL_CTRL
    bmi @continue                   ; branch to skip X scroll if in part of stomping ceiling
                                    ; used to ensure ceiling bg tiles are correct
    lda X_SCROLL_SPEED              ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    beq @exit                       ; branch if not scrolling horizontally this frame
    clc                             ; has X scroll, apply X scroll
                                    ; clear carry in preparation for addition
    adc X_SCROLL                    ; add PPU horizontal scroll
    sta X_SCROLL                    ; set new PPU horizontal scroll value
    ldx STOMP_CEILING_X_SCROLL_CTRL
    bne @exit                       ; exit if in part of stomping ceiling that prevents nametable change
    bcc @continue                   ; branch if not overflow into next nametable
    lda PPUCTRL_SETTINGS
    eor #$01
    sta PPUCTRL_SETTINGS            ; switch nametable horizontally
    inc X_SCREEN                    ; move to next horizontal screen

@continue:
    ldy LEVEL_WIDTH                    ; load how many screen horizontally the level is
    dey
    sty $00                            ; store width of level layout minus 1
    lda X_SCREEN                       ; load which screen scrolled to
    cmp $00                            ; see if past last horizontal screen for the level
    bcs @set_y_scroll_type_exit        ; exit if past last horizontal screen
    cmp SCREENS_DRAWN
    beq @check_should_write_supertiles
    bcs @set_x_draw_routine
    lda SCREENS_DRAWN                  ; X_SCREEN < SCREENS_DRAWN
    bmi @set_x_draw_routine
    bcc @exit

@check_should_write_supertiles:
    lda X_SCROLL            ; load PPU horizontal scroll
    cmp X_SCROLL_DRAW_POINT ; compare to point in which should start drawing next column of supertiles
    bcc @exit               ; exit if shouldn't start writing next column of supertiles

@set_x_draw_routine:
    lda X_DRAW_ROUTINE
    bne @exit          ; exit if already drawing next column
    lda #$02           ; set to x_nt_draw_routine_02
    sta X_DRAW_ROUTINE ; which determines start location supertile column
                       ; then writes 1 or 2 supertiles to the CPU_GRAPHICS_BUFFER

@exit:
    rts

@set_y_scroll_type_exit:
    lda #$01
    sta SCREEN_SCROLL_TYPE ; set vertical/overhead scrolling

handle_y_scroll_exit:
    rts

handle_y_scroll:
    lda Y_SCROLL             ; load PPU vertical scroll
    and #$0f
    sta $00                  ; set low nibble of PPU vertical scroll (for use in determining nametable row changes)
    lda Y_SCROLL_SPEED       ; load number of vertical pixels to scroll vertically this frame
    beq handle_y_scroll_exit ; exit if no vertical scroll is occurring this frame
    bmi @set_y_scroll        ; branch if scrolling up
    clc                      ; scrolling down, clear carry in preparation for addition
    adc Y_SCROLL             ; add PPU vertical scroll to vertical frame scroll
    sta Y_SCROLL             ; set new PPU vertical scroll value
    bcs @advance_y_screen    ; branch if overflow when adding to PPU vertical scroll
    cmp #$f0
    bcc @calc_nt_row_scroll  ; branch if didn't scroll camera past bottom of nametables

; overflow, i.e. scrolled past the bottom of the nametables, resetting back to top
@advance_y_screen:
    clc          ; clear carry in preparation for addition
    adc #$10     ; move camera back to top of nametables
    sta Y_SCROLL ; set new PPU vertical scroll value
    inc Y_SCREEN ; move to next screen down vertically

@calc_nt_row_scroll:
    lda $00                    ; load low nibble of original PPU vertical scroll
    cmp #$07
    bcs @set_scroll_flags_exit ; branch if odd numbered nametable row (0-indexed)
    lda Y_SCROLL               ; top of camera is on even nametable row (0-indexed), load PPU vertical scroll
    and #$0f
    cmp #$07                   ; see if still on even nametable row
    bcc @set_scroll_flags_exit ; branch if still on even numbered row (0-indexed)
    ldy #$00                   ; moved top of camera from even nametable row to odd nametable row (0-indexed)
    lda Y_SCROLL_DIR           ; 0 = vertically scrolling up, 1 = vertically scrolling down
    beq @set_y_draw_routine_01 ; branch if scrolling up
    lda NT_ROW_SCROLL          ; scrolling down, incrementing number of nametable rows scrolled
    .ifdef Probotector
        clc
        adc #$02
    .else
        adc #$01
    .endif
    cmp #$1e                   ; see if scrolled past the last nametable row (wrapping around)
    bcc @check_bottom          ; branch if no need to update screen
    inc LEVEL_Y_SCREEN         ; move to next screen down vertically
    lda #$00                   ; set nametable row back to top of nametables

@check_bottom:
    sta NT_ROW_SCROLL          ; set number of nametables rows scrolled
    ldy #$00
    lda LEVEL_Y_SCREEN         ; load vertical screen index (similar to y axis on a 2-d cardinal plane)
    clc                        ; clear carry in preparation for addition
    adc #$02
    cmp LEVEL_HEIGHT           ; see if near the bottom of the level
    bne @set_y_draw_routine_01 ; branch if not 2 screens away from the bottom
    lda NT_ROW_SCROLL          ; 2 screens from bottom
    cmp #$1c
    bcc @set_y_draw_routine_01 ; branch if not about to wrap
    ldy #$40                   ; close to bottom of level and about to wrap bottom of nametable
                               ; !(HUH) never happens

@set_y_draw_routine_01:
    jsr @calc_scroll_flags_exit
    ldy #$01                         ; scrolling vertically down
    jmp set_y_scroll_draw_routine_01 ; set scrolling down and routine to y_scroll_draw_routine_01

@set_scroll_flags_exit:
    ldy #$00

@calc_scroll_flags_exit:
    tya
    ora Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    rts

@set_y_scroll:
    clc                      ; clear carry in preparation for addition
    adc Y_SCROLL             ; add PPU vertical scroll
    sta Y_SCROLL             ; set new PPU vertical scroll value
    bcc @dec_y_screen
    cmp #$f0
    bcc @calc_nt_row_scroll2

@dec_y_screen:
    sec          ; set carry flag in preparation for subtraction
    sbc #$10
    sta Y_SCROLL ; set new PPU vertical scroll
    dec Y_SCREEN

@calc_nt_row_scroll2:
    lda $00
    cmp #$07
    bcc @set_scroll_flags_exit
    lda Y_SCROLL                      ; load PPU vertical scroll
    and #$0f
    cmp #$07
    bcs @set_scroll_flags_exit
    ldy #$00
    lda Y_SCROLL_DIR                  ; 0 = vertically scrolling up, 1 = vertically scrolling down
    bne @set_y_scroll_draw_routine_01
    lda NT_ROW_SCROLL
    sec                               ; set carry flag in preparation for subtraction
    sbc #$02                          ; moved up two nametable rows
    bpl @set_row                      ; branch if no underflow
    lda #$1c                          ; underflow
    dec LEVEL_Y_SCREEN                ; update to indicate one screen higher

@set_row:
    sta NT_ROW_SCROLL                 ; set number of nametable rows scrolled
    ldy #$00
    lda LEVEL_Y_SCREEN
    ora NT_ROW_SCROLL
    bne @set_y_scroll_draw_routine_01
    ldy #$80                          ; at the top of the level

@set_y_scroll_draw_routine_01:
    jsr @calc_scroll_flags_exit
    ldy #$00                    ; scrolling up

; input
;  * y - scroll dir (0 = vertically scrolling up, 1 = vertically scrolling down)
set_y_scroll_draw_routine_01:
    sty Y_SCROLL_DIR   ; 0 = vertically scrolling up, 1 = vertically scrolling down
    lda #$01
    sta Y_DRAW_ROUTINE ; set to y_scroll_draw_routine_01
    rts

; simulates player input for demo levels for both players
; begins firing after #$e0 frames (see DEMO_FIRE_DELAY_TIMER)
run_demo_input:
    inc DEMO_FIRE_DELAY_TIMER ; starts at 0 increments to #$ff and stops
                              ; used by demo logic to wait #$e0 frames until begin firing
    bne @player_loop
    dec DEMO_FIRE_DELAY_TIMER ; wrapped around, pin to #$ff

@player_loop:
    ldx #$01                  ; start at player 2
    jsr set_player_demo_input ; set values specific for demo player input
    dex                       ; do the same for player

; simulates player input for demo levels, always fires after #$e0 frames
; input
;  * x - player number, starts at 1 and goes to 0
set_player_demo_input:
    lda FRAME_COUNTER           ; load frame counter
    lsr                         ; shift right, pushing lsb (0th bit) to carry flag
    bcs @write_input            ; if odd frame, keep same input as previous frame, don't decrement DEMO_INPUT_NUM_FRAMES
    lda DEMO_INPUT_NUM_FRAMES,x ; even frame, load the number of frames for the demo input table
    bne @dec_input_frame_count  ; if number of frames from previous input hasn't completed, then skip reading next input instruction
    lda CURRENT_LEVEL           ; load current level
    cmp #$03                    ; see if greater than or equal to #$03
    bcc @continue               ; branch to continue with current level if less than #$03
    lda #$00                    ; finished showing all demo levels restart at level 0

@continue:
    asl                         ; each entry in demo_input_ptr_tbl is 2 bytes, so double
    asl                         ; since each level has 2 entries (1 for each player), double again.
                                ; this determines the player 1 entry for the level
    sta $08                     ; store demo_input_ptr_tbl entry offset into $08
    txa                         ; move player number to a
    asl                         ; if player 1, nothing happens, but if player 2, then double offset since each entry is #$2 bytes
    adc $08                     ; add result to demo_input_pointer_table entry offset into $08 to get player-specific offset
    tay                         ; move result to offset register
    lda demo_input_ptr_tbl,y    ; read low byte of input pointer table
    sta $08                     ; store pointer address value in $08
    lda demo_input_ptr_tbl+1,y  ; load high byte of input pointer table (demo_input_pointer_table + 1)
    sta $09                     ; store pointer address value in $09
    ldy DEMO_INPUT_TBL_INDEX,x  ; the offset into demo_input_tbl_lX_pX of to read
    lda ($08),y                 ; load the 1-byte controller input from the 2-byte address
    cmp #$ff                    ; #$ff signals end of demo input
    beq @end_demo_level         ; set DEMO_LEVEL_END_FLAG to #$01 and exit if we've read $ff byte (end of code)
    sta DEMO_INPUT_VAL,x        ; store the controller input for the demo input table
    iny                         ; increment 1 to get the number of frames
    lda ($08),y                 ; load the 1-byte number of frames to use input from the 2-byte address
    sta DEMO_INPUT_NUM_FRAMES,x ; store the number of frames for the input
    iny                         ; increment table read offset
    sty DEMO_INPUT_TBL_INDEX,x  ; increment byte read offset into demo_input_lvl_xx_px

@dec_input_frame_count:
    dec DEMO_INPUT_NUM_FRAMES,x ; decrement number of frames to press input

@write_input:
    lda DEMO_INPUT_VAL,x        ; read controller input to execute
    sta CONTROLLER_STATE_DIFF,x ; store controller input as new input
    sta CONTROLLER_STATE,x      ; store controller input
    lda DEMO_FIRE_DELAY_TIMER   ; load delay timer before firing weapon
    cmp #$e0                    ; wait #$e0 frames before firing weapon
    bcc @exit                   ; move to next player if DEMO_FIRE_DELAY_TIMER < #$e0
    lda PLAYER_CURRENT_WEAPON,x ; get current player's weapon
    and #$0f                    ; strip out rapid fire flag
    cmp #$01                    ; see if current weapon is machine gun
    beq @m_or_laser             ; branch if M weapon
    cmp #$03                    ; see if laser
    bne @fire_weapon_input      ; branch if not laser

; hold down b button for m or laser weapon during demo
@m_or_laser:
    lda CONTROLLER_STATE,x ; load controller input
    ora #$40               ; set 6th bit to 1 (b button)
    sta CONTROLLER_STATE,x ; save toggled flag back to CONTROLLER_STATE
    bne @exit              ; go to next player (always jumps due to ora instruction)

; for non M, nor L weapon, press b button every #$07 frames
@fire_weapon_input:
    lda FRAME_COUNTER           ; load frame counter
    and #$06                    ; checking every 7th frame
    bne @exit                   ; move to next player without firing weapon
    lda CONTROLLER_STATE_DIFF,x ; load current controller input
    ora #$40                    ; press b button
    sta CONTROLLER_STATE_DIFF,x ; store input

@exit:
    rts

; finished reading all of demo data, end demo for level
@end_demo_level:
    inc DEMO_LEVEL_END_FLAG ; set demo end flag to #$01
    rts

demo_input_ptr_tbl:
    .addr demo_input_lvl_00_p1
    .addr demo_input_lvl_00_p2
    .addr demo_input_lvl_01_p1
    .addr demo_input_lvl_01_p2
    .addr demo_input_lvl_02_p1
    .addr demo_input_lvl_02_p2

; the following area contains the automated input for the demo levels
;  * first byte is input code (up, down, left, right, b, a, start, select)
;  * second byte is number of even-numbered frames to apply the input for
; player firing isn't specified in these input tables
; instead, it is handled automatically as part of running the demo
;  * m or l weapons are always firing, other weapons fire every #$07 frames
demo_input_lvl_00_p1:
    .byte $00,$6f ; no input for #6f even frames
    .byte $01,$33 ; right for #33 even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $08,$21 ; up for #21 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $01,$0f ; right for #0f even frames
    .byte $00,$13 ; no input for #13 even frames
    .byte $01,$15 ; right for #15 even frames
    .byte $02,$06 ; left for #06 even frames
    .byte $00,$07 ; no input for #07 even frames
    .byte $01,$1a ; right for #1a even frames
    .byte $09,$5a ; up right for #5a even frames
    .byte $08,$05 ; up for #05 even frames
    .byte $00,$06 ; no input for #06 even frames
    .byte $08,$10 ; up for #10 even frames
    .byte $00,$17 ; no input for #17 even frames
    .byte $01,$0a ; right for #0a even frames
    .byte $09,$19 ; up right for #19 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $0a,$0e ; up left for #0e even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $88,$0a ; up and a for #0a even frames
    .byte $89,$06 ; up right and a for #06 even frames
    .byte $88,$03 ; up and a for #03 even frames
    .byte $8a,$02 ; up left and a for #02 even frames
    .byte $0a,$01 ; up left for #01 even frame
    .byte $08,$05 ; up for #05 even frames
    .byte $09,$04 ; up right for #04 even frames
    .byte $89,$12 ; up right and a for #12 even frames
    .byte $09,$13 ; up right for #13 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $88,$04 ; up and a for #04 even frames
    .byte $89,$05 ; up right and a for #05 even frames
    .byte $09,$0e ; up right for #0e even frames
    .byte $01,$0e ; right for #0e even frames
    .byte $ff,$ff ; end input

demo_input_lvl_00_p2:
    .byte $00,$84 ; no input for #84 even frames
    .byte $01,$1e ; right for #1e even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $02,$07 ; left for #07 even frames
    .byte $00,$01 ; no input for #01 even frame
    .byte $01,$04 ; right for #04 even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $88,$05 ; up and a for #05 even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $09,$0b ; up right for #0b even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $0a,$07 ; up left for #07 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $00,$08 ; no input for #08 even frames
    .byte $01,$02 ; right for #02 even frames
    .byte $81,$05 ; right and a for #05 even frames
    .byte $01,$04 ; right for #04 even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$04 ; up for #04 even frames
    .byte $00,$0e ; no input for #0e even frames
    .byte $01,$07 ; right for #07 even frames
    .byte $02,$04 ; left for #04 even frames
    .byte $00,$1a ; no input for #1a even frames
    .byte $01,$05 ; right for #05 even frames
    .byte $81,$05 ; right and a for #05 even frames
    .byte $01,$47 ; right for #47 even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$09 ; up for #09 even frames
    .byte $09,$10 ; up right for #10 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $0a,$10 ; up left for #10 even frames
    .byte $8a,$05 ; up left and a for #05 even frames
    .byte $88,$02 ; up and a for #02 even frames
    .byte $09,$1d ; up right for #1d even frames
    .byte $01,$1a ; right for #1a even frames
    .byte $09,$07 ; up right for #07 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $0a,$0e ; up left for #0e even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $0a,$0e ; up left for #0e even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $88,$05 ; up and a for #05 even frames
    .byte $89,$01 ; up right and a for #01 even frame
    .byte $09,$1a ; up right for #1a even frames
    .byte $89,$06 ; up right and a for #06 even frames
    .byte $09,$06 ; up right for #06 even frames
    .byte $01,$20 ; right for #20 even frames
    .byte $ff,$ff ; end input

demo_input_lvl_01_p1:
    .byte $00,$38 ; no input for #38 even frames
    .byte $02,$04 ; left for #04 even frames
    .byte $0a,$1d ; up left for #1d even frames
    .byte $02,$08 ; left for #08 even frames
    .byte $0a,$01 ; up left for #01 even frame
    .byte $02,$01 ; left for #01 even frame
    .byte $00,$05 ; no input for #05 even frames
    .byte $01,$08 ; right for #08 even frames
    .byte $08,$25 ; up for #25 even frames
    .byte $01,$07 ; right for #07 even frames
    .byte $09,$02 ; up right for #02 even frames
    .byte $08,$0b ; up for #0b even frames
    .byte $0a,$01 ; up left for #01 even frame
    .byte $02,$02 ; left for #02 even frames
    .byte $0a,$07 ; up left for #07 even frames
    .byte $08,$0a ; up for #0a even frames
    .byte $00,$0c ; no input for #0c even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $00,$03 ; no input for #03 even frames
    .byte $08,$0c ; up for #0c even frames
    .byte $01,$06 ; right for #06 even frames
    .byte $08,$1a ; up for #1a even frames
    .byte $01,$0a ; right for #0a even frames
    .byte $05,$01 ; right down for #01 even frame
    .byte $04,$0b ; down for #0b even frames
    .byte $05,$03 ; right down for #03 even frames
    .byte $04,$05 ; down for #05 even frames
    .byte $05,$03 ; right down for #03 even frames
    .byte $01,$06 ; right for #06 even frames
    .byte $09,$07 ; up right for #07 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $04,$09 ; down for #09 even frames
    .byte $05,$06 ; right down for #06 even frames
    .byte $01,$0c ; right for #0c even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$0a ; up for #0a even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $01,$0a ; right for #0a even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$0c ; up for #0c even frames
    .byte $09,$07 ; up right for #07 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $09,$03 ; up right for #03 even frames
    .byte $08,$12 ; up for #12 even frames
    .byte $0a,$04 ; up left for #04 even frames
    .byte $08,$0a ; up for #0a even frames
    .byte $00,$06 ; no input for #06 even frames
    .byte $08,$11 ; up for #11 even frames
    .byte $0a,$14 ; up left for #14 even frames
    .byte $08,$12 ; up for #12 even frames
    .byte $0a,$11 ; up left for #11 even frames
    .byte $02,$04 ; left for #04 even frames
    .byte $0a,$01 ; up left for #01 even frame
    .byte $08,$05 ; up for #05 even frames
    .byte $0a,$0b ; up left for #0b even frames
    .byte $02,$05 ; left for #05 even frames
    .byte $06,$05 ; left down for #05 even frames
    .byte $02,$02 ; left for #02 even frames
    .byte $06,$02 ; left down for #02 even frames
    .byte $02,$07 ; left for #07 even frames
    .byte $0a,$12 ; up left for #12 even frames
    .byte $08,$17 ; up for #17 even frames
    .byte $00,$06 ; no input for #06 even frames
    .byte $02,$01 ; left for #01 even frame
    .byte $0a,$02 ; up left for #02 even frames
    .byte $08,$06 ; up for #06 even frames
    .byte $00,$06 ; no input for #06 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $00,$10 ; no input for #10 even frames
    .byte $02,$06 ; left for #06 even frames
    .byte $00,$0c ; no input for #0c even frames
    .byte $01,$07 ; right for #07 even frames
    .byte $04,$08 ; down for #08 even frames
    .byte $01,$04 ; right for #04 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $ff,$ff ; end input

demo_input_lvl_01_p2:
    .byte $00,$56 ; no input for #56 even frames
    .byte $02,$14 ; left for #14 even frames
    .byte $00,$06 ; no input for #06 even frames
    .byte $01,$04 ; right for #04 even frames
    .byte $00,$0b ; no input for #0b even frames
    .byte $08,$22 ; up for #22 even frames
    .byte $09,$08 ; up right for #08 even frames
    .byte $08,$15 ; up for #15 even frames
    .byte $0a,$13 ; up left for #13 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $09,$06 ; up right for #06 even frames
    .byte $08,$06 ; up for #06 even frames
    .byte $09,$03 ; up right for #03 even frames
    .byte $01,$05 ; right for #05 even frames
    .byte $09,$0d ; up right for #0d even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $0a,$0f ; up left for #0f even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $09,$03 ; up right for #03 even frames
    .byte $01,$15 ; right for #15 even frames
    .byte $09,$06 ; up right for #06 even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $05,$04 ; right down for #04 even frames
    .byte $04,$0c ; down for #0c even frames
    .byte $05,$06 ; right down for #06 even frames
    .byte $01,$04 ; right for #04 even frames
    .byte $09,$11 ; up right for #11 even frames
    .byte $08,$05 ; up for #05 even frames
    .byte $09,$15 ; up right for #15 even frames
    .byte $08,$03 ; up for #03 even frames
    .byte $0a,$01 ; up left for #01 even frame
    .byte $02,$03 ; left for #03 even frames
    .byte $04,$12 ; down for #12 even frames
    .byte $05,$03 ; right down for #03 even frames
    .byte $01,$05 ; right for #05 even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$26 ; up for #26 even frames
    .byte $0a,$13 ; up left for #13 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $09,$01 ; up right for #01 even frame
    .byte $08,$03 ; up for #03 even frames
    .byte $0a,$07 ; up left for #07 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $01,$03 ; right for #03 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $0a,$10 ; up left for #10 even frames
    .byte $08,$01 ; up for #01 even frame
    .byte $01,$04 ; right for #04 even frames
    .byte $09,$04 ; up right for #04 even frames
    .byte $08,$09 ; up for #09 even frames
    .byte $0a,$26 ; up left for #26 even frames
    .byte $08,$04 ; up for #04 even frames
    .byte $09,$06 ; up right for #06 even frames
    .byte $08,$08 ; up for #08 even frames
    .byte $09,$10 ; up right for #10 even frames
    .byte $08,$06 ; up for #06 even frames
    .byte $0a,$1c ; up left for #1c even frames
    .byte $08,$0d ; up for #0d even frames
    .byte $00,$1d ; no input for #1d even frames
    .byte $04,$0f ; down for #0f even frames
    .byte $01,$01 ; right for #01 even frame
    .byte $ff,$ff ; end input

demo_input_lvl_02_p1:
    .byte $00,$54 ; no input for #54 even frames
    .byte $01,$43 ; right for #43 even frames
    .byte $08,$02 ; up for #02 even frames
    .byte $09,$2c ; up right for #2c even frames
    .byte $01,$04 ; right for #04 even frames
    .byte $04,$13 ; down for #13 even frames
    .byte $01,$0b ; right for #0b even frames
    .byte $05,$01 ; right down for #01 even frame
    .byte $04,$13 ; down for #13 even frames
    .byte $84,$01 ; down and a for #01 even frame
    .byte $86,$0b ; left down and a for #0b even frames
    .byte $84,$01 ; down and a for #01 even frame
    .byte $81,$02 ; right and a for #02 even frames
    .byte $01,$12 ; right for #12 even frames
    .byte $09,$1f ; up right for #1f even frames
    .byte $01,$02 ; right for #02 even frames
    .byte $04,$0a ; down for #0a even frames
    .byte $01,$03 ; right for #03 even frames
    .byte $09,$05 ; up right for #05 even frames
    .byte $01,$02 ; right for #02 even frames
    .byte $04,$24 ; down for #24 even frames
    .byte $01,$0a ; right for #0a even frames
    .byte $09,$0c ; up right for #0c even frames
    .byte $08,$01 ; up for #01 even frame
    ; continues in bank 1

; end of bank
; unused #$0 bytes out of #$2,000 bytes total (100% full)
; unused 0 bytes out of 8,192 bytes total (100% full)
bank_0_unused_space: