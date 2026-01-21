; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/

; Bank A contains Level 2 (First Base) and Level 4 (Inner Base) enemies.
; Then, Bank A contains Level 7 (Headquarters) screen layout and the table of
; supertiles per screen for Level 7. Finally, Bank A contains the start of
; Level 7's supertile definitions.
; Enemies in this bank:
; * Enemy Type #$2e - Overhead Tank Soldier
; * Enemy Type #$30 - Overhead Tank Soldier Bullet
; * Enemy Type #$5b - Rotating Turret
; * Enemy Type #$5c - Stationary Red Soldier
; * Enemy Type #$63 - Overhead Soldier
; * Enemy Type #$69 - Tank Boss
; * Enemy Type #$6a - Tank Gunner
; * Enemy Type #$36 - Tank Boss Electrode
; * Enemy Type #$25 - Rack-Mounted Turret
; * Enemy Type #$2c - Collapsible Ceiling
; * Enemy Type #$2d - Falling Ceiling Tile
; * Enemy Type #$31 - Spinning Bubbles
; * Enemy Type #$39 - Elevator
; * Enemy Type #$40 - Winged Soldier
; * Enemy Type #$41 - Winged Soldier Generator
; * Enemy Type #$43 - Chandelier Arm
; * Enemy Type #$44 - Chandelier Arm Laser
; * Enemy Type #$6b - Bubbles
; * Enemy Type #$6c - Ceiling Vent
; * Enemy Type #$42 - Laser Chandelier

; 8 KiB PRG ROM
.segment "BANKA"

.include "constants.asm"

; import from bank 3
.import advance_enemy_routine
.import set_delay_adv_enemy_routine
.import update_enemy_pos
.import apply_velocity
.import player_enemy_x_dist
.import copy_enemy_vars_to_zp
.import get_bg_collision
.import enemy_routine_explosions
.import enemy_explosion_routine_00
.import enemy_explosion_routine_01
.import enemy_explosion_routine_03
.import set_enemy_hp
.import try_create_enemy_from_existing
.import set_enemy_routine
.import remove_enemy
.import overhead_tank_soldier_draw_destroyed
.import set_enemy_destroy_dir_and_vel
.import enemy_bullet_routine_01
.import enemy_overhead_bullet_routine_03
.import enemy_overhead_bullet_routine_04
.import enemy_bullet_routine_00
.import enemy_routine_init_explosions
.import enemy_routine_explosions
.import bg_enemy_explosion_routine_01
.import init_bg_boss_pre_irq_max_scrolls
.import set_bg_boss_scroll_nt
.import bg_boss_apply_vel
.import set_enemy_hp_hard
.import set_boss_defeated_remove_enemy
.import bg_enemy_explosion_routine_00
.import enemy_routine_boss_defeated_01
.import flip_enemy_x_dir
.import update_pos_check_offscreen
.import clear_enemy_vel
.import set_enemy_animation_sprite
.import check_bg_wall_collision
.import check_bg_collision_at_y
.import get_enemy_bg_collision_code_onscreen
.import set_y_pos_for_bg
.import set_enemy_destroy_sprite_and_vel
.import apply_gravity_adv_routine_after_delay
.import hone_to_player_set_enemy_vel
.import set_vel_to_target_player
.import enemy_routine_boss_defeated_00
.import clear_bg_collision_data
.import clear_enemy_sprite
.import set_enemy_hp_from_a_and_y
.import get_enemy_bg_collision_code
.import flip_enemy_y_dir
.import update_nametable_square_at_pos
.import load_banks_update_supertiles
.import apply_destroy_vel_adv_routine_after_delay

; import from bank f
.import overhead_quad_aim_dir_01
.import convert_dir_to_overhead_dir
.import get_rotate_01
.import fire_bullet_at_player
.import dir_to_overhead_dir_tbl
.import load_banks_update_supertile_if_room
.import rotate_aim_dir_01_zp
.import fire_near_player
.import play_sound
.import set_nmi_noop_irq
.import run_routine_from_tbl_below
.import fire_bullet_at_player_1x_speed
.import reset_scroll_draw_point
.import reset_draw_point
.import init_irq_scroll

; export for bank 3
.export overhead_tank_soldier_routine_ptr_tbl
.export overhead_soldier_routine_ptr_tbl
.export stationary_red_soldier_routine_ptr_tbl
.export overhead_tank_soldier_bullet_routine_ptr_tbl
.export overhead_rotating_turret_routine_ptr_tbl
.export tank_boss_routine_ptr_tbl
.export tank_gunner_routine_ptr_tbl
.export elevator_routine_ptr_tbl
.export winged_soldier_gen_routine_ptr_tbl
.export winged_soldier_routine_ptr_tbl
.export rack_turret_routine_ptr_tbl
.export spinning_bubbles_routine_ptr_tbl
.export laser_chandelier_routine_ptr_tbl
.export chandelier_arm_routine_ptr_tbl
.export chandelier_arm_laser_routine_ptr_tbl
.export ceiling_vent_routine_ptr_tbl
.export ceiling_vent_bubble_routine_ptr_tbl
.export collapsible_ceiling_routine_ptr_tbl
.export falling_ceiling_tile_routine_ptr_tbl
.export tank_boss_electrode_routine_ptr_tbl

; export for bank f
.export level_7_supertiles_screen_ptr_table
.export level_7_supertile_data
.export level_7_screen_layout_tbl

.byte $3a ; bank byte

; enemy type #$2e
overhead_tank_soldier_routine_ptr_tbl:
    .addr overhead_tank_soldier_routine_00 ; set hp, destroy attributes, position, delays, aim dir, increment number of tanks, advance routine
    .addr overhead_tank_soldier_routine_01 ; update position, see if scrolled into active position, and if so advance routine
    .addr overhead_tank_soldier_routine_02 ; set tank sprite for aim direction, wait for firing delay, fire single center shot, advance routine
    .addr overhead_tank_soldier_routine_03 ; remove if at bottom, fire 3 shots when delay elapsed, go to overhead_tank_soldier_routine_02
    .addr enemy_routine_init_explosions    ; enemy destroyed routine - configure for 5 explosions
    .addr enemy_routine_explosions         ; generate 5 explosions, with an #$08 frame delay between each one
    .addr overhead_tank_soldier_routine_06 ; set scroll flags, draw destroyed tank, start explosions
    .addr enemy_explosion_routine_01       ; animate explosion sequence
    .addr enemy_explosion_routine_03       ; mark destroyed, remove enemy

; set hp, destroy attributes, position, delays, aim dir, increment number of tanks, advance routine
overhead_tank_soldier_routine_00:
    lda #$10                    ; HP = #$10, #$14, or #$17
    jsr set_enemy_hp            ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$85
    sta ENEMY_DESTROY_ATTRS,x   ; allow bullets to travel through while initializing
    lda ENEMY_X_POS,x           ; load enemy's X position
    clc                         ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_X_POS,x
    lda ENEMY_Y_POS,x           ; load enemy's Y position
    clc                         ; clear carry in preparation for addition
    adc #$04
    sta ENEMY_Y_POS,x
    lda #$04
    sta ENEMY_ANIMATION_DELAY,x
    lda #$1b
    sta ENEMY_FIRING_DELAY,x
    lda #$06
    sta ENEMY_VAR_1,x           ; set aim direction to be straight down (6 o'clock)
    lda #$03
    sta ENEMY_VAR_2,x           ; set default enemy aim direction (7 o'clock)
    inc NUM_TANKS               ; increment number of tanks on screen
    jsr update_enemy_pos        ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine   ; advance to next routine

; update position, see if scrolled into active position, and if so advance routine
overhead_tank_soldier_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$20
    bcc @exit
    lda #$05
    sta ENEMY_DESTROY_ATTRS,x       ; enemy in range, enable bullet collisions
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to overhead_tank_soldier_routine_02

@exit:
    rts

; set tank sprite for aim direction, wait for firing delay, fire single center shot, advance routine
overhead_tank_soldier_routine_02:
    ldy ENEMY_VAR_2,x                           ; load tank aim direction
    lda overhead_tank_soldier_sprite_tbl,y      ; load appropriate sprite for aim direction
    sta ENEMY_SPRITE,x                          ; set overhead tank soldier sprite
    lda overhead_tank_soldier_sprite_attr_tbl,y ; load horizontal flip for sprite flag
    jsr set_overhead_enemy_sprite_attr          ; set enemy sprite attribute including recoil based on ENEMY_VAR_5 timer and a register
    jsr update_enemy_pos                        ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x                    ; decrement bullet fire recoil timer
    bne @check_target                           ; branch if still firing bullet
    lda ENEMY_VAR_2,x                           ; not firing bullet, load aim direction
    cmp #$06                                    ; see if aiming at 9 o'clock
    beq @check_target                           ; branch if aiming at 9 o'clock
    lda ENEMY_VAR_3,x                           ; not aiming at 9 o'clock, load player to target
    sta $0a
    jsr copy_enemy_vars_to_zp                   ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$04                                    ; set bullet speed code to 1.5x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player                   ; fire center single bullet at player index specified in $0a (0 = p1, 1 = p2)
    jsr overhead_tank_soldier_bullet_set_pos    ; set initial position of overhead tank soldier bullet based on aim direction
    lda #$4d
    sta ENEMY_FIRING_DELAY,x                    ; set bullet fire recoil timer to #$4d

@check_target:
    dec ENEMY_ANIMATION_DELAY,x  ; decrement targeting delay
    bne @continue
    lda #$08
    sta ENEMY_ANIMATION_DELAY,x  ; set net target time to target delay
    jsr player_enemy_x_dist      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                          ; transfer closest player index to a
    sta ENEMY_VAR_3,x            ; set to targeted player index
    jsr overhead_quad_aim_dir_01 ; determine next aim direction to get closer to that value
                                 ; then use that value to update the overhead aim direction ENEMY_VAR_2,x

@continue:
    dec ENEMY_DELAY,x               ; decrement delay before firing 3 shots
    bne @exit
    lda #$00
    sta ENEMY_FRAME,x
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and routine to overhead_tank_soldier_routine_03

@exit:
    rts

; sprite_53, sprite_52, sprite_51
overhead_tank_soldier_sprite_tbl:
    .byte $53,$52,$51,$52,$53,$53,$53,$53

overhead_tank_soldier_sprite_attr_tbl:
    .byte $40,$40,$00,$00,$00,$00,$00,$40

; remove if at bottom, fire 3 shots when delay elapsed, go to overhead_tank_soldier_routine_02
overhead_tank_soldier_routine_03:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    cmp #$c0
    bcc @continue
    jmp remove_enemy  ; tank at bottom of screen, remove

@continue:
    jsr update_enemy_pos                ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne @exit
    lda ENEMY_FRAME,x
    asl
    tay
    lda overhead_tank_supertile_tbl,y   ; create recoil effect by updating bg tiles for tank
    sta $08                             ; set first supertile
    lda overhead_tank_supertile_tbl+1,y
    sta $0c                             ; set second supertile index
    lda #$11
    sta $0d
    lda ENEMY_Y_POS,x                   ; load enemy's Y position
    adc #$08
    tay
    lda ENEMY_X_POS,x                   ; load enemy's X position
    sbc #$08
    jsr load_banks_update_supertiles    ; update 2 supertiles: $08 at (a,y), and $0c at offset encoded in $0d
    lda #$01
    bcs @set_delay_exit
    inc ENEMY_FRAME,x
    lda ENEMY_FRAME,x
    cmp #$02
    bcs @set_delay_dec_routine
    lda #$02
    sta $08

; firing 3 shots
@tank_turret_loop:
    ldy #$30                           ; enemy type = overhead tank soldier bullet
    jsr try_create_enemy_from_existing ; create overhead tank soldier bullet
    bcc @exit                          ; exit if unable to create overhead tank soldier bullet
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    clc                                ; clear carry in preparation for addition
    adc #$1c
    sta $09
    lda ENEMY_X_POS,x                  ; load enemy's X position
    ldx $11
    ldy $08
    clc                                ; clear carry in preparation for addition
    adc overhead_tank_bullet_adj_tbl,y
    sta ENEMY_X_POS,x
    lda $09
    sta ENEMY_Y_POS,x
    tya
    sta ENEMY_ATTRIBUTES,x
    dec $08
    bpl @tank_turret_loop
    ldx ENEMY_CURRENT_SLOT
    lda #$08

@set_delay_exit:
    sta ENEMY_DELAY,x

@exit:
    rts

@set_delay_dec_routine:
    lda #$60
    sta ENEMY_DELAY,x
    lda #$03
    jmp set_enemy_routine ; set routine to overhead_tank_soldier_routine_02

overhead_tank_bullet_adj_tbl:
    .byte $f4,$0c,$00

overhead_tank_supertile_tbl:
    .byte $69,$6a ; firing/retracted cannons
    .byte $46,$47 ; extended cannons

; set scroll flags, draw destroyed tank, start explosions
overhead_tank_soldier_routine_06:
    dec NUM_TANKS            ; tank destroyed decrement number of tanks on screen
    bne @draw_destroyed_tank ; branch to skip setting Y_SCROLL_FLAGS and LEVEL_Y_SCROLL_FLAGS if another tank is on screen
    lda #$40                 ; both tanks destroyed, allow scroll
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS

@draw_destroyed_tank:
    lda #$00
    jsr overhead_tank_soldier_draw_destroyed ; draw 4 supertiles for the destroyed overhead tank
    bcs @continue                            ; branch if unable to draw supertiles to try again next frame
                                             ; !(BUG) if tiles weren't drawn, the routine wasn't advanced and NUM_TANKS will be decremented again
                                             ; this means that if 2 tanks are on screen and one is destroyed, but drawing of supertiles failed,
                                             ; then the player can scroll past the second tank without destroying it
    jmp enemy_explosion_routine_00           ; set empty sprite, play optional enemy destroyed sound, disable collisions
                                             ; and advance routine

@continue:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; input
;  * zero flag - set when bullet was created, clear when unable to create
overhead_tank_soldier_bullet_set_pos:
    bne @exit                                    ; exit if unable to create bullet
    lda #$06
    sta ENEMY_VAR_5,x                            ; set recoil effect timer
    ldy ENEMY_VAR_2,x                            ; load tank aim direction
    ldx $11                                      ; load created bullet enemy slot
    lda ENEMY_Y_POS,x                            ; load overhead tank bullet's Y position
    clc                                          ; clear carry in preparation for addition
    adc overhead_tank_soldier_bullet_y_adj_tbl,y
    sta ENEMY_Y_POS,x
    lda ENEMY_X_POS,x                            ; load enemy's X position
    clc                                          ; clear carry in preparation for addition
    adc overhead_tank_soldier_bullet_x_adj_tbl,y
    sta ENEMY_X_POS,x
    ldx ENEMY_CURRENT_SLOT

@exit:
    rts

overhead_tank_soldier_bullet_y_adj_tbl:
    .byte $f6 ; 0 = 3 o'clock
    .byte $00 ; 1 = 5 o'clock
    .byte $02 ; 2 = 6 o'clock
    .byte $00 ; 3 = 7 o'clock
    .byte $f6 ; 4 = 9 o'clock
    .byte $f6 ; 5 = 9 o'clock
    .byte $f6 ; 6 = 9 o'clock
    .byte $f6 ; 7 = 3 o'clock

overhead_tank_soldier_bullet_x_adj_tbl:
    .byte $0e ; 0 = 3 o'clock
    .byte $0c ; 1 = 5 o'clock
    .byte $00 ; 2 = 6 o'clock
    .byte $f4 ; 3 = 7 o'clock
    .byte $f2 ; 4 = 9 o'clock
    .byte $f2 ; 5 = 9 o'clock
    .byte $00 ; 6 = 9 o'clock
    .byte $0e ; 7 = 3 o'clock

; enemy type #$30
overhead_tank_soldier_bullet_routine_ptr_tbl:
    .addr overhead_tank_soldier_bullet_routine_00 ; set bullet type (#$02), destroy attributes (#$80) and velocity (direction) based on attribute
    .addr enemy_bullet_routine_01                 ; wait for muzzle delay, set actual bullet sprite, update position
    .addr overhead_tank_soldier_bullet_routine_02 ; flash palette, apply velocity, decrement destroy delay, remove if delay elapsed
    .addr enemy_overhead_bullet_routine_03        ; enemy destroyed routine, set animation delay before advance routine
    .addr enemy_overhead_bullet_routine_04        ; show bullet explosion, then remove enemy

; set bullet type (#$02), destroy attributes (#$80) and velocity (direction) based on attribute
overhead_tank_soldier_bullet_routine_00:
    lda #$02
    sta ENEMY_VAR_1,x                            ; set bullet type to #$02 (silver bullet)
    jsr enemy_bullet_routine_00
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x                    ; mark tank bullet so other bullets travel through it
    lda ENEMY_ATTRIBUTES,x
    asl
    asl                                          ; quadruple since each entry is #$04 bytes
    tay                                          ; transfer to offset register
    lda overhead_tank_soldier_bullet_vel_tbl,y   ; load bullet fractional Y velocity
    sta ENEMY_Y_VELOCITY_FRACT,x                 ; set bullet fractional Y velocity
    lda overhead_tank_soldier_bullet_vel_tbl+1,y ; load bullet fast Y velocity
    sta ENEMY_Y_VELOCITY_FAST,x                  ; set bullet fast Y velocity
    lda overhead_tank_soldier_bullet_vel_tbl+2,y ; load bullet fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x                 ; set bullet fractional X velocity
    lda overhead_tank_soldier_bullet_vel_tbl+3,y ; load bullet fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x                  ; set bullet fast X velocity
    rts

overhead_tank_soldier_bullet_vel_tbl:
    .byte $e8,$01,$6c,$ff ; x vel = -0.578125, y vel = 1.90625 (left bullet)
    .byte $e8,$01,$94,$00 ; x vel =  0.578125, y vel = 1.90625 (right bullet)
    .byte $00,$02,$00,$00 ; x vel =  0       , y vel = 2.00000 (center bullet)

; flash palette, apply velocity, decrement destroy delay, remove if delay elapsed
overhead_tank_soldier_bullet_routine_02:
    lda GLOBAL_TIMER
    and #$01
    sta ENEMY_SPRITE_ATTR,x   ; flash palette
    jsr apply_velocity        ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_FIRING_DELAY,x  ; decrement bullet destroy delay
    bne @exit                 ; exit if bullet destroy delay hasn't elapsed
    jmp advance_enemy_routine ; bullet destroy delay elapsed
                              ; advance to enemy_overhead_bullet_routine_03 (destroy bullet)

@exit:
    rts

; enemy type #$5b
overhead_rotating_turret_routine_ptr_tbl:
    .addr overhead_rotating_turret_routine_00 ; set HP, correct position, set initial aim direction, advance routine
    .addr overhead_rotating_turret_routine_01 ; set position, wait until in main portion of screen, activate for firing, advance routine
    .addr overhead_rotating_turret_routine_02 ; check if scrolled offscreen, update position, fire if delay elapsed, target closest player if delay elapsed
    .addr overhead_rotating_turret_routine_03 ; update background tiles based on aim direction, decrement routine
    .addr enemy_routine_init_explosions       ; enemy destroyed routine - configure for 5 explosions
    .addr enemy_routine_explosions            ; generate 5 explosions, with an #$08 frame delay between each one
    .addr overhead_rotating_turret_routine_06
    .addr enemy_explosion_routine_01          ; animate explosion sequence
    .addr enemy_explosion_routine_03          ; mark destroyed, remove enemy

; set HP, correct position, set initial aim direction, advance routine
overhead_rotating_turret_routine_00:
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite (uses bg tiles instead)
    lda #$08                  ; HP = #$08, #$0c, or #$0f
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$85
    sta ENEMY_DESTROY_ATTRS,x ; allow bullets to travel through while initializing
    lda ENEMY_X_POS,x         ; load enemy's X position
    adc #$08
    sta ENEMY_X_POS,x         ; correct X position
    lda #$06
    sta ENEMY_VAR_1,x
    lda #$02
    sta ENEMY_VAR_2,x         ; set initial aim direction to 6 o'clock
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; set position, wait until in main portion of screen, activate for firing, advance routine
overhead_rotating_turret_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$20
    bcc @exit                       ; exit if still at very top of screen
    lda #$05                        ; activating enemy, set destroy attributes
    sta ENEMY_DESTROY_ATTRS,x       ; disable player-enemy collision, spike explosion, sound_1b (BAKUHA1) explosion
    lda #$02
    sta ENEMY_VAR_4,x               ; set number of bullets to fire per attack round
    lda #$01
    sta ENEMY_FIRING_DELAY,x        ; set initial firing delay
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to overhead_rotating_turret_routine_02
                                    ; to fire and target player

@exit:
    rts

; check if scrolled offscreen, update position, fire if delay elapsed, target closest player if delay elapsed
overhead_rotating_turret_routine_02:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    cmp #$c0
    bcc @continue     ; branch to continue if not at bottom of screen
    jmp remove_enemy  ; scrolling off screen, remove enemy

@continue:
    jsr update_enemy_pos                        ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x                    ; decrement firing delay
    bne @target_if_elapsed                      ; branch if firing delay has not elapsed to check targeting delay
    lda ENEMY_VAR_3,x                           ; firing delay elapsed, fire at ENEMY_VAR_3,x
    sta $0a                                     ; store player index of player to target (0 = p1, 1 = p2)
    jsr copy_enemy_vars_to_zp                   ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                                    ; bullet speed code #$03 (1.25x speed)
    jsr fire_near_player                        ; fire near or at player $0a using speed code y
    jsr overhead_rotating_turret_set_bullet_pos ; set initial position of turret bullet based on overhead aim direction
    lda #$10                                    ; a delay of #$10 between bullets in an attack round
    dec ENEMY_VAR_4,x                           ; decrement number of bullets to fire this attack round
    bpl @set_firing_delay                       ; branch if more bullets to fire
    lda #$02                                    ; finished firing all bullets
    sta ENEMY_VAR_4,x                           ; reset bullet count for next attack round
    lda #$57                                    ; attack round complete, load longer delay

@set_firing_delay:
    sta ENEMY_FIRING_DELAY,x ; set delay until next fire

@target_if_elapsed:
    dec ENEMY_DELAY,x                 ; decrement target player delay
    bne overhead_rotating_turret_exit ; exit if target player delay hasn't elapsed
    lda #$0c                          ; time to target player
    sta ENEMY_DELAY,x                 ; reset target player delay
    jsr player_enemy_x_dist           ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_Y_POS,x                 ; load enemy's Y position
    sec                               ; set carry flag in preparation for subtraction
    sbc #$10
    cmp PLAYER_SPRITE_Y_POS,y         ; see if player above or below enemy
    tya                               ; transfer closet player index to a
    bcc @target_player                ; branch if enemy above player
    eor #$01                          ; enemy below player, target other player if they are playing
    sta $0a                           ; set player index of player to target (if 1p game, still targets p1)

@target_player:
    sta ENEMY_VAR_3,x
    jsr rotate_aim_dir_01_zp                            ; determine next aim direction [#$00-#$17] ($0c) to target player with index $0a
                                                        ; adjusts ENEMY_VAR_1,x to get closer to that value using quadrant_aim_dir_01
                                                        ; not using overhead_quad_aim_dir_01 which would set ENEMY_VAR_2,x as wel
                                                        ; because overhead rotating turret can only aim downward for this enemy
    lda ENEMY_VAR_2,x                                   ; load current overhead aim direction
    sta $0b                                             ; backup for comparison later
    lda ENEMY_VAR_1,x                                   ; load computed new aim direction (#$00 = 3 o'clock up to #$17 = 2:30 o'clock inclusively)
    cmp #$0d                                            ; see if aiming above enemy (up-screen)
    bcs overhead_rotating_turret_exit                   ; branch if aiming above enemy (up-screen)
    tay                                                 ; aiming downward (#$00 - #$0c), transfer aim direction to offset register
    lda overhead_rotating_turret_overhead_aim_dir_tbl,y
    sta ENEMY_VAR_2,x                                   ; set overhead aim direction
    cmp $0b                                             ; see if turret has rotated (new overhead aim direction)
    beq overhead_rotating_turret_exit                   ; branch if turret didn't change aiming direction
    jmp advance_enemy_routine                           ; advance to overhead_rotating_turret_routine_03 to update bg tiles

overhead_rotating_turret_exit:
    rts

; convert from a downward aim direction [#$00-#$0c] to an overhead aim direction [#$00-#$04]
overhead_rotating_turret_overhead_aim_dir_tbl:
    .byte $00,$00,$01,$01,$01,$02,$02,$02,$03,$03,$03,$04,$04

; update background tiles based on aim direction, decrement routine
overhead_rotating_turret_routine_03:
    jsr update_enemy_pos                          ; adjust position based on scroll (does not apply velocity)
    ldy ENEMY_VAR_2,x                             ; load overhead aim direction
    lda overhead_rotating_turret_bg_tile_tbl,y    ; load supertile index
    jsr overhead_rotating_turret_update_supertile
    bcs overhead_rotating_turret_exit
    lda #$03
    jmp set_enemy_routine                         ; set routine to overhead_rotating_turret_routine_02

; supertile index for overhead rotating turret (level_2_supertile_data)
overhead_rotating_turret_bg_tile_tbl:
    .byte $97 ; #$00 - 3 o'clock
    .byte $96 ; #$01 - 4:30 o'clock
    .byte $d4 ; #$02 - 6 o'clock
    .byte $93 ; #$03 - 7:30 o'clock
    .byte $92 ; #$04 - 9:00 o'clock

overhead_rotating_turret_routine_06:
    lda #$98                                      ; level supertile offset
    jsr overhead_rotating_turret_update_supertile
    bcs @continue
    jmp enemy_explosion_routine_00                ; set empty sprite, play optional enemy destroyed sound, disable collisions

@continue:
    jmp update_enemy_pos

; input
;  * a - supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
;    indexes into level_2_supertile_data
overhead_rotating_turret_update_supertile:
    sta $08                                 ; set supertile data and palette data offset
    lda ENEMY_Y_POS,x                       ; load enemy's Y position
    sbc #$0c
    tay
    lda ENEMY_X_POS,x                       ; load enemy's X position
    sbc #$0c
    jsr load_banks_update_supertile_if_room ; update nametable supertile (4x4 tile) at (a, y)
    ldx ENEMY_CURRENT_SLOT                  ; restore x to current enemy slot
    rts

overhead_rotating_turret_set_bullet_pos:
    bne @exit                                           ; branch if unable to create bullet
    ldy ENEMY_VAR_2,x                                   ; load overhead aim direction
    ldx $11                                             ; load enemy slot of created bullet
    lda ENEMY_Y_POS,x                                   ; load bullet's Y position
    clc                                                 ; clear carry in preparation for addition
    adc overhead_rotating_turret_bullet_x_pos_adj_tbl,y
    sta ENEMY_Y_POS,x                                   ; set initial Y position of bullet based on turret overhead aim direction
    lda ENEMY_X_POS,x                                   ; load enemy's X position
    clc                                                 ; clear carry in preparation for addition
    adc overhead_rotating_turret_bullet_y_pos_adj_tbl,y
    sta ENEMY_X_POS,x                                   ; set initial Y position of bullet based on turret overhead aim direction
    ldx ENEMY_CURRENT_SLOT                              ; restore enemy slot to overhead rotating turret

@exit:
    rts

overhead_rotating_turret_bullet_x_pos_adj_tbl:
    .byte $fb ; #$00 - 3 o'clock
    .byte $04 ; #$01 - 4:30 o'clock
    .byte $0c ; #$02 - 6 o'clock
    .byte $04 ; #$03 - 7:30 o'clock
    .byte $fb ; #$04 - 9 o'clock

overhead_rotating_turret_bullet_y_pos_adj_tbl:
    .byte $12 ; #$00 - 3 o'clock
    .byte $0d ; #$01 - 4:30 o'clock
    .byte $00 ; #$02 - 6 o'clock
    .byte $f3 ; #$03 - 7:30 o'clock
    .byte $ee ; #$04 - 9 o'clock

; enemy type #$5c - overhead stationary red soldier (level 2 only)
; compare to #$63 overhead soldier, which can be red and fires at player, but
; isn't stationary.
stationary_red_soldier_routine_ptr_tbl:
    .addr stationary_red_soldier_routine_00 ; initialize variables and advance routine
    .addr stationary_red_soldier_routine_01 ; set sprite, wait for targeting delay, target player, wait for firing delay, fire at player 2 times, repeat
    .addr enemy_explosion_routine_00        ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01        ; animate explosion sequence
    .addr enemy_explosion_routine_03        ; mark destroyed, remove enemy

; initialize variables and advance routine
stationary_red_soldier_routine_00:
    lda #$02
    sta ENEMY_DESTROY_ATTRS,x       ; set circular explosion animation for when destroyed
    lda #$01
    sta ENEMY_VAR_4,x               ; set number of bullets to fire to #$02
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$06
    sta ENEMY_VAR_1,x               ; set initial aim direction to 6 o'clock (down)
    lda #$03
    sta ENEMY_VAR_2,x               ; set initial sprite offset to #$03, i.e. sprite_41
    lda #$20
    sta ENEMY_FIRING_DELAY,x        ; set initial firing delay to #$20
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to stationary_red_soldier_routine_01

; set sprite, wait for targeting delay, target player, wait for firing delay, fire at player 2 times, repeat
stationary_red_soldier_routine_01:
    ldy ENEMY_VAR_2,x                            ; load offset for soldier sprite
    lda stationary_red_soldier_sprite_tbl,y
    sta ENEMY_SPRITE,x                           ; set soldier sprite based
    lda stationary_red_soldier_sprite_attr_tbl,y
    jsr set_overhead_enemy_sprite_attr           ; set enemy sprite attribute including recoil based on ENEMY_VAR_5 timer and a register
    jsr update_enemy_pos                         ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x                     ; decrement firing delay
    bne @target_player                           ; branch if firing delay not elapsed
    lda ENEMY_VAR_3,x                            ; firing delay elapsed, load targeted player index
    sta $0a                                      ; set player to target
    jsr copy_enemy_vars_to_zp                    ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                                     ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player                    ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr stationary_red_soldier_adj_bullet_pos    ; adjust bullet position based on soldier facing direction
    lda #$10                                     ; short firing delay between first and second bullet
    dec ENEMY_VAR_4,x                            ; decrement number of bullets to fire
    bpl @set_firing_delay_exit                   ; branch if more bullet(s) to fire to set short firing delay and exit
    lda #$01                                     ; fired all bullets, reset bullet count and set longer random firing delay
    sta ENEMY_VAR_4,x                            ; set number of bullets to fire to #$02
    lda RANDOM_NUM                               ; load random number
    adc GLOBAL_TIMER
    sta RANDOM_NUM                               ; randomize random number
    and #$0f
    adc #$77

@set_firing_delay_exit:
    sta ENEMY_FIRING_DELAY,x ; set random firing delay
    rts

@target_player:
    dec ENEMY_DELAY,x            ; decrement sprite update delay
    bne @exit                    ; exit if sprite update delay hasn't elapsed
    lda #$08
    sta ENEMY_DELAY,x            ; reset sprite update delay
    jsr player_enemy_x_dist      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya
    sta ENEMY_VAR_3,x            ; set closest player
    jsr overhead_quad_aim_dir_01 ; determine next aim direction to get closer to that value
                                 ; then use that value to update the overhead aim direction ENEMY_VAR_2,x
                                 ; which is used to set the soldier sprite

@exit:
    rts

; sprite_47, sprite_41, sprite_3e, sprite_44, sprite_48
stationary_red_soldier_sprite_tbl:
    .byte $47,$41,$3e,$41,$47,$44,$48,$44

stationary_red_soldier_sprite_attr_tbl:
    .byte $40,$40,$00,$00,$00,$00,$00,$40

; !(UNUSED) unused
bank_a_unused_00:
    jmp set_enemy_destroy_dir_and_vel
    jmp apply_destroy_vel_adv_routine_after_delay

; input
;  * x - enemy slot index
;  * $11 - created bullet enemy slot
;  * $17 - overhead soldier aim direction
;  * zero flag - set when bullet created, clear when unable to create
stationary_red_soldier_adj_bullet_pos:
    bne @exit                                 ; exit if fire_bullet_at_player was unable to create a bullet
    ldy ENEMY_VAR_2,x                         ; load soldier aim direction
    jmp red_stationary_soldier_adj_bullet_pos

@exit:
    rts

; enemy type #$63
overhead_soldier_routine_ptr_tbl:
    .addr overhead_soldier_routine_00 ; initialize variables, set target, apply velocity, set firing delay, and advance routine
    .addr overhead_soldier_routine_01 ; march around targeting player and avoiding background collisions
    .addr overhead_soldier_routine_02 ; red marching overhead soldier whose firing delay has elapsed, fire at player
    .addr enemy_explosion_routine_00  ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01  ; animate explosion sequence
    .addr enemy_explosion_routine_03  ; mark destroyed, remove enemy

; initialize variables, set target, apply velocity, set firing delay, and advance routine
overhead_soldier_routine_00:
    lda #$02
    sta ENEMY_DESTROY_ATTRS,x ; set circular explosion animation for when destroyed
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcc @continue             ; branch if walking downward
    lda #$40                  ; walking upward
    sta ENEMY_VAR_4,x
    lda #$e0
    sta ENEMY_Y_POS,x         ; set initial Y position to bottom of screen

@continue:
    jsr @target
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$20
    sta ENEMY_FIRING_DELAY,x        ; set soldier firing delay
    lda #$30
    jmp set_delay_adv_enemy_routine ; set delay to #$30 and set routine to overhead_soldier_routine_01

; set aim target and velocity based on direction
@target:
    jsr overhead_soldier_get_target ; $0a = target player
                                    ; ($0b, $0c) = random position at bottom of screen when no player to target
    jsr copy_enemy_vars_to_zp       ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_01               ; set enemy aim direction ($0c) and rotation direction (a) targeting player $0a using quadrant_aim_dir_01
                                    ; if no player to target, use $(0b, $0c)
    lda $0c                         ; load aim direction
    sta ENEMY_VAR_1,x               ; store aim direction (#$00 - 3 o'clock, #$06 = 6 o'clock, #$0b = 9 o'clock)
    tay                             ; transfer to offset register
    jsr convert_dir_to_overhead_dir ; set ENEMY_VAR_2,x to overhead aim direction from aim direction y
    jmp overhead_soldier_set_vel    ; set the X and Y velocity based on overhead aim direction

; update ENEMY_VAR_2 with new aim direction based on targeted position
overhead_soldier_target:
    jsr overhead_soldier_get_target ; $0a = target player
                                    ; ($0b, $0c) = random position at bottom of screen when no player to target
    jsr overhead_quad_aim_dir_01    ; determine next aim direction to get closer to that value
                                    ; then use that value to update the overhead aim direction ENEMY_VAR_2,x

; sets the velocity based on ENEMY_VAR_2,x
; similar to alien_ladybug_set_vel
; input
;  * ENEMY_AVR_Z,x - overhead aim direction
overhead_soldier_set_vel:
    lda ENEMY_VAR_2,x                ; load overhead aim direction
    asl
    asl                              ; quadruple since each entry is #$04 bytes
    tay                              ; transfer to offset register
    lda overhead_soldier_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x     ; store enemy's fractional Y velocity
    lda overhead_soldier_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x      ; store enemy's fast Y velocity
    lda overhead_soldier_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x     ; store enemy's fractional X velocity
    lda overhead_soldier_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x      ; store enemy's fast X velocity
    rts

overhead_soldier_vel_tbl:
    .byte $00,$00,$c0,$00 ; y vel =  0.00, x vel =  0.75 (walk right)
    .byte $87,$00,$87,$00 ; y vel =  0.53, x vel =  0.53 (walk down right)
    .byte $c0,$00,$00,$00 ; y vel =  0.75, x vel =  0.00 (walk down)
    .byte $87,$00,$79,$ff ; y vel =  0.53, x vel = -0.53 (walk down left)
    .byte $00,$00,$40,$ff ; y vel =  0.00, x vel = -0.75 (walk left)
    .byte $79,$ff,$79,$ff ; y vel = -0.53, x vel = -0.53 (walk left up)
    .byte $40,$ff,$00,$00 ; y vel = -0.75, x vel =  0.00 (walk up)
    .byte $79,$ff,$87,$00 ; y vel = -0.53, x vel =  0.53 (walk up right)

; march around targeting player and avoiding background collisions
overhead_soldier_routine_01:
    ldy ENEMY_VAR_3,x                    ; see if encountered bg collision in previous frame
    beq @check_movement                  ; branch if no bg collision
    lda overhead_soldier_y_col_tbl-1,y   ; detected a collision in previous frame
                                         ; load y adjustment for where bg collision was detected
                                         ; !(OBS) neat small optimization here since y cannot be 0
                                         ; so table only needs to have 3 bytes
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x                    ; add to soldier Y position
    sta $08                              ; set Y position to use for bg collision detection
    lda overhead_soldier_x_col_tbl-1,y   ; load x adjustment for bg collision detection
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_X_POS,x                    ; add to soldier Y position
                                         ; to set X position to use for bg collision detection
    ldy $08                              ; load Y position
    jsr get_bg_collision                 ; get background collision code for position (a,y)
                                         ; this is the location where collision was detected the previous frame
    bne @check_movement                  ; branch if collision confirmed
    ldy ENEMY_VAR_3,x                    ; no collision at specified direction, use that direction
    lda overhead_soldier_aim_dir_tbl-1,y ; load restricted aim direction (3, 9, 6, or 12 o'clock)
    sta ENEMY_VAR_1,x                    ; set aim direction
    lda #$08
    sta ENEMY_DELAY,x                    ; set re-targeting delay
    lda #$00
    sta ENEMY_VAR_3,x                    ; clear collision flag
    jmp @anim_handle_fire                ; set sprite, apply velocity, advance routine

; collision as specified from previous frame is still there, or no collision from previous frame
; check solider is in frame, and then for horizontal collision, finally vertical collision
@check_movement:
    lda ENEMY_X_VELOCITY_FAST,x            ; load enemy's fast X velocity
    ora ENEMY_X_VELOCITY_FRACT,x
    beq @check_y_collision_dir_aim         ; branch to check for collision in vertical direction if not moving horizontally
    lda ENEMY_X_VELOCITY_FAST,x            ; movement has horizontal component, load enemy's fast X velocity
    rol
    rol
    and #$01                               ; #$01 when moving left, down-left, or up-left, #$00 otherwise
    tay
    lda ENEMY_X_POS,x                      ; load enemy's X position
    cmp overhead_soldier_screen_edge_tbl,y ; when walking leftward set carry when right of left edge
                                           ; or set carry when right of right edge
    iny
    sty $13                                ; #$02 when moving left, down-left, or up-left, #$01 otherwise
    ror                                    ; shift carry to bit 7
    eor ENEMY_X_VELOCITY_FAST,x
    bmi @collision_or_outside_turn_around  ; branch if past horizontal edge of screen and walking farther away
                                           ; i.e. walking leftward and left of left edge
                                           ; or walking right/down and right of right edge
                                           ; to skip collision check
    lda ENEMY_X_VELOCITY_FAST,x            ; not outside bounds and walking toward center
                                           ; load enemy's fast X velocity
    asl
    lda #$08
    ldy #$01
    bcc @test_x_collision                  ; branch if no leftward component to direction
    lda #$f8                               ; moving rightward, straight down, or straight up
                                           ; adjust collision x test point by -8
    iny

@test_x_collision:
    sty $13                        ; #$01 when moving rightward, #$00 otherwise
    clc                            ; clear carry in preparation for addition
    adc ENEMY_X_POS,x              ; add either 8 or -8 depending on direction
    ldy ENEMY_Y_POS,x              ; load enemy's Y position
    jsr get_bg_collision           ; get background collision code for position (a,y)
                                   ; seeing if have collision in horizontal direction
    beq @check_y_collision_dir_aim ; branch if no collision to check for vertical collision

; collision to left/right of soldier, or outside screen and walking farther out
; either turn soldier around, target player, or change direction to up or down
; input
;  * $13
;    * #$01 - collision to the right
;    * #$02 - collision to the left
@collision_or_outside_turn_around:
    lda ENEMY_VAR_3,x      ; see if previous frame collision detection was set
    bne @turn_around       ; turn around if previous frame encountered collision
                           ; and new perpendicular direction also has collision
    jmp @left_right_target ; no collision, set new target

@turn_around:
    jsr overhead_soldier_turn_around ; reached a corner, turn around
    jmp @anim_handle_fire            ; set sprite, apply velocity, advance routine

@check_y_collision_dir_aim:
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    ora ENEMY_Y_VELOCITY_FRACT,x
    beq @anim_or_target          ; branch if not moving vertically
    lda ENEMY_Y_VELOCITY_FAST,x  ; moving vertically, load enemy's fast Y velocity
    asl
    lda #$10                     ; assuming moving down (16)
    ldy #$03
    bcc @aim_check_collision     ; branch if moving down
    lda #$f8                     ; moving up (-8)
    iny

; check collision above/below player depending on vertical direction
@aim_check_collision:
    sty $13                          ; set direction of soldier
                                     ; #$03 - moving down, #$04 - moving up
    clc                              ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x                ; move soldier up or down depending on velocity
    cmp #$e8                         ; compare to bottom of screen
    bcs @anim_or_target              ; branch if at very bottom of screen
    tay                              ; not at bottom of screen,
    lda ENEMY_X_POS,x                ; load enemy's X position
    jsr get_bg_collision             ; get background collision code for position (a,y)
    beq @anim_or_target              ; branch if no collision in direction
    lda ENEMY_VAR_3,x                ; collision above/below player
    beq @up_down_collision
    jsr overhead_soldier_turn_around ; reached a corner, turn around
    jmp @anim_handle_fire            ; set sprite, apply velocity, advance routine

; at bottom of screen or no collision
; either animate, or re-target
@anim_or_target:
    ldy ENEMY_VAR_3,x
    bne @anim_handle_fire ; branch if have direction to set sprite, apply velocity, advance routine
    dec ENEMY_DELAY,x     ; no direction, decrement aim delay
    beq @aim_to_target    ; branch if delay elapsed
                          ; to determine target, aim toward target, set next targeting delay, update pos

; set overhead soldier sprite and sprite attribute, and apply velocity
; for red soldiers, also decrement firing delay and if delay elapsed, advance
; routine to actually fire at the player.
@anim_handle_fire:
    jsr overhead_soldier_animate ; set sprite and sprite attribute based on walking direction and animation frame index
    jsr apply_velocity           ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_ATTRIBUTES,x
    and #$02                     ; see if red overhead soldier (fires at player)
    beq @exit                    ; branch if green soldier
    dec ENEMY_FIRING_DELAY,x     ; red soldier, decrement firing delay
    bne @exit                    ; exit if firing delay hasn't elapsed
    lda #$20                     ; firing delay elapsed, reset it, and advance routine to fire
    sta ENEMY_FIRING_DELAY,x
    jmp advance_enemy_routine    ; advance to overhead_soldier_routine_02

@exit:
    rts

; vertical collision with bg, try to target player and set velocity
; if passed player (below and walking down, or above and walking up) aim horizontally left/right towards player
; compare to @left_right_target
; input
;  * $13 - overhead soldier direction
;    * #$03 - collision below overhead soldier (moving down)
;    * #$04 - collision above overhead soldier (moving up)
@up_down_collision:
    jsr overhead_soldier_get_target_pos ; set ($0b, $0c) to targeted player's (x,y) position or bottom of screen
    lda ENEMY_Y_POS,x                   ; load enemy's Y position
    cmp $0c                             ; compare to targeted player's Y position
    ror                                 ; move carry to bit 7
    eor ENEMY_Y_VELOCITY_FAST,x         ; set when moving up, set when below
    bmi @aim_to_target                  ; branch if soldier is moving up and below player
                                        ; or soldier is moving down and above the player
                                        ; to aim towards player
    lda ENEMY_X_POS,x                   ; already targeting player, just walk left/right
                                        ; load enemy's X position
    cmp $0b                             ; compare to player's X position
    lda #$00                            ; aim right (3 o'clock)
    bcc @set_vel_left_right
    lda #$01                            ; aim left (9 o'clock)

@set_vel_left_right:
    jmp @set_dir_vel ; set aim straight left or straight right

; horizontal collision, try to target player and set velocity
; if already targeting player and collision, then aim vertically up/down towards player
; compare to @up_down_collision
; input
;  * $13 - overhead soldier direction
;    * #$01 - collision to the right of overhead soldier (soldier moving right)
;    * #$02 - collision to the left of overhead soldier (soldier moving left)
@left_right_target:
    jsr overhead_soldier_get_target_pos ; set ($0b, $0c) to targeted player's (x,y) position or bottom of screen
    lda ENEMY_X_POS,x                   ; load enemy's X position
    cmp $0b                             ; compare soldier X position to player X position
    ror                                 ; move carry to bit 7
    eor ENEMY_X_VELOCITY_FAST,x
    bmi @aim_to_target                  ; branch if soldier is moving left and to the right of player
                                        ; or soldier is moving right and to the left of the player
    lda ENEMY_Y_POS,x                   ; already targeting player, just walk up/down
                                        ; load enemy's Y position
    cmp $0c                             ; compare to player's Y position
    lda #$02                            ; aim down (6 o'clock)
    bcc @set_vel_up_down
    lda #$03                            ; aim up (12 o'clock)

@set_vel_up_down:
    jmp @set_dir_vel ; set aim straight up or straight down

; determine target, aim toward target, set next targeting delay, update position
; checks of soldier passed player and if so, updates ENEMY_VAR_4,x appropriately
; input
;  * ENEMY_VAR_4,x - player index of player to target (0 = p1, 1 = p2)
;    when negative, target bottom of screen
@aim_to_target:
    jsr overhead_soldier_get_target_pos ; set ($0b, $0c) to targeted player's (x,y) position or bottom of screen
    lda ENEMY_Y_POS,x                   ; load enemy's Y position
    cmp $0c                             ; compare to targeted Y position
    bcc @above_player                   ; branch if above target Y position
    lda ENEMY_VAR_4,x                   ; soldier is below targeted Y position
    asl
    bmi @set_delay_aim                  ; branch if soldier walking down and below player
    lda #$80                            ; soldier just walked below player, mark ENEMY_VAR_4 negative
                                        ; so subsequent calls will target bottom of screen
    bne @set_target_delay_aim           ; always branch to specify passed player

@above_player:
    lda ENEMY_VAR_4,x ; load player index to target
    and #$bf          ; strip bit 6 (walking up flag)
                      ; if the soldier is walking up, this changes their behavior
                      ; and has them act like a soldier that walks down

@set_target_delay_aim:
    sta ENEMY_VAR_4,x ; set player index to target
                      ; when negative, target bottom of screen

@set_delay_aim:
    lda FRAME_COUNTER                ; load frame counter
    sbc RANDOM_NUM
    sta RANDOM_NUM                   ; randomize random number
    lda RANDOM_NUM                   ; load random number
    and #$07
    tay
    lda overhead_soldier_delay_tbl,y
    sta ENEMY_DELAY,x                ; set random delay
    jsr overhead_soldier_target      ; update ENEMY_VAR_2 with new aim direction based on targeted position
                                     ; either targeted player position, or random spot at bottom of screen
    jmp update_enemy_pos             ; adjust position based on scroll (does not apply velocity)

; collided with bg, set new direction a, and indicate where collision was in $13
; input
;  * a - aim direction
;  * $13 - direction soldier was walking when encountered collision and collision direction
;    * #$01 - overhead soldier is moving rightward (collision to right)
;    * #$02 - overhead soldier is moving leftward (collision to left)
;    * #$03 - overhead soldier is moving down (collision below)
;    * #$04 - overhead soldier is moving up (collision above)
@set_dir_vel:
    tay
    lda overhead_soldier_aim_dir_tbl,y          ; load restricted aim direction
    sta ENEMY_VAR_1,x                           ; set aim direction (3, 9, 6, or 12 o'clock)
    lda overhead_soldier_overhead_aim_dir_tbl,y
    sta ENEMY_VAR_2,x                           ; set overhead aim direction (3, 9, 6, or 12 o'clock)
    lda $13
    sta ENEMY_VAR_3,x                           ; set previous direction before encountering collision
    jsr overhead_soldier_set_vel
    jmp update_enemy_pos

; adjustment from player Y position for use when calculating bg collision
overhead_soldier_y_col_tbl:
    .byte $00 ; ENEMY_VAR_3,x = #$01 (3 o'clock)
    .byte $00 ; ENEMY_VAR_3,x = #$02 (9 o'clock)
    .byte $10 ; ENEMY_VAR_3,x = #$03 (6 o'clock)
    .byte $f8 ; ENEMY_VAR_3,x = #$04 (12 o'clock)

; adjustment from player X position for use when calculating bg collision
overhead_soldier_x_col_tbl:
    .byte $08 ; ENEMY_VAR_3,x = #$01 (3 o'clock)
    .byte $f8 ; ENEMY_VAR_3,x = #$02 (9 o'clock)
    .byte $00 ; ENEMY_VAR_3,x = #$03 (6 o'clock)
    .byte $00 ; ENEMY_VAR_3,x = #$04 (12 o'clock)

overhead_soldier_aim_dir_tbl:
    .byte $00 ; ENEMY_VAR_3,x = #$01 (3 o'clock)
    .byte $0c ; ENEMY_VAR_3,x = #$02 (9 o'clock)
    .byte $06 ; ENEMY_VAR_3,x = #$03 (6 o'clock)
    .byte $12 ; ENEMY_VAR_3,x = #$04 (12 o'clock)

overhead_soldier_overhead_aim_dir_tbl:
    .byte $00 ; 3 o'clock
    .byte $04 ; 9 o'clock
    .byte $02 ; 6 o'clock
    .byte $06 ; 12 o'clock

overhead_soldier_screen_edge_tbl:
    .byte $f0 ; right edge, moving right
    .byte $10 ; left edge, moving left

overhead_soldier_delay_tbl:
    .byte $1a,$08,$18,$20,$02,$1c,$05,$0e

overhead_soldier_set_firing_sprite:
    lda #$03
    sta ENEMY_FRAME,x
    bne overhead_soldier_set_sprite

; set overhead soldier sprite and sprite attribute based on walking direction
; and animation frame index
overhead_soldier_animate:
    inc ENEMY_ANIMATION_DELAY,x
    lda ENEMY_ANIMATION_DELAY,x
    cmp #$08
    bcc @continue
    lda #$00                    ; animation delay at max, move to next frame
    sta ENEMY_ANIMATION_DELAY,x
    inc ENEMY_FRAME,x
    lda ENEMY_FRAME,x
    and #$03                    ; strip so frames cycle every 4 frames
    sta ENEMY_FRAME,x

@continue:
    ldy ENEMY_VAR_2,x ; load aim direction to know which sprite attribute to use

overhead_soldier_set_sprite:
    lda ENEMY_ATTRIBUTES,x
    lsr
    lsr
    lda #$01               ; sprite palette (red)
    bcs @set_and_exit      ; compare to sprite palette bit (0 = green, 1 = red)
    lda #$03               ; sprite palette (green)

@set_and_exit:
    ora overhead_soldier_sprite_attr_tbl,y ; merge with sprite flip based on direction
    sty $08
    jsr set_overhead_enemy_sprite_attr     ; set enemy sprite attribute including recoil based on ENEMY_VAR_5 timer and a register
    lda $08
    asl
    asl
    clc                                    ; clear carry in preparation for addition
    adc ENEMY_FRAME,x
    tay                                    ; determine offset based on aim direction and enemy frame
    lda overhead_solider_sprite_tbl,y      ; load sprite based on direction and frame
    sta ENEMY_SPRITE,x                     ; set overhead soldier sprite
    rts

overhead_soldier_sprite_attr_tbl:
    .byte $40,$40,$00,$00,$00,$00,$00,$40

; sprite_3c, sprite_3d, sprite_3e, sprite_3f
; sprite_40, sprite_41, sprite_42, sprite_43,
; sprite_44, sprite_45, sprite_46, sprite_47
; sprite_48, sprite_49, sprite_4a
overhead_solider_sprite_tbl:
    .byte $45,$46,$47,$46 ; ENEMY_VAR_2 = #$00 (3 o'clock) - walking right
    .byte $3f,$40,$41,$40 ; ENEMY_VAR_2 = #$01 (4:30 o'clock) - walking down right
    .byte $3c,$3d,$3e,$3d ; ENEMY_VAR_2 = #$02 (6 o'clock) - walking down
    .byte $3f,$40,$41,$40 ; ENEMY_VAR_2 = #$03 (7:30 o'clock) - walking down left
    .byte $45,$46,$47,$46 ; ENEMY_VAR_2 = #$04 (9 o'clock) - walking left
    .byte $42,$43,$44,$43 ; ENEMY_VAR_2 = #$05 (10:30 o'clock) - walking up left
    .byte $48,$49,$4a,$49 ; ENEMY_VAR_2 = #$06 (12:00 o'clock) - walking up
    .byte $42,$43,$44,$43 ; ENEMY_VAR_2 = #$07 (1:30 o'clock) - walking up right

; reached a corner, turn around
; set new aim direction and overhead aim direction
overhead_soldier_turn_around:
    lda ENEMY_VAR_1,x
    clc               ; clear carry in preparation for addition
    adc #$0c
    cmp #$18
    bcc @continue
    sbc #$18

@continue:
    sta ENEMY_VAR_1,x             ; set new direction
    lda ENEMY_VAR_2,x
    clc                           ; clear carry in preparation for addition
    adc #$04
    cmp #$08
    bcc @set_overhead_aim_and_vel
    sbc #$08

@set_overhead_aim_and_vel:
    sta ENEMY_VAR_2,x            ; set new overhead direction
    jmp overhead_soldier_set_vel ; set the X and Y velocity based on overhead aim direction

; calculate closest player to target in $0a
; calculates ($0b,$0c) as random target for when no player to target
; input
;  * x - enemy slot index
;  * ENEMY_VAR_4,x - suggested player index to target
; output
;  * $0a - player index to target, 0 = player 1, 1 = player 2, #$ff = no player
;  * ($0b,$0c) - random position to aim towards at bottom of screen.  used when
;    soldier has walked below all players ($0a is negative)
;  * ENEMY_VAR_4,x - player index to target
overhead_soldier_get_target:
    jsr overhead_soldier_aim_bottom ; load random ($0b, $0c) position at bottom of screen
    lda ENEMY_VAR_4,x               ; load player index of player to target
    bmi @set_target_exit            ; branch if not targeting a player
    jsr player_enemy_x_dist         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    sty $0a                         ; set player index to target, 0 = player 1, 1 = player 2
    lda ENEMY_VAR_4,x
    and #$fe                        ; strip targeted player bit
    ora $0a
    sta ENEMY_VAR_4,x               ; set player index to target, 0 = player 1, 1 = player 2

@set_target_exit:
    sta $0a ; set player index to target, 0 = player 1, 1 = player 2
    rts

; set ($0b, $0c) to targeted player's (x,y) position or bottom of screen
; input
;  * x - enemy slot index
;  * ENEMY_VAR_4,x - player index of player to target (0 = p1, 1 = p2)
;    when negative, target bottom of screen
; output
;  * $0b - targeted X position
;  * $0c - targeted Y position
overhead_soldier_get_target_pos:
    jsr overhead_soldier_aim_bottom ; load random ($0b, $0c) position at bottom of screen
    lda ENEMY_VAR_4,x               ; load player index to target, 0 = player 1, 1 = player 2
    bmi @exit                       ; exit if ENEMY_VAR_4,x is negative to use random position at bottom of screen
    and #$01                        ; not targeting bottom of screen, strip to target player index
    tay
    lda PLAYER_SPRITE_Y_POS,y
    sta $0c                         ; set target player's Y position
    lda PLAYER_SPRITE_X_POS,y
    sta $0b                         ; set target player's X position

@exit:
    rts

; set random ($0b, $0c) position at bottom of screen
; output
;  * $0b - random X position to target
;  * $0c - bottom of screen (#$ff)
overhead_soldier_aim_bottom:
    lda RANDOM_NUM                  ; load random number
    and #$03                        ; strip to #$00 to #$03 (inclusive)
    tay
    lda overhead_target_x_pos_tbl,y
    sta $0b                         ; set random X position to aim towards
    lda #$ff
    sta $0c                         ; aim towards bottom of screen
    rts

overhead_target_x_pos_tbl:
    .byte $20 ; 12.5%
    .byte $60 ; 37.5%
    .byte $a0 ; 62.5%
    .byte $e0 ; 87.5%

; red marching overhead soldier whose firing delay has elapsed, fire at player
overhead_soldier_routine_02:
    lda ENEMY_VAR_4,x       ; load player index of player to target
    bpl @continue
    jsr player_enemy_x_dist ; target player value is negative, target closest player instead of specified player
                            ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                     ; transfer closest player index to a

@continue:
    sta $0a                                ; set player index of player to target
    pha                                    ; backup player index of player to target
    jsr copy_enemy_vars_to_zp              ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_01                      ; set enemy aim direction ($0c) and rotation direction (a) targeting player $0a using quadrant_aim_dir_01
                                           ; if no player to target, use $(0b, $0c)
    ldy $0c                                ; load aim direction
    lda dir_to_overhead_dir_tbl,y          ; convert to overhead aim direction
    tay
    sta $17                                ; save overhead aim direction in $17 for overhead_soldier_adj_bullet_pos
    jsr overhead_soldier_set_firing_sprite ; set sprite to be the firing sprite
    pla                                    ; restore player index of player to target
    sta $0a                                ; store player index of player to target
    jsr update_enemy_pos                   ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x               ; decrement red soldier firing delay
    beq @set_random_firing_delay           ; branch if delay elapsed to set new random firing delay
                                           ; set routine to overhead_soldier_routine_01
    lda ENEMY_FIRING_DELAY,x               ; load firing delay
    cmp #$10
    bne @exit                              ; exit if not at #$10
    jsr copy_enemy_vars_to_zp              ; firing delay at #$10, fire at player $0a
                                           ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                               ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player              ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr overhead_soldier_adj_bullet_pos
    lda #$08
    sta ENEMY_FIRING_DELAY,x               ; set firing delay to #$08

@exit:
    rts

; set random firing delay and set routine to overhead_soldier_routine_01
@set_random_firing_delay:
    lda RANDOM_NUM           ; load random number
    adc FRAME_COUNTER
    sta RANDOM_NUM           ; randomize random number
    and #$1f
    adc #$70
    sta ENEMY_FIRING_DELAY,x ; set random firing delay
    lda #$02
    jmp set_enemy_routine    ; set routine to overhead_soldier_routine_01

; !(UNUSED)
bank_a_unused_01:
    jmp set_enemy_destroy_dir_and_vel ; set enemy destroyed X velocity to +/-.375 and Y velocity to -3.5

; !(UNUSED)
bank_a_unused_02:
    jmp apply_destroy_vel_adv_routine_after_delay

; input
;  * x - enemy slot index
;  * $11 - created bullet enemy slot
;  * $17 - overhead soldier aim direction
;  * zero flag - set when bullet created, clear when unable to create
overhead_soldier_adj_bullet_pos:
    bne overhead_soldier_exit ; exit if bullet was not created
    ldy $17                   ; bullet created, load aim direction

; input
;  * y - enemy aim direction
;  * x - enemy slot index
;  * $11 - created bullet enemy slot
;  * $17 - overhead soldier aim direction
red_stationary_soldier_adj_bullet_pos:
    lda #$06
    sta ENEMY_VAR_5,x               ; set sprite recoil delay timer
    ldx $11                         ; load the created bullet enemy slot index
    lda ENEMY_Y_POS,x               ; load bullet's Y position
    clc                             ; clear carry in preparation for addition
    adc overhead_bullet_y_adj_tbl,y ; add adjustment based on soldier's aim direction
    sta ENEMY_Y_POS,x               ; set bullet's new Y position
    lda ENEMY_X_POS,x               ; load bullet's X position
    clc                             ; clear carry in preparation for addition
    adc overhead_bullet_x_adj_tbl,y ; add adjustment based on soldier's aim direction
    sta ENEMY_X_POS,x               ; set bullet's new X position
    ldx ENEMY_CURRENT_SLOT          ; restore x to overhead soldier's enemy slot

overhead_soldier_exit:
    rts

overhead_bullet_y_adj_tbl:
    .byte $fc,$03,$04,$03,$fc,$f3,$f0,$f3

overhead_bullet_x_adj_tbl:
    .byte $08,$08,$fd,$f8,$f8,$f8,$03,$08

; sets overhead enemy's sprite attribute to a, including recoil based on
; ENEMY_VAR_5 timer.  used by overhead tank soldier (#$2e), tank gunner (#$6a),
; overhead stationary red soldier (#$5c), and overhead soldier (#$63)
; input
;  * a - sprite attribute value
set_overhead_enemy_sprite_attr:
    ldy ENEMY_VAR_5,x         ; load red soldier firing sprite recoil delay timer
    beq @set_sprite_attr_exit ; branch to continue if no sprite recoil delay timer
    dec ENEMY_VAR_5,x         ; decrement recoil delay timer
    ora #$04                  ; set recoil effect

@set_sprite_attr_exit:
    sta ENEMY_SPRITE_ATTR,x
    rts

; enemy type #$69
tank_boss_routine_ptr_tbl:
    .addr tank_boss_routine_00          ; disable player enemy collision, set destroy flags, set hp, advance routine
    .addr tank_boss_routine_01
    .addr tank_boss_routine_02
    .addr tank_boss_routine_03          ; draws floor tiles, create gunners and electrode enemies
    .addr tank_boss_routine_04
    .addr tank_boss_routine_05          ; enemy destroyed routine
    .addr tank_boss_routine_06
    .addr tank_boss_routine_07          ; remove scanline interrupts, hide enemy, play destroyed sound, and disable collisions
    .addr bg_enemy_explosion_routine_01 ; animate sequence of explosions
    .addr tank_boss_routine_09          ; set mirroring, set boss defeated flag, and remove enemy

; disable player enemy collision, set destroy flags, set hp, advance routine
tank_boss_routine_00:
    lda #$91
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision, disable bullet collision, spike explosion, sound_26 (B OUT)
    lda #$20                  ; HP = #$20, #$30, or #$38
    jsr set_enemy_hp_hard     ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    jmp advance_enemy_routine ; advance to next routine

tank_boss_routine_01:
    lda #$4e
    sta ENEMY_SPRITE,x         ; sprite_4e
    lda Y_SCROLL               ; load PPU vertical scroll
    bne tank_boss_routine_exit ; exit if scroll isn't complete
    lda #$00
    sta NT_CURRENT_COLUMN
    .ifdef Probotector
        lda #$03
        sta DRAW_X_SCREEN
        lda #$02
        sta SCREENS_DRAWN      ; set boss screen
    .endif
    lda #$02
    sta SCREEN_SCROLL_TYPE
    lda #$08
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine  ; advance to next routine

tank_boss_routine_02:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne tank_boss_routine_exit
    dec ENEMY_VAR_1,x
    beq tank_boss_reset
    lda #$00
    sta X_SCROLL_DRAW_POINT

tank_boss_routine_exit:
    rts

tank_boss_reset:
    lda #$01
    sta NT_MIRRORING                     ; set nametable mirroring (0: vertical; 1: horizontal)
    lda #$80
    sta ENEMY_X_POS,x
    lda #$88
    sta ENEMY_Y_POS,x
    lda #$ff
    sta ENEMY_VAR_6,x
    lda #$0c
    sta IRQ_TYPE                         ; set irq routine type to irq_handler_0c_ptr_tbl
                                         ; level 2 boss screen
    jsr tank_boss_set_irqs
    jsr init_bg_boss_pre_irq_max_scrolls ; init boss screen pre-irq scroll max values
    lda #$00
    sta ENEMY_VAR_6,x
    lda #$62
    sta ENEMY_Y_POS,x
    lda #$10
    sta ENEMY_DESTROY_ATTRS,x            ; enable collision, set destroy sound to sound_26 (B OUT) - boss destroy
    jsr tank_boss_set_scroll_nt_irqs
    lda #$31                             ; sound_31 (BOSS BGM) - Ruined Base
    jsr play_sound                       ; play boss background music
    lda #$05
    sta ENEMY_VAR_1,x                    ; initialize drawing count (tank_boss_floor_tiles_tbl offset)
                                         ; to update attribute for floor tiles
    jmp advance_enemy_routine            ; advance to next routine

; draws floor tiles, create gunners and electrode enemies
tank_boss_routine_03:
    .ifdef Superc
        ldy #$03
        lda #$55

    @loop:
        sta ATTRIBUTE_OVERRIDES+6,y      ; replace supertile indexes #$ec,#$ed,#$ee,#$ef attribute bytes
                                         ; with #$55
        dey
        bpl @loop
    .endif
    lda ENEMY_VAR_1,x                    ; load which floor tiles to draw
    asl
    asl                                  ; quadruple since each entry is #$04 bytes
    tay                                  ; transfer to offset register
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$03                             ; repeat mode
    sta CPU_GRAPHICS_BUFFER,x            ; set repeat mode
    inx                                  ; increment graphics buffer write offset
    lda tank_boss_floor_tiles_tbl+1,y    ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x            ; set PPU address low byte
    inx                                  ; increment graphics buffer write offset
    lda tank_boss_floor_tiles_tbl,y      ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x            ; set PPU address high byte
    inx                                  ; increment graphics buffer write offset
    lda tank_boss_floor_tiles_tbl+2,y    ; load number of repetitions
    sta CPU_GRAPHICS_BUFFER,x            ; set number of repetitions
    inx                                  ; increment graphics buffer write offset
    lda tank_boss_floor_tiles_tbl+3,y    ; load pattern tile (or attribute byte) to repeatedly write to PPU
    sta CPU_GRAPHICS_BUFFER,x            ; set graphic byte to write repeatedly
    inx                                  ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET           ; update graphics buffer write offset
    ldx ENEMY_CURRENT_SLOT               ; restore enemy current slot
    jsr tank_boss_set_vel_scroll_nt_irqs
    dec ENEMY_VAR_1,x                    ; decrement floor tile draw offset
    bpl @exit
    lda #$00
    ldy #$2f

@collision_update_loop:
    sta BG_COLLISION_DATA+24,y
    sta SECOND_BG_COLLISION_DATA+24,y
    dey
    bpl @collision_update_loop
    lda #$87
    sta ENEMY_FIRING_DELAY,x          ; set timer before changing direction from left to down
    lda #$06
    sta ENEMY_VAR_1,x
    lda #$03
    sta ENEMY_VAR_2,x                 ; set initial aim direction to down right
    lda #$00
    sta ENEMY_VAR_4,x                 ; initialize tank boss subroutine index to tank_boss_subroutine_00
    lda #$f0
    sta ENEMY_ANIMATION_DELAY,x
    ldy #$03                          ; initialize enemy index loop counter

@create_boss_enemies_loop:
    sty $08                            ; store enemy attributes in $08
    lda tank_boss_enemy_tbl,y          ; level 2, enemy type = tank boss electrode or tank boss tank gunner
    tay                                ; transfer enemy type to y
    jsr try_create_enemy_from_existing ; create tank boss electrode or tank boss tank gunner
    bcc @adv_routine_exit              ; branch if unable to create tank boss tank gunner
    ldy $11                            ; load slot of created enemy
    lda $08                            ; load enemy creation offset
    sta ENEMY_ATTRIBUTES,y             ; set enemy attribute based on loop counter
                                       ; used by tank gunner to distinguish left and right gunner
                                       ; used by electrode to distinguish shaft vs probe
    txa                                ; transfer tank boss enemy slot to a
    sta ENEMY_VAR_3,y                  ; set tank boss enemy slot for use by created enemy
    ldy $08                            ; load enemy creation loop counter
    dey                                ; move to next enemy to create
    bpl @create_boss_enemies_loop      ; brach if more enemies to create

@adv_routine_exit:
    jmp advance_enemy_routine ; advance to next routine

@exit:
    rts

tank_boss_enemy_tbl:
    .byte $6a,$6a,$36,$36

; tank boss tiles, set palette and draw floor tiles over tank
tank_boss_floor_tiles_tbl:
    .byte $a8,$20,$58,$08 ; PPU address $20a8, repeat tile #$08 #$58 times
    .byte $08,$21,$58,$08 ; PPU address $2108, repeat tile #$08 #$58 times
    .byte $68,$21,$58,$08 ; PPU address $2168, repeat tile #$08 #$58 times
    .byte $c8,$21,$58,$08 ; PPU address $21c8, repeat tile #$08 #$58 times
    .byte $28,$22,$58,$08 ; PPU address $2228, repeat tile #$08 #$58 times
    .byte $ca,$23,$1e,$55 ; PPU address $23ca (attribute table), repeat palette #$55 #$1e times

tank_boss_routine_04:
    dec ENEMY_ANIMATION_DELAY,x
    beq @adv_frame
    lda ENEMY_FRAME,x
    lsr
    bcc @target_player
    lsr
    bcs @retract_probe
    lda ENEMY_ANIMATION_DELAY,x
    lsr
    bcc @target_player
    inc ENEMY_VAR_5,x           ; extend tank boss electrode by 1
    lda ENEMY_VAR_5,x           ; load how much the electrode is extended
    cmp #$10
    bcc @target_player
    bcs @adv_frame              ; always branch

@retract_probe:
    lda ENEMY_ANIMATION_DELAY,x
    lsr
    bcc @target_player
    dec ENEMY_VAR_5,x           ; retract the tank boss electrode by 1
    bne @target_player

@adv_frame:
    inc ENEMY_FRAME,x              ; further advance probe
    lda ENEMY_FRAME,x
    and #$03
    sta ENEMY_FRAME,x              ; set max frame of #$03 (retracting)
    tay
    lda tank_boss_anim_delay_tbl,y
    sta ENEMY_ANIMATION_DELAY,x

@target_player:
    jsr player_enemy_x_dist                 ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                                     ; transfer closest player index to a
    sta ENEMY_VAR_3,x                       ; set to closest player index
    jsr overhead_quad_aim_dir_01            ; determine next aim direction to get closer to that value
                                            ; then use that value to update the overhead aim direction ENEMY_VAR_2,x
    ldy ENEMY_VAR_2,x                       ; load overhead aim direction
    lda tank_boss_soldier_sprite_tbl,y
    sta ENEMY_SPRITE,x
    lda tank_boss_soldier_sprite_attr_tbl,y
    sta ENEMY_SPRITE_ATTR,x
    jsr run_tank_boss_subroutine
    jsr tank_boss_set_vel_scroll_nt_irqs
    dec ENEMY_FIRING_DELAY,x                ; decrement tank change direction timer
    bne @exit                               ; exit if tank should keep going in same direction
    lda ENEMY_VAR_2,x                       ; change direction, load overhead aim direction
    cmp #$06
    beq @set_firing_delay_exit              ; branch if aiming up
    lda ENEMY_VAR_3,x                       ; not aiming up, load closest player
    sta $0a                                 ;  set the closest player to the enemy (0 = p1, 1 = p2)
    jsr copy_enemy_vars_to_zp               ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                                ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player               ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr tank_boss_set_bullet_pos            ; set created bullet initial position based on aim direction

@set_firing_delay_exit:
    lda #$43
    sta ENEMY_FIRING_DELAY,x ; set firing delay

@exit:
    rts

; sprite_50, sprite_4f, sprite_4e, sprite_4f
tank_boss_soldier_sprite_tbl:
    .byte $50,$4f,$4e,$4f,$50,$50,$50,$50

tank_boss_soldier_sprite_attr_tbl:
    .byte $43,$43,$03,$03,$03,$03,$03,$43

tank_boss_anim_delay_tbl:
    .byte $80,$00,$80,$00

; enemy destroyed routine
tank_boss_routine_05:
    jsr tank_boss_set_scroll_nt_irqs
    jmp enemy_routine_boss_defeated_00

tank_boss_routine_06:
    jsr tank_boss_set_scroll_nt_irqs
    jmp enemy_routine_boss_defeated_01

; remove scanline interrupts, hide enemy, play destroyed sound, and disable collisions
tank_boss_routine_07:
    jsr set_nmi_noop_irq              ; remove any scanline interrupts
    jmp bg_enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

tank_boss_routine_09:
    lda #$00
    sta NT_MIRRORING                   ; set nametable mirroring (0: vertical; 1: horizontal)
    .ifdef Probotector
        sta X_SCREEN
    .endif
    jmp set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy

; set created bullet initial position based on aim direction
; input
;  * $11 - created bullet enemy slot
;  * x - enemy slot index of tank boss
;  * zero flag - set when bullet created, clear when unable to create
tank_boss_set_bullet_pos:
    bne @exit                            ; exit if unable to create a bullet
    ldy ENEMY_VAR_2,x                    ; load overhead aim direction (#$04 = left, #$02 = down, #$00 = right)
    ldx $11                              ; load enemy slot of created bullet
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    clc                                  ; clear carry in preparation for addition
    adc tank_boss_bullet_y_pos_adj_tbl,y ; load adjustment from tank boss based on aim direction
    sta ENEMY_Y_POS,x                    ; set bullet's initial y position
    lda ENEMY_X_POS,x                    ; load enemy's X position
    clc                                  ; clear carry in preparation for addition
    adc tank_boss_bullet_x_pos_adj_tbl,y
    sta ENEMY_X_POS,x
    ldx ENEMY_CURRENT_SLOT

@exit:
    rts

tank_boss_bullet_y_pos_adj_tbl:
    .byte $ec ; direction #$00 (3 o'clock) = -20
    .byte $f6 ; direction #$01 (4:30 o'clock) = -10
    .byte $f8 ; direction #$02 (6 o'clock) = -8
    .byte $f6 ; direction #$03 (7:30 o'clock) = -10
    .byte $ec ; direction #$04 (9:00 o'clock) = -20
    .byte $ec ; (!UNUSED) direction #$05 (10:30 o'clock) = -20
    .byte $ec ; (!UNUSED) direction #$06 (12:00 o'clock) = -20
    .byte $ec ; (!UNUSED) direction #$07 (1:30 o'clock) = -20

tank_boss_bullet_x_pos_adj_tbl:
    .byte $0e ; direction #$00 (3 o'clock) = 14
    .byte $0c ; direction #$01 (4:30 o'clock) = 12
    .byte $00 ; direction #$02 (6 o'clock) = 0
    .byte $f4 ; direction #$03 (7:30 o'clock) = -12
    .byte $f2 ; direction #$04 (9:00 o'clock) = -14
    .byte $f2 ; (!UNUSED) direction #$05 (10:30 o'clock) = -14
    .byte $00 ; (!UNUSED) direction #$06 (12:00 o'clock) = 0
    .byte $0e ; (!UNUSED) direction #$07 (1:30 o'clock) = 14

tank_boss_set_vel_scroll_nt_irqs:
    jsr bg_boss_apply_vel

tank_boss_set_scroll_nt_irqs:
    jsr set_bg_boss_scroll_nt

tank_boss_set_irqs:
    lda IRQ_Y_SCROLL         ; load vertical scroll used for tank boss
    and #$07                 ; for bg enemies, fine grain scrolling happens by adjusting the irqs
    sta SCANLINE_IRQ_2_DIFF
    .ifdef Probotector
        lda #$1a
    .else
        lda #$27
    .endif
    sec                      ; set carry flag in preparation for subtraction
    sbc SCANLINE_IRQ_2_DIFF  ; subtract any fine-grain scrolling
    sta SCANLINE_IRQ_1       ; set where first scanline interrupt occurs
                             ; anywhere between #$27 scanlines from the top down to #$20
    inc SCANLINE_IRQ_2_DIFF  ; set small empty region above the tank boss
                             ; that changes in heigh for when the tank boss is moving vertically
    lda #$97
    sta SCANLINE_IRQ_3_DIFF  ; set height of main region where tank boss is (always #$97)
    ldy #$08
    lda IRQ_PPUCTRL_SETTINGS
    and #$02
    beq @continue            ; branch if using nametable $2000
    ldy #$0b

@continue:
    sty IRQ_PPUADDR+1
    lda IRQ_Y_SCROLL
    and #$f8          ; strip any fine grain scrolling (bits 0, 1, and 2)
                      ; that's handled by height of region between 1st scanline irq and 2nd scanline irq
    asl
    rol IRQ_PPUADDR+1
    asl
    rol IRQ_PPUADDR+1
    sta IRQ_PPUADDR   ; set PPU address for tank boss region based on vertical scroll
    rts

; sets the tank boss velocity based on ENEMY_VAR_4,x
; input
;  * x - enemy slot index
tank_boss_set_vel:
    lda ENEMY_VAR_4,x
    asl
    asl                          ; quadruple since each entry is #$04 bytes
    tay                          ; transfer to offset register
    lda tank_boss_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda tank_boss_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda tank_boss_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda tank_boss_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    rts

tank_boss_vel_tbl:
    .byte $00,$00,$00,$00 ; X velocity =  0.00, Y velocity =  0.00
    .byte $00,$00,$60,$00 ; X velocity =  0.00, Y velocity =  0.375
    .byte $40,$00,$00,$00 ; X velocity =  0.25, Y velocity =  0.00
    .byte $c0,$ff,$00,$00 ; X velocity = -0.25, Y velocity =  0.00

run_tank_boss_subroutine:
    lda ENEMY_VAR_4,x
    jsr run_routine_from_tbl_below

tank_boss_subroutine_ptr_tbl:
    .addr tank_boss_subroutine_00
    .addr tank_boss_subroutine_01
    .addr tank_boss_subroutine_02
    .addr tank_boss_subroutine_03

tank_boss_subroutine_00:
    inc ENEMY_VAR_4,x
    jsr tank_boss_set_vel
    lda ENEMY_X_POS,x             ; load enemy's X position
    bpl tank_boss_subroutine_exit
    jmp flip_enemy_x_dir          ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

; move horizontally checking for horizontal boundary
tank_boss_subroutine_01:
    lda ENEMY_X_VELOCITY_FAST,x    ; load enemy's fast X velocity
    rol
    rol
    and #$01
    tay
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp tank_boss_x_boundary_tbl,y
    ror
    eor ENEMY_X_VELOCITY_FAST,x
    bpl tank_boss_subroutine_exit
    lda #$20
    sta ENEMY_DELAY,x

tank_boss_subroutine_adv_routine:
    inc ENEMY_VAR_4,x
    jmp tank_boss_set_vel

tank_boss_subroutine_02:
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    cmp #$88
    bcs tank_boss_subroutine_adv_routine
    dec ENEMY_DELAY,x
    bne tank_boss_subroutine_exit
    adc #$20
    sta $08
    ldy PLAYER_GAME_OVER_STATUS          ; load p1 game over status (0 = not game over, 1 = game over)
    bne @continue
    cmp PLAYER_SPRITE_Y_POS
    bcc tank_boss_set_subroutine_00      ; set subroutine to tank_boss_subroutine_00

@continue:
    ldy PLAYER_GAME_OVER_STATUS+1        ; load p2 game over status (0 = not game over, 1 = game over)
    bne tank_boss_subroutine_adv_routine
    cmp PLAYER_SPRITE_Y_POS+1
    bcs tank_boss_subroutine_adv_routine

tank_boss_set_subroutine_00:
    lda #$00
    sta ENEMY_VAR_4,x     ; set subroutine to tank_boss_subroutine_00
    jmp tank_boss_set_vel

tank_boss_subroutine_03:
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$60
    bcc tank_boss_set_subroutine_00 ; set subroutine to tank_boss_subroutine_00

tank_boss_subroutine_exit:
    rts

tank_boss_x_boundary_tbl:
    .byte $c0,$40

; enemy type #$6a
tank_gunner_routine_ptr_tbl:
    .addr tank_gunner_routine_00
    .addr tank_gunner_routine_01
    .addr tank_gunner_routine_02 ; enemy destroyed routine
    .addr tank_gunner_routine_03
    .addr tank_gunner_routine_04
    .addr tank_gunner_routine_05
    .addr tank_gunner_routine_06

tank_gunner_routine_00:
    lda #$10                         ; HP = #$10, #$14, or #$17
    jsr set_enemy_hp                 ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$06
    sta ENEMY_VAR_1,x                ; set initial aim direction to down
    lda #$03
    sta ENEMY_VAR_2,x                ; set initial overhead aim direction
    ldy ENEMY_ATTRIBUTES,x           ; load enemy attributes, which specify the firing delay index
    lda tank_gunner_fire_delay_tbl,y
    sta ENEMY_FIRING_DELAY,x         ; set firing delay
    lda #$01
    jmp set_delay_adv_enemy_routine  ; set delay to #$01 and set routine to tank_gunner_routine_01

tank_gunner_fire_delay_tbl:
    .byte $27,$35

tank_gunner_routine_01:
    jsr player_enemy_x_dist            ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                                ; transfer closest player index to a
    sta ENEMY_VAR_4,x
    jsr overhead_quad_aim_dir_01       ; determine next aim direction to get closer to that value
                                       ; then use that value to update the overhead aim direction ENEMY_VAR_2,x
    ldy ENEMY_VAR_2,x                  ; load overhead enemy aim direction
    lda tank_gunner_sprite_tbl,y       ; load appropriate sprite based on overhead aim direction
    sta ENEMY_SPRITE,x
    lda tank_gunner_sprite_attr_tbl,y
    jsr set_overhead_enemy_sprite_attr ; set enemy sprite attribute including recoil based on ENEMY_VAR_5 timer and a register
    jsr tank_gunner_set_pos
    dec ENEMY_FIRING_DELAY,x           ; decrement firing delay
    bne @exit                          ; exit if firing delay hasn't elapsed
    lda ENEMY_VAR_2,x                  ; load overhead aim direction
    cmp #$06
    beq @set_fire_delay                ; branch if aiming up, don't fire
    lda ENEMY_VAR_4,x                  ; load player index of player to target
    sta $0a                            ; set targeted player index
    jsr copy_enemy_vars_to_zp          ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                           ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player          ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr tank_gunner_set_bullet_pos     ; set gunner recoil timer and created bullet's initial position

@set_fire_delay:
    lda #$55
    sta ENEMY_FIRING_DELAY,x

@exit:
    rts

tank_gunner_sprite_tbl:
    .byte $4d,$4c,$4b,$4c,$4d,$4d,$4d,$4d

tank_gunner_sprite_attr_tbl:
    .byte $41,$41,$01,$01,$01,$01,$01,$41

; enemy destroyed routine
tank_gunner_routine_02:
    jsr tank_gunner_set_pos
    jmp enemy_routine_init_explosions ; configure variables for 5 explosions
                                      ; will happen in enemy_routine_explosions

tank_gunner_routine_03:
    jsr tank_gunner_set_pos
    jmp enemy_routine_explosions ; generate 5 explosions, with an #$08 frame delay between each one

tank_gunner_routine_04:
    jsr tank_gunner_set_pos
    jsr tank_gunner_set_destroyed_bg ; set destroyed tank gunner bg tiles
    jmp enemy_explosion_routine_00   ; set empty sprite, play optional enemy destroyed sound, disable collisions

tank_gunner_routine_05:
    jsr tank_gunner_set_pos
    jmp enemy_explosion_routine_01 ; animate explosion sequence

tank_gunner_routine_06:
    jsr tank_gunner_set_pos
    jmp enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set tank gunner position based on tank boss's position
; input
;  * x - enemy slot index
tank_gunner_set_pos:
    ldy ENEMY_VAR_3,x      ; load enemy slot index of tank boss
    lda ENEMY_Y_POS,y      ; load tank boss's Y position
    clc                    ; clear carry in preparation for addition
    adc #$01
    sta ENEMY_Y_POS,x      ; set tank gunner Y position
    lda ENEMY_ATTRIBUTES,x ; load which gunner
    lsr
    lda #$1c               ; 28 in decimal
    bcc @continue          ; branch if right gunner (gunner 0)
    lda #$e3               ; left gunner, use -28 in decimal

@continue:
    adc ENEMY_X_POS,y
    sta ENEMY_X_POS,x ; set new X position
    rts

; set gunner recoil and bullet position
tank_gunner_set_bullet_pos:
    bne @exit                          ;  branch when unable to create bullet
    lda #$06
    sta ENEMY_VAR_5,x                  ; set gunner recoil effect timer
    ldy ENEMY_VAR_2,x                  ; load overhead aim direction
    ldx $11                            ; load created bullet enemy slot
    lda ENEMY_Y_POS,x                  ; load bullet's Y position
    clc                                ; clear carry in preparation for addition
    adc tank_gunner_bullet_y_adj_tbl,y ; add Y offset from gunner
    sta ENEMY_Y_POS,x                  ; set bullet's initial Y position
    lda ENEMY_X_POS,x                  ; load enemy's X position
    clc                                ; clear carry in preparation for addition
    adc tank_gunner_bullet_x_adj_tbl,y ; add X offset from gunner
    sta ENEMY_X_POS,x                  ; set bullet's initial X position
    ldx ENEMY_CURRENT_SLOT

@exit:
    rts

tank_gunner_bullet_y_adj_tbl:
    .byte $08 ; #$00 - right
    .byte $12 ; #$01 - down right
    .byte $14 ; #$02 - down
    .byte $12 ; #$03 - down left
    .byte $08 ; #$04 - left
    .byte $08 ; #$05 - left up
    .byte $08 ; #$06 - up
    .byte $08 ; #$07 - up right

tank_gunner_bullet_x_adj_tbl:
    .byte $0e ; #$00 - right
    .byte $0c ; #$01 - down right
    .byte $00 ; #$02 - down
    .byte $f4 ; #$03 - down left
    .byte $f2 ; #$04 - left
    .byte $f2 ; #$05 - left up
    .byte $00 ; #$06 - up
    .byte $0e ; #$07 - up right

tank_gunner_set_destroyed_bg:
    ldy #$00               ; right gunner index
    lda ENEMY_ATTRIBUTES,x ; load gunner index
    beq @continue          ; branch if right gunner
    ldy #$1e               ; left gunner, start in middle of table

@continue:
    ldx GRAPHICS_BUFFER_OFFSET

@graphic_loop:
    lda tank_gunner_destroy_tile_tbl,y
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny
    cmp #$ff
    bne @graphic_loop
    stx GRAPHICS_BUFFER_OFFSET
    ldx ENEMY_CURRENT_SLOT
    rts

tank_gunner_destroy_tile_tbl:
    .byte $06             ; block mode
    .byte $2d,$52         ; PPU address $2d52
    .byte $04             ; block size
    .byte $e5,$e6,$e7,$b3
    .byte $2d,$72         ; PPU address $2d72
    .byte $04             ; block size
    .byte $e8,$e9,$e8,$ba
    .byte $2d,$92         ; PPU address $2d92
    .byte $04             ; block size
    .byte $ea,$00,$f1,$c0
    .byte $2d,$b2         ; PPU address $29b2
    .byte $04             ; block size
    .byte $f2,$00,$f2,$c5
    .byte $ff             ; end of data marker

; start data for left gunner
    .byte $06             ; block mode
    .byte $2d,$4a         ; PPU address $2d4a
    .byte $04             ; block size
    .byte $ad,$e5,$e6,$e7
    .byte $2d,$6a         ; PPU address $2d6a
    .byte $04             ; block size
    .byte $b4,$e8,$e9,$e8
    .byte $2d,$8a         ; PPU address $2d8a
    .byte $04             ; block size
    .byte $20,$ea,$00,$f1
    .byte $2d,$aa         ; PPU address $2daa
    .byte $04             ; block size
    .byte $c1,$f2,$00,$f2
    .byte $ff             ; end of data marker

; enemy type #$36
tank_boss_electrode_routine_ptr_tbl:
    .addr tank_boss_electrode_routine_00 ; set enemy destroyed routine, advance
    .addr tank_boss_electrode_routine_01 ; set position of shaft and probes from tank boss position and extension/retraction amount
    .addr remove_enemy                   ; enemy destroyed routine

; set enemy destroyed routine, advance
tank_boss_electrode_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x ; mark electrode so bullets travel through it
    jmp advance_enemy_routine ; advance to next routine

; set position of shaft and probes from tank boss position and extension/retraction amount
tank_boss_electrode_routine_01:
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcc @dancing_lightning ; branch if extendable/retractable probes
    lda #$54               ; electrode shaft, load electrode shaft sprite
    sta ENEMY_SPRITE,x     ; sprite_54
    jmp @set_shafts_pos    ; set position of electrode shaft

@dancing_lightning:
    lda GLOBAL_TIMER   ; use global timer to create flashing electricity effect
    and #$03
    clc                ; clear carry in preparation for addition
    adc #$55
    sta ENEMY_SPRITE,x ; sprite_55, sprite_56, sprite_57, or sprite_58
    jmp @calc_pos      ; jump to calculate position based on how extended/retracted

@set_shafts_pos:
    lda #$00
    beq @set_pos ; always branch

; calculate and set electrode extend/retract amount to set position
@calc_pos:
    ldy ENEMY_VAR_3,x ; load tank boss enemy slot offset
    lda ENEMY_VAR_5,y ; load electrode extend/retract amount from tank boss

; input
;  * a - amount to add to tank boss Y position
@set_pos:
    ldy ENEMY_VAR_3,x ; load tank boss enemy slot offset
    clc               ; clear carry in preparation for addition
    adc ENEMY_Y_POS,y ; add tank boss Y position to a
    sta ENEMY_Y_POS,x
    lda ENEMY_X_POS,y
    sta ENEMY_X_POS,x
    rts

; enemy type #$25
rack_turret_routine_ptr_tbl:
    .addr rack_turret_routine_00     ; set HP, initial X velocity, adjust Y position, set firing delay, advance routine
    .addr rack_turret_routine_01     ; travel left and right on rack firing at closest player when appropriate
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set HP, initial X velocity, adjust Y position, set firing delay, advance routine
rack_turret_routine_00:
    lda #$04                  ; HP = #$04, #$08, or #$0b
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda ENEMY_ATTRIBUTES,x    ; load enemy attributes
    and #$01                  ; strip to just X velocity index (0 = -0.5, 1 = 0.5)
    sta ENEMY_VAR_1,x         ; set X velocity index
    jsr rack_turret_set_x_vel ; set X velocity based on ENEMY_VAR_1 (0 = -0.5, 1 = 0.5)
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    clc                       ; clear carry in preparation for addition
    adc #$0a
    sta ENEMY_Y_POS,x         ; adjust enemy's Y position by #$0a pixels down
    lda #$27
    sta ENEMY_FIRING_DELAY,x  ; set initial bullet firing delay
    jsr apply_velocity        ; apply enemy's velocity to its position, removing enemy if off-screen
    jmp advance_enemy_routine ; advance to next routine

; travel left and right on rack firing at closest player when appropriate
rack_turret_routine_01:
    jsr player_enemy_x_dist   ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                       ; transfer closest player index to a
    sta ENEMY_VAR_3,x         ; store targeted player
    lda PLAYER_SPRITE_X_POS,y ; load closest player X position
    sec                       ; set carry flag in preparation for subtraction
    sbc ENEMY_X_POS,x         ; subtract enemy X position
    ldy #$02
    bcs @compare_dist         ; branch if player to right of enemy
    ldy #$00                  ; player to left of enemy
    eor #$ff
    adc #$01                  ; overflow, flip all bits and add #$01

@compare_dist:
    cmp #$20      ; compare distance between closest player and enemy
    bcs @continue ; branch if player more than #$20 pixels away horizontally
    ldy #$01      ; player relatively close horizontally

@continue:
    tya                               ; transfer sprite index to a
    sta ENEMY_VAR_2,x                 ; set sprite and sprite attribute index
    lda rack_turret_sprite_tbl,y      ; load enemy sprite
    sta ENEMY_SPRITE,x                ; set rack-mount sprite
    lda rack_turret_sprite_attr_tbl,y ; load sprite attribute
    sta ENEMY_SPRITE_ATTR,x           ; set sprite attribute (palette and sprite horizontal flip bit)
    jsr apply_velocity                ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_X_VELOCITY_FAST,x       ; load enemy's fast X velocity
    rol
    rol
    and #$01                          ; put direction bit of fast velocity to a (0 = positive, 1 = negative)
    tay                               ; transfer wall index to y (0 = left wall, 1 = right wall)
    lda ENEMY_X_POS,x                 ; load enemy's X position
    cmp rack_turret_wall_x_tbl,y      ; compare enemy position to x dir change point (wall X position)
    ror                               ; push carry flag to bit 7
    eor ENEMY_X_VELOCITY_FAST,x       ; xor with bit 7 of fast velocity
    bpl @check_fire                   ; branch if direction shouldn't change
    jsr rack_turret_flip_x_dir        ; enemy x is less than left wall X and direction is negative or
                                      ; enemy x is greater than right wall X and direction is positive
                                      ; so flip x direction

; check to see if can/should fire a bullet
@check_fire:
    dec ENEMY_FIRING_DELAY,x              ; decrement bullet firing delay
    bne @exit                             ; exit if bullet firing delay hasn't elapsed
    ldy ENEMY_VAR_2,x                     ; load enemy X velocity index (0 = -0.5, 1 = 0.5)
    lda ENEMY_Y_POS,x                     ; load enemy's Y position
    clc                                   ; clear carry in preparation for addition
    adc rack_turret_bullet_y_offset_tbl,y ; add enemy Y offset to get bullet initial Y position
    sta $08                               ; set bullet creation Y position
    lda ENEMY_X_POS,x                     ; load enemy's X position
    clc                                   ; clear carry in preparation for addition
    adc rack_turret_bullet_x_offset_tbl,y ; add enemy X offset to get bullet initial X position
    sta $09                               ; set bullet creation X point
    ldy ENEMY_VAR_3,x                     ; load player index of player to target (0 = p1, 1 = p2)
    sty $0a                               ; set player index of player to target for fire_bullet_at_player_1x_speed
    lda ENEMY_Y_POS,x                     ; load enemy's Y position
    cmp #$a0
    bcs @exit                             ; exit without firing if enemy in bottom 37.5% of screen
    cmp PLAYER_SPRITE_Y_POS,y
    bcs @exit                             ; exit without firing if enemy is below player
    jsr fire_bullet_at_player_1x_speed    ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    lda #$37
    sta ENEMY_FIRING_DELAY,x              ; set next bullet firing delay

@exit:
    rts

; right and left edges of screen
rack_turret_wall_x_tbl:
    .byte $e0,$20 ; 87.5%, 12.5%

; sprite_64, sprite_65
rack_turret_sprite_tbl:
    .byte $64,$65,$64

; sprite attributes (palette and whether or not to flip horizontally)
rack_turret_sprite_attr_tbl:
    .byte $03,$03,$43

rack_turret_bullet_y_offset_tbl:
    .byte $0b,$13,$0b

rack_turret_bullet_x_offset_tbl:
    .byte $f5,$00,$0b

rack_turret_flip_x_dir:
    lda ENEMY_VAR_1,x ; load enemy x direction index (0 = -0.5, 1 = 0.5)
    eor #$01          ; flip the x direction
    sta ENEMY_VAR_1,x ; set enemy x direction index (0 = -0.5, 1 = 0.5)

; sets the X fast and fractional velocity for rack-mounted turret based on ENEMY_VAR_1
;  * 0 => -0.5
;  * 1 => 0.5
rack_turret_set_x_vel:
    lda ENEMY_VAR_1,x             ; load X velocity index
    asl                           ; double since each entry is #$02 bytes
    tay                           ; transfer to offset register
    lda rack_turret_x_vel_tbl,y   ; load enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x  ; store enemy's fractional X velocity
    lda rack_turret_x_vel_tbl+1,y ; load  enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x   ; store enemy's fast X velocity
    rts

rack_turret_x_vel_tbl:
    .byte $80,$ff ; -0.5
    .byte $80,$00 ;  0.5

; enemy type #$2c
collapsible_ceiling_routine_ptr_tbl:
    .addr collapsible_ceiling_routine_00 ; disable player enemy collision, adjust Y position, advance routine
    .addr collapsible_ceiling_routine_01 ; wait for vertical scrolling and for scrolling to be locked, advance routine
    .addr collapsible_ceiling_routine_02 ; keep track of closest bullet for when destroyed in next routine
    .addr collapsible_ceiling_routine_03 ; enemy destroyed routine, change bg collision from solid to empty, set vars for dropping tiles, advance routine
    .addr collapsible_ceiling_routine_04 ; animate and create falling tiles to the left and right of initial collapse point
    .addr collapsible_ceiling_routine_05 ; allow scrolling and remove enemy

; disable player enemy collision, adjust Y position, advance routine
collapsible_ceiling_routine_00:
    lda #$01
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    adc #$04
    sta ENEMY_Y_POS,x         ; add #$04 to enemy Y position
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for vertical scrolling and for scrolling to be locked, advance routine
collapsible_ceiling_routine_01:
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    lda SCREEN_SCROLL_TYPE    ; 0 = horizontal, 1 = vertical/overhead
    beq @exit                 ; exit if horizontal scroll
    lda LEVEL_Y_SCROLL_FLAGS
    bpl @exit                 ; branch if can scroll up
    jmp advance_enemy_routine ; advance to next routine

@exit:
    rts

; keep track of closest bullet for when destroyed in next routine
collapsible_ceiling_routine_02:
    lda #$01
    sta ENEMY_SPRITE,x ; set invisible sprite, enemy is background tiles
    lda #$ff
    sta $08            ; initialize closest Y position to #$ff
    sta $09            ; initialize closest bullet index to #$ff
    ldy #$0f

; find closest bullet to the collapsible ceiling
@find_closest_bullet:
    lda PLAYER_BULLET_STATE,y ; (0 = unused, 1 = normal, 2 = destroyed)
    cmp #$01
    bne @continue             ; move to next bullet if bullet is destroyed
    lda PLAYER_BULLET_X_POS,y
    cmp #$38
    bcc @continue             ; move to next bullet if bullet is to left of ceiling
    cmp #$c8
    bcs @continue             ; move to next bullet if bullet is to right of ceiling
    lda PLAYER_BULLET_Y_POS,y
    cmp $08
    bcs @continue             ; move to next bullet if bullet not closer than previous closest bullet
    sta $08                   ; found new closest bullet, set closest bullet Y position
    sty $09                   ; set closest bullet index

@continue:
    dey                       ; move to next bullet
    bpl @find_closest_bullet  ; continue looking if another bullet
    lda #$80                  ; looped through all bullets
    ldy $09                   ; load closest bullet X position
    bmi @continue2
    lda PLAYER_BULLET_X_POS,y ; load X position of closest bullet

@continue2:
    sta ENEMY_X_POS,x    ; set enemy X position to either #$80 or closest bullet's X position
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; enemy destroyed routine, change bg collision from solid to empty, set vars for dropping tiles, advance routine
collapsible_ceiling_routine_03:
    lda #$20
    sta SECOND_BG_COLLISION_DATA+9
    lda #$00
    sta SECOND_BG_COLLISION_DATA+10
    sta SECOND_BG_COLLISION_DATA+11
    sta SECOND_BG_COLLISION_DATA+12
    sta SECOND_BG_COLLISION_DATA+13
    lda #$02
    sta SECOND_BG_COLLISION_DATA+14 ; change bg collision to empty
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    sec                             ; set carry flag in preparation for subtraction
    sbc #$04
    sta ENEMY_Y_POS,x               ; adjust enemy Y position up by #$04
    lda ENEMY_X_POS,x               ; load X position of bullet that hit the ceiling
    and #$f0
    ora #$04
    sta ENEMY_VAR_1,x               ; set X position of the first tile to drop
    sec                             ; set carry flag in preparation for subtraction
    sbc #$10
    cmp #$30                        ; see if last tile on the left
    bcs @continue                   ; branch if more tile tiles to the left
    lda #$00                        ; left-most tile is dropping first, mark no more tiles to left

@continue:
    sta ENEMY_VAR_2,x         ; store enemy adjusted Y position
    lda #$01
    sta ENEMY_VAR_3,x         ; set delay timer until next tile to the right is dropped
    lda #$10
    sta ENEMY_VAR_4,x         ; set delay timer until next tile to the left is dropped
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; animate and create falling tiles to the left and right of initial collapse point
collapsible_ceiling_routine_04:
    jsr update_enemy_pos               ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_VAR_1,x                  ; load next tile to the right's X position
    beq @left_tile                     ; branch if all tiles to right have been dropped, move to logic for left tile
    dec ENEMY_VAR_3,x                  ; decrement right tile drop delay timer
    bne @left_tile                     ; branch if timer hasn't elapsed, move to logic for left tile
    lda #$00                           ; black 2x2 square
    sta $08                            ; nametable_square_update_tbl offset
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    sec                                ; set carry flag in preparation for subtraction
    sbc #$04                           ; calculated Y position of where to draw the nametable 4 nametable tiles
    tay                                ; move to y since parameter to nametable update method
    lda ENEMY_VAR_1,x                  ; X position of tile
    jsr update_nametable_square_at_pos ; update 2x2 nametable at (a,y) with tiles specified by $08
    lda #$01                           ; if unable to update nametable, set right tile drop delay to #$01 to try again next frame
    bcs @set_right_delay               ; branch if unable to update the 4 nametable tiles
    ldy #$2d                           ; enemy type = falling ceiling tile
    jsr try_create_enemy_from_existing ; create falling ceiling tile
    bcc @continue                      ; branch if unable to create falling ceiling tile
    ldy $11                            ; load slot index of created enemy
    lda ENEMY_VAR_1,x                  ; load right edge X position
    clc                                ; clear carry in preparation for addition
    adc #$04                           ; add #$04 pixels
    sta ENEMY_X_POS,y                  ; set created falling ceiling X position

@continue:
    lda ENEMY_VAR_1,x   ; load right edge X position
    clc                 ; clear carry in preparation for addition
    adc #$10            ; add #$10 to move to next tile to the right
    cmp #$d0            ; see if drawn all tiles to the right
    bcc @set_right_edge ; branch if more tiles to draw to the right
    lda #$00            ; drawn all tiles to right, mark complete by setting to #$00

@set_right_edge:
    sta ENEMY_VAR_1,x ; set next tile on the right to drop's X position
    lda #$10          ; initialize delay until next tile to the right is dropped

@set_right_delay:
    sta ENEMY_VAR_3,x ; set right tile drop delay

@left_tile:
    lda ENEMY_VAR_2,x                  ; load left edge X position
    beq @check_adv_routine_exit        ; branch if left edge complete, check if right edge also complete
                                       ; if both complete advance routine, otherwise exit
    dec ENEMY_VAR_4,x                  ; decrement right tile drop delay timer
    bne @check_adv_routine_exit
    lda #$00                           ; black 2x2 square
    sta $08                            ; nametable_square_update_tbl offset
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    sec                                ; set carry flag in preparation for subtraction
    sbc #$08                           ; calculated Y position of where to draw the nametable 4 nametable tiles
    tay                                ; move to y since parameter to nametable update method
    lda ENEMY_VAR_2,x                  ; X position of tile
    jsr update_nametable_square_at_pos ; update 2x2 nametable at (a,y) with tiles specified by $08
    lda #$01                           ; if unable to update nametable, set left tile drop delay to #$01 to try again next frame
    bcs @set_left_delay                ; branch if unable to update the 4 nametable tiles
    ldy #$2d                           ; enemy type = falling ceiling tile
    jsr try_create_enemy_from_existing ; create falling ceiling tile
    bcc @calc_left_edge_pos            ; branch if unable to create falling ceiling tile
    ldy $11                            ; load slot index of created enemy
    lda ENEMY_VAR_2,x                  ; load left edge X position
    clc                                ; clear carry in preparation for addition
    adc #$04                           ; add #$04 pixels
    sta ENEMY_X_POS,y                  ; set created falling ceiling X position

@calc_left_edge_pos:
    lda ENEMY_VAR_2,x  ; load left edge X position
    sec                ; set carry flag in preparation for subtraction
    sbc #$10           ; subtract #$10 to move to next tile to the left
    cmp #$30           ; see if drawn all tiles to the left
    bcs @set_left_edge ; branch if more tiles to draw to the left
    lda #$00           ; drawn all tiles to the left, mark complete by setting to #$00

@set_left_edge:
    sta ENEMY_VAR_2,x ; set next tile on the left to drop's X position
    lda #$10          ; initialize delay until next tile to the left is dropped

@set_left_delay:
    sta ENEMY_VAR_4,x ; set timer until next tile on the left is dropped

@check_adv_routine_exit:
    lda ENEMY_VAR_1,x         ; load right edge X position
    ora ENEMY_VAR_2,x         ; merge with left edge X position
    bne @exit                 ; exit if haven't initiated all ceiling tile drops
    jmp advance_enemy_routine ; dropped all tiles, advance to next routine

@exit:
    rts

; allow scrolling and remove enemy
collapsible_ceiling_routine_05:
    lda #$40
    sta LEVEL_Y_SCROLL_FLAGS ; allow vertical scrolling
    sta Y_SCROLL_FLAGS
    jmp remove_enemy

; enemy type #$2d
falling_ceiling_tile_routine_ptr_tbl:
    .addr falling_ceiling_tile_routine_00 ; set sprite, hp, advance routine
    .addr falling_ceiling_tile_routine_01 ; fall until collide with bg, then advance routine
    .addr enemy_explosion_routine_00      ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01      ; animate explosion sequence
    .addr enemy_explosion_routine_03      ; mark destroyed, remove enemy

; set sprite, hp, advance routine
falling_ceiling_tile_routine_00:
    lda #$62
    sta ENEMY_SPRITE,x   ; sprite_62
    lda #$04
    sta ENEMY_HP,x       ; set HP to #$04
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)

falling_ceiling_tile_adv_routine:
    jmp advance_enemy_routine ; advance to next routine

; fall until collide with bg, then advance routine
falling_ceiling_tile_routine_01:
    lda ENEMY_Y_VELOCITY_FRACT,x         ; load enemy's fractional Y velocity
    clc                                  ; clear carry in preparation for addition
    adc #$20
    sta ENEMY_Y_VELOCITY_FRACT,x         ; store enemy's fractional Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x          ; load enemy's fast Y velocity
    adc #$00
    sta ENEMY_Y_VELOCITY_FAST,x          ; store enemy's fast Y velocity
                                         ; applies gravity to the falling ceiling tile
    jsr apply_velocity                   ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$00                             ; don't adjust enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code      ; test background collision
    bne falling_ceiling_tile_adv_routine ; branch if collided with background to advance routine to destroy
    rts

; enemy type #$31
spinning_bubbles_routine_ptr_tbl:
    .addr spinning_bubbles_routine_00 ; set sprite and delay, advance routine
    .addr spinning_bubbles_routine_01
    .addr spinning_bubbles_routine_02
    .addr enemy_explosion_routine_00  ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01  ; animate explosion sequence
    .addr enemy_explosion_routine_03  ; mark destroyed, remove enemy

; set sprite and delay, advance routine
spinning_bubbles_routine_00:
    lda #$06
    sta ENEMY_SPRITE,x              ; sprite_06
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$40
    jmp set_delay_adv_enemy_routine ; set delay to #$40 and set routine to spinning_bubbles_routine_01

spinning_bubbles_routine_01:
    ldy #$05                        ; animation index for spinning bubbles
    jsr set_enemy_animation_sprite  ; animate sprite for spinning bubbles based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne @exit                       ; exit if targeting delay hasn't elapsed
    jsr player_enemy_x_dist         ; targeting delay elapsed, target player
                                    ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$06                        ; 1.75x speed
    jsr set_vel_to_target_player    ; set enemy velocity to target player index $0a at 1.75x speed
    lda #$20
    sta ENEMY_VAR_2,x
    lda #$30
    jmp set_delay_adv_enemy_routine ; set delay to #$30 and set routine to spinning_bubbles_routine_02

@exit:
    rts

spinning_bubbles_routine_02:
    ldy #$06                         ; animation index for spinning bubbles
    jsr set_enemy_animation_sprite   ; animate sprite for spinning bubbles based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda ENEMY_VAR_2,x
    beq @apply_velocity
    dec ENEMY_DELAY,x                ; decrement targeting delay
    bne @apply_velocity
    lda #$06
    sta ENEMY_DELAY,x                ; initialize delay to #$06
    jsr player_enemy_x_dist          ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$06                         ; speed adjust code for 1.75x speed
    jsr hone_to_player_set_enemy_vel ; determine next aim dir to get closer to player index ($0a)
                                     ; and set enemy velocity to that target direction
    dec ENEMY_VAR_2,x

@apply_velocity:
    jmp apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

; enemy type #$39
elevator_routine_ptr_tbl:
    .addr elevator_routine_00 ; set collision properties, advance routine
    .addr elevator_routine_01 ; wait for activation, set location and delay, advance routine
    .addr elevator_routine_02 ; wait for delay, enable scanline interrupts, advance routine
    .addr elevator_routine_03
    .addr elevator_routine_04 ; slow elevator Y speed to -0.25, advance routine
    .addr elevator_routine_05 ; play siren, wait for Y_SCROLL to be #$00, advance routine
    .addr elevator_routine_06 ; stop any elevator velocity, remove enemy, remove scanline interrupts

; set collision properties, advance routine
elevator_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through elevator floor and player can collide
    jmp advance_enemy_routine ; advance to next routine

; wait for activation, set location and delay, advance routine
elevator_routine_01:
    lda Y_SCROLL             ; load PPU vertical scroll
    bne elevator_exit        ; branch if not yet at activation point
                             ; set_elevator_vel will y autoscroll until nametable is fully visible
                             ; this will cause Y_SCROLL to be 0 and the elevator to activate
    lda #$00                 ; player on elevator
    sta ELEVATOR_ENABLED     ; disable elevator
    lda #$c0
    sta LEVEL_Y_SCROLL_FLAGS ; don't allow player to scroll
    lda #$a0
    sta ENEMY_Y_POS,x        ; set elevator Y position

elevator_set_delay_adv_routine:
    lda #$00
    sta ENEMY_VAR_1,x
    lda #$08
    sta ENEMY_DELAY,x
    jmp advance_enemy_routine ; advance to next routine

elevator_exit:
    rts

; wait for delay, enable scanline interrupts, advance routine
elevator_routine_02:
    lda ENEMY_DELAY,x
    beq @continue
    dec ENEMY_DELAY,x
    bne elevator_exit

@continue:
    ldy #$00
    jsr set_elevator_graphics
    bcc elevator_exit
    lda #$02
    sta IRQ_TYPE              ; set irq routine type to irq_handler_02_ptr_tbl
    jsr elevator_scroll
    lda #$00
    sta IRQ_X_SCROLL
    lda #$00
    sta IRQ_PPUCTRL_SETTINGS
    jsr elevator_scroll
    jmp advance_enemy_routine ; advance to next routine

elevator_routine_03:
    jsr elevator_scroll
    ldy #$01
    jsr set_elevator_graphics
    bcc elevator_exit
    jmp advance_enemy_routine ; advance to next routine

; slow elevator Y speed to -0.25, advance routine
elevator_routine_04:
    jsr elevator_scroll
    lda #$80
    sta ELEVATOR_ENABLED            ; enable the elevator
    lda #$c0
    sta ELEVATOR_FRACT_VEL
    lda #$ff
    sta ELEVATOR_FAST_VEL           ; -0.25 Y velocity
    lda #$40
    sta LEVEL_Y_SCROLL_FLAGS        ; allow player to cause Y scroll
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to elevator_routine_05

; play siren, wait for Y_SCROLL to be #$00, advance routine
elevator_routine_05:
    lda GLOBAL_TIMER
    and #$03
    bne @skip_sound
    dec ENEMY_DELAY,x
    bne @skip_sound
    lda #$80
    sta ENEMY_DELAY,x
    lda #$21          ; sound_21 (SILEN) - siren
    jsr play_sound    ; play sound_21 (SILEN) - siren

@skip_sound:
    jsr elevator_scroll
    lda ELEVATOR_ENABLED
    bne elevator_exit                  ; exit if elevator is enabled/moving
    lda Y_SCROLL                       ; load PPU vertical scroll
    bne elevator_exit
    jmp elevator_set_delay_adv_routine

; stop any elevator velocity, remove enemy, remove scanline interrupts
elevator_routine_06:
    jsr elevator_scroll
    ldy #$02
    jsr set_elevator_graphics
    bcc @exit
    jsr set_nmi_noop_irq      ; remove any scanline interrupts
    lda #$40
    sta LEVEL_Y_SCROLL_FLAGS
    lda #$00
    sta ELEVATOR_FAST_VEL
    jmp remove_enemy

@exit:
    rts

; input
;  * y - specific tiles to load for situation
;    * 0 - redraw nametable where elevator starts or stops
;    * 1 - draw elevator on nametable for when moving
;    * 2 - draw elevator on nametable for after done moving
set_elevator_graphics:
    sty $0c                               ; set specific set of graphics to load
                                          ; e.g. loading
    lda elevator_bg_tiles_size_tbl,y
    sta $0b                               ; set total number of graphics tables for situation
                                          ; always #$04
    tya                                   ; transfer set of tiles to load to a
    asl                                   ; double since each entry is a #$02 byte memory address
    tay                                   ; transfer to offset register
    lda elevator_bg_tiles_ptr_ptr_tbl,y   ; load elevator_bg_tiles_ptr_tbl_xx high byte
    sta $09
    lda elevator_bg_tiles_ptr_ptr_tbl+1,y
    sta $0a
    lda ENEMY_VAR_1,x                     ; load which set of tiles to draw (elevator_bg_tiles_xx_yy)
    asl                                   ; double since each entry is a #$02 byte memory address
    tay                                   ; transfer to offset register
    lda ($09),y                           ; load elevator_bg_tiles_ptr_tbl_xx
    sta $08
    iny
    lda ($09),y
    sta $09                               ; ($09) now stores the 2-byte memory address to the start of elevator_bg_tiles_xx_yy
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$10
    bcs @restore_x_exit                   ; exit if graphics buffer full
    ldy #$00                              ; initialize read offset
    lda ($08),y                           ; load elevator_bg_tiles_xx_yy,y
    sta $0a                               ; store number of bytes in the table
    iny                                   ; increment read offset

@graphics_loop:
    lda ($08),y                      ; read graphic byte
    sta CPU_GRAPHICS_BUFFER,x        ; write graphic byte to cpu buffer
    iny                              ; increment read offset
    inx                              ; increment write offset
    dec $0a                          ; decrement remaining bytes to read from elevator_bg_tiles_xx_yy
    bne @graphics_loop
    stx GRAPHICS_BUFFER_OFFSET       ; update graphics buffer offset
    ldx ENEMY_CURRENT_SLOT           ; restore x to the elevator enemy offset
    inc ENEMY_VAR_1,x                ; increment to next graphics table, i.e. next elevator_bg_tiles_xx_yy
    lda ENEMY_VAR_1,x                ; load next group of graphics to load (elevator_bg_tiles_ptr_tbl_00, elevator_bg_tiles_ptr_tbl_01, or elevator_bg_tiles_ptr_tbl_02)
    cmp $0b                          ; see if read all graphics tables for situation
    bcc @exit                        ; exit if more tile data to draw
    lda #$00                         ; drawn all tile, reset graphics group
    sta ENEMY_VAR_1,x
    ldy $0c                          ; load which graphics were drawn
    lda elevator_collision_pos_tbl,y ; load index into collision data of where to start overwriting
    tay                              ; transfer to offset register
    lda #$06                         ; initialize loop counter, writing 12 collision bytes
    sta $0d                          ; set loop counter

@collision_loop:
    lda #$11
    sta BG_COLLISION_DATA+1,y
    lda #$00
    sta BG_COLLISION_DATA+9,y
    iny
    dec $0d
    bne @collision_loop
    lda #$21
    sta BG_COLLISION_DATA-6,y
    lda #$12
    sta BG_COLLISION_DATA+1,y
    lda #$20
    sta BG_COLLISION_DATA+2,y
    lda #$02
    sta BG_COLLISION_DATA+9,y
    sec
    rts

@restore_x_exit:
    ldx ENEMY_CURRENT_SLOT
    clc

@exit:
    rts

elevator_collision_pos_tbl:
    .byte $50,$d0,$d0

; elevator tile ptr ptr tbl
elevator_bg_tiles_ptr_ptr_tbl:
    .addr elevator_bg_tiles_ptr_tbl_00 ; tiles to replace nametable where elevator starts or stops
    .addr elevator_bg_tiles_ptr_tbl_01 ; tiles to draw elevator on nametable for when moving
    .addr elevator_bg_tiles_ptr_tbl_02 ; tiles to draw elevator on nametable for after done moving

; number of frames required to draw
elevator_bg_tiles_size_tbl:
    .byte $04,$04,$04

; tiles to replace nametable where elevator starts
elevator_bg_tiles_ptr_tbl_00:
    .addr elevator_bg_tiles_00_00
    .addr elevator_bg_tiles_00_01
    .addr elevator_bg_tiles_00_02
    .addr elevator_bg_tiles_00_03

; tiles to draw elevator on nametable for when moving
elevator_bg_tiles_ptr_tbl_01:
    .addr elevator_bg_tiles_01_00
    .addr elevator_bg_tiles_01_01
    .addr elevator_bg_tiles_01_02
    .addr elevator_bg_tiles_01_03

; tiles to draw elevator on nametable for after done moving
elevator_bg_tiles_ptr_tbl_02:
    .addr elevator_bg_tiles_02_00
    .addr elevator_bg_tiles_02_01
    .addr elevator_bg_tiles_02_02
    .addr elevator_bg_tiles_02_03

elevator_bg_tiles_00_00:
    .byte $15             ; table size
    .byte $06             ; block mode
    .byte $22,$80         ; PPU address $2280
    .byte $04             ; block size
    .byte $40,$40,$29,$6e
    .byte $22,$9c         ; PPU address $229c
    .byte $04             ; block size
    .byte $2a,$6f,$40,$40
    .byte $ff             ; end of block mode
    .byte $03             ; repeat mode
    .byte $22,$84         ; PPU address $2284
    .byte $18
    .byte $2b             ; repeat #$2b #$18 times

elevator_bg_tiles_00_01:
    .byte $25                                                             ; table size
    .byte $06                                                             ; block mode
    .byte $22,$a0                                                         ; PPU address $22a0
    .byte $20                                                             ; block size
    .byte $6b,$6c,$70,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71
    .byte $72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$73,$6b,$6c
    .byte $ff                                                             ; end of block mode

elevator_bg_tiles_00_02:
    .byte $25                                                             ; table size
    .byte $06                                                             ; block mode
    .byte $22,$c0                                                         ; PPU address $22c0
    .byte $20                                                             ; block size
    .byte $30,$6d,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74
    .byte $1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$30,$6d
    .byte $ff                                                             ; end of block mode

; replace tiles behind under elevator at top with black (blank) tiles
elevator_bg_tiles_00_03:
    .byte $0a     ; table size
    .byte $03     ; repeat mode
    .byte $22,$e0 ; PPU address $22e0
    .byte $24
    .byte $00     ; repeat #$00 (black tile) #$24 times (1 nt row)
    .byte $03     ; repeat mode
    .byte $23,$e8 ; PPU address $22e8 (attribute table)
    .byte $08
    .byte $00     ; repeat #$00 #$08 times

; edges of screen
elevator_bg_tiles_01_00:
    .byte $13         ; table size
    .byte $06         ; block mode
    .byte $2e,$80     ; PPU address $2e80
    .byte $03         ; block size
    .byte $3a,$f0,$0c
    .byte $2e,$9d     ; PPU address $2e9d
    .byte $03         ; block size
    .byte $d5,$3d,$3a
    .byte $ff         ; end of block mode
    .byte $03         ; repeat mode
    .byte $2e,$83     ; PPU address $2e83
    .byte $1a
    .byte $00         ; repeat #$00 #$1a times

elevator_bg_tiles_01_01:
    .byte $13         ; table size
    .byte $06         ; block mode
    .byte $2e,$a0     ; PPU address $2ea0
    .byte $03         ; block size
    .byte $f5,$f6,$0c
    .byte $2e,$bd     ; PPU address $2ebd
    .byte $03         ; block size
    .byte $d5,$ee,$43
    .byte $ff         ; end of block mode
    .byte $03         ; repeat mode
    .byte $2e,$a3     ; PPU address $2ea3
    .byte $1a
    .byte $00         ; repeat #$00 #$1a times

elevator_bg_tiles_01_02:
    .byte $13         ; table size
    .byte $06         ; block mode
    .byte $2e,$c0     ; PPU address $2ec0
    .byte $03         ; block size
    .byte $3b,$f7,$0c
    .byte $2e,$dd     ; PPU address $2edd
    .byte $03         ; block size
    .byte $d5,$3e,$44
    .byte $ff         ; end of block mode
    .byte $03         ; repeat mode
    .byte $2e,$c3     ; PPU address $2ec3
    .byte $1a
    .byte $00         ; repeat #$00 #$1a times

elevator_bg_tiles_01_03:
    .byte $13         ; table size
    .byte $06         ; block mode
    .byte $2e,$e0     ; PPU address $2ee0
    .byte $03         ; block size
    .byte $c5,$f1,$1b
    .byte $2e,$fd     ; PPU address $2efd
    .byte $03         ; block size
    .byte $d6,$e9,$c5
    .byte $ff         ; end of block mode
    .byte $03         ; repeat mode
    .byte $2e,$e3     ; PPU address $2ee3
    .byte $1a
    .byte $00         ; repeat #$00 #$1a times

elevator_bg_tiles_02_00:
    .byte $15             ; table size
    .byte $06             ; block mode
    .byte $2e,$80         ; PPU address $2e80
    .byte $04             ; block size
    .byte $40,$40,$29,$6e
    .byte $2e,$9c         ; PPU address $2e9c
    .byte $04             ; block size
    .byte $2a,$6f,$40,$40
    .byte $ff             ; end of block mode
    .byte $03             ; repeat mode
    .byte $2e,$84         ; PPU address $2e84
    .byte $18
    .byte $2b             ; repeat #$2b #$18 times

elevator_bg_tiles_02_01:
    .byte $25                                                             ; table size
    .byte $06                                                             ; block mode
    .byte $2e,$a0                                                         ; PPU address $2ea0
    .byte $20                                                             ; block size
    .byte $6b,$6c,$70,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71
    .byte $72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$73,$6b,$6c
    .byte $ff                                                             ; end of block mode

elevator_bg_tiles_02_02:
    .byte $25                                                             ; table size
    .byte $06                                                             ; block mode
    .byte $2e,$c0                                                         ; PPU address $2ec0
    .byte $20                                                             ; block size
    .byte $30,$6d,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74
    .byte $1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$1e,$74,$30,$6d
    .byte $ff                                                             ; end of block mode

elevator_bg_tiles_02_03:
    .byte $0a     ; table size
    .byte $03     ; repeat mode
    .byte $2e,$e0 ; PPU address $2ee0
    .byte $20
    .byte $00     ; repeat #$00 #$20 times
    .byte $03     ; repeat mode
    .byte $2f,$e8 ; PPU address $2fe8
    .byte $08
    .byte $00     ; repeat #$00 #$08 times

; scrolls elevator by #$20
elevator_scroll:
    lda ENEMY_Y_POS,x        ; load enemy's Y position
    .ifdef Probotector
        sec
        sbc #$0c
    .endif
    sta SCANLINE_IRQ_1
    .ifdef Probotector
        lda ENEMY_Y_POS,x
    .endif
    clc                      ; clear carry in preparation for addition
    adc #$20
    clc                      ; clear carry in preparation for addition
    adc Y_SCROLL             ; add vertical scroll
    bcs @adj_offset_continue
    cmp #$f0
    bcc @continue

@adj_offset_continue:
    adc #$0f

@continue:
    sta $00
    and #$07
    sta SCANLINE_IRQ_3_DIFF
    lda #$1f
    sec                     ; set carry flag in preparation for subtraction
    sbc SCANLINE_IRQ_3_DIFF
    sta SCANLINE_IRQ_2_DIFF ; set the number of scanlines after SCANLINE_IRQ_1 to run the next scanline IRQ
    lda #$09
    sta IRQ_PPUADDR+1
    lda $00
    and #$f8
    asl
    rol IRQ_PPUADDR+1
    asl
    rol IRQ_PPUADDR+1
    sta IRQ_PPUADDR
    lda #$a0
    sec                     ; set carry flag in preparation for subtraction
    sbc ENEMY_Y_POS,x
    sta IRQ_Y_SCROLL
    rts

; enemy type #$40
winged_soldier_routine_ptr_tbl:
    .addr winged_soldier_routine_00  ; sets X velocity based on ENEMY_ATTRIBUTES, sets sprite attribute, advances routine
    .addr winged_soldier_routine_01  ; walk to edge of ledge
    .addr winged_soldier_routine_02  ; face screen, wait for delay, set falling velocity
    .addr winged_soldier_routine_03  ; open wings, apply gravity, check collision
    .addr winged_soldier_routine_04  ; landing on ledge, set sprite, wait for delay, target player, go to winged_soldier_routine_01
    .addr winged_soldier_routine_05  ; enemy destroyed routine, set collision velocity
    .addr winged_soldier_routine_06  ; enemy falling after collision until ENEMY_DELAY elapses
    .addr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; sets X velocity based on ENEMY_ATTRIBUTES, sets sprite attribute, advances routine
winged_soldier_routine_00:
    jsr update_enemy_pos   ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ATTRIBUTES,x
    and #$01
    sta ENEMY_ATTRIBUTES,x

; sets the X velocity, sprite attribute, and advances routine
; input
;  * a - x direction (0 = -1 (left), 1 = 1 (right))
winged_soldier_set_dir_adv_routine:
    asl                                ; double since entries contain both fractional and fast velocities
    tay                                ; transfer to offset register
    lda winged_soldier_x_vel_tbl,y     ; load enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x       ; store enemy's fractional X velocity
    lda winged_soldier_x_vel_tbl+1,y   ; load enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x        ; store enemy's fast X velocity
    jsr winged_soldier_set_sprite_attr ; set sprite attribute based on horizontal facing direction
    jmp advance_enemy_routine          ; advance to next routine

; set sprite attribute based on horizontal facing direction
; input
;  * a - X fast velocity
winged_soldier_set_sprite_attr:
    asl                       ; shift x direction into carry bit
    lda #$41                  ; assume facing left, set horizontal sprite flip bit
    bcs @set_sprite_attr_exit
    lda #$01                  ; facing right, clear horizontal sprite flip bit

@set_sprite_attr_exit:
    sta ENEMY_SPRITE_ATTR,x ; set palette and horizontal facing direction
    rts

winged_soldier_x_vel_tbl:
    .byte $00,$ff ; X velocity = -1.00
    .byte $00,$01 ; X velocity =  1.00

; walk to edge of ledge
winged_soldier_routine_01:
    lda #$12                                 ; add #$12 to Y position when determining bg collision
    jsr get_enemy_bg_collision_code_onscreen ; get bg collision code if on-screen
    beq @clear_vel_adv_routine               ; branch if no collision to stop soldier and move to next routine
                                             ; to face screen for jump
    jsr set_y_pos_for_bg                     ; collided with bg, set enemy Y position based on background collision
    lda $03                                  ; load BG_COLLISION_DATA offset low nibble (horizontal component)
    and #$01
    sta $03                                  ; keep whether using high nibble or low nibble
    lda #$01
    ldy ENEMY_X_VELOCITY_FAST,x              ; load enemy's fast X velocity
    bpl @continue                            ; branch if moving left
    lda #$ff                                 ; moving right

@continue:
    sta $08          ; store direction in $08 (1 = left, -1 = right)
    clc              ; clear carry in preparation for addition
    adc $03
    cmp #$02
    and #$01
    sta $03
    lda $04          ; load previously checked BG_COLLISION_DATA offset
    bcc @check_ledge
    clc              ; clear carry in preparation for addition
    adc $08          ; calc new BG_COLLISION_DATA offset

@check_ledge:
    tay                          ; transfer BG_COLLISION_DATA offset to offset register
    jsr check_bg_collision_at_y  ; check bg collision at BG_COLLISION_DATA,y
    beq @clear_vel_adv_routine   ; branch if at edge of ledge
    lda #$08                     ; not at edge of ledge
    ldy ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    bpl @check_bg_wall_collision ; branch if moving right
    lda #$f8                     ; moving left, use -8

@check_bg_wall_collision:
    jsr check_bg_wall_collision        ; check for wall collision
    beq @target_player                 ; branch if no wall collision
    jsr flip_enemy_x_dir               ; ran into wall
                                       ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25
    jsr winged_soldier_set_sprite_attr ; set sprite attribute based on horizontal facing direction
    inc ENEMY_VAR_1,x                  ; increase ran into wall count

@target_player:
    jsr player_enemy_x_dist   ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    sec                       ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_Y_POS,y ; subtract targeted player's Y position
    bcs @continue5
    eor #$ff
    adc #$01

@continue5:
    cmp #$08
    lda ENEMY_VAR_1,x
    bcs @continue6
    cmp #$02          ; see if have run into less tha 2 walls
    bcc @animate

@continue6:
    cmp #$03                   ; compare number of times enemy has run into wall to #$03
    bcs @clear_vel_adv_routine ; branch if bumped into more than 2 walls to begin jump down a ledge
    lda ENEMY_X_POS,x          ; load enemy's X position
    sec                        ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_X_POS,y  ; subtract player y's x distance
    bcs @check_if_close        ; branch if enemy to left of player
    eor #$ff
    adc #$01                   ; negative result, flip all bits and add 1

@check_if_close:
    cmp #$28                   ; see if enemy more than #$28 pixels horizontally away from player
    bcc @clear_vel_adv_routine ; branch if close to player

@animate:
    ldy #$07                       ; animation index for winged soldier running
    jsr set_enemy_animation_sprite ; animate sprite for winged soldier running based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jmp update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)

@clear_vel_adv_routine:
    jsr clear_enemy_vel             ; set enemy X and Y velocity to 0
    jsr update_pos_check_offscreen  ; adjust position based on scroll (does not apply velocity)
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to winged_soldier_routine_01

winged_soldier_exit:
    rts

; face screen, wait for delay, set falling velocity
winged_soldier_routine_02:
    jsr winged_soldier_face_screen
    dec ENEMY_DELAY,x              ; decrement targeting delay
    bne winged_soldier_exit        ; exit if delay hasn't elapsed
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS        ; compare enemy position to player position to determine enemy velocity
    ldy #$00                       ; set Y velocity to -1.5 and X velocity to -0.75 (move down left)
    bcs @continue                  ; branch if enemy to the right of player
    ldy #$04                       ; set Y velocity to -1.5 and X velocity to 0.75 (move down right)

@continue:
    lda winged_soldier_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x   ; store enemy's fractional Y velocity
    lda winged_soldier_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x    ; store enemy's fast Y velocity
    lda winged_soldier_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x   ; store enemy's fractional X velocity
    lda winged_soldier_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x    ; store enemy's fast X velocity
    jmp advance_enemy_routine      ; advance to next routine

; !(UNUSED)
bank_a_unused_03:
    .byte $01,$02

winged_soldier_vel_tbl:
    .byte $80,$fe,$40,$ff ; y vel = -1.5, x vel = -0.75
    .byte $80,$fe,$c0,$00 ; y vel = -1.5, x vel =  0.75

; open wings, apply gravity, check collision
winged_soldier_routine_03:
    lda #$6c                       ; sprite_6c
    sta ENEMY_SPRITE,x             ; set sprite for winged soldier facing screen with wings open
    jsr update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_VELOCITY_FRACT,x   ; load enemy's fractional Y velocity
    clc                            ; clear carry in preparation for addition
    adc #$18
    sta ENEMY_Y_VELOCITY_FRACT,x   ; store enemy's fractional Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x    ; load enemy's fast Y velocity
    adc #$00
    sta ENEMY_Y_VELOCITY_FAST,x    ; store enemy's fast Y velocity
    lda #$08
    ldy ENEMY_X_VELOCITY_FAST,x    ; load enemy's fast X velocity
    bpl @check_collision
    lda #$f8

@check_collision:
    jsr check_bg_wall_collision
    beq @continue
    jsr flip_enemy_x_dir        ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25
    inc ENEMY_VAR_1,x           ; increment number of times direction has changed

@continue:
    lda ENEMY_Y_VELOCITY_FAST,x              ; load enemy's fast Y velocity
    cmp #$02
    bcc winged_soldier_exit2                 ; branch if Y velocity isn't really fast
    lda #$12                                 ; really fast falling velocity, add #$12 to Y position when determining bg collision
    jsr get_enemy_bg_collision_code_onscreen ; get bg collision code if on-screen
    beq winged_soldier_exit2                 ; exit if no collision
    jsr set_y_pos_for_bg                     ; set enemy Y position based on background collision
    jsr clear_enemy_vel                      ; set enemy X and Y velocity to 0
    lda #$08
    jmp set_delay_adv_enemy_routine          ; set delay to #$08 and set routine to winged_soldier_routine_04

winged_soldier_exit2:
    rts

; landing on ledge, set facing screen sprite, wait for delay, target player, go to winged_soldier_routine_01
winged_soldier_routine_04:
    jsr winged_soldier_face_screen
    dec ENEMY_DELAY,x
    bne winged_soldier_exit2
    lda #$00
    sta ENEMY_ANIMATION_DELAY,x
    jsr player_enemy_x_dist        ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y
    lda #$00
    bcs @continue
    lda #$01

@continue:
    jsr winged_soldier_set_dir_adv_routine ; set X velocity, sprite attribute based on a, and advances routine
    lda #$02
    jmp set_enemy_routine                  ; set routine to winged_soldier_routine_01

; enemy destroyed routine
winged_soldier_routine_05:
    lda #$6c                             ; winged soldier sprite
    jmp set_enemy_destroy_sprite_and_vel ; set enemy sprite to sprite_6c and set destroyed velocity

winged_soldier_routine_06:
    lda #$6c                                  ; winged soldier sprite
    jmp apply_gravity_adv_routine_after_delay ; set sprite to sprite_6c for fall, apply gravity
                                              ; advance the enemy routine if ENEMY_DELAY has elapsed

winged_soldier_face_screen:
    lda #$6b                                 ; sprite_6b
    sta ENEMY_SPRITE,x                       ; set sprite to winged soldier facing screen
    jsr update_pos_check_offscreen           ; adjust position based on scroll (does not apply velocity)
    lda #$12                                 ; add #$12 to Y position when determining bg collision
    jsr get_enemy_bg_collision_code_onscreen ; get bg collision code if on-screen
    beq @exit
    jsr set_y_pos_for_bg                     ; set enemy Y position based on background collision

@exit:
    rts

; enemy type #$41
winged_soldier_gen_routine_ptr_tbl:
    .addr winged_soldier_gen_routine_00
    .addr winged_soldier_gen_routine_01

winged_soldier_gen_routine_00:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$03
    sta ENEMY_VAR_1,x
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to winged_soldier_gen_routine_01

winged_soldier_gen_routine_01:
    jsr update_enemy_pos       ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x          ; load enemy's Y position
    cmp #$10
    bcc @exit
    cmp #$a0
    bcs @remove_enemy
    dec ENEMY_DELAY,x
    bne @exit
    lda #$30
    dec ENEMY_VAR_1,x
    bne @create_winged_soldier
    lda #$03
    sta ENEMY_VAR_1,x
    lda #$f0

@create_winged_soldier:
    sta ENEMY_DELAY,x
    ldy #$40                           ; enemy type = winged soldier
    jsr try_create_enemy_from_existing ; create winged soldier

@exit:
    rts

@remove_enemy:
    jmp remove_enemy

; enemy type #$43
chandelier_arm_routine_ptr_tbl:
    .addr chandelier_arm_routine_00 ; set sprite attribute, set HP, advance routine
    .addr chandelier_arm_routine_01 ; wait to drop laser, advance routine
    .addr chandelier_arm_routine_02 ; wait for timer, create laser, advance routine
    .addr chandelier_arm_routine_03 ; decrement invincible timer, when elapsed, restore HP, advance routine
    .addr chandelier_arm_routine_04 ; wait for timer, go to chandelier_arm_routine_01
    .addr chandelier_arm_routine_05 ; enemy destroyed routine
    .addr chandelier_arm_routine_06
    .addr chandelier_arm_routine_07

; set sprite attribute, set HP, advance routine
chandelier_arm_routine_00:
    lda #$03
    sta ENEMY_SPRITE_ATTR,x       ; set palette
    lda #$10                      ; set base ENEMY_HP to #$10
    ldy #$06                      ; set enemy_hp_adjust_tbl offset to #$06
    jsr set_enemy_hp_from_a_and_y ; set ENEMY_HP calculated using medium HP difficulty (adjusted by ENEMY_DIFFICULTY)
                                  ; sets enemy HP = #$10, #$14, or #$18
    jmp advance_enemy_routine     ; advance to next routine

; wait to drop laser, advance routine
chandelier_arm_routine_01:
    lda #$01
    jsr chandelier_arm_set_sprite_and_pos
    ldy ENEMY_VAR_5,x                     ; load enemy slot of laser chandelier
    lda ENEMY_VAR_4,y                     ; load index of chandelier arm that is dropping a laser
    bpl chandelier_arm_routine_exit
    cmp #$ff
    beq chandelier_arm_routine_exit       ; exit if chandelier is paused from dropping lasers
    and #$7f                              ; laser arm index is negative and not #$ff
                                          ; strip bit 7 to determine which arm is dropping a laser
    cmp ENEMY_ATTRIBUTES,x                ; see if this chandelier arm should drop the laser
    bne chandelier_arm_routine_exit       ; exit if another arm should drop the laser
    sta ENEMY_VAR_4,y                     ; set to positive
    jmp advance_enemy_routine             ; advance to next routine

chandelier_arm_routine_exit:
    rts

; wait for timer, create laser, advance routine
chandelier_arm_routine_02:
    jsr chandelier_arm_set_sprite_63_and_pos
    inc ENEMY_VAR_1,x                        ; increment arm laser animation timer
    lda ENEMY_VAR_1,x
    cmp #$0f
    bcc chandelier_arm_routine_exit          ; exit if not yet at checkpoint
    ldy #$44                                 ; timer reached checkpoint, create laser
                                             ; enemy type = chandelier arm laser
    jsr try_create_enemy_from_existing       ; create chandelier arm laser
    bcc @continue                            ; branch if unable to create chandelier arm laser
    ldy $11                                  ; load enemy slot of created chandelier arm laser
    txa                                      ; transfer chandelier arm enemy slot to a
    sta ENEMY_VAR_5,y                        ; set to the slot where the chandelier arm is

@continue:
    lda ENEMY_HP,x                  ; backup HP, arm is invincible when dropping laser
    sta ENEMY_VAR_2,x               ; HP will be restored after laser is spawned
    lda #$f0                        ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$10                        ; delay before restoring HP
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to chandelier_arm_routine_03

; decrement invincible timer, when elapsed, restore HP, advance routine
chandelier_arm_routine_03:
    jsr chandelier_arm_set_sprite_63_and_pos
    dec ENEMY_DELAY,x                        ; decrement invincible timer
    bne chandelier_arm_routine_exit          ; exit if delay hasn't elapsed
    lda ENEMY_VAR_2,x
    sta ENEMY_HP,x                           ; restore HP, no longer creating laser and no longer invincible
    jmp advance_enemy_routine                ; advance to next routine

; wait for timer, go to chandelier_arm_routine_01
chandelier_arm_routine_04:
    jsr chandelier_arm_set_sprite_63_and_pos
    dec ENEMY_VAR_1,x                        ; decrement arm laser animation timer
    bne chandelier_arm_routine_exit          ; exit if timer not elapsed
    lda #$02
    jmp set_enemy_routine                    ; set routine to chandelier_arm_routine_01

; enemy destroyed routine
chandelier_arm_routine_05:
    lda ENEMY_VAR_5,x      ; load enemy slot of laser chandelier
    tax
    dec ENEMY_VAR_3,x      ; decrement total number of active chandelier arms
    ldx ENEMY_CURRENT_SLOT ; restore chandelier enemy slot index
    ldy ENEMY_ATTRIBUTES,x ; load chandelier index
    cpy #$08
    bne @set_bit_field     ; branch if not the right-most arm
    ldy ENEMY_VAR_5,x      ; rightmost arm, load laser chandelier enemy slot
    lda #$00
    sta ENEMY_FRAME,y
    beq @draw_destroyed    ; always branch

@set_bit_field:
    lda chandelier_arm_bit_field_tbl,y ; load bit field representation of arms
                                       ; where destroyed arm is 0 and all others are 1
    ldy ENEMY_VAR_5,x                  ; load laser chandelier enemy slot
    and ENEMY_ANIMATION_DELAY,y        ; strip destroyed arm from bit field
    sta ENEMY_ANIMATION_DELAY,y        ; set bit field representing which arms are destroyed
                                       ; 0 = destroyed, 1 = active, one bit per arm
                                       ; note, not used for animation

@draw_destroyed:
    jsr chandelier_arm_set_pos
    jsr chandelier_arm_draw_destroyed
    jmp enemy_explosion_routine_00    ; set empty sprite, play optional enemy destroyed sound, disable collisions

; bit field representing which arms are destroyed
; 0 = destroyed, 1 = active, one bit per arm
chandelier_arm_bit_field_tbl:
    .byte $7f,$bf,$df,$ef,$f7,$fb,$fd,$fe

chandelier_arm_routine_06:
    jsr chandelier_arm_set_pos
    jmp enemy_explosion_routine_01 ; animate explosion sequence

chandelier_arm_routine_07:
    jsr chandelier_arm_set_pos
    jmp enemy_explosion_routine_03 ; mark destroyed, remove enemy

; input
;  * x - enemy slot index
chandelier_arm_set_sprite_63_and_pos:
    lda #$63 ; sprite_63

; input
;  * a - sprite code
;  * x - enemy slot index
chandelier_arm_set_sprite_and_pos:
    sta ENEMY_SPRITE,x

chandelier_arm_set_pos:
    ldy ENEMY_ATTRIBUTES,x         ; load arm number
    lda chandelier_arm_x_adj_tbl,y
    sta $09                        ; set X offset from enemy center
    asl
    lda #$00
    bcc @continue                  ; branch if right of center
    lda #$ff                       ; left of center subtract

@continue:
    sta $0b           ; decrement/increment based on left/right of center
    ldy ENEMY_VAR_5,x ; load enemy slot of laser chandelier
    lda $09           ; load X offset from center
    clc               ; clear carry in preparation for addition
    adc ENEMY_X_POS,y ; add to chandelier arm X position
    sta ENEMY_X_POS,x ; set X position
    lda $0b           ; load whether incrementing or decrementing
    adc ENEMY_VAR_7,y ; add to overflow/underflow accumulator
                      ; happens when center is off screen
    sta ENEMY_VAR_7,x ; set if any overflow or underflow
    bne @clear_sprite
    lda #$18
    clc               ; clear carry in preparation for addition
    adc ENEMY_VAR_1,x
    clc               ; clear carry in preparation for addition
    adc ENEMY_Y_POS,y
    sta ENEMY_Y_POS,x
    lda #$00
    adc ENEMY_VAR_6,y
    sta ENEMY_VAR_6,x
    bne @clear_sprite
    rts

@clear_sprite:
    jmp clear_enemy_sprite

chandelier_arm_x_adj_tbl:
    .byte $c0,$d0,$e0,$f0,$00,$10,$20,$30,$40

chandelier_arm_draw_destroyed:
    lda ENEMY_ATTRIBUTES,x
    asl
    sta $08
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET

@loop:
    lda chandelier_arm_tile_tbl,y
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny
    cmp #$ff
    bne @loop
    stx GRAPHICS_BUFFER_OFFSET
    lda CPU_GRAPHICS_BUFFER-5,x
    clc                           ; clear carry in preparation for addition
    adc $08
    sta CPU_GRAPHICS_BUFFER-5,x
    ldx ENEMY_CURRENT_SLOT
    rts

chandelier_arm_tile_tbl:
    .byte $07     ; block mode
    .byte $20,$87 ; PPU address $2087
    .byte $02     ; block size
    .byte $fe,$00
    .byte $ff     ; end of graphics block

; enemy type #$44
chandelier_arm_laser_routine_ptr_tbl:
    .addr chandelier_arm_laser_routine_00
    .addr chandelier_arm_laser_routine_01
    .addr chandelier_arm_laser_routine_02

chandelier_arm_laser_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x    ; mark chandelier arm laser so bullets travel through it
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    clc                          ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_Y_POS,x
    sta ENEMY_VAR_1,x
    lda #$00
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda #$08
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda #$10
    jsr play_sound
    jmp advance_enemy_routine    ; advance to next routine

chandelier_arm_laser_routine_01:
    jsr chandelier_arm_laser_set_x_pos
    lda #$d2
    jsr chandelier_arm_laser_draw
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    cmp #$e0
    bcs @continue
    lda #$00                           ; don't adjust enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code    ; test background collision
    beq chandelier_arm_exit

@continue:
    lda ENEMY_VAR_1,x
    sta ENEMY_Y_POS,x
    jmp advance_enemy_routine ; advance to next routine

chandelier_arm_exit:
    rts

chandelier_arm_laser_routine_02:
    jsr chandelier_arm_laser_set_x_pos
    lda #$00
    jsr chandelier_arm_laser_draw
    lda #$00                           ; don't adjust enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code    ; test background collision
    beq chandelier_arm_exit
    jmp remove_enemy

chandelier_arm_laser_set_x_pos:
    lda #$01
    sta ENEMY_SPRITE,x ; set invisible sprite
    jsr apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen
    ldy ENEMY_VAR_5,x  ; load chandelier arm that the laser is falling from
    lda ENEMY_X_POS,y  ; load chandelier arm X position
    sta ENEMY_X_POS,x  ; set chandelier arm laser X position
    rts

chandelier_arm_laser_draw:
    sta $08
    lda ENEMY_X_POS,x        ; load enemy's X position
    clc                      ; clear carry in preparation for addition
    adc IRQ_X_SCROLL         ; add X scroll amount
    lsr
    lsr
    lsr
    sta $00
    lda IRQ_PPUCTRL_SETTINGS
    lsr
    lsr
    lda #$08
    bcc @continue
    lda #$0a

@continue:
    sta $01
    lda ENEMY_Y_POS,x ; load enemy's Y position
    clc               ; clear carry in preparation for addition
    adc IRQ_Y_SCROLL
    bcs @calc_y
    cmp #$f0
    bcc @draw_laser

@calc_y:
    adc #$0f
    tay
    lda $01
    eor #$02
    sta $01
    tya

@draw_laser:
    and #$f8
    asl
    rol $01
    asl
    rol $01
    clc                        ; clear carry in preparation for addition
    adc $00
    sta $00
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                   ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x
    inx
    lda $01
    sta CPU_GRAPHICS_BUFFER,x
    inx
    lda $00
    sta CPU_GRAPHICS_BUFFER,x
    inx
    lda #$01
    sta CPU_GRAPHICS_BUFFER,x
    inx
    lda $08
    sta CPU_GRAPHICS_BUFFER,x
    inx
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x
    inx
    stx GRAPHICS_BUFFER_OFFSET
    ldx ENEMY_CURRENT_SLOT
    rts

; enemy type #$6b
ceiling_vent_bubble_routine_ptr_tbl:
    .addr ceiling_vent_bubble_routine_00 ; set sprite and random velocity
    .addr ceiling_vent_bubble_routine_01
    .addr ceiling_vent_bubble_routine_02
    .addr enemy_explosion_routine_00     ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01     ; animate explosion sequence
    .addr enemy_explosion_routine_03     ; mark destroyed, remove enemy

; set sprite and random velocity
ceiling_vent_bubble_routine_00:
    lda #$5e                              ; sprite_5e
    sta ENEMY_SPRITE,x                    ; set blue bubble sprite
    lda RANDOM_NUM                        ; load random number
    and #$07                              ; random number between 0 and #$07
    asl                                   ; double since each entry is #$02 bytes
    tay                                   ; transfer to offset registers
    lda ceiling_vent_bubble_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x          ; store enemy's fractional Y velocity
    lda ceiling_vent_bubble_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x           ; store enemy's fast Y velocity
    lda RANDOM_NUM                        ; load random number
    lsr
    lsr
    lsr
    lsr                                   ; move high nibble to low nibble
    pha                                   ; backup a on stack
    and #$03                              ; random number between 0 and #$03
    asl                                   ; double since each entry is #$02 bytes
    tay                                   ; transfer to offset register
    lda ceiling_vent_bubble_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x          ; store enemy's fractional X velocity
    lda ceiling_vent_bubble_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x           ; store enemy's fast X velocity
    pla                                   ; pop random number between 0 and #$0f from stack
    lsr
    lsr
    bcc @continue                         ; branch if not randomly flipping X velocity
    jsr flip_enemy_x_dir                  ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

@continue:
    jmp advance_enemy_routine ; advance to next routine

ceiling_vent_bubble_y_vel_tbl:
    .byte $00,$08 ; Y velocity = 8.00
    .byte $00,$07 ; Y velocity = 7.00
    .byte $00,$06 ; Y velocity = 6.00
    .byte $00,$05 ; Y velocity = 5.00
    .byte $80,$04 ; Y velocity = 4.50
    .byte $40,$07 ; Y velocity = 7.25
    .byte $80,$06 ; Y velocity = 6.50
    .byte $80,$05 ; Y velocity = 5.50

ceiling_vent_bubble_x_vel_tbl:
    .byte $40,$00 ; X velocity = 0.250
    .byte $60,$00 ; X velocity = 0.375
    .byte $80,$00 ; X velocity = 0.500
    .byte $c0,$00 ; X velocity = 0.750

ceiling_vent_bubble_routine_01:
    lda ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @apply_float_vel        ; branch if falling
    cmp #$ff                    ; bubble rising, see if -1
    beq @continue               ; branch if rising at Y velocity of -1

; apply upwards velocity
@apply_float_vel:
    lda ENEMY_Y_VELOCITY_FRACT,x ; load enemy's fractional Y velocity
    sec                          ; set carry flag in preparation for subtraction
    sbc #$80
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    sbc #$00
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity

@continue:
    jsr apply_velocity                    ; apply enemy's velocity to its position, removing enemy if off-screen
    jsr ceiling_vent_bubble_get_collision
    beq @check_x
    jsr flip_enemy_y_dir                  ; flip the sign of the enemy's Y velocity, e.g. 1.25 becomes -1.25
    jmp advance_enemy_routine             ; advance to next routine

@check_x:
    jsr ceiling_vent_bubble_check_x_bounds
    beq ceiling_vent_bubble_exit
    jmp flip_enemy_x_dir                   ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

ceiling_vent_bubble_exit:
    rts

ceiling_vent_bubble_routine_02:
    jsr apply_velocity                    ; apply enemy's velocity to its position, removing enemy if off-screen
    jsr ceiling_vent_bubble_get_collision
    beq @continue
    jsr flip_enemy_y_dir                  ; flip the sign of the enemy's Y velocity, e.g. 1.25 becomes -1.25

@continue:
    jsr ceiling_vent_bubble_check_x_bounds
    beq ceiling_vent_bubble_exit           ; exit if no collision
    jmp flip_enemy_x_dir                   ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

ceiling_vent_bubble_get_collision:
    lda #$04
    ldy ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @check_collision
    lda #$fc

@check_collision:
    clc                  ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x
    tay
    lda ENEMY_X_POS,x    ; load enemy's X position
    jmp get_bg_collision

ceiling_vent_bubble_check_x_bounds:
    lda ENEMY_X_VELOCITY_FAST,x           ; load enemy's fast X velocity
    rol
    rol
    and #$01
    tay
    lda ENEMY_X_POS,x                     ; load enemy's X position
    cmp ceiling_vent_bubble_x_bound_tbl,y
    ror
    eor ENEMY_X_VELOCITY_FAST,x
    bmi @exit
    lda #$00

@exit:
    rts

ceiling_vent_bubble_x_bound_tbl:
    .byte $f0,$10

; enemy type #$6c
ceiling_vent_routine_ptr_tbl:
    .addr ceiling_vent_routine_00    ; set HP, correct X position, set to create #$14 bubbles total, set delay, advance routine
    .addr ceiling_vent_routine_01    ; update position, wait for delay, create bubble, check if created all bubbles
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set HP, correct X position, set to create #$14 bubbles total, set delay, advance routine
ceiling_vent_routine_00:
    lda #$01                        ; !(HUH) immediately overwritten
    lda #$20
    sta ENEMY_HP,x                  ; set hp to #$20
    lda #$14
    sta ENEMY_VAR_1,x               ; create #$14 bubbles total before removing self
    lda ENEMY_X_POS,x               ; load enemy's X position
    sec                             ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_X_POS,x               ; set corrected X position
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to ceiling_vent_routine_01

; update position, wait for delay, create bubble, check if created all bubbles
ceiling_vent_routine_01:
    jsr update_enemy_pos               ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                  ; decrement bubble creation delay
    bne @exit                          ; exit if bubble creation delay hasn't elapsed
    lda #$14
    sta ENEMY_DELAY,x                  ; set next round of bubble creation delay
    ldy #$6b                           ; enemy type = bubbles
    jsr try_create_enemy_from_existing ; create bubble with HP of #$01
    bcc @exit                          ; exit if unable to create bubble
    dec ENEMY_VAR_1,x                  ; decrement remaining number of bubbles to create
    bne @exit                          ; exit if more bubble(s) to create
    jmp remove_enemy                   ; created all bubbles, remove enemy

@exit:
    rts

; enemy type #$42
laser_chandelier_routine_ptr_tbl:
    .addr laser_chandelier_routine_00    ; set enemy destroy attributes
    .addr laser_chandelier_routine_01    ; wait for elevator to stop, play boss music
    .addr laser_chandelier_routine_02
    .addr laser_chandelier_routine_03
    .addr laser_chandelier_routine_04
    .addr enemy_routine_boss_defeated_00 ; enemy destroyed routine
    .addr enemy_routine_boss_defeated_01
    .addr bg_enemy_explosion_routine_00  ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr bg_enemy_explosion_routine_01  ; animate sequence of explosions
    .addr laser_chandelier_routine_09

; set enemy destroy attributes
laser_chandelier_routine_00:
    lda #$10
    sta ENEMY_DESTROY_ATTRS,x ; enable collision, set destroy sound to sound_26 (B OUT) - boss destroy
    jmp advance_enemy_routine ; advance to next routine

laser_chandelier_exit:
    rts

; wait for elevator to stop, play boss music
laser_chandelier_routine_01:
    lda ELEVATOR_ENABLED
    ora X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    ora ELEVATOR_FAST_VEL
    bne laser_chandelier_exit ; exit if elevator is still active
    lda #$c8
    sta Y_AUTOSCROLL_STOP_POS
    lda #$31                  ; sound_31 (BOSS BGM) - Ruined Base
    jsr play_sound            ; play boss background music
    jmp advance_enemy_routine ; advance to next routine

; set scanline interrupt points
laser_chandelier_routine_02:
    lda Y_AUTOSCROLL_STOP_POS
    bne laser_chandelier_exit
    lda #$c0
    sta Y_SCROLL_FLAGS
    lda #$7c
    sta ENEMY_X_POS,x         ; set enemy X position
    lda #$38
    sta ENEMY_Y_POS,x         ; set enemy Y position
    lda #$00
    sta ENEMY_VAR_6,x
    lda #$01
    sta ENEMY_VAR_7,x
    jsr init_irq_scroll       ; set post-irq scroll to current scroll
    lda #$05                  ; table contains same pointer twice (same as position #$04)
    sta IRQ_TYPE              ; set irq routine type to irq_handler_04_ptr_tbl
                              ; level 4 laser chandelier
    .ifdef Probotector
        lda #$0c
    .else
        lda #$18
    .endif
    sta SCANLINE_IRQ_1        ; first interrupt occurs at scanline #$18
    lda #$01
    sta SCANLINE_IRQ_2_DIFF   ; set the number of scanlines after SCANLINE_IRQ_1 to run the next scanline IRQ
    lda #$ab
    sta SCANLINE_IRQ_3_DIFF   ; set the number of scanlines after SCANLINE_IRQ_2 to run the next scanline IRQ
    lda #$00
    sta ENEMY_VAR_3,x         ; initialize number of active arms to 0
                              ; will be incremented as chandelier arms are created
    ldy #$08                  ; attempt to create all #$08 arms

@create_arm_loop:
    sty $08                            ; set remaining number of arms to create
    lda #$43                           ; enemy type = chandelier arm
    tay
    jsr try_create_enemy_from_existing ; create chandelier arm
    bcc @continue                      ; branch to break loop if unable to create chandelier arm
    inc ENEMY_VAR_3,x                  ; created chandelier arm, increment total number of active arms
    ldy $11                            ; load slot of created arm
    lda $08                            ; load arm number
    sta ENEMY_ATTRIBUTES,y             ; set chandelier arm's number as its attribute
    txa
    sta ENEMY_VAR_5,y                  ; set chandelier arm's variable to enemy slot of laser chandelier
    ldy $08                            ; load remaining number of arms to create
    dey                                ; decrement remaining number of arms
    bpl @create_arm_loop               ; branch if more arms to create

@continue:
    lda #$ff
    sta ENEMY_FRAME,x
    sta ENEMY_ANIMATION_DELAY,x ; set all laser arms as active (bit field of all 1s)
                                ; not used for animation delay
    lda #$02
    sta SCREEN_SCROLL_TYPE      ; set special boss screen scroll
    jsr reset_draw_point
    lda #$08
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine   ; advance to next routine

laser_chandelier_routine_03:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne laser_chandelier_exit2
    dec ENEMY_VAR_1,x                     ; change mode
    beq laser_chandelier_slide_into_level ; mode is now zero
    lda #$00
    sta X_SCROLL_DRAW_POINT

laser_chandelier_exit2:
    rts

; initializes variables for having boss come into level from right
laser_chandelier_slide_into_level:
    jsr clear_bg_collision_data
    lda #$00
    sta ENEMY_VAR_1,x            ; set mode to #$00 - coming into level from right
    lda #$09
    sta ENEMY_VAR_4,x            ; set rightmost arm to fire a laser
    lda #$40
    sta ENEMY_FIRING_DELAY,x     ; set delay before moving to next arm
    lda #$00
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda #$f9
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity (-7)
    jmp advance_enemy_routine    ; advance to next routine

laser_chandelier_routine_04:
    jsr laser_chandelier_run_subroutine
    jsr laser_chandelier_set_scroll     ; set scroll and base nametable for rendering chandelier
    lda ENEMY_VAR_3,x                   ; load remaining number of undestroyed arms
    bne laser_chandelier_exit2          ; exit if any active arms
    jmp advance_enemy_routine           ; all arms destroyed, advance to next routine (destroy routine)

laser_chandelier_routine_09:
    jsr set_nmi_noop_irq
    jsr reset_scroll_draw_point
    inc DRAW_X_SCREEN
    lda #$08
    sta LEFT_TOP_HALF_CHR_BANK
    lda #$0a
    sta LEFT_BOTTOM_CHR_HALF_BANK
    jmp set_boss_defeated_remove_enemy

laser_chandelier_set_x_vel:
    lda ENEMY_VAR_3,x                  ; load remaining number of undestroyed arms
    and #$0e
    tay
    lda laser_chandelier_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x       ; store enemy's fractional X velocity
    lda laser_chandelier_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x        ; store enemy's fast X velocity
    rts

laser_chandelier_x_vel_tbl:
    .byte $c0,$00 ; 0.750
    .byte $a0,$00 ; 0.625
    .byte $80,$00 ; 0.500
    .byte $60,$00 ; 0.375
    .byte $40,$00 ; 0.250

laser_chandelier_run_subroutine:
    lda ENEMY_VAR_1,x
    jsr run_routine_from_tbl_below

laser_chandelier_subroutine_ptr_tbl:
    .addr laser_chandelier_subroutine_00 ; slide into level from the right
    .addr laser_chandelier_subroutine_01 ; stopped at left edge
    .addr laser_chandelier_subroutine_02 ; move right
    .addr laser_chandelier_subroutine_01 ; stopped at right edge
    .addr laser_chandelier_subroutine_04 ; move left

; slide into level from the right
laser_chandelier_subroutine_00:
    lda ENEMY_X_VELOCITY_FRACT,x                  ; load enemy's fractional X velocity
    clc                                           ; clear carry in preparation for addition
    adc #$16
    sta ENEMY_X_VELOCITY_FRACT,x                  ; store enemy's fractional X velocity
    lda ENEMY_X_VELOCITY_FAST,x                   ; load enemy's fast X velocity
    adc #$00
    sta ENEMY_X_VELOCITY_FAST,x                   ; store enemy's fast X velocity
    bmi laser_chandelier_subroutine_exit
    lda #$08
    bne laser_chandelier_set_delay_adv_subroutine

; stopped
laser_chandelier_subroutine_01:
    jsr clear_enemy_vel                 ; set enemy X and Y velocity to 0
    jsr laser_chandelier_set_laser_arm
    dec ENEMY_DELAY,x
    beq laser_chandelier_adv_subroutine

laser_chandelier_subroutine_exit:
    rts

; move right
laser_chandelier_subroutine_02:
    jsr laser_chandelier_set_laser_arm
    jsr laser_chandelier_set_x_vel     ; set X velocity based on number of undestroyed arms
    ldy #$00
    lda ENEMY_FRAME,x
    bne @continue
    lda ENEMY_ANIMATION_DELAY,x        ; load bit field representing which arms are destroyed
                                       ; 0 = destroyed, 1 = active, one bit per arm
                                       ; note, not used for animation

; shift right looking for rightmost active arm
@loop:
    iny
    cpy #$08
    bcs @continue ; branch if found rightmost active arm
    lsr           ; arm destroyed, move to next arm to the left
    bcc @loop

@continue:
    lda ENEMY_VAR_7,x
    cmp laser_chandelier_offscreen_tbl,y
    bne laser_chandelier_subroutine_exit
    lda ENEMY_X_POS,x                        ; load enemy's X position
    cmp laser_chandelier_right_x_bound_tbl,y
    bcc laser_chandelier_subroutine_exit
    lda #$30                                 ; rightmost active arm is at edge, advance subroutine to stop

laser_chandelier_set_delay_adv_subroutine:
    sta ENEMY_DELAY,x

laser_chandelier_adv_subroutine:
    inc ENEMY_VAR_1,x
    rts

; move left
laser_chandelier_subroutine_04:
    jsr laser_chandelier_set_laser_arm
    jsr laser_chandelier_set_x_vel     ; set X velocity based on number of undestroyed arms
    jsr flip_enemy_x_dir               ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25
    ldy #$ff
    lda ENEMY_ANIMATION_DELAY,x        ; load bit field representing which arms are destroyed
                                       ; 0 = destroyed, 1 = active, one bit per arm
                                       ; note, not used for animation

; shift left looking for leftmost active arm
@loop:
    iny
    cpy #$08
    bcs @continue ; branch if arm is 1, meaning found active arm
    asl           ; arm is destroyed, check next arm to the right
    bcc @loop

@continue:
    lda ENEMY_VAR_7,x
    cmp laser_chandelier_arm_overflow_tbl,y
    bne laser_chandelier_subroutine_exit    ; exit if off screen
    lda ENEMY_X_POS,x                       ; load laser chandelier's X position
    cmp laser_chandelier_left_x_bound_tbl,y
    bcs laser_chandelier_subroutine_exit    ; branch if x position is too far to the right
    lda #$30                                ; leftmost active arm is at left edge, advance subroutine to pause before going right
    sta ENEMY_DELAY,x
    lda #$01
    sta ENEMY_VAR_1,x                       ; set laser_chandelier_subroutine_01
    rts

laser_chandelier_offscreen_tbl:
    .byte $00,$00,$00,$00,$00,$01,$01,$01,$01

laser_chandelier_right_x_bound_tbl:
    .byte $b0,$c0,$d0,$e0,$f0,$00,$10,$20,$30

laser_chandelier_arm_overflow_tbl:
    .byte $00,$00,$00,$00,$00,$ff,$ff,$ff,$ff

laser_chandelier_left_x_bound_tbl:
    .byte $50,$40
    .byte $30,$20,$10,$ff,$f0,$e0,$d0

laser_chandelier_set_laser_arm:
    dec ENEMY_FIRING_DELAY,x ; decrement laser index timer
    bne @exit                ; exit if timer not elapsed
    lda ENEMY_VAR_4,x        ; load active laser arm
    cmp #$ff
    beq @continue            ; brach if #$ff
    and #$7f

@continue:
    sta ENEMY_VAR_4,x
    lda #$14
    ldy ENEMY_VAR_3,x ; load remaining number of laser arms
    cpy #$06
    bcs @set_delay    ; branch if 6 or more laser arms active
    lda #$10          ; 5 or fewer laser arms, use shorter delay (drop lasers faster)

@set_delay:
    sta ENEMY_FIRING_DELAY,x ; set laser arm firing delay
    lda ENEMY_VAR_2,x        ; load laser arm firing direction
    lsr
    lda #$ff                 ; load default direction (right to left)
    bcc @advance             ; branch if right to left
    lda #$01                 ; left to right

@advance:
    clc                      ; clear carry in preparation for addition
    adc ENEMY_VAR_4,x        ; move to next laser arm index (add or subtract 1)
    cmp #$09
    bcc @set_activate        ; branch if laser arm index valid
    tay                      ; laser arm index valid, swap laser arm drop direction
    lda ENEMY_VAR_2,x
    eor #$01
    sta ENEMY_VAR_2,x        ; swap laser arm firing direction
    lda #$30
    sta ENEMY_FIRING_DELAY,x ; set longer firing delay
    tya
    jmp @set_laser_arm_index ; set laser arm index
                             ; will be invalid index, and next loop will increment/decrement

@set_activate:
    ora #$80 ; mark that the laser arm should activate

@set_laser_arm_index:
    sta ENEMY_VAR_4,x

@exit:
    rts

; set scroll and base nametable for rendering chandelier
laser_chandelier_set_scroll:
    jsr bg_boss_apply_vel
    lda #$7c
    sec                   ; set carry flag in preparation for subtraction
    sbc ENEMY_X_POS,x
    sta IRQ_X_SCROLL      ; set scroll to emulate enemy moving
                          ; by moving the scroll in the opposite direction
    rol                   ; rotate carry to bit 0
    eor ENEMY_VAR_7,x
    lsr
    lda #$a8              ; base nametable $2000 (top left)
    bcs @set_ppu_exit
    lda #$a9              ; base nametable $2400 (top right)

@set_ppu_exit:
    sta IRQ_PPUCTRL_SETTINGS ; set nametable to use after drawing the ceiling tiles
                             ; (1st scanline interrupt)
    rts

; !(UNUSED) - changes palette colors for laser chandelier arm lasers
; lasers become blue
laser_chandelier_set_laser_palette:
    lda #$14
    sta $08
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$40
    bcs @exit

@loop:
    lda laser_chandelier_unused_attr_tbl,y
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny
    dec $08
    bne @loop
    stx GRAPHICS_BUFFER_OFFSET
    clc

@exit:
    ldx ENEMY_CURRENT_SLOT
    rts

laser_chandelier_unused_attr_tbl:
    .byte $03                 ; repeat mode
    .byte $23,$d8             ; PPU address $23d8 (attribute table 0)
    .byte $12,$aa             ; repeat #$aa #$12 times
    .byte $06                 ; block mode
    .byte $23,$ea             ; PPU address $23ea (attribute table 0)
    .byte $05                 ; block size
    .byte $ae,$fa,$fa,$fa,$fa ; attribute bytes
    .byte $ff                 ; end of block mode
    .byte $03                 ; repeat mode
    .byte $23,$ef             ; PPU address $23ef (attribute table 0)
    .byte $08,$aa             ; repeat #$aa #$08 times

level_7_screen_layout_tbl:
    .byte $02     ; LEVEL_WIDTH
    .byte $0e     ; LEVEL_HEIGHT
    .byte $01,$00
    .byte $02,$00
    .byte $03,$00
    .byte $04,$00
    .byte $05,$00
    .byte $06,$00
    .byte $07,$00
    .byte $08,$00
    .byte $09,$00
    .byte $0a,$00
    .byte $0b,$00
    .byte $0c,$00
    .byte $0d,$00
    .byte $0e,$00

level_7_supertiles_screen_ptr_table:
    .addr level_7_supertiles_screen_00
    .addr level_7_supertiles_screen_01
    .addr level_7_supertiles_screen_02
    .addr level_7_supertiles_screen_03
    .addr level_7_supertiles_screen_04
    .addr level_7_supertiles_screen_05
    .addr level_7_supertiles_screen_06
    .addr level_7_supertiles_screen_07
    .addr level_7_supertiles_screen_08
    .addr level_7_supertiles_screen_09
    .addr level_7_supertiles_screen_0a
    .addr level_7_supertiles_screen_0b
    .addr level_7_supertiles_screen_0c
    .addr level_7_supertiles_screen_0d
    .addr level_7_supertiles_screen_0e

level_7_supertiles_screen_00:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_7_supertiles_screen_01:
    .byte $38,$38,$38,$38,$38,$38,$38,$38,$39,$39,$39,$39,$39,$39,$39,$39
    .byte $3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f
    .byte $40,$40,$48,$48,$48,$48,$40,$40,$0b,$09,$0a,$0b,$00,$0b,$0b,$0b
    .byte $02,$01,$03,$02,$10,$03,$02,$03,$01,$03,$02,$03,$10,$01,$0e,$02

level_7_supertiles_screen_02:
    .byte $02,$01,$02,$01,$06,$08,$02,$01,$03,$02,$03,$02,$03,$05,$03,$02
    .byte $02,$03,$02,$01,$02,$05,$02,$03,$01,$02,$03,$02,$02,$07,$03,$02
    .byte $03,$02,$03,$02,$06,$08,$02,$03,$03,$02,$03,$0f,$04,$02,$03,$02
    .byte $0d,$0e,$0f,$04,$00,$02,$02,$03,$00,$00,$00,$00,$0b,$03,$03,$02

level_7_supertiles_screen_03:
    .byte $3c,$37,$3c,$00,$0d,$0e,$0e,$0d,$20,$21,$37,$3c,$3c,$3c,$3c,$3c
    .byte $24,$25,$20,$21,$38,$38,$38,$38,$22,$22,$24,$25,$26,$27,$39,$39
    .byte $22,$22,$28,$29,$2a,$2b,$49,$3a,$29,$23,$3c,$3c,$3b,$3b,$3b,$46
    .byte $37,$37,$37,$37,$37,$12,$13,$16,$37,$37,$37,$3d,$13,$16,$17,$1b

level_7_supertiles_screen_04:
    .byte $40,$40,$14,$15,$16,$17,$11,$11,$40,$40,$18,$19,$1a,$1b,$11,$11
    .byte $21,$40,$40,$3e,$3e,$44,$1f,$1a,$25,$20,$21,$47,$39,$39,$39,$39
    .byte $22,$24,$25,$20,$42,$4b,$3a,$3a,$22,$22,$28,$24,$2c,$2d,$4a,$3b
    .byte $29,$23,$3c,$3c,$3c,$3c,$37,$37,$3c,$37,$37,$37,$37,$37,$37,$37

level_7_supertiles_screen_05:
    .byte $40,$40,$40,$2e,$30,$30,$30,$30,$40,$40,$43,$2f,$34,$31,$32,$32
    .byte $20,$21,$40,$40,$40,$33,$35,$36,$24,$25,$20,$21,$40,$3e,$3e,$3e
    .byte $22,$22,$24,$25,$26,$27,$47,$39,$22,$22,$28,$29,$2a,$2b,$49,$3a
    .byte $29,$23,$3c,$40,$3f,$3f,$3f,$46,$40,$40,$40,$40,$40,$40,$13,$16

level_7_supertiles_screen_06:
    .byte $37,$37,$37,$37,$12,$13,$16,$17,$37,$37,$14,$15,$16,$17,$11,$11
    .byte $37,$3d,$18,$19,$1a,$1b,$11,$11,$21,$3e,$3e,$3e,$3e,$44,$1f,$1a
    .byte $25,$20,$41,$47,$39,$39,$39,$39,$22,$24,$25,$20,$42,$4b,$3a,$3a
    .byte $22,$22,$22,$24,$2c,$2d,$4a,$3b,$22,$29,$23,$3c,$3c,$3c,$37,$37

level_7_supertiles_screen_07:
    .byte $29,$23,$40,$48,$48,$48,$48,$40,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $62,$00,$00,$00,$00,$00,$00,$63,$5d,$4c,$4d,$00,$00,$56,$57,$5e
    .byte $5d,$50,$4e,$4f,$53,$54,$55,$5e,$5d,$50,$52,$52,$52,$51,$55,$5e
    .byte $5d,$50,$51,$52,$51,$51,$55,$5e,$5d,$00,$00,$00,$00,$00,$00,$5e

level_7_supertiles_screen_08:
    .byte $5d,$50,$4c,$4d,$00,$4f,$55,$5e,$5d,$51,$51,$57,$50,$51,$55,$5e
    .byte $5d,$50,$51,$51,$52,$51,$55,$5e,$5d,$50,$52,$52,$52,$52,$55,$5e
    .byte $5d,$50,$51,$52,$52,$51,$55,$5e,$5d,$50,$50,$51,$51,$51,$5a,$68
    .byte $5c,$58,$59,$52,$5a,$5b,$5f,$11,$22,$22,$6a,$52,$6b,$11,$11,$11

level_7_supertiles_screen_09:
    .byte $22,$22,$5d,$51,$5e,$11,$11,$11,$22,$22,$5d,$50,$5e,$11,$11,$11
    .byte $22,$22,$5d,$51,$5e,$11,$11,$11,$22,$22,$5d,$50,$5e,$11,$11,$11
    .byte $22,$22,$5d,$51,$5e,$11,$11,$11,$22,$22,$5d,$50,$5e,$11,$11,$11
    .byte $22,$28,$5d,$51,$5e,$11,$11,$11,$22,$22,$5d,$54,$5e,$11,$11,$11

level_7_supertiles_screen_0a:
    .byte $22,$66,$60,$00,$61,$67,$11,$11,$64,$3c,$3c,$37,$37,$37,$65,$1a
    .byte $3c,$37,$6c,$69,$6d,$37,$37,$3c,$37,$37,$37,$37,$37,$37,$37,$6e
    .byte $37,$37,$37,$37,$6c,$69,$69,$6d,$6e,$6e,$37,$3d,$3c,$3c,$3c,$3c
    .byte $6c,$69,$69,$69,$6d,$37,$37,$37,$37,$37,$37,$37,$37,$37,$37,$37

level_7_supertiles_screen_0b:
    .byte $37,$37,$37,$37,$6e,$37,$37,$37,$37,$37,$37,$37,$6c,$69,$69,$6d
    .byte $37,$6e,$37,$37,$6e,$3c,$3c,$3c,$6c,$69,$69,$69,$69,$6d,$37,$37
    .byte $37,$37,$37,$37,$37,$37,$37,$37,$37,$37,$6e,$37,$37,$37,$3d,$6e
    .byte $37,$6c,$69,$69,$69,$69,$69,$6d,$37,$3c,$3c,$3c,$3c,$3c,$3c,$3c

level_7_supertiles_screen_0c:
    .byte $37,$37,$37,$37,$37,$37,$37,$37,$37,$3d,$6e,$37,$6e,$37,$37,$37
    .byte $6c,$69,$69,$69,$69,$6d,$37,$37,$3c,$3c,$3c,$3c,$3c,$3c,$37,$37
    .byte $3c,$37,$37,$6e,$37,$37,$37,$6e,$37,$37,$6c,$69,$69,$69,$69,$6d
    .byte $37,$37,$3c,$3c,$3c,$3c,$3c,$3c,$00,$00,$00,$00,$00,$00,$00,$00

level_7_supertiles_screen_0d:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_7_supertiles_screen_0e:
    .byte $00,$71,$72,$73,$74,$75,$76,$00,$00,$77,$78,$79,$7a,$7b,$00,$00
    .byte $7c,$7d,$7e,$7f,$80,$81,$82,$83,$00,$84,$85,$86,$87,$88,$89,$00
    .byte $00,$8a,$8b,$8c,$8d,$8e,$8f,$00,$90,$91,$92,$93,$94,$95,$96,$97
    .byte $98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0

level_7_supertile_data:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $17,$5d,$18,$5e,$5b,$5c,$5f,$5a,$20,$58,$21,$59,$16,$19,$1a,$1b
    .byte $3a,$1c,$1d,$3b,$3c,$3d,$3e,$3f,$22,$40,$23,$41,$42,$43,$44,$45
    .byte $1e,$46,$1f,$60,$61,$62,$63,$64,$24,$65,$25,$66,$67,$68,$69,$6a
    .byte $03,$00,$00,$00,$71,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $03,$00,$00,$73,$71,$00,$00,$74,$04,$00,$00,$75,$72,$00,$00,$76
    .byte $03,$00,$00,$00,$71,$00,$00,$00,$04,$00,$00,$00,$72,$00,$00,$00
    .byte $01,$00,$00,$6d,$6b,$00,$00,$00,$02,$00,$00,$00,$6c,$00,$00,$70
    .byte $00,$00,$00,$6d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$70
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$77,$00,$00,$7a
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7b,$00,$78,$79
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7a,$7b,$78,$79
    .byte $03,$00,$00,$76,$71,$00,$00,$6d,$04,$00,$00,$6f,$72,$00,$00,$70
    .byte $3a,$1c,$1d,$3b,$3c,$3d,$3e,$3f,$22,$40,$23,$41,$42,$43,$44,$7c
    .byte $1e,$46,$1f,$60,$61,$62,$63,$64,$24,$65,$25,$66,$05,$7d,$06,$7e
    .byte $1e,$46,$1f,$60,$61,$62,$63,$64,$24,$65,$25,$66,$05,$7d,$06,$7f
    .byte $03,$00,$00,$73,$71,$00,$00,$74,$04,$00,$00,$75,$00,$00,$00,$00
    .byte $2a,$8e,$2b,$8f,$2c,$90,$2d,$91,$2b,$8f,$2a,$8e,$2d,$91,$2c,$90
    .byte $0c,$b5,$0d,$b6,$b7,$b8,$00,$00,$00,$80,$51,$81,$82,$83,$84,$4d
    .byte $00,$80,$51,$81,$82,$83,$84,$4d,$4d,$81,$4f,$85,$86,$87,$50,$88
    .byte $0c,$b5,$0d,$00,$b7,$b8,$00,$00,$00,$80,$51,$81,$82,$83,$84,$4e
    .byte $00,$80,$51,$81,$82,$83,$84,$4d,$4d,$81,$4f,$85,$8b,$50,$88,$8a
    .byte $4d,$81,$4f,$85,$86,$87,$50,$88,$26,$89,$27,$28,$29,$8a,$28,$92
    .byte $26,$89,$27,$28,$29,$8a,$28,$92,$2b,$8f,$2a,$8e,$2d,$91,$2c,$90
    .byte $4e,$8c,$50,$88,$8d,$29,$8a,$28,$00,$00,$00,$00,$b7,$b8,$b9,$ba
    .byte $29,$8a,$28,$92,$93,$0b,$0b,$94,$00,$00,$00,$00,$b7,$b8,$b9,$ba
    .byte $2a,$8e,$2b,$8f,$2c,$90,$2d,$91,$00,$93,$07,$8e,$00,$00,$00,$93
    .byte $2a,$8e,$2b,$8f,$2c,$90,$2d,$91,$2a,$8e,$2a,$8e,$0b,$94,$0b,$94
    .byte $00,$80,$51,$81,$82,$83,$84,$4e,$4e,$8c,$50,$88,$8d,$29,$8a,$28
    .byte $4d,$81,$4f,$85,$8b,$50,$88,$8a,$29,$8a,$28,$92,$93,$0b,$0b,$94
    .byte $26,$89,$27,$28,$29,$8a,$28,$92,$2a,$8e,$2b,$8f,$2c,$90,$2d,$91
    .byte $00,$93,$07,$8e,$00,$00,$00,$93,$0e,$bd,$0e,$00,$be,$bf,$be,$bf
    .byte $56,$95,$08,$00,$99,$98,$97,$96,$54,$9a,$52,$99,$55,$9d,$9c,$9b
    .byte $0c,$b5,$0d,$b6,$00,$b8,$b9,$ba,$56,$95,$08,$00,$99,$98,$97,$96
    .byte $32,$a5,$31,$a4,$a9,$a8,$a7,$a6,$31,$a4,$32,$a5,$a7,$a6,$a9,$a8
    .byte $09,$a4,$0a,$00,$0a,$00,$00,$00,$00,$00,$0c,$b5,$b9,$ba,$b7,$b8
    .byte $2f,$9f,$2e,$9e,$39,$2f,$30,$a0,$31,$a4,$32,$a5,$a7,$a6,$a9,$a8
    .byte $54,$9a,$52,$99,$55,$9d,$9c,$9b,$2f,$9f,$2e,$9e,$39,$2f,$30,$a0
    .byte $56,$95,$08,$00,$99,$98,$97,$96,$54,$9a,$52,$99,$30,$55,$9d,$a1
    .byte $0f,$c0,$0f,$c0,$00,$c2,$c1,$c2,$56,$95,$08,$00,$a2,$98,$97,$96
    .byte $32,$a5,$31,$a4,$a9,$a8,$a7,$a6,$31,$a4,$31,$a4,$ab,$aa,$ab,$aa
    .byte $32,$a5,$31,$a4,$a9,$a8,$a7,$a6,$09,$a4,$0a,$00,$0a,$00,$00,$00
    .byte $39,$2f,$30,$a0,$ab,$aa,$aa,$0a,$00,$00,$00,$00,$b9,$ba,$ca,$cb
    .byte $55,$9d,$53,$a2,$2f,$30,$a0,$a3,$00,$00,$00,$00,$ca,$cb,$ca,$cb
    .byte $54,$9a,$52,$99,$30,$55,$9d,$a1,$39,$2f,$30,$a0,$ab,$aa,$aa,$0a
    .byte $56,$95,$08,$00,$a2,$98,$97,$96,$55,$9d,$53,$a2,$2f,$30,$a0,$a3
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$33,$ac,$34,$ad,$ae,$af,$84,$4d
    .byte $15,$b2,$37,$b3,$00,$93,$0b,$94,$0c,$b5,$00,$00,$b7,$b8,$b9,$ba
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$35,$b0,$36,$b1,$84,$87,$84,$4d
    .byte $38,$b4,$37,$b3,$2b,$8f,$2b,$8f,$2d,$91,$2c,$90,$2a,$8e,$2a,$8e
    .byte $38,$b4,$37,$b3,$2a,$8e,$2b,$8f,$2c,$90,$2d,$91,$2b,$8f,$2a,$8e
    .byte $00,$93,$0b,$94,$00,$00,$00,$00,$0c,$b5,$0d,$b6,$b7,$b8,$b9,$ba
    .byte $38,$b4,$37,$b3,$2c,$90,$2d,$91,$00,$93,$07,$8e,$00,$00,$00,$93
    .byte $07,$91,$2c,$91,$00,$93,$2a,$8e,$0c,$00,$00,$93,$b7,$b8,$00,$00
    .byte $2c,$90,$2c,$90,$2a,$8e,$2b,$8f,$0b,$94,$07,$8e,$00,$00,$00,$93
    .byte $0c,$b5,$0d,$b6,$b7,$b8,$b9,$ba,$0d,$b6,$0c,$b5,$b9,$ba,$b7,$b8 ; #$37 - alien spider spawn destroyed
    .byte $0c,$b5,$0d,$b6,$bb,$bc,$bb,$bc,$0e,$bd,$0e,$bd,$be,$bf,$be,$bf
    .byte $0f,$c0,$0f,$c0,$c1,$c2,$c1,$c2,$10,$c3,$10,$c3,$c4,$c5,$c4,$c5
    .byte $11,$c6,$11,$c6,$c7,$c8,$c7,$c8,$12,$c9,$12,$c9,$ca,$cb,$ca,$cb
    .byte $13,$cc,$13,$cc,$cd,$ce,$cd,$ce,$14                             ; continues in bank b

; end of bank
; unused #$0 bytes out of #$2,000 bytes total (100% full)
; unused 0 bytes out of 8,192 bytes total (100% full)
bank_a_unused_space: