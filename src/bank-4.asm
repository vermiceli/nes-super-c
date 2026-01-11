; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank 4 contains Level 7 (Headquarters) and Level 8 (The Final Stage) enemies.
; Then the bank contains the logic that assigns enemies to specific locations on
; each of the level screens.  Then there is the logic for handling the hidden
; sound menu.  Finally, Bank 4 contains Level 1 (Fort Firestorm) screen layout
; and the start of the table of supertiles per screen for Level 1.  That data is
; continued in Bank 5.
; Enemies in this bank:
; * Enemy Type #$45 - Wadder (Alien Mouth)
; * Enemy Type #$46 - Red Poisonous Insect Gel
; * Enemy Type #$2a - Red Bubble
; * Enemy Type #$47 - Jameera (Alien Cyclops)
; * Enemy Type #$48 - Jameera Projectile
; * Enemy Type #$49 - Stomping Ceiling
; * Enemy Type #$2b - Boss Red Poisonous Insect Gel
; * Enemy Type #$37 - Blue Poisonous Insect Gel
; * Enemy Type #$38 - Unknown (Unused)
; * Enemy Type #$6d - Kimkoh (Final Boss)
; * Enemy Type #$6e - Falling Rubble
; * Enemy Type #$3b - Manooki
; * Enemy Type #$3d - Eggron
; * Enemy Type #$3e - Bugger
; * Enemy Type #$52 - Temple of Terror Core
; * Enemy Type #$53 - Temple of Terror Acid Drop Generator
; * Enemy Type #$54 - Temple of Terror Fire Ring Projectile
; * Enemy Type #$56 - Temple of Terror Poison Drop
; * Enemy Type #$51 - Temple of Terror Skull

; 8 KiB PRG ROM
.segment "BANK4"

.include "constants.asm"

; import from bank 3
.import enemy_explosion_routine_00
.import enemy_explosion_routine_01
.import enemy_explosion_routine_03
.import set_enemy_hp
.import player_enemy_x_dist
.import get_enemy_bg_collision_code
.import add_a_to_enemy_y_fract_vel
.import clear_enemy_x_vel
.import set_y_pos_for_bg
.import try_create_enemy_from_existing
.import apply_velocity
.import advance_enemy_routine
.import update_enemy_pos
.import set_enemy_animation_sprite
.import remove_enemy
.import flip_enemy_x_dir
.import set_enemy_routine
.import set_delay_adv_enemy_routine
.import update_enemy_nametable_supertile
.import get_enemy_bg_collision_code_onscreen
.import clear_enemy_y_vel
.import check_bg_wall_collision
.import update_pos_check_offscreen
.import enemy_routine_boss_defeated_00
.import enemy_routine_boss_defeated_01
.import bg_enemy_explosion_routine_00
.import bg_enemy_explosion_routine_01
.import set_enemy_hp_hard
.import set_boss_defeated_remove_enemy
.import test_player_fg_collision
.import set_vel_to_target_player
.import hone_to_player_set_enemy_vel
.import copy_enemy_vars_to_zp
.import load_banks_update_enemy_supertiles
.import bg_enemy_set_scrolls
.import restore_scroll_and_ppuctrl
.import init_bg_boss_pre_irq_max_scrolls
.import backup_scroll_and_ppuctrl
.import clear_enemy_vel
.import earthquake_shake
.import shake_enemy_pattern
.import set_bg_boss_scroll_nt
.import bg_boss_apply_vel
.import copy_enemy_vars

; import from bank 5
.import level_1_supertiles_screen_00
.import level_1_supertiles_screen_01
.import level_1_supertiles_screen_02
.import level_1_supertiles_screen_03
.import level_1_supertiles_screen_04
.import level_1_supertiles_screen_05
.import level_1_supertiles_screen_06

; import from bank f
.import run_routine_from_tbl_below
.import play_sound
.import load_sound_banks_init_channels
.import level_enemy_screen_ptr_ptr_tbl
.import create_enemy_y
.import replace_bullet_with_enemy
.import initialize_enemy
.import set_enemy_collision_and_type
.import load_banks_update_supertile_and_palette
.import get_rotate_01
.import set_nmi_noop_irq
.import clear_y_scroll
.import init_irq_scroll
.import load_alt_collision_code_indices
.import create_and_init_enemy
.import set_level_palette

; export for bank 3
.export manooki_routine_ptr_tbl
.export manooki_projectile_routine_ptr_tbl
.export spider_spawn_routine_ptr_tbl
.export alien_spider_routine_ptr_tbl
.export temple_of_terror_skull_routine_ptr_tbl
.export poison_drop_gen_routine_ptr_tbl
.export temple_of_terror_core_routine_ptr_tbl
.export poison_drop_routine_ptr_tbl
.export fire_ring_projectile_routine_ptr_tbl
.export temple_of_terror_red_blob_routine_ptr_tbl
.export red_bubble_routine_ptr_tbl
.export alien_mouth_routine_ptr_tbl
.export final_stage_red_blob_routine_ptr_tbl
.export alien_cyclops_routine_ptr_tbl
.export cyclops_projectile_routine_ptr_tbl
.export stomping_ceiling_routine_ptr_tbl
.export final_boss_routine_ptr_tbl
.export falling_rubble_routine_ptr_tbl
.export blue_blob_routine_ptr_tbl
.export final_boss_red_blob_routine_ptr_tbl
.export enemy_38_routine_ptr_tbl

; export for bank f
.export lvl_1_create_intro_heli
.export create_screen_enemies
.export run_sound_menu_routine
.export level_1_supertiles_screen_ptr_table
.export level_1_enemy_screen_ptr_tbl
.export level_2_enemy_screen_ptr_tbl
.export level_3_enemy_screen_ptr_tbl
.export level_4_enemy_screen_ptr_tbl
.export level_5_enemy_screen_ptr_tbl
.export level_6_enemy_screen_ptr_tbl
.export level_7_enemy_screen_ptr_tbl
.export level_8_enemy_screen_ptr_tbl
.export level_1_screen_layout_tbl

.byte $34 ; bank byte

; enemy type #$45
alien_mouth_routine_ptr_tbl:
    .addr alien_mouth_routine_00     ; set HP, set X and Y position based on attributes, advance routine
    .addr alien_mouth_routine_01     ; wait for activation, then advance routine
    .addr alien_mouth_routine_02     ; animate breathing 5 times, then open fully to create red blob, repeat, remove if at bottom
    .addr alien_mouth_routine_03     ; enemy destroyed routine - draw destroyed supertiles and advance to explosion animation
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set HP, set X and Y position based on attributes, advance routine
alien_mouth_routine_00:
    lda #$0c                    ; HP = #$0c, #$10, or #$13
    jsr set_enemy_hp            ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    jsr update_enemy_pos        ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ATTRIBUTES,x
    and #$03                    ; keep bits 0 and 1
    tay                         ; transfer to offset register
    lda ENEMY_Y_POS,x           ; load enemy's Y position
    clc                         ; clear carry in preparation for addition
    adc alien_mouth_y_adj_tbl,y ; add y offset amount
    sta ENEMY_Y_POS,x           ; store new Y position
    lda ENEMY_X_POS,x           ; load enemy's X position
    sec                         ; set carry flag in preparation for subtraction
    sbc #$08                    ; subtract #$08 from X position
    sta ENEMY_X_POS,x           ; set new X position
    jmp advance_enemy_routine   ; advance to next routine

alien_mouth_y_adj_tbl:
    .byte $04 ;  4
    .byte $f8 ; -8
    .byte $ff ; -1
    .byte $fc ; -4

; wait for activation, then advance routine
alien_mouth_routine_01:
    jsr update_enemy_pos        ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x           ; load enemy's X position
    cmp #$f0
    bcs alien_mouth_exit        ; exit if enemy in right 93.75% of screen
    lda ENEMY_Y_POS,x           ; load enemy's Y position
    cmp #$20
    bcc alien_mouth_exit        ; exit if at top 12.5% of screen
    cmp #$d0
    bcs alien_mouth_exit        ; exit if at bottom ~81.25% of screen
    lda #$01
    sta ENEMY_ANIMATION_DELAY,x
    jmp advance_enemy_routine   ; advance to next routine

alien_mouth_exit:
    rts

; animate breathing 5 times, then open fully to create red blob, repeat, remove if at bottom
alien_mouth_routine_02:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    cmp #$d0
    bcc @continue     ; continue if in main portion of screen
    jmp remove_enemy  ; remove enemy if in top 18.75% of screen

@continue:
    lda #$01
    sta ENEMY_SPRITE,x                    ; invisible sprite, enemy uses background tiles
    jsr update_enemy_pos                  ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_ANIMATION_DELAY,x           ; decrement animation delay
    bne alien_mouth_exit                  ; exit if animation delay hasn't elapse
    jsr alien_mouth_draw_frame_supertiles ; draw supertile(s) based on ENEMY_ATTRIBUTES and current ENEMY_FRAME
    bcs @set_anim_delay_exit              ; branch if unable to update supertiles to try again next frame
    lda ENEMY_VAR_2,x                     ; load animation direction (0 = ascending, 1 = descending)
    beq @anim_opening                     ; branch if ascending animation (opening)
    dec ENEMY_FRAME,x                     ; closing, decrement frame
    bpl @set_anim_delay_10_exit           ; branch if mouth was open or had teeth showing
    inc ENEMY_FRAME,x                     ; decremented below first animation frame
    inc ENEMY_FRAME,x                     ; update frame to #$01 (show teeth)
    jmp @change_anim_direction            ; set descending animation direction to continue breathing

; either breathing and showing teeth, or opening up to create red blob
@anim_opening:
    inc ENEMY_FRAME,x ; move to next animation frame
    lda ENEMY_VAR_1,x ; load number of breaths taken
    cmp #$05          ; see if 5 breaths have been taken
    lda #$02          ; assume breathing (not opening), which has #$02 animation frames max
    bcc @continue2    ; branch if not yet taken 5 breaths
    lda #$04          ; opening to generate red blob, which has #$04 animation frames max

@continue2:
    cmp ENEMY_FRAME,x                  ; compare current frame index to max number of frames in sequence
    bne @set_anim_delay_10_exit        ; branch if not at the last frame
    dec ENEMY_FRAME,x
    dec ENEMY_FRAME,x                  ; finished animation move down two frames, e.g. #$02 -> #$00 or #$04 -> #$02
    inc ENEMY_VAR_1,x                  ; increment number of breaths
    lda ENEMY_VAR_1,x                  ; load number of breaths taken
    cmp #$06                           ; see if mouth was fully open
    bcc @change_anim_direction         ; branch if breathing (more breaths to take)
    lda #$00                           ; mouth fully open, create red blob
    sta ENEMY_VAR_1,x                  ; reset number of breaths
    ldy #$46                           ; enemy type = final stage poisonous insect gel (red blob)
    jsr try_create_enemy_from_existing ; create final stage poisonous insect gel (red blob)

@change_anim_direction:
    lda ENEMY_VAR_2,x
    eor #$01
    sta ENEMY_VAR_2,x

@set_anim_delay_10_exit:
    lda #$0a

@set_anim_delay_exit:
    sta ENEMY_ANIMATION_DELAY,x
    rts

; enemy destroyed routine - draw destroyed supertiles and advance to explosion animation
alien_mouth_routine_03:
    lda #$04
    jsr alien_mouth_draw_supertiles
    bcs alien_mouth_exit            ; branch if unable to update supertiles to try again next frame
    jmp enemy_explosion_routine_00  ; set empty sprite, play optional enemy destroyed sound, disable collisions

; draws the supertile(s) for the alien mouth depending on ENEMY_ATTRIBUTES and current ENEMY_FRAME
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
alien_mouth_draw_frame_supertiles:
    lda ENEMY_FRAME,x

; draws the supertile(s) for the alien mouth depending on ENEMY_ATTRIBUTES and enemy frame specified in a
; input
;  * a - alien mouth frame index
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
alien_mouth_draw_supertiles:
    sta $0a                                ; store frame index in $0a
    lda ENEMY_ATTRIBUTES,x
    and #$03                               ; strip to Y position adjustment to determine what supertile(s) need to be drawn
    asl                                    ; double since each entry is a #$02 byte memory address
    tay                                    ; transfer to offset register
    lda alien_mouth_supertile_ptr_tbl,y    ; load low byte of supertile data memory address pointer
    sta $08                                ; set low byte of supertile data memory address pointer
    lda alien_mouth_supertile_ptr_tbl+1,y  ; load high byte of supertile data memory address pointer
    sta $09                                ; set high byte of supertile data memory address pointer
    tya                                    ; transfer offset back to a
    beq @alien_mouth_attr_00               ; branch if using alien_mouth_supertiles_attr_00, which only updates 1 supertile instead of 2
    lda $0a                                ; updating 2 supertiles, load frame index
    asl                                    ; double since there are 2 tiles per frame index
    tay                                    ; transfer to offset register
    lda ($08),y                            ; load first supertile index (offset into level_8_supertile_data)
    sta $0c                                ; store in $0c for use by load_banks_update_enemy_supertiles
    iny                                    ; increment read offset
    lda ($08),y                            ; load second supertile index (offset into level_8_supertile_data)
    ldy #$e0                               ; x offset = #$08, y offset = #$e8 (one supertile above)
    jsr load_banks_update_enemy_supertiles ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y
    lda #$01                               ; !(HUH) not sure why used, clears zero flag
    rts

; used for alien_mouth_supertiles_attr_00, which only updates one supertile instead of 2
; this is because the alien mouth aligns with the level's platform and only 1 supertile update is needed
@alien_mouth_attr_00:
    ldy $0a                              ; load frame index
    lda ($08),y                          ; load supertile to update based on frame index (offset into level_8_supertile_data)
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    lda #$01
    rts

alien_mouth_supertile_ptr_tbl:
    .addr alien_mouth_supertiles_attr_00 ; Y pos + 4
    .addr alien_mouth_supertiles_attr_01 ; Y pos - 8
    .addr alien_mouth_supertiles_attr_02 ; Y pos - 1
    .addr alien_mouth_supertiles_attr_03 ; Y pos - 4

; Y pos + 4
alien_mouth_supertiles_attr_00:
    .byte $4f ; ENEMY_FRAME = #$00
    .byte $60 ; ENEMY_FRAME = #$01
    .byte $61 ; ENEMY_FRAME = #$02
    .byte $62 ; ENEMY_FRAME = #$03
    .byte $0b ; ENEMY_FRAME = #$04

; Y pos - 8
alien_mouth_supertiles_attr_01:
    .byte $47,$4b ; ENEMY_FRAME #$00
    .byte $48,$4c ; ENEMY_FRAME #$01
    .byte $49,$4d ; ENEMY_FRAME #$02
    .byte $4a,$4e ; ENEMY_FRAME #$03
    .byte $04,$08 ; ENEMY_FRAME #$04

; Y pos - 1 (bottom of ascending ramp)
alien_mouth_supertiles_attr_02:
    .byte $50,$54 ; ENEMY_FRAME = #$00
    .byte $51,$55 ; ENEMY_FRAME = #$01
    .byte $52,$56 ; ENEMY_FRAME = #$02
    .byte $53,$57 ; ENEMY_FRAME = #$03
    .byte $19,$1d ; ENEMY_FRAME = #$04

; Y pos - 4
alien_mouth_supertiles_attr_03:
    .byte $58,$5c ; ENEMY_FRAME = #$00
    .byte $59,$5d ; ENEMY_FRAME = #$01
    .byte $5a,$5e ; ENEMY_FRAME = #$02
    .byte $5b,$5f ; ENEMY_FRAME = #$03
    .byte $23,$27 ; ENEMY_FRAME = #$04

; enemy type #$46, not enemy type #$2b (see final_boss_red_blob_routine_ptr_tbl)
final_stage_red_blob_routine_ptr_tbl:
    .addr final_stage_red_blob_routine_00 ; set destroy attributes, set velocity to target closest player, set palette, advance routine
    .addr final_stage_red_blob_routine_01 ; set animation, wait for delay applying velocity, set number of times to re-target, advance routine
    .addr final_stage_red_blob_routine_02 ; animate stationary enemy, applying scroll until delay, then re-target player and advance routine
    .addr final_stage_red_blob_routine_03 ; when re-targeting delay has elapsed, set velocity to target closest player, resets delay, repeat #$20 times
    .addr enemy_explosion_routine_00      ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01      ; animate explosion sequence
    .addr enemy_explosion_routine_03      ; mark destroyed, remove enemy

; set destroy attributes, set velocity to target closest player, set palette, advance routine
final_stage_red_blob_routine_00:
    lda #$16                        ; sound_14 and circular destruction explosion
    sta ENEMY_DESTROY_ATTRS,x       ; set destroy explosion sound to sound_14 (Z OUT) and set destroy explosion type to circular
    jsr player_enemy_x_dist         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$00                        ; 0.5x speed
    jsr set_vel_to_target_player    ; set enemy velocity to target player index $0a at .5x speed
    lda #$02
    sta ENEMY_SPRITE_ATTR,x         ; set sprite palette, making sprite visible
    lda #$40
    jmp set_delay_adv_enemy_routine ; set delay to #$40 and set routine to final_stage_red_blob_routine_01

; set animation, wait for delay applying velocity, set number of times to re-target, advance routine
final_stage_red_blob_routine_01:
    ldy #$16                        ; animation index for red blob
    jsr set_enemy_animation_sprite  ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_DELAY,x               ; decrement advance routine delay
    bne red_blob_exit               ; exit if delay hasn't elapsed
    lda #$20
    sta ENEMY_VAR_2,x               ; set number of re-target instances to be #$20
    lda #$20                        ; set delay until next re-target to be #$20
                                    ; the red blob will be stationary during this delay
    jmp set_delay_adv_enemy_routine ; set delay to #$20 and set routine to final_stage_red_blob_routine_02

; animate stationary enemy, applying scroll until delay, then re-target player and advance routine
final_stage_red_blob_routine_02:
    ldy #$16                        ; animation index for red blob
    jsr set_enemy_animation_sprite  ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
                                    ; red blob is stationary during this delay
    dec ENEMY_DELAY,x               ; decrement delay
    bne red_blob_exit               ; exit if re-targeting delay hasn't elapsed
    jsr player_enemy_x_dist         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$05                        ; 1.62x speed
    jsr set_vel_to_target_player    ; set enemy velocity to target player index $0a  at 1.62x speed
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to final_stage_red_blob_routine_03

; when re-targeting delay has elapsed, set velocity to target closest player, resets delay, repeat #$20 times
final_stage_red_blob_routine_03:
    ldy #$16                         ; animation index for red blob
    jsr set_enemy_animation_sprite   ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda ENEMY_VAR_2,x                ; load number of times to re-target to player
    beq @apply_velocity_exit         ; exit if no longer targeting player
    dec ENEMY_DELAY,x                ; decrement re-targeting delay
    bne @apply_velocity_exit         ; exit if re-targeting delay hasn't elapsed
    lda #$07                         ; delay elapsed, set next delay
    sta ENEMY_DELAY,x
    jsr player_enemy_x_dist          ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$05                         ; speed adjust code for 1.62x speed
    jsr hone_to_player_set_enemy_vel ; determine next aim dir to get closer to player index ($0a)
                                     ; and set enemy velocity to that target direction
    dec ENEMY_VAR_2,x                ; decrement total number of re-targeting attempts

@apply_velocity_exit:
    jmp apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

red_blob_exit:
    rts

; enemy type #$2a
red_bubble_routine_ptr_tbl:
    .addr red_bubble_routine_00      ; set initial direction, sprite palette, disable collisions, determine whether to have flashing delay
    .addr red_bubble_routine_01      ; (optional) wait for activation delay, flashing every #$04 frames, then make visible and advance routine
    .addr red_bubble_routine_02      ; enable collisions, animate, wait for delay before honing (targeting) closer towards closest player
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set initial direction, sprite palette, disable collisions, determine whether to have flashing delay
red_bubble_routine_00:
    lda #$06
    sta ENEMY_VAR_1,x         ; set initial aim direction to down
    lda #$02
    sta ENEMY_SPRITE_ATTR,x   ; set sprite palette
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda ENEMY_ATTRIBUTES,x    ; load attributes to see if there should be a #$20 frame flashing delay before attacking
    lsr                       ; push bit 0 of attributes to carry
    lda #$20                  ; assume not skipping flashing delay, and set delay to #$20
    bcs @adv_routine          ; branch to advance routine if bit 0 set
    jsr advance_enemy_routine ; bit 0 clear, skipping red_bubble_routine_01 (flashing delay)
    lda #$01                  ; set delay before targeting player to #$01

@adv_routine:
    jmp set_delay_adv_enemy_routine ; set delay to #$01 or #$20
                                    ; and set routine to red_bubble_routine_01 or red_bubble_routine_02

; wait for activation delay, flashing every #$02 frames, then make visible and advance routine
red_bubble_routine_01:
    lda ENEMY_DELAY,x ; load delay for timing sprite flashing
                      ; alternate between #$02 frames invisible and #$02 frames visible
    lsr
    lsr
    lda #$a8          ; sprite_a8 (red bubble)
    bcc @exit         ; branch if bit 1 is clear
    lda #$00          ; invisible sprite

@exit:
    sta ENEMY_SPRITE,x              ; set sprite
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x               ; decrement flashing delay
    bne red_blob_exit               ; exit if flashing not elapsed
    lda #$01                        ; flashing delay elapsed, advance routine
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to red_bubble_routine_02

; enable collisions, animate, wait for delay before honing (targeting) closer towards closest player
red_bubble_routine_02:
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x        ; enable player-enemy and player bullet-enemy collision
    ldy #$15                         ; animation index for red bubble
    jsr set_enemy_animation_sprite   ; animate sprite for red bubble based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    dec ENEMY_DELAY,x                ; decrement until next hone towards player
    bne @exit
    lda #$07
    sta ENEMY_DELAY,x                ; set delay to hone towards player
    jsr player_enemy_x_dist          ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$01                         ; speed adjust code for 0.75x speed
    jsr hone_to_player_set_enemy_vel ; determine next aim dir to get closer to player index ($0a)
                                     ; and set enemy velocity to that target direction

@exit:
    jmp apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

; enemy type #$47
alien_cyclops_routine_ptr_tbl:
    .addr alien_cyclops_routine_00   ; initialize X pos, Y pos, HP, number of projectiles
    .addr alien_cyclops_routine_01   ; fire projectiles (2 per round) on repeated loop
    .addr alien_cyclops_routine_02   ; enemy destroyed routine, update nametable supertile, enemy_explosion_routine_00
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize X pos, Y pos, HP, number of projectiles
alien_cyclops_routine_00:
    lda #$10                        ; HP = #$10, #$14, or #$17
    jsr set_enemy_hp                ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$01
    sta ENEMY_VAR_2,x               ; set number of projectiles to fire to 2
    lda ENEMY_X_POS,x               ; load enemy's X position
    sec                             ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_X_POS,x               ; update the X position to be 8 to the left
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    clc                             ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_Y_POS,x               ; update the Y position to be 8 down
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to alien_cyclops_routine_01

; fire projectiles (2 per round) on repeated loop
alien_cyclops_routine_01:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    cmp #$d0          ; see if in bottom ~81% of screen
    bcc @continue     ; branch if not in the very bottom of the screen to continue
    jmp remove_enemy  ; cyclops in bottom of screen, remove it

@continue:
    lda #$01
    sta ENEMY_SPRITE,x                   ; set invisible sprite (alien cyclops uses nametable and not a sprite)
    jsr update_enemy_pos                 ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                    ; decrement delay until next projectile
    bne alien_cyclops_exit               ; exit if delay hasn't elapsed
    ldy ENEMY_FRAME,x                    ; firing projectile, load enemy frame
    lda alien_cyclops_frame_tbl,y        ; load supertile index for cyclops (either mouth open or mouth closed)
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    lda #$01
    bcs @set_delay                       ; branch to set a 1 frame delay if unable to update supertile this frame
    lda ENEMY_FRAME,x                    ; updated supertile, load current frame
    eor #$01                             ; flip bit 0
    sta ENEMY_FRAME,x                    ; set next animation frame index
    beq @check_delay                     ; branch if next animation frame offset is 0 (mouth open)
                                         ; mouth just opened, fire projectile
    ldy #$48                             ; enemy type = alien cyclops projectile
    jsr try_create_enemy_from_existing   ; create alien cyclops projectile
    lda #$08
    bne @set_delay                       ; always branch to set an 8 frame delay before launching next projectile

; mouth just closed, determine delay
; based on whether or not both projectiles have been fired for attack
@check_delay:
    lda #$10          ; load delay between projectiles
    dec ENEMY_VAR_2,x ; decrement remaining number of projectiles to fire
    bpl @set_delay    ; branch to set shorter delay if more projectiles to fire
    lda #$01          ; fired all projectiles, reinitialize to fire 2 projectiles
    sta ENEMY_VAR_2,x ; set to one less than number of projectiles to fire
    lda #$50          ; set longer delay until next round of attack

@set_delay:
    sta ENEMY_DELAY,x ; set delay until next projectile is fired

alien_cyclops_exit:
    rts

; specifies supertile offset for alien cyclops
alien_cyclops_frame_tbl:
    .byte $6e ; mouth open
    .byte $6d ; mouth closed

; enemy destroyed routine, update nametable supertile, enemy_explosion_routine_00
alien_cyclops_routine_02:
    lda #$6c
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    bcs alien_cyclops_exit
    jmp enemy_explosion_routine_00       ; set empty sprite, play optional enemy destroyed sound, disable collisions

; enemy type #$48
cyclops_projectile_routine_ptr_tbl:
    .addr cyclops_projectile_routine_00
    .addr cyclops_projectile_routine_01

cyclops_projectile_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x    ; mark cyclops projectile so bullets travel through it
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    clc                          ; clear carry in preparation for addition
    adc #$04
    sta ENEMY_Y_POS,x
    lda #$00
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda #$fc
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    jmp advance_enemy_routine    ; advance to next routine

cyclops_projectile_routine_01:
    lda #$06
    sta ENEMY_SPRITE,x      ; sprite_06
    lda #$02
    sta ENEMY_SPRITE_ATTR,x
    jmp apply_velocity      ; apply enemy's velocity to its position, removing enemy if off-screen

; enemy type #$49
stomping_ceiling_routine_ptr_tbl:
    .addr stomping_ceiling_routine_00 ; set enemy destroyed attributes, initialize Y position, advance routine
    .addr stomping_ceiling_routine_01 ; initialize IRQ and IRQ-related variables
    .addr stomping_ceiling_routine_02 ; draw background required for stomping effect
    .addr stomping_ceiling_routine_03 ; stomping routine, check if should de-activate
    .addr stomping_ceiling_routine_04 ; de-activate stomping ceiling

; set enemy destroyed attributes, initialize Y position, advance routine
stomping_ceiling_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$20
    sta ENEMY_Y_POS,x         ; set initial Y position
    jmp advance_enemy_routine ; advance to next routine

; initialize IRQ and IRQ-related variables
stomping_ceiling_routine_01:
    lda X_SCROLL                    ; load PPU horizontal scroll
    bne @exit                       ; branch if not at starting point for drop down animation
    lda #$00
    sta X_DRAW_ROUTINE
    sta Y_DRAW_ROUTINE              ; initialize draw routines
    jsr init_irq_scroll             ; set post-irq scroll to current scroll
    jsr backup_scroll_and_ppuctrl
    lda #$80                        ; stop horizontal scroll for pre-irq
    sta STOMP_CEILING_X_SCROLL_CTRL ; so that the correct tiles are shown for ceiling
    lda #$01                        ; horizontal mirroring
                                    ;  A A
                                    ;  B B
    sta NT_MIRRORING                ; set nametable mirroring (0: vertical; 1: horizontal)
                                    ; changing from vertical to horizontal mirroring
    lda #$06
    sta IRQ_TYPE                    ; set irq routine type to irq_handler_06_ptr_tbl
                                    ; level 8 stomping ceiling
    .ifdef Probotector
        lda #$ac
    .else
        lda #$b9
    .endif
    sta SCANLINE_IRQ_1              ; set IRQ interrupt to happen at ~72% of the screen
    lda #$e0
    sta IRQ_PPUADDR                 ; PPU address for post-irq (floor) $22e0 or $2ee0
    lda ENEMY_ATTRIBUTES,x
    lsr
    lda #$2e
    ldy #$aa                        ; base nametable $2800 (bottom left)
    bcc @continue
    lda #$22
    ldy #$a8                        ; base nametable $2000 (top left)

@continue:
    sta IRQ_PPUADDR+1                    ; setting post-IRQ PPUADDR to $2ee0 or $22e0
    sty PPUCTRL_SETTINGS                 ; set base nametable to either $2800 or $2000
    sty IRQ_PPUCTRL_SETTINGS
    jsr init_bg_boss_pre_irq_max_scrolls ; init boss screen pre-irq scroll max values
    lda #$08
    sta ENEMY_VAR_1,x
    lda #$00
    sta NT_ROW_SCROLL
    sta LEVEL_Y_SCREEN
    sta Y_SCROLL_DIR                     ; 0 = vertically scrolling up, 1 = vertically scrolling down
    sta NT_CURRENT_COLUMN
    jmp advance_enemy_routine            ; advance to next routine

@exit:
    rts

; draw background required for stomping effect
stomping_ceiling_routine_02:
    jsr stomping_ceiling_apply_x_scroll ; set X position and post-IRQ horizontal scroll based on X_SCROLL_SPEED
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne stomping_ceiling_routine_exit
    dec ENEMY_VAR_1,x
    beq @continue
    lda #$00
    sta X_SCROLL_DRAW_POINT
    rts

@continue:
    lda #$01
    sta STOMP_CEILING_X_SCROLL_CTRL
    lda #$00
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine       ; advance to next routine

stomping_ceiling_routine_exit:
    rts

; stomping routine, check if should de-activate
stomping_ceiling_routine_03:
    lda #$00
    sta PLAYER_Y_VEL_BG_COLLISION_ADJ   ; clear any y pushing for bg collisions, will re-calculate
    jsr run_stomp_routine
    jsr stomping_ceiling_apply_x_scroll ; set X position and post-IRQ horizontal scroll based on X_SCROLL_SPEED
    lda ENEMY_VAR_7,x                   ; see which of the 3 screens is active for enemy
    beq stomping_ceiling_routine_exit
    cmp #$fe
    bcs stomping_ceiling_routine_exit   ; exit if not #$fd (last screen of enemy)
    lda ENEMY_Y_POS,x                   ; player advanced far enough to de-activate enemy
                                        ; once ceiling fully back to top
    cmp #$20                            ; load enemy's Y position and compare to top
    bne stomping_ceiling_routine_exit   ; exit if not back to the top
    jmp advance_enemy_routine           ; advance to next routine

; de-activate stomping ceiling
stomping_ceiling_routine_04:
    jsr stomping_ceiling_apply_x_scroll ; set X position and post-IRQ horizontal scroll based on X_SCROLL_SPEED
    lda X_SCROLL                        ; load PPU horizontal scroll
    bne stomping_ceiling_routine_exit   ; exit if not scrolled into screen yet
    jsr restore_scroll_and_ppuctrl      ; restore scroll and PPU control values
    lda X_SCROLL                        ; load PPU horizontal scroll
    sta IRQ_X_SCROLL
    lda #$00
    sta STOMP_CEILING_X_SCROLL_CTRL
    jsr clear_y_scroll
    inc LEVEL_Y_SCREEN
    dec DRAW_X_SCREEN
    jsr set_nmi_noop_irq                ; remove any scanline interrupts
    jmp remove_enemy

; set the enemy's X position and post-IRQ horizontal scroll based on X_SCROLL_SPEED
stomping_ceiling_apply_x_scroll:
    lda ENEMY_X_POS,x        ; load enemy's X position
    sec                      ; set carry flag in preparation for subtraction
    sbc X_SCROLL_SPEED       ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    sta ENEMY_X_POS,x
    lda ENEMY_VAR_7,x
    sbc #$00
    sta ENEMY_VAR_7,x
    jsr bg_enemy_set_scrolls ; simulate moving (ENEMY_X_POS, ENEMY_Y_POS) by scrolling in the opposite direction
    lda X_SCROLL             ; load PPU horizontal scroll
    sta IRQ_X_SCROLL
    rts

run_stomp_routine:
    lda ENEMY_VAR_1,x              ; load stomp state
    jsr run_routine_from_tbl_below

stomp_routine_tbl:
    .addr stomp_routine_00 ; stomping
    .addr stomp_routine_01 ; slammed at bottom - alternate Y velocity between 2 and -2 every 4 frames until delay elapses, then advance stomp routine
    .addr stomp_routine_02 ; waiting to ascend - wait for delay, set ascending velocity to -1, advance routine
    .addr stomp_routine_03 ; ascending
    .addr stomp_routine_04 ; waiting to descend

; stomping
stomp_routine_00:
    lda #$10
    jsr add_a_to_enemy_y_fract_vel ; speed up the slam action as ceiling descends
    lda ENEMY_Y_POS,x              ; load enemy's Y position
    sta $08
    jsr stomp_routine_apply_y_vel  ; apply Y velocity to Y position
    lda ENEMY_Y_POS,x              ; load enemy's Y position
    sec                            ; set carry flag in preparation for subtraction
    sbc $08
    sta $9a                        ; store amount of Y velocity sped up this frame
    lda ENEMY_Y_POS,x              ; load enemy's Y position
    cmp #$a0                       ; see if at bottom of slam
    bcc stomp_routine_exit         ; exit if not at the bottom of the slam
    lda #$22                       ; stomping ceiling slam sound
    jsr play_sound                 ; play sound for slamming to ground
    lda #$20

stomp_routine_adv_exit:
    sta ENEMY_DELAY,x
    inc ENEMY_VAR_1,x

stomp_routine_exit:
    rts

; slammed at bottom - alternate Y velocity between 2 and -2 every 4 frames until delay elapses, then advance stomp routine
stomp_routine_01:
    lda ENEMY_DELAY,x
    and #$03
    bne @continue                 ; branch if not timer not a multiple of four to keep same Y velocity
    lda ENEMY_DELAY,x             ; delay timer a multiple of 4, adust Y velocity
    lsr
    and #$02                      ; (keep bits 1 and 2 of ENEMY_DELAY, i.e. either #$00 or #$02
    tay                           ; transfer new Y velocity index into offset register
    jsr set_stomp_y_vel           ; sets the Y velocity of the stomping ceiling to create bounce effect
                                  ; in this case, setting Y velocity to either 2 or -2
    jsr stomp_routine_apply_y_vel ; apply Y velocity to Y position

@continue:
    dec ENEMY_DELAY,x          ; decrement delay
    bne stomp_routine_exit     ; exit if delay hasn't elapsed
    lda #$40
    bne stomp_routine_adv_exit ; set delay to #$40 and move to stomp_routine_02

; waiting to ascend - wait for delay, set ascending velocity to -1, advance stomp routine
stomp_routine_02:
    dec ENEMY_DELAY,x
    bne stomp_routine_exit ; exit if delay hasn't elapsed
    ldy #$04               ; wait delay elapsed, start ascending with a Y velocity of -1
    jsr set_stomp_y_vel    ; sets the Y velocity of the stomping ceiling
    inc ENEMY_VAR_1,x      ; move to stomp_routine_03 (ascending)
    rts

; ascending
stomp_routine_03:
    jsr stomp_routine_apply_y_vel ; apply Y velocity to Y position
    lda ENEMY_Y_POS,x             ; load enemy's Y position
    cmp #$21
    bcs stomp_routine_exit
    lda #$20
    sta ENEMY_Y_POS,x
    lda #$05
    sta $08

@create_red_bubble_loop:
    ldy #$2a                            ; enemy type = red bubble
    jsr try_create_enemy_from_existing  ; create red bubble
    bcc stomp_routine_set_delay_40_exit ; branch if unable to create red bubble
    ldx $11
    ldy $08
    lda stomp_routine_y_pos_tbl,y
    sta ENEMY_Y_POS,x
    lda stomp_routine_x_pos_tbl,y
    sta ENEMY_X_POS,x
    lda #$01
    sta ENEMY_ATTRIBUTES,x
    ldx ENEMY_CURRENT_SLOT
    dec $08
    bpl @create_red_bubble_loop

stomp_routine_set_delay_40_exit:
    lda #$40
    jmp stomp_routine_adv_exit

stomp_routine_y_pos_tbl:
    .byte $38,$38,$30,$30,$28,$28

stomp_routine_x_pos_tbl:
    .byte $30,$d0,$50,$b0,$70,$90

; waiting to descend
stomp_routine_04:
    dec ENEMY_DELAY,x      ; decrement descending delay
    bne stomp_routine_exit ; exit if delay hasn't elapsed
    jsr clear_enemy_y_vel  ; can now begin stomp, move to stomp_routine_00 (stomping)
                           ; set enemy Y velocity to 0
    lda #$00
    sta ENEMY_VAR_1,x      ; set routine to stomp_routine_00 (stomping)
    rts

; apply Y velocity to Y position
stomp_routine_apply_y_vel:
    lda ENEMY_Y_VEL_ACCUM,x
    clc                          ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x
    sta ENEMY_Y_VEL_ACCUM,x
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    adc ENEMY_Y_VELOCITY_FAST,x  ; add fast velocity (and any overflow from y accumulator)
    sta ENEMY_Y_POS,x            ; set new Y position
    rts

; sets the Y velocity of the stomping ceiling
; input
;  * y - Y velocity index (0 = 2, 2 = -2, 4 = -1)
set_stomp_y_vel:
    lda stomp_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda stomp_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    rts

stomp_y_vel_tbl:
    .byte $00,$02 ;  2
    .byte $00,$fe ; -2
    .byte $00,$ff ; -1

; enemy type #$2b, not enemy type #$46 (see final_stage_red_blob_routine_ptr_tbl)
final_boss_red_blob_routine_ptr_tbl:
    .addr final_boss_red_blob_routine_00 ; set invincible, and bullets can't collide, set activation delay, advance routine
    .addr final_boss_red_blob_routine_01 ; wait to make enemy visible, advance routine
    .addr final_boss_red_blob_routine_02 ; animate enemy as it follows its velocity until off-screen when removed
    .addr remove_enemy                   ; enemy destroyed routine

; set invincible, and bullets can't collide, set activation delay, advance routine
final_boss_red_blob_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x                    ; mark reb blob so bullets travel through it
    lda #$f1                                     ; special invincible HP
    sta ENEMY_HP,x                               ; any HP greater than #$f0 is invincible
    ldy ENEMY_VAR_1,x                            ; load activate delay index
    lda final_boss_red_blob_activate_delay_tbl,y ; load activate delay
    jmp set_delay_adv_enemy_routine              ; set delay and set routine to final_boss_red_blob_routine_01

final_boss_red_blob_activate_delay_tbl:
    .byte $1c,$18,$14,$10,$0c,$08,$04,$01

; wait to make enemy visible, advance routine
final_boss_red_blob_routine_01:
    dec ENEMY_DELAY,x         ; decrement activation delay
    bne @exit                 ; exit if activation delay hasn't elapsed
    lda #$02                  ; activation delay elapsed
    sta ENEMY_SPRITE_ATTR,x   ; set sprite palette, makes sprite visible
    jmp advance_enemy_routine ; advance to next routine

@exit:
    rts

; animate enemy as it follows its velocity until off-screen when removed
final_boss_red_blob_routine_02:
    ldy #$16                       ; animation index for red blob
    jsr set_enemy_animation_sprite ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen
    rts

; enemy type #$37
blue_blob_routine_ptr_tbl:
    .addr blue_blob_routine_00       ; set sprite palette, set initial position, set random X and Y velocity
    .addr blue_blob_routine_01       ; animate, applying gravity until falling, clear X velocity, then advance routine
    .addr blue_blob_routine_02       ; animate while falling straight down until off screen when removed
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set sprite palette, set initial position, set random X and Y velocity
blue_blob_routine_00:
    lda #$03
    sta ENEMY_SPRITE_ATTR,x      ; set sprite palette
    lda #$b8
    sta ENEMY_Y_POS,x            ; set initial Y position
    lda #$d0
    sta ENEMY_X_POS,x            ; set initial X position
    lda RANDOM_NUM               ; load random number
    and #$07
    asl                          ; get random number between 0 and #$0e inclusively
    tay                          ; transfer to offset register
    lda blue_blob_y_vel_tbl,y    ; load enemy's fractional Y velocity
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda blue_blob_y_vel_tbl+1,y  ; load enemy's fast Y velocity
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
                                 ; sets Y velocity to either -6.00 or -6.25
    lda RANDOM_NUM               ; load random number
    clc                          ; clear carry in preparation for addition
    adc FRAME_COUNTER
    and #$07
    asl                          ; get random number between 0 and #$0e inclusively
    tay                          ; transfer to offset register
    lda blue_blob_x_vel_tbl,y    ; load enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda blue_blob_x_vel_tbl+1,y  ; load enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    jmp advance_enemy_routine    ; advance to next routine

; animate, applying gravity until falling, clear X velocity, then advance routine
blue_blob_routine_01:
    ldy #$16                       ; animation index for green blob
    jsr set_enemy_animation_sprite ; animate sprite for green blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda #$20
    jsr add_a_to_enemy_y_fract_vel ; apply gravity
    jsr apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_Y_VELOCITY_FAST,x    ; load enemy's fast Y velocity
    bmi @exit                      ; exit if enemy still rising
    cmp #$01
    bcc @exit                      ; exit if enemy velocity is not yet falling fast enough
    jsr clear_enemy_x_vel          ; blue blob is falling, clear X velocity so it falls straight down
                                   ; set enemy X velocity (fast and fractional) to 0
    jmp advance_enemy_routine      ; advance to next routine

@exit:
    rts

; animate while falling straight down until off screen when removed
blue_blob_routine_02:
    ldy #$16                       ; animation index for green blob
    jsr set_enemy_animation_sprite ; animate sprite for green blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jmp apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen

; always either -6.25 or -6.00
; !(HUH) not sure why table so large. Perhaps they simplified this to be easier
blue_blob_y_vel_tbl:
    .byte $c0,$f9 ; -6.25
    .byte $00,$fa ; -6.00
    .byte $c0,$f9 ; -6.25
    .byte $00,$fa ; -6.00
    .byte $c0,$f9 ; -6.25
    .byte $00,$fa ; -6.00
    .byte $c0,$f9 ; -6.25
    .byte $00,$fa ; -6.00

blue_blob_x_vel_tbl:
    .byte $00,$fd ; -3.00
    .byte $30,$fd ; -2.8125
    .byte $60,$fd ; -2.625
    .byte $90,$fd ; -2.4375
    .byte $c0,$fd ; -2.25
    .byte $f0,$fd ; -2.0625
    .byte $20,$fe ; -1.875
    .byte $50,$fe ; -1.6875

; enemy type #$38 (unused)
enemy_38_routine_ptr_tbl:
    .addr enemy_38_routine_00 ; set sprite, destroy attributes, initial attack delay, position, and num of blue blobs per attack
    .addr enemy_38_routine_01 ; wait for attack delay, create blue blob, repeat
    .addr remove_enemy        ; enemy destroyed routine

; set sprite, destroy attributes, initial attack delay, position, and num of blue blobs per attack
enemy_38_routine_00:
    lda #$06
    sta ENEMY_SPRITE,x        ; sprite_06 (steel bullet)
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$02                  ; create 2 blue blobs per attack
    sta ENEMY_VAR_2,x         ; set number of blue blobs to create
    lda ENEMY_VAR_1,x         ; load configuration
    and #$01
    tay                       ; transfer to offset register
    lda enemy_38_delay_tbl,y  ; load initial delay before creating blue blob
    sta ENEMY_DELAY,x         ; set delay
    lda enemy_38_y_adj_tbl,y  ; load amount to add to Y position from enemy creator Y position
    sta $00                   ; set amount to add to Y position from enemy creator Y position
    lda enemy_38_x_adj_tbl,y  ; load amount to add to X position from enemy creator Y position
    ldy ENEMY_VAR_5,x         ; load enemy slot of enemy that created this enemy
    clc                       ; clear carry in preparation for addition
    adc ENEMY_X_POS,y         ; add x adjustment to original enemy
    sta ENEMY_X_POS,x         ; adjust enemy X position to be shifted from position
                              ; where enemy that created this enemy is
    lda $00                   ; load amount to add to Y position from enemy creator Y position
    clc                       ; clear carry in preparation for addition
    adc ENEMY_Y_POS,y         ; add y adjustment to original enemy
    sta ENEMY_Y_POS,x         ; adjust enemy Y position to be shifted from position
                              ; where enemy that created this enemy is
    jmp advance_enemy_routine ; advance to next routine

; wait for attack delay, create blue blob, repeat
enemy_38_routine_01:
    dec ENEMY_DELAY,x                  ; decrement blue blob creation delay
    bne @exit                          ; exit if delay hasn't elapsed
    ldy #$37                           ; enemy type #$37 - final boss blue blob
    jsr try_create_enemy_from_existing ; create final boss blue blob
    lda #$20                           ; shorter delay between blue blob creation
    dec ENEMY_VAR_2,x                  ; decrement number of blue blobs to create
    bne @continue                      ; set shorter delay and exit if more blobs to create
    lda #$02                           ; create #$02 blue blobs per attack
    sta ENEMY_VAR_2,x                  ; set number of blue blobs to create for next round of attack
    lda #$f0                           ; longer delay between attack rounds

@continue:
    sta ENEMY_DELAY,x ; set delay before creating next blue blob

@exit:
    rts

enemy_38_delay_tbl:
    .byte $80,$c0

enemy_38_y_adj_tbl:
    .byte $f0,$40

enemy_38_x_adj_tbl:
    .byte $00,$00

; enemy type #$6d
final_boss_routine_ptr_tbl:
    .addr final_boss_routine_00          ; set enemy destroy attributes, set HP, advance routine
    .addr final_boss_routine_01          ; wait for scroll, set boss screen scroll type, load bg collision codes for screen, advance routine
    .addr final_boss_routine_02          ; wait for drawing, set boss palette, play bg music, set animation, advance routine
    .addr final_boss_routine_03          ; shake, enable scanline interrupt, wait for delay, create crumbling rubble, rise up
    .addr final_boss_routine_04          ; wait for boss to be revealed, creating rubble. Once revealed, set hp, clear velocity, advance routine
    .addr final_boss_routine_05
    .addr final_boss_routine_06          ; main loop, animate and attack based on timers
    .addr final_boss_routine_07          ; enemy destroyed routine
    .addr final_boss_routine_08
    .addr final_boss_routine_09
    .addr bg_enemy_explosion_routine_01  ; animate sequence of explosions
    .addr set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy

; set enemy destroy attributes, set HP, advance routine
final_boss_routine_00:
    lda #$91
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision, disable bullet collision, spike explosion, sound_26 (B OUT)
    lda #$f0
    sta ENEMY_HP,x            ; set special HP not destroyed by destroy_all_enemies routine
    jmp advance_enemy_routine ; advance to next routine

; wait for scroll, set boss screen scroll type, load bg collision codes for screen, advance routine
final_boss_routine_01:
    lda SCREEN_SCROLL_TYPE              ; 0 = horizontal, 1 = vertical/overhead
    beq final_boss_exit
    lda #$00
    sta NT_CURRENT_COLUMN
    lda #$02
    sta SCREEN_SCROLL_TYPE
    ldy #$50                            ; final boss collision code indices
    jsr load_alt_collision_code_indices ; load collision code indices for final boss screen
    ldx ENEMY_CURRENT_SLOT
    lda #$08
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine           ; advance to next routine

; wait for drawing, set boss palette, play bg music, set animation, advance routine
final_boss_routine_02:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne final_boss_exit                   ; exit if still rendering
    dec ENEMY_VAR_1,x
    beq final_boss_play_sound_adv_routine
    lda #$00
    sta X_SCROLL_DRAW_POINT

final_boss_exit:
    rts

; reveal final boss (rise from ground)
final_boss_play_sound_adv_routine:
    ldy #$ac                        ; light magenta, medium magenta, dark magenta
    jsr set_level_palette           ; set sprite palette 3 if boss not defeated
    ldx ENEMY_CURRENT_SLOT
    lda #$30                        ; sound_30 (GREAT) - Great Heli - Ruined Base (Boss 1)
    jsr play_sound                  ; play sound_30 (GREAT)
    lda #$01
    sta ENEMY_VAR_4,x               ; initialize the number of blue poisonous insect gel create count
    sta ENEMY_ANIMATION_DELAY,x
    lda #$00
    sta ENEMY_ATTRIBUTES,x
    lda #$40
    jmp set_delay_adv_enemy_routine ; set delay to #$40 and set routine to final_boss_routine_03

; shake, enable scanline interrupt, wait for delay, create crumbling rubble, rise up
final_boss_routine_03:
    jsr final_boss_earthquake                ; create earthquake effect when ENEMY_VAR_4,x is non-zero
    jsr final_boss_apply_vel_set_nt_irq_data
    dec ENEMY_DELAY,x
    bne final_boss_exit
    lda #$02
    sta $08

@create_rubble_loop:
    ldy #$6e                           ; enemy type = falling rubble
    jsr try_create_enemy_from_existing ; create final boss falling rubble
    bcc @continue                      ; branch if unable to create final boss falling rubble
    ldy $11
    lda $08
    sta ENEMY_VAR_1,y
    dec $08
    bpl @create_rubble_loop

@continue:
    jsr final_boss_collapse_wall          ; write a single row of black tiles for collapsing back wall
                                          ; uses ENEMY_ATTRIBUTES,x to know which row to remove
    lda #$10
    sta ENEMY_DELAY,x
    inc ENEMY_ATTRIBUTES,x                ; move to next row of wall to hide
    lda ENEMY_ATTRIBUTES,x
    cmp #$14                              ; see if completed all rows
    bcc final_boss_exit                   ; exit if more rows to collapse
    lda #$01                              ; finished collapsing back wall, set horizontal mirroring
    sta NT_MIRRORING                      ; set nametable mirroring (0: vertical; 1: horizontal)
    lda #$c8
    sta ENEMY_X_POS,x                     ; set final boss X position
    lda #$58
    sta ENEMY_Y_POS,x                     ; set final boss Y position
    lda #$01
    sta ENEMY_VAR_6,x
    lda #$00
    sta X_SCROLL                          ; set PPU horizontal scroll to no scroll
    sta Y_SCROLL                          ; set PPU vertical scroll to top of nametables (no scroll)
    lda #$a8
    sta PPUCTRL_SETTINGS                  ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
    jsr init_irq_scroll                   ; set post-irq scroll to current scroll
    lda #$0d
    sta IRQ_TYPE                          ; set irq routine type to irq_handler_0d_ptr_tbl
                                          ; level 8 final boss
    jsr final_boss_set_irq_data           ; set scanline IRQ data for boss
    jsr init_bg_boss_pre_irq_max_scrolls  ; init boss screen pre-irq scroll max values
    lda #$00
    sta ENEMY_VAR_6,x
    lda #$e0
    sta ENEMY_Y_POS,x
    jsr final_boss_set_scroll_nt_irq_data
    ldy #$00
    jsr final_boss_set_y_vel              ; set rising Y velocity
    lda #$40
    jmp set_delay_adv_enemy_routine       ; set delay to #$40 and set routine to final_boss_routine_04

; wait for boss to be revealed, creating rubble. Once revealed, set hp, clear velocity, advance routine
final_boss_routine_04:
    lda #$01
    sta ENEMY_SPRITE,x                 ; set invisible sprite (uses bg tiles instead)
    jsr final_boss_earthquake_nt_irq
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    cmp #$5a
    bcc final_boss_set_hp_adv_routine  ; branch if Y position is less than #$5a to
                                       ; set HP, destroy attributes, clear X scroll and Y velocity, advance routine
    dec ENEMY_DELAY,x                  ; boss not yet fully revealed
    bne final_boss_exit2
    lda #$01
    sta ENEMY_DELAY,x
    ldy #$6e                           ; enemy type = falling rubble
    jsr try_create_enemy_from_existing ; create final boss falling rubble
    bcc final_boss_exit2               ; branch if unable to create final boss falling rubble
    ldy $11
    lda #$01
    sta ENEMY_VAR_2,y                  ; set remaining number of times to blink before firing #$08

final_boss_exit2:
    rts

; set HP, destroy attributes, clear X scroll and Y velocity, advance routine
final_boss_set_hp_adv_routine:
    lda #$30                        ; HP = #$30, #$40, or #$48
    jsr set_enemy_hp_hard           ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$10
    sta ENEMY_DESTROY_ATTRS,x       ; enable collision, set destroy sound to sound_26 (B OUT) - boss destroy
    jsr clear_x_scroll
    jsr clear_enemy_y_vel           ; set enemy Y velocity to 0
    lda #$20
    jmp set_delay_adv_enemy_routine ; set delay to #$20 and set routine to final_boss_routine_05

final_boss_routine_05:
    jsr final_boss_apply_vel_set_nt_irq_data
    dec ENEMY_DELAY,x
    bne final_boss_exit2
    ldy GRAPHICS_BUFFER_OFFSET
    lda #$06
    sta CPU_GRAPHICS_BUFFER,y
    lda #$2d
    sta CPU_GRAPHICS_BUFFER+1,y
    lda #$d6
    sta CPU_GRAPHICS_BUFFER+2,y
    lda #$02
    sta CPU_GRAPHICS_BUFFER+3,y
    lda #$b9
    sta CPU_GRAPHICS_BUFFER+4,y
    lda #$ba
    sta CPU_GRAPHICS_BUFFER+5,y
    lda #$ff
    sta CPU_GRAPHICS_BUFFER+6,y
    tya
    clc                                      ; clear carry in preparation for addition
    adc #$07
    sta GRAPHICS_BUFFER_OFFSET
    ldy #$b0                                 ; white, dark gray, dark teal
    jsr set_level_palette                    ; set sprite palette 3 if boss not defeated
    ldx ENEMY_CURRENT_SLOT
    lda #$20
    sta ENEMY_ANIMATION_DELAY,x
    lda #$00
    sta ENEMY_FRAME,x
    sta ENEMY_VAR_3,x
    lda #$06
    sta ENEMY_VAR_2,x
    lda #$30
    sta ENEMY_DELAY,x
    jmp advance_enemy_routine                ; advance to next routine

; main loop, animate and attack based on timers
final_boss_routine_06:
    jsr final_boss_animate_attack            ; animate eye and fire blue and red insect gels based on timers
    jmp final_boss_apply_vel_set_nt_irq_data

; enemy destroyed routine
final_boss_routine_07:
    ldy #$02
    jsr final_boss_set_y_vel           ; set sinking Y velocity
    jsr final_boss_earthquake_nt_irq
    jmp enemy_routine_boss_defeated_00

final_boss_routine_08:
    jsr final_boss_earthquake_nt_irq
    jmp enemy_routine_boss_defeated_01

final_boss_routine_09:
    jsr set_nmi_noop_irq              ; remove any scanline interrupts
    jsr clear_x_scroll
    jmp bg_enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

; animate eye and fire blue and red insect gels based on timers
final_boss_animate_attack:
    dec ENEMY_DELAY,x
    bne @check_red_gel_delay
    inc ENEMY_VAR_4,x        ; increment number of blue insect gels created
    lda ENEMY_VAR_4,x
    cmp #$0a                 ; see if created max amount
    lda #$14
    bcc @create_insect       ; branch if more blue insect gels more to create
    lda #$00                 ; no delay between blue insect gel creation
    sta ENEMY_VAR_4,x        ; reset blue insect gel count
    lda #$80

@create_insect:
    sta ENEMY_DELAY,x         ; set delay until next blue insect gel enemy generation
    ldy #$37                  ; enemy type #$37 (blue poisonous insect gel)
    sty $17                   ; set enemy type in $17 (for use in copy_enemy_vars)
    jsr create_and_init_enemy ; create blue poisonous insect gel
    jsr copy_enemy_vars

@check_red_gel_delay:
    lda ENEMY_FIRING_DELAY,x              ; load red insect gel creation delay timer
    beq @continue
    dec ENEMY_FIRING_DELAY,x
    bne @continue                         ; branch if timer not yet elapsed
    jsr final_boss_create_red_insect_gels ; target player and create #$08 red insect gels

@continue:
    dec ENEMY_ANIMATION_DELAY,x ; decrement eye animation delay
    bne @exit                   ; exit if blink animation hasn't elapsed
    lda ENEMY_VAR_3,x           ; load eye state (0 = opening, 1 = closing)
    bne @close_eye
    inc ENEMY_FRAME,x           ; move to next frame (0 = eye closed, 1 = half open, 2 = open)
    jsr final_boss_draw_eye     ; draw the eye for the final boss' ENEMY_FRAME,x
    lda ENEMY_FRAME,x
    cmp #$02                    ; see if past last frame
    lda #$08                    ; short opening delay
    bcc @set_delay_exit         ; branch if valid eye frame index
    lda #$10                    ; eye fully open, use a longer delay
    dec ENEMY_VAR_2,x           ; decrement remaining blinks before firing red insect gels
    bne @change_eye_direction   ; branch if more blinks before firing
    lda #$04
    sta ENEMY_FIRING_DELAY,x    ; set gel insect creation delay timer
    lda #$06
    sta ENEMY_VAR_2,x           ; re-initialize number of blinks before firing again
    lda #$40
    bne @change_eye_direction   ; always branch

@close_eye:
    dec ENEMY_FRAME,x       ; move to previous frame (0 = eye closed, 1 = half open, 2 = open)
    jsr final_boss_draw_eye ; draw the eye for the final boss' ENEMY_FRAME,x
    lda #$08
    ldy ENEMY_FRAME,x
    bne @set_delay_exit
    lda #$10                ; eye closed

@change_eye_direction:
    tay               ; transfer delay to y
    lda ENEMY_VAR_3,x ; load eye state (0 = opening, 1 = closing)
    eor #$01
    sta ENEMY_VAR_3,x ; swap eye open/close direction
    tya               ; transfer delay to a

@set_delay_exit:
    sta ENEMY_ANIMATION_DELAY,x ; set eye animation delay

@exit:
    rts

; draws the eye for the final boss' ENEMY_FRAME,x
final_boss_draw_eye:
    lda ENEMY_FRAME,x ; load frame of eye to draw
    cmp #$03
    bcc @continue     ; branch if valid frame index
    lda #$02          ; set fully open if invalid frame

@continue:
    asl                                  ; double since each entry is a #$02 byte memory address
    tay                                  ; transfer to offset register
    lda final_boss_eye_tiles_ptr_tbl,y
    sta $08
    lda final_boss_eye_tiles_ptr_tbl+1,y
    sta $09
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET

@write_tile_loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    iny
    inx
    cmp #$ff
    bne @write_tile_loop
    stx GRAPHICS_BUFFER_OFFSET
    ldx ENEMY_CURRENT_SLOT
    rts

final_boss_eye_tiles_ptr_tbl:
    .addr final_boss_eye_tiles_00 ; eye closed
    .addr final_boss_eye_tiles_01 ; half open
    .addr final_boss_eye_tiles_02 ; fully opened

; block mode
; #$02 tiles at PPU address $2d58
; #$02 tiles at PPu address $2d78
final_boss_eye_tiles_00:
    .byte $06,$2d,$58,$02,$0d,$0e,$2d,$78,$02,$16,$17,$ff

; block mode
; #$02 tiles at PPU address $2d58
; #$02 tiles at PPu address $2d78
final_boss_eye_tiles_01:
    .byte $06,$2d,$58,$02,$c6,$c7,$2d,$78,$02,$c8,$c9,$ff

; block mode
; #$02 tiles at PPU address $2d58
; #$02 tiles at PPu address $2d78
final_boss_eye_tiles_02:
    .byte $06,$2d,$58,$02,$ca,$cb,$2d,$78,$02,$cc,$cd,$ff

; creates #$08 red insect gels (enemy type #$2b)
final_boss_create_red_insect_gels:
    jsr player_enemy_x_dist      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$05                     ; 1.62x speed
    jsr set_vel_to_target_player ; set enemy velocity to target player index $0a at 1.62x speed
    ldy #$07

@red_insect_create_loop:
    sty $08
    lda #$2b                           ; enemy type = final boss poisonous insect gel (red blob)
    tay
    jsr try_create_enemy_from_existing ; create final boss poisonous insect gel (red blob)
    bcc @exit                          ; exit if unable to create final boss poisonous insect gel (red blob)
    ldy $11
    lda $08
    sta ENEMY_VAR_1,y
    lda ENEMY_Y_VELOCITY_FRACT,x       ; load enemy's fractional Y velocity
    sta ENEMY_Y_VELOCITY_FRACT,y
    lda ENEMY_Y_VELOCITY_FAST,x        ; load enemy's fast Y velocity
    sta ENEMY_Y_VELOCITY_FAST,y
    lda ENEMY_X_VELOCITY_FRACT,x       ; load enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,y
    lda ENEMY_X_VELOCITY_FAST,x        ; load enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,y
    ldy $08
    dey
    bpl @red_insect_create_loop

@exit:
    jmp clear_enemy_vel ; set enemy X and Y velocity to 0

clear_x_scroll:
    lda #$00
    sta X_SCROLL     ; set PPU horizontal scroll to no scroll
    sta IRQ_X_SCROLL
    sta Y_SCROLL     ; set PPU vertical scroll to top of nametables (no scroll)
    rts

; set boss' rising/sinking velocity
; input
;  * y - index into final_boss_y_vel_tbl (0 = -0.25, 2 = 0.25)
;  * x - enemy slot offset
final_boss_set_y_vel:
    lda final_boss_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda final_boss_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    rts

final_boss_y_vel_tbl:
    .byte $c0,$ff ; y velocity = -0.25
    .byte $40,$00 ; y velocity =  0.25

; write a single row of black tiles for collapsing back wall
; input
;  * ENEMY_ATTRIBUTES,x - row number
final_boss_collapse_wall:
    lda #$00
    sta $00
    ldy GRAPHICS_BUFFER_OFFSET
    lda #$03
    sta CPU_GRAPHICS_BUFFER,y   ; set repeat mode
    lda ENEMY_ATTRIBUTES,x      ; load the row number (0 = top)
    asl
    rol $00
    asl
    rol $00
    asl
    rol $00
    asl
    rol $00
    asl                         ; for larger row numbers, bits are pushed to $00
    rol $00                     ; which cause the next PPU address high byte to increment
    clc                         ; clear carry in preparation for addition
    adc #$9c
    sta CPU_GRAPHICS_BUFFER+2,y ; set PPU write address low byte
    lda $00
    adc #$20                    ; add any carry to starting row
    sta CPU_GRAPHICS_BUFFER+1,y ; set PPU write address high byte
    lda #$04
    sta CPU_GRAPHICS_BUFFER+3,y ; writing 4 bytes
    lda #$00
    sta CPU_GRAPHICS_BUFFER+4,y ; writing black tile 4 times
    tya
    clc                         ; clear carry in preparation for addition
    adc #$05
    sta GRAPHICS_BUFFER_OFFSET  ; set new graphics buffer offset
    rts

; create earthquake effect when ENEMY_VAR_4,x is non-zero
final_boss_earthquake:
    lda ENEMY_VAR_4,x
    beq @exit
    jmp earthquake_shake ; shake vertically and horizontally based on GLOBAL_TIMER

@exit:
    rts

final_boss_earthquake_nt_irq:
    jsr final_boss_earthquake             ; create earthquake effect when ENEMY_VAR_4,x is non-zero
    jsr shake_enemy_pattern               ; shake enemy as it rises/falls based on GLOBAL_TIMER
    jmp final_boss_set_scroll_nt_irq_data

final_boss_apply_vel_set_nt_irq_data:
    jsr bg_boss_apply_vel

final_boss_set_scroll_nt_irq_data:
    jsr set_bg_boss_scroll_nt

; set scanline IRQ data, an in particular the size of the region for the boss
; which gets larger as boss rises
final_boss_set_irq_data:
    .ifdef Probotector
        lda #$dc
        ldy IRQ_TYPE
        beq @continue
    .endif
    lda #$f0
    sec                ; set carry flag in preparation for subtraction
    sbc IRQ_Y_SCROLL   ; subtract vertical scroll for use after 1st IRQ
    clc                ; clear carry in preparation for addition
    .ifdef Probotector
        adc #$24
    .else
        adc #$31
    .endif

@continue:
    sta SCANLINE_IRQ_1      ; set number of scanlines until first interrupt
    lda #$05
    sta SCANLINE_IRQ_2_DIFF ; set the number of scanlines after SCANLINE_IRQ_1 to run the next scanline IRQ
                            ; this creates a small black region above the final boss #$05 scanlines tall
    .ifdef Probotector
        lda #$ad
    .else
        lda #$ba
    .endif
    sec                     ; set carry flag in preparation for subtraction
    sbc SCANLINE_IRQ_1
    sta SCANLINE_IRQ_3_DIFF ; set size of region showing boss bg
                            ; number of scanlines after 2nd scanline interrupt (irq_handler_0d_01)
    lda #$c0
    sta IRQ_PPUADDR
    lda #$2c
    sta IRQ_PPUADDR+1       ; set PPU address for after 1st interrupt to $2cc0
    rts

; enemy type #$6e
falling_rubble_routine_ptr_tbl:
    .addr falling_rubble_routine_00 ; set initial position and velocities
    .addr falling_rubble_routine_01 ; animate and apply stronger gravity to fall faster

falling_rubble_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x              ; mark falling rubble so bullets travel through it
    lda #$03
    sta ENEMY_SPRITE_ATTR,x                ; set palette for falling rubble
    lda ENEMY_VAR_2,x
    bne @random_rubble
    lda ENEMY_ATTRIBUTES,x                 ; crumbling side wall before seeing final boss
                                           ; load amount to add to initial Y position
    asl
    asl
    asl                                    ; triple amount below initial y offset
    adc #$24
    sta ENEMY_Y_POS,x                      ; set initial Y position
    ldy ENEMY_VAR_1,x                      ; load initial X position offset
    lda falling_rubble_initial_x_pos_tbl,y
    sta ENEMY_X_POS,x                      ; set falling rubble initial X position
    lda falling_rubble_anim_delay_tbl,y
    jmp set_delay_adv_enemy_routine        ; set delay and set routine to falling_rubble_routine_01

; use initial random X position, and random X and Y velocities
; crumbling rock as final boss ascends from ground
@random_rubble:
    lda #$c0
    sta ENEMY_Y_POS,x                ; set initial Y position
    lda RANDOM_NUM                   ; load random number
    and #$78
    ora #$80
    sta ENEMY_X_POS,x
    lda RANDOM_NUM                   ; load random number
    and #$03
    asl
    tay
    lda falling_rubble_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x     ; store enemy's fractional Y velocity
    lda falling_rubble_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x      ; store enemy's fast Y velocity
    lda RANDOM_NUM                   ; load random number
    bmi @adv_routine                 ; branch to skip setting X velocity randomly
    and #$04
    lsr
    tay
    lda falling_rubble_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x     ; store enemy's fractional X velocity
    lda falling_rubble_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x      ; store enemy's fast X velocity
                                     ; either 0.25 or -0.25

@adv_routine:
    jmp advance_enemy_routine ; advance to next routine

falling_rubble_initial_x_pos_tbl:
    .byte $e8,$f0,$f8

falling_rubble_anim_delay_tbl:
    .byte $00,$06,$03

falling_rubble_y_vel_tbl:
    .byte $00,$ff ; Y velocity = -1.00
    .byte $c0,$fe ; Y velocity = -1.25
    .byte $40,$ff ; Y velocity = -0.75
    .byte $80,$ff ; Y velocity = -0.50

falling_rubble_x_vel_tbl:
    .byte $40,$00 ; X velocity =  0.25
    .byte $c0,$ff ; X velocity = -0.25

; animate and apply stronger gravity to fall faster
falling_rubble_routine_01:
    lda ENEMY_DELAY,x
    beq @continue     ; continue if delay elapsed
    dec ENEMY_DELAY,x
    rts

@continue:
    ldy #$17                       ; animation index for falling rubble
    jsr set_enemy_animation_sprite ; animate sprite for falling rubble based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda #$20
    jsr add_a_to_enemy_y_fract_vel ; speed up falling effect
    jmp apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen

; enemy type #$3b
manooki_routine_ptr_tbl:
    .addr manooki_routine_00
    .addr manooki_routine_01
    .addr manooki_routine_02
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

manooki_routine_00:
    lda #$f1                  ; special invincible HP
    sta ENEMY_HP,x            ; any HP greater than #$f0 is invincible
                              ; bullet-enemy collision sound still plays (sound_11)
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x ; enable player-enemy and player bullet-enemy collision
    lda #$10
    sta ENEMY_FIRING_DELAY,x
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

manooki_routine_01:
    jsr manooki_set_sprite    ; set sprite attribute, and sprite
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    cmp #$b8
    bcs @exit
    lda #$18                  ; HP = #$18, #$1c, or #$1f
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine ; advance to next routine

@exit:
    rts

manooki_routine_02:
    jsr player_enemy_x_dist         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    sty $12
    lda #$28                        ; add #$28 to enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    bne @collision
    lda #$14
    jsr add_a_to_enemy_y_fract_vel
    jsr clear_enemy_x_vel           ; set enemy X velocity (fast and fractional) to 0
    jmp @create_projectile

@collision:
    jsr set_y_pos_for_bg ; set enemy Y position based on background collision
    ldy #$00
    lda ENEMY_VAR_1,x
    sec                  ; set carry flag in preparation for subtraction
    sbc ENEMY_HP,x
    asl
    asl
    asl
    asl
    asl
    clc                  ; clear carry in preparation for addition
    adc ENEMY_VAR_2,x
    bcc @continue
    lda #$ff

@continue:
    sta ENEMY_VAR_2,x
    lda ENEMY_HP,x
    sta ENEMY_VAR_1,x
    lda ENEMY_VAR_2,x
    beq @set_vel
    dec ENEMY_VAR_2,x
    ldy $12
    lda ENEMY_X_POS,x         ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y
    ldy #$02
    lda #$00
    bcs @set_attr
    ldy #$04
    lda #$01

@set_attr:
    sta ENEMY_ATTRIBUTES,x

@set_vel:
    lda #$00
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda #$01
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda manooki_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda manooki_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity

@create_projectile:
    jsr manooki_set_sprite                ; set sprite attribute, and sprite
    jsr apply_velocity                    ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_FIRING_DELAY,x
    bne @exit
    ldy #$3c                              ; enemy type = manooki projectile
    jsr try_create_enemy_from_existing    ; create manooki projectile
    bcc @set_delay_exit                   ; branch if unable to create manooki projectile
    lda ENEMY_ATTRIBUTES,x
    and #$01
    tay
    lda manooki_projectile_x_offset_tbl,y
    ldy $11
    clc                                   ; clear carry in preparation for addition
    adc ENEMY_X_POS,x
    sta ENEMY_X_POS,y
    lda ENEMY_Y_POS,x                     ; load enemy's Y position
    sec                                   ; set carry flag in preparation for subtraction
    sbc #$10
    sta ENEMY_Y_POS,y

@set_delay_exit:
    lda #$17
    sta ENEMY_FIRING_DELAY,x

@exit:
    rts

manooki_x_vel_tbl:
    .byte $00,$00 ; x vel =  0.000
    .byte $e0,$ff ; x vel = -0.125
    .byte $20,$00 ; x vel =  0.125

manooki_projectile_x_offset_tbl:
    .byte $f9 ; -7
    .byte $07 ;  7

; sets sprite attribute, and sprite based on facing direction, ENEMY_FRAME, and ENEMY_ANIM_DELAY
manooki_set_sprite:
    lda ENEMY_ATTRIBUTES,x
    lsr                    ; push facing direction to carry
    lda #$03
    bcc @set_sprite        ; branch if facing right
    lda #$43               ; facing left, flip sprite horizontally

@set_sprite:
    sta ENEMY_SPRITE_ATTR,x
    ldy #$0e                       ; animation index for manooki
    jmp set_enemy_animation_sprite ; animate sprite for manooki based on ENEMY_FRAME and ENEMY_ANIM_DELAY

manooki_projectile_routine_ptr_tbl:
    .addr manooki_projectile_routine_00
    .addr manooki_projectile_routine_01
    .addr remove_enemy                  ; enemy destroyed routine

manooki_projectile_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x            ; mark manooki projectile so bullets travel through it
    lda ENEMY_ATTRIBUTES,x
    clc                                  ; clear carry in preparation for addition
    adc RANDOM_NUM
    and #$07
    asl
    tay
    lda manooki_projectile_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x         ; store enemy's fractional X velocity
    lda manooki_projectile_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x          ; store enemy's fast X velocity
    lda RANDOM_NUM                       ; load random number
    and #$07
    asl
    tay
    lda manooki_projectile_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x         ; store enemy's fractional Y velocity
    lda manooki_projectile_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x          ; store enemy's fast Y velocity
    lda #$02
    sta ENEMY_SPRITE_ATTR,x
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcs @adv_routine
    jsr flip_enemy_x_dir                 ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

@adv_routine:
    jmp advance_enemy_routine ; advance to next routine

manooki_projectile_y_vel_tbl:
    .byte $00,$ff,$10,$ff,$20,$ff,$30,$ff,$40,$ff,$50,$ff,$60,$ff,$70,$ff

manooki_projectile_x_vel_tbl:
    .byte $a0,$00,$c0,$00,$e0,$00,$00,$01,$20,$01,$40,$01,$60,$01,$80,$01

manooki_projectile_routine_01:
    ldy #$0f                        ; animation index for manooki projectile
    jsr set_enemy_animation_sprite  ; animate sprite for manooki projectile based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_Y_VELOCITY_FRACT,x    ; load enemy's fractional Y velocity
    clc                             ; clear carry in preparation for addition
    adc #$0c
    sta ENEMY_Y_VELOCITY_FRACT,x    ; store enemy's fractional Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x     ; load enemy's fast Y velocity
    adc #$00
    sta ENEMY_Y_VELOCITY_FAST,x     ; store enemy's fast Y velocity
    lda #$00                        ; don't adjust enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    beq @exit
    jmp remove_enemy

@exit:
    rts

; enemy type #$3d
spider_spawn_routine_ptr_tbl:
    .addr spider_spawn_routine_00    ; initialize HP, X pos, frame, spawn delay timer, advance routine
    .addr spider_spawn_routine_01    ; animate, and after enough loops, advance routine
    .addr spider_spawn_routine_02    ; wait for animation delay, wait for animation frame 1, move to frame 2, create spider, set longer delay, set routine to spider_spawn_routine_01
    .addr spider_spawn_routine_03    ; enemy destroyed routine - set destroyed supertile, run enemy_explosion_routine_00
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize HP, X pos, frame, advance routine
spider_spawn_routine_00:
    lda #$10                        ; HP = #$10, #$14, or #$17
    jsr set_enemy_hp                ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda ENEMY_X_POS,x               ; load enemy's X position
    sec                             ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_X_POS,x               ; subtract 8 from enemy's X position
    lda #$01
    sta ENEMY_FRAME,x               ; initialize enemy animation frame
    lda #$03
    sta ENEMY_VAR_2,x               ; initialize timer delaying creation of enemy spider
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$01
    jmp set_delay_adv_enemy_routine ; set animation delay to #$01 and set routine to spider_spawn_routine_01

; animate, and after enough loops, advance routine
spider_spawn_routine_01:
    lda #$01
    sta ENEMY_SPRITE,x              ; set invisible sprite (uses bg tiles instead)
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x               ; decrement animation delay
    bne spider_spawn_exit           ; exit if animation delay hasn't elapsed
    jsr spider_spawn_set_supertile  ; delay elapsed, update enemy supertile based on ENEMY_FRAME
    bcs spider_spawn_set_delay_exit ; exit if unable to update the supertile try again next frame
    lda ENEMY_VAR_1,x               ; load check spider generation check skip flag
                                    ; !(HUH) this feels unnecessarily convoluted, this value is really just the opposite of ENEMY_FRAME,x
                                    ; while in this routine (spider_spawn_routine_01)
    bne spider_spawn_update_frame   ; branch if flag set to simply update frame +/- 1, set delay and exit
    lda ENEMY_FRAME,x               ; flag clear
    cmp #$01
    bcc spider_spawn_adv_frame      ; branch if on frame 0 (one of 2 dancing frames)
                                    ; to advance to frame 1, set delay to #$08, and exit
                                    ; otherwise, toggle ENEMY_VAR_1, check if need to generate spider

; input
;  * carry flag - whether or not to check if should create spider
;  * set when enemy frame = 1 when fall through from 2 lines above, clear when called by spider_spawn_update_frame
spider_spawn_upd_frame_check_adv_routine:
    lda ENEMY_VAR_1,x
    eor #$01
    sta ENEMY_VAR_1,x                     ; toggle spider generation check skip flag
    bcs spider_spawn_check_if_adv_routine ; branch enemy frame is 1 to check if need to generate spider
                                          ; this routine decrements spider creation delay, if elapsed, advance routine

spider_spawn_adv_frame:
    inc ENEMY_FRAME,x                 ; advance frame from #$00 to #$01
    bne spider_spawn_set_delay_8_exit ; always branch

; increment/decrement frame to continue frame 0 <-> frame 1 dance animation
; set delay to #$08, then exit
spider_spawn_update_frame:
    lda ENEMY_FRAME,x
    clc                                          ; clear flag to prevent spider_spawn_check_if_adv_routine check if incrementing frame
    beq spider_spawn_upd_frame_check_adv_routine ; branch if on frame 0 to increment frame
                                                 ; otherwise, decrement frame
                                                 ; both branch paths set delay to #$08, then exit

spider_spawn_dec_frame:
    dec ENEMY_FRAME,x

spider_spawn_set_delay_8_exit:
    lda #$08

spider_spawn_set_delay_exit:
    sta ENEMY_DELAY,x

spider_spawn_exit:
    rts

; decrements spider creation delay, if elapsed, re-initializes same delay, advance to frame 2, set animation delay to #$10, advance routine
spider_spawn_check_if_adv_routine:
    dec ENEMY_VAR_2,x               ; decrement spider spawn delay
    bne spider_spawn_dec_frame      ; branch to decrement frame, set delay, and exit if delay hasn't elapsed
    lda #$06
    sta ENEMY_VAR_2,x               ; initialize the spawn delay counter
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$c0                        ; compare to bottom 25% of screen
    bcs spider_spawn_dec_frame      ; branch if in bottom 25% of screen to decrement frame, and set animation delay
                                    ; without advancing the enemy routine
    inc ENEMY_FRAME,x               ; in top 75% of screen, advance frame
    lda #$10
    jmp set_delay_adv_enemy_routine ; set animation delay to #$10 and set routine to spider_spawn_routine_02

; wait for animation delay, wait for animation frame 1, move to frame 2, create spider, set longer delay, set routine to spider_spawn_routine_01
spider_spawn_routine_02:
    jsr update_enemy_pos               ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                  ; decrement animation delay
    bne spider_spawn_exit              ; exit if animation delay hasn't elapsed
    jsr spider_spawn_set_supertile     ; update enemy supertile based on ENEMY_FRAME
    bcs spider_spawn_set_delay_exit    ; exit if unable to update the supertile try again next frame
    lda ENEMY_FRAME,x
    cmp #$02
    bcc spider_spawn_adv_frame         ; branch if in dancing animation (frame 0 or frame 1), to advance to spew spider frame
    ldy #$3e                           ; at correct frame, enemy type = alien spider
    jsr try_create_enemy_from_existing ; create alien spider
    bcc @dec_enemy_routine             ; branch if unable to create alien spider
                                       ; to simply animate as if created and go back to spider_spawn_routine_01
    ldy $11                            ; load slot for created spider spawn
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    sec                                ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_Y_POS,y                  ; set spider initial Y position to 8 pixels above spider spawn

; handle animation delay for creating spider (even if spider wasn't successfully created)
@dec_enemy_routine:
    dec ENEMY_FRAME,x     ; move back to frame 1
    lda #$20
    sta ENEMY_DELAY,x     ; set longer animation delay
    lda #$02
    jmp set_enemy_routine ; set routine to spider_spawn_routine_01

; enemy destroyed routine - set destroyed supertile, run enemy_explosion_routine_00
spider_spawn_routine_03:
    ldy #$03
    jsr spider_spawn_set_supertile_y ; set enemy destroyed supertile
    bcs spider_spawn_exit            ; exit if unable to update the supertile to try again next frame
    jmp enemy_explosion_routine_00   ; set empty sprite, play optional enemy destroyed sound, disable collisions

; uses the current ENEMY_FRAME to update the supertile for the alien spider spawn
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
;  * a - used when unable to update supertile to specify delay until retry (always #$01)
spider_spawn_set_supertile:
    ldy ENEMY_FRAME,x

; input
;  * y - alien spider spawn frame index
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
spider_spawn_set_supertile_y:
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    cmp #$20                             ; see if in top 12.5% of screen
    bcc @exit_no_update                  ; exit without updating nametable if in top part of screen
    cmp #$d0                             ; see if in bottom 18.75% of screen
    bcs @exit_no_update                  ; exit without updating nametable if in bottom part of screen
    lda spider_spawn_supertile_tbl,y     ; load supertile to draw based on y (offsets into level_7_supertile_data)
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)

@exit:
    lda #$01
    rts

@exit_no_update:
    sec
    bcs @exit ; always branch to exit without updating nametable

; level_7_supertile_data offset
; loops between #$6e and #$6f (dance effect), then changes to #$70 to spew spider
; #$37 is for alien spider spawn destroyed
spider_spawn_supertile_tbl:
    .byte $6e,$6f,$70,$37

; enemy type #$3e
alien_spider_routine_ptr_tbl:
    .addr alien_spider_routine_00
    .addr alien_spider_routine_01
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

alien_spider_routine_00:
    lda #$01
    sta ENEMY_VAR_1,x
    lda RANDOM_NUM                 ; load random number
    and #$03                       ; strip to numbers 0 to #$03
    asl
    asl                            ; determines alien_spider_vel_tbl offset
    tay                            ; transfer random velocity index to offset register
    jsr alien_spider_target_player ; target closest player with velocity determined by y
    jmp advance_enemy_routine      ; advance to next routine

; targets closest player with velocity determined by y
; input
;  * y - alien_spider_vel_tbl offset, determines spider's velocities
alien_spider_target_player:
    lda alien_spider_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda alien_spider_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda alien_spider_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda alien_spider_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    jsr player_enemy_x_dist      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_X_POS,x            ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y    ; compare to closest player's position
    lda #$43                     ; sprite attribute with sprite flipped horizontally and palette #$03
    bcc @continue                ; branch if spider to left of targeted player
    jsr flip_enemy_x_dir         ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25
    lda #$03                     ; sprite palette with no horizontal sprite flip

@continue:
    sta ENEMY_SPRITE_ATTR,x ; set sprite palette and horizontal flip
    rts

alien_spider_routine_01:
    jsr @run_spider_routine        ; run main spider logic
    jmp update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)

@run_spider_routine:
    lda ENEMY_VAR_1,x              ; 1 = spider in air, 0 = spider crawling
    beq spider_crawling            ; branch if spider is crawling
    lda #$9e                       ; spider is in air, sprite_9e
    sta ENEMY_SPRITE,x             ; set enemy sprite to sprite_9e (alien spider in air)
    lda #$18
    jsr add_a_to_enemy_y_fract_vel ; apply gravity
    lda #$10                       ; use point #$10 in front of spider when determining wall collision
    ldy ENEMY_X_VELOCITY_FAST,x    ; load enemy's fast X velocity
    bpl @test_collisions           ; branch if spider moving to right
    lda #$f0                       ; use point #$10 in front of spider when determining wall collision

@test_collisions:
    jsr check_bg_wall_collision ; check for wall collision (horizontal collision)
    beq @continue               ; branch if no wall collision
    jsr clear_enemy_x_vel       ; collided with wall, clear X velocity
                                ; set enemy X velocity (fast and fractional) to 0

@continue:
    lda ENEMY_VAR_6,x           ; load whether off screen vertically
    bne @exit                   ; exit if off screen vertically
    lda #$0e                    ; add #$0e to enemy Y position when determining background collision
    ldy ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @test_ground_collision
    lda #$f2                    ; subtract #$14 from enemy Y position when determining background collision

@test_ground_collision:
    jsr get_enemy_bg_collision_code ; test background collision
    beq @exit                       ; exit if no bg collision
    ldy ENEMY_Y_VELOCITY_FAST,x     ; load enemy's fast Y velocity
    bpl @landed_on_ground           ; branch if no Y velocity or falling
    jmp clear_enemy_y_vel           ; set enemy Y velocity to 0

@landed_on_ground:
    ldy #$10                       ; no Y velocity, +/- 2 X velocity crawl speed
    jsr alien_spider_target_player ; target closest player with velocity determined by y
    lda #$00
    sta ENEMY_VAR_1,x              ; set spider on ground crawling

@exit:
    rts

spider_crawling:
    ldy #$10                       ; animation index for alien spider
    jsr set_enemy_animation_sprite ; animate sprite for alien spider based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda #$10                       ; #$10 pixels in front of spider (used for bg collision detection)
    ldy ENEMY_X_VELOCITY_FAST,x    ; load enemy's fast X velocity
    bpl @continue                  ; branch if moving right
    lda #$f0                       ; #$10 pixels in front of spider (used for bg collision detection)

@continue:
    jsr check_bg_wall_collision ; see if ran into wall
    beq @continue2              ; branch if did not run into a wall
    jsr flip_enemy_x_dir        ; ran into a wall, turn around
                                ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

@continue2:
    lda #$0e                                 ; add #$0e to spider Y position for testing
    jsr get_enemy_bg_collision_code_onscreen ; see if ran off edge
    beq @not_on_ground                       ; branch if ran off edge
    jmp set_y_pos_for_bg                     ; still on ground
                                             ; set enemy Y position based on background collision

@not_on_ground:
    lda #$01
    sta ENEMY_VAR_1,x       ; set spider in air flag
    jsr clear_enemy_y_vel   ; set enemy Y velocity to 0
    ldy #$00                ; assume falling left (-0.5 x vel)
    lda ENEMY_SPRITE_ATTR,x
    asl                     ; pushing horizontal sprite flip bit to bit 7
    bpl @set_falling_x_vel  ; branch if falling left
    ldy #$02                ; falling right (0.5 x vel)

@set_falling_x_vel:
    lda alien_spider_falling_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x           ; store enemy's fractional X velocity
    lda alien_spider_falling_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x            ; store enemy's fast X velocity
    rts

; the X velocity direction is determined based on targeting player location
alien_spider_vel_tbl:
    .byte $00,$fe,$80,$00 ; y vel = -2, x vel = +/-0.5
    .byte $00,$fe,$00,$01 ; y vel = -2, x vel = +/-1.0
    .byte $00,$fe,$80,$01 ; y vel = -2, x vel = +/-1.5
    .byte $00,$fe,$00,$02 ; y vel = -2, x vel = +/-2.0
    .byte $00,$00,$00,$02 ; y vel =  0, x vel = +/-2.0

alien_spider_falling_x_vel_tbl:
    .byte $80,$ff ; x vel = -0.5
    .byte $80,$00 ; x vel =  0.5

; enemy type #$52
temple_of_terror_core_routine_ptr_tbl:
    .addr temple_of_terror_core_routine_00 ; initialize HP, position, frame, set delay and advance routine
    .addr temple_of_terror_core_routine_01
    .addr temple_of_terror_core_routine_02
    .addr temple_of_terror_core_routine_03
    .addr temple_of_terror_core_routine_04 ; enemy destroyed routine
    .addr enemy_explosion_routine_01       ; animate explosion sequence
    .addr temple_of_terror_core_routine_06

; initialize HP, position, frame, set delay and advance routine
temple_of_terror_core_routine_00:
    lda #$01
    sta ENEMY_SPRITE,x              ; set invisible sprite
    lda #$01
    sta ENEMY_DESTROY_ATTRS,x       ; disable player enemy collision
    lda #$18                        ; HP = #$18, #$1c, or #$1f
    jsr set_enemy_hp                ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    sta ENEMY_VAR_1,x               ; set to use as official HP
    lda #$f0                        ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x                  ; instead ENEMY_VAR_1,x is used to track HP
    lda #$70
    sta ENEMY_Y_POS,x
    lda #$80
    sta ENEMY_X_POS,x               ; set position to (#$80, #$70)
    lda #$01
    sta ENEMY_FRAME,x
    lda #$40
    jmp set_delay_adv_enemy_routine ; set delay to #$40 and set routine to temple_of_terror_core_routine_01

temple_of_terror_core_routine_01:
    dec ENEMY_DELAY,x
    bne temple_of_terror_core_exit             ; exit if delay hasn't elapsed
    jsr temple_of_terror_draw_frame_supertiles
    lda #$01
    bcs temple_of_terror_core_set_delay_exit
    lda ENEMY_FRAME,x
    cmp #$02
    lda #$08
    bcc @next_frame
    dec ENEMY_FRAME,x
    lda ENEMY_VAR_1,x
    sta ENEMY_HP,x
    lda #$10
    sta ENEMY_FIRING_DELAY,x
    lda #$70
    jmp set_delay_adv_enemy_routine            ; set delay to #$70 and set routine to temple_of_terror_core_routine_02

@next_frame:
    inc ENEMY_FRAME,x

temple_of_terror_core_set_delay_exit:
    sta ENEMY_DELAY,x

temple_of_terror_core_exit:
    rts

temple_of_terror_core_prev_frame:
    dec ENEMY_FRAME,x
    jmp temple_of_terror_core_set_delay_exit

temple_of_terror_core_routine_02:
    dec ENEMY_FIRING_DELAY,x
    bne @continue
    ldy #$54                           ; enemy type = temple of terror core fire ring projectile
    jsr try_create_enemy_from_existing ; create temple of terror core fire ring projectile

@continue:
    dec ENEMY_DELAY,x
    bne temple_of_terror_core_exit
    lda ENEMY_HP,x
    sta ENEMY_VAR_1,x
    lda #$f0                        ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to temple_of_terror_core_routine_03

temple_of_terror_core_routine_03:
    dec ENEMY_DELAY,x
    bne temple_of_terror_core_exit
    jsr temple_of_terror_draw_frame_supertiles
    lda #$01
    bcs temple_of_terror_core_set_delay_exit
    lda #$08
    ldy ENEMY_FRAME,x
    bne temple_of_terror_core_prev_frame
    inc ENEMY_FRAME,x
    lda #$50
    sta ENEMY_DELAY,x
    lda #$02
    jmp set_enemy_routine                      ; set routine to temple_of_terror_core_routine_01

; enemy destroyed routine
temple_of_terror_core_routine_04:
    lda #$03                               ; temple_of_terror_supertile_offset_tbl offset
    jsr temple_of_terror_draw_supertiles_a
    bcs temple_of_terror_core_exit
    jmp enemy_explosion_routine_00         ; set empty sprite, play optional enemy destroyed sound, disable collisions

temple_of_terror_core_routine_06:
    ldy ENEMY_VAR_5,x
    lda #$01
    sta ENEMY_VAR_1,y
    jmp enemy_explosion_routine_03 ; mark destroyed, remove enemy

; draws 4 supertiles where a indicates which 4, and y indicates position
temple_of_terror_draw_frame_supertiles:
    lda ENEMY_FRAME,x

; draws 4 supertiles where a indicates which 4, and y indicates position
; input
;  * a - starting supertile to draw (multiplied by 4 to index into temple_of_terror_supertile_offset_tbl)
temple_of_terror_draw_supertiles_a:
    ldy #$00

; draws 4 supertiles where a indicates which 4, and y indicates position
; input
;  * a - starting supertile to draw (multiplied by 4 to index into temple_of_terror_supertile_offset_tbl)
;  * y - base offset of position to draw
temple_of_terror_draw_supertiles:
    sty $0e
    asl
    asl                        ; each row has 4 supertiles
    sta $0d                    ; set starting supertile offset to draw
    lda GRAPHICS_BUFFER_OFFSET
    bne @exit                  ; exit if drawing something else
    ldy #$03                   ; drawing #$04 supertiles

@loop:
    sty $0f                                     ; set remaining number of supertiles to draw
    tya
    clc                                         ; clear carry in preparation for addition
    adc $0d
    tay
    lda temple_of_terror_supertile_offset_tbl,y
    sta $08                                     ; set supertile data offset into level_7_supertile_data
    lda $0f
    clc                                         ; clear carry in preparation for addition
    adc $0e
    tay
    lda temple_of_terror_supertile_y_pos_tbl,y  ; load Y position of supertile to draw
    sta $0c                                     ; set Y position of supertile to draw
    lda temple_of_terror_supertile_x_pos_tbl,y  ; load x position of supertile to draw
    ldy $0c                                     ; load Y position of supertile to draw
    jsr load_banks_update_supertile_and_palette ; update nametable supertile (4x4 tile) and its palette at (a, y)
    txa
    sec                                         ; set carry flag in preparation for subtraction
    sbc #$04
    sta GRAPHICS_BUFFER_OFFSET
    tax
    lda #$ff
    sta CPU_GRAPHICS_BUFFER-1,x
    ldy $0f
    dey
    bpl @loop
    clc

@exit:
    ldx ENEMY_CURRENT_SLOT
    rts

temple_of_terror_supertile_offset_tbl:
    .byte $7f,$80,$86,$87
    .byte $a1,$a2,$a3,$a4
    .byte $a5,$a6,$a7,$a8
    .byte $a5,$a6,$a7,$a8
    .byte $73,$74,$79,$7a
    .byte $a9,$aa,$ab,$ac
    .byte $ad,$ae,$af,$b0
    .byte $ad,$ae,$af,$b0

temple_of_terror_supertile_y_pos_tbl:
    .byte $60,$60,$80,$80
    .byte $20,$20,$40,$40

temple_of_terror_supertile_x_pos_tbl:
    .byte $70,$90,$70,$90
    .byte $70,$90,$70,$90

; enemy type #$53
poison_drop_gen_routine_ptr_tbl:
    .addr poison_drop_gen_routine_00
    .addr poison_drop_gen_routine_01
    .addr poison_drop_gen_routine_02 ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr poison_drop_gen_routine_03

poison_drop_gen_routine_00:
    lda #$01
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision
    lda #$10                  ; HP = #$10, #$14, or #$17
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda ENEMY_ATTRIBUTES,x
    lsr
    lda #$c4
    bcs @continue
    lda #$3c

@continue:
    sta ENEMY_X_POS,x
    lda #$27
    sta ENEMY_Y_POS,x
    jmp set_delay_adv_enemy_routine ; set delay to #$27 and set routine to poison_drop_gen_routine_01

poison_drop_gen_routine_01:
    lda #$01
    sta ENEMY_SPRITE,x                 ; set invisible sprite
    dec ENEMY_DELAY,x
    bne @exit
    ldy #$56                           ; enemy type = poison drop
    jsr try_create_enemy_from_existing ; create poison drop
    lda #$a0
    sta ENEMY_DELAY,x

@exit:
    rts

; enemy destroyed routine
poison_drop_gen_routine_02:
    jmp enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

poison_drop_gen_routine_03:
    jmp enemy_explosion_routine_03 ; mark destroyed, remove enemy

; enemy type #$54
fire_ring_projectile_routine_ptr_tbl:
    .addr fire_ring_projectile_routine_00
    .addr fire_ring_projectile_routine_01
    .addr fire_ring_projectile_routine_02
    .addr enemy_explosion_routine_00      ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01      ; animate explosion sequence
    .addr enemy_explosion_routine_03      ; mark destroyed, remove enemy

fire_ring_projectile_routine_00:
    lda #$03
    sta ENEMY_HP,x                  ; set fire ring HP to 3
    lda #$02
    sta ENEMY_SPRITE_ATTR,x         ; set sprite palette
    lda #$28
    jmp set_delay_adv_enemy_routine ; set delay to #$28 and set routine to fire_ring_projectile_routine_01

fire_ring_projectile_routine_01:
    ldy #$11                       ; animation index for fire ring projectile
    jsr set_enemy_animation_sprite ; animate sprite for fire ring projectile based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    dec ENEMY_DELAY,x
    bne @exit                      ; exit if delay hasn't elapsed
    jsr player_enemy_x_dist        ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$07                       ; 1.87x speed
    jsr set_vel_to_target_player   ; set enemy velocity to target player index $0a at 1.87x speed
    jmp advance_enemy_routine      ; advance to next routine

@exit:
    rts

fire_ring_projectile_routine_02:
    ldy #$11                       ; animation index for fire ring projectile
    jsr set_enemy_animation_sprite ; animate sprite for fire ring projectile based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jmp apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen

temple_of_terror_red_blob_routine_ptr_tbl:
    .addr temple_of_terror_red_blob_routine_00
    .addr temple_of_terror_red_blob_routine_01
    .addr enemy_explosion_routine_00           ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01           ; animate explosion sequence
    .addr enemy_explosion_routine_03           ; mark destroyed, remove enemy

temple_of_terror_red_blob_routine_00:
    jsr player_enemy_x_dist                      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$06
    sty $06
    jsr copy_enemy_vars_to_zp                    ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_01                            ; set enemy aim direction ($0c) and rotation direction (a) targeting player $0a using quadrant_aim_dir_01
                                                 ; if no player to target, use $(0b, $0c)
    ldy ENEMY_ATTRIBUTES,x
    lda temple_of_terror_red_block_aim_dir_tbl,y
    clc                                          ; clear carry in preparation for addition
    adc $0c
    cmp #$18
    bcc @continue
    lda #$00

@continue:
    sta ENEMY_VAR_1,x                ; set aim direction (#$00 - 3 o'clock to #$0c - 9 o'clock)
    ldy #$06                         ; speed adjust code for 1.75x speed
    jsr hone_to_player_set_enemy_vel ; determine next aim dir to get closer to player index ($0a)
                                     ; and set enemy velocity to that target direction
    lda #$02
    sta ENEMY_SPRITE_ATTR,x
    lda #$20
    sta ENEMY_VAR_2,x
    lda #$2c
    jmp set_delay_adv_enemy_routine  ; set delay to #$2c and set routine to temple_of_terror_red_blob_routine_01

; unused
bank_4_unused_00:
    rts

temple_of_terror_red_block_aim_dir_tbl:
    .byte $00,$04,$fc

temple_of_terror_red_blob_routine_01:
    ldy #$12                         ; animation index for red blob
    jsr set_enemy_animation_sprite   ; animate sprite for poison drop based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda ENEMY_VAR_2,x
    beq @apply_vel_exit
    dec ENEMY_DELAY,x
    bne @apply_vel_exit
    lda #$06
    sta ENEMY_DELAY,x
    jsr player_enemy_x_dist          ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$06                         ; speed adjust code for 1.75x speed
    jsr hone_to_player_set_enemy_vel ; determine next aim dir to get closer to player index ($0a)
                                     ; and set enemy velocity to that target direction
    dec ENEMY_VAR_2,x

@apply_vel_exit:
    jmp apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

; enemy type #$56
poison_drop_routine_ptr_tbl:
    .addr poison_drop_routine_00
    .addr poison_drop_routine_01
    .addr poison_drop_routine_02
    .addr poison_drop_routine_03
    .addr poison_drop_routine_04
    .addr poison_drop_routine_05
    .addr poison_drop_routine_06
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

poison_drop_routine_00:
    jsr poison_drop_set_sprite_pos
    lda #$0c
    jmp set_delay_adv_enemy_routine ; set delay to #$0c and set routine to poison_drop_routine_01

poison_drop_routine_01:
    dec ENEMY_DELAY,x
    bne poison_drop_exit
    lda #$0c
    sta ENEMY_DELAY,x
    inc ENEMY_FRAME,x
    jsr poison_drop_set_sprite_pos
    lda ENEMY_FRAME,x
    cmp #$03
    bcc poison_drop_exit
    jmp advance_enemy_routine      ; advance to next routine

poison_drop_routine_02:
    lda #$10
    jsr add_a_to_enemy_y_fract_vel
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$4c
    bcc poison_drop_exit
    lda #$4c
    sta ENEMY_Y_POS,x
    lda #$05
    sta ENEMY_VAR_2,x
    jsr clear_enemy_y_vel           ; set enemy Y velocity to 0
    lda #$00
    sta ENEMY_FRAME,x
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and set routine to poison_drop_routine_03

poison_drop_exit:
    rts

poison_drop_routine_03:
    ldy #$13                       ; animation index for poison drop
    jsr set_enemy_animation_sprite ; animate sprite for poison drop based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda ENEMY_ATTRIBUTES,x
    and #$01
    asl
    sta $00
    lda ENEMY_X_VELOCITY_FAST,x    ; load enemy's fast X velocity
    rol
    rol
    and #$01
    clc                            ; clear carry in preparation for addition
    adc $00
    tay
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp poison_drop_turn_pos_tbl,y
    ror
    eor ENEMY_X_VELOCITY_FAST,x
    bpl @continue                  ; branch if moving left and to the right of #$60
                                   ; or moving right and to the left of #$60 to continue
    jsr flip_enemy_x_dir           ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

@continue:
    jsr apply_velocity                 ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_DELAY,x
    bne poison_drop_exit
    lda #$30
    sta ENEMY_DELAY,x
    jsr poison_drop_target_player_slow ; target player with slower X velocity (0.5)
    dec ENEMY_VAR_2,x
    bne poison_drop_exit
    jsr clear_enemy_x_vel              ; set enemy X velocity (fast and fractional) to 0
    lda #$04
    sta ENEMY_FRAME,x
    jsr poison_drop_set_sprite_96
    lda #$0c
    jmp set_delay_adv_enemy_routine    ; set delay to #$0c and set routine to poison_drop_routine_04

poison_drop_turn_pos_tbl:
    .byte $60

; !(UNUSED)
bank_4_unused_01:
    jsr test_player_fg_collision ; test player enemy (or enemy bullet) collision

poison_drop_routine_04:
    dec ENEMY_DELAY,x
    bne poison_drop_exit
    lda #$0c
    sta ENEMY_DELAY,x
    inc ENEMY_FRAME,x
    lda ENEMY_FRAME,x
    cmp #$07
    bcs @set_sprite_97
    jmp poison_drop_set_sprite_96

@set_sprite_97:
    lda #$97
    sta ENEMY_SPRITE,x        ; sprite_97
    jmp advance_enemy_routine ; advance to next routine

poison_drop_routine_05:
    lda #$10
    jsr add_a_to_enemy_y_fract_vel
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$0c                        ; add #$0c to enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    beq poison_drop_exit
    jsr set_y_pos_for_bg            ; set enemy Y position based on background collision
    jsr clear_enemy_y_vel           ; set enemy Y velocity to 0
    ldy #$02
    jsr poison_drop_target_player   ; target player with faster X velocity (1.5)
    lda #$00
    sta ENEMY_FRAME,x
    jmp advance_enemy_routine       ; advance to next routine

poison_drop_routine_06:
    ldy #$14                       ; animation index for poison drop crawl
    jsr set_enemy_animation_sprite ; animate sprite for poison drop crawl based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jmp apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen

poison_drop_set_sprite_96:
    lda #$96
    bne poison_drop_set_sprite_a_pos

poison_drop_set_sprite_pos:
    lda #$94

poison_drop_set_sprite_a_pos:
    clc                             ; clear carry in preparation for addition
    adc ENEMY_FRAME,x
    sta ENEMY_SPRITE,x
    ldy ENEMY_FRAME,x
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    clc                             ; clear carry in preparation for addition
    adc poison_drop_y_pos_adj_tbl,y
    sta ENEMY_Y_POS,x
    lda ENEMY_ATTRIBUTES,x
    lsr                             ; push direction into carry

; set sprite attribute based on direction in the carry flag
; input
;  * cary - set for moving right, clear for moving left
poison_drop_set_sprite_attr:
    lda #$03
    bcc @set ; branch if moving left
    lda #$43 ; moving right

@set:
    sta ENEMY_SPRITE_ATTR,x
    rts

poison_drop_y_pos_adj_tbl:
    .byte $00,$04,$08,$04,$04,$08,$02

; target player with slower X velocity (0.5)
poison_drop_target_player_slow:
    ldy #$00

poison_drop_target_player:
    lda poison_drop_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x  ; store enemy's fractional X velocity
    lda poison_drop_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x   ; store enemy's fast X velocity
    jsr player_enemy_x_dist       ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_X_POS,x             ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y
    bcc @continue
    jsr flip_enemy_x_dir          ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

@continue:
    lda ENEMY_X_VELOCITY_FAST,x     ; load enemy's fast X velocity
    eor #$80
    asl
    jmp poison_drop_set_sprite_attr

poison_drop_x_vel_tbl:
    .byte $80,$00 ; 0.5
    .byte $80,$01 ; 1.5

; enemy type #$51
temple_of_terror_skull_routine_ptr_tbl:
    .addr temple_of_terror_skull_routine_00 ; set collision, HP, and advance routine
    .addr temple_of_terror_skull_routine_01 ; wait for Y_SCROLL_FLAGS, play boss music, set IRQ, create core, and 2 acid drop generators, advance routine
    .addr temple_of_terror_skull_routine_02
    .addr temple_of_terror_skull_routine_03
    .addr temple_of_terror_skull_routine_04
    .addr temple_of_terror_skull_routine_05
    .addr enemy_routine_boss_defeated_00    ; enemy destroyed routine
    .addr enemy_routine_boss_defeated_01
    .addr temple_of_terror_skull_routine_08
    .addr bg_enemy_explosion_routine_01     ; animate sequence of explosions
    .addr temple_of_terror_skull_routine_09

; set collision, HP, and advance routine
temple_of_terror_skull_routine_00:
    lda #$11
    sta ENEMY_DESTROY_ATTRS,x ; set bullet collision box and disable player-enemy collision
    lda #$20                  ; HP = #$20, #$30, or #$38
    jsr set_enemy_hp_hard     ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    sta ENEMY_VAR_2,x
    lda #$f0                  ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x            ; instead ENEMY_VAR_2,x is used to track HP
    jmp advance_enemy_routine ; advance to next routine

; wait for Y_SCROLL_FLAGS, play boss music, set IRQ, create core, and 2 acid drop generators, advance routine
temple_of_terror_skull_routine_01:
    lda #$01
    sta ENEMY_SPRITE,x              ; set invisible sprite
    lda Y_SCROLL_FLAGS              ; wait for Y_SCROLL_FLAGS to change from #$80 to #$c0
    cmp #$c0
    bne temple_of_terror_skull_exit
    lda #$31                        ; scroll almost complete
                                    ; sound_31 (BOSS BGM) - Ruined Base
    jsr play_sound                  ; play boss background music
    lda #$48
    sta ENEMY_Y_POS,x
    lda #$80
    sta ENEMY_X_POS,x               ; set position to (#$48, $80)
    lda #$0a
    sta IRQ_TYPE                    ; set irq routine type to irq_handler_09_ptr_tbl
                                    ; level 7 headquarters boss (temple of terror)
    .ifdef Probotector
        lda #$46
    .else
        lda #$52
    .endif
    sta SCANLINE_IRQ_1              ; set where irq_handler_09_00 will run
                                    ; this is where the left pattern table top half tiles are updated from #$2c to #$30
    lda #$e0
    sta Y_SCROLL                    ; set PPU vertical scroll to show bottom nametable
    sta IRQ_Y_SCROLL
    lda X_SCROLL                    ; load PPU horizontal scroll
    sta IRQ_X_SCROLL
    lda PPUCTRL_SETTINGS
    sta IRQ_PPUCTRL_SETTINGS
    ldy #$02

; create core and two acid drop generators
@create_enemies:
    sty $08
    lda temple_of_terror_enemy_tbl,y   ; enemy type #$52 = temple of terror core
    tay                                ; or enemy type #$53 temple of terror poison/acid drop generator
    jsr try_create_enemy_from_existing ; create core, or poison/acid drop generator
    bcc @adv_routine                   ; branch if unable to create enemy
    ldy $11                            ; load slot of created enemy
    lda $08                            ; set attribute based on enemy index
    sta ENEMY_ATTRIBUTES,y             ; core = #$02, first acid drop generator = #$01, second = #$02
    txa
    sta ENEMY_VAR_5,y                  ; set enemy slot of skull in generated enemy's ENEMY_VAR_5
    ldy $08
    dey
    bpl @create_enemies

@adv_routine:
    jmp advance_enemy_routine ; advance to next routine

temple_of_terror_skull_exit:
    rts

; enemy type #$52 (Temple of Terror Core)
; enemy type #$53 (Temple of Terror Acid Drop Generator)
temple_of_terror_enemy_tbl:
    .byte $52,$53,$53

temple_of_terror_skull_routine_02:
    lda ENEMY_VAR_1,x
    beq temple_of_terror_skull_exit
    lda #$01
    sta ENEMY_FRAME,x
    lda #$40
    jmp set_delay_adv_enemy_routine ; set delay to #$40 and set routine to temple_of_terror_skull_routine_03

temple_of_terror_skull_routine_03:
    dec ENEMY_DELAY,x
    bne temple_of_terror_skull_exit2
    jsr template_of_terror_skull_draw_frame_supertiles
    lda #$01
    bcs temple_of_terror_skull_set_delay_exit
    lda ENEMY_FRAME,x
    cmp #$02
    lda #$08
    bcc @next_frame_exit
    dec ENEMY_FRAME,x
    lda ENEMY_VAR_2,x
    sta ENEMY_HP,x
    lda #$08
    sta ENEMY_FIRING_DELAY,x
    lda #$50
    jmp set_delay_adv_enemy_routine                    ; set delay to #$50 and set routine to temple_of_terror_skull_routine_04

@next_frame_exit:
    inc ENEMY_FRAME,x

temple_of_terror_skull_set_delay_exit:
    sta ENEMY_DELAY,x

temple_of_terror_skull_exit2:
    rts

temple_of_terror_skull_prev_frame_exit:
    dec ENEMY_FRAME,x
    jmp temple_of_terror_skull_set_delay_exit

temple_of_terror_skull_routine_04:
    dec ENEMY_FIRING_DELAY,x
    bne @check_adv_routine
    ldy #$02

@create_projectile_loop:
    sty $08
    ldy #$55                           ; enemy type = temple of terror skull projectile (poisonous insect gel)
    jsr try_create_enemy_from_existing ; create temple of terror skull projectile (poisonous insect gel)
    bcc @check_adv_routine             ; branch if unable to create temple of terror skull projectile (poisonous insect gel)
    ldy $11
    lda $08
    sta ENEMY_ATTRIBUTES,y
    ldy $08
    dey
    bpl @create_projectile_loop

@check_adv_routine:
    dec ENEMY_DELAY,x
    bne temple_of_terror_skull_exit2
    lda ENEMY_HP,x
    sta ENEMY_VAR_2,x                ; copy HP
    lda #$f0                         ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$01
    jmp set_delay_adv_enemy_routine  ; set delay to #$01 and set routine to temple_of_terror_skull_routine_05

temple_of_terror_skull_routine_05:
    dec ENEMY_DELAY,x
    bne temple_of_terror_skull_exit2
    jsr template_of_terror_skull_draw_frame_supertiles
    lda #$01
    bcs temple_of_terror_skull_set_delay_exit
    lda #$08
    ldy ENEMY_FRAME,x
    bne temple_of_terror_skull_prev_frame_exit
    inc ENEMY_FRAME,x
    lda #$80
    sta ENEMY_DELAY,x
    lda #$04
    jmp set_enemy_routine                              ; set routine to temple_of_terror_core_routine_03

temple_of_terror_skull_routine_08:
    lda #$03
    jsr template_of_terror_skull_draw_supertiles_a
    bcs temple_of_terror_skull_exit2
    jmp bg_enemy_explosion_routine_00              ; set empty sprite, play optional enemy destroyed sound, disable collisions

temple_of_terror_skull_routine_09:
    jmp set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy

template_of_terror_skull_draw_frame_supertiles:
    lda ENEMY_FRAME,x

template_of_terror_skull_draw_supertiles_a:
    clc                                  ; clear carry in preparation for addition
    adc #$04
    ldy #$04
    jmp temple_of_terror_draw_supertiles

; calculates screen index into first horizontal screen of level_xx_enemy_screen_ptr_tbl
; to do this, it converts scroll and Y_SCREEN into a 2-d array representing level layout
; input
;  * y - #$00 or #$02
; output
;  * $00 - high nibble of horizontal scroll shifted to the low nibble
;  * $01 - high nibble of vertical scroll
;  * $02
;  * $03 - row number of screen
calc_screen_row:
    sty $02
    lda X_SCROLL    ; load PPU horizontal scroll
    lsr
    lsr
    lsr
    lsr
    sta $00         ; set high nibble of X_SCROLL to $00's low nibble
    ldy #$00
    lda Y_SCROLL    ; load PPU vertical scroll
    and #$f0
    sta $01         ; set high nibble of Y_SCROLL to $01
    bne @calc_index ; branch if scrolled close to top of nametable
    ldy $02         ; not near top of nametable, don't change

@calc_index:
    sty $02           ; set high nibble of Y_SCROLL or $02
    lda #$00
    ldy Y_SCREEN      ; load current vertical screen index (similar to screen Y position on 2-d cardinal plane)
    beq @add_x_amount ; branch if at top of level (more screens above)

; not at the top of the level, add LEVEL_WIDTH for each screens above current screen
; calculating index into level_xx_enemy_screen_ptr_tbl
@loop:
    clc             ; clear carry in preparation for addition
    adc LEVEL_WIDTH
    dey
    bne @loop

@add_x_amount:
    clc          ; clear carry in preparation for addition
    adc X_SCREEN ; add number of horizontal screens scrolled
    sta $03      ; set index into level_xx_enemy_screen_ptr_tbl

calc_screen_row_exit:
    rts

; updates tiles and sets screen enemies
create_screen_enemies:
    lda BOSS_DEFEATED_FLAGS
    bne calc_screen_row_exit ; exit if playing end of level music or boss is already defeated
    lda SCREEN_SCROLL_TYPE   ; 0 = horizontal, 1 = vertical/overhead
    bne @y_scroll            ; branch if vertical scroll
    jsr @horizontal_level    ; horizontal scroll level, e.g. level 1

@y_scroll:
    lda LEVEL_Y_SCROLL_FLAGS
    cmp #$c0
    beq calc_screen_row_exit
    ldy #$00
    jsr calc_screen_row
    lda Y_SCROLL_SPEED       ; load how many pixels vertically to scroll this frame
    beq calc_screen_row_exit ; exit if no vertical scroll change this frame
    asl
    lda #$07
    bcs @add_y_scroll        ; branch if scrolling up (Y_SCROLL_SPEED is negative)
    lda #$0a                 ; scrolling down vertically

@add_y_scroll:
    sta $04
    lda Y_SCROLL               ; load PPU vertical scroll
    and #$0f
    cmp $04                    ; compare to either #$07 (scrolling up) or #$0a (scrolling down)
    ror
    eor Y_SCROLL_SPEED
    bpl calc_screen_row_exit
    lda $01                    ; load high nibble of vertical scroll
    cmp SCREEN_ENEMY_Y_CHECK
    beq calc_screen_row_exit
    sta SCREEN_ENEMY_Y_CHECK   ; set high nibble vertical scroll level enemy generation checkpoint
    lda Y_SCROLL_SPEED
    bmi @create_screen_enemies
    lda #$e0
    clc                        ; clear carry in preparation for addition
    adc $01
    bcs @calc_screen_number
    cmp #$f0
    bcc @set_enemy_gen_point

@calc_screen_number:
    adc #$0f
    tay             ; backup Y scroll position
    lda $03
    clc             ; clear carry in preparation for addition
    adc LEVEL_WIDTH
    sta $03         ; set screen number
    tya             ; restore Y scroll position

@set_enemy_gen_point:
    sta $01 ; set Y scroll position high nibble

; input
;  * $02
;  * $03 - screen offset
@create_screen_enemies:
    jsr get_enemy_screen_data ; initialize ($08) to the level-specific screen-specific $03 enemy data, e.g. level_4_enemy_screen_01
    ldy #$00                  ; initialize level_x_enemy_screen_xx read offset

; vertical level
@create_lvl_enemy_loop:
    lda ($0a),y        ; read level screen enemy byte ((X,Y) position)
    cmp #$ff           ; see if end of data marker
    beq @next_screen2  ; branch if data is #$ff, nothing to do
    and #$f0           ; not end of data, look at Y position where enemy is generated
    cmp $01            ; compare to Y scroll position high nibble
    beq @continue_loop ; branch to check X position
    bcs @next_screen   ; past the enemy generation check point
    bne @next_enemy

@continue_loop:
    lda ($0a),y                  ; re-load (X,Y) position
    and #$0f                     ; strip to just X position
    cmp $00                      ; compare to horizontal scroll shifted to high nibble
                                 ; (set in calc_screen_row)
    rol
    eor $02
    lsr
    bcc @next_enemy
    jsr create_lvl_defined_enemy ; create enemy from level_x_enemy_screen_xx ($0a),y
    bcs @create_lvl_enemy_loop

@next_enemy:
    iny
    iny
    iny
    bne @create_lvl_enemy_loop

@next_screen:
    lda $02
    beq @next_screen2

@exit:
    rts

; goes through and creates enemies for both screens
; when appropriate based on player position
@next_screen2:
    lda $02                    ; load counter of screens checked
    bne @exit                  ; exit if both screens already checked
    inc $02                    ; increment screens performed
    inc $03                    ; move to next screen
    jmp @create_screen_enemies

@horizontal_level:
    lda STOMP_CEILING_X_SCROLL_CTRL
    bne @exit
    ldy #$02
    jsr calc_screen_row             ; determine which level row the screen is on
    lda X_SCROLL                    ; load PPU horizontal scroll
    and #$08
    cmp SCREEN_ENEMY_X_CHECK
    beq @exit
    sta SCREEN_ENEMY_X_CHECK        ; set horizontal scroll level enemy generation checkpoint
    and #$08
    beq @exit
    inc $03                         ; increment screen for loading enemy data

@check_screen_enemies_for_pos:
    jsr get_enemy_screen_data ; initialize ($08) to the level-specific screen-specific $03 enemy data, e.g. level_4_enemy_screen_01
    ldy #$00

; horizontal level
@check_screen_enemies:
    lda ($0a),y                  ; load enemy screen data byte 0 ((x,y) position)
    cmp #$ff                     ; see if no more enemy data
    beq @next_row                ; branch if at end of enemy screen data
    cmp $01                      ; compare to high nibble of Y_SCROLL
    rol
    eor $02                      ; set to #$01 when player below Y value and on first screen
                                 ; or set to #$01 when player above Y value and on second screen
    lsr                          ; shift back, setting bit 7 to 0
    bcc @check_next_enemy
    lda ($0a),y                  ; re-load first byte of screen enemy data (enemy (x,y) position)
    and #$0f                     ; strip to just the X position
    cmp $00                      ; compare to high nibble of X_SCROLL
    bne @next_enemy2
    jsr create_lvl_defined_enemy ; create enemy from level_x_enemy_screen_xx ($0a),y
    jmp @check_screen_enemies

@next_enemy2:
    iny
    iny
    iny
    bne @check_screen_enemies ; always branch

@check_next_enemy:
    lda $02
    beq @next_enemy2

@exit2:
    rts

@next_row:
    lda $02
    bne @exit2
    inc $02
    lda $03                           ; load current screen
    clc                               ; clear carry in preparation for addition
    adc LEVEL_WIDTH                   ; add the number of horizontal screens the level has
    sta $03                           ; move to next vertical level
    jmp @check_screen_enemies_for_pos

; initializes ($08) to the level-specific screen-specific enemy data, e.g. level_4_enemy_screen_01
; input
;  * $03 - screen offset
; output
;  * ($0a) - two byte address to level-specific enemy screen data, e.g. level_4_enemy_screen_01
get_enemy_screen_data:
    lda CURRENT_LEVEL                      ; load current level
    asl                                    ; double since each entry is a #$02 byte memory address
    tay                                    ; transfer to offset register
    lda level_enemy_screen_ptr_ptr_tbl,y   ; load low byte of level_x_enemy_screen_ptr_tbl address
    sta $08                                ; store low byte of address
    lda level_enemy_screen_ptr_ptr_tbl+1,y ; load high byte of level_x_enemy_screen_ptr_tbl address
    sta $09                                ; store high byte of address
    lda #$00
    sta $0a
    lda $03                                ; doubling $03 with overflow going to $0a
    asl
    rol $0a
    clc                                    ; clear carry in preparation for addition
    adc $08                                ; adding 2 * $03 to low byte of level_x_enemy_screen_ptr_tbl address
    sta $08                                ; store result in $08
    lda $0a                                ; handling overflow from adding (2 * $03) + $08
    adc $09                                ; add any overflow to high byte of level_x_enemy_screen_ptr_tbl address
    sta $09                                ; store new high byte
    ldy #$00
    lda ($08),y                            ; read low byte of address within level-specific screen enemy data
    sta $0a
    iny
    lda ($08),y                            ; read high byte of address within level-specific screen enemy data
    sta $0b
    rts

; creates an enemy for the level screen as defined in level_x_enemy_screen_xx
; input
;  * $0a - 2 byte address to level_x_enemy_screen_xx
;  * y - offset from ($0a) level_x_enemy_screen_xx
create_lvl_defined_enemy:
    lda ($0a),y                   ; load enemy (x,y) position
    sta $0c                       ; set enemy (x,y) position in $0c
    iny                           ; increment read offset
    lda ($0a),y
    sta $0d                       ; store enemy attributes, and enemy slot overwrite value
                                  ; bit 7 = overwrite bullet, bit 6 = overwrite slot 0
                                  ; bits [0-6] contain initial ENEMY_ATTRIBUTES
    iny                           ; increment read offset
    lda ($0a),y                   ; load enemy type
    sta $08                       ; set actual enemy type
    iny                           ; increment read offset
    sty $07                       ; store read offset in $07
    jsr create_enemy_y            ; create enemy of type y in first available slot
                                  ; !(HUH) frequently y is not the correct enemy type
                                  ; the correct value is set down below with set_enemy_collision_and_type
    beq @enemy_created            ; exit if enemy successfully created
    lda $0d                       ; load whether or not to overwrite an in use bullet slot
    bpl @exit                     ; don't overwrite any bullets if bit 7 is set
    jsr replace_bullet_with_enemy ; try to replace an enemy bullet with the enemy specified in y
    beq @enemy_created            ; branch if bullet was replaced with newly created enemy
    bit $0d                       ; no bullet to replace, continue
    bvc @exit                     ; exit if bit 6 is clear indicating to not overwrite slot #$00
    ldx #$00
    jsr initialize_enemy          ; create enemy of type y at slot x

@enemy_created:
    lda $0d                          ; load enemy slot overwrite byte
    and #$7f                         ; strip bit 7
    sta ENEMY_ATTRIBUTES,x
    ldy $08                          ; re-load enemy type
    jsr set_enemy_collision_and_type ; set the enemy attributes, collision index, and enemy type
                                     ; this overwrites whatever was set in create_enemy_y with the correct value !(HUH)
    ldy $07
    lda $0c                          ; load enemy (x,y) position
    asl
    asl
    asl
    asl                              ; move X position into high nibble
    sec                              ; set carry flag in preparation for subtraction
    sbc X_SCROLL                     ; subtract the horizontal pixels already scrolled into screen
    clc                              ; clear carry in preparation for addition
    adc #$08                         ; add #$08 to result
    sta ENEMY_X_POS,x                ; set enemy's X position
    lda $0c                          ; load enemy (x,y) position
    and #$f0                         ; strip to just high nibble (Y position)
    ora #$08                         ; add #$08 to the Y position
    sec                              ; set carry flag in preparation for subtraction
    sbc Y_SCROLL                     ; subtract the vertical pixels already scrolled into screen
    bcs @set_y_pos_exit              ; branch if result didn't wrap around
    sbc #$0f                         ; result wrapped around, just use top of screen

@set_y_pos_exit:
    sta ENEMY_Y_POS,x ; set enemy Y position
    sec
    rts

@exit:
    clc
    rts

; if level 1 (fort firestorm), create the intro helicopter enemy at (#$60,#$1c)
; otherwise do nothing
lvl_1_create_intro_heli:
    lda CURRENT_LEVEL                ; load current level
    bne @exit                        ; exit if not level 1 (fort firestorm)
    jsr create_enemy_y               ; level 1, create enemy of type y in first available slot
                                     ; ENEMY_TYPE will be rewritten later, y in this case doesn't matter
    bne @exit                        ; branch if unable to create the enemy
    ldy #$62                         ; enemy type #$62 (intro helicopter)
    jsr set_enemy_collision_and_type ; set the collision box type, and enemy type based on y
    lda #$60
    sta ENEMY_X_POS,x                ; set enemy X position
    lda #$1c
    sta ENEMY_Y_POS,x                ; set enemy Y position

@exit:
    rts

; byte 0
;  * high nibble: value plus #$08 is assigned to ENEMY_Y_POS, e.g. #$8d -> #$88
;  * low nibble: value shifted to high nibble plus #$08 is assigned to ENEMY_X_POS, e.g. #$8d -> #$d8
; byte 1
;  * bit 7: whether or not to overwrite an enemy bullet if enemy slots are full
;  * bit 6: whether or not to overwrite enemy slot 0
;  * bits 0-5: enemy attributes
; byte 2 - enemy type
level_1_enemy_screen_ptr_tbl:
    .addr level_1_enemy_screen_no_enemies
    .addr level_1_enemy_screen_no_enemies+1
    .addr level_1_enemy_screen_no_enemies+2
    .addr level_1_enemy_screen_no_enemies+3
    .addr level_1_enemy_screen_no_enemies+4
    .addr level_1_enemy_screen_no_enemies+5
    .addr level_1_enemy_screen_no_enemies+6
    .addr level_1_enemy_screen_no_enemies+7
    .addr level_1_enemy_screen_no_enemies+8
    .addr level_1_enemy_screen_no_enemies+9
    .addr level_1_enemy_screen_no_enemies+10
    .addr level_1_enemy_screen_no_enemies+11
    .addr level_1_enemy_screen_no_enemies+12
    .addr level_1_enemy_screen_no_enemies+13
    .addr level_1_enemy_screen_no_enemies+14
    .addr level_1_enemy_screen_no_enemies+15
    .addr level_1_enemy_screen_no_enemies+16
    .addr level_1_enemy_screen_no_enemies+17
    .addr level_1_enemy_screen_no_enemies+18
    .addr level_1_enemy_screen_no_enemies+19
    .addr level_1_enemy_screen_no_enemies+20
    .addr level_1_enemy_screen_no_enemies+21
    .addr level_1_enemy_screen_16
    .addr level_1_enemy_screen_17
    .addr level_1_enemy_screen_18
    .addr level_1_enemy_screen_19
    .addr level_1_enemy_screen_1a
    .addr level_1_enemy_screen_1b
    .addr level_1_enemy_screen_1b+1
    .addr level_1_enemy_screen_1b+2
    .addr level_1_enemy_screen_1b+3
    .addr level_1_enemy_screen_1b+4
    .addr level_1_enemy_screen_20
    .addr level_1_enemy_screen_21
    .addr level_1_enemy_screen_22
    .addr level_1_enemy_screen_23
    .addr level_1_enemy_screen_24
    .addr level_1_enemy_screen_25
    .addr level_1_enemy_screen_26
    .addr level_1_enemy_screen_27
    .addr level_1_enemy_screen_27+1
    .addr level_1_enemy_screen_27+2
    .addr level_1_enemy_screen_27+3
    .addr level_1_enemy_screen_2b
    .addr level_1_enemy_screen_2c
    .addr level_1_enemy_screen_2d
    .addr level_1_enemy_screen_2e
    .addr level_1_enemy_screen_2f
    .addr level_1_enemy_screen_30
    .addr level_1_enemy_screen_30+1
    .addr level_1_enemy_screen_30+2
    .addr level_1_enemy_screen_30+3
    .addr level_1_enemy_screen_30+4
    .addr level_1_enemy_screen_30+5
    .addr level_1_enemy_screen_30+6
    .addr level_1_enemy_screen_30+7

level_1_enemy_screen_no_enemies:
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $ff,$ff,$ff,$ff,$ff,$ff

level_1_enemy_screen_16:
    .byte $8b,$80,$6f ; enemy type #$6f (background storm), attrs: #$00, pos: (#$b8,#$88), can overwrite bullet
    .byte $ff

level_1_enemy_screen_17:
    .byte $97,$91,$01 ; enemy type #$01 (flying capsule), attrs: #$11, pos: (#$78,#$98), can overwrite bullet
    .byte $a8,$83,$09 ; enemy type #$09 (sniper), attrs: #$03, pos: (#$88,#$a8), can overwrite bullet
    .byte $ad,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$d8,#$a8), can overwrite bullet
    .byte $e6,$90,$01 ; enemy type #$01 (flying capsule), attrs: #$10, pos: (#$68,#$e8), can overwrite bullet
    .byte $ff

level_1_enemy_screen_18:
    .byte $ac,$a1,$0f ; enemy type #$0f (grenade generator), attrs: #$21, pos: (#$c8,#$a8), can overwrite bullet
    .byte $ff

level_1_enemy_screen_19:
    .byte $39,$80,$10 ; enemy type #$10 (soldier generator), attrs: #$00, pos: (#$98,#$38), can overwrite bullet
    .byte $8f,$80,$20 ; enemy type #$20 (wall cannon), attrs: #$00, pos: (#$f8,#$88), can overwrite bullet
    .byte $ff

level_1_enemy_screen_1a:
    .byte $b5,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$58,#$b8), can overwrite bullet
    .byte $81,$c0,$21 ; enemy type #$21 (helicopter core), attrs: #$40, pos: (#$18,#$88), can overwrite bullet, can overwrite slot 0
    .byte $ff

level_1_enemy_screen_1b:
    .byte $ff,$ff,$ff,$ff,$ff

level_1_enemy_screen_20:
    .byte $ab,$80,$10 ; enemy type #$10 (soldier generator), attrs: #$00, pos: (#$b8,#$a8), can overwrite bullet
    .byte $9e,$96,$01 ; enemy type #$01 (flying capsule), attrs: #$16, pos: (#$e8,#$98), can overwrite bullet
    .byte $ff

level_1_enemy_screen_21:
    .byte $98,$80,$6f ; enemy type #$6f (background storm), attrs: #$00, pos: (#$88,#$98), can overwrite bullet
    .byte $ff

level_1_enemy_screen_22:
    .byte $36,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$68,#$38), can overwrite bullet
    .byte $3b,$85,$05 ; enemy type #$05 (grenade thrower), attrs: #$05, pos: (#$b8,#$38), can overwrite bullet
    .byte $ff

level_1_enemy_screen_23:
    .byte $05,$96,$01 ; enemy type #$01 (flying capsule), attrs: #$16, pos: (#$58,#$08), can overwrite bullet
    .byte $24,$93,$01 ; enemy type #$01 (flying capsule), attrs: #$13, pos: (#$48,#$28), can overwrite bullet
    .byte $62,$80,$0f ; enemy type #$0f (grenade generator), attrs: #$00, pos: (#$28,#$68), can overwrite bullet
    .byte $ff

level_1_enemy_screen_24:
    .byte $11,$a2,$0f ; enemy type #$0f (grenade generator), attrs: #$22, pos: (#$18,#$18), can overwrite bullet
    .byte $ff

level_1_enemy_screen_25:
    .byte $09,$81,$20 ; enemy type #$20 (wall cannon), attrs: #$01, pos: (#$98,#$08), can overwrite bullet
    .byte $ff

level_1_enemy_screen_26:
    .byte $01,$80,$20 ; enemy type #$20 (wall cannon), attrs: #$00, pos: (#$18,#$08), can overwrite bullet
    .byte $ff

level_1_enemy_screen_27:
    .byte $ff,$ff,$ff,$ff

; last byte is the enemy type
level_1_enemy_screen_2b:
    .byte $35,$90,$01 ; enemy type #$01 (flying capsule), attrs: #$10, pos: (#$58,#$38), can overwrite bullet
    .byte $4b,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$b8,#$48), can overwrite bullet
    .byte $93,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$38,#$98)
    .byte $95,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$58,#$98)
    .byte $95,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$58,#$98)
    .byte $97,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$78,#$98)
    .byte $99,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$98,#$98)
    .byte $9a,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$a8,#$98)
    .byte $9c,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$c8,#$98)
    .byte $ff

level_1_enemy_screen_2c:
    .byte $56,$94,$01 ; enemy type #$01 (flying capsule), attrs: #$14, pos: (#$68,#$58), can overwrite bullet
    .byte $4b,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$b8,#$48), can overwrite bullet
    .byte $56,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$68,#$58), can overwrite bullet
    .byte $ff

level_1_enemy_screen_2d:
    .byte $1b,$80,$6f ; enemy type #$6f (background storm), attrs: #$00, pos: (#$b8,#$18), can overwrite bullet
    .byte $26,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$68,#$28), can overwrite bullet
    .byte $ff

level_1_enemy_screen_2e:
    .byte $ff

level_1_enemy_screen_2f:
    .byte $11,$85,$07 ; enemy type #$07 (sandbag sniper), attrs: #$05, pos: (#$18,#$18), can overwrite bullet
    .byte $ff

level_1_enemy_screen_30:
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

level_2_enemy_screen_ptr_tbl:
    .ifdef Probotector
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_02
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_02
        .addr level_2_enemy_screen_02
        .addr level_2_enemy_screen_04
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_04
        .addr level_2_enemy_screen_04
        .addr level_2_enemy_screen_06
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_06
        .addr level_2_enemy_screen_06
        .addr level_2_enemy_screen_08
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_08
        .addr level_2_enemy_screen_08
        .addr level_2_enemy_screen_0a
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_0c
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_0e
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_10
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_12
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_14
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_16
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_18
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_1a
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
    .else
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_02
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_04
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_06
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_08
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_0a
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_0c
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_0e
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_10
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_12
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_14
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_16
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_18
        .addr level_2_enemy_screen_no_enemies
        .addr level_2_enemy_screen_1a
        .addr level_2_enemy_screen_no_enemies
    .endif

level_2_enemy_screen_no_enemies:
    .byte $ff

level_2_enemy_screen_02:
    .byte $40,$c0,$69 ; enemy type #$69 (tank boss), attrs: #$40, pos: (#$08,#$48), can overwrite bullet, can overwrite slot 0
    .byte $ff

level_2_enemy_screen_04:
    .byte $37,$c0,$0d ; enemy type #$0d (door), attrs: #$40, pos: (#$78,#$38), can overwrite bullet, can overwrite slot 0
    .byte $58,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$88,#$58), can overwrite bullet
    .byte $83,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$38,#$88), can overwrite bullet
    .byte $8c,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$c8,#$88), can overwrite bullet
    .byte $ff

level_2_enemy_screen_06:
    .byte $1d,$80,$5b ; enemy type #$5b (rotating turret), attrs: #$00, pos: (#$d8,#$18), can overwrite bullet
    .byte $11,$81,$5b ; enemy type #$5b (rotating turret), attrs: #$01, pos: (#$18,#$18), can overwrite bullet
    .byte $d5,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$58,#$d8), can overwrite bullet
    .byte $da,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$a8,#$d8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_08:
    .byte $47,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$78,#$48), can overwrite bullet
    .byte $b5,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$58,#$b8), can overwrite bullet
    .byte $ba,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$a8,#$b8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_0a:
    .byte $65,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$58,#$68), can overwrite bullet
    .byte $6a,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$a8,#$68), can overwrite bullet
    .byte $78,$b6,$01 ; enemy type #$01 (flying capsule), attrs: #$36, pos: (#$88,#$78), can overwrite bullet
    .byte $c1,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$18,#$c8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_0c:
    .byte $15,$80,$5b ; enemy type #$5b (rotating turret), attrs: #$00, pos: (#$58,#$18), can overwrite bullet
    .byte $91,$81,$5b ; enemy type #$5b (rotating turret), attrs: #$01, pos: (#$18,#$98), can overwrite bullet
    .byte $ff

level_2_enemy_screen_0e:
    .byte $17,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$78,#$18), can overwrite bullet
    .byte $18,$b2,$01 ; enemy type #$01 (flying capsule), attrs: #$32, pos: (#$88,#$18), can overwrite bullet
    .byte $38,$81,$01 ; enemy type #$01 (flying capsule), attrs: #$01, pos: (#$88,#$38), can overwrite bullet
    .byte $a5,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$58,#$a8), can overwrite bullet
    .byte $aa,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$a8,#$a8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_10:
    .byte $65,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$58,#$68), can overwrite bullet
    .byte $6a,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$a8,#$68), can overwrite bullet
    .byte $b3,$80,$2e ; enemy type #$2e (overhead tank soldier), attrs: #$00, pos: (#$38,#$b8), can overwrite bullet
    .byte $bb,$80,$2e ; enemy type #$2e (overhead tank soldier), attrs: #$00, pos: (#$b8,#$b8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_12:
    .byte $b7,$80,$2e ; enemy type #$2e (overhead tank soldier), attrs: #$00, pos: (#$78,#$b8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_14:
    .byte $18,$b1,$01 ; enemy type #$01 (flying capsule), attrs: #$31, pos: (#$88,#$18), can overwrite bullet
    .byte $38,$83,$01 ; enemy type #$01 (flying capsule), attrs: #$03, pos: (#$88,#$38), can overwrite bullet
    .byte $ff

level_2_enemy_screen_16:
    .byte $8d,$80,$5c ; enemy type #$5c (red soldier), attrs: #$00, pos: (#$d8,#$88), can overwrite bullet
    .byte $b8,$b0,$01 ; enemy type #$01 (flying capsule), attrs: #$30, pos: (#$88,#$b8), can overwrite bullet
    .byte $ff

level_2_enemy_screen_18:
    .byte $ff

level_2_enemy_screen_1a:
    .byte $ff

level_3_enemy_screen_ptr_tbl:
    .addr level_3_enemy_screen_00
    .addr level_3_enemy_screen_01
    .addr level_3_enemy_screen_04
    .addr level_3_enemy_screen_05
    .addr level_3_enemy_screen_06
    .addr level_3_enemy_screen_07
    .addr level_3_enemy_screen_08
    .addr level_3_enemy_screen_09
    .addr level_3_enemy_screen_0a
    .addr level_3_enemy_screen_0b
    .addr level_3_enemy_screen_0c
    .addr level_3_enemy_screen_0d
    .addr level_3_enemy_screen_0e
    .addr level_3_enemy_screen_0f
    .addr level_3_enemy_screen_10
    .addr level_3_enemy_screen_11
    .addr level_3_enemy_screen_12
    .addr level_3_enemy_screen_13
    .addr level_3_enemy_screen_14
    .addr level_3_enemy_screen_15
    .addr level_3_enemy_screen_16
    .addr level_3_enemy_screen_17

level_3_enemy_screen_00:
    .byte $ff

level_3_enemy_screen_01:
    .byte $9e,$80,$26 ; enemy type #$26 (hiding sniper), attrs: #$00, pos: (#$e8,#$98), can overwrite bullet
    .byte $b7,$83,$27 ; enemy type #$27 (raised grass-covered turret), attrs: #$03, pos: (#$78,#$b8), can overwrite bullet
    .byte $ff

level_3_enemy_screen_04:
    .byte $97,$80,$26 ; enemy type #$26 (hiding sniper), attrs: #$00, pos: (#$78,#$98), can overwrite bullet
    .byte $3b,$a2,$09 ; enemy type #$09 (sniper), attrs: #$22, pos: (#$b8,#$38), can overwrite bullet
    .byte $3b,$96,$01 ; enemy type #$01 (flying capsule), attrs: #$16, pos: (#$b8,#$38), can overwrite bullet
    .byte $8b,$94,$01 ; enemy type #$01 (flying capsule), attrs: #$14, pos: (#$b8,#$88), can overwrite bullet
    .byte $ff

level_3_enemy_screen_05:
    .byte $1b,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$b8,#$18)
    .byte $1b,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$b8,#$18)
    .byte $37,$a2,$09 ; enemy type #$09 (sniper), attrs: #$22, pos: (#$78,#$38), can overwrite bullet
    .byte $b3,$83,$27 ; enemy type #$27 (raised grass-covered turret), attrs: #$03, pos: (#$38,#$b8), can overwrite bullet
    .byte $bb,$83,$27 ; enemy type #$27 (raised grass-covered turret), attrs: #$03, pos: (#$b8,#$b8), can overwrite bullet
    .byte $ff

level_3_enemy_screen_06:
    .byte $17,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$78,#$18)
    .byte $ff

level_3_enemy_screen_07:
    .byte $42,$10,$01 ; enemy type #$01 (flying capsule), attrs: #$10, pos: (#$28,#$48)
    .byte $8a,$00,$28 ; enemy type #$28 (cannon turret), attrs: #$00, pos: (#$a8,#$88)
    .byte $ff

level_3_enemy_screen_08:
    .byte $14,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$48,#$18)
    .byte $16,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$68,#$18)
    .byte $39,$a1,$09 ; enemy type #$09 (sniper), attrs: #$21, pos: (#$98,#$38), can overwrite bullet
    .byte $3c,$a1,$09 ; enemy type #$09 (sniper), attrs: #$21, pos: (#$c8,#$38), can overwrite bullet
    .byte $3e,$a1,$09 ; enemy type #$09 (sniper), attrs: #$21, pos: (#$e8,#$38), can overwrite bullet
    .byte $40,$92,$01 ; enemy type #$01 (flying capsule), attrs: #$12, pos: (#$08,#$48), can overwrite bullet
    .byte $a6,$85,$07 ; enemy type #$07 (sandbag sniper), attrs: #$05, pos: (#$68,#$a8), can overwrite bullet
    .byte $ff

level_3_enemy_screen_09:
    .byte $34,$a0,$10 ; enemy type #$10 (soldier generator), attrs: #$20, pos: (#$48,#$38), can overwrite bullet
    .byte $ff

level_3_enemy_screen_0a:
    .byte $37,$a2,$09 ; enemy type #$09 (sniper), attrs: #$22, pos: (#$78,#$38), can overwrite bullet
    .byte $3b,$a2,$09 ; enemy type #$09 (sniper), attrs: #$22, pos: (#$b8,#$38), can overwrite bullet
    .byte $84,$00,$26 ; enemy type #$26 (hiding sniper), attrs: #$00, pos: (#$48,#$88)
    .byte $8a,$01,$26 ; enemy type #$26 (hiding sniper), attrs: #$01, pos: (#$a8,#$88)
    .byte $ff

level_3_enemy_screen_0b:
    .byte $33,$a2,$09 ; enemy type #$09 (sniper), attrs: #$22, pos: (#$38,#$38), can overwrite bullet
    .byte $ff

level_3_enemy_screen_0c:
    .byte $17,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$78,#$18)
    .byte $17,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$78,#$18)
    .byte $19,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$98,#$18)
    .byte $19,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$98,#$18)
    .byte $1b,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$b8,#$18)
    .byte $1b,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$b8,#$18)
    .byte $ff

level_3_enemy_screen_0d:
    .byte $33,$a2,$09 ; enemy type #$09 (sniper), attrs: #$22, pos: (#$38,#$38), can overwrite bullet
    .byte $3b,$a1,$09 ; enemy type #$09 (sniper), attrs: #$21, pos: (#$b8,#$38), can overwrite bullet
    .byte $a8,$85,$07 ; enemy type #$07 (sandbag sniper), attrs: #$05, pos: (#$88,#$a8), can overwrite bullet
    .byte $ff

level_3_enemy_screen_0e:
    .byte $12,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$28,#$18)
    .byte $12,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$28,#$18)
    .byte $14,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$48,#$18)
    .byte $14,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$48,#$18)
    .byte $16,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$68,#$18)
    .byte $16,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$68,#$18)
    .byte $33,$a3,$09 ; enemy type #$09 (sniper), attrs: #$23, pos: (#$38,#$38), can overwrite bullet
    .byte $ff

level_3_enemy_screen_0f:
    .byte $34,$a0,$10 ; enemy type #$10 (soldier generator), attrs: #$20, pos: (#$48,#$38), can overwrite bullet
    .byte $ff

level_3_enemy_screen_10:
    .byte $98,$c0,$4e ; enemy type #$4e (robot spider), attrs: #$40, pos: (#$88,#$98)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $ff

level_3_enemy_screen_11:
    .byte $48,$c0,$3f ; enemy type #$3f (earthquake), attrs: #$40, pos: (#$88,#$48)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $5e,$15,$01 ; enemy type #$01 (flying capsule), attrs: #$15, pos: (#$e8,#$58)
    .byte $ff

level_3_enemy_screen_12:
    .byte $ff

level_3_enemy_screen_13:
    .byte $45,$11,$01 ; enemy type #$01 (flying capsule), attrs: #$11, pos: (#$58,#$48)
    .byte $75,$13,$01 ; enemy type #$01 (flying capsule), attrs: #$13, pos: (#$58,#$78)
    .byte $ff

level_3_enemy_screen_14:
    .byte $ff

level_3_enemy_screen_15:
    .byte $8e,$c0,$4b ; enemy type #$4b (fortress wall core), attrs: #$40, pos: (#$e8,#$88)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $ff

level_3_enemy_screen_16:
    .byte $ff

level_3_enemy_screen_17:
    .byte $ff

level_4_enemy_screen_ptr_tbl:
    .addr level_4_enemy_screen_00
    .addr level_4_enemy_screen_01
    .addr level_4_enemy_screen_02
    .addr level_4_enemy_screen_03
    .addr level_4_enemy_screen_04
    .addr level_4_enemy_screen_05
    .addr level_4_enemy_screen_06
    .addr level_4_enemy_screen_07
    .addr level_4_enemy_screen_08
    .addr level_4_enemy_screen_09
    .addr level_4_enemy_screen_0a
    .addr level_4_enemy_screen_0b
    .addr level_4_enemy_screen_0c
    .addr level_4_enemy_screen_0d
    .addr level_4_enemy_screen_0e
    .addr level_4_enemy_screen_0f
    .addr level_4_enemy_screen_10
    .addr level_4_enemy_screen_11
    .addr level_4_enemy_screen_12
    .addr level_4_enemy_screen_13
    .addr level_4_enemy_screen_14
    .addr level_4_enemy_screen_15
    .addr level_4_enemy_screen_16
    .addr level_4_enemy_screen_17
    .addr level_4_enemy_screen_18
    .addr level_4_enemy_screen_19
    .addr level_4_enemy_screen_1a
    .addr level_4_enemy_screen_1b
    .addr level_4_enemy_screen_1c
    .addr level_4_enemy_screen_1d
    .addr level_4_enemy_screen_1e
    .addr level_4_enemy_screen_1f
    .addr level_4_enemy_screen_20
    .addr level_4_enemy_screen_21
    .addr level_4_enemy_screen_22
    .addr level_4_enemy_screen_23
    .addr level_4_enemy_screen_24
    .addr level_4_enemy_screen_25
    .addr level_4_enemy_screen_26
    .addr level_4_enemy_screen_27
    .addr level_4_enemy_screen_28
    .addr level_4_enemy_screen_29
    .addr level_4_enemy_screen_2a
    .addr level_4_enemy_screen_2b
    .addr level_4_enemy_screen_2c
    .addr level_4_enemy_screen_2d
    .addr level_4_enemy_screen_2e
    .addr level_4_enemy_screen_2f
    .addr level_4_enemy_screen_30
    .addr level_4_enemy_screen_31
    .addr level_4_enemy_screen_32
    .addr level_4_enemy_screen_33
    .addr level_4_enemy_screen_34
    .addr level_4_enemy_screen_35
    .addr level_4_enemy_screen_36
    .addr level_4_enemy_screen_37
    .addr level_4_enemy_screen_38
    .addr level_4_enemy_screen_39
    .addr level_4_enemy_screen_3a
    .addr level_4_enemy_screen_3b
    .addr level_4_enemy_screen_3c
    .addr level_4_enemy_screen_3d
    .addr level_4_enemy_screen_3e
    .addr level_4_enemy_screen_3f
    .addr level_4_enemy_screen_40
    .addr level_4_enemy_screen_41
    .addr level_4_enemy_screen_42
    .addr level_4_enemy_screen_43
    .addr level_4_enemy_screen_44
    .addr level_4_enemy_screen_45
    .addr level_4_enemy_screen_46
    .addr level_4_enemy_screen_47
    .addr level_4_enemy_screen_48
    .addr level_4_enemy_screen_49
    .addr level_4_enemy_screen_4a
    .addr level_4_enemy_screen_4b
    .addr level_4_enemy_screen_4c
    .addr level_4_enemy_screen_4d
    .addr level_4_enemy_screen_4e
    .addr level_4_enemy_screen_4f
    .addr level_4_enemy_screen_50
    .addr level_4_enemy_screen_51
    .addr level_4_enemy_screen_52
    .addr level_4_enemy_screen_53
    .addr level_4_enemy_screen_54
    .addr level_4_enemy_screen_55
    .addr level_4_enemy_screen_56
    .addr level_4_enemy_screen_57
    .addr level_4_enemy_screen_58
    .addr level_4_enemy_screen_59
    .addr level_4_enemy_screen_5a

level_4_enemy_screen_00:
    .byte $ff

level_4_enemy_screen_01:
    .byte $ff

level_4_enemy_screen_02:
    .byte $ff

level_4_enemy_screen_03:
    .byte $ff

level_4_enemy_screen_04:
    .byte $ff

level_4_enemy_screen_05:
    .byte $ff

level_4_enemy_screen_06:
    .byte $ff

level_4_enemy_screen_07:
    .byte $ff

level_4_enemy_screen_08:
    .byte $ff

level_4_enemy_screen_09:
    .byte $ff

level_4_enemy_screen_0a:
    .byte $48,$c0,$42 ; enemy type #$42 (laser chandelier), attrs: #$40, pos: (#$88,#$48)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $71,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$18,#$78), can overwrite bullet
    .byte $77,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$78,#$78), can overwrite bullet
    .byte $79,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$98,#$78), can overwrite bullet
    .byte $7e,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$e8,#$78), can overwrite bullet
    .byte $a4,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$48,#$a8), can overwrite bullet
    .byte $a6,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$68,#$a8), can overwrite bullet
    .byte $a8,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$88,#$a8), can overwrite bullet
    .byte $ac,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$c8,#$a8), can overwrite bullet
    .byte $d5,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$58,#$d8), can overwrite bullet
    .byte $d8,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$88,#$d8), can overwrite bullet
    .byte $db,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$b8,#$d8), can overwrite bullet
    .byte $de,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$e8,#$d8), can overwrite bullet
    .byte $ff

level_4_enemy_screen_0b:
    .byte $ff

level_4_enemy_screen_0c:
    .byte $ff

level_4_enemy_screen_0d:
    .byte $ff

level_4_enemy_screen_0e:
    .byte $ff

level_4_enemy_screen_0f:
    .byte $ff

level_4_enemy_screen_10:
    .byte $ff

level_4_enemy_screen_11:
    .byte $15,$81,$40 ; enemy type #$40 (winged soldier), attrs: #$01, pos: (#$58,#$18), can overwrite bullet
    .byte $1a,$80,$40 ; enemy type #$40 (winged soldier), attrs: #$00, pos: (#$a8,#$18), can overwrite bullet
    .byte $3e,$80,$40 ; enemy type #$40 (winged soldier), attrs: #$00, pos: (#$e8,#$38), can overwrite bullet
    .byte $41,$81,$41 ; enemy type #$41 (winged soldier generator), attrs: #$01, pos: (#$18,#$48), can overwrite bullet
    .byte $65,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$58,#$68), can overwrite bullet
    .byte $68,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$88,#$68), can overwrite bullet
    .byte $7b,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$b8,#$78), can overwrite bullet
    .byte $7e,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$e8,#$78), can overwrite bullet
    .byte $ae,$80,$41 ; enemy type #$41 (winged soldier generator), attrs: #$00, pos: (#$e8,#$a8), can overwrite bullet
    .byte $a2,$81,$40 ; enemy type #$40 (winged soldier), attrs: #$01, pos: (#$28,#$a8), can overwrite bullet
    .byte $a8,$80,$40 ; enemy type #$40 (winged soldier), attrs: #$00, pos: (#$88,#$a8), can overwrite bullet
    .byte $d4,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$48,#$d8), can overwrite bullet
    .byte $da,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$a8,#$d8), can overwrite bullet
    .byte $ff

level_4_enemy_screen_12:
    .byte $ff

level_4_enemy_screen_13:
    .byte $ff

level_4_enemy_screen_14:
    .byte $ff

level_4_enemy_screen_15:
    .byte $ff

level_4_enemy_screen_16:
    .byte $ff

level_4_enemy_screen_17:
    .byte $ff

level_4_enemy_screen_18:
    .byte $22,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$28,#$28), can overwrite bullet
    .byte $2d,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$d8,#$28), can overwrite bullet
    .byte $56,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$68,#$58), can overwrite bullet
    .byte $5a,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$a8,#$58), can overwrite bullet
    .byte $73,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$38,#$78), can overwrite bullet
    .byte $8a,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$a8,#$88), can overwrite bullet
    .byte $ac,$a1,$01 ; enemy type #$01 (flying capsule), attrs: #$21, pos: (#$c8,#$a8), can overwrite bullet
    .byte $a4,$a2,$01 ; enemy type #$01 (flying capsule), attrs: #$22, pos: (#$48,#$a8), can overwrite bullet
    .byte $b8,$81,$25 ; enemy type #$25 (rack-mounted turret), attrs: #$01, pos: (#$88,#$b8), can overwrite bullet
    .byte $d8,$80,$25 ; enemy type #$25 (rack-mounted turret), attrs: #$00, pos: (#$88,#$d8), can overwrite bullet
    .byte $ff

level_4_enemy_screen_19:
    .byte $ff

level_4_enemy_screen_1a:
    .byte $ff

level_4_enemy_screen_1b:
    .byte $ff

level_4_enemy_screen_1c:
    .byte $ff

level_4_enemy_screen_1d:
    .byte $ff

level_4_enemy_screen_1e:
    .byte $ff

level_4_enemy_screen_1f:
    .byte $62,$92,$0a ; enemy type #$0a (rotating gun), attrs: #$12, pos: (#$28,#$68), can overwrite bullet
    .byte $6c,$92,$0a ; enemy type #$0a (rotating gun), attrs: #$12, pos: (#$c8,#$68), can overwrite bullet
    .byte $d8,$80,$25 ; enemy type #$25 (rack-mounted turret), attrs: #$00, pos: (#$88,#$d8), can overwrite bullet
    .byte $ff

level_4_enemy_screen_20:
    .byte $ff

level_4_enemy_screen_21:
    .byte $ff

level_4_enemy_screen_22:
    .byte $ff

level_4_enemy_screen_23:
    .byte $ff

level_4_enemy_screen_24:
    .byte $ff

level_4_enemy_screen_25:
    .byte $ff

level_4_enemy_screen_26:
    .byte $28,$81,$25 ; enemy type #$25 (rack-mounted turret), attrs: #$01, pos: (#$88,#$28), can overwrite bullet
    .byte $94,$c0,$39 ; enemy type #$39 (elevator), attrs: #$40, pos: (#$48,#$98), can overwrite bullet, can overwrite slot 0
    .byte $92,$81,$40 ; enemy type #$40 (winged soldier), attrs: #$01, pos: (#$28,#$98), can overwrite bullet
    .byte $9c,$80,$40 ; enemy type #$40 (winged soldier), attrs: #$00, pos: (#$c8,#$98), can overwrite bullet
    .byte $ff

level_4_enemy_screen_27:
    .byte $ff

level_4_enemy_screen_28:
    .byte $ff

level_4_enemy_screen_29:
    .byte $ff

level_4_enemy_screen_2a:
    .byte $ff

level_4_enemy_screen_2b:
    .byte $ff

level_4_enemy_screen_2c:
    .byte $ff

level_4_enemy_screen_2d:
    .byte $11,$81,$41 ; enemy type #$41 (winged soldier generator), attrs: #$01, pos: (#$18,#$18), can overwrite bullet
    .byte $15,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$58,#$18), can overwrite bullet
    .byte $2e,$80,$41 ; enemy type #$41 (winged soldier generator), attrs: #$00, pos: (#$e8,#$28), can overwrite bullet
    .byte $66,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$68,#$68), can overwrite bullet
    .byte $ff

level_4_enemy_screen_2e:
    .byte $ff

level_4_enemy_screen_2f:
    .byte $ff

level_4_enemy_screen_30:
    .byte $ff

level_4_enemy_screen_31:
    .byte $ff

level_4_enemy_screen_32:
    .byte $ff

level_4_enemy_screen_33:
    .byte $ff

level_4_enemy_screen_34:
    .byte $44,$92,$0a ; enemy type #$0a (rotating gun), attrs: #$12, pos: (#$48,#$48), can overwrite bullet
    .byte $4a,$92,$0a ; enemy type #$0a (rotating gun), attrs: #$12, pos: (#$a8,#$48), can overwrite bullet
    .byte $ff

level_4_enemy_screen_35:
    .byte $ff

level_4_enemy_screen_36:
    .byte $ff

level_4_enemy_screen_37:
    .byte $ff

level_4_enemy_screen_38:
    .byte $ff

level_4_enemy_screen_39:
    .byte $ff

level_4_enemy_screen_3a:
    .byte $ff

level_4_enemy_screen_3b:
    .byte $05,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$58,#$08), can overwrite bullet
    .byte $09,$80,$31 ; enemy type #$31 (spinning bubbles), attrs: #$00, pos: (#$98,#$08), can overwrite bullet
    .byte $14,$a0,$01 ; enemy type #$01 (flying capsule), attrs: #$20, pos: (#$48,#$18), can overwrite bullet
    .byte $2b,$a4,$01 ; enemy type #$01 (flying capsule), attrs: #$24, pos: (#$b8,#$28), can overwrite bullet
    .byte $94,$c0,$39 ; enemy type #$39 (elevator), attrs: #$40, pos: (#$48,#$98), can overwrite bullet, can overwrite slot 0
    .byte $ff

level_4_enemy_screen_3c:
    .byte $ff

level_4_enemy_screen_3d:
    .byte $ff

level_4_enemy_screen_3e:
    .byte $ff

level_4_enemy_screen_3f:
    .byte $ff

level_4_enemy_screen_40:
    .byte $ff

level_4_enemy_screen_41:
    .byte $ff

level_4_enemy_screen_42:
    .byte $18,$c0,$2c ; enemy type #$2c (collapsible ceiling), attrs: #$40, pos: (#$88,#$18)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $41,$01,$41 ; enemy type #$41 (winged soldier generator), attrs: #$01, pos: (#$18,#$48)
    .byte $48,$82,$0a ; enemy type #$0a (rotating gun), attrs: #$02, pos: (#$88,#$48), can overwrite bullet
    .byte $ff

level_4_enemy_screen_43:
    .byte $ff

level_4_enemy_screen_44:
    .byte $ff

level_4_enemy_screen_45:
    .byte $ff

level_4_enemy_screen_46:
    .byte $ff

level_4_enemy_screen_47:
    .byte $ff

level_4_enemy_screen_48:
    .byte $ff

level_4_enemy_screen_49:
    .byte $08,$82,$0a ; enemy type #$0a (rotating gun), attrs: #$02, pos: (#$88,#$08), can overwrite bullet
    .byte $24,$82,$0a ; enemy type #$0a (rotating gun), attrs: #$02, pos: (#$48,#$28), can overwrite bullet
    .byte $6e,$00,$41 ; enemy type #$41 (winged soldier generator), attrs: #$00, pos: (#$e8,#$68)
    .byte $ff

level_4_enemy_screen_4a:
    .byte $ff

level_4_enemy_screen_4b:
    .byte $ff

level_4_enemy_screen_4c:
    .byte $ff

level_4_enemy_screen_4d:
    .byte $ff

level_4_enemy_screen_4e:
    .byte $ff

level_4_enemy_screen_4f:
    .byte $ff

level_4_enemy_screen_50:
    .byte $11,$01,$41 ; enemy type #$41 (winged soldier generator), attrs: #$01, pos: (#$18,#$18)
    .byte $42,$82,$0a ; enemy type #$0a (rotating gun), attrs: #$02, pos: (#$28,#$48), can overwrite bullet
    .byte $4a,$81,$0a ; enemy type #$0a (rotating gun), attrs: #$01, pos: (#$a8,#$48), can overwrite bullet
    .byte $9e,$00,$41 ; enemy type #$41 (winged soldier generator), attrs: #$00, pos: (#$e8,#$98)
    .byte $ff

level_4_enemy_screen_51:
    .byte $ff

level_4_enemy_screen_52:
    .byte $ff

level_4_enemy_screen_53:
    .byte $ff

level_4_enemy_screen_54:
    .byte $ff

level_4_enemy_screen_55:
    .byte $52,$00,$6c ; enemy type #$6c (ceiling vent), attrs: #$00, pos: (#$28,#$58)
    .byte $91,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$18,#$98)
    .byte $91,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$18,#$98)
    .byte $9a,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$a8,#$98)
    .byte $9c,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$c8,#$98)
    .byte $9f,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$f8,#$98)
    .byte $67,$90,$01 ; enemy type #$01 (flying capsule), attrs: #$10, pos: (#$78,#$68), can overwrite bullet
    .byte $ff

level_4_enemy_screen_56:
    .byte $52,$00,$6c ; enemy type #$6c (ceiling vent), attrs: #$00, pos: (#$28,#$58)
    .byte $92,$09,$03 ; enemy type #$03 (soldier), attrs: #$09, pos: (#$28,#$98)
    .byte $95,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$58,#$98)
    .byte $95,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$58,#$98)
    .byte $9b,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$b8,#$98)
    .byte $9c,$09,$03 ; enemy type #$03 (soldier), attrs: #$09, pos: (#$c8,#$98)
    .byte $9e,$08,$03 ; enemy type #$03 (soldier), attrs: #$08, pos: (#$e8,#$98)
    .byte $9e,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$e8,#$98)
    .byte $ff

level_4_enemy_screen_57:
    .byte $18,$c0,$2c ; enemy type #$2c (collapsible ceiling), attrs: #$40, pos: (#$88,#$18)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $45,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$58,#$48), can overwrite bullet
    .byte $4a,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$a8,#$48), can overwrite bullet
    .byte $91,$00,$03 ; enemy type #$03 (soldier), attrs: #$00, pos: (#$18,#$98)
    .byte $91,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$18,#$98)
    .byte $96,$09,$03 ; enemy type #$03 (soldier), attrs: #$09, pos: (#$68,#$98)
    .byte $98,$01,$03 ; enemy type #$03 (soldier), attrs: #$01, pos: (#$88,#$98)
    .byte $ff

level_4_enemy_screen_58:
    .byte $ff

level_4_enemy_screen_59:
    .byte $ff

level_4_enemy_screen_5a:
    .byte $ff

level_5_enemy_screen_ptr_tbl:
    .addr level_5_enemy_screen_00
    .addr level_5_enemy_screen_01
    .addr level_5_enemy_screen_02
    .addr level_5_enemy_screen_03
    .addr level_5_enemy_screen_04
    .addr level_5_enemy_screen_05
    .addr level_5_enemy_screen_06
    .addr level_5_enemy_screen_07
    .addr level_5_enemy_screen_08
    .addr level_5_enemy_screen_09
    .addr level_5_enemy_screen_0a
    .addr level_5_enemy_screen_0b
    .addr level_5_enemy_screen_0c
    .addr level_5_enemy_screen_0d
    .addr level_5_enemy_screen_0e
    .addr level_5_enemy_screen_0f
    .addr level_5_enemy_screen_10
    .addr level_5_enemy_screen_11
    .addr level_5_enemy_screen_12
    .addr level_5_enemy_screen_13
    .addr level_5_enemy_screen_14
    .addr level_5_enemy_screen_15
    .addr level_5_enemy_screen_16
    .addr level_5_enemy_screen_17
    .addr level_5_enemy_screen_18
    .addr level_5_enemy_screen_19
    .addr level_5_enemy_screen_1a
    .addr level_5_enemy_screen_1b
    .addr level_5_enemy_screen_1c
    .addr level_5_enemy_screen_1d
    .addr level_5_enemy_screen_1e
    .addr level_5_enemy_screen_1f
    .addr level_5_enemy_screen_20
    .addr level_5_enemy_screen_21
    .addr level_5_enemy_screen_22
    .addr level_5_enemy_screen_23
    .addr level_5_enemy_screen_24
    .addr level_5_enemy_screen_25
    .addr level_5_enemy_screen_26
    .addr level_5_enemy_screen_27
    .addr level_5_enemy_screen_28
    .addr level_5_enemy_screen_29
    .addr level_5_enemy_screen_2a
    .addr level_5_enemy_screen_2b
    .addr level_5_enemy_screen_2c
    .addr level_5_enemy_screen_2d
    .addr level_5_enemy_screen_2e
    .addr level_5_enemy_screen_2f
    .addr level_5_enemy_screen_30
    .addr level_5_enemy_screen_31
    .addr level_5_enemy_screen_32
    .addr level_5_enemy_screen_33
    .addr level_5_enemy_screen_34
    .addr level_5_enemy_screen_35
    .addr level_5_enemy_screen_36
    .addr level_5_enemy_screen_37
    .addr level_5_enemy_screen_38
    .addr level_5_enemy_screen_39
    .addr level_5_enemy_screen_3a
    .addr level_5_enemy_screen_3b
    .addr level_5_enemy_screen_3c
    .addr level_5_enemy_screen_3d
    .addr level_5_enemy_screen_3e

level_5_enemy_screen_00:
    .byte $aa,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$a8,#$a8), can overwrite bullet
    .byte $c1,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$18,#$c8), can overwrite bullet
    .byte $d6,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$68,#$d8), can overwrite bullet
    .byte $ff

level_5_enemy_screen_01:
    .byte $ff

level_5_enemy_screen_02:
    .byte $ff

level_5_enemy_screen_03:
    .byte $ff

level_5_enemy_screen_04:
    .byte $ff

level_5_enemy_screen_05:
    .byte $ff

level_5_enemy_screen_06:
    .byte $ff

level_5_enemy_screen_07:
    .byte $ff

level_5_enemy_screen_08:
    .byte $ff

level_5_enemy_screen_09:
    .byte $03,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$38,#$08), can overwrite bullet
    .byte $28,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$88,#$28), can overwrite bullet
    .byte $43,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$38,#$48), can overwrite bullet
    .byte $7b,$80,$2f ; enemy type #$2f (falling rock), attrs: #$00, pos: (#$b8,#$78), can overwrite bullet
    .byte $ff

level_5_enemy_screen_0a:
    .byte $ff

level_5_enemy_screen_0b:
    .byte $59,$80,$58 ; enemy type #$58 (storage room soldier generator), attrs: #$00, pos: (#$98,#$58), can overwrite bullet
    .byte $ff

level_5_enemy_screen_0c:
    .byte $e9,$80,$32
    .byte $ff

level_5_enemy_screen_0d:
    .byte $ff

level_5_enemy_screen_0e:
    .byte $ff

level_5_enemy_screen_0f:
    .byte $ff

level_5_enemy_screen_10:
    .byte $ff

level_5_enemy_screen_11:
    .byte $ff

level_5_enemy_screen_12:
    .byte $13,$82,$09 ; enemy type #$09 (sniper), attrs: #$02, pos: (#$38,#$18), can overwrite bullet
    .byte $4a,$81,$09 ; enemy type #$09 (sniper), attrs: #$01, pos: (#$a8,#$48), can overwrite bullet
    .byte $81,$80,$34 ; enemy type #$34 (jet pack soldier), attrs: #$00, pos: (#$18,#$88), can overwrite bullet
    .byte $8e,$80,$34 ; enemy type #$34 (jet pack soldier), attrs: #$00, pos: (#$e8,#$88), can overwrite bullet
    .byte $ff

level_5_enemy_screen_13:
    .byte $ff

level_5_enemy_screen_14:
    .byte $ff

level_5_enemy_screen_15:
    .byte $1b,$80,$58
    .byte $ff

level_5_enemy_screen_16:
    .byte $ad,$80,$32
    .byte $ff

level_5_enemy_screen_17:
    .byte $88,$c0,$59
    .byte $ff

level_5_enemy_screen_18:
    .byte $ff

level_5_enemy_screen_19:
    .byte $ff

level_5_enemy_screen_1a:
    .byte $ff

level_5_enemy_screen_1b:
    .byte $0e,$80,$34 ; enemy type #$34 (jet pack soldier), attrs: #$00, pos: (#$e8,#$08), can overwrite bullet
    .byte $21,$80,$34 ; enemy type #$34 (jet pack soldier), attrs: #$00, pos: (#$18,#$28), can overwrite bullet
    .byte $56,$a6,$01 ; enemy type #$01 (flying capsule), attrs: #$26, pos: (#$68,#$58), can overwrite bullet
    .byte $6b,$a1,$01 ; enemy type #$01 (flying capsule), attrs: #$21, pos: (#$b8,#$68), can overwrite bullet
    .byte $a6,$84,$04 ; enemy type #$04 (pill box sensor), attrs: #$04, pos: (#$68,#$a8), can overwrite bullet
    .byte $ff

level_5_enemy_screen_1c:
    .byte $ff

level_5_enemy_screen_1d:
    .byte $ff

level_5_enemy_screen_1e:
    .byte $ff

level_5_enemy_screen_1f:
    .byte $ff

level_5_enemy_screen_20:
    .byte $ff

level_5_enemy_screen_21:
    .byte $ff

level_5_enemy_screen_22:
    .byte $ff

level_5_enemy_screen_23:
    .byte $ff

level_5_enemy_screen_24:
    .byte $0a,$8b,$0a ; enemy type #$0a (rotating gun), attrs: #$0b, pos: (#$a8,#$08), can overwrite bullet
    .byte $42,$83,$09 ; enemy type #$09 (sniper), attrs: #$03, pos: (#$28,#$48), can overwrite bullet
    .byte $c0,$8a,$0a ; enemy type #$0a (rotating gun), attrs: #$0a, pos: (#$08,#$c8), can overwrite bullet
    .byte $ff

level_5_enemy_screen_25:
    .byte $ff

level_5_enemy_screen_26:
    .byte $ff

level_5_enemy_screen_27:
    .byte $ff

level_5_enemy_screen_28:
    .byte $ff

level_5_enemy_screen_29:
    .byte $ff

level_5_enemy_screen_2a:
    .byte $ff

level_5_enemy_screen_2b:
    .byte $ff

level_5_enemy_screen_2c:
    .byte $ff

level_5_enemy_screen_2d:
    .byte $0e,$8a,$0a ; enemy type #$0a (rotating gun), attrs: #$0a, pos: (#$e8,#$08), can overwrite bullet
    .byte $60,$84,$0b ; enemy type #$0b (gray turret), attrs: #$04, pos: (#$08,#$68), can overwrite bullet
    .byte $8c,$83,$04 ; enemy type #$04 (pill box sensor), attrs: #$03, pos: (#$c8,#$88), can overwrite bullet
    .byte $ca,$85,$0b ; enemy type #$0b (gray turret), attrs: #$05, pos: (#$a8,#$c8), can overwrite bullet
    .byte $ee,$90,$04 ; enemy type #$04 (pill box sensor), attrs: #$10, pos: (#$e8,#$e8), can overwrite bullet
    .byte $ff

level_5_enemy_screen_2e:
    .byte $ff

level_5_enemy_screen_2f:
    .byte $ff

level_5_enemy_screen_30:
    .byte $ff

level_5_enemy_screen_31:
    .byte $ff

level_5_enemy_screen_32:
    .byte $ff

level_5_enemy_screen_33:
    .byte $ff

level_5_enemy_screen_34:
    .byte $ff

level_5_enemy_screen_35:
    .byte $ff

level_5_enemy_screen_36:
    .byte $ff

level_5_enemy_screen_37:
    .byte $ff

level_5_enemy_screen_38:
    .byte $ff

level_5_enemy_screen_39:
    .byte $ff

level_5_enemy_screen_3a:
    .byte $ff

level_5_enemy_screen_3b:
    .byte $ff

level_5_enemy_screen_3c:
    .byte $ff

level_5_enemy_screen_3d:
    .byte $ff

level_5_enemy_screen_3e:
    .byte $ff

level_6_enemy_screen_ptr_tbl:
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_02
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_04
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_06
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_08
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_0a
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_0c
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_0e
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_10
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_12
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_14
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_16
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_18
    .addr level_6_enemy_screen_no_enemies
    .addr level_6_enemy_screen_1a
    .addr level_6_enemy_screen_no_enemies

level_6_enemy_screen_no_enemies:
    .byte $ff

level_6_enemy_screen_02:
    .byte $10,$c0,$64     ; enemy type #$64 (suspicious face), attrs: #$40, pos: (#$08,#$18), can overwrite bullet, can overwrite slot 0
    .byte $5d,$80,$70     ; enemy type #$70 (suspicious face arm), attrs: #$00, pos: (#$d8,#$58), can overwrite bullet
    .byte $52,$81,$70     ; enemy type #$70 (suspicious face arm), attrs: #$01, pos: (#$28,#$58), can overwrite bullet
    .ifdef Probotector
        .byte $61,$81,$71 ; enemy type #$71 (area 6 tile swapper), attrs: #$01, pos: (#$18,#$68), can overwrite bullet
    .else
        .byte $71,$81,$71 ; enemy type #$71 (area 6 tile swapper), attrs: #$01, pos: (#$18,#$78), can overwrite bullet
    .endif
    .byte $ff

level_6_enemy_screen_04:
    .byte $a7,$c1,$0d ; enemy type #$0d (door), attrs: #$41, pos: (#$78,#$a8), can overwrite bullet, can overwrite slot 0
    .byte $ff

level_6_enemy_screen_06:
    .ifdef Probotector
        .byte $d1,$c0,$71 ; enemy type #$71 (area 6 tile swapper), attrs: #$40, pos: (#$18,#$d8)
                          ; can overwrite bullet, can overwrite slot 0
    .else
        .byte $e1,$c0,$71 ; enemy type #$71 (area 6 tile swapper), attrs: #$40, pos: (#$18,#$e8)
                          ; can overwrite bullet, can overwrite slot 0
    .endif
    .byte $ff

level_6_enemy_screen_08:
    .byte $02,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$08), can overwrite bullet
    .byte $0d,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$08), can overwrite bullet
    .byte $42,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$48), can overwrite bullet
    .byte $4d,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$48), can overwrite bullet
    .byte $82,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$88), can overwrite bullet
    .byte $8d,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$88), can overwrite bullet
    .byte $98,$b6,$01 ; enemy type #$01 (flying capsule), attrs: #$36, pos: (#$88,#$98), can overwrite bullet
    .byte $c2,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$c8), can overwrite bullet
    .byte $cd,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$c8), can overwrite bullet
    .byte $ff

level_6_enemy_screen_0a:
    .byte $12,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$18), can overwrite bullet
    .byte $1d,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$18), can overwrite bullet
    .byte $52,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$58), can overwrite bullet
    .byte $5d,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$58), can overwrite bullet
    .byte $58,$b1,$01 ; enemy type #$01 (flying capsule), attrs: #$31, pos: (#$88,#$58), can overwrite bullet
    .byte $58,$82,$01 ; enemy type #$01 (flying capsule), attrs: #$02, pos: (#$88,#$58), can overwrite bullet
    .byte $92,$81,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$01, pos: (#$28,#$98), can overwrite bullet
    .byte $9d,$80,$60 ; enemy type #$60 (baby alien ladybug), attrs: #$00, pos: (#$d8,#$98), can overwrite bullet
    .byte $ff

level_6_enemy_screen_0c:
    .byte $ff

level_6_enemy_screen_0e:
    .byte $14,$81,$5f ; enemy type #$5f (big faced one-eyed monster), attrs: #$01, pos: (#$48,#$18), can overwrite bullet
    .byte $4c,$80,$5f ; enemy type #$5f (big faced one-eyed monster), attrs: #$00, pos: (#$c8,#$48), can overwrite bullet
    .byte $44,$81,$5f ; enemy type #$5f (big faced one-eyed monster), attrs: #$01, pos: (#$48,#$48), can overwrite bullet
    .byte $a8,$80,$5f ; enemy type #$5f (big faced one-eyed monster), attrs: #$00, pos: (#$88,#$a8), can overwrite bullet
    .byte $ff

level_6_enemy_screen_10:
    .byte $ff

level_6_enemy_screen_12:
    .byte $1a,$81,$5d ; enemy type #$5d (mouth pit), attrs: #$01, pos: (#$a8,#$18), can overwrite bullet
    .byte $88,$b3,$01 ; enemy type #$01 (flying capsule), attrs: #$33, pos: (#$88,#$88), can overwrite bullet
    .byte $a8,$80,$01 ; enemy type #$01 (flying capsule), attrs: #$00, pos: (#$88,#$a8), can overwrite bullet
    .byte $e0,$80,$5e ; enemy type #$5e (mouth pit generator), attrs: #$00, pos: (#$08,#$e8), can overwrite bullet
    .byte $ff

level_6_enemy_screen_14:
    .byte $ff

level_6_enemy_screen_16:
    .byte $ff

level_6_enemy_screen_18:
    .byte $ff

level_6_enemy_screen_1a:
    .byte $ff

level_7_enemy_screen_ptr_tbl:
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_02
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_04
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_06
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_08
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_0a
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_0c
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_0e
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_10
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_12
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_14
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_16
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_18
    .addr level_7_enemy_screen_no_enemies
    .addr level_7_enemy_screen_1a
    .addr level_7_enemy_screen_no_enemies

level_7_enemy_screen_no_enemies:
    .byte $ff

level_7_enemy_screen_02:
    .byte $ff

level_7_enemy_screen_04:
    .byte $8a,$a0,$01 ; enemy type #$01 (flying capsule), attrs: #$20, pos: (#$a8,#$88), can overwrite bullet
    .byte $86,$a4,$01 ; enemy type #$01 (flying capsule), attrs: #$24, pos: (#$68,#$88), can overwrite bullet
    .byte $e4,$01,$3b ; enemy type #$3b (manooki), attrs: #$01, pos: (#$48,#$e8)
    .byte $ff

level_7_enemy_screen_06:
    .byte $8b,$00,$3b ; enemy type #$3b (manooki), attrs: #$00, pos: (#$b8,#$88)
    .byte $d6,$01,$3b ; enemy type #$3b (manooki), attrs: #$01, pos: (#$68,#$d8)
    .byte $ff

level_7_enemy_screen_08:
    .byte $7b,$00,$3b ; enemy type #$3b (manooki), attrs: #$00, pos: (#$b8,#$78)
    .byte $ff

level_7_enemy_screen_0a:
    .byte $14,$01,$3b ; enemy type #$3b (manooki), attrs: #$01, pos: (#$48,#$18)
    .byte $bb,$00,$3b ; enemy type #$3b (manooki), attrs: #$00, pos: (#$b8,#$b8)
    .byte $ff

level_7_enemy_screen_0c:
    .byte $b8,$a2,$01 ; enemy type #$01 (flying capsule), attrs: #$22, pos: (#$88,#$b8), can overwrite bullet
    .byte $ff

level_7_enemy_screen_0e:
    .byte $8c,$a3,$01 ; enemy type #$01 (flying capsule), attrs: #$23, pos: (#$c8,#$88), can overwrite bullet
    .byte $84,$a0,$01 ; enemy type #$01 (flying capsule), attrs: #$20, pos: (#$48,#$88), can overwrite bullet
    .byte $ff

level_7_enemy_screen_10:
    .byte $ff

level_7_enemy_screen_12:
    .byte $7f,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$f8,#$78)
    .byte $7b,$a1,$01 ; enemy type #$01 (flying capsule), attrs: #$21, pos: (#$b8,#$78), can overwrite bullet
    .byte $77,$a4,$01 ; enemy type #$01 (flying capsule), attrs: #$24, pos: (#$78,#$78), can overwrite bullet
    .byte $b1,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$18,#$b8)
    .byte $b3,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$38,#$b8)
    .byte $ff

level_7_enemy_screen_14:
    .byte $09,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$98,#$08)
    .byte $43,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$38,#$48)
    .byte $49,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$98,#$48)
    .byte $a5,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$58,#$a8)
    .byte $af,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$f8,#$a8)
    .byte $ff

level_7_enemy_screen_16:
    .byte $35,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$58,#$38)
    .byte $39,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$98,#$38)
    .byte $97,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$78,#$98)
    .byte $9f,$00,$3d ; enemy type #$3d (eggron), attrs: #$00, pos: (#$f8,#$98)
    .byte $ff

level_7_enemy_screen_18:
    .byte $47,$00,$51 ; enemy type #$51 (temple of terror), attrs: #$00, pos: (#$78,#$48)
    .byte $ff

level_7_enemy_screen_1a:
    .byte $ff

level_8_enemy_screen_ptr_tbl:
    .addr level_8_enemy_screen_00
    .addr level_8_enemy_screen_01
    .addr level_8_enemy_screen_02
    .addr level_8_enemy_screen_03
    .addr level_8_enemy_screen_04
    .addr level_8_enemy_screen_05
    .addr level_8_enemy_screen_06
    .addr level_8_enemy_screen_07
    .addr level_8_enemy_screen_08
    .addr level_8_enemy_screen_09
    .addr level_8_enemy_screen_0a
    .addr level_8_enemy_screen_0b
    .addr level_8_enemy_screen_0c
    .addr level_8_enemy_screen_0d
    .addr level_8_enemy_screen_0e
    .addr level_8_enemy_screen_0f
    .addr level_8_enemy_screen_10
    .addr level_8_enemy_screen_11
    .addr level_8_enemy_screen_12
    .addr level_8_enemy_screen_13
    .addr level_8_enemy_screen_14
    .addr level_8_enemy_screen_15
    .addr level_8_enemy_screen_16
    .addr level_8_enemy_screen_17
    .addr level_8_enemy_screen_18
    .addr level_8_enemy_screen_19
    .addr level_8_enemy_screen_1a
    .addr level_8_enemy_screen_1b
    .addr level_8_enemy_screen_1c
    .addr level_8_enemy_screen_1d
    .addr level_8_enemy_screen_1e
    .addr level_8_enemy_screen_1f
    .addr level_8_enemy_screen_20
    .addr level_8_enemy_screen_21
    .addr level_8_enemy_screen_22
    .addr level_8_enemy_screen_23
    .addr level_8_enemy_screen_24
    .addr level_8_enemy_screen_25
    .addr level_8_enemy_screen_26
    .addr level_8_enemy_screen_27
    .addr level_8_enemy_screen_28
    .addr level_8_enemy_screen_29
    .addr level_8_enemy_screen_2a
    .addr level_8_enemy_screen_2b
    .addr level_8_enemy_screen_2c
    .addr level_8_enemy_screen_2d
    .addr level_8_enemy_screen_2e
    .addr level_8_enemy_screen_2f
    .addr level_8_enemy_screen_30
    .addr level_8_enemy_screen_31
    .addr level_8_enemy_screen_32
    .addr level_8_enemy_screen_33
    .addr level_8_enemy_screen_34
    .addr level_8_enemy_screen_35
    .addr level_8_enemy_screen_36
    .addr level_8_enemy_screen_37
    .addr level_8_enemy_screen_38
    .addr level_8_enemy_screen_39
    .addr level_8_enemy_screen_3a
    .addr level_8_enemy_screen_3b
    .addr level_8_enemy_screen_3c
    .addr level_8_enemy_screen_3d
    .addr level_8_enemy_screen_3e
    .addr level_8_enemy_screen_3f

level_8_enemy_screen_00:
    .byte $ff

level_8_enemy_screen_01:
    .byte $ff

level_8_enemy_screen_02:
    .byte $ff

level_8_enemy_screen_03:
    .byte $ff

level_8_enemy_screen_04:
    .byte $ff

level_8_enemy_screen_05:
    .byte $ff

level_8_enemy_screen_06:
    .byte $ff

level_8_enemy_screen_07:
    .byte $ff

level_8_enemy_screen_08:
    .byte $ff

level_8_enemy_screen_09:
    .byte $ff

level_8_enemy_screen_0a:
    .byte $ff

level_8_enemy_screen_0b:
    .byte $ff

level_8_enemy_screen_0c:
    .byte $ff

level_8_enemy_screen_0d:
    .byte $ff

level_8_enemy_screen_0e:
    .byte $ff

level_8_enemy_screen_0f:
    .byte $ff

level_8_enemy_screen_10:
    .byte $ff

level_8_enemy_screen_11:
    .byte $ff

level_8_enemy_screen_12:
    .byte $ff

level_8_enemy_screen_13:
    .byte $ff

level_8_enemy_screen_14:
    .byte $ff

level_8_enemy_screen_15:
    .byte $ff

level_8_enemy_screen_16:
    .byte $89,$82,$45 ; enemy type #$45 (wadder), attrs: #$02, pos: (#$98,#$88), can overwrite bullet
    .byte $ff

level_8_enemy_screen_17:
    .byte $a5,$83,$45 ; enemy type #$45 (wadder), attrs: #$03, pos: (#$58,#$a8), can overwrite bullet
    .byte $ff

level_8_enemy_screen_18:
    .byte $31,$93,$01 ; enemy type #$01 (flying capsule), attrs: #$13, pos: (#$18,#$38), can overwrite bullet
    .byte $71,$96,$01 ; enemy type #$01 (flying capsule), attrs: #$16, pos: (#$18,#$78), can overwrite bullet
    .byte $ff

level_8_enemy_screen_19:
    .byte $48,$c0,$49 ; enemy type #$49 (stomping ceiling), attrs: #$40, pos: (#$88,#$48)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $ff

level_8_enemy_screen_1a:
    .byte $48,$c1,$49 ; enemy type #$49 (stomping ceiling), attrs: #$41, pos: (#$88,#$48)
                      ; can overwrite bullet, can overwrite slot 0
    .byte $ff

level_8_enemy_screen_1b:
    .byte $ff

level_8_enemy_screen_1c:
    .byte $88,$c0,$6d ; enemy type #$6d (kimkoh), attrs: #$40, pos: (#$88,#$88), can overwrite bullet, can overwrite slot 0
    .byte $ff

level_8_enemy_screen_1d:
    .byte $ff

level_8_enemy_screen_1e:
    .byte $ff

level_8_enemy_screen_1f:
    .byte $ff

level_8_enemy_screen_20:
    .byte $ff

level_8_enemy_screen_21:
    .byte $ff

level_8_enemy_screen_22:
    .byte $ff

level_8_enemy_screen_23:
    .byte $ff

level_8_enemy_screen_24:
    .byte $ff

level_8_enemy_screen_25:
    .byte $87,$83,$45 ; enemy type #$45 (wadder), attrs: #$03, pos: (#$78,#$88), can overwrite bullet
    .byte $a3,$92,$01 ; enemy type #$01 (flying capsule), attrs: #$12, pos: (#$38,#$a8), can overwrite bullet
    .byte $af,$82,$47 ; enemy type #$47 (jameera), attrs: #$02, pos: (#$f8,#$a8), can overwrite bullet
    .byte $ff

level_8_enemy_screen_26:
    .byte $2b,$86,$47 ; enemy type #$47 (jameera), attrs: #$06, pos: (#$b8,#$28), can overwrite bullet
    .byte $ff

level_8_enemy_screen_27:
    .byte $ff

level_8_enemy_screen_28:
    .byte $ff

level_8_enemy_screen_29:
    .byte $ff

level_8_enemy_screen_2a:
    .byte $ff

level_8_enemy_screen_2b:
    .byte $ff

level_8_enemy_screen_2c:
    .byte $ff

level_8_enemy_screen_2d:
    .byte $ff

level_8_enemy_screen_2e:
    .byte $ff

level_8_enemy_screen_2f:
    .byte $ff

level_8_enemy_screen_30:
    .byte $ff

level_8_enemy_screen_31:
    .byte $12,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$28,#$18)
    .byte $44,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$48,#$48)
    .byte $46,$90,$01 ; enemy type #$01 (flying capsule), attrs: #$10, pos: (#$68,#$48), can overwrite bullet
    .byte $82,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$28,#$88)
    .byte $a6,$93,$01 ; enemy type #$01 (flying capsule), attrs: #$13, pos: (#$68,#$a8), can overwrite bullet
    .byte $c4,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$48,#$c8)
    .byte $c9,$82,$45 ; enemy type #$45 (wadder), attrs: #$02, pos: (#$98,#$c8), can overwrite bullet
    .byte $c3,$80,$45 ; enemy type #$45 (wadder), attrs: #$00, pos: (#$38,#$c8), can overwrite bullet
    .byte $ff

level_8_enemy_screen_32:
    .byte $2d,$80,$45 ; enemy type #$45 (wadder), attrs: #$00, pos: (#$d8,#$28), can overwrite bullet
    .byte $22,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$28,#$28)
    .byte $54,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$48,#$58)
    .byte $82,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$28,#$88)
    .byte $b4,$00,$2a ; enemy type #$2a (red bubble), attrs: #$00, pos: (#$48,#$b8)
    .byte $a5,$81,$45 ; enemy type #$45 (wadder), attrs: #$01, pos: (#$58,#$a8), can overwrite bullet
    .byte $ff

level_8_enemy_screen_33:
    .byte $23,$80,$45 ; enemy type #$45 (wadder), attrs: #$00, pos: (#$38,#$28), can overwrite bullet
    .byte $29,$80,$45 ; enemy type #$45 (wadder), attrs: #$00, pos: (#$98,#$28), can overwrite bullet
    .byte $5b,$90,$01 ; enemy type #$01 (flying capsule), attrs: #$10, pos: (#$b8,#$58), can overwrite bullet
    .byte $8b,$94,$01 ; enemy type #$01 (flying capsule), attrs: #$14, pos: (#$b8,#$88), can overwrite bullet
    .byte $ff

level_8_enemy_screen_34:
    .byte $2f,$82,$45 ; enemy type #$45 (wadder), attrs: #$02, pos: (#$f8,#$28), can overwrite bullet
    .byte $46,$91,$01 ; enemy type #$01 (flying capsule), attrs: #$11, pos: (#$68,#$48), can overwrite bullet
    .byte $83,$82,$45 ; enemy type #$45 (wadder), attrs: #$02, pos: (#$38,#$88), can overwrite bullet
    .byte $ff

level_8_enemy_screen_35:
    .byte $ff

level_8_enemy_screen_36:
    .byte $ff

level_8_enemy_screen_37:
    .byte $ff

level_8_enemy_screen_38:
    .byte $ff

level_8_enemy_screen_39:
    .byte $ff

level_8_enemy_screen_3a:
    .byte $ff

level_8_enemy_screen_3b:
    .byte $ff

level_8_enemy_screen_3c:
    .byte $ff

level_8_enemy_screen_3d:
    .byte $ff

level_8_enemy_screen_3e:
    .byte $ff

level_8_enemy_screen_3f:
    .byte $ff

; the rest of this bank is related to the sound menu
run_sound_menu_routine:
    lda SOUND_MENU_ROUTINE         ; load current sound menu init routine
    jsr run_routine_from_tbl_below

sound_menu_routine:
    .addr sound_menu_routine_00 ; clears sound menu variables and inits vars next routine
    .addr sound_menu_routine_01 ; clears the bottom #$0d rows of the visible nametable to make space for sound menu
    .addr sound_menu_routine_02 ; draws sound menu UI including the names
    .addr sound_menu_routine_03 ; checks for input and plays/pauses or scrolls

; clear sound menu variables and inits vars next routine
sound_menu_routine_00:
    ldy #$18
    lda #$00

; set $68 to $50 to #$00
@loop:
    sta $50,y                  ; clear sound menu UI variable
    dey
    bpl @loop                  ; loop until y is #$ff
    lda #$23                   ; clearing the bottom #$0d rows of the visible nametable
                               ; starting at $23a0 and moving up a row each loop
    sta $57
    lda #$a0
    sta $56                    ; last row of the visible nametable
    lda #$0d                   ; number of rows to clear
    sta $55                    ; store number of rows to clear
    jmp adv_sound_routine_exit ; advance sound routine and exit

; clears the bottom #$0d rows of the visible nametable to make space for sound menu
sound_menu_routine_01:
    jsr clear_sound_menu_row        ; clears the entire row of the nametable starting
                                    ; at PPU address $57 (high PPU byte), and $56 (low PPU byte)
    dec $55
    bpl run_sound_menu_routine_exit ; exit if there are still rows to clear next frame
    lda #$00                        ; cleared all rows, set delay of #$04 frames and start drawing sound menu UI
    sta $55                         ; start at the first sound element to draw (sound_menu_ui_elements_tbl offset)
    lda #$04
    sta $52                         ; set delay of #$04 frames before drawing sound menu UI

adv_sound_routine_exit:
    inc SOUND_MENU_ROUTINE

run_sound_menu_routine_exit:
    rts

; draws sound menu UI including the names
; input
;  * $52 - delay before drawing next sound menu UI element
;  * $55 - current sound element to draw
sound_menu_routine_02:
    dec $52                         ; decrement delay of #$04 frames before drawing sound menu UI
    bne run_sound_menu_routine_exit ; exit if delay hasn't elapsed
    lda #$01                        ; delay elapsed
    sta $52                         ; set delay of 1 frame between drawing sound menu UI elements
    jsr draw_sound_menu_ui_element  ; draw specific sound menu UI element from sound_menu_ui_elements_tbl
    inc $55                         ; move to next sound menu UI element
    lda $55                         ; load sound menu UI element to draw
    cmp #$0e                        ; see if written all sound elements
    bcc run_sound_menu_routine_exit ; exit if drawn all elements of sound menu
    jsr sound_menu_draw_names       ; draw sound menu names, incorporating scroll
    jmp adv_sound_routine_exit      ; advance sound routine and exit

; checks for input and plays/pauses or scrolls
sound_menu_routine_03:
    jsr handle_medley                 ; decrement medley duration if playing medley of sounds (MEDOLEY)
    jsr check_input_scroll            ; check if up/down on d-pad is pressed accounting for holding down/up
    jsr sound_menu_check_select_sound ; see if b or a is pressed and play or pause the sound respectively
    lda #$21
    sta SPRITE_ATTR                   ; set cursor palette
    lda #$b9                          ; sprite_b9
    sta SPRITES                       ; sound menu cursor
    lda #$34
    sta SPRITE_X_POS
    lda SOUND_MENU_INDEX              ; load selected sound code position from list
    sec                               ; set carry flag in preparation for subtraction
    sbc SOUND_MENU_SCROLL
    asl
    asl
    asl
    .ifdef Probotector
        adc #$9f
    .else
        adc #$a7
    .endif
    sta SPRITE_Y_POS                  ; set cursor position
    rts

; checks for A or B button and plays or pauses the song respectively
; if no button pressed, simply exits
sound_menu_check_select_sound:
    lda CONTROLLER_STATE_DIFF_B        ; read current controller input
    bpl @continue                      ; branch if a button isn't pressed
    jsr sound_menu_init_sound          ; a button pressed, stop playing sound
    jmp load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels

@continue:
    asl
    bmi @play_sound ; branch if b button pressed
    rts

@play_sound:
    jsr sound_menu_init_sound          ; get sound menu variables ready to play sound
    lda SOUND_MENU_INDEX               ; load selected sound code position from list
    cmp #$26                           ; see if if on last sound in sound menu, i.e. MEDOLEY
                                       ; sound medley plays various sound codes for different durations
    beq @init_sound_medley             ; branch if playing sound medley (MEDOLEY)
    cmp #$0e                           ; see if sound effect or music
    bcs @play_sound_menu_sound         ; branch if sound effect
    jsr load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels

@play_sound_menu_sound:
    ldy SOUND_MENU_INDEX       ; load selected sound code position from list
    lda sound_menu_codes_tbl,y ; load selected sound based on index
    jmp play_sound             ; play selected sound

; initializes variables for use for playing a medley of sounds (MEDOLEY)
@init_sound_medley:
    jsr load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels
    lda sound_medley_ptr_tbl
    sta $5c                            ; store medley sound table address low byte
    lda sound_medley_ptr_tbl+1
    sta $5d                            ; store medley sound table address high byte
    lda sound_medley_ptr_tbl+2
    sta X_DRAW_ROUTINE                 ; !(HUH) unused
    lda sound_medley_ptr_tbl+3
    sta SCREEN_SCROLL_TYPE             ; !(HUH) unused
    lda #$01
    sta SOUND_PLAYING_MEDLEY           ; mark that sound medley is playing
    rts

; check if up/down on d-pad is pressed accounting for holding down/up
; limits checking d-pad every frame unless a different direction is pressed
check_input_scroll:
    lda SOUND_PLAYING_MEDLEY    ; load whether or not playing medley of sounds (MEDOLEY)
    bne sound_menu_exit         ; exit if playing medley
    lda $f7
    and #$0c
    beq sound_menu_exit
    ldy #$04
    dec $58                     ; decrement delay between checking for next input
                                ; helps with slowing scroll for holding down d-pad up/down
    beq @check_input            ; branch if delay between input checks have elapsed
                                ; delay between checks hasn't elapsed, check if input is different
                                ; this short-circuits the delay
    lda CONTROLLER_STATE_DIFF_B ; load current controller input
    and #$0c                    ; see if down or up d-pad is pressed
    beq sound_menu_exit         ; exit if no sound selection change
    ldy #$20                    ; reset delay between checking input

@check_input:
    sty $58                    ; set delay until next time check input for non-changing input (holding down d-pad)
    lsr
    lsr
    lsr
    bcc change_sound_selection ; branch if down button not pressed (up button pressed)
    lda SOUND_MENU_INDEX       ; down button pressed, load selected sound code position from list
    cmp #$26                   ; see if if passed last sound in sound menu, i.e. MEDOLEY (see sound_menu_codes_tbl)
    bcs sound_menu_exit        ; exit if selected the last sound and pressed down
    inc SOUND_MENU_INDEX       ; increment selected sound position from list
    lda SOUND_MENU_INDEX       ; load selected sound code position from list
    sec                        ; set carry flag in preparation for subtraction
    sbc SOUND_MENU_SCROLL      ; sound menu scroll offset
    cmp #$07                   ; see if need to scroll down
    bcc sound_menu_exit        ; exit if no need to scroll the sound menu
    inc SOUND_MENU_SCROLL      ; increment sound menu scroll
    jmp sound_menu_draw_names  ; scroll sound menu ui

sound_menu_exit:
    rts

change_sound_selection:
    lsr
    bcc sound_menu_exit
    lda SOUND_MENU_INDEX  ; load selected sound code position from list
    beq sound_menu_exit
    dec SOUND_MENU_INDEX  ; decrement selected sound code position from list
    lda SOUND_MENU_INDEX  ; load selected sound code position from list
    cmp SOUND_MENU_SCROLL
    bcs sound_menu_exit
    dec SOUND_MENU_SCROLL

sound_menu_draw_names:
    lda #$22
    sta $01                    ; set PPU address high byte
    .ifdef Probotector
        lda #$88
    .else
        lda #$a8
    .endif
    sta $00                    ; set PPU address low byte
    lda #$00
    sta $09
    lda SOUND_MENU_SCROLL
    asl
    rol $09
    asl
    rol $09
    asl
    rol $09
    adc sound_name_ptr_tbl
    sta $08
    lda sound_name_ptr_tbl+1
    adc $09
    sta $09
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                   ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x
    inx                        ; increment graphics buffer write offset
    lda #$06                   ; writing 7 sound names to the table
    sta $0a                    ; store sound name count

draw_sound_names:
    ldy #$00                  ; initialize sound name read offset
    lda $01                   ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x ; store in graphics buffer
    inx                       ; increment graphics buffer write offset
    lda $00                   ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x ; store in graphics buffer
    inx                       ; increment graphics buffer write offset
    lda #$08                  ; all sound names are 8 letters
    sta $0b                   ; store count in $0b for loop
    sta CPU_GRAPHICS_BUFFER,x ; write length of graphics block to buffer
    inx                       ; increment graphics write offset

; write sound name to graphics buffer
@sound_name_loop:
    lda ($08),y               ; load next letter in sound name
    sta CPU_GRAPHICS_BUFFER,x ; write to graphics buffer
    inx                       ; increment graphics buffer write offset
    iny                       ; increment sound name read offset
    dec $0b                   ; decrement sound name length count
    bne @sound_name_loop      ; branch if more letters to write
    lda $08                   ; load low byte of sound name pointer
    clc                       ; clear carry in preparation for addition
    adc #$08                  ; move to next sound name
    sta $08                   ; update low byte of sound name pointer
    bcc @continue             ; branch if no overflow
    inc $09                   ; low byte overflow, add 1 to high byte

@continue:
    lda $00
    clc           ; clear carry in preparation for addition
    adc #$20      ; move to next row in PPU
    sta $00       ; update low byte of PPU address
    bcc @next_row ; branch if no overflow
    inc $01       ; overflow, add 1 to high byte of PPU address

@next_row:
    dec $0a                ; decrement remaining number of sound names to draw
    bpl draw_sound_names   ; branch to move to next sound code name if more to draw
    lda #$ff               ; done drawing, going to write #$ff to graphics buffer
    jmp sound_menu_ui_exit ; write #$ff and exit

; sets a row of the nametable to all blank
; clears player select and copyright text with black
; so sound menu can be drawn
; input
;  * $56 - low byte of PPU write address (should be first block of nametable row, e.g. #$a0)
;  * $57 - high byte of PPU write address, e.g. #$23
clear_sound_menu_row:
    ldx GRAPHICS_BUFFER_OFFSET ; load the current graphics buffer offset
    lda #$03                   ; ppuctrl_tbl byte
    sta CPU_GRAPHICS_BUFFER,x  ; set PPUCTRL
    inx                        ; increment graphics buffer write offset
    lda $57                    ; load high byte of PPU write address
    sta CPU_GRAPHICS_BUFFER,x  ; store high byte of PPU write address
    inx                        ; increment graphics buffer write offset
    lda $56                    ; load low byte of PPU write address
    sta CPU_GRAPHICS_BUFFER,x  ; store low byte of PPU write address
    inx                        ; increment graphics buffer write offset
    lda #$20                   ; number of tiles to write (entire nametable row)
    sta CPU_GRAPHICS_BUFFER,x  ; set number of tiles to write
    inx                        ; increment graphics buffer write offset
    lda #$00                   ; pattern table tile index (empty tile)
    sta CPU_GRAPHICS_BUFFER,x  ; set pattern table tile index that'll be drawn #$20 times
    inx                        ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET
    lda $56                    ; load low byte of PPU write address
    sec                        ; set carry flag in preparation for subtraction
    sbc #$20                   ; move to next higher row in the nametable
    sta $56                    ; set next row to clear's low PPU address
    lda $57                    ; load high byte of PPU write address (accounting for underflow)
    sbc #$00                   ; subtract 0 and any carry
    sta $57                    ; update PPU address high byte
    rts

; draws specific sound menu UI element from sound_menu_ui_elements_tbl
; input
;  * $55 - sound menu UI element to draw (offset into sound_menu_ui_elements_tbl)
draw_sound_menu_ui_element:
    lda $55                            ; load sound menu UI element to draw
    asl                                ; double since each entry is a #$02 byte memory address
    tay                                ; transfer to offset register
    lda sound_menu_ui_elements_tbl,y   ; load specific graphics to draw low byte
    sta $08
    lda sound_menu_ui_elements_tbl+1,y ; load specific graphics to draw high byte
    sta $09
    ldx GRAPHICS_BUFFER_OFFSET         ; load graphics buffer offset
    lda #$06                           ; ppuctrl_tbl-1 offset (PPUCTRL = #$28), add 1 VRAM increment (going across)
    sta CPU_GRAPHICS_BUFFER,x          ; set VRAM increment to go across
    inx                                ; increment graphics buffer write offset
    ldy #$00                           ; initialize graphics data read offset

@draw_element:
    lda ($08),y               ; load sound menu ui graphics byte
    cmp #$ff
    beq sound_menu_ui_exit    ; when read #$ff finished writing graphics, exit
    sta CPU_GRAPHICS_BUFFER,x ; write graphics data byte
    inx                       ; increment graphics buffer write offset
    iny                       ; increment graphics data read offset
    bne @draw_element         ; loop to next byte
                              ; iny shouldn't ever get too large when it overflows to #$00, instead loop
                              ; is executed until #$ff is read

sound_menu_ui_exit:
    sta CPU_GRAPHICS_BUFFER,x  ; write #$ff to graphics buffer
    inx                        ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET ; set graphics buffer offset
    rts

; table where each byte is the sound code associated with the sound
; in the sound menu
sound_menu_codes_tbl:
    .byte $28,$2b,$2a,$29,$2c,$2d,$2f,$30,$31,$32,$33,$34,$35,$36,$06,$0a
    .byte $0b,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$18,$19,$1a,$1b,$1c
    .byte $1f,$20,$21,$23,$25,$26,$ff

sound_name_ptr_tbl:
    .addr sound_names_00

sound_names_00:
    .byte $0c,$11,$17,$02,$00,$00,$00,$00 ; BGM1 (sound_28)
    .byte $0c,$11,$17,$03,$00,$00,$00,$00 ; BGM2 (sound_2b)
    .byte $0c,$11,$17,$04,$00,$00,$00,$00 ; BGM3 (sound_2a)
    .byte $0c,$11,$17,$05,$00,$00,$00,$00 ; BGM4 (sound_29)
    .byte $0c,$11,$17,$06,$00,$00,$00,$00 ; BGM5 (sound_2c)
    .byte $0c,$11,$17,$07,$00,$00,$00,$00 ; BGM6 (sound_2d)
    .byte $0c,$11,$17,$08,$00,$00,$00,$00 ; BGM7 (sound_2f)
    .byte $11,$1c,$0f,$0b,$1e,$00,$00,$00 ; GREAT (sound_30)
    .byte $0c,$19,$1d,$1d,$00,$0c,$11,$17 ; BOSS BGM (sound_31)
    .byte $0c,$19,$1d,$1d,$03,$0c,$11,$17 ; BOSS2BGM (sound_32)
    .byte $1a,$00,$0d,$16,$0f,$0b,$1c,$00 ; P CLEAR (sound_33)
    .byte $0b,$00,$0d,$16,$0f,$0b,$1c,$00 ; A CLEAR (sound_34)
    .byte $19,$20,$0f,$1c,$00,$00,$00,$00 ; OVER (sound_35)
    .byte $0f,$18,$0e,$13,$18,$11,$00,$00 ; ENDING (sound_36)
    .byte $1d,$1f,$1e,$0b,$00,$00,$00,$00 ; SUTA (sound_06)
    .byte $1e,$12,$1f,$18,$0e,$0f,$1c,$00 ; THUNDER (sound_0a)
    .byte $1d,$12,$19,$1e,$00,$00,$00,$00 ; SHOT (sound_0b)
    .byte $16,$0b,$1d,$0f,$1c,$00,$00,$00 ; LASER (sound_0d)
    .byte $1d,$1a,$1c,$0f,$0b,$0e,$00,$00 ; SPREAD (sound_0e)
    .byte $10,$13,$1c,$0f,$00,$00,$00,$00 ; FIRE (sound_0f)
    .byte $0c,$00,$1d,$12,$19,$1e,$00,$00 ; B SHOT (sound_10)
    .byte $1e,$00,$0e,$0b,$17,$0f,$11,$0f ; T DAMEGE (sound_11)
    .byte $12,$0b,$1c,$0f,$1e,$1d,$1f,$00 ; HARETSU (sound_12)
    .byte $1e,$00,$19,$1f,$1e,$00,$00,$00 ; T OUT (sound_13)
    .byte $24,$00,$19,$1f,$1e,$00,$00,$00 ; Z OUT (sound_14)
    .byte $0b,$00,$19,$1f,$1e,$00,$00,$00 ; A OUT (sound_15)
    .byte $1c,$19,$16,$16,$00,$00,$00,$00 ; ROLL (sound_16)
    .byte $0b,$1a,$1a,$0f,$0b,$1c,$00,$00 ; APPEAR (sound_18)
    .byte $1a,$19,$21,$0f,$1c,$00,$00,$00 ; POWER (sound_19)
    .byte $0c,$19,$1d,$1d,$00,$0c,$15,$00 ; BOSS BK (sound_1a)
    .byte $0c,$0b,$15,$1f,$12,$0b,$02,$00 ; BAKUHA1 (sound_1b)
    .byte $0c,$0b,$15,$1f,$12,$0b,$03,$00 ; BAKUHA2 (sound_1c)
    .byte $0b,$1c,$1f,$15,$1f,$00,$00,$00 ; ARUKU (sound_1f)
    .byte $14,$13,$21,$0b,$1c,$0f,$00,$00 ; JIWARE (sound_20)
    .byte $1d,$13,$16,$0f,$18,$00,$00,$00 ; SILEN (sound_21)
    .byte $1a,$00,$02,$1f,$1a,$00,$00,$00 ; P 1UP (sound_23)
    .byte $1a,$00,$19,$1f,$1e,$00,$00,$00 ; P OUT (sound_25)
    .byte $0c,$00,$19,$1f,$1e,$00,$00,$00 ; B OUT (sound_26)
    .byte $17,$0f,$0e,$19,$16,$0f,$23,$00 ; MEDOLEY (see sound_medley_00)

sound_menu_ui_elements_tbl:
    .addr sound_menu_ui_elements_00     ; sound menu palette (attribute table data)
    .addr sound_menu_ui_elements_01     ; sound mode box top line
    .addr sound_menu_ui_elements_02     ; |SOUND MODE|
    .addr sound_menu_ui_elements_03     ; sound mode box bottom line
    .ifdef Superc
        .addr sound_menu_ui_elements_04
    .endif
    .addr sound_menu_ui_elements_05     ; sound menu box top line
    .addr sound_menu_ui_elements_06     ; |                   |
    .addr sound_menu_ui_elements_07     ; |          B...SOUND|
    .addr sound_menu_ui_elements_08     ; |              START|
    .addr sound_menu_ui_elements_09     ; |                   |
    .addr sound_menu_ui_elements_0a     ; |          A...SOUND|
    .addr sound_menu_ui_elements_0b     ; |               OFF |
    .addr sound_menu_ui_elements_0c     ; |                   |
    .ifdef Probotector
        .byte $29,$9f
    .endif
    .addr sound_menu_ui_elements_0d     ; sound menu box bottom line
    .ifdef Probotector
        .byte $29,$9f
    .endif

; sound menu palette (attribute table data)
sound_menu_ui_elements_00:
    .byte $23,$e0
    .byte $20
    .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
    .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
    .byte $ff

; sound mode box top line
sound_menu_ui_elements_01:
    .byte $22,$0a                                         ; PPU address
    .byte $0c                                             ; length of tiles is #$0c
    .byte $28,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$29
    .byte $ff                                             ; end

; |SOUND MODE|
sound_menu_ui_elements_02:
    .byte $22,$2a                                         ; PPU address
    .byte $0c                                             ; length of tiles is #$0c
    .byte $2d,$1d,$19,$1f,$18,$0e,$00,$17,$19,$0e,$0f,$2d ; |SOUND MODE|
    .byte $ff                                             ; end

; sound mode box bottom line
sound_menu_ui_elements_03:
    .byte $22,$4a                                         ; PPU address
    .byte $0c                                             ; length of tiles is #$0c
    .byte $2a,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2b
    .byte $ff                                             ; end

sound_menu_ui_elements_04:
    .ifdef Superc
        .byte $22,$6a ; PPU address
        .byte $01     ; length of tiles is #$01
        .byte $00
        .byte $ff     ; end
    .endif

; sound menu box top line
sound_menu_ui_elements_05:
.ifdef Probotector
    .byte $22,$65                                                         ; PPU address
.else
    .byte $22,$85                                                         ; PPU address
.endif
    .byte $16                                                             ; length of tiles is #$16
    .byte $28,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c
    .byte $2c,$2c,$2c,$2c,$2c,$29
    .byte $ff                                                             ; end

; |                   |
sound_menu_ui_elements_06:
    .ifdef Probotector
        .byte $22,$85  ; PPU address
    .else
        .byte $22,$a5  ; PPU address
    .endif
    .byte $01          ; length of tiles is 1
    .byte $2d          ; |
    .ifdef Probotector
        .byte $22,$9a  ; PPU address
    .else
        .byte $22,$ba  ; PPU address
    .endif
    .byte $01          ; length of tiles is 1
    .byte $2d          ; |
    .byte $ff          ; end

; |          B...SOUND|
sound_menu_ui_elements_07:
    .ifdef Probotector
        .byte $22,$a5                             ; PPU address
    .else
        .byte $22,$c5                             ; PPU address
    .endif
    .byte $01                                     ; length of tiles
    .byte $2d                                     ; |
    .ifdef Probotector
        .byte $22,$b1                             ; PPU address
    .else
        .byte $22,$d1                             ; PPU address
    .endif
    .byte $0a                                     ; length of tiles
    .byte $0c,$25,$25,$25,$1d,$19,$1f,$18,$0e,$2d ; B...SOUND|
    .byte $ff                                     ; end

; |              START|
sound_menu_ui_elements_08:
    .ifdef Probotector
        .byte $22,$c5             ; PPU address
    .else
        .byte $22,$e5             ; PPU address
    .endif
    .byte $01                     ; length of tiles
    .byte $2d                     ; |
    .ifdef Probotector
        .byte $22,$d5             ; PPU address
    .else
        .byte $22,$f5             ; PPU address
    .endif
    .byte $06                     ; length of tiles
    .byte $1d,$1e,$0b,$1c,$1e,$2d ; START|
    .byte $ff

; |                   |
sound_menu_ui_elements_09:
    .ifdef Probotector
        .byte $22,$e5  ; PPU address
    .else
        .byte $23,$05  ; PPU address
    .endif
    .byte $01          ; length of tiles
    .byte $2d          ; |
    .ifdef Probotector
        .byte $22,$fa  ; PPU address
    .else
        .byte $23,$1a  ; PPU address
    .endif
    .byte $01          ; length of tiles
    .byte $2d          ; |
    .byte $ff

; |          A...SOUND|
sound_menu_ui_elements_0a:
    .ifdef Probotector
        .byte $23,$05                             ; PPU address
    .else
        .byte $23,$25                             ; PPU address
    .endif
    .byte $01                                     ; length of tiles
    .byte $2d                                     ; |
    .ifdef Probotector
        .byte $23,$11                             ; PPU address
    .else
        .byte $23,$31                             ; PPU address
    .endif
    .byte $0a                                     ; length of tiles
    .byte $0b,$25,$25,$25,$1d,$19,$1f,$18,$0e,$2d ; A...SOUND|
    .byte $ff

; |               OFF |
sound_menu_ui_elements_0b:
    .ifdef Probotector
        .byte $23,$25         ; PPU address
    .else
        .byte $23,$45         ; PPU address
    .endif
    .byte $01                 ; length of tiles
    .byte $2d                 ; |
    .ifdef Probotector
        .byte $23,$36         ; PPU address
    .else
        .byte $23,$56         ; PPU address
    .endif
    .byte $05                 ; length of tiles
    .byte $19,$10,$10,$00,$2d ; OFF |
    .byte $ff

; |                   |
sound_menu_ui_elements_0c:
    .ifdef Probotector
        .byte $23,$45  ; PPU address
    .else
        .byte $23,$65  ; PPU address
    .endif
    .byte $01          ; length of tiles
    .byte $2d          ; |
    .ifdef Probotector
        .byte $23,$5a  ; PPU address
    .else
        .byte $23,$7a  ; PPU address
    .endif
    .byte $01          ; length of tiles
    .byte $2d          ; |
    .byte $ff

; sound menu box bottom line
sound_menu_ui_elements_0d:
    .ifdef Probotector
        .byte $23,$65                                         ; PPU address
    .else
        .byte $23,$85                                         ; PPU address
    .endif
    .byte $16
    .byte $2a,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c
    .byte $2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2b,$ff

; decrement medley duration if playing medley of sounds (MEDOLEY)
handle_medley:
    lda SOUND_PLAYING_MEDLEY ; is playing medley of sounds (MEDOLEY)
    beq @no_medley_exit      ; exit if not playing medley of sounds (MEDOLEY)
    jsr @dec_medley_duration

@no_medley_exit:
    rts

@dec_medley_duration:
    lda SOUND_MEDLEY_TIMER  ; load sound medley duration timer part 1
    ora $5b                 ; merge with sound medley duration timer part 1
    beq change_medley_sound ; if both timers elapsed, play next sound in medley
    dec SOUND_MEDLEY_TIMER  ; decrement timer part 1
    bne @exit               ; exit if timer part 1 is non-zero
    dec $5b                 ; decrement timer part 2

@exit:
    rts

; when playing medley (MEDOLEY), multiple sounds are played in a sequence
; this label changes to the next sound in the medley
change_medley_sound:
    ldy #$00
    lda ($5c),y                        ; read sound medley byte
    cmp #$ff
    beq sound_menu_init_sound
    sta $09
    and #$3f                           ; !(HUH) not sure why needed, bits 6 and 7 are always 0
    sta $5b
    iny
    lda ($5c),y
    sta SOUND_MEDLEY_TIMER
    iny
    lda ($5c),y
    sta $08
    lda $09
    asl
    bmi play_sound_menu_sound
    jsr load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels

play_sound_menu_sound:
    lda $08               ; load sound code
    jsr play_sound        ; play selected sound code from the sound menu
    jmp @adv_sound_medley ; !(HUH) useless jump

@adv_sound_medley:
    lda $5c   ; load medley sound table address low byte
    clc       ; clear carry in preparation for addition
    adc #$03  ; move to next sound medley in sound_medley_00
    sta $5c   ; store medley sound table address low byte
    bcc @exit ; branch if no overflow
    inc $5d   ; overflow occurred, add 1 to medley sound table address high byte

@exit:
    rts

; used before playing sound and when stopping playing a sound in sound menu
sound_menu_init_sound:
    lda #$00
    sta SOUND_PLAYING_MEDLEY     ; clear playing medley of sounds (MEDOLEY) bit
    sta SOUND_MEDLEY_TIMER       ; clear sound medley duration timer part 1
    sta $5b                      ; clear sound medley duration timer part 2
    sta UNUSED_SOUND_MENU_FLAG
    sta UNUSED_SOUND_MENU_FLAG+1
    rts

; table defining the order and duration of sounds that are played for the medley (MEDOLEY)
sound_medley_ptr_tbl:
    .addr sound_medley_00
    .addr sound_medley_01 ; !(HUH) unused

; byte 1 and 0 are both used together to determine duration of sound
; byte 0 - part 1 of length of sound ($5b)
; byte 1 - part 2 of length of sound ($5a)
; byte 2 - sound code to play
sound_medley_00:
    .byte $0e,$5c,$28 ; play sound_28 for #$d5c frames (57 seconds)
    .byte $0d,$e4,$30 ; play sound_30 for #$ce4 frames (55 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $09,$34,$2b ; play sound_2b for #$834 frames (35 seconds)
    .byte $06,$dc,$31 ; play sound_31 for #$5dc frames (25 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $0c,$40,$2a ; play sound_2a for #$b40 frames (48 seconds)
    .byte $08,$08,$32 ; play sound_32 for #$708 frames (30 seconds)
    .byte $06,$dc,$31 ; play sound_31 for #$5dc frames (25 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $0c,$b8,$29 ; play sound_29 for #$bb8 frames (50 seconds)
    .byte $06,$dc,$31 ; play sound_31 for #$5dc frames (25 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $09,$34,$2c ; play sound_2c for #$834 frames (35 seconds)
    .byte $06,$dc,$31 ; play sound_31 for #$5dc frames (25 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $09,$ac,$2d ; play sound_2d for #$8ac frames (37 seconds)
    .byte $08,$08,$32 ; play sound_32 for #$708 frames (30 seconds)
    .byte $0d,$e4,$30 ; play sound_30 for #$ce4 frames (55 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $0c,$b8,$2e ; play sound_2e for #$bb8 frames (50 seconds)
    .byte $06,$dc,$31 ; play sound_31 for #$5dc frames (25 seconds)
    .byte $02,$01,$33 ; play sound_33 for #$101 frames (4 seconds)
    .byte $0f,$10,$2f ; play sound_2f for #$e10 frames (60 seconds)
    .byte $0d,$e4,$30 ; play sound_30 for #$ce4 frames (55 seconds)
    .byte $02,$68,$34 ; play sound_34 for #$168 frames (6 seconds)
    .byte $0c,$62,$36 ; play sound_36 for #$b62 frames (48 seconds)
    .byte $01,$f0,$35 ; play sound_35 for #$f0 frames (4 seconds)
    .byte $ff

; !(UNUSED)
sound_medley_01:
    .byte $ff

; offset is based on DRAW_Y_SCREEN * LEVEL_WIDTH
; first two bytes are LEVEL_WIDTH and LEVEL_HEIGHT
; each byte specifies an offset into level_1_supertiles_screen_ptr_table,
; e.g. $0b -> level_1_supertiles_screen_0b
level_1_screen_layout_tbl:
    .byte $0e                                                     ; LEVEL_WIDTH
    .byte $04                                                     ; LEVEL_HEIGHT
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$09,$0a,$0e,$0b,$0f,$10,$11
    .byte $01,$12,$00,$09,$0a,$0b,$0c,$05,$06,$0d,$07,$08,$00,$00
    .byte $02,$03,$04,$05,$06,$07,$08,$00,$00,$00,$00,$00,$00,$00

level_1_supertiles_screen_ptr_table:
    .addr level_1_supertiles_screen_00
    .addr level_1_supertiles_screen_01
    .addr level_1_supertiles_screen_02
    .addr level_1_supertiles_screen_03
    .addr level_1_supertiles_screen_04
    .addr level_1_supertiles_screen_05
    .addr level_1_supertiles_screen_06 ; continues in bank 5

; end of bank
; unused #$0 bytes out of #$2,000 bytes total (100% full)
; unused 0 bytes out of 8,192 bytes total (100% full)
bank_4_unused_space: