; Bank 8 contains Level 5 (The Cliff) and Level 6 (Entry to HQ) enemies.
; Then, Bank 8 contains Level 5 screen layout and the start of the table of
; supertiles per screen for Level 5.  That data is continued in Bank 9.
; Enemies in this bank:
; * Enemy Type #$2f - Falling Rock
; * Enemy Type #$33 - Turret Metal Bullet
; * Enemy Type #$34 - Jet Pack Soldier
; * Enemy Type #$58 - Storage Room Soldier Generator
; * Enemy Type #$5a - Alien Skull
; * Enemy Type #$35 - Red Poisonous Insect Gel
; * Enemy Type #$59 - Krypto-Crustacean
; * Enemy Type #$5d - Mouth Pit
; * Enemy Type #$5e - Mouth Pit Generator
; * Enemy Type #$5f - Big Faced One-Eyed Monster
; * Enemy Type #$24 - Alien Ladybug
; * Enemy Type #$60 - Baby Alien Ladybug
; * Enemy Type #$61 - Boss Screen Baby Alien Ladybug
; * Enemy Type #$70 - Suspicious Face Arm
; * Enemy Type #$71 - Area 6 Tile Swapper
; * Enemy Type #$65 - Jagger Froid
; * Enemy Type #$68 - Jagger Froid Projectile
; * Enemy Type #$64 - Suspicious Face
; * Enemy Type #$66 - Alien Serpent

; 8 KiB PRG ROM
.segment "BANK8"

.include "constants.asm"

; import from bank 3
.import enemy_explosion_routine_00
.import enemy_explosion_routine_01
.import enemy_explosion_routine_03
.import set_delay_adv_enemy_routine
.import flip_enemy_y_dir
.import player_enemy_y_dist
.import set_enemy_animation_sprite
.import modify_enemy_x_vel
.import advance_enemy_routine
.import apply_velocity
.import update_enemy_pos
.import set_y_pos_for_bg
.import clear_enemy_x_vel
.import get_bg_collision_code
.import clear_enemy_y_vel
.import get_enemy_bg_collision_code
.import set_enemy_hp_from_a_and_y
.import load_banks_update_enemy_supertiles
.import remove_enemy
.import try_create_enemy_from_existing
.import update_enemy_nametable_supertile
.import set_enemy_hp
.import backup_scroll_and_ppuctrl
.import set_enemy_hp_hard
.import modify_enemy_y_vel
.import bg_boss_apply_vel
.import init_bg_boss_pre_irq_max_scrolls
.import bg_enemy_set_scrolls
.import clear_bg_collision_data
.import add_a_to_enemy_y_fract_vel
.import get_falling_enemy_bg_collision_code
.import player_enemy_x_dist
.import set_vel_to_target_player
.import copy_enemy_vars_to_zp
.import set_enemy_routine
.import apply_x_velocity
.import copy_enemy_vars
.import bg_enemy_explosion_routine_01
.import enemy_routine_boss_defeated_00
.import enemy_routine_boss_defeated_01
.import bg_enemy_explosion_routine_00
.import get_bg_collision
.import hone_to_player_set_enemy_vel
.import set_boss_defeated_remove_enemy
.import earthquake_shake
.import set_destroyed_enemy_routine
.import set_enemy_vel_from_aim_dir
.import restore_scroll_and_ppuctrl

; import from bank 9
.import level_5_supertiles_screen_07
.import level_5_supertiles_screen_08
.import level_5_supertiles_screen_09
.import level_5_supertiles_screen_0a
.import level_5_supertiles_screen_0b
.import level_5_supertiles_screen_0c
.import level_5_supertiles_screen_0d
.import level_5_supertiles_screen_0e
.import level_5_supertiles_screen_0f
.import level_5_supertiles_screen_10
.import level_5_supertiles_screen_11

; import from bank f
.import fire_bullet_at_player
.import reset_draw_point
.import init_irq_scroll
.import run_routine_from_tbl_below
.import play_sound
.import get_rotate_01
.import convert_dir_to_overhead_dir
.import overhead_quad_aim_dir_01
.import set_enemy_collision_and_type
.import set_nmi_noop_irq
.import create_and_init_enemy
.import fire_near_player
.import set_level_palette
.import dir_to_overhead_dir_tbl
.import reset_scroll_draw_point

; export for bank 3
.export jet_pack_soldier_routine_ptr_tbl
.export falling_rock_routine_ptr_tbl
.export storage_bay_routine_ptr_tbl
.export metal_covered_turret_routine_ptr_tb
.export metal_bullet_routine_ptr_tbl
.export krypto_crustacean_routine_ptr_tbl
.export alien_skull_routine_ptr_tbl
.export red_blob_routine_ptr_tbl
.export alien_ladybug_routine_ptr_tbl
.export mouth_pit_routine_ptr_tbl
.export mouth_pit_gen_routine_ptr_tbl
.export big_face_routine_ptr_tbl
.export baby_alien_ladybug_routine_ptr_tbl
.export area_6_chr_swap_routine_ptr_tbl
.export suspicious_face_arm_routine_ptr_tbl
.export suspicious_face_routine_ptr_tbl
.export boss_baby_alien_ladybug_routine_ptr_tbl
.export jagger_froid_routine_ptr_tbl
.export alien_serpent_routine_ptr_tbl
.export jagger_froid_projectile_routine_ptr_tbl

; export for bank f
.export level_5_supertiles_screen_ptr_table
.export level_5_screen_layout_tbl

.byte $38 ; bank byte

; enemy type #$2f
falling_rock_routine_ptr_tbl:
    .addr falling_rock_routine_00    ; set HP, sprite, sprite palette, and position
    .addr falling_rock_routine_01    ; apply velocity depending on bg collision
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set HP, sprite, sprite palette, and position
falling_rock_routine_00:
    lda #$05                       ; set base adjustment ENEMY_HP to #$05
    ldy #$06                       ; set enemy_hp_adjust_tbl offset to #$06
    jsr set_enemy_hp_from_a_and_y  ; set ENEMY_HP calculated using medium HP difficulty (adjusted by ENEMY_DIFFICULTY)
                                   ; sets enemy HP = #$05, #$09, or #$0d
    lda #$03
    sta ENEMY_SPRITE_ATTR,x        ; set sprite palette
    jsr update_enemy_pos           ; adjust position based on scroll (does not apply velocity)
    ldy #$08                       ; animation index for falling rock
    jsr set_enemy_animation_sprite ; animate sprite for falling rock based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jmp advance_enemy_routine      ; advance to next routine

; apply velocity depending on bg collision
falling_rock_routine_01:
    lda ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    ora ENEMY_X_VELOCITY_FRACT,x
    beq @continue                ; branch if no X velocity to skip animation update
    ldy #$08                     ; animation index for falling rock
    lda ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    bmi @set_animation_sprite    ; branch if moving left
    iny                          ; moving right, set falling rock animation sequence

@set_animation_sprite:
    jsr set_enemy_animation_sprite ; animate sprite for falling rock based on ENEMY_FRAME and ENEMY_ANIM_DELAY

@continue:
    lda ENEMY_VAR_1,x               ; load previous frame background collision
    sta $17                         ; backup in $17
    lda #$10                        ; add #$10 to enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    sta ENEMY_VAR_1,x               ; store bg collision type
    beq @set_vel                    ; branch if no background collision
    jsr set_y_pos_for_bg            ; set enemy Y position based on background collision
    lda $17                         ; restore previous frame background collision
    bne @set_vel
    lda ENEMY_Y_VELOCITY_FAST,x     ; load enemy's fast Y velocity
    beq @set_vel                    ; branch if falling rock is not falling
    jsr clear_enemy_x_vel           ; set enemy X velocity (fast and fractional) to 0

@set_vel:
    lda ENEMY_Y_VELOCITY_FRACT,x ; load enemy's fractional Y velocity
    clc                          ; clear carry in preparation for addition
    adc #$20                     ; apply gravity
    sta ENEMY_Y_VELOCITY_FRACT,x ; apply gravity to falling Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    adc #$00                     ; adding any overflow from the fractional velocity addition
    sta ENEMY_Y_VELOCITY_FAST,x  ; set fast falling Y velocity
    ldy ENEMY_Y_POS,x            ; load enemy's Y position
    lda ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    asl                          ; push direction into carry (0 = right, 1 = left)
    lda #$10                     ; assume falling right, load amount to add to X velocity
    bcc @set_x_vel               ; branch if rock is falling to the right (positive X velocity)
    lda #$f0                     ; rocking is falling left (negative X velocity), add negative X velocity

@set_x_vel:
    clc                       ; clear carry in preparation for addition
    adc ENEMY_X_POS,x         ; add to rock's X position
    jsr get_bg_collision_code ; determine background collision at point (a,y)
    cmp #$02                  ; see if rock landed on flat surface
    bne @check_incline        ; branch if didn't land on flat surface
    jsr clear_enemy_x_vel     ; landed on flat surface
                              ; set enemy X velocity (fast and fractional) to 0

@check_incline:
    lda ENEMY_VAR_1,x         ; load bg collision
    beq @apply_vel_exit
    cmp #$06                  ; see if falling rock on incline (start)
    bcc @clear_y_vel          ; branch if not on an incline
                              ; (water, land, flat floating platform, no collision)
    ldy #$00                  ; assume on positive incline
    cmp #$09                  ; see if falling rock on positive incline (middle portion)
    bcc @incline_update_x_vel ; branch if on positive incline
    ldy #$02                  ; on negative incline, use offset for negative X velocity

@incline_update_x_vel:
    lda ENEMY_X_VELOCITY_FRACT,x   ; load fractional X velocity
    clc                            ; clear carry in preparation for addition
    adc falling_rock_x_acc_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x   ; store enemy's fractional X velocity
    lda ENEMY_X_VELOCITY_FAST,x    ; load enemy's fast X velocity
    adc falling_rock_x_acc_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x    ; store enemy's fast X velocity

@clear_y_vel:
    jsr clear_enemy_y_vel ; set enemy Y velocity to 0

@apply_vel_exit:
    jmp apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

; x acceleration of falling rock
falling_rock_x_acc_tbl:
    .byte $fd,$ff ; -0.01171875
    .byte $03,$00 ;  0.01171875

metal_covered_turret_routine_ptr_tb:
    .addr metal_covered_turret_routine_00 ; initialize collision, hp, sprite, and position
    .addr metal_covered_turret_routine_01 ; wait for activation
    .addr metal_covered_turret_routine_02 ; animate activation
    .addr metal_covered_turret_routine_03 ; fire single bullet every #$50 frames
    .addr metal_covered_turret_routine_04 ; enemy destroyed routine, set supertile and run explosion (enemy_explosion_routine_00)
    .addr enemy_explosion_routine_01      ; animate explosion sequence
    .addr enemy_explosion_routine_03      ; mark destroyed, remove enemy

; initialize collision, hp, sprite, and position
metal_covered_turret_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$08                  ; HP = #$08, #$0c, or #$0f
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite (uses bg tiles instead)
    lda ENEMY_X_POS,x         ; load enemy's X position
    sec                       ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_X_POS,x         ; offset enemy X position by -8
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    sec                       ; set carry flag in preparation for subtraction
    sbc #$20
    sta ENEMY_Y_POS,x         ; offset enemy Y position by #$20
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for activation
metal_covered_turret_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x               ; load enemy's X position
    cmp #$f0
    bcs metal_covered_turret_exit   ; exit if enemy is in rightmost ~94% of screen
    jsr compare_y_dist_to_20        ; compare the y distance between the closest player (y-axis) and the enemy to #$20
    bcs metal_covered_turret_exit   ; branch if enemy is more than (or equal to) #$20 pixels above/below the enemy
    lda #$01                        ; player is vertically close to enemy advance routine
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to metal_covered_turret_routine_02

; animate activation
metal_covered_turret_routine_02:
    jsr update_enemy_pos                   ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne metal_covered_turret_exit          ; exit if activation animation delay hasn't elapsed
    jsr metal_covered_turret_set_supertile ; set supertile based on ENEMY_FRAME
    lda #$01
    bcs @set_delay_exit                    ; branch if unable set supertile to try again next frame
    inc ENEMY_FRAME,x                      ; increment frame for next animation
    lda ENEMY_FRAME,x                      ; load animation frame
    cmp #$02                               ; see if past last frame in the animation
    lda #$0c                               ; assume more frames to draw, set delay to #$0c
    bcc @set_delay_exit                    ; branch if more frames to draw to set delay
    lda #$01
    sta ENEMY_DESTROY_ATTRS,x              ; disable player enemy collision
    lda #$30
    jmp set_delay_adv_enemy_routine        ; set delay to #$30 and set routine to metal_covered_turret_routine_03

@set_delay_exit:
    sta ENEMY_DELAY,x

metal_covered_turret_exit:
    rts

; fire single bullet every #$50 frames
metal_covered_turret_routine_03:
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x    ; load enemy's X position
    cmp #$20             ; compare enemy position to leftmost 12.5% of screen
    bcs @continue        ; branch if not too far to the left
    jmp remove_enemy     ; enemy in leftmost 12.5%, remove

@continue:
    dec ENEMY_DELAY,x                           ; decrement firing delay
    bne metal_covered_turret_exit               ; exit if firing delay hasn't elapsed
    lda #$50
    sta ENEMY_DELAY,x                           ; set next bullet firing delay
    ldy #$33                                    ; enemy type = turret metal bullet
    jsr try_create_enemy_from_existing          ; create turret metal bullet
    bcc metal_covered_turret_exit               ; branch if unable to create metal bullet
    lda ENEMY_ATTRIBUTES,x
    and #$01                                    ; keep turret facing direction
    tay                                         ; transfer to offset register
    lda metal_covered_turret_bullet_x_adj_tbl,y ; load initial offset based on firing direction
    ldy $11                                     ; load created bullet slot
    clc                                         ; clear carry in preparation for addition
    adc ENEMY_X_POS,x
    sta ENEMY_X_POS,y                           ; adjust X position based on aiming direction
    rts

metal_covered_turret_bullet_x_adj_tbl:
    .byte $f4 ; left = -12
    .byte $0c ; right = 12

; enemy destroyed routine, set supertile and run explosion (enemy_explosion_routine_00)
metal_covered_turret_routine_04:
    jsr update_enemy_pos                     ; adjust position based on scroll (does not apply velocity)
    ldy #$02                                 ; set enemy destroyed supertile offset
    jsr metal_covered_turret_set_supertile_y ; set destroyed supertile
    bcs metal_covered_turret_exit            ; branch if unable to update supertile to try again next frame
    jmp enemy_explosion_routine_00           ; updated supertile, set empty sprite, play optional enemy destroyed sound, disable collisions

metal_covered_turret_set_supertile:
    ldy ENEMY_FRAME,x

metal_covered_turret_set_supertile_y:
    lda ENEMY_ATTRIBUTES,x
    lsr                    ; move aim direction to carry
    bcc @continue          ; branch if facing left
    iny                    ; facing right, increase supertile offset
    iny
    iny

@continue:
    lda metal_covered_turret_supertile_tbl,y
    jmp update_enemy_nametable_supertile     ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)

; level_5_supertile_data offset
metal_covered_turret_supertile_tbl:
    .byte $88,$89,$87 ; facing left
    .byte $8a,$8b,$8c ; facing right

; enemy type #$33
metal_bullet_routine_ptr_tbl:
    .addr metal_bullet_routine_00 ; set X velocity based on attributes
    .addr metal_bullet_routine_01 ; set sprite, apply velocity, check for bg collision
    .addr remove_enemy            ; enemy destroyed routine

; set X velocity based on attributes
metal_bullet_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x      ; mark metal bullet so other bullets travel through it
    jsr update_enemy_pos           ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ATTRIBUTES,x
    and #$01                       ; strip to just bullet velocity index
    asl                            ; double since each entry is #$02 bytes
    tay                            ; transfer to offset register
    lda metal_bullet_x_vel_tbl,y   ; load X fractional velocity
    sta ENEMY_X_VELOCITY_FRACT,x   ; set X fractional velocity
    lda metal_bullet_x_vel_tbl+1,y ; load X fast velocity
    sta ENEMY_X_VELOCITY_FAST,x    ; set X fast velocity
    jmp advance_enemy_routine      ; advance to next routine

metal_bullet_x_vel_tbl:
    .byte $00,$fc ; -4 pixels/frame
    .byte $00,$04 ;  4 pixels/frame

; set sprite, apply velocity, check for bg collision
metal_bullet_routine_01:
    lda #$06
    sta ENEMY_SPRITE,x              ; sprite_06
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$00                        ; don't adjust enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    beq @exit                       ; exit if no background collision
    jmp remove_enemy                ; remove enemy if collided with background

@exit:
    rts

; enemy type #$34
jet_pack_soldier_routine_ptr_tbl:
    .addr jet_pack_soldier_routine_00 ; (ascending) set initial position, ascending Y velocity, and initial firing delay
    .addr jet_pack_soldier_routine_01 ; (ascending, firing) set sprite based, fire if delay elapsed, ascend until at top, then descend and advance routine
    .addr jet_pack_soldier_routine_02 ; set velocity, fire if delay elapsed, ascend if descending too close to bottom
    .addr enemy_explosion_routine_00  ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01  ; animate explosion sequence
    .addr enemy_explosion_routine_03  ; mark destroyed, remove enemy

; set initial position, Y velocity, and initial firing delay
jet_pack_soldier_routine_00:
    lda #$c8
    sta ENEMY_Y_POS,x               ; set initial Y position to #$c8
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    ldy #$00                        ; rising jet pack soldier
    jsr jet_pack_soldier_set_y_vel  ; set jet pack soldier's Y velocity to be rising
    lda #$60                        ; initial firing delay is #$60
    jmp set_delay_adv_enemy_routine ; set firing delay to #$60 and set routine to jet_pack_soldier_routine_01

; input
;  * y - initial velocity direction (#$00 = rising, #$02 = falling)
jet_pack_soldier_set_y_vel:
    lda jet_pack_soldier_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x       ; store enemy's fractional Y velocity
    lda jet_pack_soldier_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x        ; store enemy's fast Y velocity
    rts

jet_pack_soldier_y_vel_tbl:
    .byte $00,$ff ; -1 (rising)
    .byte $00,$01 ;  1 (falling)

; set sprite based on closest player, fire if delay elapsed, ascend until at top, then descend and advance routine
jet_pack_soldier_routine_01:
    jsr jet_pack_soldier_set_sprite      ; set sprite and sprite attribute based on ENEMY_FRAME and closest player
    jsr apply_velocity                   ; apply enemy's velocity to its position, removing enemy if off-screen
    jsr jet_pack_soldier_fire_if_elapsed ; decrement firing delay and if elapsed fire at player index in ENEMY_VAR_1,x
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    cmp #$20
    bcs jet_pack_soldier_exit            ; exit if jet pack is in the bottom 87.5% of the screen (not in the very top)
    ldy #$02                             ; enemy is in top of screen, begin descending
    jsr jet_pack_soldier_set_y_vel       ; set Y velocity so enemy is falling/descending
    lda #$90
    sta ENEMY_VAR_2,x
    jmp advance_enemy_routine            ; advance to next routine

jet_pack_soldier_exit:
    rts

; set velocity, fire if delay elapsed, ascend if descending too close to bottom
jet_pack_soldier_routine_02:
    jsr jet_pack_soldier_set_sprite      ; set sprite and sprite attribute based on ENEMY_FRAME and closest player
    ldy #$fc                             ; push 4 1s to $01 high bits
    jsr modify_enemy_x_vel               ; adjust X velocity based on ENEMY_X_POS,x distance from ENEMY_VAR_2,x
    jsr apply_velocity                   ; apply enemy's velocity to its position, removing enemy if off-screen
    jsr jet_pack_soldier_fire_if_elapsed ; decrement firing delay and if elapsed fire at player index in ENEMY_VAR_1,x
    lda ENEMY_Y_VELOCITY_FAST,x          ; load enemy's fast Y velocity
    bmi jet_pack_soldier_exit            ; exit if ascending
    lda ENEMY_Y_POS,x                    ; descending, load enemy's Y position
    cmp #$d0                             ; see if in bottom ~19% of screen
    bcc jet_pack_soldier_exit            ; exit if not in bottom of screen
    jmp flip_enemy_y_dir                 ; at very bottom of screen, start ascending
                                         ; flip the sign of the enemy's Y velocity, e.g. 1.25 becomes -1.25

; set jet pack soldier sprite and sprite attribute based on ENEMY_FRAME and closest player
; input
;  * x - enemy slot index
jet_pack_soldier_set_sprite:
    ldy #$0c                       ; animation index for jetpack soldier
    jsr set_enemy_animation_sprite ; animate sprite for jetpack soldier based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr player_enemy_y_dist        ; a = closest y distance to enemy from players, y = closest player (#$00 or #$01)
    tya                            ; transfer closest player index to a
    sta ENEMY_VAR_1,x              ; set to player index to target
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y
    lda #$41                       ; assume player to left of enemy and flip sprite horizontally
    bcc @continue                  ; branch if player to left of enemy
    lda #$01                       ; player to right of enemy, do not flip sprite horizontally

@continue:
    sta ENEMY_SPRITE_ATTR,x ; set sprite attribute, including facing direction
    rts

; decrement firing delay and if delay elapsed fire at player index in ENEMY_VAR_1,x
; input
;  * x - enemy slot index
jet_pack_soldier_fire_if_elapsed:
    dec ENEMY_DELAY,x       ; decrement firing delay
    bne @exit               ; exit if firing delay hasn't elapsed
    lda #$67
    sta ENEMY_DELAY,x       ; set next firing delay
    lda ENEMY_Y_POS,x       ; load enemy's Y position
    sec                     ; set carry flag in preparation for subtraction
    sbc #$10
    sta $08                 ; set bullet initial Y position to #$10 pixels higher than enemy position
    lda ENEMY_SPRITE_ATTR,x ; load sprite attribute to see which direction enemy is facing
    asl
    asl                     ; push horizontal flip bit to the carry flag
    lda #$0a                ; assume facing right, adding #$0a to Y position
    bcs @continue           ; branch if facing right
    lda #$f6                ; subtracting #$0a from enemy Y position

@continue:
    clc                       ; clear carry in preparation for addition
    adc ENEMY_X_POS,x         ; add/subtract #$0a to/from enemy X position
    sta $09                   ; set bullet initial X position
    lda ENEMY_VAR_1,x         ; load player index of player to target
    sta $0a                   ; set player index to target when firing
    ldy #$02                  ; set bullet speed code to normal 1x speed (see bullet_velocity_adjust_ptr_tbl)
    jmp fire_bullet_at_player ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)

@exit:
    rts

; enemy type #$58
storage_bay_routine_ptr_tbl:
    .addr storage_bay_routine_00 ; set collision, apply velocity
    .addr storage_bay_routine_01 ; wait for activation
    .addr storage_bay_routine_02 ; animate storage room door opening
    .addr storage_bay_routine_03 ; generate 3 soldiers with a #$10 frame delay between each one

; set collision, apply velocity
storage_bay_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for activation
storage_bay_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    jsr compare_y_dist_to_20        ; compare the y distance between the closest player (y-axis) and the enemy to #$20
    bcs storage_bay_exit            ; branch if enemy is more than (or equal to) #$20 pixels above/below the enemy
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to storage_bay_routine_02

; animate storage room door opening
storage_bay_routine_02:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne storage_bay_exit            ; exit if animation delay hasn't elapsed
    jsr storage_bay_set_supertile
    lda #$01
    bcs @set_delay_exit             ; exit if unable to update supertile to try next frame
    inc ENEMY_FRAME,x               ; increment frame for next animation
    lda ENEMY_FRAME,x               ; load animation frame
    cmp #$03                        ; see if past last frame in the animation
    lda #$0a                        ; assume more frames to draw, set delay to #$0a
    bcc @set_delay_exit             ; branch if more frames to draw to set delay
    lda #$03                        ; finished activation animation
    sta ENEMY_VAR_1,x               ; initialize number of soldiers to generate
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to storage_bay_routine_03

@set_delay_exit:
    sta ENEMY_DELAY,x

storage_bay_exit:
    rts

; generate 3 soldiers with a #$10 frame delay between each one
storage_bay_routine_03:
    jsr update_enemy_pos               ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                  ; decrement soldier generation delay
    bne storage_bay_exit               ; exit if delay hasn't elapsed
    lda #$10                           ; load soldier generation delay
    sta ENEMY_DELAY,x                  ; set soldier generation delay
    ldy #$03                           ; enemy type = soldier
    jsr try_create_enemy_from_existing ; create a soldier
    bcc @continue                      ; branch if unable to create soldier (skip particular soldier generation)
    ldy $11                            ; created soldier, load soldier enemy slot
    lda #$81
    sta ENEMY_ATTRIBUTES,y             ; set soldier direction to right
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    clc                                ; clear carry in preparation for addition
    adc #$18
    sta ENEMY_Y_POS,y                  ; adjust soldier Y position by #$18 lower
    lda ENEMY_X_POS,x                  ; load enemy's X position
    clc                                ; clear carry in preparation for addition
    adc #$04
    sta ENEMY_X_POS,y                  ; adjust soldier X position by #$04 to the right

@continue:
    dec ENEMY_VAR_1,x    ; decrement remaining number of soldiers to generate
    bne storage_bay_exit ; exit if more soldiers to generate
    jmp remove_enemy     ; generated all soldiers, remove storage room

storage_bay_set_supertile:
    lda ENEMY_FRAME,x                      ; load storage bay supertile offset
    asl                                    ; double since each entry is #$02 bytes
    tay                                    ; transfer to offset register
    lda storage_bay_supertile_tbl+1,y      ; load second supertile index
    sta $0c                                ; set second supertile index (bottom)
    lda storage_bay_supertile_tbl,y        ; load first supertile index
    ldy #$1f                               ; x offset = -8, y offset = #$18 (one half supertile below)
    jmp load_banks_update_enemy_supertiles ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y

storage_bay_supertile_tbl:
    .byte $61,$75 ; frame 0
    .byte $76,$77 ; frame 1
    .byte $78,$79 ; frame 2

; enemy type #$5a
alien_skull_routine_ptr_tbl:
    .addr alien_skull_routine_00     ; set HP, initial X velocity (+/-.5), Y velocity (-1.25), and sprite attribute
    .addr alien_skull_routine_01     ; animate alien skill, apply gravity, fall until hit the ground, then set new X velocity and set HP to 1
    .addr alien_skull_routine_02     ; enemy on ground, animate while it crawls in a single direction until off screen or destroyed
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set HP, initial X velocity (+/-.5), Y velocity (-1.25), and sprite attribute
alien_skull_routine_00:
    lda #$02                        ; set base ENEMY_HP to #$02
    ldy #$06                        ; set enemy_hp_adjust_tbl offset to #$06
    jsr set_enemy_hp_from_a_and_y   ; set ENEMY_HP calculated using medium HP difficulty (adjusted by ENEMY_DIFFICULTY)
                                    ; sets enemy HP = #$02, #$06, or #$0a
    lda ENEMY_ATTRIBUTES,x          ; load enemy attributes
    asl                             ; double since each entry in alien_skull_x_vel_tbl is #$02 bytes
    tay                             ; transfer to offset register
    lda alien_skull_x_vel_tbl,y     ; load enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x    ; store enemy's fractional X velocity
    lda alien_skull_x_vel_tbl+1,y   ; load  enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x     ; store enemy's fast X velocity
    lda #$c0
    sta ENEMY_Y_VELOCITY_FRACT,x    ; store enemy's fractional Y velocity
    lda #$fe
    sta ENEMY_Y_VELOCITY_FAST,x     ; store enemy's fast Y velocity
                                    ; set initial Y velocity to -1.25
    jsr alien_skull_set_sprite_attr ; set sprite attribute based on X velocity
    ora #$20                        ; set background priority, i.e. background draws on top of sprite
    sta ENEMY_SPRITE_ATTR,x         ; set background draw priority
                                    ; this makes it look like the alien skull is coming out of the krypto-crustacean bays
    jmp advance_enemy_routine       ; advance to next routine

alien_skull_x_vel_tbl:
    .byte $80,$00 ;  0.5
    .byte $80,$ff ; -0.5

; animate alien skill, apply gravity, fall until hit the ground, then set new X velocity and set HP to 1
alien_skull_routine_01:
    ldy #$0b                       ; animation index for alien skull
    jsr set_enemy_animation_sprite ; animate sprite for alien skull based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda #$14                       ; emulate gravity by adding 0.078125 to Y velocity each frame
    jsr add_a_to_enemy_y_fract_vel ; apply gravity
    jsr apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp #$10
    bcc @clear_x_vel               ; branch if on far left edge of screen to stop X velocity
    cmp #$f0                       ; compare to far right edge of screen
    bcc @continue                  ; branch if in main part of screen, no need to stop X velocity

@clear_x_vel:
    jsr clear_enemy_x_vel ; set enemy X velocity (fast and fractional) to 0

@continue:
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$a0
    bcc @check_bg_collision         ; branch if still high up to keep the sprite background priority
    jsr alien_skull_set_sprite_attr ; set sprite attribute based on X velocity
                                    ; this effectively strips bit 5 to remove background priority

@check_bg_collision:
    lda #$08                                ; add #$08 to Y position when checking for background collision
    jsr get_falling_enemy_bg_collision_code
    beq @exit                               ; exit if no collision
    jsr set_y_pos_for_bg                    ; enemy landed on ground
                                            ; set enemy Y position based on background collision
    jsr clear_enemy_y_vel                   ; set enemy Y velocity to 0
    lda ENEMY_ATTRIBUTES,x                  ; load enemy attributes
    asl                                     ; double since each entry in alien_skull_ground_x_vel_tbl is #$02 bytes
    tay                                     ; transfer to offset register
    lda alien_skull_ground_x_vel_tbl,y      ; load  enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x            ; store enemy's fractional X velocity
    lda alien_skull_ground_x_vel_tbl+1,y    ; load enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x             ; store enemy's fast X velocity
                                            ; sets X velocity to +/-1.25
    jsr alien_skull_set_sprite_attr         ; set sprite attribute based on X velocity
    lda #$01
    sta ENEMY_HP,x                          ; set HP to #$01 now that enemy is 'crawling' on the ground
    jmp advance_enemy_routine               ; advance to next routine

@exit:
    rts

alien_skull_ground_x_vel_tbl:
    .byte $c0,$fe ; -1.25
    .byte $40,$01 ;  1.25

; enemy on ground, animate while it crawls in a single direction until off screen or destroyed
alien_skull_routine_02:
    ldy #$0b                       ; animation index for alien skull
    jsr set_enemy_animation_sprite ; animate sprite for alien skull based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jmp apply_velocity             ; apply enemy's velocity to its position, removing enemy if off-screen

; set sprite attribute based on X velocity
; input
;  * x - enemy slot index
; output
;  * a - the ENEMY_SPRITE_ATTR
alien_skull_set_sprite_attr:
    lda ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    ora ENEMY_X_VELOCITY_FRACT,x
    beq @exit                    ; branch if no X velocity
    lda #$02                     ; assume facing right, no horizontal flip
    ldy ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    bmi @continue                ; branch if x direction is to the left
    lda #$42                     ; alien skull moving right, flip sprite horizontally

@continue:
    sta ENEMY_SPRITE_ATTR,x ; set sprite attribute, including horizontal flip and palette

@exit:
    rts

; enemy type #$35
; generated by krytpo-crustacean in boss screen
red_blob_routine_ptr_tbl:
    .addr red_blob_routine_00        ; set sprite palette, bg priority, Y velocity, and delay before advancing routine
    .addr red_blob_routine_01        ; animate while waiting for bg priority delay, remove sprite background priority, set random targeting delay, advance routine
    .addr red_blob_routine_02        ; animate while waiting for targeting delay to elapse, then target player at 1x speed, advance routing
    .addr red_blob_routine_03        ; animate until collide with background, the advance routine to begin destroy sequence
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set sprite palette, bg priority, Y velocity, and delay before advancing routine
red_blob_routine_00:
    lda #$22
    sta ENEMY_SPRITE_ATTR,x         ; set sprite attribute, including palette and background priority
                                    ; sprite will draw behind background
    lda #$00
    sta ENEMY_Y_VELOCITY_FRACT,x    ; store enemy's fractional Y velocity
    lda #$02
    sta ENEMY_Y_VELOCITY_FAST,x     ; store enemy's fast Y velocity
                                    ; sets Y velocity to 2
    lda #$0a                        ; initial delay before removing background priority
    jmp set_delay_adv_enemy_routine ; set delay to #$0a and set routine to red_blob_routine_01

; animate while waiting for bg priority delay, remove sprite background priority, set random targeting delay, advance routine
red_blob_routine_01:
    ldy #$0a                        ; animation index for red blob
    jsr set_enemy_animation_sprite  ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_DELAY,x               ; decrement background priority removal delay timer
    bne red_blob_exit               ; exit if delay hasn't elapsed
    lda #$02                        ; delay elapsed, remove background priority
    sta ENEMY_SPRITE_ATTR,x         ; remove background priority, sprite will render in front of background
    lda RANDOM_NUM                  ; load random number
    adc FRAME_COUNTER
    sta RANDOM_NUM                  ; randomize random number
    and #$03                        ; get a random number between 0 and 3 inclusively
    tay                             ; transfer to offset register
    lda red_blob_target_delay_tbl,y ; load random targeting delay
    jmp set_delay_adv_enemy_routine ; set delay and set routine to red_blob_routine_02

red_blob_exit:
    rts

; delay before targeting player
red_blob_target_delay_tbl:
    .byte $40,$60,$70,$50

; animate while waiting for targeting delay to elapse, then target player at 1x speed, advance routing
red_blob_routine_02:
    ldy #$0a                       ; animation index for red blob
    jsr set_enemy_animation_sprite ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    dec ENEMY_DELAY,x              ; decrement targeting delay
    bne red_blob_exit              ; exit if targeting delay hasn't elapsed
    jsr player_enemy_x_dist        ; targeting delay elapsed, target player
                                   ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$02                       ; 1x speed (normal speed)
    jsr set_vel_to_target_player   ; set enemy velocity to target player index $0a at 1x speed
    jmp advance_enemy_routine      ; advance to next routine

; animate until collide with background, the advance routine to begin destroy sequence
red_blob_routine_03:
    ldy #$0a                        ; animation index for red blob
    jsr set_enemy_animation_sprite  ; animate sprite for red blob based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$00                        ; don't adjust enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    beq red_blob_exit               ; exit if not collided with any background
    jmp advance_enemy_routine       ; advance to next routine to begin destroy routines

; compare the y distance between the closest player (y-axis) and the enemy to #$20
; input
;  * x - enemy slot index
; output
;  * carry flag - set when y distance is greater than o equal to #$20
;  * zero flag - set when y distance between player and enemy is equal to #$20
compare_y_dist_to_20:
    jsr player_enemy_y_dist   ; a = closest y distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    sec                       ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_Y_POS,y ; subtract player's position
    bcs @continue             ; branch if player equal or above enemy
    eor #$ff                  ; player above enemy, flip all bits add 1
    adc #$01

@continue:
    cmp #$20 ; compare player-enemy y distance to #$20
    rts

; enemy type #$59
krypto_crustacean_routine_ptr_tbl:
    .addr krypto_crustacean_routine_00 ; initialize destroyed attributes and HP
    .addr krypto_crustacean_routine_01
    .addr krypto_crustacean_routine_02
    .addr krypto_crustacean_routine_03
    .addr krypto_crustacean_routine_04
    .addr krypto_crustacean_routine_05 ; enemy destroyed routine
    .addr krypto_crustacean_routine_06
    .addr krypto_crustacean_routine_07
    .addr krypto_crustacean_routine_08
    .addr krypto_crustacean_routine_09

; initialize destroyed attributes and HP
krypto_crustacean_routine_00:
    lda #$10
    sta ENEMY_DESTROY_ATTRS,x ; enable collision, set destroy sound to sound_26 (B OUT) - boss destroy
    lda #$f0                  ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    jmp advance_enemy_routine ; advance to next routine

krypto_crustacean_routine_01:
    lda SCREEN_SCROLL_TYPE     ; 0 = horizontal, 1 = vertical/overhead
    beq krypto_crustacean_exit
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne krypto_crustacean_exit
    lda ENEMY_GEN_CTRL
    ora #$40                   ; set bit 6 to pause random enemy generation
    sta ENEMY_GEN_CTRL
    lda #$b8
    sta Y_AUTOSCROLL_STOP_POS
    lda #$31                   ; sound_31 (BOSS BGM) - Ruined Base
    jsr play_sound             ; play boss background music
    jmp advance_enemy_routine  ; advance to next routine

krypto_crustacean_exit:
    rts

krypto_crustacean_routine_02:
    lda Y_AUTOSCROLL_STOP_POS
    bne krypto_crustacean_exit
    jsr backup_scroll_and_ppuctrl
    jsr reset_draw_point
    lda #$08
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine     ; advance to next routine

krypto_crustacean_routine_03:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne krypto_crustacean_exit
    dec ENEMY_VAR_1,x
    beq @continue
    lda #$00
    sta X_SCROLL_DRAW_POINT
    rts

@continue:
    lda #$01
    sta SCREEN_SCROLL_TYPE               ; set vertical/overhead scrolling
    lda #$01
    sta NT_MIRRORING                     ; set nametable mirroring (0: vertical; 1: horizontal)
    lda #$80
    sta ENEMY_X_POS,x
    sta ENEMY_VAR_2,x
    lda #$c4
    sta ENEMY_Y_POS,x
    lda #$ff
    sta ENEMY_VAR_6,x
    lda #$00
    sta ENEMY_VAR_7,x
    lda #$a8
    sta PPUCTRL_SETTINGS                 ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
    jsr init_irq_scroll                  ; set post-irq scroll to current scroll
    jsr init_bg_boss_pre_irq_max_scrolls ; init boss screen pre-irq scroll max values
    lda #$0b
    sta IRQ_TYPE                         ; set irq routine type to irq_handler_0b_ptr_tbl
                                         ; level 5 cliff boss (krypto-crustacean)
    lda #$c1
    sta SCANLINE_IRQ_1                   ; set where irq_handler_0b_00 will run
                                         ; this is where the PPU address and left pattern table tiles are updated
    lda #$00
    sta ENEMY_VAR_3,x
    lda #$e0
    sta ENEMY_X_VELOCITY_FRACT,x         ; store enemy's fractional X velocity
    lda #$00
    sta ENEMY_X_VELOCITY_FAST,x          ; store enemy's fast X velocity
    ldy #$00
    jsr krypto_crustacean_set_y_vel
    jsr clear_bg_collision_data
    lda #$f0
    sta ENEMY_ANIMATION_DELAY,x
    lda #$00
    sta ENEMY_FRAME,x
    lda #$10
    sta ENEMY_FIRING_DELAY,x
    lda #$00
    sta ENEMY_ATTRIBUTES,x
    lda #$20
    sta ENEMY_VAR_4,x
    lda #$5a
    sta LEFT_TOP_HALF_CHR_BANK           ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$06
    sta LEFT_BOTTOM_CHR_HALF_BANK        ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    jmp advance_enemy_routine            ; advance to next routine

; input
;  * x - enemy slot index
;  * y - krypto_crustacean_y_vel_tbl offset
krypto_crustacean_set_y_vel:
    lda krypto_crustacean_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x        ; store enemy's fractional Y velocity
    lda krypto_crustacean_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x         ; store enemy's fast Y velocity
    rts

krypto_crustacean_y_vel_tbl:
    .byte $40,$00 ; 0.25
    .byte $80,$00 ; 0.50

krypto_crustacean_routine_04:
    lda #$01
    sta ENEMY_SPRITE,x                       ; set invisible sprite
    jsr krypto_crustacean_run_flight_routine ; animate path of boss based on flight mode ENEMY_VAR_3,x
    ldy #$fc
    jsr modify_enemy_x_vel                   ; adjust X velocity based on ENEMY_X_POS,x distance from ENEMY_VAR_2,x
    lda ENEMY_VAR_5,x                        ; see if descended into level at pacing left/right height
                                             ; incremented in krypto_crustacean_flight_routine_00 when descended
    beq @continue                            ; branch if not yet fully descended into level
    ldy #$01                                 ; descended to height to start slowing down
    jsr modify_enemy_y_vel                   ; gradually slow down descent to get to left/right pace height
                                             ; adjust Y velocity based on ENEMY_Y_POS,x distance from ENEMY_VAR_1,x

@continue:
    jsr krypto_crustacean_apply_vel
    jsr krypto_crustacean_anim
    jsr krypto_crustacean_create_skull_if_ready
    jsr krypto_crustacean_create_gel_if_ready
    rts

; enemy destroyed routine
krypto_crustacean_routine_05:
    jsr clear_enemy_x_vel
    ldy #$02
    jsr krypto_crustacean_set_y_vel
    jsr krypto_crustacean_safe_apply_vel
    jmp enemy_routine_boss_defeated_00

krypto_crustacean_routine_06:
    jsr krypto_crustacean_safe_apply_vel
    jmp enemy_routine_boss_defeated_01

krypto_crustacean_routine_07:
    jsr krypto_crustacean_safe_apply_vel
    jmp bg_enemy_explosion_routine_00    ; set empty sprite, play optional enemy destroyed sound, disable collisions

krypto_crustacean_routine_08:
    jsr krypto_crustacean_safe_apply_vel
    jmp bg_enemy_explosion_routine_01

krypto_crustacean_routine_09:
    jsr set_nmi_noop_irq               ; remove any scanline interrupts
    jsr reset_scroll_draw_point
    inc DRAW_X_SCREEN                  ; advance to next current level screen number (how many screens into the level)
    jsr restore_scroll_and_ppuctrl     ; restore scroll and PPU control values
    lda #$20
    sta LEFT_TOP_HALF_CHR_BANK         ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$22
    sta LEFT_BOTTOM_CHR_HALF_BANK      ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    jmp set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy

krypto_crustacean_anim:
    lda ENEMY_VAR_3,x                     ; load flight mode
    cmp #$02
    bne @adv_frame                        ; branch if not dipping down
    ldy ENEMY_FRAME,x                     ; dipping down
    lda krypto_crustacean_dip_delay_tbl,y
    asl                                   ; move bit 6 to bit 7
    bmi krypto_crustacean_draw_ports_exit ; exit if bit 6 was set, no delay
    lda ENEMY_ANIMATION_DELAY,x
    cmp #$0c                              ; see if animation delay not at max
    bcc @adv_frame                        ; branch to continue if not at max delay
    lda #$0c
    sta ENEMY_ANIMATION_DELAY,x           ; set max delay

@adv_frame:
    dec ENEMY_ANIMATION_DELAY,x
    bne krypto_crustacean_draw_ports_exit
    inc ENEMY_FRAME,x
    lda ENEMY_FRAME,x
    cmp #$08
    bcc @init_draw
    lda #$00

@init_draw:
    sta ENEMY_FRAME,x
    tay
    lda krypto_crustacean_anim_delay_tbl,y
    sta ENEMY_ANIMATION_DELAY,x
    tya
    asl                                        ; double since each entry is a #$02 byte memory address
    tay                                        ; transfer to offset register
    lda krypto_crustacean_bg_tiles_ptr_tbl,y
    sta $08
    lda krypto_crustacean_bg_tiles_ptr_tbl+1,y

; input
;  * $08 - graphics data read address low byte
;  * a - graphics data read address high byte
krypto_crustacean_draw_ports:
    sta $09
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET

@loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    iny
    inx
    cmp #$ff
    bne @loop
    stx GRAPHICS_BUFFER_OFFSET
    ldx ENEMY_CURRENT_SLOT

krypto_crustacean_draw_ports_exit:
    rts

krypto_crustacean_anim_delay_tbl:
    .byte $0c,$50,$0c,$10,$0c,$50,$0c,$10

krypto_crustacean_bg_tiles_ptr_tbl:
    .addr krypto_crustacean_bg_tiles_00
    .addr krypto_crustacean_bg_tiles_01
    .addr krypto_crustacean_bg_tiles_00
    .addr krypto_crustacean_bg_tiles_03
    .addr krypto_crustacean_bg_tiles_04
    .addr krypto_crustacean_bg_tiles_05
    .addr krypto_crustacean_bg_tiles_04
    .addr krypto_crustacean_bg_tiles_07

krypto_crustacean_bg_tiles_03:
    .byte $07,$21,$d3,$02,$18,$24,$21,$b4,$05,$10,$19,$25,$32,$3d,$21,$b5
    .byte $05,$00,$1a,$26,$33,$3e,$21,$d6,$04,$00,$27,$34,$3f,$ff

krypto_crustacean_bg_tiles_00:
    .byte $07,$21,$d3,$02,$7a,$fe,$21,$b4,$05,$79,$25,$32,$7b,$7c,$21,$95
    .byte $06,$00,$1a,$26,$33,$3e,$7d,$21,$96,$06,$00,$00,$27,$34,$3f,$7e
    .byte $21,$97,$02,$00,$00,$ff

krypto_crustacean_bg_tiles_01:
    .byte $07,$21,$b4,$05,$f3,$f7,$fa,$fc,$7c,$21,$95,$06,$f0,$f4,$f8,$fb
    .byte $00,$fd,$21,$96,$06,$f1,$f5,$f9,$00,$00,$00,$21,$97,$02,$f2,$f6
    .byte $ff

krypto_crustacean_bg_tiles_07:
    .byte $07,$21,$c9,$04,$00,$1b,$28,$35,$21,$aa,$05,$00,$11,$1c,$29,$36
    .byte $21,$ab,$05,$07,$12,$1d,$2a,$37,$21,$cc,$02,$13,$1e,$ff

krypto_crustacean_bg_tiles_04:
    .byte $07,$21,$88,$02,$00,$00,$21,$89,$06,$00,$00,$1b,$28,$35,$68,$21
    .byte $8a,$06,$00,$11,$1c,$29,$36,$69,$21,$ab,$05,$65,$1d,$2a,$67,$6a
    .byte $21,$cc,$02,$66,$7f,$ff

krypto_crustacean_bg_tiles_05:
    .byte $07,$21,$88,$02,$6b,$6e,$21,$89,$06,$6c,$6f,$72,$00,$00,$00,$21
    .byte $8a,$06,$6d,$70,$73,$75,$00,$78,$21,$ab,$05,$71,$74,$76,$77,$6a
    .byte $ff

krypto_crustacean_create_skull_if_ready:
    ldy ENEMY_FRAME,x
    lda krypto_crustacean_dip_delay_tbl,y        ; if negative, don't update position
    bmi @exit
    dec ENEMY_FIRING_DELAY,x
    bne @exit
    lda #$30                                     ; create alien skull countdown elapsed
    sta ENEMY_FIRING_DELAY,x                     ; set new timer to create alien skulls
    ldy #$5a                                     ; enemy type = alien skull
    jsr try_create_enemy_from_existing           ; create alien skull
    bcc @exit                                    ; branch if unable to create alien skull
    ldy ENEMY_FRAME,x
    lda krypto_crustacean_dip_delay_tbl,y
    sta $08
    tay
    lda krypto_crustacean_skull_pos_offset_tbl,y
    clc                                          ; clear carry in preparation for addition
    adc ENEMY_X_POS,x
    ldy $11                                      ; enemy slot of created alien skull, if created
    sta ENEMY_X_POS,y
    lda ENEMY_Y_POS,x                            ; load enemy's Y position
    sec                                          ; set carry flag in preparation for subtraction
    sbc #$10
    sta ENEMY_Y_POS,y
    lda $08
    sta ENEMY_ATTRIBUTES,y

@exit:
    rts

; ENEMY_ATTRIBUTES for krypto-crustacean
; when negative, frame isn't updated
krypto_crustacean_dip_delay_tbl:
    .byte $80,$00,$80,$c0,$80,$01,$80,$c0

krypto_crustacean_skull_pos_offset_tbl:
    .byte $30,$d0

; animate port opening and creation of red poisonous insect gels
krypto_crustacean_create_gel_if_ready:
    lda ENEMY_VAR_3,x      ; load flight mode
    cmp #$01               ; see if pacing left and right at same height
    beq @continue          ; branch if pacing left and right at same height
    lda ENEMY_ATTRIBUTES,x ; not pacing left and right, load animation frame of port opening
    beq @exit              ; exit if insect gel port is closed

@continue:
    lda GLOBAL_TIMER
    lsr
    bcc @exit                                       ; branch if even frame
    dec ENEMY_VAR_4,x                               ; odd frame, decrement insect gel port animation timer
    bne @exit                                       ; exit if timer not elapsed
    lda ENEMY_ATTRIBUTES,x                          ; load which frame of the 3 bottom ports to draw
    asl                                             ; double since each entry is a #$02 byte memory address
    tay                                             ; transfer to offset register
    lda krypto_crustacean_ports_bg_tile_ptr_tbl,y
    sta $08
    lda krypto_crustacean_ports_bg_tile_ptr_tbl+1,y
    jsr krypto_crustacean_draw_ports                ; draw the bg tiles for the 3 bottom ports of the krypto-crustacean
    inc ENEMY_ATTRIBUTES,x                          ; move to next frame of bottom ports
    lda ENEMY_ATTRIBUTES,x
    cmp #$04
    lda #$06
    bcc @set_delay_create_insect_gel
    lda #$00
    sta ENEMY_ATTRIBUTES,x                          ; move to first frame for next round
    lda #$50

@set_delay_create_insect_gel:
    sta ENEMY_VAR_4,x      ; set poison insect gel creation delay timer
    lda ENEMY_ATTRIBUTES,x
    cmp #$02
    bne @exit
    lda #$02
    sta $08

@create_insect_gel:
    ldy #$35                                      ; enemy type = poisonous insect gel (red blob)
    jsr try_create_enemy_from_existing            ; create poisonous insect gel (red blob)
    bcc @exit                                     ; branch if unable to create poisonous insect gel (red blob)
    ldy $08
    lda krypto_crustacean_insect_gel_offset_tbl,y
    clc                                           ; clear carry in preparation for addition
    adc ENEMY_X_POS,x
    ldy $11                                       ; load enemy slot of created insect gel
    sta ENEMY_X_POS,y
    lda ENEMY_Y_POS,x                             ; load enemy's Y position
    clc                                           ; clear carry in preparation for addition
    adc #$12
    sta ENEMY_Y_POS,y
    dec $08
    bpl @create_insect_gel

@exit:
    rts

krypto_crustacean_insect_gel_offset_tbl:
    .byte $e0,$00,$20

; bg tiles for the 3 ports at the bottom of the krypto-crustacean
krypto_crustacean_ports_bg_tile_ptr_tbl:
    .addr krypto_crustacean_port_bg_tile_00 ; half-open
    .addr krypto_crustacean_port_bg_tile_01 ; fully open
    .addr krypto_crustacean_port_bg_tile_00 ; half-open
    .addr krypto_crustacean_port_bg_tile_03 ; fully closed

; fully closed
krypto_crustacean_port_bg_tile_03:
    .byte $06                                             ; block mode
    .byte $22,$8a                                         ; PPU address
    .byte $0c                                             ; block size
    .byte $53,$54,$55,$56,$53,$54,$55,$56,$53,$54,$55,$56 ; graphics bytes
    .byte $22,$ab                                         ; PPU address
    .byte $0a                                             ; block size
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00         ; graphics bytes
    .byte $ff                                             ; end of data marker

; half-open
krypto_crustacean_port_bg_tile_00:
    .byte $06                                             ; block mode
    .byte $22,$8a                                         ; PPU address
    .byte $0c                                             ; block size
    .byte $57,$58,$59,$5a,$57,$58,$59,$5a,$57,$58,$59,$5a ; graphics bytes
    .byte $22,$ab                                         ; PPU address
    .byte $0a                                             ; block size
    .byte $5b,$5c,$00,$00,$5b,$5c,$00,$00,$5b,$5c         ; graphics bytes
    .byte $ff                                             ; end of data marker

; fully open
krypto_crustacean_port_bg_tile_01:
    .byte $06                                             ; block mode
    .byte $22,$8a                                         ; PPU address
    .byte $0c                                             ; block size
    .byte $5d,$5e,$5f,$60,$5d,$5e,$5f,$60,$5e,$5d,$5f,$60 ; graphics bytes
    .byte $22,$ab                                         ; PPU address
    .byte $0a                                             ; block size
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00         ; graphics bytes
    .byte $ff

krypto_crustacean_run_flight_routine:
    lda ENEMY_VAR_3,x
    jsr run_routine_from_tbl_below

krypto_crustacean_flight_routine_ptr_tbl:
    .addr krypto_crustacean_flight_routine_00 ; initial drop into level
    .addr krypto_crustacean_flight_routine_01 ; float left and right
    .addr krypto_crustacean_flight_routine_02 ; dip down
    .addr krypto_crustacean_flight_routine_03 ; rise back up

; initial drop into level
krypto_crustacean_flight_routine_00:
    lda ENEMY_VAR_6,x
    bne krypto_crustacean_flight_routine_exit          ; exit if boss not on visible nametable
    lda ENEMY_Y_POS,x                                  ; load enemy's Y position
    cmp #$48                                           ; boss on visible nametable
    bcc krypto_crustacean_flight_routine_exit          ; exit if not yet at left/right pacing height
    lda #$f0
    jsr krypto_crustacean_set_delay_adv_flight_routine ; set how long to pace left/right and advance routine
    lda #$20                                           ; HP = #$20, #$30, or #$38
    jsr set_enemy_hp_hard                              ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda ENEMY_Y_POS,x                                  ; load enemy's Y position
    sta ENEMY_VAR_1,x
    inc ENEMY_VAR_5,x
    ldy #$02
    jmp krypto_crustacean_set_y_vel

; float left and right
krypto_crustacean_flight_routine_01:
    lda FRAME_COUNTER                         ; load frame counter
    lsr
    bcs krypto_crustacean_flight_routine_exit ; exit if odd frame
    dec ENEMY_DELAY,x                         ; even frame, decrement timer for how long to pace left/right
    beq krypto_crustacean_adv_flight_routine  ; branch to advance to dip down if timer elapsed

krypto_crustacean_flight_routine_exit:
    rts

; dip down
krypto_crustacean_flight_routine_02:
    lda #$01
    jsr krypto_crustacean_set_y_pos
    lda ENEMY_Y_POS,x                         ; load enemy's Y position
    cmp #$a4
    bcc krypto_crustacean_flight_routine_exit

krypto_crustacean_set_delay_adv_flight_routine:
    sta ENEMY_DELAY,x

krypto_crustacean_adv_flight_routine:
    inc ENEMY_VAR_3,x
    rts

; rise back up
krypto_crustacean_flight_routine_03:
    lda #$ff
    jsr krypto_crustacean_set_y_pos
    lda ENEMY_Y_POS,x                         ; load enemy's Y position
    cmp #$48
    bcs krypto_crustacean_flight_routine_exit
    lda #$f0
    sta ENEMY_DELAY,x
    lda #$01
    sta ENEMY_VAR_3,x
    rts

; sets the Y position and Y velocity adjustment when rising/dipping
; input
;  * a - y position adjustment amount (positive moves down)
; output
;  * ENEMY_VAR_1,x - value used by modify_enemy_y_vel to adjust Y velocity
krypto_crustacean_set_y_pos:
    sta $00
    lda ENEMY_VAR_1,x
    clc               ; clear carry in preparation for addition
    adc $00
    sta ENEMY_VAR_1,x ; used by modify_enemy_y_vel
    lda ENEMY_Y_POS,x ; load enemy's Y position
    clc               ; clear carry in preparation for addition
    adc $00
    sta ENEMY_Y_POS,x

krypto_crustacean_exit2:
    rts

; apply Y velocity if Y position in top 56.25% of screen (not quite the lowest
; the boss dips)
krypto_crustacean_safe_apply_vel:
    lda ENEMY_Y_POS,x           ; load enemy's Y position
    cmp #$90
    bcs krypto_crustacean_exit2

krypto_crustacean_apply_vel:
    jsr bg_boss_apply_vel
    jmp bg_enemy_set_scrolls ; simulate moving (ENEMY_X_POS, ENEMY_Y_POS) by scrolling in the opposite direction

; !(UNUSED)
; replaces boss tiles with tiles that enable the flashing red palette effect
; presumably was going to be used when boss was almost destroyed
krypto_crustacean_set_flashing:
    lda #$14
    sta $08                    ; writing #$14 bytes to graphics buffer
    ldy #$00                   ; initialize graphics data read offset
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$40
    bcs @exit                  ; exit if graphics buffer full

@loop:
    lda krypto_crustacean_flashing_tiles_tbl,y
    sta CPU_GRAPHICS_BUFFER,x
    inx                                        ; increment graphics buffer write offset
    iny                                        ; increment graphics data read offset
    dec $08                                    ; decrement byte write count
    bne @loop
    stx GRAPHICS_BUFFER_OFFSET
    clc

@exit:
    ldx ENEMY_CURRENT_SLOT ; restore x to enemy current slot
    rts

krypto_crustacean_flashing_tiles_tbl:
    .byte $03,$23,$d8,$12,$aa,$06,$23,$ea,$05,$ae,$fa,$fa,$fa,$fa,$ff,$03
    .byte $23,$ef,$08,$aa

; enemy type #$5d
mouth_pit_routine_ptr_tbl:
    .addr mouth_pit_routine_00
    .addr mouth_pit_routine_01
    .addr mouth_pit_routine_02
    .addr mouth_pit_routine_03       ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

mouth_pit_routine_00:
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite
    lda #$01
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision
    lda #$06                  ; HP = #$06, #$0a, or #$0d
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda ENEMY_ATTRIBUTES,x
    lsr
    lda #$02
    bcc @continue
    lda ENEMY_X_POS,x         ; attribute bit set (from level screen enemy), need to adjust position
                              ; this isn't used for mouth pits generated by mouth pit generator (enemy type = #$5e)
                              ; load enemy's X position
    sec                       ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_X_POS,x
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    clc                       ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_Y_POS,x
    lda #$ff                  ; open/close repeatedly until destroyed or offscreen

@continue:
    sta ENEMY_VAR_2,x               ; set number of times to open and close
                                    ; #$ff = opens/closes repeatedly until destroyed or offscreen
    lda #$01
    sta ENEMY_FRAME,x
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to mouth_pit_routine_01

mouth_pit_routine_01:
    jsr update_enemy_pos   ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcc @adv_routine
    lda ENEMY_Y_POS,x      ; load enemy's Y position
    cmp #$28
    bcc @exit

@adv_routine:
    jmp advance_enemy_routine ; advance to next routine

@exit:
    rts

mouth_pit_routine_02:
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne mouth_pit_exit        ; exit if delay not elapsed
    jsr mouth_pit_anim        ; write graphic tiles and attribute tiles based on ENEMY_FRAME,x
    lda #$01
    bcs @set_delay_frame_exit
    lda ENEMY_VAR_1,x
    bne @dec_frame
    inc ENEMY_FRAME,x
    ldy ENEMY_Y_POS,x         ; load enemy's Y position
    cpy #$c0
    bcs @continue             ; branch if close to bottom
    lda ENEMY_FRAME,x         ; not close to bottom
    cmp #$05
    bcs @continue             ; branch if past last frame
    cmp #$02
    bne @set_delay_8_exit     ; exit with delay if frame before showing teeth
    lda #$1e                  ; not about to show teeth, or already showing teeth
    bne @set_delay_frame_exit ; always branch to set longer delay if teeth aren't showing

@continue:
    lda #$01
    sta ENEMY_VAR_1,x         ; set closing animation
    dec ENEMY_FRAME,x
    dec ENEMY_FRAME,x
    lda #$0c
    bne @set_delay_frame_exit ; always branch to set short delay

@dec_frame:
    dec ENEMY_FRAME,x
    lda #$ff             ; remove mouth pit when at bottom of screen
    ldy ENEMY_Y_POS,x    ; load enemy's Y position
    cpy #$c0
    bcs @delay_or_remove ; branch if close to the bottom of screen
    ldy ENEMY_VAR_2,x    ; remove mouth pit when finished all open/close cycles
    beq @delay_or_remove
    lda #$01             ; mouth pit not at bottom of screen, and has more open/close cycles

@delay_or_remove:
    cmp ENEMY_FRAME,x     ; compare either #$ff or #$01 to current frame
    bne @set_delay_8_exit
    tay
    bmi mouth_pit_remove  ; remove if at bottom of screen, or finished all open/close cycles
    inc ENEMY_FRAME,x     ; move open/close cycles and not at bottom of screen, start opening
    inc ENEMY_FRAME,x
    lda ENEMY_VAR_2,x     ; load remaining number of open/close cycles
    bmi @set_open_anim
    dec ENEMY_VAR_2,x     ; finished open/close cycle, decrement remaining times to open and close

@set_open_anim:
    lda #$00
    sta ENEMY_VAR_1,x ; set animation direction so mouth opens

@set_delay_8_exit:
    lda #$08

@set_delay_frame_exit:
    sta ENEMY_DELAY,x
    ldy ENEMY_FRAME,x
    lda mouth_pit_destroy_attr_tbl,y
    sta ENEMY_DESTROY_ATTRS,x

mouth_pit_exit:
    rts

mouth_pit_remove:
    jmp remove_enemy

mouth_pit_destroy_attr_tbl:
    .byte $81,$01,$01,$00,$00,$81

; enemy destroyed routine
mouth_pit_routine_03:
    lda #$05
    sta ENEMY_FRAME,x
    jsr mouth_pit_anim             ; write graphic tiles and attribute tiles based on ENEMY_FRAME,x
    bcs @continue
    jmp enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

@continue:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; writes graphic tiles and attribute tiles based on ENEMY_FRAME,x
; input
;  * x - enemy slot index
mouth_pit_anim:
    lda GRAPHICS_BUFFER_OFFSET
    cmp #$20
    lda #$01
    bcs mouth_pit_exit                ; exit if graphics buffer full
    jsr @calc_ppu_addr
    lda ENEMY_FRAME,x                 ; load current frame
    asl                               ; double since each entry is a #$02 byte address
    tay                               ; transfer to offset register
    lda mouth_pit_graphic_ptr_tbl,y
    sta $08
    lda mouth_pit_graphic_ptr_tbl+1,y
    sta $09
    ldy #$00                          ; initialize graphics read offset
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                          ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x         ; set block mode
    inx                               ; increment graphics buffer write offset

@write_graphic_blocks:
    lda ($08),y                 ; load next graphic block size
    cmp #$ff
    beq @write_attribute_data   ; branch if finished writing graphic tiles
                                ; to write attribute data
    clc                         ; clear carry in preparation for addition
    adc $00
    sta CPU_GRAPHICS_BUFFER+1,x ; write PPU address low byte
    lda $01
    adc #$00
    sta CPU_GRAPHICS_BUFFER,x   ; write PPU address high byte
    inx                         ; increment graphics buffer write offset
    inx                         ; increment graphics buffer write offset
    iny
    lda ($08),y
    sta $0a                     ; set block size
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny

@write_graphic_bytes:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    inx                       ; increment graphics buffer write offset
    iny                       ; increment graphics read offset
    dec $0a                   ; decrement graphic block counter
    bne @write_graphic_bytes
    beq @write_graphic_blocks

@write_attribute_data:
    iny
    lda $00
    asl
    rol $01
    asl
    asl
    lsr $01
    ror
    lsr $01
    ror
    lsr $01
    ror
    lsr
    lsr
    ora #$c0
    sta $00
    lda #$02
    sta $01  ; set number of graphic blocks to write

@write_attr_blocks:
    lda #$23
    sta CPU_GRAPHICS_BUFFER,x ; write PPU address high byte
    inx                       ; increment graphics buffer write offset
    lda $00
    sta CPU_GRAPHICS_BUFFER,x ; write PPU address low byte
    inx                       ; increment graphics buffer write offset
    lda #$02
    sta CPU_GRAPHICS_BUFFER,x ; write block size
    sta $02                   ; set block size in $02 for loop counter
    inx                       ; increment graphics buffer write offset

@attr_write_loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x  ; write attribute byte
    inx                        ; increment graphics buffer write offset
    iny                        ; increment graphics read offset
    dec $02                    ; decrement remaining bytes in block
    bne @attr_write_loop
    lda $00
    clc                        ; clear carry in preparation for addition
    adc #$08
    sta $00                    ; move down to next row of attribute table
    dec $01
    bne @write_attr_blocks
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x
    inx
    stx GRAPHICS_BUFFER_OFFSET
    ldx ENEMY_CURRENT_SLOT
    clc
    rts

@calc_ppu_addr:
    lda #$08
    sta $01
    lda ENEMY_Y_POS,x    ; load enemy's Y position
    sec                  ; set carry flag in preparation for subtraction
    sbc #$10
    clc                  ; clear carry in preparation for addition
    adc Y_SCROLL         ; add vertical scroll
    bcs @handle_overflow ; branch if overflow
    cmp #$f0
    bcc @continue

@handle_overflow:
    adc #$0f

@continue:
    and #$e0
    asl
    rol $01
    asl
    rol $01
    sta $02
    lda ENEMY_X_POS,x ; load enemy's X position
    sec               ; set carry flag in preparation for subtraction
    sbc #$10
    lsr
    lsr
    lsr
    and #$1c
    ora $02
    sta $00
    rts

mouth_pit_graphic_ptr_tbl:
    .addr mouth_pit_graphic_00 ; ENEMY_FRAME,x = #$00
    .addr mouth_pit_graphic_01 ; ENEMY_FRAME,x = #$01
    .addr mouth_pit_graphic_02 ; ENEMY_FRAME,x = #$02
    .addr mouth_pit_graphic_03 ; ENEMY_FRAME,x = #$03
    .addr mouth_pit_graphic_04 ; ENEMY_FRAME,x = #$04
    .addr mouth_pit_graphic_05 ; ENEMY_FRAME,x = #$05

mouth_pit_graphic_00:
    .byte $63             ; added to current PPU address low byte
    .byte $02             ; block size
    .byte $09,$0b
    .byte $83             ; added to current PPU address low byte
    .byte $02             ; block size
    .byte $47,$0b
    .byte $ff             ; end of graphic tiles
    .byte $00,$00,$00,$00

mouth_pit_graphic_01:
    .byte $43             ; added to current PPU address low byte
    .byte $02             ; block size
    .byte $0c,$0a
    .byte $62             ; added to current PPU address low byte
    .byte $04             ; block size
    .byte $0a,$88,$89,$0c
    .byte $82             ; added to current PPU address low byte
    .byte $04             ; block size
    .byte $0a,$8a,$8b,$0c
    .byte $a3             ; added to current PPU address low byte
    .byte $02             ; block size
    .byte $0c,$0a
    .byte $ff             ; end of graphic tiles
    .byte $00,$00,$00,$00

mouth_pit_graphic_02:
    .byte $42                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $0b,$8c,$8d,$47
    .byte $61                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $0c,$8e,$8f,$90,$91,$0a
    .byte $81                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $0c,$92,$93,$94,$95,$0a
    .byte $a2                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $0b,$96,$97,$09
    .byte $ff                     ; end of graphic tiles
    .byte $00,$00,$00,$00         ; attribute bytes

mouth_pit_graphic_03:
    .byte $22                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $0b,$0c,$0a,$09
    .byte $41                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $47,$98,$99,$9a,$9b,$0b
    .byte $61                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $9c,$9d,$9e,$9f,$a0,$a1
    .byte $81                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $a2,$a3,$a4,$a5,$a6,$a7
    .byte $a1                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $09,$a8,$a9,$aa,$ab,$0b
    .byte $c2                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $0b,$0c,$0a,$47
    .byte $ff                     ; end of graphic tiles
    .byte $80,$20,$08,$02         ; attribute bytes

mouth_pit_graphic_04:
    .byte $22                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $ac,$ad,$ae,$af
    .byte $41                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $b0,$b1,$b2,$b3,$b4,$b5
    .byte $61                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $b6,$b7,$b8,$b9,$ba,$bb
    .byte $81                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $bc,$bd,$be,$bf,$c0,$c1
    .byte $a1                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $c2,$c3,$c4,$c5,$c6,$c7
    .byte $c2                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $c8,$c9,$ca,$cb
    .byte $ff                     ; end of graphic tiles
    .byte $80,$20,$08,$02         ; attribute bytes

mouth_pit_graphic_05:
    .byte $22                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $0b,$0c,$0a,$09
    .byte $41                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $47,$0b,$0c,$0a,$47,$0b
    .byte $61                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $0c,$0a,$09,$0b,$0c,$0a
    .byte $81                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $0c,$0a,$47,$0b,$0c,$0a
    .byte $a1                     ; added to current PPU address low byte
    .byte $06                     ; block size
    .byte $09,$0b,$0c,$0a,$09,$0b
    .byte $c2                     ; added to current PPU address low byte
    .byte $04                     ; block size
    .byte $0b,$0c,$0b,$0c
    .byte $ff                     ; end of graphic tiles
    .byte $00,$00,$00,$00         ; attribute bytes

; enemy type #$5e
mouth_pit_gen_routine_ptr_tbl:
    .addr mouth_pit_gen_routine_00 ; set special HP, set delay, advance routine
    .addr mouth_pit_gen_routine_01 ; main routine, executed repeatedly, also enemy destroyed routine

; set special HP, set delay, advance routine
mouth_pit_gen_routine_00:
    lda #$f0                        ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to mouth_pit_routine_01

; main routine, executed repeatedly, also enemy destroyed routine
; by having this routine be the destroyed routine, it means the mouth pit generator cannot be destroyed
mouth_pit_gen_routine_01:
    jsr update_enemy_pos          ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x             ; load enemy's Y position
    cmp #$e0
    bcc @continue                 ; branch if in top 87.5% of screen
    ldy ENEMY_VAR_2,x             ; generator is in bottom 12.5% of screen
                                  ; load screen
    bmi @remove_enemy             ; branch if 3rd screen of mouth pits
    lda mouth_pit_screen_tbl,y    ; include mouth pit in next screen, load number
    sta ENEMY_VAR_2,x             ; set mouth pit screen number, used to alter generator behavior
    lda mouth_pit_gen_y_pos_tbl,y ; load mouth pit Y position
    sta ENEMY_Y_POS,x             ; set mouth pit generator Y position

@continue:
    dec ENEMY_DELAY,x                  ; decrement mouth pit generation delay
    bne @exit                          ; exit if enemy creation delay hasn't elapsed
    jsr calc_mouth_pit_location        ; enemy creation delay elapsed
                                       ; calc mouth pit location and store in ($09, $08)
                                       ; choose a region randomly and see if empty
    lda #$01
    bcs @set_delay_exit                ; exit if mouth pit already exists at region near ($09, $0b)
                                       ; to try different position next frame
    ldy #$5d                           ; no mouth pit in region, create one
                                       ; enemy type = mouth pit
    jsr try_create_enemy_from_existing ; create mouth pit
    bcc @set_delay_exit                ; branch if unable to create mouth pit
    ldy $11                            ; load enemy slot of created mouth pit
    lda $08                            ; load newly created mouth pit Y position
    sta ENEMY_Y_POS,y                  ; set newly created mouth pit Y position
    lda $09                            ; load newly created mouth pit Y position
    sta ENEMY_X_POS,y                  ; set newly created mouth pit X position
    lda #$47                           ; set delay before creating next mouth pit

@set_delay_exit:
    sta ENEMY_DELAY,x

@exit:
    rts

@remove_enemy:
    jmp remove_enemy

mouth_pit_screen_tbl:
    .byte $01,$02,$80

mouth_pit_gen_y_pos_tbl:
    .byte $00,$c0,$00

; randomly choose a location to see if can generate a mouth pit
; if no mouth pit is at calculated location, then set output variables and exit with carry clear
; if mouth pit, exit with carry set
; input
;  * $0a
; output
;  * $09 - mouth pit creation X position
;  * $08 - mouth pit creation Y position
;  * carry - set when mouth pit exists and randomly chosen location, clear otherwise
calc_mouth_pit_location:
    lda RANDOM_NUM                   ; load random number
    lsr
    and #$07                         ; random number between #$00 and #$07
    tay
    lda mouth_pit_gen_x_offset_tbl,y
    sta $09                          ; set random x left or right of player
    lda RANDOM_NUM                   ; load random number
    adc GLOBAL_TIMER
    lsr
    lsr
    lsr
    and #$07                         ; random number between #$00 and #$07
    tay
    lda mouth_pit_gen_y_offset_tbl,y
    sta $08                          ; set random y above or below player
    inc ENEMY_VAR_1,x                ; increment number of attempts at generating a mouth pit
    lda ENEMY_VAR_1,x                ; load number of attempts at generating a mouth pit
    and #$01
    tay                              ; strip to even odd bit, selecting a random player
    lda PLAYER_GAME_OVER_STATUS,y    ; see if player y is in game over
    beq @check_x                     ; branch if not in game over
    eor #$01                         ; player is in game over (or p2 was checked and it's a 1 player game)
    tay                              ; select other player

; check if player X position + x offset in center 75% of screen
@check_x:
    lda PLAYER_SPRITE_X_POS,y ; load X position of random non-game over player
    clc                       ; clear carry in preparation for addition
    adc $09                   ; add x region position to player X position
    cmp #$20
    bcc @new_x_test           ; branch if result is in left 12.5% of screen
    cmp #$e0
    bcc @check_y              ; branch to move to y if result is in center 75% of screen

; player X position + x offset was on edge of screen, use subtraction instead
@new_x_test:
    lda #$00
    sec                       ; set carry flag in preparation for subtraction
    sbc $09
    clc                       ; clear carry in preparation for addition
    adc PLAYER_SPRITE_X_POS,y ; subtract x region from player X position

; check if player Y position + y offset in center 75% of screen
@check_y:
    and #$e0
    sta $09                   ; store x region for mouth pit
    lda PLAYER_SPRITE_Y_POS,y ; load random non-game over player's Y position
    clc                       ; clear carry in preparation for addition
    adc $08
    cmp #$e0
    bcs @new_y_test           ; branch if result is in bottom 12.5% of screen
    cmp #$38
    bcs @continue             ; branch if result is in y region 21.875% to 87.5% of screen

; player Y position + y offset was on edge of screen, use subtraction instead
@new_y_test:
    lda PLAYER_SPRITE_Y_POS,y
    sec                       ; set carry flag in preparation for subtraction
    sbc $08                   ; subtract y offset from random non-game over player Y pos

@continue:
    jsr add_y_scroll     ; add vertical scroll to Y position
    and #$e0
    sta $08              ; store y region for mouth pit
    cmp #$10
    bcc @handle_bottom   ; branch if result is in top 6.25% of screen
    cmp #$e0
    bcc @handle_y_scroll ; branch if result is between 6.25% and 87.5% of screen vertically

; at bottom of screen
@handle_bottom:
    lda $0a
    asl
    lda #$20
    bcc @handle_y_scroll
    lda #$c0

@handle_y_scroll:
    sta $0b              ; store y region where mouth pit is desired to be created
    sec                  ; set carry flag in preparation for subtraction
    sbc Y_SCROLL         ; subtract PPU vertical scroll
    bcc @handle_overflow
    cmp #$f0
    bcc @check_screen

@handle_overflow:
    sec      ; set carry flag in preparation for subtraction
    sbc #$10

@check_screen:
    sta $08                ; store y region for mouth pit
    cmp #$18
    bcc mouth_pit_exit_set ; exit if at very top of screen
    ldy ENEMY_VAR_2,x      ; load which screen the generator is on #$00, #$01, or #$80
    bmi @should_create     ; branch if lsat mouth pit generator
    bne @has_mouth_pit     ; branch if generator is on 2nd screen

; first or last screen with mouth pit generator
; check if mouth pit generation point is below mouth pit generator y location
; or if last screen of mouth pits, if so, just exit without creating a mouth pit
@should_create:
    cmp ENEMY_Y_POS,x      ; compare mouth pit y region with mouth pit generator Y position
    ror                    ; push carry to bit 7
    eor ENEMY_VAR_2,x
    bmi mouth_pit_exit_set ; exit if last screen of mouth pits
                           ; or if Y position of generation is lower than mouth pit generator Y position

@has_mouth_pit:
    jmp is_mouth_pit_in_region ; see if mouth pit exist in area near ($09, $0b)

mouth_pit_exit_set:
    sec
    rts

; determine if mouth pit in region near ($09, $0b)
; input
;  * $09 - X position of desired location for mouth pit
;  * $0b - Y position of desired location for mouth pit
; output
;  * carry - set when mouth pit already exists in region, clear otherwise
is_mouth_pit_in_region:
    ldy #$0d ; initialize loop index at last enemy slot index

@enemy_loop:
    lda ENEMY_TYPE,y
    cmp #$5d               ; compare enemy type to mouth pit
    bne @next              ; move to next enemy if current one isn't mouth pit
    lda ENEMY_Y_POS,y      ; load mouth pit Y position
    jsr add_y_scroll       ; add vertical scroll
    and #$e0               ; strip to just region
    cmp $0b                ; compare mouth pit Y position region to desired X position region
    bne @next              ; move to next enemy if current mouth pit not in desired x region
    lda ENEMY_X_POS,y
    and #$e0
    cmp $09                ; compare mouth pit X position region to desired X position region
    beq mouth_pit_exit_set ; exit with carry set
                           ; indicating a mouth pit already exists at location

@next:
    dey
    bpl @enemy_loop
    clc
    rts

; x distance left or right of player where the mouth pit may be generated in
mouth_pit_gen_y_offset_tbl:
    .byte $e0,$c0,$00,$a0,$e0,$00,$20,$c0

; y distance left or right of player where the mouth pit may be generated in
mouth_pit_gen_x_offset_tbl:
    .byte $00,$20,$40,$60,$00,$e0,$c0,$a0

; input
;  * a - Y position
; output
;  * a - the sum of the Y position and vertical scroll accounting for wrapping around
add_y_scroll:
    clc          ; clear carry in preparation for addition
    adc Y_SCROLL ; add vertical scroll to Y position
    bcs @wrap    ; branch if overflow
    cmp #$f0     ; no overflow
    bcc @exit    ; branch if result less than #$f0

@wrap:
    adc #$0f ; overflow or result greater than #$f0
             ; added #$0f more to wrap

@exit:
    rts

; enemy type #$5f
big_face_routine_ptr_tbl:
    .addr big_face_routine_00        ; initialize variables, including including player target, and initial velocity
    .addr big_face_routine_01        ; get close to player
    .addr big_face_routine_02        ; circle player until a certain part of level, then flee
    .addr big_face_routine_03        ; animate big face fleeing
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize variables, including including player target, and initial velocity
big_face_routine_00:
    lda #$08
    sta ENEMY_HP,x          ; set HP to #$08
    lda #$02
    sta ENEMY_SPRITE_ATTR,x ; set sprite palette
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcc @continue
    lda #$d0
    sta ENEMY_Y_POS,x       ; set initial Y position to #$d0 (from bottom)

@continue:
    jsr player_enemy_x_dist      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                          ; transfer closest player index to a
    sta ENEMY_VAR_1,x            ; set player index of player to target
    lda PLAYER_SPRITE_Y_POS,y
    sta ENEMY_VAR_3,x            ; set to targeted player Y position
    lda PLAYER_SPRITE_X_POS,y
    sta ENEMY_VAR_4,x            ; set to targeted player X position
    lda ENEMY_ATTRIBUTES,x
    asl
    tay
    lda big_face_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda big_face_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    jmp advance_enemy_routine    ; advance to next routine

; get close to player
big_face_routine_01:
    lda #$84                    ; sprite_84
    ldy ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @continue
    lda #$88                    ; sprite_88

@continue:
    sta ENEMY_SPRITE,x           ; set sprite to either sprite_84 or sprite_88
    lda #$03
    sta ENEMY_SPRITE_ATTR,x      ; set sprite palette
    jsr apply_velocity           ; apply enemy's velocity to its position, removing enemy if off-screen
    jsr big_face_load_player_pos ; load targeted player pos in ($09,$08)
    ldy #$00                     ; X vel = 0.00
    lda ENEMY_X_POS,x            ; load enemy's X position
    cmp $09                      ; compare to targeted player's X position
    beq @set_vel                 ; branch if player and enemy are at same X position to stop X velocity
    ldy #$02                     ; X vel = 3.00
    bcc @set_vel                 ; branch if big head to left of player to use positive X velocity
    ldy #$04                     ; X vel = -3.00

@set_vel:
    lda big_face_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda big_face_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    sec                          ; set carry flag in preparation for subtraction
    sbc $08                      ; compare to targeted player's Y position
    bcs @calc_pos
    eor #$ff                     ; big head above player
    adc #$01

@calc_pos:
    cmp #$38                    ; see if big head is within #$38 pixels vertically from player
    bcs big_face_exit           ; exit if far away vertically
    lda #$60                    ; near player above, set parameterized position to #$60 (12 o'clock)
    ldy ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @play_sound_adv_routine
    lda #$20                    ; near player, set parameterized position to #$20 (6 o'clock)

@play_sound_adv_routine:
    sta ENEMY_VAR_2,x         ; set position to either 12 o'clock or 6 o'clock
    lda #$16                  ; sound_16 (ROLL)
    jsr play_sound            ; play sound_16 (ROLL)
    jmp advance_enemy_routine ; advance to next routine

big_face_exit:
    rts

big_face_y_vel_tbl:
    .byte $00,$02 ; Y vel =  2.00
    .byte $00,$fe ; Y vel = -2.00

big_face_x_vel_tbl:
    .byte $00,$00 ; X vel =  0.00
    .byte $00,$03 ; X vel =  3.00
    .byte $00,$fd ; X vel = -3.00

; circle player until a certain part of level, then flee
big_face_routine_02:
    jsr big_face_adj_target_pos ; update target position to match targeted player's position
    lda ENEMY_VAR_2,x           ; load parameterized position along circle surrounding player [#$00-#$7f]
    clc                         ; clear carry in preparation for addition
    adc #$01                    ; move over one position
    and #$7f                    ; handle wrapping
    sta ENEMY_VAR_2,x           ; set new position
    ldy ENEMY_VAR_2,x           ; re-load position
    lda big_face_y_offset_tbl,y
    clc                         ; clear carry in preparation for addition
    adc ENEMY_VAR_3,x           ; add offset to targeted player Y position
    sta ENEMY_Y_POS,x           ; set big head Y position
    ror
    eor big_face_y_offset_tbl,y
    bmi @hide_big_head          ; branch if overflow and offset was positive
                                ; or no overflow, but offset was negative
    lda big_face_x_offset_tbl,y
    clc                         ; clear carry in preparation for addition
    adc ENEMY_VAR_4,x           ; add offset to targeted player X position
    sta ENEMY_X_POS,x           ; set enemy X position
    ror
    eor big_face_x_offset_tbl,y ; branch if overflow and offset was positive
                                ; or no overflow, but offset was negative
    bpl @continue

@hide_big_head:
    lda #$00
    sta ENEMY_SPRITE,x
    beq @check_should_leave ; always branch

@continue:
    jsr big_face_set_sprite

@check_should_leave:
    lda Y_SCREEN       ; load vertical screen index (similar to y axis on a 2-d cardinal plane)
    cmp #$05           ; see if on 5th to last screen of level 6 (contains blue floor change)
    bne big_face_exit  ; exit if not on specific screen
    lda Y_SCROLL       ; load PPU vertical scroll
    cmp #$60           ; see if at point where floor changes from gray to blue
    bcs big_face_exit  ; exit if not yet at point where big faces should leave
    lda ENEMY_SPRITE,x
    bne @flee          ; branch if enemy still visible to set flee velocity
    jmp remove_enemy

; big face stopped attacking and is leaving offscreen
; pick velocity based on current position, and use that to back out off screen
@flee:
    lda ENEMY_VAR_2,x
    clc                            ; clear carry in preparation for addition
    adc #$08                       ; move #$08 positions clockwise
    and #$7f                       ; handle wrapping around
    lsr
    lsr
    and #$1c
    tay
    lda big_face_leave_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x   ; store enemy's fractional Y velocity
    lda big_face_leave_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x    ; store enemy's fast Y velocity
    lda big_face_leave_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x   ; store enemy's fractional X velocity
    lda big_face_leave_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x    ; store enemy's fast X velocity
    jmp advance_enemy_routine      ; advance to next routine

big_face_leave_vel_tbl:
    .byte $00,$00,$00,$02 ; y vel =  0.00, x vel =  2.00
    .byte $6a,$01,$6a,$01 ; y vel =  1.41, x vel =  1.41
    .byte $00,$02,$00,$00 ; y vel =  2.00, x vel =  0.00
    .byte $6a,$01,$96,$fe ; y vel =  1.41, x vel = -1.41
    .byte $00,$00,$00,$fe ; y vel =  0.00, x vel = -2.00
    .byte $96,$fe,$96,$fe ; y vel = -1.41, x vel = -1.41
    .byte $00,$fe,$00,$00 ; y vel = -2.00, x vel =  0.00
    .byte $96,$fe,$6a,$01 ; y vel = -1.41, x vel =  1.41

; Y offset from targeted player position based on ENEMY_VAR_2,x (parameterized position [#$00-#$7f])
; reads past this table into big_face_x_offset_tbl
big_face_y_offset_tbl:
    .byte $00,$02,$05,$08,$0a,$0d,$10,$12,$15,$17,$1a,$1c,$1f,$21,$23,$25
    .byte $27,$29,$2b,$2c,$2e,$2f,$31,$32,$33,$34,$35,$36,$36,$37,$37,$37

big_face_x_offset_tbl:
    .byte $37,$37,$37,$37,$36,$36,$35,$34,$33,$32,$31,$2f,$2e,$2c,$2b,$29
    .byte $27,$25,$23,$21,$1f,$1c,$1a,$17,$15,$12,$10,$0d,$0a,$08,$05,$02
    .byte $00,$fe,$fb,$f8,$f6,$f3,$f0,$ee,$eb,$e9,$e6,$e4,$e1,$df,$dd,$db
    .byte $d9,$d7,$d5,$d4,$d2,$d1,$cf,$ce,$cd,$cc,$cb,$ca,$ca,$c9,$c9,$c9
    .byte $c9,$c9,$c9,$c9,$ca,$ca,$cb,$cc,$cd,$ce,$cf,$d1,$d2,$d4,$d5,$d7
    .byte $d9,$db,$dd,$df,$e1,$e4,$e6,$e9,$eb,$ee,$f0,$f3,$f6,$f8,$fb,$fe
    .byte $00,$02,$05,$08,$0a,$0d,$10,$12,$15,$17,$1a,$1c,$1f,$21,$23,$25
    .byte $27,$29,$2b,$2c,$2e,$2f,$31,$32,$33,$34,$35,$36,$36,$37,$37,$37

; animate big face fleeing
big_face_routine_03:
    jsr big_face_set_sprite
    jmp apply_velocity      ; apply enemy's velocity to its position, removing enemy if off-screen

big_face_set_sprite:
    lda ENEMY_VAR_2,x              ; load parameterized position along circle surrounding player [#$00-#$7f]
    clc                            ; clear carry in preparation for addition
    adc #$08                       ; move 8 positions clockwise
    and #$7f                       ; handle wrapping around
    lsr
    lsr
    lsr
    lsr                            ; every #$16 positions is a different sprite
    tay                            ; transfer sprite index to offset register
    lda big_face_sprite_tbl,y      ; load big face sprite
    sta ENEMY_SPRITE,x
    lda big_face_sprite_attr_tbl,y ; load sprite attribute (palette and horizontal flip)
    sta ENEMY_SPRITE_ATTR,x
    rts

; sprite_84, sprite_85, sprite_86, sprite_87, sprite_88
big_face_sprite_tbl:
    .byte $86,$87,$88,$87,$86,$85,$84,$85

big_face_sprite_attr_tbl:
    .byte $03,$03,$03,$43,$43,$43,$03,$03

; load targeted player pos in ($09,$08)
; input
;  * x - enemy slot index
; output
;  * $08 - player Y position
;  * $09 - player X position
big_face_load_player_pos:
    ldy ENEMY_VAR_1,x             ; load player index of player to target
    lda PLAYER_GAME_OVER_STATUS,y ; load player game over status (0 = not game over, 1 = game over)
    beq @load_player_pos          ; branch if not in game over
    tya                           ; targeted player in game over
    eor #$01                      ; swap to other player index
    tay
    sta ENEMY_VAR_1,x             ; set player index of player to target

@load_player_pos:
    lda PLAYER_SPRITE_Y_POS,y
    sta $08                   ; set targeted player Y position
    lda PLAYER_SPRITE_X_POS,y
    sta $09                   ; set targeted player X position
    rts

; update target position to match targeted player's position
; !(HUH) this is vastly overcomplicated
; can just set ENEMY_VAR_3,x and ENEMY_VAR_4,x based on $08 and $09 respectively
; and have the same effect
big_face_adj_target_pos:
    jsr big_face_load_player_pos ; load targeted player pos in ($09,$08)
    lda ENEMY_VAR_3,x            ; load targeted player Y position
    cmp $08
    beq @continue                ; branch if targeted Y position hasn't changed
    ldy #$00                     ; targeted player has moved Y position, offset Y velocity
                                 ; to continue circling player
                                 ; Y change = 1.00
    bcc @adj_target_y            ; branch to add 1 to target Y pos if player moving down
    ldy #$02                     ; player moving up, offset in opposite direction
                                 ; Y change = -1.00

; player has moved vertically, adjust target Y position
; so that big face always circles player
; player can only move 1 pixel per frame in any direction
@adj_target_y:
    lda big_face_circle_pos_change_tbl,y   ; load Y fractional acceleration (always 0)
    clc
    adc ENEMY_Y_VELOCITY_FRACT,x           ; add to existing Y fractional velocity
    sta ENEMY_Y_VELOCITY_FRACT,x           ; store enemy's new fractional Y velocity
                                           ; !(OBS) always 0 so never adjusts velocity
    lda big_face_circle_pos_change_tbl+1,y ; load ENEMY_VAR_3,x adjustment
    adc ENEMY_VAR_3,x                      ; add to target Y position
    sta ENEMY_VAR_3,x                      ; store targeted player Y position

@continue:
    lda ENEMY_VAR_3,x       ; load targeted player Y position
    sec
    sbc Y_SCROLL_SPEED
    sta ENEMY_VAR_3,x       ; set targeted player Y position adjusting for scroll
    lda ENEMY_VAR_4,x       ; load targeted player X position
    cmp $09                 ; compare to targeted player's X position
    beq @exit               ; exit if targeted player hasn't moved horizontally
    ldy #$00                ; X change = 1.00
    bcc @set_circling_x_vel
    ldy #$02                ; X change = -1.00

@set_circling_x_vel:
    lda big_face_circle_pos_change_tbl,y   ; load X fractional acceleration (always 0)
    clc
    adc ENEMY_X_VELOCITY_FRACT,x           ; add to existing Y fractional velocity
    sta ENEMY_X_VELOCITY_FRACT,x           ; store enemy's new fractional X velocity
                                           ; !(OBS) always 0 so never adjusts velocity
    lda big_face_circle_pos_change_tbl+1,y ; load ENEMY_VAR_4,x adjustment
    adc ENEMY_VAR_4,x                      ; add to target X position
    sta ENEMY_VAR_4,x                      ; store targeted player X position

@exit:
    rts

; used to adjust the Y and X velocity of big face when the player moves
; when the player moves in one direction, the velocity is adjusted in the other
; direction
big_face_circle_pos_change_tbl:
    .byte $00,$01
    .byte $00,$ff

; enemy type #$24
alien_ladybug_routine_ptr_tbl:
    .addr alien_ladybug_routine_00
    .addr alien_ladybug_routine_01
    .addr alien_ladybug_routine_02   ; wait for delay, fire projectile, go back to alien_ladybug_routine_01
    .addr enemy_explosion_routine_00 ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

alien_ladybug_routine_00:
    lda #$14
    sta ENEMY_DESTROY_ATTRS,x ; set destroyed sound to sound_14 (Z OUT)
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcc @continue
    lda #$40
    sta ENEMY_VAR_4,x
    lda #$e0
    sta ENEMY_Y_POS,x         ; set initial Y position to #$e0

@continue:
    lda #$00
    sta ENEMY_COLLISION_INDEX,x     ; set enemy's collision index (specifies collision box dimensions)
    jsr @set_target
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$20
    sta ENEMY_FIRING_DELAY,x
    lda #$30
    jmp set_delay_adv_enemy_routine ; set delay to #$30 and set routine to alien_ladybug_routine_01

@set_target:
    jsr alien_ladybug_set_target_p  ; target either a random point on bottom of screen or the closest player
    jsr copy_enemy_vars_to_zp       ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_01               ; set enemy aim direction ($0c) and rotation direction (a) targeting player $0a using quadrant_aim_dir_01
                                    ; if no player to target, use $(0b, $0c)
    lda $0c                         ; load aim direction
    sta ENEMY_VAR_1,x               ; store aim direction
    tay                             ; transfer to offset register
    jsr convert_dir_to_overhead_dir ; set ENEMY_VAR_2,x to overhead aim direction from aim direction y
    jmp alien_ladybug_set_vel       ; set the X and Y velocity based on overhead aim direction

; set target to random point or closest player and update aim to get closer to that target
alien_ladybug_set_target_p_aim:
    jsr alien_ladybug_set_target_p ; target either a random point on bottom of screen or the closest player
    jsr overhead_quad_aim_dir_01   ; determine next aim direction to get closer to that value
                                   ; then use that value to update the overhead aim direction ENEMY_VAR_2,x

; sets the alien ladybug's X and Y velocity based on ENEMY_VAR_2
; similar to overhead_soldier_set_vel
alien_ladybug_set_vel:
    lda ENEMY_VAR_2,x             ; load overhead aim direction
    asl
    asl                           ; quadruple since each entry is #$04 bytes
    tay                           ; transfer to offset register
    lda alien_ladybug_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x  ; store enemy's fractional Y velocity
    lda alien_ladybug_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x   ; store enemy's fast Y velocity
    lda alien_ladybug_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x  ; store enemy's fractional X velocity
    lda alien_ladybug_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x   ; store enemy's fast X velocity
    rts

alien_ladybug_vel_tbl:
    .byte $00,$00,$c0,$00 ; y vel =  0.00, x vel =  0.75 (crawl right)
    .byte $87,$00,$87,$00 ; y vel =  0.53, x vel =  0.53 (crawl down right)
    .byte $c0,$00,$00,$00 ; y vel =  0.75, x vel =  0.00 (crawl down)
    .byte $87,$00,$79,$ff ; y vel =  0.53, x vel = -0.53 (crawl down left)
    .byte $00,$00,$40,$ff ; y vel =  0.00, x vel = -0.75 (crawl left)
    .byte $79,$ff,$79,$ff ; y vel = -0.53, x vel = -0.53 (crawl left up)
    .byte $40,$ff,$00,$00 ; y vel = -0.75, x vel =  0.00 (crawl up)
    .byte $79,$ff,$87,$00 ; y vel = -0.53, x vel =  0.53 (crawl up right)

alien_ladybug_routine_01:
    ldy ENEMY_VAR_3,x                          ; load direction traveling when encountered bg collision
                                               ; #$01 = moving right, #$02 = moving left, #$03 = moving down, #$04 = moving up
                                               ; doesn't distinguish angular collision directions
    beq @test_collision                        ; branch to test collision when no collision
    lda alien_ladybug_collision_offset_tbl-1,y ; previous frame encountered collision
                                               ; one component of the direction when encountered collision is in ENEMY_VAR_3,x
                                               ; confirm collision exists in this direction
                                               ; for alien ladybugs walking at an angle, since ENEMY_VAR_3,x only has up/down/left/right
                                               ; sometimes that direction will not have any collision
    clc
    adc ENEMY_Y_POS,x
    sta $08                                    ; set bg collision test point Y coordinate
    lda alien_ladybug_collision_offset_tbl+3,y
    clc
    adc ENEMY_X_POS,x                          ; set bg collision test point X coordinate
    ldy $08                                    ; load bg collision test point Y coordinate
    jsr get_bg_collision                       ; get background collision code for position (a,y)
    bne @test_collision                        ; branch if collision
    ldy ENEMY_VAR_3,x                          ; collision doesn't exist in direction
    lda alien_ladybug_dir_tbl,y
    sta ENEMY_VAR_1,x                          ; set new aim direction to dir with no collision
    lda #$08
    sta ENEMY_DELAY,x
    lda #$00
    sta ENEMY_VAR_3,x                          ; found new direction, so clear provisional direction
    jmp @animate_check_fire                    ; animate sprite, if ladybug fires and if firing delay elapsed, advance routine

@test_collision:
    lda ENEMY_X_VELOCITY_FAST,x        ; load enemy's fast X velocity
    ora ENEMY_X_VELOCITY_FRACT,x
    beq @test_y_collision              ; branch if no X velocity to test collision based on Y velocity
    lda ENEMY_X_VELOCITY_FAST,x        ; bg collision, load enemy's fast X velocity
    rol
    rol
    and #$01                           ; #$00 = moving right, #$01 = moving left
    tay                                ; transfer to offset register
    lda ENEMY_X_POS,x                  ; load enemy's X position
    cmp alien_ladybug_x_boundary_tbl,y
    iny
    sty $13                            ; #$01 = moving right, #$02 = moving left
    ror                                ; push carry flag to bit 7
    eor ENEMY_X_VELOCITY_FAST,x
    bmi @check_fire                    ; branch if moving left and less than left boundary (#$10)
                                       ; or moving right and larger than right boundary (#$f0)
                                       ; this is when the alien ladybug is outside the X boundary
    lda ENEMY_X_VELOCITY_FAST,x        ; within boundary
                                       ; load enemy's fast X velocity
    asl
    lda #$08                           ; assume test collision at #$08 to the right
    ldy #$01
    bcc @test_x_collision              ; branch if moving right
    lda #$f8                           ; moving left, test collision at #$08 to the left
    iny

@test_x_collision:
    sty $13               ; #$01 moving right, #$02 moving left
    clc
    adc ENEMY_X_POS,x
    ldy ENEMY_Y_POS,x     ; load enemy's Y position
    jsr get_bg_collision  ; get background collision code for position (a,y)
    beq @test_y_collision ; branch to test collision based on Y velocity if not X velocity collision

@check_fire:
    lda ENEMY_VAR_3,x
    beq @get_new_dir
    jsr alien_ladybug_invert_direction ; flip alien direction, e.g. if moving up-left, set dir to down-right
    jmp @animate_check_fire            ; animate sprite, if ladybug fires and if firing delay elapsed, advance routine

@test_y_collision:
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    ora ENEMY_Y_VELOCITY_FRACT,x
    beq @no_collision            ; branch if no Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    asl                          ; push bit 7 (up-down dir bit) to carry
    lda #$08
    ldy #$03
    bcc @continue                ; branch if moving down
    lda #$f8                     ; moving up
    iny

@continue:
    sty $13                            ; #$03 = moving down, #$04 = moving up
    clc
    adc ENEMY_Y_POS,x                  ; add/subtract #$08 based on ladybug Y velocity
    tay                                ; move result to Y position for get_bg_collision calculation
    lda ENEMY_X_POS,x                  ; load enemy's X position
    jsr get_bg_collision               ; test background collision
    beq @no_collision                  ; branch if no background collision
    lda ENEMY_VAR_3,x                  ; background collision
    beq @target_player
    jsr alien_ladybug_invert_direction ; flip alien direction, e.g. if moving up-left, set dir to down-right
    jmp @animate_check_fire            ; animate sprite, if ladybug fires and if firing delay elapsed, advance routine

@no_collision:
    ldy ENEMY_VAR_3,x
    bne @animate_check_fire     ; animate sprite, if ladybug fires and if firing delay elapsed, advance routine
    dec ENEMY_DELAY,x
    beq @set_target_and_options ; branch if delay elapsed to set target and targeting delay based on targeting options

; animate sprite based on delay, see if ladybug fires, and if firing delay elapsed, advance routine
; input
;  * x - enemy slot offset
@animate_check_fire:
    jsr alien_ladybug_animate ; update animation based on animation timer
                              ; when delay elapsed, update sprite and frame for next animation
    jsr apply_velocity        ; apply enemy's velocity to its position, removing enemy if off-screen
    lda ENEMY_ATTRIBUTES,x
    and #$02                  ; strip to just the firing flag
    beq @exit                 ; exit if alien ladybug does not fire projectiles
    dec ENEMY_FIRING_DELAY,x  ; alien ladybug fires projectiles, decrement firing delay timer
    bne @exit                 ; exit if firing delay hasn't elapsed
    lda #$10
    sta ENEMY_FIRING_DELAY,x  ; reset delay for next firing
    jmp advance_enemy_routine ; advance routine to alien_ladybug_routine_02

@exit:
    rts

@target_player:
    jsr alien_ladybug_target_from_options ; set target ($0b, $0c) based on targeting options ENEMY_VAR_4,x
    lda ENEMY_Y_POS,x                     ; load enemy's Y position
    cmp $0c                               ; compare target Y position to enemy Y position
    ror                                   ; move carry to bit 7 (set when Y position is below target)
    eor ENEMY_Y_VELOCITY_FAST,x           ; set negative flag if negative Y velocity or below target, but not both
                                          ; other bits don't matter, only negative flag
    bmi @set_target_and_options           ; branch if below target and moving down, or above target and moving up
    lda ENEMY_X_POS,x                     ; moving towards target, load enemy's X position
    cmp $0b                               ; compare to target X position
    lda #$00                              ; crawl right
    bcc @set_dir_vel1                     ; branch if to the left of target position to crawl right
    lda #$01                              ; crawl left

@set_dir_vel1:
    jmp alien_ladybug_set_dir_vel ; set aim direction and velocity based on a

; ran into wall
@get_new_dir:
    jsr alien_ladybug_target_from_options ; set target ($0b, $0c) based on targeting options ENEMY_VAR_4,x
    lda ENEMY_X_POS,x                     ; load enemy's X position
    cmp $0b                               ; compare enemy X position to target X position
    ror                                   ; push cmp result into bit 7
                                          ; if ENEMY_X_POS,x is greater than or equal to target X position, sets bit 7
                                          ; otherwise clear bit 7
    eor ENEMY_X_VELOCITY_FAST,x
    bmi @set_target_and_options           ; branch if either enemy moving left and target is to right or
                                          ; enemy moving right and target is to the left
    lda ENEMY_Y_POS,x                     ; collided with bg but target is in horizontal direction of collision
                                          ; load enemy's Y position
    cmp $0c                               ; compare enemy Y position to target Y position
    lda #$02                              ; default assume enemy above target and crawl down
    bcc @set_dir_vel2                     ; branch if enemy above target
    lda #$03                              ; enemy below target, crawl up

@set_dir_vel2:
    jmp alien_ladybug_set_dir_vel ; set aim direction and velocity based on a

; set target options, target, and targeting delay based on targeting options
; for example, if below player, target bottom of screen
@set_target_and_options:
    jsr alien_ladybug_target_from_options ; set target ($0b, $0c) based on targeting options ENEMY_VAR_4,x
    lda ENEMY_Y_POS,x                     ; load enemy's Y position
    cmp $0c                               ; compare enemy Y position to target Y position
    bcc @prep_targeting                   ; branch if enemy above target Y position
    lda ENEMY_VAR_4,x                     ; enemy below target Y position, load targeting options
    asl                                   ; discard bit 7
    bmi @set_target_p_with_delay          ; branch if bit 6 set to keep existing target
    lda #$80                              ; bit 6 not set to target random point on bottom of screen
    bne @set_targeting_options            ; alway branch to target random point on bottom of screen

@prep_targeting:
    lda ENEMY_VAR_4,x ; load targeting options
    and #$bf

@set_targeting_options:
    sta ENEMY_VAR_4,x ; set targeting options

; target either closest player or random point on bottom of screen
; then set a random delay for when to refresh target
; aim towards target, and set appropriate velocity
@set_target_p_with_delay:
    lda FRAME_COUNTER                  ; load frame counter
    sbc RANDOM_NUM
    sta RANDOM_NUM                     ; randomize random number
    lda RANDOM_NUM                     ; load random number
    and #$07                           ; keep random number between 0 and 7 inclusively
    tay                                ; transfer delay index to offset register
    lda alien_ladybug_aim_delay_tbl,y
    sta ENEMY_DELAY,x                  ; set delay until re-target
    jsr alien_ladybug_set_target_p_aim ; set target to random point or closest player and update aim to get closer to that target
    jmp update_enemy_pos               ; adjust position based on scroll (does not apply velocity)

; set aim direction and velocity based on a
alien_ladybug_set_dir_vel:
    tay                               ; transfer to offset register
    lda alien_ladybug_dir_tbl+1,y
    sta ENEMY_VAR_1,x
    lda alien_ladybug_dir_index_tbl,y ; load direction (up, down, left, or right)
    sta ENEMY_VAR_2,x                 ; store direction
    lda $13                           ; load direction when encountered bg collision
    sta ENEMY_VAR_3,x                 ; #$01 = moving right, #$02 = moving left, #$03 = moving down, #$04 = moving up
    jsr alien_ladybug_set_vel         ; set the X and Y velocity based on overhead aim direction
    jmp update_enemy_pos              ; adjust position based on scroll (does not apply velocity)

alien_ladybug_collision_offset_tbl:
    .byte $00 ; right Y offset
    .byte $00 ; left Y offset
    .byte $08 ; down Y offset
    .byte $f8 ; up Y offset
    .byte $08 ; right X offset
    .byte $f8 ; left X offset
    .byte $00 ; down X offset

alien_ladybug_dir_tbl:
    .byte $00 ; up X offset (goes with previous table)
    .byte $00 ; #$00 - crawl right
    .byte $0c ; #$01 - crawl left
    .byte $06 ; #$02 - crawl down
    .byte $12 ; #$03 - crawl up

alien_ladybug_dir_index_tbl:
    .byte $00 ; #$00 - crawl right
    .byte $04 ; #$01 - crawl left
    .byte $02 ; #$02 - crawl down
    .byte $06 ; #$03 - crawl up

alien_ladybug_x_boundary_tbl:
    .byte $f0 ; moving right
    .byte $10 ; moving left

alien_ladybug_aim_delay_tbl:
    .byte $1a,$08,$18,$20,$02,$1c,$05,$0e

; flip alien ladybug direction, e.g. if moving up-left, set dir to down-right
; input
;  * x - enemy slot offset
alien_ladybug_invert_direction:
    lda ENEMY_VAR_1,x ; load current aim direction
    clc
    adc #$0c          ; invert direction, e.g. down moves to up, left moves to right, etc.
    cmp #$18          ; see if past last possible direction
    bcc @continue     ; branch if not past last direction
    sbc #$18          ; wrap direction back around so it is the opposite direction
                      ; e.g. #$11 (up) becomes #$05 (down)

@continue:
    sta ENEMY_VAR_1,x             ; update aim direction
    lda ENEMY_VAR_2,x             ; load original overhead aim direction
    clc
    adc #$04                      ; invert overhead direction, e.g. down moves to up, left moves to right, etc.
    cmp #$08                      ; see if past last possible overhead direction
    bcc @set_overhead_dir_and_vel ; branch if not past last direction
    sbc #$08                      ; wrap direction back around so it is the opposite direction
                                  ; e.g. #$06 (up) becomes #$02 (down)

@set_overhead_dir_and_vel:
    sta ENEMY_VAR_2,x         ; set overhead aim direction
    jmp alien_ladybug_set_vel ; set the X and Y velocity based on overhead aim direction

; target either a random point on bottom of screen or the closest player
; based on targeting options (ENEMY_VAR_4)
; input
;  * x - enemy offset slot
; output
;  * $0a - targeting options
;  * $0b - x point to target
;  * $0c - y point to target
alien_ladybug_set_target_p:
    jsr alien_ladybug_random_down_target ; target random point ($0b, $0c) on bottom of screen
    lda ENEMY_VAR_4,x                    ; load targeting options
    bmi @exit                            ; branch if bit 7 set to target randomly bottom of screen
    jsr player_enemy_x_dist              ; bit 7 clear, target closest player
                                         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    sty $0a                              ; set to closest player
    lda ENEMY_VAR_4,x
    and #$fe                             ; strip targeting player
    ora $0a                              ; set player to target
    sta ENEMY_VAR_4,x                    ; update targeting options

@exit:
    sta $0a
    rts

; sets target ($0b, $0c) based on targeting options (ENEMY_VAR_4,x)
; either targets a specific player or random point on bottom of screen
; input
;  * ENEMY_VAR_4,x - player index of player to target
; output
;  * $0b - X position to target
;  * $0c - Y position to target
alien_ladybug_target_from_options:
    jsr alien_ladybug_random_down_target ; target random point ($0b, $0c) on bottom of screen
    lda ENEMY_VAR_4,x
    bmi @exit                            ; exit if should target random point on bottom of screen
    and #$01                             ; strip to targeting player
    tay                                  ; transfer to offset register
    lda PLAYER_SPRITE_Y_POS,y            ; load targeting player Y position
    sta $0c                              ; store Y position target in $0c
    lda PLAYER_SPRITE_X_POS,y            ; load targeting player X position
    sta $0b                              ; store X position target in $0b

@exit:
    rts

; targets a random point on the bottom of the screen
; output
;  * $0b - X position to target
;  * $0c - Y position to target
alien_ladybug_random_down_target:
    lda RANDOM_NUM                 ; load random number
    and #$03
    tay                            ; transfer to offset register
    lda alien_ladybug_target_tbl,y
    sta $0b
    lda #$ff                       ; load y target position
    sta $0c                        ; bottom of screen
    rts

alien_ladybug_target_tbl:
    .byte $20 ; (#$20, #$ff)
    .byte $60 ; (#$60, #$ff)
    .byte $a0 ; (#$a0, #$ff)
    .byte $e0 ; (#$e0, #$ff)

; initialize animation, wait for firing delay to get to #$0c, fire, and lower delay to #$08, go back to alien_ladybug_routine_01
alien_ladybug_routine_02:
    jsr alien_ladybug_init_animation ; initialize animation and animation timer, starting with frame 0
    jsr update_enemy_pos             ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x         ; see if post fir delay has elapsed
    beq @set_long_delay              ; set longer delay and go back to alien_ladybug_routine_01
    lda ENEMY_FIRING_DELAY,x
    cmp #$0c
    bne @exit
    lda #$06
    sta ENEMY_VAR_5,x
    lda ENEMY_VAR_4,x                ; see if targeting a specific player
    bpl @fire                        ; branch if targeting a specific player
    jsr player_enemy_x_dist          ; not targeting any specific player, target closest
                                     ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)

@fire:
    sta $0a                   ; store distance in $0a
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                  ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    lda #$08
    sta ENEMY_FIRING_DELAY,x  ; lower delay from #$0c (firing point) down to #$08
                              ; once delay goes from #$08 to #$00, go back to alien_ladybug_routine_01

@exit:
    rts

@set_long_delay:
    lda #$40
    sta ENEMY_FIRING_DELAY,x ; set longer delay
    lda #$02
    jmp set_enemy_routine    ; set routine to alien_ladybug_routine_01

; initialize animation and animation timer, starting with frame 0
; input
;  * x - enemy slot index
alien_ladybug_init_animation:
    lda #$00
    sta ENEMY_FRAME,x
    bne alien_ladybug_set_sprite ; !(HUH) this will never happen, zero flag set in lda instruction
                                 ; set alien ladybug sprite and sprite attribute based on ENEMY_FRAME,x and y

; update animation based on animation timer
; when delay elapsed, update sprite and frame for next animation
; input
;  * x - enemy slot index
alien_ladybug_animate:
    inc ENEMY_ANIMATION_DELAY,x
    lda ENEMY_ANIMATION_DELAY,x
    cmp #$08
    bcc @set_sprite
    lda #$00
    sta ENEMY_ANIMATION_DELAY,x
    inc ENEMY_FRAME,x
    lda ENEMY_FRAME,x
    and #$01
    sta ENEMY_FRAME,x

@set_sprite:
    ldy ENEMY_VAR_2,x ; load overhead aim direction

; set alien ladybug sprite and sprite attribute based on ENEMY_FRAME,x and y
; input
;  * y - overhead aim direction
;  * x - enemy slot index
alien_ladybug_set_sprite:
    lda alien_ladybug_sprite_attr_tbl,y
    sta ENEMY_SPRITE_ATTR,x             ; set sprite attribute based on overhead aim direction
    tya                                 ; transfer overhead aim direction to a
    asl                                 ; double since each direction has two animation frames
    clc                                 ; clear carry in preparation for addition
    adc ENEMY_FRAME,x                   ; add enemy frame (#$00 or #$01)
    tay                                 ; transfer to offset register
    lda alien_ladybug_sprite_tbl,y
    sta ENEMY_SPRITE,x                  ; set sprite based on overhead aim direction and frame
    rts

alien_ladybug_sprite_attr_tbl:
    .byte $40,$40,$00,$00,$00,$00,$00,$40

; two frames per aim direction
; sprite_7c, sprite_7d, sprite_7a, sprite_7b, sprite_78, sprite_79
; sprite_7d, sprite_7e, sprite_7f, sprite_80, sprite_81
alien_ladybug_sprite_tbl:
    .byte $7c,$7d ; overhead aim dir #$00 - crawl right
    .byte $7a,$7b ; overhead aim dir #$01 - crawl down right
    .byte $78,$79 ; overhead aim dir #$02 - crawl down
    .byte $7a,$7b ; overhead aim dir #$03 - crawl down left
    .byte $7c,$7d ; overhead aim dir #$04 - crawl left
    .byte $7e,$7f ; overhead aim dir #$05 - crawl left up
    .byte $80,$81 ; overhead aim dir #$06 - crawl up
    .byte $7e,$7f ; overhead aim dir #$07 - crawl up right

; enemy type #$60
baby_alien_ladybug_routine_ptr_tbl:
    .addr baby_alien_ladybug_routine_00
    .addr baby_alien_ladybug_routine_01
    .addr baby_alien_ladybug_routine_02
    .addr baby_alien_ladybug_routine_03
    .addr baby_alien_ladybug_routine_04 ; enemy destroyed routine
    .addr baby_alien_ladybug_routine_05
    .addr baby_alien_ladybug_routine_06

baby_alien_ladybug_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x       ; bullets travel through enemy and player can collide
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    clc                             ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_VAR_1,x
    jsr baby_alien_ladybug_handle_y
    jmp advance_enemy_routine       ; advance to next routine

baby_alien_ladybug_routine_01:
    lda #$01
    sta ENEMY_SPRITE_ATTR,x
    jsr baby_alien_ladybug_handle_y
    lda ENEMY_VAR_1,x
    cmp #$10
    bcc baby_alien_ladybug_exit
    cmp #$ff
    bne @continue
    jmp remove_enemy

@continue:
    sta ENEMY_Y_POS,x
    lda ENEMY_ATTRIBUTES,x
    and #$01
    tay
    lda baby_alien_ladybug_spawn_x_tbl,y
    sta ENEMY_X_POS,x
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x            ; enable player-enemy and player bullet-enemy collision
    lda #$00
    sta ENEMY_COLLISION_INDEX,x          ; set enemy's collision index (specifies collision box dimensions)
    ldy #$0d                             ; animation index for baby alien ladybug
    jsr set_enemy_animation_sprite       ; animate sprite for baby alien ladybug based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    lda #$32
    jmp set_delay_adv_enemy_routine      ; set delay to #$30 and set routine to alien_ladybug_routine_01

baby_alien_ladybug_exit:
    rts

baby_alien_ladybug_spawn_x_tbl:
    .byte $e0,$20

baby_alien_ladybug_routine_02:
    ldy #$0d                        ; animation index for baby alien ladybug
    jsr set_enemy_animation_sprite  ; animate sprite for baby alien ladybug based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr baby_alien_ladybug_handle_y
    dec ENEMY_DELAY,x
    bne baby_alien_ladybug_exit
    jsr player_enemy_x_dist         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    sec                             ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_Y_POS,y
    ldy #$00
    bcc @handle_overflow            ; branch if player below enemy
    ldy #$04                        ; player above enemy

@handle_overflow:
    bcs @check_if_far
    eor #$ff
    adc #$01

@check_if_far:
    cmp #$20
    bcs @check_y_scroll
    ldy #$02

@check_y_scroll:
    lda Y_SCROLL_SPEED
    beq @check_side
    iny
    iny

@check_side:
    lda ENEMY_ATTRIBUTES,x
    lsr
    bcc @set_vel           ; branch if enemy on right side
    iny

@set_vel:
    tya
    asl
    asl                                ; quadruple since each entry has #$04 bytes
    tay                                ; transfer to offset register
    lda baby_alien_ladybug_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x       ; store enemy's fractional Y velocity
    lda baby_alien_ladybug_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x        ; store enemy's fast Y velocity
    lda baby_alien_ladybug_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x       ; store enemy's fractional X velocity
    lda baby_alien_ladybug_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x        ; store enemy's fast X velocity
    jmp advance_enemy_routine          ; advance to next routine

baby_alien_ladybug_vel_tbl:
    .byte $81,$00,$a6,$fe ; Y velocity =  0.5039, X velocity = -1.3516
    .byte $81,$00,$5a,$01 ; Y velocity =  0.5039, X velocity =  1.3516
    .byte $00,$00,$80,$fe ; Y velocity = -0.0000, X velocity = -1.5000
    .byte $00,$00,$80,$01 ; Y velocity = -0.0000, X velocity =  1.5000
    .byte $7f,$ff,$a6,$fe ; Y velocity = -0.5039, X velocity = -1.3516
    .byte $7f,$ff,$5a,$01 ; Y velocity = -0.5039, X velocity =  1.3516
    .byte $7f,$ff,$a6,$fe ; Y velocity = -0.5039, X velocity = -1.3516
    .byte $7f,$ff,$5a,$01 ; Y velocity = -0.5039, X velocity =  1.3516

baby_alien_ladybug_routine_03:
    ldy #$0d                                    ; animation index for baby alien ladybug
    jsr set_enemy_animation_sprite              ; animate sprite for baby alien ladybug based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr baby_alien_ladybug_apply_vel
    jsr baby_alien_ladybug_x_edge_detect_narrow
    bpl baby_alien_ladybug_exit2

; set enemy collision code, type, initialize frame, and set routine to baby_alien_ladybug_routine_01
baby_alien_ladybug_init:
    ldy #$60                         ; enemy type #$60 (baby alien ladybug)
    jsr set_enemy_collision_and_type ; set the collision box type, and enemy type based on y
    lda #$02
    sta ENEMY_ROUTINE,x              ; set routine to baby_alien_ladybug_routine_01
    lda #$00
    sta ENEMY_FRAME,x

baby_alien_ladybug_exit2:
    rts

; enemy destroyed routine
baby_alien_ladybug_routine_04:
    jsr enemy_explosion_routine_00     ; set empty sprite, play optional enemy destroyed sound, disable collisions
    jmp baby_alien_ladybug_check_spawn ; set ENEMY_VAR_1,x (spawn Y coordinate) and if destroyed re-spawn

baby_alien_ladybug_routine_05:
    jsr enemy_explosion_routine_01     ; animate explosion sequence
    jmp baby_alien_ladybug_check_spawn ; set ENEMY_VAR_1,x (spawn Y coordinate) and if destroyed re-spawn

baby_alien_ladybug_routine_06:
    jsr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set ENEMY_VAR_1,x (spawn Y coordinate) and if destroyed re-spawn
baby_alien_ladybug_check_spawn:
    jsr baby_alien_ladybug_check_y_bound
    lda ENEMY_ROUTINE,x
    beq baby_alien_ladybug_init          ; branch to reset enemy if destroyed
    rts

; apply velocity, check if spawn point scrolled to bottom and if so, mark to not
; be re-spawned
baby_alien_ladybug_apply_vel:
    jsr apply_x_velocity
    lda ENEMY_Y_VEL_ACCUM,x
    clc                                ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x
    sta ENEMY_Y_VEL_ACCUM,x
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    adc ENEMY_Y_VELOCITY_FAST,x
    jmp baby_alien_ladybug_set_spawn_y

baby_alien_ladybug_handle_y:
    lda ENEMY_Y_POS,x ; load enemy's Y position

baby_alien_ladybug_set_spawn_y:
    sec                                  ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED
    sta ENEMY_Y_POS,x                    ; set Y position
    cmp #$e8
    bcc baby_alien_ladybug_check_y_bound ; branch if not at very bottom of screen
    jsr baby_alien_ladybug_init

; adjusts ENEMY_VAR_1,x (spawn Y coordinate) based on Y scroll
; if scrolled to bottom of screen, mark enemy to not be re-spawned
baby_alien_ladybug_check_y_bound:
    lda ENEMY_VAR_1,x
    cmp #$ff
    beq baby_alien_ladybug_exit2
    sec                          ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED
    sta ENEMY_VAR_1,x
    cmp #$e0
    bcc baby_alien_ladybug_exit2
    lda #$ff                     ; scrolled to bottom of screen
    sta ENEMY_VAR_1,x            ; set enemy to be removed and not re-spawned
    rts

; enemy type #$61
boss_baby_alien_ladybug_routine_ptr_tbl:
    .addr boss_baby_alien_ladybug_routine_00
    .addr boss_baby_alien_ladybug_routine_01
    .addr enemy_explosion_routine_00         ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01         ; animate explosion sequence
    .addr enemy_explosion_routine_03         ; mark destroyed, remove enemy

boss_baby_alien_ladybug_routine_00:
    lda #$06
    sta ENEMY_VAR_1,x
    lda #$01
    sta ENEMY_SPRITE_ATTR,x
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to boss_baby_alien_ladybug_routine_01

boss_baby_alien_ladybug_routine_01:
    ldy #$0d                               ; animation index for baby alien ladybug
    jsr set_enemy_animation_sprite         ; animate sprite for baby alien ladybug based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr baby_alien_ladybug_check_collision
    jsr apply_velocity                     ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_DELAY,x
    bne @exit
    lda #$18
    sta ENEMY_DELAY,x
    jsr player_enemy_x_dist                ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$01                               ; speed adjust code for 0.75x speed
    jmp hone_to_player_set_enemy_vel       ; determine next aim dir to get closer to player index ($0a)
                                           ; and set enemy velocity to that target direction

@exit:
    rts

baby_alien_ladybug_check_collision:
    lda #$08                    ; assume moving right with #$08 x offset
    ldy ENEMY_X_VELOCITY_FAST,x ; load enemy's fast X velocity
    bpl @continue               ; branch if moving right
    lda #$f8                    ; moving left, will subtract #$08

@continue:
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_X_POS,x                    ; add/subtract 8 from X position
    ldy ENEMY_Y_POS,x                    ; load enemy's Y position
    jsr get_bg_collision                 ; get background collision code for position (a,y)
    cmp #$02                             ; see if enemy on land
    bcs @stop_x_vel                      ; branch to stop X velocity if not on land nor floating platform
    lda #$02
    jsr baby_alien_ladybug_x_edge_detect
    bpl @check_collision                 ; branch if moving to right, or not at a defined edge

@stop_x_vel:
    jsr clear_enemy_x_vel ; set enemy X velocity (fast and fractional) to 0
    lda #$01
    sta ENEMY_DELAY,x     ; delay for one frame

@check_collision:
    lda #$08                    ; assume moving down
    ldy ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @set_y_pos              ; branch if moving down
    lda #$f8                    ; moving up screen, will subtract #$08

@set_y_pos:
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x                    ; add/subtract #$08 from Y position
    tay
    lda ENEMY_X_POS,x                    ; load enemy's X position
    jsr get_bg_collision                 ; get background collision code for position (a,y)
    cmp #$02
    bcs @stop_y_vel
    jsr baby_alien_ladybug_y_edge_detect
    bpl @exit

@stop_y_vel:
    jsr clear_enemy_y_vel ; set enemy Y velocity to 0
    lda #$01
    sta ENEMY_DELAY,x

@exit:
    rts

; determine direction of enemy X velocity based on a narrower screen edge
; input
;  * x - enemy slot index
; output
;  * negative flag - set when past defined edge, or if fast X velocity is negative
baby_alien_ladybug_x_edge_detect_narrow:
    lda #$00

; determine direction of enemy X velocity based on screen edge
; input
;  * a - boss_screen_baby_alien_x_edge_tbl starting offset (screen edge extremes)
;  * x - enemy slot index
; output
;  * negative flag - set when past defined edge, or if fast X velocity is negative
baby_alien_ladybug_x_edge_detect:
    sta $08                                 ; store screen edge extremes index
    lda ENEMY_X_VELOCITY_FAST,x             ; load enemy's fast X velocity
    rol
    rol                                     ; push direction bit to bit 0
    and #$01                                ; strip to just direction (0 = right, 1 = left)
    clc                                     ; clear carry in preparation for addition
    adc $08                                 ; add screen edge index
    tay                                     ; transfer to offset register
    lda ENEMY_X_POS,x                       ; load enemy's X position
    cmp boss_screen_baby_alien_x_edge_tbl,y ; compare X position to edge of screen
    ror                                     ; move carry to bit 7 (set when X position is past screen edge)
                                            ; effectively swap direction if past edge
    eor ENEMY_X_VELOCITY_FAST,x             ; set negative flag if negative velocity and past left edge
                                            ; or positive velocity and past right edge
    rts

; determine direction of enemy Y velocity based on screen edge
; input
;  * x - enemy slot index
; output
;  * negative flag - set when at past vertical (top/bottom) edge
baby_alien_ladybug_y_edge_detect:
    lda ENEMY_Y_VELOCITY_FAST,x             ; load enemy's fast Y velocity
    rol
    rol                                     ; push direction bit to bit 0
    and #$01                                ; strip to just direction (0 = down, 1 = up)
    tay                                     ; transfer to offset register
    lda ENEMY_Y_POS,x                       ; load enemy's Y position
    cmp boss_screen_baby_alien_y_edge_tbl,y ; compare Y position to edge of screen
    ror                                     ; move carry to bit 7 (set when Y position is past screen edge)
                                            ; effectively swap direction if past edge
    eor ENEMY_Y_VELOCITY_FAST,x             ; set negative flag if negative velocity and past top edge
                                            ; or positive velocity and past bottom edge
    rts

boss_screen_baby_alien_x_edge_tbl:
    .byte $e0,$20 ; narrow
    .byte $f8,$08 ; wide

boss_screen_baby_alien_y_edge_tbl:
    .byte $d0,$18

; enemy type #$70
suspicious_face_arm_routine_ptr_tbl:
    .addr suspicious_face_arm_routine_00 ; update position, advance routine
    .addr suspicious_face_arm_routine_01 ; repeatedly create baby alien ladybugs after a delay
    .addr remove_enemy                   ; enemy destroyed routine

; update position, advance routine
suspicious_face_arm_routine_00:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$70
    jmp set_delay_adv_enemy_routine ; set delay to #$70 and set routine to suspicious_face_arm_routine_01

; repeatedly create baby alien ladybugs after a delay
suspicious_face_arm_routine_01:
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x         ; decrement delay
    bne @exit                 ; exit if delay hasn't elapsed
    ldy #$61                  ; enemy type = #$61 (Boss Screen Baby Alien Ladybug)
    sty $17                   ; store enemy type in $17
    ldx #$05                  ; create enemy in first #$05 slots
    jsr create_and_init_enemy ; create boss screen baby alien ladybug
    jsr copy_enemy_vars       ; copy some enemy variable from suspicious face arm to created baby alien ladybug
    lda #$f0                  ; set next delay to #$f0
    sta ENEMY_DELAY,x         ; set next delay

@exit:
    rts

; enemy type #$71
area_6_chr_swap_routine_ptr_tbl:
    .addr area_6_chr_swap_routine_00 ; update pattern table, set initial scanline irq and irq type
    .addr area_6_chr_swap_routine_01 ; enemy destroyed routine, update scanline based on vertical scroll, remove once scrolled away

; update pattern table, set initial scanline irq and irq type
area_6_chr_swap_routine_00:
    lda #$f0                        ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda ENEMY_ATTRIBUTES,x
    and #$01                        ; keep just the location bit
                                    ; (0 = before first door, 1 = suspicious face and jagger froid)
    tay
    lda left_top_chr_banks_tbl,y
    sta LEFT_TOP_HALF_CHR_BANK      ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda left_bottom_chr_banks_tbl,y
    sta LEFT_BOTTOM_CHR_HALF_BANK   ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda area_6_irq_type_tbl,y
    sta IRQ_TYPE                    ; set irq routine type to either irq_handler_0e_ptr_tbl or irq_handler_0f_ptr_tbl
                                    ; level 6 before first door or level 6 miniboss (suspicious face) and boss (jagger froid)
    lda area_6_scanline_irq_tbl,y   ; load initial scanline for triggering interrupt
    jsr area_6_set_scanline_irq     ; adjust scanline position based on vertical scroll
    jmp advance_enemy_routine       ; advance to next routine

; left pattern table top half banks
left_top_chr_banks_tbl:
    .byte $50,$54

; left pattern table bottom half banks
left_bottom_chr_banks_tbl:
    .byte $52,$56

; irq types
area_6_irq_type_tbl:
    .byte $0e ; irq_handler_0e_ptr_tbl
    .byte $0f ; irq_handler_0f_ptr_tbl

; initial scanline irq
area_6_scanline_irq_tbl:
    .byte $06,$08

; enemy destroyed routine, update scanline based on vertical scroll, remove once scrolled away
area_6_chr_swap_routine_01:
    lda SCANLINE_IRQ_1 ; load scanline where irq is triggered

; update scanline irq to account for vertical frame scroll
; input
;  * a - current location for scanline irq
area_6_set_scanline_irq:
    sec                      ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED       ; subtract any vertical scroll for this frame from the enemy fast velocity
    sta SCANLINE_IRQ_1       ; update scanline irq line
    cmp #$f0                 ; see if irq is at bottom of screen
    bcs @remove_enemy        ; branch if irq line is almost scrolled off screen to remove
    lda LEVEL_Y_SCROLL_FLAGS
    bmi @exit                ; exit if cannot scroll up
    jmp init_irq_scroll      ; set post-irq scroll to current scroll

@remove_enemy:
    jsr remove_enemy
    jmp set_nmi_noop_irq ; remove any scanline interrupts

@exit:
    rts

; enemy type #$65
jagger_froid_routine_ptr_tbl:
    .addr jagger_froid_routine_00       ; set destroyed attributes, set special HP, set CHR tile banks, disable palette cycling, set palette, advance routine
    .addr jagger_froid_routine_01
    .addr jagger_froid_routine_02
    .addr jagger_froid_routine_03       ; earthquake, time playing boss music, wait for delay, initialize enemy, advance routine
    .addr jagger_froid_routine_04
    .addr jagger_froid_routine_05
    .addr jagger_froid_routine_06       ; wait for delay set real HP, advance routine
    .addr jagger_froid_routine_07
    .addr jagger_froid_routine_08       ; enemy destroyed routine
    .addr jagger_froid_routine_09
    .addr jagger_froid_routine_0a
    .addr bg_enemy_explosion_routine_01 ; animate sequence of explosions
    .addr jagger_froid_routine_0c       ; set boss defeated flag, strip alive attribute bit, and remove enemy

; set destroyed attributes, set special HP, set CHR tile banks, disable palette cycling, set palette, advance routine
jagger_froid_routine_00:
    lda #$10
    sta ENEMY_DESTROY_ATTRS,x       ; enable collision, set destroy sound to sound_26 (B OUT) - boss destroy
    lda #$f0                        ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$58
    sta LEFT_BOTTOM_CHR_HALF_BANK   ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda #$11
    sta RIGHT_FOURTH_QTR_CHR_BANK   ; set bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    lda #$ff
    sta PALETTE_CYCLE_INDEX_2       ; disable secondary palette cycling
    ldy #$9c                        ; white, light gray, dark red
    jsr set_level_palette           ; set background palette 2 if boss not defeated
    ldy #$a0                        ; white, light gray, medium red
    jsr set_level_palette           ; set background palette 3 if boss not defeated
    ldx ENEMY_CURRENT_SLOT
    lda #$70
    jmp set_delay_adv_enemy_routine ; set delay to #$70 and set routine to jagger_froid_routine_02

jagger_froid_routine_01:
    lda GLOBAL_TIMER
    lsr
    bcc jagger_froid_exit     ; exit if even frame
    dec ENEMY_DELAY,x
    bne jagger_froid_exit     ; exit if delay hasn't elapsed
    lda #$00
    sta NT_CURRENT_COLUMN
    lda #$02
    sta SCREEN_SCROLL_TYPE
    lda #$08
    sta ENEMY_VAR_1,x
    jmp advance_enemy_routine ; advance to next routine

jagger_froid_routine_02:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne @exit               ; exit if draw routines aren't complete
    dec ENEMY_VAR_1,x
    beq @continue
    lda #$00
    sta X_SCROLL_DRAW_POINT

@exit:
    rts

@continue:
    lda #$01
    sta ENEMY_ANIMATION_DELAY,x
    lda SCANLINE_IRQ_1              ; load scanline where interrupt will occur (irq_handler_0f_00)
    sta ENEMY_VAR_1,x               ; set scanline where interrupt will occur
    lda #$b0
    jmp set_delay_adv_enemy_routine ; set delay to #$b0 and set routine to jagger_froid_routine_03

; earthquake, time playing boss music, wait for delay, initialize enemy, advance routine
jagger_froid_routine_03:
    jsr earthquake_shake              ; shake vertically and horizontally based on GLOBAL_TIMER
    jsr jagger_froid_set_scanline_irq ; adjust scanline irq for any earthquake vertical scroll change
    lda GLOBAL_TIMER
    lsr
    bcc jagger_froid_exit
    lda ENEMY_DELAY,x
    cmp #$60
    bne @continue                     ; branch if not time to play boss music
    lda #$30                          ; delay is exactly #$60, play boss music
                                      ; sound_30 (GREAT) - Great Heli - Ruined Base (Boss 1)
    jsr play_sound                    ; play sound_30 (GREAT)

@continue:
    dec ENEMY_DELAY,x
    bne jagger_froid_exit
    lda #$38                        ; delay elapsed, initialize enemy
    sta ENEMY_Y_POS,x
    lda #$80
    sta ENEMY_X_POS,x
    lda #$00
    sta ENEMY_VAR_4,x
    lda #$01
    sta ENEMY_VAR_3,x
    sta ENEMY_VAR_5,x
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to jagger_froid_routine_04

jagger_froid_exit:
    rts

; adjusts scanline location based on vertical scroll, used to ensure the
; scanline occurs at the same spot during earthquake shakes where the vertical
; scroll is rapidly set between #$00 and #$01
jagger_froid_set_scanline_irq:
    lda ENEMY_VAR_1,x  ; load interrupt scanline location for bg tile swapping (irq_handler_0f_00)
    sec                ; set carry flag in preparation for subtraction
    sbc Y_SCROLL       ; subtract PPU vertical scroll
    sta SCANLINE_IRQ_1
    rts

; animate jagger froid coming out of cave
jagger_froid_routine_04:
    jsr earthquake_shake                     ; shake vertically and horizontally based on GLOBAL_TIMER
    jsr jagger_froid_set_scanline_irq        ; adjust scanline irq for any earthquake vertical scroll change
    jsr jagger_froid_animate_entrance        ; draw tiles in non-visible namespace and swap namespaces when ready
                                             ; to animate jagger froid coming out of cave
    dec ENEMY_DELAY,x                        ; decrement delay before next stage of coming out of cave
    bne jagger_froid_exit                    ; exit if delay not elapsed
    lda #$00
    sta ENEMY_VAR_4,x
    lda #$06
    sta ENEMY_DELAY,x                        ; set delay before advancing to next stage
    lda ENEMY_VAR_2,x                        ; load base nametable (0 = $2000, 1 = $2400)
    eor #$01
    sta ENEMY_VAR_2,x                        ; move to other nametable
    lda PPUCTRL_SETTINGS
    and #$fe                                 ; strip base nametable address
    ora ENEMY_VAR_2,x                        ; merge with nametable to use (0 = $2000, 1 = $2400)
    sta PPUCTRL_SETTINGS                     ; set visible nametable
    inc ENEMY_VAR_3,x                        ; advance to next position coming out of cave
    lda ENEMY_VAR_3,x
    sta ENEMY_VAR_5,x                        ; set total number of write commands (groups of blocks) to write
    cmp #$0f
    bcc jagger_froid_exit
    lda #$05                                 ; suspicious_face_bg_05 - initial jagger froid draw
    jsr suspicious_face_jagger_froid_draw_bg ; draw jagger froid
    lda #$00
    sta NT_CURRENT_COLUMN
    lda #$01
    sta DRAW_X_SCREEN                        ; set current level screen number (how many screens into the level)
    lda #$08
    sta ENEMY_VAR_2,x                        ; set delay before creating serpent
    jmp advance_enemy_routine                ; advance to jagger_froid_routine_05

; wait for drawing to complete,
jagger_froid_routine_05:
    jsr earthquake_shake              ; shake vertically and horizontally based on GLOBAL_TIMER
    jsr jagger_froid_set_scanline_irq ; adjust scanline irq for any earthquake vertical scroll change
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne jagger_froid_exit2
    dec ENEMY_VAR_2,x                 ; decrement delay before creating serpent
    beq jagger_froid_create_serpent
    lda #$00
    sta X_SCROLL_DRAW_POINT

jagger_froid_exit2:
    rts

jagger_froid_create_serpent:
    lda #$01
    sta ENEMY_SPRITE,x                 ; set invisible sprite
    ldy #$66                           ; enemy type = alien serpent
    jsr try_create_enemy_from_existing ; create alien serpent
    ldy #$00
    sty Y_SCROLL                       ; set PPU vertical scroll to top of nametables (no scroll)
    sty X_SCROLL                       ; set PPU horizontal scroll to no scroll
    dey
    sty IRQ_Y_SCROLL
    jsr jagger_froid_set_scanline_irq  ; adjust scanline irq now that earthquake stopped
    lda #$00
    sta ENEMY_VAR_1,x
    sta ENEMY_VAR_2,x
    lda #$01
    sta ENEMY_VAR_3,x
    sta ENEMY_VAR_4,x
    lda #$20
    sta ENEMY_FIRING_DELAY,x
    lda #$80                           ; delay before setting real HP
    jmp set_delay_adv_enemy_routine    ; set delay to #$0c8 and set routine to jagger_froid_routine_06

; wait for delay set real HP, advance routine
jagger_froid_routine_06:
    dec ENEMY_DELAY,x
    bne jagger_froid_exit2          ; exit if delay not elapsed
    lda #$40                        ; HP = #$40, #$50, or #$58
    jsr set_enemy_hp_hard           ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to jagger_froid_routine_07

jagger_froid_routine_07:
    dec ENEMY_DELAY,x        ; decrement mouth open/close delay
    bne @look_towards_player ; branch if mouth open/close delay hasn't elapsed
    lda ENEMY_VAR_2,x        ; load jagger froid mouth flag (0 = mouth closed, 1 = mouth open)
    eor #$01                 ; swap status of jagger froid mouth
    sta ENEMY_VAR_2,x        ; open/close mouth
    lsr
    lda #$30                 ; longer mouth open/close delay
    bcs @draw
    lda #$20                 ; shorter mouth open/close delay

@draw:
    sta ENEMY_DELAY,x                        ; set new
    lda ENEMY_VAR_2,x                        ; load jagger froid mouth flag (0 = mouth closed, 1 = mouth open)
    clc                                      ; clear carry in preparation for addition
    adc #$06
    jsr suspicious_face_jagger_froid_draw_bg

@look_towards_player:
    jsr jagger_froid_look_towards_player ; update look direction to be towards closest player
    lda ENEMY_VAR_2,x                    ; load jagger froid mouth flag (0 = mouth closed, 1 = mouth open)
    beq @exit                            ; exit if mouth closed
    dec ENEMY_FIRING_DELAY,x             ; decrement projectile creation delay timer
    bne @exit                            ; exit if timer not elapsed
    inc ENEMY_VAR_1,x                    ; increment number of projectiles created
    ldy #$70                             ; longer projectile creation delay
    lda ENEMY_VAR_1,x                    ; load total number of projects ever created
    and #$03
    beq @create_projectile               ; use longer delay when total projectiles divisible by 3
    ldy #$10                             ; shorter projectile creation delay

@create_projectile:
    tya                                ; transfer projectile creation delay to a
    sta ENEMY_FIRING_DELAY,x           ; reset delay before creating next projectile
    ldy #$68                           ; enemy type = jagger froid projectile
    jsr try_create_enemy_from_existing ; create jagger froid projectile
    bcc @exit                          ; branch if unable to create jagger froid projectile
    ldy $11                            ; load slot of created projectile
    lda ENEMY_Y_POS,x                  ; load jagger froid's Y position
    clc                                ; clear carry in preparation for addition
    adc #$18                           ; add #$18 to jagger froid Y position
    sta ENEMY_Y_POS,y                  ; set projectile's initial Y position from jagger froid position

@exit:
    rts

; enemy destroyed routine
jagger_froid_routine_08:
    jmp enemy_routine_boss_defeated_00

jagger_froid_routine_09:
    jmp enemy_routine_boss_defeated_01

jagger_froid_routine_0a:
    ldy #$7f

@copy_collision_loop:
    lda BG_COLLISION_DATA,y
    sta SECOND_BG_COLLISION_DATA,y
    dey
    bpl @copy_collision_loop
    lda PPUCTRL_SETTINGS
    ora #$01
    sta PPUCTRL_SETTINGS
    jmp bg_enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

jagger_froid_routine_0c:
    jmp set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy
                                       ; !(HUH) why not just set this address in jagger_froid_routine_ptr_tbl

jagger_froid_look_towards_player:
    dec ENEMY_VAR_3,x         ; see if should draw eyes if necessary
    bne @exit                 ; exit if graphics buffer too full
    lda #$01
    sta ENEMY_VAR_3,x
    jsr player_enemy_y_dist   ; a = closest y distance to enemy from players, y = closest player (#$00 or #$01)
    lda PLAYER_SPRITE_X_POS,y
    ldy #$00
    cmp #$60
    bcc @continue             ; branch if closest player to left of center
    iny                       ; player in center or right of screen
    cmp #$a0
    bcc @continue             ; branch if closest player to right of center
    iny                       ; player on right side of screen

@continue:
    tya                 ; transfer player position to a (0 = left, 1 = center, 2 = right)
    cmp ENEMY_VAR_4,x   ; see if different than last time
    beq @exit           ; exit if player hasn't changed positions to new area
    lda #$ff            ; moving eyes left
    bcc @update_eye_dir ; branch if player to left of last position, to move look left
    lda #$00            ; player to right of last position, to look right

@update_eye_dir:
    adc ENEMY_VAR_4,x ; look left or right by 1 position
    cmp #$03
    bcs @draw_eyes    ; branch if somehow looking too far right to use max right direction
    sta ENEMY_VAR_4,x ; look in direction of player area

@draw_eyes:
    lda ENEMY_VAR_4,x                 ; load eye look direction
    asl                               ; double since each entry is a #$02 byte memory address
    tay                               ; transfer to offset register
    lda jagger_froid_eyes_ptr_tbl,y
    sta $08
    lda jagger_froid_eyes_ptr_tbl+1,y
    sta $09
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$20
    bcs @exit_full

@eye_tile_loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny
    cmp #$ff
    bne @eye_tile_loop
    stx GRAPHICS_BUFFER_OFFSET
    ldx ENEMY_CURRENT_SLOT

@exit:
    rts

@exit_full:
    ldx ENEMY_CURRENT_SLOT
    lda #$01
    sta ENEMY_VAR_3,x
    rts

jagger_froid_eyes_ptr_tbl:
    .addr jagger_froid_eyes_00 ; look left
    .addr jagger_froid_eyes_01 ; look center
    .addr jagger_froid_eyes_02 ; look right

; look left
jagger_froid_eyes_00:
    .byte $06     ; block mode
    .byte $20,$ad ; PPU address $20ad
    .byte $02     ; block size = #$02 tiles
    .byte $a9,$56
    .byte $20,$cd ; PPU address $20cd
    .byte $02     ; block size = #$02 tiles
    .byte $ac,$ad
    .byte $20,$b1 ; PPU address $20b1
    .byte $02     ; block size = #$02 tiles
    .byte $55,$b6
    .byte $20,$d1 ; PPU address $20d1
    .byte $02     ; block size = #$02 tiles
    .byte $b7,$b8
    .byte $ff     ; end of data marker

; look center
jagger_froid_eyes_01:
    .byte $06     ; block mode
    .byte $20,$ad ; PPU address $20ad
    .byte $02     ; block size = #$02 tiles
    .byte $55,$56
    .byte $20,$cd ; PPU address $20cd
    .byte $02     ; block size = #$02 tiles
    .byte $5b,$5c
    .byte $20,$b1 ; PPU address $20b1
    .byte $02     ; block size = #$02 tiles
    .byte $55,$56
    .byte $20,$d1 ; PPU address $20d1
    .byte $02     ; block size = #$02 tiles
    .byte $5f,$60
    .byte $ff     ; end of data marker

; look right
jagger_froid_eyes_02:
    .byte $06     ; block mode
    .byte $20,$ad ; PPU address $20ad
    .byte $02     ; #$02 tiles
    .byte $a9,$56
    .byte $20,$cd ; PPU address $20cd
    .byte $02
    .byte $ae,$b0
    .byte $20,$b1 ; PPU address $20b1
    .byte $02
    .byte $55,$b6
    .byte $20,$d1 ; PPU address $20d1
    .byte $02
    .byte $b9,$bf
    .byte $ff     ; end of data marker

jagger_froid_exit3:
    rts

jagger_froid_animate_entrance:
    lda ENEMY_VAR_4,x
    bmi jagger_froid_exit3
    lda ENEMY_VAR_2,x      ; load base nametable (0 = $2000, 1 = $2400)
    lsr                    ; push bit to carry
    lda #$08
    bcs @calc_addr         ; branch if top right nametable ($2400)
    lda #$0b               ; top left nametable ($2000)

@calc_addr:
    sta $01           ; set initial PPU address high byte
    ldy ENEMY_VAR_3,x ; load how far out the cave jagger froid is
    dey               ; subtract 1
    tya               ; transfer to a
    sec               ; set carry flag in preparation for subtraction
    sbc ENEMY_VAR_4,x
    asl
    asl
    asl
    asl
    rol $01
    asl
    rol $01
    sta $00           ; set starting PPU address low byte
                      ; $01 now contains PPU address high byte
    ldy #$00
    lda ENEMY_VAR_4,x ; load index of of section to draw
    sta $0a           ; set index of section to draw
    beq @draw         ; branch if starting at top
                      ; otherwise, calculate actual index into jagger_froid_tile_tbl
                      ; since each block block of tiles isn't the same size

@calc_index_loop:
    iny
    lda jagger_froid_tile_tbl,y
    sta $0b
    iny
    tya
    clc                         ; clear carry in preparation for addition
    adc $0b
    tay
    dec $0a
    bne @calc_index_loop

@draw:
    lda jagger_froid_tile_tbl,y
    cmp #$80
    beq @exit_drawing
    jmp @draw_jagger_froid

@exit_drawing:
    sta ENEMY_VAR_4,x ; set new starting draw offset for the next frame
    rts

@draw_jagger_froid:
    lda #$04                   ; writing 4 graphic rows
    sta $08                    ; set writing 4 graphics rows
    lda ENEMY_VAR_5,x
    sta $09                    ; set total number of write commands (groups of blocks) to write
    lda ENEMY_VAR_4,x
    sta $0a                    ; set current write command offset
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                   ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x
    inx                        ; increment graphics buffer write offset

@graphics_row_loop:
    lda jagger_froid_tile_tbl,y ; load graphics byte
    cmp #$80
    beq @exit
    clc                         ; clear carry in preparation for addition
    adc $00                     ; add base PPU address low byte
    sta CPU_GRAPHICS_BUFFER+1,x ; store PPU address low byte
    lda $01                     ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x   ; store PPU address high byte
    inx                         ; increment graphics buffer write offset
    inx                         ; increment graphics buffer write offset
    iny                         ; increment graphics buffer read offset
    lda jagger_froid_tile_tbl,y ; load number of tiles to write at this address (block size)
    sta CPU_GRAPHICS_BUFFER,x   ; set block size
    sta $0b                     ; store number of tiles to draw
    inx                         ; increment graphics buffer write offset
    iny                         ; increment graphics buffer read offset

@tile_loop:
    lda jagger_froid_tile_tbl,y ; load graphics byte
    sta CPU_GRAPHICS_BUFFER,x   ; write graphics byte
    inx                         ; increment graphics buffer write offset
    iny                         ; increment graphics buffer read offset
    dec $0b                     ; decrement total number of tiles to write in this block
    bne @tile_loop              ; branch if more tiles to write
    inc $0a                     ; increment total number of blocks written
    dec $09                     ; decrement remaining number of blocks
    beq @set_done
    lda $00                     ; load PPU address low byte
    sec                         ; set carry flag in preparation for subtraction
    sbc #$20                    ; move up one nametable row
    sta $00                     ; set new PPU address high byte
    bcs @dec_block
    dec $01                     ; decrement PPU address low byte if overflow

@dec_block:
    dec $08                ; decrement total number of blocks (rows) to write
    bne @graphics_row_loop
    beq @write_end         ; branch if written all 4 rows of tiles to graphics buffer

@set_done:
    lda #$ff
    sta $0a

@write_end:
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x
    inx
    stx GRAPHICS_BUFFER_OFFSET
    lda $0a

@exit:
    ldx ENEMY_CURRENT_SLOT
    sta ENEMY_VAR_4,x      ; set logical index of next set of tiles to draw
                           ; note, this isn't the actual jagger_froid_tile_tbl index
                           ; but the logical index of tile block
    lda $09
    sta ENEMY_VAR_5,x      ; set total number of write commands (groups of blocks) to write
    rts

jagger_froid_tile_tbl:
    .byte $0e                                                     ; PPU address low byte offset
    .byte $04                                                     ; block size
    .byte $00,$8e,$8f,$00                                         ; tiles to write
    .byte $0d                                                     ; PPU address low byte offset
    .byte $06                                                     ; block size
    .byte $00,$8a,$8b,$8c,$8d,$00                                 ; tiles to write
    .byte $0d                                                     ; PPU address low byte offset
    .byte $06                                                     ; block size
    .byte $7c,$7d,$7e,$7f,$88,$89                                 ; tiles to write
    .byte $0d                                                     ; PPU address low byte offset
    .byte $06                                                     ; block size
    .byte $79,$7a,$7a,$7a,$7a,$7b                                 ; tiles to write
    .byte $0c                                                     ; PPU address low byte offset
    .byte $08                                                     ; block size
    .byte $00,$73,$74,$75,$76,$77,$78,$00                         ; tiles to write
    .byte $0c                                                     ; PPU address low byte offset
    .byte $08                                                     ; block size
    .byte $6b,$6c,$6d,$6e,$6f,$70,$71,$72                         ; tiles to write
    .byte $0b                                                     ; PPU address low byte offset
    .byte $0a                                                     ; block size
    .byte $62,$63,$64,$65,$66,$67,$65,$68,$69,$6a                 ; tiles to write
    .byte $0b                                                     ; PPU address low byte offset
    .byte $0a                                                     ; block size
    .byte $5a,$54,$5b,$5c,$5d,$5e,$5f,$60,$59,$61                 ; tiles to write
    .byte $0a                                                     ; PPU address low byte offset
    .byte $0c                                                     ; block size
    .byte $00,$00,$54,$55,$56,$57,$58,$55,$56,$59,$00,$00         ; tiles to write
    .byte $09                                                     ; PPU address low byte offset
    .byte $0e                                                     ; block size
    .byte $00,$3c,$a8,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$3b,$49,$00 ; tiles to write
    .byte $09                                                     ; PPU address low byte offset
    .byte $0e                                                     ; block size
    .byte $00,$a8,$42,$3d,$0b,$4a,$0e,$1d,$4b,$20,$48,$47,$3b,$00 ; tiles to write
    .byte $09                                                     ; PPU address low byte offset
    .byte $0e                                                     ; block size
    .byte $3c,$0a,$3d,$42,$43,$44,$0e,$1d,$45,$46,$47,$48,$22,$49 ; tiles to write
    .byte $09                                                     ; PPU address low byte offset
    .byte $0e                                                     ; block size
    .byte $a8,$0a,$09,$39,$0c,$0d,$0e,$1d,$1e,$1f,$3a,$21,$22,$3b ; tiles to write
    .byte $09                                                     ; PPU address low byte offset
    .byte $0e                                                     ; block size
    .byte $09,$0a,$09,$0b,$0c,$0d,$0e,$1d,$1e,$1f,$20,$21,$22,$21 ; tiles to write
    .byte $80                                                     ; end of graphics marker

; enemy type #$68
jagger_froid_projectile_routine_ptr_tbl:
    .addr jagger_froid_projectile_routine_00 ; set sprite, palette, and target closest player at 1.25x velocity
    .addr jagger_froid_projectile_routine_01 ; re-target to closest player after delay elapses
    .addr enemy_explosion_routine_00         ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01         ; animate explosion sequence
    .addr enemy_explosion_routine_03         ; mark destroyed, remove enemy

; set sprite, palette, and target closest player at 1.25x velocity
jagger_froid_projectile_routine_00:
    lda #$8f
    sta ENEMY_SPRITE,x              ; sprite_8f
    lda #$02
    sta ENEMY_SPRITE_ATTR,x         ; set sprite palette
    jsr player_enemy_x_dist         ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$03                        ; 1.25x speed
    jsr set_vel_to_target_player    ; set enemy velocity to target player index $0a at 1.25x speed
    lda #$08
    jmp set_delay_adv_enemy_routine ; set delay to #$08 and set routine to jagger_froid_projectile_routine_01

; re-target to closest player after delay elapses
jagger_froid_projectile_routine_01:
    dec ENEMY_DELAY,x
    bne @exit                        ; exit to apply velocity if delay hasn't elapsed
    lda #$0c
    sta ENEMY_DELAY,x                ; delay elapsed, set delay until next re-targeting
    jsr player_enemy_x_dist          ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy #$03                         ; speed adjust code for 1.25x speed
    jsr hone_to_player_set_enemy_vel ; determine next aim dir to get closer to player index ($0a)
                                     ; and set enemy velocity to that target direction

@exit:
    jmp apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

; enemy type #$64
suspicious_face_routine_ptr_tbl:
    .addr suspicious_face_routine_00    ; set special invincible HP, disable collisions, set spike explosion when die, set invisible sprite, advance routine
    .addr suspicious_face_routine_01    ; wait for LEVEL_Y_SCROLL_FLAGS to become negative, set position, play BOSS2BGM, enable collisions, set delay and advance routine
    .addr suspicious_face_routine_02    ; wait for delay, set HP, advance routine
    .addr suspicious_face_routine_03    ; animate open and closing mouth firing when firing delay timer elapsed
    .addr suspicious_face_routine_04    ; enemy destroyed routine, enemy_routine_boss_defeated_00
    .addr suspicious_face_routine_05    ; set enemy frame to destroyed, enemy_routine_boss_defeated_01
    .addr suspicious_face_routine_06    ; draw empty cave, set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr bg_enemy_explosion_routine_01 ; animate sequence of explosions
    .addr suspicious_face_routine_08    ; create jagger froid

; set special invincible HP, disable collisions, set spike explosion when die, set invisible sprite, advance routine
suspicious_face_routine_00:
    lda #$f0                  ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$91
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision, disable bullet collision, spike explosion, sound_26 (B OUT)
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite (uses bg tiles instead)
    jmp advance_enemy_routine ; advance to next routine

; wait for LEVEL_Y_SCROLL_FLAGS to become negative, set position, play BOSS2BGM, enable collisions, set delay and advance routine
suspicious_face_routine_01:
    lda LEVEL_Y_SCROLL_FLAGS
    bpl suspicious_face_exit        ; exit if still can scroll up
    lda #$30
    sta ENEMY_Y_POS,x
    lda #$80
    sta ENEMY_X_POS,x               ; set position to (#80,#$30)
    lda #$32                        ; sound_32 (BOSS2BGM)
    jsr play_sound                  ; play sound_32 - Creature from Outer Space (Boss 3)
    lda #$10
    sta ENEMY_DESTROY_ATTRS,x       ; enable collision, set destroy sound to sound_26 (B OUT) - boss destroy
    lda #$01
    sta ENEMY_FRAME,x
    lda #$40
    jmp set_delay_adv_enemy_routine ; set delay to #$40 and set routine to suspicious_face_routine_02

suspicious_face_exit:
    rts

; wait for delay, set HP, advance routine
suspicious_face_routine_02:
    dec ENEMY_DELAY,x
    bne suspicious_face_exit        ; exit if delay hasn't elapsed
    lda #$20                        ; HP = #$20, #$30, or #$38
    jsr set_enemy_hp_hard           ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to suspicious_face_routine_03

; animate open and closing mouth firing when firing delay timer elapsed
suspicious_face_routine_03:
    dec ENEMY_DELAY,x               ; decrement animation timer
    bne @fire_when_ready            ; branch if animation timer not elapsed to check firing delay timer
    jsr suspicious_face_draw_bg     ; animation timer elapsed
                                    ; move to next animation (open/close/half-open mouth)
    lda #$01
    sta ENEMY_FIRING_DELAY,x
    ldy ENEMY_FRAME,x
    lda suspicious_face_delay_tbl,y
    sta ENEMY_DELAY,x               ; set animation delay based on current frame
    lda ENEMY_VAR_2,x
    lsr
    bcs @prev_frame                 ; branch if odd (mouth open) to decrement current animation frame
    inc ENEMY_FRAME,x               ; ENEMY_VAR_2 even (mouth closed), move up in animation
    bne @set_frame                  ; always branch

; mouth open or partially open, continue closing
@prev_frame:
    dec ENEMY_FRAME,x

@set_frame:
    lda ENEMY_FRAME,x
    cmp #$03
    bcc @exit
    lda #$01          ; looped through animation, set mouth closing frame
    sta ENEMY_FRAME,x
    inc ENEMY_VAR_2,x ; mouth either fully closed, or fully open
                      ; increment frame position index
                      ; used to know whether to decrement or increment ENEMY_FRAME for animation

@exit:
    rts

@fire_when_ready:
    lda ENEMY_VAR_2,x         ; load animation frame counter
    lsr
    bcc suspicious_face_exit  ; exit if mouth is closed
    dec ENEMY_FIRING_DELAY,x  ; mouth open, decrement bullet firing delay timer
    bne suspicious_face_exit2 ; exit if bullet firing delay timer hasn't elapsed
    jsr player_enemy_x_dist   ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$05                  ; bullet speed code #$05 (1.62x speed)
    jsr fire_near_player      ; fire near or at player $0a using speed code y
    lda #$10
    sta ENEMY_FIRING_DELAY,x  ; set bullet fire delay timer

suspicious_face_exit2:
    rts

suspicious_face_delay_tbl:
    .byte $20,$0c,$70

; enemy destroyed routine
suspicious_face_routine_04:
    jmp enemy_routine_boss_defeated_00

; set enemy frame to destroyed, enemy_routine_boss_defeated_01
suspicious_face_routine_05:
    lda #$03
    sta ENEMY_FRAME,x                  ; set frame to destroyed frame top (cave) (see suspicious_face_bg_ptr_tbl)
    jmp enemy_routine_boss_defeated_01

; draw empty cave, set empty sprite, play optional enemy destroyed sound, disable collisions
suspicious_face_routine_06:
    lda ENEMY_FRAME,x
    jsr suspicious_face_jagger_froid_draw_bg ; animate destruction
    inc ENEMY_FRAME,x                        ; move to updating the top of the cave
    lda ENEMY_FRAME,x
    cmp #$05
    bcc suspicious_face_exit2
    jmp bg_enemy_explosion_routine_00        ; set empty sprite, play optional enemy destroyed sound, disable collisions

; create jagger froid
suspicious_face_routine_08:
    jsr enemy_explosion_routine_03     ; mark destroyed, remove enemy
    lda #$00
    sta BOSS_DEFEATED_FLAGS            ; clear level boss has been removed flag
    ldy #$65                           ; enemy type = jagger froid
    jmp try_create_enemy_from_existing ; create jagger froid

suspicious_face_draw_bg:
    lda ENEMY_FRAME,x

; draws either the bg tiles for suspicious face or jagger froid
; input
;  * a - specific background to draw (offsets suspicious_face_bg_ptr_tbl)
suspicious_face_jagger_froid_draw_bg:
    ldx GRAPHICS_BUFFER_OFFSET
    asl
    tay
    lda suspicious_face_bg_ptr_tbl,y
    sta $08
    lda suspicious_face_bg_ptr_tbl+1,y
    sta $09
    ldy #$00

@loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny
    cmp #$fe
    bne @loop                   ; write until #$fe
    lda #$ff
    sta CPU_GRAPHICS_BUFFER-1,x ; write graphics end byte to buffer
    stx GRAPHICS_BUFFER_OFFSET  ; set graphics buffer offset
    ldx ENEMY_CURRENT_SLOT      ; restore enemy slot
    rts

; used for both suspicious face as well as jagger froid
suspicious_face_bg_ptr_tbl:
    .addr suspicious_face_bg_00 ; suspicious face mouth closed
    .addr suspicious_face_bg_01 ; suspicious face mouth partially open
    .addr suspicious_face_bg_02 ; suspicious face mouth open
    .addr suspicious_face_bg_03 ; destroyed frame top (cave)
    .addr suspicious_face_bg_04 ; destroyed frame bottom (cave)
    .addr suspicious_face_bg_05 ; jagger froid
    .addr suspicious_face_bg_06 ; jagger froid mouth in
    .addr suspicious_face_bg_07 ; jagger froid mouth out

; suspicious face mouth closed
suspicious_face_bg_00:
    .byte $06                                                             ; block mode
    .byte $20,$6a                                                         ; PPU write address
    .byte $0c                                                             ; block size
    .byte $92,$93,$94,$95,$92,$93,$94,$95,$92,$93,$94,$95
    .byte $20,$88                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $17,$97,$18,$98,$18,$98,$18,$98,$18,$98,$18,$98,$18,$98,$19,$99
    .byte $20,$a8                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $9a,$9b,$9c,$9d,$9e,$9d,$9e,$9d,$9e,$9d,$9e,$9d,$9e,$9f,$a0,$a1
    .byte $20,$ca                                                         ; PPU write address
    .byte $0c                                                             ; block size
    .byte $1a,$a3,$1b,$a3,$1b,$a3,$1b,$a3,$1b,$a3,$1b,$a7
    .byte $20,$ea                                                         ; PPU write address
    .byte $0c                                                             ; block size
    .byte $00,$a8,$a9,$a8,$a9,$a8,$a9,$a8,$a9,$a8,$a9,$00
    .byte $23,$ca                                                         ; PPU write address
    .byte $04                                                             ; block size
    .byte $00,$00,$00,$00
    .byte $fe                                                             ; end of graphics

; suspicious face mouth partially open
suspicious_face_bg_01:
    .byte $06                                                             ; block mode
    .byte $20,$6a                                                         ; PPU write address
    .byte $0c                                                             ; block size
    .byte $aa,$ab,$ac,$ad,$aa,$ab,$ac,$ad,$aa,$ab,$ac,$ad
    .byte $20,$88                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $ae,$b0,$b4,$b5,$b6,$b5,$b6,$b5,$b6,$b5,$b6,$b5,$b6,$b7,$b8,$b9
    .byte $20,$a8                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $bf,$c0,$c1,$c2,$c3,$c2,$c3,$c2,$c3,$c2,$c3,$c2,$c3,$c4,$c5,$c6
    .byte $20,$ca                                                         ; PPU write address
    .byte $0c                                                             ; block size
    .byte $c7,$c8,$c9,$c8,$c9,$c8,$c9,$c8,$c9,$c8,$c9,$ca
    .byte $20,$ea                                                         ; PPU write address
    .byte $0c                                                             ; block size
    .byte $00,$cb,$cc,$cb,$cc,$cb,$cc,$cb,$cc,$cb,$cc,$00
    .byte $23,$ca                                                         ; PPU write address
    .byte $04                                                             ; block size
    .byte $88,$aa,$aa,$22
    .byte $fe                                                             ; end of graphics

; suspicious face mouth open
suspicious_face_bg_02:
    .byte $06                                             ; block mode
    .byte $20,$8a                                         ; PPU write address
    .byte $0c                                             ; block size
    .byte $cd,$ce,$cf,$ce,$cf,$ce,$cf,$ce,$cf,$ce,$cf,$d0
    .byte $20,$aa                                         ; PPU write address
    .byte $0c                                             ; block size
    .byte $d1,$d2,$d3,$d2,$d3,$d2,$d3,$d2,$d3,$d2,$d3,$d4
    .byte $20,$ca                                         ; PPU write address
    .byte $0c                                             ; block size
    .byte $d5,$d6,$d7,$d6,$d7,$d6,$d7,$d6,$d7,$d6,$d7,$d8
    .byte $20,$ea                                         ; PPU write address
    .byte $0c                                             ; block size
    .byte $d9,$da,$db,$da,$db,$da,$db,$da,$db,$da,$db,$dc
    .byte $fe                                             ; end of graphics

; destroyed frame top (cave)
suspicious_face_bg_03:
    .byte $03                                                             ; repeat mode
    .byte $20,$28                                                         ; PPU write address
    .byte $10                                                             ; repeat count
    .byte $00                                                             ; byte to repeat
    .byte $03                                                             ; repeat mode
    .byte $20,$48                                                         ; PPU write address
    .byte $10                                                             ; repeat count
    .byte $00                                                             ; byte to repeat
    .byte $06                                                             ; block mode
    .byte $20,$09                                                         ; PPU write address
    .byte $0e                                                             ; block size
    .byte $e0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$e1
    .byte $20,$68                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $00,$00,$e2,$e4,$00,$00,$00,$00,$00,$00,$00,$00,$e5,$e3,$00,$00
    .byte $fe                                                             ; end of graphics

; destroyed frame bottom (cave)
suspicious_face_bg_04:
    .byte $06                                                             ; block mode
    .byte $20,$88                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $00,$e2,$e4,$e6,$e8,$ea,$ec,$ec,$ec,$ec,$eb,$e9,$e7,$e5,$e3,$00
    .byte $20,$a8                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $ee,$f0,$f2,$e8,$ea,$ec,$ed,$ed,$ed,$ed,$ec,$eb,$e9,$f3,$f1,$ef
    .byte $20,$c8                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $00,$f8,$f6,$ee,$f0,$f4,$de,$de,$de,$de,$f5,$f1,$ef,$f7,$1c,$00
    .byte $20,$e8                                                         ; PPU write address
    .byte $10                                                             ; block size
    .byte $00,$00,$00,$00,$f8,$f6,$df,$df,$df,$df,$f7,$1c,$00,$00,$00,$00
    .byte $23,$c2                                                         ; PPU write address
    .byte $04                                                             ; block size
    .byte $00,$00,$00,$00
    .byte $23,$ca                                                         ; PPU write address
    .byte $04                                                             ; block size
    .byte $00,$00,$00,$00
    .byte $fe

; jagger froid
suspicious_face_bg_05:
    .byte $06             ; block mode
    .byte $23,$c2         ; PPU write address
    .byte $04             ; block size
    .byte $ff,$33,$cc,$ff
    .byte $23,$ca         ; PPU write address
    .byte $04             ; block size
    .byte $0c,$f0,$f0,$03
    .byte $23,$db         ; PPU write address
    .byte $02             ; block size
    .byte $51,$54
    .byte $fe             ; end of graphics

; jagger froid mouth in
suspicious_face_bg_06:
    .byte $06                             ; block mode
    .byte $21,$2c                         ; PPU write address
    .byte $08                             ; block size
    .byte $00,$73,$74,$75,$76,$77,$78,$00
    .byte $21,$4d                         ; PPU write address
    .byte $06                             ; block size
    .byte $79,$7a,$7a,$7a,$7a,$7b
    .byte $21,$6d                         ; PPU write address
    .byte $06                             ; block size
    .byte $7c,$7d,$7e,$7f,$88,$89
    .byte $21,$8d                         ; PPU write address
    .byte $06                             ; block size
    .byte $00,$8a,$8b,$8c,$8d,$00
    .byte $21,$ae                         ; PPU write address
    .byte $04                             ; block size
    .byte $00,$8e,$8f,$00
    .byte $23,$db                         ; PPU write address
    .byte $02                             ; block size
    .byte $51,$54
    .byte $fe                             ; end of graphics

; jagger froid mouth out
suspicious_face_bg_07:
    .byte $06                             ; block mode
    .byte $21,$2c                         ; PPU write address
    .byte $08                             ; block size
    .byte $90,$7a,$74,$75,$76,$77,$7a,$91
    .byte $21,$4d                         ; PPU write address
    .byte $06                             ; block size
    .byte $92,$7a,$7a,$7a,$7a,$93
    .byte $21,$6d                         ; PPU write address
    .byte $06                             ; block size
    .byte $94,$95,$96,$97,$98,$99
    .byte $21,$8d                         ; PPU write address
    .byte $06                             ; block size
    .byte $9a,$9b,$9c,$9d,$9e,$9f
    .byte $21,$ae                         ; PPU write address
    .byte $04                             ; block size
    .byte $a0,$a1,$a3,$a7
    .byte $23,$db                         ; PPU write address
    .byte $02                             ; block size
    .byte $59,$56
    .byte $fe                             ; end of graphics

; enemy type #$66
alien_serpent_routine_ptr_tbl:
    .addr alien_serpent_routine_00
    .addr alien_serpent_routine_01
    .addr alien_serpent_routine_02   ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

alien_serpent_routine_00:
    lda #$18          ; HP = #$18, #$1c, or #$1f
    jsr set_enemy_hp  ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    ldy ENEMY_VAR_3,x ; load serpent part number
    beq @continue     ; branch if the head
    lda #$f1          ; not the head, set special HP not destroyed by destroy_all_enemies routine

@continue:
    sta ENEMY_HP,x             ; set serpent part HP
    lda ENEMY_VAR_3,x          ; load serpent part number
    bne @set_delay_adv_routine ; branch to advance routine if not the head
    lda #$f0                   ; serpent head
    sta ENEMY_X_POS,x          ; set initial head X position to
    ldy #$00

; setting X and Y position repeatedly
; not used for BG collision
@loop:
    lda ENEMY_Y_POS,x                 ; load enemy's Y position
    sta BG_COLLISION_DATA+64,y
    lda ENEMY_X_POS,x                 ; load enemy's X position
    sta SECOND_BG_COLLISION_DATA+32,y
    iny                               ; increment write offset
    cpy #$5c
    bcc @loop
    stx $12                           ; store enemy slot into $12
    lda #$01
    sta $08

@loop2:
    stx ENEMY_CURRENT_SLOT
    ldy #$66                           ; enemy type = alien serpent
    jsr try_create_enemy_from_existing ; create alien serpent part
    bcc @continue2                     ; branch if unable to create alien serpent
    ldy $11                            ; load slot of created alien serpent part
    lda $08                            ; increment serpent part counter (head is 0, body starts at 1)
    sta ENEMY_VAR_3,y                  ; set body part number
    inc $08                            ; increment serpent part counter
    cmp #$06                           ; see if set all parts
    tya                                ; transfer slot of newly created next alien serpent part to a
    sta ENEMY_VAR_5,x                  ; set enemy slot of next serpent part within the snake
                                       ; creating linked list
    tax                                ; transfer slot of newly created next alien serpent part to x
    bcc @loop2                         ; continue if haven't created all serpent parts

@continue2:
    lda #$ff
    sta ENEMY_VAR_5,x ; set tail's next part to #$ff meaning no next part
    ldx $12           ; restore x to the enemy offset

@set_delay_adv_routine:
    lda #$08
    jmp set_delay_adv_enemy_routine ; set delay to #$08 and set routine to alien_serpent_routine_01

alien_serpent_routine_01:
    jsr alien_serpent_set_sprite
    ldy ENEMY_X_POS,x            ; load enemy's X position
    cpy #$14
    bcc @set_invisible_sprite
    cpy #$24
    bcc @set_sprite_attr
    cpy #$ea
    bcs @set_invisible_sprite
    cpy #$dc
    bcc @exit_if_not_head

@set_sprite_attr:
    lda ENEMY_SPRITE_ATTR,x
    ora #$20
    sta ENEMY_SPRITE_ATTR,x
    jmp @exit_if_not_head

@set_invisible_sprite:
    lda #$00
    sta ENEMY_SPRITE,x

@exit_if_not_head:
    lda ENEMY_VAR_3,x ; load serpent part number
    beq @serpent_head
    rts

@serpent_head:
    jsr run_alien_serpent_head_routine
    lda SECOND_BG_COLLISION_DATA+127
    sec                                ; set carry flag in preparation for subtraction
    sbc #$01
    cmp #$5c
    bcc @continue
    adc #$5b

; setting position of serpent part based on offset from head
@continue:
    sta SECOND_BG_COLLISION_DATA+127
    tay
    lda ENEMY_Y_POS,x                 ; load enemy's Y position
    sta BG_COLLISION_DATA+64,y
    lda ENEMY_X_POS,x                 ; load enemy's X position
    sta SECOND_BG_COLLISION_DATA+32,y

@loop:
    lda ENEMY_VAR_5,x
    bmi @restore_x_exit                  ; exit if on serpent's last part (tail)
    tax                                  ; not on last part
    ldy ENEMY_VAR_3,x                    ; load serpent part (0 = head)
    lda alien_serpent_pos_offset_tbl-1,y
    clc                                  ; clear carry in preparation for addition
    adc SECOND_BG_COLLISION_DATA+127
    bcs @handle_max
    cmp #$5c
    bcc @set_pos

@handle_max:
    sbc #$5c

@set_pos:
    tay
    lda BG_COLLISION_DATA+64,y
    sta ENEMY_Y_POS,x
    lda SECOND_BG_COLLISION_DATA+32,y
    sta ENEMY_X_POS,x
    jmp @loop

@restore_x_exit:
    ldx ENEMY_CURRENT_SLOT
    rts

alien_serpent_pos_offset_tbl:
    .byte $0f,$1e,$2d,$3c,$4b,$5a

; enemy destroyed routine
alien_serpent_routine_02:
    lda ENEMY_VAR_5,x
    cmp #$ff
    beq @prep_explosion
    tax
    jsr set_destroyed_enemy_routine
    ldx ENEMY_CURRENT_SLOT

@prep_explosion:
    jsr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    rts

alien_serpent_set_sprite:
    ldy #$08
    lda ENEMY_VAR_3,x
    bne @continue
    ldy ENEMY_VAR_1,x
    lda dir_to_overhead_dir_tbl,y ; convert to overhead aim direction
    tay

@continue:
    lda alien_serpent_sprite_tbl,y
    sta ENEMY_SPRITE,x
    lda alien_serpent_sprite_attr_tbl,y
    sta ENEMY_SPRITE_ATTR,x
    rts

; sprite_8b, sprite_8a, sprite_89, sprite_8c, sprite_8e (body)
alien_serpent_sprite_tbl:
    .byte $8b,$8a,$89,$8a,$8b,$8c,$8d,$8c,$8e

alien_serpent_sprite_attr_tbl:
    .byte $43,$43,$03,$03,$03,$03,$03,$43,$03

run_alien_serpent_head_routine:
    lda ENEMY_VAR_2,x
    jsr run_routine_from_tbl_below

alien_serpent_head_routine_tbl:
    .addr alien_serpent_head_routine_00
    .addr alien_serpent_head_routine_01
    .addr alien_serpent_head_routine_02

alien_serpent_head_routine_00:
    ldy ENEMY_VAR_4,x                   ; load serpent route index
    lda alien_serpent_route_tbl,y
    tay
    lda alien_serpent_head_y_pos_tbl,y
    sta ENEMY_Y_POS,x
    lda alien_serpent_head_x_pos_tbl,y
    sta ENEMY_X_POS,x                   ; set initial position
    lda #$ff
    sta ENEMY_FRAME,x
    jsr alien_serpent_set_vel_and_delay ; set head's velocity and direction
                                        ; along with the delay before changing direction based on ENEMY_VAR_4,x
    jmp alien_serpent_adv_head_routine

alien_serpent_head_routine_01:
    jsr alien_serpent_apply_vel
    dec ENEMY_DELAY,x                   ; decrement direction change delay
    bne alien_serpent_head_exit         ; exit if delay hasn't elapsed
    jsr alien_serpent_set_vel_and_delay ; set head's velocity and direction
                                        ; along with the delay before changing direction based on ENEMY_VAR_4,x
    bpl alien_serpent_head_exit
    lda ENEMY_VAR_4,x                   ; load index of path the serpent will follow
    clc                                 ; clear carry in preparation for addition
    adc #$01                            ; !(OBS) optimization could be to use inc ENEMY_VAR_4,x
    cmp #$04
    bcc @continue
    lda #$00                            ; loop back to first path

@continue:
    sta ENEMY_VAR_4,x ; set index of path the serpent will follow
    lda #$80
    sta ENEMY_DELAY,x ; set in cave delay

alien_serpent_adv_head_routine:
    inc ENEMY_VAR_2,x ; advance alien serpent head routine

alien_serpent_head_exit:
    rts

alien_serpent_head_routine_02:
    dec ENEMY_DELAY,x
    bne alien_serpent_head_exit
    lda #$00
    sta ENEMY_VAR_2,x
    rts

; sets the alien serpent head's velocity and direction along with the delay
; before changing direction based on ENEMY_VAR_4,x and ENEMY_FRAME,x
; input
;  * x - enemy slot index
alien_serpent_set_vel_and_delay:
    inc ENEMY_FRAME,x
    ldy ENEMY_VAR_4,x                 ; load serpent route index
    lda alien_serpent_route_tbl,y
    asl                               ; double since each entry is a #$02 byte address
    tay                               ; transfer to offset register
    lda alien_serpent_vel_ptr_tbl,y
    sta $00
    lda alien_serpent_vel_ptr_tbl+1,y
    sta $01
    lda ENEMY_FRAME,x
    asl
    tay
    lda ($00),y
    bmi @exit
    sta ENEMY_VAR_1,x                 ; set current aim direction
    iny
    lda ($00),y
    sta ENEMY_DELAY,x                 ; set delay before moving to next direction
    ldy #$03
    sty $06                           ; set speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr set_enemy_vel_from_aim_dir    ; use aim direction (ENEMY_VAR_1,x) and speed code ($06) to set velocity
    lda #$00

@exit:
    rts

alien_serpent_route_tbl:
    .byte $02,$03,$02,$03

alien_serpent_head_y_pos_tbl:
    .byte $48,$48,$48,$48

alien_serpent_head_x_pos_tbl:
    .byte $f0,$10,$f0,$10

alien_serpent_vel_ptr_tbl:
    .addr alien_serpent_vel_00 ; !(UNUSED) - no loop from right to left
    .addr alien_serpent_vel_01 ; !(UNUSED) - no loop from left to right
    .addr alien_serpent_vel_02 ; loop from right to left
    .addr alien_serpent_vel_03 ; loop from left to right

; byte 0 - ENEMY_VAR_1,x (aim direction)
; byte 1 - ENEMY_DELAY,x (delay before changing direction)
alien_serpent_vel_00:
    .byte $09,$16 ; frame #$00
    .byte $07,$40 ; frame #$01
    .byte $08,$10 ; frame #$02
    .byte $09,$10 ; frame #$03
    .byte $0a,$10 ; frame #$04
    .byte $0b,$10 ; frame #$05
    .byte $0c,$10 ; frame #$06
    .byte $0d,$10 ; frame #$07
    .byte $0e,$10 ; frame #$08
    .byte $0f,$10 ; frame #$09
    .byte $10,$10 ; frame #$0a
    .byte $11,$40 ; frame #$0b
    .byte $0f,$16 ; frame #$0c
    .byte $ff

; byte 0 - ENEMY_VAR_1,x (aim direction)
; byte 1 - ENEMY_DELAY,x (delay before changing direction)
alien_serpent_vel_01:
    .byte $03,$16 ; frame #$00
    .byte $05,$40 ; frame #$01
    .byte $04,$10 ; frame #$02
    .byte $03,$10 ; frame #$03
    .byte $02,$10 ; frame #$04
    .byte $01,$10 ; frame #$05
    .byte $00,$10 ; frame #$06
    .byte $17,$10 ; frame #$07
    .byte $16,$10 ; frame #$08
    .byte $15,$10 ; frame #$09
    .byte $14,$10 ; frame #$0a
    .byte $13,$40 ; frame #$0b
    .byte $15,$16 ; frame #$0c
    .byte $ff

; byte 0 - ENEMY_VAR_1,x (aim direction)
; byte 1 - ENEMY_DELAY,x (delay before changing direction)
alien_serpent_vel_02:
    .byte $09,$16 ; frame #$00
    .byte $07,$40 ; frame #$01
    .byte $08,$10 ; frame #$02
    .byte $09,$10 ; frame #$03
    .byte $0a,$10 ; frame #$04
    .byte $0b,$10 ; frame #$05
    .byte $0c,$10 ; frame #$06
    .byte $0d,$0a ; frame #$07
    .byte $0e,$0a ; frame #$08
    .byte $0f,$0a ; frame #$09
    .byte $10,$0a ; frame #$0a
    .byte $11,$0a ; frame #$0b
    .byte $12,$0a ; frame #$0c
    .byte $13,$0a ; frame #$0d
    .byte $14,$0a ; frame #$0e
    .byte $15,$0a ; frame #$0f
    .byte $16,$0a ; frame #$10
    .byte $17,$0a ; frame #$11
    .byte $00,$10 ; frame #$12
    .byte $01,$0a ; frame #$13
    .byte $02,$0a ; frame #$14
    .byte $03,$0a ; frame #$15
    .byte $04,$0a ; frame #$16
    .byte $05,$0a ; frame #$17
    .byte $06,$0a ; frame #$18
    .byte $07,$0a ; frame #$19
    .byte $08,$0a ; frame #$1a
    .byte $09,$0a ; frame #$1b
    .byte $0a,$0a ; frame #$1c
    .byte $0b,$0a ; frame #$1d
    .byte $0c,$10 ; frame #$1e
    .byte $0d,$10 ; frame #$1f
    .byte $0e,$10 ; frame #$20
    .byte $0f,$10 ; frame #$21
    .byte $10,$10 ; frame #$22
    .byte $11,$40 ; frame #$23
    .byte $0f,$16 ; frame #$24
    .byte $ff

; byte 0 - ENEMY_VAR_1,x (aim direction)
; byte 1 - ENEMY_DELAY,x (delay before changing direction)
alien_serpent_vel_03:
    .byte $03,$16 ; frame #$00
    .byte $05,$40 ; frame #$01
    .byte $04,$10 ; frame #$02
    .byte $03,$10 ; frame #$03
    .byte $02,$10 ; frame #$04
    .byte $01,$10 ; frame #$05
    .byte $00,$10 ; frame #$06
    .byte $17,$0a ; frame #$07
    .byte $16,$0a ; frame #$08
    .byte $15,$0a ; frame #$09
    .byte $14,$0a ; frame #$0a
    .byte $13,$0a ; frame #$0b
    .byte $12,$0a ; frame #$0c
    .byte $11,$0a ; frame #$0d
    .byte $10,$0a ; frame #$0e
    .byte $0f,$0a ; frame #$0f
    .byte $0e,$0a ; frame #$10
    .byte $0d,$0a ; frame #$11
    .byte $0c,$10 ; frame #$12
    .byte $0b,$0a ; frame #$13
    .byte $0a,$0a ; frame #$14
    .byte $09,$0a ; frame #$15
    .byte $08,$0a ; frame #$16
    .byte $07,$0a ; frame #$17
    .byte $06,$0a ; frame #$18
    .byte $05,$0a ; frame #$19
    .byte $04,$0a ; frame #$1a
    .byte $03,$0a ; frame #$1b
    .byte $02,$0a ; frame #$1c
    .byte $01,$0a ; frame #$1d
    .byte $00,$10 ; frame #$1e
    .byte $17,$10 ; frame #$1f
    .byte $16,$10 ; frame #$20
    .byte $15,$10 ; frame #$21
    .byte $14,$10 ; frame #$22
    .byte $13,$40 ; frame #$23
    .byte $15,$16 ; frame #$24
    .byte $ff

alien_serpent_apply_vel:
    lda ENEMY_Y_VEL_ACCUM,x
    clc                          ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x
    sta ENEMY_Y_VEL_ACCUM,x
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    adc ENEMY_Y_VELOCITY_FAST,x
    sta ENEMY_Y_POS,x
    lda ENEMY_X_VEL_ACCUM,x
    clc                          ; clear carry in preparation for addition
    adc ENEMY_X_VELOCITY_FRACT,x
    sta ENEMY_X_VEL_ACCUM,x
    lda ENEMY_X_POS,x            ; load enemy's X position
    adc ENEMY_X_VELOCITY_FAST,x
    sta ENEMY_X_POS,x
    rts

level_5_screen_layout_tbl:
    .byte $09                                 ; LEVEL_WIDTH
    .byte $07                                 ; LEVEL_HEIGHT
    .byte $07,$08,$09,$00,$00,$00,$00,$00,$00
    .byte $06,$0a,$0b,$0c,$00,$00,$00,$00,$00
    .byte $05,$00,$0a,$0d,$0e,$0f,$11,$00,$10
    .byte $04,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $03,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $02,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00

level_5_supertiles_screen_ptr_table:
    .addr level_5_supertiles_screen_00
    .addr level_5_supertiles_screen_01
    .addr level_5_supertiles_screen_02
    .addr level_5_supertiles_screen_03
    .addr level_5_supertiles_screen_04
    .addr level_5_supertiles_screen_05
    .addr level_5_supertiles_screen_06
    .addr level_5_supertiles_screen_07
    .addr level_5_supertiles_screen_08
    .addr level_5_supertiles_screen_09
    .addr level_5_supertiles_screen_0a
    .addr level_5_supertiles_screen_0b
    .addr level_5_supertiles_screen_0c
    .addr level_5_supertiles_screen_0d
    .addr level_5_supertiles_screen_0e
    .addr level_5_supertiles_screen_0f
    .addr level_5_supertiles_screen_10
    .addr level_5_supertiles_screen_11

level_5_supertiles_screen_00:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_5_supertiles_screen_01:
    .byte $32,$39,$3a,$3f,$40,$41,$3c,$0e,$33,$31,$32,$39,$3a,$3b,$5b,$1c
    .byte $26,$25,$25,$26,$26,$29,$34,$35,$57,$54,$55,$56,$34,$35,$38,$32
    .byte $58,$59,$34,$35,$38,$31,$32,$33,$00,$00,$28,$25,$26,$25,$26,$25
    .byte $42,$43,$42,$43,$42,$43,$42,$43,$46,$47,$46,$47,$46,$47,$46,$47

level_5_supertiles_screen_02:
    .byte $25,$26,$27,$18,$1b,$00,$3c,$0e,$1a,$1b,$00,$1c,$1f,$00,$00,$1c
    .byte $1f,$1f,$18,$1a,$19,$1b,$5a,$34,$58,$59,$1c,$1f,$1f,$1e,$35,$38
    .byte $19,$1b,$5b,$22,$00,$13,$0e,$30,$1f,$1e,$35,$2b,$58,$24,$25,$25
    .byte $3b,$28,$25,$27,$00,$00,$54,$55,$39,$3a,$00,$18,$19,$1a,$19,$1a

level_5_supertiles_screen_03:
    .byte $58,$59,$34,$35,$38,$0e,$32,$33,$00,$17,$2c,$3d,$26,$26,$25,$26
    .byte $3a,$3b,$00,$00,$00,$00,$54,$55,$3d,$2d,$16,$00,$58,$18,$19,$1a
    .byte $00,$58,$59,$00,$14,$1d,$1f,$1f,$3a,$9a,$9b,$58,$3c,$2d,$16,$5b
    .byte $0e,$39,$3a,$54,$55,$56,$34,$35,$25,$26,$27,$00,$00,$00,$13,$32

level_5_supertiles_screen_04:
    .byte $58,$59,$14,$3b,$00,$00,$00,$00,$00,$58,$20,$39,$3a,$54,$55,$56
    .byte $00,$00,$3c,$3e,$27,$22,$00,$5a,$58,$59,$21,$57,$17,$2f,$58,$59
    .byte $00,$00,$2a,$3a,$3b,$00,$00,$5b,$54,$55,$3c,$0e,$39,$16,$59,$34
    .byte $00,$58,$59,$1f,$1f,$1e,$35,$38,$00,$00,$00,$00,$35,$38,$31,$33

level_5_supertiles_screen_05:
    .byte $00,$00,$28,$25,$26,$25,$27,$00,$58,$14,$3b,$00,$00,$00,$00,$00
    .byte $00,$24,$29,$00,$00,$21,$54,$55,$00,$00,$34,$15,$58,$2e,$16,$59
    .byte $58,$59,$28,$27,$00,$5b,$34,$35,$55,$56,$5b,$00,$22,$00,$13,$37
    .byte $00,$00,$57,$17,$2f,$59,$24,$25,$00,$00,$00,$00,$00,$00,$00,$00

level_5_supertiles_screen_06:
    .byte $00,$34,$15,$00,$24,$25,$26,$27,$00,$28,$27,$58,$59,$00,$58,$59
    .byte $14,$3b,$00,$00,$00,$00,$00,$00,$20,$39,$3a,$00,$21,$00,$58,$59
    .byte $1c,$1c,$1f,$57,$2e,$16,$00,$00,$00,$00,$00,$58,$59,$34,$15,$00
    .byte $54,$55,$56,$34,$35,$38,$23,$59,$00,$00                         ; continues in bank 9

; end of bank
; unused #$0 bytes out of #$2,000 bytes total (100% full)
; unused 0 bytes out of 8,192 bytes total (100% full)
bank_8_unused_space: