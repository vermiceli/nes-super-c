; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank 3 is used for all levels when executing enemy routines.  It contains
; shared enemy logic, shared enemies, as well as foreground and background
; collision detection logic
; * Enemy Type #$00 - Weapon Item
; * Enemy Type #$01 - Flying Capsule
; * Enemy Type #$0e - Overhead Level Bullet
; * Enemy Type #$02 - Bullet
; * Enemy Type #$03 - Soldier (and Orian)
; * Enemy Type #$04 - Pill Box Sensor
; * Enemy Type #$05 - Grenade Thrower Soldier
; * Enemy Type #$06 - Grenade
; * Enemy Type #$07 - Sandbag Sniper
; * Enemy Type #$09 - Sniper
; * Enemy Type #$0a - Rotating Gun
; * Enemy Type #$0b - Gray Turret
; * Enemy Type #$0c - Enemy Explosion Animation
; * Enemy Type #$0d - Door
; * Enemy Type #$0f - Grenade Generator
; * Enemy Type #$10 - Soldier Generator

; 8 KiB PRG ROM
.segment "BANK3"

.include "constants.asm"

; import from bank 2
.import wall_cannon_routine_ptr_tbl
.import bg_storm_routine_ptr_tbl
.import helicopter_core_routine_ptr_tbl
.import helicopter_bay_routine_ptr_tbl
.import helicopter_turret_routine_ptr_tbl
.import crouching_sniper_routine_ptr_tbl
.import grass_covered_turret_routine_ptr_tbl
.import ground_mortar_routine_ptr_tbl
.import mortar_round_routine_ptr_tbl
.import robot_spider_routine_ptr_tbl
.import robot_spider_bullet_routine_ptr_tbl
.import jungle_earthquake_routine_ptr_tbl
.import fortress_wall_core_routine_ptr_tbl
.import fortress_wall_turret_routine_ptr_tbl
.import intro_helicopter_routine_ptr_tbl

; import from bank 4
.import manooki_routine_ptr_tbl
.import manooki_projectile_routine_ptr_tbl
.import spider_spawn_routine_ptr_tbl
.import alien_spider_routine_ptr_tbl
.import temple_of_terror_skull_routine_ptr_tbl
.import temple_of_terror_core_routine_ptr_tbl
.import poison_drop_gen_routine_ptr_tbl
.import poison_drop_routine_ptr_tbl
.import fire_ring_projectile_routine_ptr_tbl
.import temple_of_terror_red_blob_routine_ptr_tbl
.import red_bubble_routine_ptr_tbl
.import alien_mouth_routine_ptr_tbl
.import final_stage_red_blob_routine_ptr_tbl
.import alien_cyclops_routine_ptr_tbl
.import cyclops_projectile_routine_ptr_tbl
.import stomping_ceiling_routine_ptr_tbl
.import final_boss_routine_ptr_tbl
.import falling_rubble_routine_ptr_tbl
.import blue_blob_routine_ptr_tbl
.import final_boss_red_blob_routine_ptr_tbl
.import enemy_38_routine_ptr_tbl

; import from bank 8
.import jet_pack_soldier_routine_ptr_tbl
.import falling_rock_routine_ptr_tbl
.import storage_bay_routine_ptr_tbl
.import metal_covered_turret_routine_ptr_tb
.import metal_bullet_routine_ptr_tbl
.import krypto_crustacean_routine_ptr_tbl
.import alien_skull_routine_ptr_tbl
.import red_blob_routine_ptr_tbl
.import alien_ladybug_routine_ptr_tbl
.import mouth_pit_routine_ptr_tbl
.import mouth_pit_gen_routine_ptr_tbl
.import big_face_routine_ptr_tbl
.import baby_alien_ladybug_routine_ptr_tbl
.import area_6_chr_swap_routine_ptr_tbl
.import suspicious_face_arm_routine_ptr_tbl
.import suspicious_face_routine_ptr_tbl
.import boss_baby_alien_ladybug_routine_ptr_tbl
.import jagger_froid_routine_ptr_tbl
.import alien_serpent_routine_ptr_tbl
.import jagger_froid_projectile_routine_ptr_tbl

; import from bank a
.import overhead_tank_soldier_routine_ptr_tbl
.import overhead_soldier_routine_ptr_tbl
.import stationary_red_soldier_routine_ptr_tbl
.import overhead_tank_soldier_bullet_routine_ptr_tbl
.import overhead_rotating_turret_routine_ptr_tbl
.import tank_boss_routine_ptr_tbl
.import tank_gunner_routine_ptr_tbl
.import elevator_routine_ptr_tbl
.import winged_soldier_gen_routine_ptr_tbl
.import winged_soldier_routine_ptr_tbl
.import rack_turret_routine_ptr_tbl
.import spinning_bubbles_routine_ptr_tbl
.import laser_chandelier_routine_ptr_tbl
.import chandelier_arm_routine_ptr_tbl
.import chandelier_arm_laser_routine_ptr_tbl
.import ceiling_vent_routine_ptr_tbl
.import ceiling_vent_bubble_routine_ptr_tbl
.import collapsible_ceiling_routine_ptr_tbl
.import falling_ceiling_tile_routine_ptr_tbl
.import tank_boss_electrode_routine_ptr_tbl

; import from bank f
.import run_routine_from_tbl_below
.import add_single_bcd_to_score
.import add_score_to_player
.import kill_player_x
.import create_enemy_y
.import get_landing_y_pos
.import clear_enemy_type
.import play_sound
.import bg_collision_test_tbl
.import load_banks_create_f_child_flames_or_destroy
.import load_sound_banks_init_channels
.import replace_bullet_with_enemy
.import bg_collision_test_no_incline_tbl
.import fire_bullet_at_player
.import get_rotate_00
.import rotate_aim_dir_00_zp
.import load_banks_update_supertile_and_palette_if_room
.import load_banks_update_supertile_and_palette
.import calc_velocities_for_dir
.import get_rotate_01
.import set_enemy_velocity
.import rotate_aim_dir_01

; export for bank 0
.export get_bg_collision_y_range
.export is_on_ground_water
.export overhead_player_get_bg_collision

; export for bank 2
.export clear_irq_bg_collision_data
.export clear_partial_irq_collision_data
.export update_nametable_square
.export grenade_routine_02
.export robot_spider_get_ppu_addr
.export set_nametable_row
.export robot_spider_routine_04
.export jungle_earthquake
.export x_earthquake_shake
.export clear_partial_bg_collision_data
.export clear_nametable_row

; export for bank 4
.export test_player_fg_collision

; export for bank 8
.export player_enemy_y_dist
.export modify_enemy_x_vel
.export get_falling_enemy_bg_collision_code
.export apply_x_velocity
.export set_destroyed_enemy_routine
.export set_enemy_vel_from_aim_dir

; export for bank a
.export overhead_tank_soldier_draw_destroyed
.export load_banks_update_supertiles
.export set_enemy_destroy_dir_and_vel
.export enemy_bullet_routine_01
.export enemy_overhead_bullet_routine_03
.export enemy_overhead_bullet_routine_04
.export update_nametable_square_at_pos
.export apply_destroy_vel_adv_routine_after_delay

; export for bank f
.export exe_enemy_routines

; export for multiple banks
.export check_bg_collision_at_y               ; bank 0, bank a
.export get_bg_collision_code                 ; bank 0, bank 8
.export set_delay_adv_enemy_routine           ; bank 2, bank 4, bank 8, bank a
.export update_enemy_pos                      ; bank 2, bank 4, bank 8, bank a
.export set_enemy_hp_hard                     ; bank 2, bank 4, bank 8, bank a
.export init_bg_boss_pre_irq_max_scrolls      ; bank 2, bank 4, bank 8, bank a
.export try_create_enemy_from_existing        ; bank 2, bank 4, bank 8, bank a
.export advance_enemy_routine                 ; bank 2, bank 4, bank 8, bank a
.export enemy_routine_boss_defeated_00        ; bank 2, bank 4, bank 8, bank a
.export enemy_routine_boss_defeated_01        ; bank 2, bank 4, bank 8, bank a
.export bg_enemy_explosion_routine_00         ; bank 2, bank 4, bank 8, bank a
.export remove_enemy                          ; bank 2, bank 4, bank 8, bank a
.export set_boss_defeated_remove_enemy        ; bank 2, bank 4, bank 8, bank a
.export flip_enemy_x_dir                      ; bank 2, bank 4, bank a
.export get_enemy_bg_collision_code_onscreen  ; bank 2, bank 4, bank a
.export update_pos_check_offscreen            ; bank 2, bank 4, bank a
.export copy_enemy_vars_to_zp                 ; bank 2, bank 4, bank 8, bank a, bank f
.export set_enemy_routine                     ; bank 2, bank 4, bank 8, bank a
.export clear_enemy_sprite                    ; bank 2, bank a
.export enemy_routine_explosions              ; bank 2, bank a
.export enemy_explosion_routine_00            ; bank 2, bank 4, bank 8, bank a
.export enemy_explosion_routine_01            ; bank 2, bank 4, bank 8, bank a
.export enemy_explosion_routine_03            ; bank 2, bank 4, bank 8, bank a
.export enemy_routine_init_explosions         ; bank 2, bank a
.export player_enemy_x_dist                   ; bank 2, bank 4, bank 8, bank a
.export set_enemy_hp                          ; bank 2, bank 4, bank 8, bank a
.export apply_gravity_adv_routine_after_delay ; bank 2, bank a
.export set_enemy_destroy_sprite_and_vel      ; bank 2, bank a
.export get_enemy_bg_collision_code           ; bank 2, bank 4, bank 8, bank a
.export apply_velocity                        ; bank 2, bank 4, bank 8, bank a
.export enemy_bullet_routine_00               ; bank 2, bank a
.export clear_enemy_vel                       ; bank 2, bank 4, bank a
.export bg_boss_apply_vel                     ; bank 2, bank 4, bank 8, bank a
.export set_enemy_animation_sprite            ; bank 2, bank 4, bank 8, bank a
.export flip_enemy_y_dir                      ; bank 2, bank 8, bank a
.export set_y_pos_for_bg                      ; bank 4, bank 8, bank a
.export set_enemy_hp_from_a_and_y             ; bank 8, bank a
.export load_banks_update_enemy_supertiles    ; bank 2, bank 4, bank 8
.export clear_bg_collision_data               ; bank 8, bank a
.export modify_enemy_y_vel                    ; bank 2, bank 8
.export bg_enemy_set_scrolls                  ; bank 2, bank 4, bank 8
.export set_vel_to_target_player              ; bank 8, bank 4, bank a
.export bg_enemy_explosion_routine_01         ; bank 2, bank 4, bank 8, bank a
.export get_bg_collision                      ; bank 0, bank 8, bank a
.export add_a_to_enemy_y_fract_vel            ; bank 4, bank 8
.export clear_enemy_x_vel                     ; bank 4, bank 8
.export update_enemy_nametable_supertile      ; bank 4, bank 8
.export clear_enemy_y_vel                     ; bank 4, bank 8
.export check_bg_wall_collision               ; bank 4, bank a
.export hone_to_player_set_enemy_vel          ; bank 4, bank 8, bank a
.export backup_scroll_and_ppuctrl             ; bank 4, bank 8
.export earthquake_shake                      ; bank 4, bank 8
.export shake_enemy_pattern                   ; bank 2, bank 4
.export set_bg_boss_scroll_nt                 ; bank 4, bank a
.export copy_enemy_vars                       ; bank 4, bank 8
.export restore_scroll_and_ppuctrl            ; bank 4, bank 8

; broken up by player action state with special #$9f action state for bullets
; base number is PLAYER_ACTION_STATE + ENEMY_COLLISION_INDEX
; base row within action state is dependent on enemy type (see enemy_collision_code_tbl)
; byte 0 - negative offset from player/bullet Y position for collision tests (start Y pos)
; byte 1 - negative offset from player/bullet X position for collision tests (start X pos)
; byte 2 - collision box height
; byte 3 - collision box width
collision_box_tbl:
; PLAYER_ACTION_STATE = #$ff (normal)
    .byte $ea,$f4,$2a,$16
    .byte $f0,$fa,$1e,$0a
    .byte $e5,$f7,$34,$10
    .byte $e1,$eb,$3c,$28
    .byte $f1,$ef,$28,$20
    .byte $e1,$d3,$3c,$58
    .byte $d1,$db,$6c,$48
    .byte $d1,$eb,$5c,$28

; PLAYER_ACTION_STATE = #$1f (jumping)
    .byte $f2,$f2,$1a,$1a
    .byte $f8,$f8,$0e,$0e
    .byte $ed,$f5,$24,$14
    .byte $e9,$e9,$2c,$2c
    .byte $f9,$ed,$18,$24
    .byte $e9,$d1,$2c,$5c
    .byte $d9,$d9,$5c,$4c
    .byte $d9,$e9,$4c,$2c

; PLAYER_ACTION_STATE = #$3f (crouching)
    .byte $ea,$ea,$16,$2a
    .byte $f0,$f0,$0a,$1e
    .byte $e5,$ed,$20,$24
    .byte $e1,$e1,$28,$3c
    .byte $f1,$e5,$14,$34
    .byte $e1,$c9,$28,$6c
    .byte $d1,$d1,$58,$5c
    .byte $d1,$e1,$48,$3c

; PLAYER_ACTION_STATE = #$5f (crouching on incline)
    .byte $ea,$f4,$22,$16
    .byte $f0,$fa,$16,$0a
    .byte $e5,$f7,$2c,$10
    .byte $e1,$eb,$34,$28
    .byte $f1,$ef,$20,$20
    .byte $e1,$d3,$34,$58
    .byte $d1,$db,$64,$48
    .byte $d1,$eb,$54,$28

; PLAYER_ACTION_STATE = #$7f (in water)
    .byte $f8,$f4,$1a,$16
    .byte $fe,$fa,$0e,$0a
    .byte $f3,$f7,$24,$10
    .byte $ef,$eb,$2c,$28
    .byte $ff,$ef,$18,$20
    .byte $ef,$d3,$2c,$58
    .byte $df,$db,$5c,$48
    .byte $df,$eb,$4c,$28

; PLAYER_ACTION_STATE = #$9f (bullet collision box)
    .byte $f3,$f3,$18,$18
    .byte $f7,$f7,$10,$10
    .byte $ec,$f3,$26,$18
    .byte $eb,$eb,$28,$28
    .byte $fb,$ef,$14,$20
    .byte $eb,$d3,$28,$58
    .byte $d3,$eb,$28,$28
    .byte $db,$eb,$48,$28

; PLAYER_ACTION_STATE = #$bf (overhead level)
    .byte $f0,$f4,$1e,$16
    .byte $f6,$fa,$12,$0a
    .byte $eb,$f7,$28,$10
    .byte $e7,$eb,$30,$28
    .byte $f7,$ef,$1c,$20
    .byte $e7,$d3,$30,$58
    .byte $d7,$db,$60,$48
    .byte $d7,$eb,$50,$28

; test player to enemy (or enemy bullet) collision and handle collision
; if player invincible (B weapon), then kills enemy
; if player invincible after dying, do nothing
; if enemy is a weapon item, pick it up
; (alternates which player to test every frame)
; for details see docs/Collision Detection.md
; input
;  * x - enemy slot index
;  * $06 - ENEMY_Y_POS
;  * $05 - ENEMY_X_POS
;  * $07 - ENEMY_COLLISION_INDEX
test_player_fg_collision:
    lda GLOBAL_TIMER
    and #$01                           ; select player based on frame is even or odd
    tax
    lda PLAYER_ACTION_STATE,x          ; load player action state for selected player
    sec
    adc $07                            ; add ENEMY_COLLISION_INDEX + 1
    tay                                ; y = PLAYER_ACTION_STATE + ENEMY_COLLISION_INDEX + 1
                                       ; this is the row within collision_box_tbl where the collision configuration is
    lda PLAYER_SPRITE_X_POS,x          ; load player X position
    sbc collision_box_tbl+1,y          ; subtract collision box center x offset (since value is negative, effectively adding)
                                       ; this is the amount of x distance on the left side of the enemy to consider collision
    sbc $05                            ; PLAYER_SPRITE_X_POS - collision_box_center_x_offset - ENEMY_X_POS
    cmp collision_box_tbl+3,y          ; see if player is within collision box width
                                       ; this works for both when enemy to right of player or when enemy to left of player
                                       ; when approaching from left value will be negative, until 0
                                       ; when approaching from the right value will eventually equal width
    bcs @exit                          ; exit if player-enemy is not within the collision box width
    lda PLAYER_SPRITE_Y_POS,x          ; player in collision box width, check collision box height
    sbc collision_box_tbl,y            ; subtract collision box center y offset
    sbc $06                            ; (PLAYER_SPRITE_Y_POS + collision_box_center_y_offset) - ENEMY_Y_POS
    cmp collision_box_tbl+2,y          ; see if player is within collision box height
    bcc @player_in_enemy_collision_box ; branch if player within collision box (both width and height)

@exit:
    rts ; no player enemy collision, exit

@player_in_enemy_collision_box:
    stx $11                     ; store player index in $11
    jsr @player_enemy_collision
    ldx $11                     ; restore player index from $11

@exit2:
    rts

; player-enemy collision
@player_enemy_collision:
    ldy $11                     ; load player index
    ldx ENEMY_CURRENT_SLOT      ; load enemy slot index
    lda ENEMY_DESTROY_ATTRS,x   ; load to see if should test player collision (bit 0 = 0)
    ora PLAYER_SKIP_COLLISION,y ; 1 to skip player sprite collision tests, 0 otherwise
    lsr                         ; shift whether or not to test player collision to carry
    bcs @exit2                  ; exit if not testing player collision
    lda PLAYER_SPRITE_X_POS,y   ; testing player enemy collision, load player X position
    sec                         ; set carry flag in preparation for subtraction
    sbc $05                     ; subtract collision point x offset from player position
    bcs @handle_collision
    eor #$ff
    adc #$01

@handle_collision:
    cmp #$40
    bcs @exit2
    lda ENEMY_TYPE,x
    beq @pick_up_weapon_item           ; branch if player collided with weapon item
    lda NEW_LIFE_INVINCIBILITY_TIMER,y
    bne @exit2                         ; exit if player still in invincible state after spawning
    lda INVINCIBILITY_TIMER,y
    bne @player_invincible             ; branch if player invincible to kill enemy
    ldx $11                            ; player not invincible and collided with enemy, kill player
                                       ; load player index of player who collided with enemy
    jmp kill_player_x                  ; play player death sound and update player state for player x

@player_invincible:
    sty PLAYER_INDEX               ; store player index in $21
    lda ENEMY_HP,x                 ; load enemy's hp
    cmp #$f0                       ; see if special invincible hp codes
    bcs @exit2                     ; exit if enemy has special invincible hp
    jmp add_to_score_set_destroyed ; enemy is not invincible, kill enemy

; player has collided with a weapon item, pick it up
@pick_up_weapon_item:
    sty PLAYER_INDEX               ; store player index in $21
    lda #$50
    sta $01                        ; adding 500 to player's score
    jsr add_single_bcd_to_score    ; add score, stored as binary-coded decimal in $01, to player's score
                                   ; value in $01 represents the 100s and 10s place
                                   ; if appropriate, handles giving 1-up, and/or setting new high score
    lda #$19                       ; pick up weapon item sound
    jsr play_sound                 ; sound_19 (POWER) - pick up weapon item
    ldx ENEMY_CURRENT_SLOT
    lda ENEMY_ATTRIBUTES,x
    and #$0f                       ; load weapon item
    ldy $11                        ; load player index
    jsr run_routine_from_tbl_below

weapon_item_pickup_tbl:
    .addr weapon_item_pickup        ; machine (M)
    .addr weapon_item_pickup        ; spray (S)
    .addr weapon_item_pickup        ; laser (L)
    .addr weapon_item_pickup        ; flame (F)
    .addr r_weapon_item_pickup      ; rapid fire (R)
    .addr b_weapon_item_pickup      ; barrier (B)
    .addr falcon_weapon_item_pickup ; falcon

; B weapon item
b_weapon_item_pickup:
    lda #$80
    sta INVINCIBILITY_TIMER,y ; set player invincible timer to #$80 frames
    jmp remove_enemy          ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0

; falcon weapon item
falcon_weapon_item_pickup:
    jsr destroy_all_enemies ; destroy all appropriate enemies
    jmp remove_enemy        ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0

r_weapon_item_pickup:
    lda PLAYER_CURRENT_WEAPON,y
    ora #$80                          ; set rapid fire flag
    bne set_weapon_remove_weapon_item ; always branch

; weapon item pickup for MSLF weapons
weapon_item_pickup:
    lda ENEMY_ATTRIBUTES,x
    and #$0f
    clc                    ; clear carry in preparation for addition
    adc #$01               ; convert from attribute to specific weapon

set_weapon_remove_weapon_item:
    sta PLAYER_CURRENT_WEAPON,y
    jmp remove_enemy            ; remove weapon item
                                ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0

; test every bullet to see if it collides with the specified enemy
; input
;  * $06 - ENEMY_Y_POS
;  * $05 - ENEMY_X_POS
;  * $07 - ENEMY_COLLISION_INDEX
test_player_bullet_collision:
    ldx #$0f ; number of player bullets
    sec

; check to see if bullet is within enemy's bullet-specific collision box
; for details see docs/Collision Detection.md
@player_bullet_loop:
    lda PLAYER_BULLET_COLLISION_CODE,x  ; load player bullet collision code (offset into collision_box_tbl)
                                        ; seems to always be #$9f
    beq @next_player_bullet             ; continue to next bullet if no bullet in current slot
    adc $07                             ; add ENEMY_COLLISION_INDEX
    tay                                 ; y = PLAYER_BULLET_COLLISION_CODE + ENEMY_COLLISION_INDEX + 1
                                        ; y now specifies which row in collision_box_tbl to use
    lda PLAYER_BULLET_X_POS,x           ; load player bullet X position
    sbc collision_box_tbl+1,y           ; subtract collision box center x offset (since value is negative, effectively adding)
                                        ; this is the amount of x distance on the left side of the enemy to consider collision
    sbc $05                             ; PLAYER_BULLET_X_POS - collision_box_center_x_offset - ENEMY_X_POS
    cmp collision_box_tbl+3,y           ; see if bullet is within collision box width
                                        ; this works for both when enemy to right of bullet or when enemy to left of bullet
                                        ; when approaching from left value will be negative, until 0
                                        ; when approaching from the right value will eventually equal width
    bcs @next_player_bullet             ; exit if player bullet x is not within the collision box width
    lda PLAYER_BULLET_Y_POS,x           ; player bullet in collision box width, check collision box height
    sbc collision_box_tbl,y
    sbc $06                             ; (PLAYER_BULLET_Y_POS + collision_box_center_y_offset) - ENEMY_Y_POS
    cmp collision_box_tbl+2,y
    bcc @player_bullet_in_collision_box ; branch if player within collision box (both width and height)

@next_player_bullet:
    dex                     ; move to next bullet
    bpl @player_bullet_loop ; loop if more bullets

@exit:
    rts

@player_bullet_in_collision_box:
    ldy ENEMY_CURRENT_SLOT    ; load enemy index
    lda ENEMY_DESTROY_ATTRS,y
    bmi @exit                 ; exit if configured to allow bullets to travel through enemy (bit 7 set)
    lda PLAYER_BULLET_X_POS,x
    sec                       ; set carry flag in preparation for subtraction
    sbc $05                   ; get x distance between player bullet and enemy
    bcs @continue             ; branch if bullet to right of enemy (already positive distance)
    eor #$ff                  ; convert from negative to positive
    adc #$01                  ; overflow, flip all bits and add 1

@continue:
    bmi @exit                                       ; check if bullet was really far from enemy, but still collided
                                                    ; e.g. bullet collides with enemy but distance is larger than #$7f
                                                    ; which shouldn't be possible
    lda PLAYER_BULLET_WEAPON_TYPE,x                 ; load weapon type and player that created the bullet
    rol
    rol
    rol
    and #$01                                        ; bit 6 of PLAYER_BULLET_WEAPON_TYPE specifies bullet owner
    sta PLAYER_INDEX                                ; store who fired bullet in $21 (0 = p1, 1 = p2)
    lda PLAYER_BULLET_DMG,x                         ; load how much damage bullet should give
    ldy PLAYER_BULLET_X_POS,x                       ; load bullet X position
    jsr @player_bullet_attack_enemy
    bne @exit
    lda #$00
    sta PLAYER_BULLET_COLLISION_CODE,x              ; bullet used, clear bullet collision code
    jmp load_banks_create_f_child_flames_or_destroy ; create child flames for F weapon or destroy if not F weapon

@player_bullet_attack_enemy:
    sta $08                         ; store bullet damage
    stx $11
    ldy ENEMY_CURRENT_SLOT          ; load enemy index
    lda ENEMY_HP,y                  ; load enemy's HP
    cmp #$f0                        ; see if special HP where can't be destroyed
    bcs @play_invincible_sound_exit
    sbc $08                         ; subtract bullet damage
    bcs @hit_enemy                  ; branch if enemy HP is still positive
    lda #$00                        ; if resulting HP is negative, set to 0

@hit_enemy:
    sta ENEMY_HP,y                 ; set enemy's updated HP
    bne @play_damage_sound_exit    ; branch if enemy still has HP
    ldx ENEMY_CURRENT_SLOT         ; enemy has 0 HP, destroy
    jsr add_to_score_set_destroyed
    ldx $11
    dec $08
    bmi @exit2
    ldy #$01
    rts

@play_damage_sound_exit:
    lda #$11 ; sound_11

@play_sound_exit:
    jsr play_sound ; play sound_11 (T DAMEGE) enemy takes damage

@exit2:
    ldy #$00
    rts

; !(OBS) - probably intended to have a separate sound for this case
; otherwise, could have just used @play_damage_sound_exit
; both play the same sound_11 sound code
@play_invincible_sound_exit:
    lda #$11             ; sound_11
    bne @play_sound_exit ; always branch

; execute all of the active enemy routines
exe_enemy_routines:
    ldy #$00
    lda Y_SCROLL_SPEED ; load how much to scroll vertically this frame
    bpl @continue      ; branch if no vertical scroll or downward scroll
    dey                ; scrolling up

@continue:
    sty SCROLLING_UP_FLAG ; set scroll up flag (#$ff = scrolling up, #$00 = not scrolling (or scrolling down))
    ldx #$0d              ; initialize enemy slot index. 14 enemies total in slots [0-13]

exe_enemy_routine_loop:
    stx ENEMY_CURRENT_SLOT           ; set enemy slot index
    lda ENEMY_SPRITE,x               ; load current enemy's sprite
    beq @continue                    ; if no sprite for enemy, no need to check position, simply execute routine
    lda ENEMY_Y_POS,x                ; sprite is not 0, load enemy Y position
    sta $06
    lda ENEMY_X_POS,x                ; load enemy's X position
    sta $05
    lda ENEMY_COLLISION_INDEX,x      ; load enemy's collision index (specifies collision box dimensions)
    sta $07
    jsr test_player_fg_collision     ; test player enemy (or enemy bullet) collision
                                     ; killing player or enemy on collision, picking up weapon item, etc.
    ldx ENEMY_CURRENT_SLOT           ; restore enemy offset
    jsr test_player_bullet_collision ; test every player bullet to see if it collides with the specified enemy
    ldx ENEMY_CURRENT_SLOT           ; restore enemy offset

@continue:
    jsr exe_enemy_routine
    dex                        ; decrement enemy offset
    bpl exe_enemy_routine_loop ; branch to go to the next enemy if more to process
    rts                        ; processed all enemies, exit

; executes the xth enemy routine
; input
;  * x - enemy slot index
exe_enemy_routine:
    lda ENEMY_TYPE,x              ; load current enemy type
    asl                           ; double since enemy type pointer is 2 bytes
    tay                           ; store offset into pointer table into y
    lda enemy_routine_ptr_tbl,y   ; load low byte of the routine pointer
    sta $02                       ; store in $02
    lda enemy_routine_ptr_tbl+1,y ; load high byte of the enemy pointer
    sta $03                       ; store enemy routine high byte into $03
    lda ENEMY_ROUTINE,x           ; load the current routine index
                                  ; subtract 1 to get real routine, since all offsets are off by 1 (...routine_ptr_tbl-2)
                                  ; ex: for flying capsule, ENEMY_ROUTINE = 1 runs flying_capsule_routine_00
    asl
    tay
    lda ($02),y                   ; load the low byte of the enemy sub-routine to execute
    sta $04                       ; store in $04
    iny                           ; increment read offset
    lda ($02),y                   ; load the high byte of the enemy sub-routine to execute
    sta $05                       ; store in $05
    jmp ($04)                     ; jump to that sub-routine, e.g. flying_capsule_routine_00

; sets the enemy type to #$7f (slot available), and sets the enemy sprite to 0
; input
;  * x - enemy slot index
remove_enemy:
    lda #$00
    sta ENEMY_ROUTINE,x
    jsr clear_enemy_type ; marks enemy type as #$7f, indicating the enemy slot is free

; sets enemy sprite to #$00
; input
;  * x - enemy slot index
clear_enemy_sprite:
    lda #$00
    sta ENEMY_SPRITE,x
    rts

; if enemy is initialized, e.g. routine is set, then sets the enemy routine to a.
; otherwise, the sprite will be cleared (set to 0)
; remember enemy routines are off by one, so setting ENEMY_ROUTINE to #$03, results in the 2nd routine being run
; ex: for overhead tank soldier, setting ENEMY_ROUTINE to #$02 causes overhead_tank_soldier_routine_01 to run the next frame
; input
;  * a - enemy routine to set
;  * x - enemy slot index
set_enemy_routine:
    ldy ENEMY_ROUTINE,x
    beq clear_enemy_sprite ; clear sprite if enemy routine is not set
    sta ENEMY_ROUTINE,x    ; set routine
    rts

; grenade enemy (enemy type #$06) destroyed routine
; mortar round (enemy type #$29) enemy destroyed routine
grenade_routine_02:
    jsr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    lda #$0c
    sta ENEMY_COLLISION_INDEX,x    ; set enemy's collision index (specifies collision box dimensions)
    lda #$82
    sta ENEMY_DESTROY_ATTRS,x      ; set spike explosion and allow bullets to travel through enemy
    rts

; set empty sprite, play optional enemy destroyed sound, disable collisions, advance routine
; red bubble (enemy type #$2a) enemy destroyed routine
; falling ceiling tile (enemy type #$2d) enemy destroyed routine
; falling rock (enemy type #$2f) enemy destroyed routine
; spinning bubbles (enemy type #$31) enemy destroyed routine
; blue blob (enemy type #$37) enemy destroyed routine
; manooki (enemy type #$3b) enemy destroyed routine
; alien spider (enemy type #$3e) enemy destroyed routine
; red poisonous insect gel (enemy type #$46) enemy destroyed routine
; fortress wall turret (enemy type #$50) enemy destroyed routine
; fire ring projectile (enemy type #$54) enemy destroyed routine
; temple of terror red blob (enemy type #$55) enemy destroyed routine
; poison drop (enemy type #$56) enemy destroyed routine
; alien skull (enemy type #$5a) enemy destroyed routine
; red soldier (enemy type #$5c) enemy destroyed routine
; big face (enemy type #$5f) enemy destroyed routine
; boss baby alien ladybug (enemy type #$61) enemy destroyed routine
; overhead soldier (enemy type #$63) enemy destroyed routine
; jagger froid projectile (enemy type #$68) enemy destroyed routine
; ceiling vent bubbles (enemy type #$6b) enemy destroyed routine
; ceiling vent (enemy type #$6c) enemy destroyed routine
enemy_explosion_routine_00:
    lda ENEMY_SPRITE,x
    beq remove_enemy     ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; set empty sprite, play optional enemy destroyed sound, disable collisions, advance routine
; used by helicopter core, fortress wall core, final boss, temple of terror skull,
; krypto-crustacean, jagger froid, suspicious face, tank boss, and laser chandelier
bg_enemy_explosion_routine_00:
    lda #$02
    sta ENEMY_SPRITE_ATTR,x
    lda ENEMY_DESTROY_ATTRS,x          ; load enemy's sound to play when being destroyed
    lsr
    lsr
    and #$0f                           ; strip to just sound code bits
    tay
    lda enemy_destroyed_sound_tbl,y    ; load sound to play for destroyed enemy
    beq @continue                      ; branch if no sound is played for destroyed enemy
    bpl @play_sound                    ; branch if not negative, i.e. doesn't stop bg music
    pha                                ; when sound code is negative, reinitialize channels (stops playing bg music)
    jsr load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels
    pla
    and #$7f

@play_sound:
    jsr play_sound

@continue:
    lda #$01
    sta ENEMY_SPRITE,x          ; set empty sprite (sprite_01)
    lda #$ff
    sta ENEMY_FRAME,x           ; mark enemy frame
    lda #$01
    sta ENEMY_ANIMATION_DELAY,x
    lda ENEMY_DESTROY_ATTRS,x
    ora #$81
    sta ENEMY_DESTROY_ATTRS,x   ; disable player-enemy collision and player bullet-enemy collision

; advance enemy routine to next routine for current enemy slot
; input
;  * x - enemy slot index
advance_enemy_routine:
    lda ENEMY_ROUTINE,x    ; load enemy routine index
    beq clear_enemy_sprite ; if routine not set, exit
    inc ENEMY_ROUTINE,x    ; increment enemy routine index
    rts

; set the animation delay to a and advanced the ENEMY_ROUTINE
; input
;  * a - the ENEMY_DELAY
set_delay_adv_enemy_routine:
    sta ENEMY_DELAY,x
    jmp advance_enemy_routine ; advance to next routine

; sound codes for when an enemy is destroyed based on bits 2345 of ENEMY_DESTROY_ATTRS
enemy_destroyed_sound_tbl:
    .byte $13                         ; sound_13 (T OUT) - soldier destroyed
    .byte $1b                         ; sound_1b (BAKUHA1) - explosion
    .byte $1c                         ; sound_1c (BAKUHA2) - explosion
    .byte $1a                         ; sound_1a (BOSS BK) - boss explosion
    .byte $a6                         ; sound_26 (B OUT) - boss destroy
    .byte $14                         ; sound_14 (Z OUT)
    .byte $12                         ; sound_12 (HARETSU)
    .byte $18                         ; sound_18 (APPEAR)
    .byte $24                         ; sound_24
    .byte $00,$00,$00,$00,$00,$00,$00 ; no sound

bg_enemy_explosion_routine_01:
    lda ENEMY_SPRITE,x
    beq advance_enemy_routine       ; advance to next routine
    bne destroy_explosion_animation ; always branch to animate explosion

; animate explosion sequence (spiky or circular explosion)
enemy_explosion_routine_01:
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; animate sequence of explosions
destroy_explosion_animation:
    lda ENEMY_ROUTINE,x
    beq @exit
    dec ENEMY_ANIMATION_DELAY,x
    bne @exit
    lda ENEMY_DESTROY_ATTRS,x   ; loading explosion type (spike explosion or circular explosion)
    lsr
    lsr
    lda #$04                    ; assume using spike explosion animation
    bcc @continue               ; branch if using spike explosion animation
    lda #$03                    ; bit 2 set, use circular explosion animation

@continue:
    sta $00                             ; set number of explosions in animation
    inc ENEMY_FRAME,x
    ldy ENEMY_FRAME,x
    cpy $00                             ; see if displayed all explosions
    bcs advance_enemy_routine           ; finished animating explosion, advance to next routine
    lda #$05                            ; there are more explosions to animate
    sta ENEMY_ANIMATION_DELAY,x         ; set animation delay to #$05 frames
    lda ENEMY_DESTROY_ATTRS,x           ; loading explosion type (spike explosion or circular explosion)
    lsr
    lsr
    lda spike_explosion_sprite_tbl,y    ; assume using spike explosion animation
    bcc @set_sprite                     ; branch if using spike explosion animation
    lda circular_explosion_sprite_tbl,y ; bit 2 set, use circular explosion animation

@set_sprite:
    sta ENEMY_SPRITE,x

@exit:
    rts

; sets boss defeated flag, strips alive attribute bit, and removes enemy
set_boss_defeated_remove_enemy:
    lda #$80
    sta BOSS_DEFEATED_FLAGS           ; set level boss removed bit (bit 7)
    bne strip_alive_attr_remove_enemy ; always branch

; mark destroyed, remove enemy
enemy_explosion_routine_03:
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)

strip_alive_attr_remove_enemy:
    lda ENEMY_ATTRIBUTES,x
    and #$fb
    sta ENEMY_ATTRIBUTES,x ; clear bit 2
    ldy #$ff
    jmp remove_enemy       ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0

; spike explosion animation
; sprite_02, sprite_0f, sprite_10, sprite_11
spike_explosion_sprite_tbl:
    .byte $02,$0f,$10,$11

; circular explosion animation
; sprite_12, sprite_13, sprite_14
circular_explosion_sprite_tbl:
    .byte $12,$13,$14

; initialize values for 5 explosions (used in enemy_routine_explosions)
; wall cannon (enemy type #$20) enemy destroyed routine
; overhead tank soldier (enemy type #$2e) enemy destroyed routine
; helicopter turret (enemy type #$22) enemy destroyed routine
; ground mortar (enemy type #$28) enemy destroyed routine
; overhead rotating turret (enemy type #$5b) enemy destroyed routine
; tank gunner (enemy type #$6a) enemy destroyed routine
enemy_routine_init_explosions:
    lda ENEMY_DESTROY_ATTRS,x
    and #$e3
    ora #$04                  ; these enemies have either #$01 (sound_1b) or #$09 (no sound)
    sta ENEMY_DESTROY_ATTRS,x

; enemy destroyed routine
enemy_door_routine_03:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$05
    sta ENEMY_VAR_4,x               ; generate 5 explosions
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to enemy_routine_explosions

; generate ENEMY_VAR_4 explosions, with an #$08 frame delay between each one
; can generate up to #$05 explosions
; door (enemy type #$0d)
; wall cannon (enemy type #$20)
; helicopter turret (enemy type #$22)
; ground mortar (enemy type #$28)
; overhead tank soldier (enemy type #$2e)
; rotating turret (enemy type #$5b)
; tank gunner (enemy type #$6a)
enemy_routine_explosions:
    jsr update_enemy_pos                  ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                     ; decrement delay until generate next explosion
    bne standard_explosion_exit           ; exit if delay hasn't elapsed
    lda #$08                              ; delay elapsed, re-initialize delay until next explosion
    sta ENEMY_DELAY,x                     ; set delay until next explosion
    dec ENEMY_VAR_4,x                     ; decrement number of remaining explosions
    bmi standard_explosion_adv_routine    ; advance routine if generated all explosions
    ldy #$0c                              ; enemy type = enemy explosion
    jsr try_create_enemy_from_existing    ; create enemy explosion
    bcc standard_explosion_exit           ; branch if unable to create enemy explosion
    ldy ENEMY_VAR_4,x                     ; load current explosion index
    lda standard_explosion_y_offset_tbl,y ; load y offset from enemy position
    adc ENEMY_Y_POS,x                     ; add y offset to enemy position
    sta $08                               ; store Y position in $08
    ror                                   ; move overflow flag to bit 7, bit 0 to carry
    eor standard_explosion_y_offset_tbl,y ; checking if bit 7 of offset is set and no carry
                                          ; or vice versa: bit 7 of offset is clear and a carry
                                          ; this checks if offscreen
    bmi clear_created_enemy_sprite        ; branch if off screen below or above
                                          ; set explosion sprite to #$00 (not visible)
    lda standard_explosion_x_offset_tbl,y ; load x offset from enemy position
    adc ENEMY_X_POS,x                     ; add x offset from enemy position
    sta $09                               ; store Y position in $09
    ror
    eor standard_explosion_x_offset_tbl,y ; checking if bit 7 of offset is set and no carry
                                          ; or vice versa: bit 7 of offset is clear and a carry
                                          ; this checks if offscreen
    bpl set_explosion_pos_and_sound       ; branch to continue if not off screen to the left nor right
                                          ; to set the explosion position to ($09,$08)

; set enemy in slot $11's sprite to #$00 (not visible)
; used to clear explosions
; input
;  * $11 - enemy slot index
clear_created_enemy_sprite:
    ldy $11            ; load enemy slot of created explosion
    lda #$00
    sta ENEMY_SPRITE,y ; clear explosion sprite

standard_explosion_exit:
    rts

standard_explosion_adv_routine:
    jmp advance_enemy_routine ; advance to next routine

standard_explosion_y_offset_tbl:
    .byte $0e,$f0,$02,$fc,$08

standard_explosion_x_offset_tbl:
    .byte $f8,$03,$f4,$0e,$fe

; enemy destroyed routine
; temple of terror skull (enemy type #$51) enemy destroyed routine
; helicopter core (enemy type #$21)
; fortress wall core (enemy type #$4b)
; krypto-crustacean (enemy type #$59)
; suspicious face (enemy type #$64)
; jagger froid (enemy type #$65)
; tank boss (enemy type #$69)
; final boss (enemy type #$6b)
enemy_routine_boss_defeated_00:
    inc BOSS_DEFEATED_FLAGS
    lda #$1d                ; create #$1d explosions
    bne boss_defeated_init  ; always branch to disable collision, destroy all enemies

; enemy destroyed routine
robot_spider_routine_04:
    lda #$0e ; create #$0e explosions

; disable collision, destroy all enemies
boss_defeated_init:
    sta ENEMY_VAR_4,x               ; set number of explosions
    lda #$01
    sta ENEMY_SPRITE,x              ; set empty sprite
    lda ENEMY_DESTROY_ATTRS,x
    ora #$81
    sta ENEMY_DESTROY_ATTRS,x       ; disable player-enemy collision and player bullet-enemy collision
    jsr destroy_all_enemies
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to enemy_routine_boss_defeated_01
                                    ; used by both laser chandelier and robot spider
                                    ; (both advance to enemy_routine_boss_defeated_01)

; generate ENEMY_VAR_4 explosions, with an #$08 frame delay between each one
; can generate up to #$1e explosions
; helicopter core (enemy type #$21)
; fortress wall core (enemy type #$4b)
; kryto-crustacean (enemy type #$59)
; suspicious face (enemy type #$64)
; jagger froid (enemy type #$65)
; tank boss (enemy type #$69)
; final boss (enemy type #$6d)
enemy_routine_boss_defeated_01:
    dec ENEMY_DELAY,x
    bne enemy_routine_boss_defeated_exit ; exit if explosion delay not elapsed
    lda #$08
    sta ENEMY_DELAY,x                    ; set next explosion delay
    dec ENEMY_VAR_4,x                    ; decrement number of remaining explosions
    bmi enemy_routine_boss_defeated_adv
    ldy #$0c                             ; enemy type = enemy explosion
    jsr try_create_enemy_from_existing   ; create enemy explosion
    bcc enemy_routine_boss_defeated_exit ; branch if unable to create enemy explosion
    ldy ENEMY_VAR_4,x                    ; load current explosion index
    lda explosion_y_offset_tbl,y         ; load Y offset from enemy position for explosion
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x                    ; add offset to destroyed enemy's Y position
    sta $08                              ; set for when setting explosion position (set_explosion_pos_and_sound)
    ror
    eor explosion_y_offset_tbl,y
    bmi clear_created_enemy_sprite       ; branch if overflow or underflow (wrapped around)
                                         ; to set explosion sprite to #$00 (not visible)
    lda explosion_x_offset_tbl,y         ; load X offset from enemy position for explosion
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_X_POS,x                    ; add offset to destroyed enemy's X position
    sta $09                              ; set for when setting explosion position (set_explosion_pos_and_sound)
    ror
    eor explosion_x_offset_tbl,y         ; set bit 7 for overflow or underflow (wrapped around)
    ldy ENEMY_VAR_7,x                    ; #$00 when on-screen, #$ff when off-screen
    beq @continue                        ; when non-zero, wraparound is expected
                                         ; so branch if should not invert overflow logic
    eor #$80                             ; overflow/underflow expected
                                         ; e.g. helicopter core is destroyed when off screen
                                         ; when destroyed off screen, overflows are good
                                         ; because that means the explosions will be on screen

@continue:
    tay                            ; set negative flag if overflow/underflow
    bmi clear_created_enemy_sprite ; set explosion sprite to #$00 (not visible)

; set the created explosions position and destroyed sound code (sound_24)
; input
;  * $09 - explosion X position
;  * $08 - explosion Y position
;  * $11 - enemy slot index of the explosion
set_explosion_pos_and_sound:
    ldy $11                   ; load created explosion enemy slot index
    lda $09                   ; load created explosion's calculated X position
    sta ENEMY_X_POS,y         ; set explosion's X position
    lda $08                   ; load created explosion's calculated Y position
    sta ENEMY_Y_POS,y         ; set explosion's Y position
    lda #$01
    sta ENEMY_SPRITE,y        ; set empty sprite
    lda ENEMY_DESTROY_ATTRS,y
    ora #$a1
    sta ENEMY_DESTROY_ATTRS,y ; disable collision, set enemy destroyed sound to sound_24

enemy_routine_boss_defeated_exit:
    rts

enemy_routine_boss_defeated_adv:
    jmp advance_enemy_routine ; advance to next routine

explosion_y_offset_tbl:
    .byte $00,$f8,$18,$08,$e8,$00,$f8,$18,$00,$e8,$f0,$e8,$00,$10,$00,$00
    .byte $f8,$18,$08,$e8,$00,$f8,$18,$00,$e8,$f0,$e8,$00,$10,$00

explosion_x_offset_tbl:
    .byte $f8,$10,$00,$e0,$00,$dd,$20,$15,$f0,$00,$e8,$18,$18,$20,$00,$f8
    .byte $10,$00,$e0,$00,$dd,$20,$15,$f0,$00,$e8,$18,$18,$20,$00

; input
;  * x - enemy slot index
;  * PLAYER_INDEX - player index (0 = p1, 1 = p2)
add_to_score_set_destroyed:
    jsr add_enemy_points_to_score ; add points to player score based on which enemy was destroyed
    ldx ENEMY_CURRENT_SLOT

; set enemy routine to their appropriate destroyed routine
set_destroyed_enemy_routine:
    lda ENEMY_TYPE,x                  ; load current enemy type
    lsr                               ; half the value since each byte in enemy_destroyed_routine_tbl contains 2 enemy types
                                      ; and push enemy lsb to the carry flag (odd or even)
    tay                               ; transfer offset to y
    lda enemy_destroyed_routine_tbl,y ; load byte containing enemy destroyed routine nibble
    bcs @set_routine                  ; if enemy type loaded is odd, bits 0-3 is the routine number to set
    lsr                               ; enemy type loaded was even, look at high 4 nibble
    lsr
    lsr
    lsr

@set_routine:
    and #$0f                       ; keep bits .... xxxx
    cmp ENEMY_ROUTINE,x            ; compare against current enemy routine being executed for the enemy
    bcc set_destroyed_routine_exit ; enemy destroyed routine is less than current enemy routine index, exit
    beq set_destroyed_routine_exit ; enemy already on destroyed routine, exit
    sta ENEMY_ROUTINE,x            ; set enemy destroyed routine
    lda #$01
    sta ENEMY_HP,x
    lda ENEMY_DESTROY_ATTRS,x
    ora #$80
    sta ENEMY_DESTROY_ATTRS,x      ; allow bullets to travel through enemy, e.g. weapon item
    lda ENEMY_DESTROY_ATTRS,x
    asl
    bpl set_destroyed_routine_exit
    lda #$01
    sta ENEMY_DELAY,x

set_destroyed_routine_exit:
    rts

; adds the appropriate points to player score based on which enemy was destroyed
; input
;  * x - enemy slot index
;  * PLAYER_INDEX - player index (0 = p1, 1 = p2)
add_enemy_points_to_score:
    ldy ENEMY_TYPE,x                ; load enemy type
    lda enemy_type_points_ptr_tbl,y
    tay
    cpy #$16
    bcs set_destroyed_routine_exit  ; exit if invalid data in table (not possible)
    lda enemy_points_tbl,y
    sta $01                         ; set binary-coded decimal (2 digits) added to the player's score (100s and 10s place)
    lda enemy_points_tbl+1,y
    sta $02                         ; set binary-coded decimal (2 digits) added to the player's score (10,000s and 1,000 place)
    jmp add_score_to_player         ; add score, stored as binary-coded decimal, to player's score
                                    ; if appropriate, handles giving 1-up, and/or setting new high score

; offset into enemy_points_tbl
; each byte is a different enemy type
enemy_type_points_ptr_tbl:
    .byte $00,$0c,$00,$04,$08,$0a,$00,$0a,$00,$08,$08,$0c,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $0c,$14,$06,$00,$04,$08,$06,$08,$0e,$00,$06,$00,$00,$00,$0e,$0a
    .byte $00,$06,$0a,$00,$08,$04,$00,$02,$00,$00,$00,$0c,$00,$06,$06,$00
    .byte $04,$00,$14,$06,$00,$0a,$04,$08,$00,$00,$00,$14,$00,$00,$10,$00
    .byte $06,$14,$08,$06,$04,$04,$04,$00,$00,$14,$06,$0c,$06,$0c,$00,$0a
    .byte $04,$04,$00,$04,$10,$12,$0e,$00,$04,$14,$0c,$02,$00,$14,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; the amount of points to add to player's score
enemy_points_tbl:
    .byte $00,$00 ; 00000 points
    .byte $03,$00 ; 00030 points
    .byte $06,$00 ; 00060 points
    .byte $10,$00 ; 00100 points
    .byte $20,$00 ; 00200 points
    .byte $30,$00 ; 00300 points
    .byte $50,$00 ; 00500 points
    .byte $00,$01 ; 01000 points
    .byte $00,$05 ; 05000 points
    .byte $00,$10 ; 10000 points
    .byte $00,$15 ; 15000 points

enemy_destroyed_routine_tbl:
    .byte $03 ; weapon item (00) / flying capsule (01)
    .byte $44 ; bullet (02) / soldier (03)
    .byte $33 ; weapon box (04) / grenade thrower (05)
    .byte $33 ; grenade (06) / sandbag sniper (07)
    .byte $33 ; unused / sniper (09)
    .byte $66 ; rotating gun (0a) / gray turret (0b)
    .byte $04 ; enemy explosion animation (0c) / door (0d)
    .byte $41 ; overhead level bullet (0e) / grenade generator (0f)
    .byte $40 ; soldier generator (10) / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $56 ; wall cannon (20) / helicopter core (21)
    .byte $37 ; helicopter turret (22) / helicopter bay (23)
    .byte $43 ; alien ladybug (24) / rack-mounted turret (25)
    .byte $55 ; hiding sniper (26) / raised grass-covered turret (27)
    .byte $43 ; ground mortar (28) / mortar round (29)
    .byte $44 ; red bubble (2a) / final boss red poisonous insect gel (2b)
    .byte $43 ; collapsible ceiling (2c) / falling ceiling tile (2d)
    .byte $53 ; overhead tank soldier (2e) / falling rock (2f)
    .byte $04 ; overhead tank soldier bullet (30) / spinning bubbles (31)
    .byte $53 ; raised metal-covered turret (32) / turret metal bullet (33)
    .byte $45 ; jet pack soldier (34) / red poisonous insect gel (35)
    .byte $34 ; tank boss electrode (36) / blue poisonous insect gel (37)
    .byte $30 ; unknown (38) / elevator (39)
    .byte $44 ; robot spider bullet (3a) / manooki (3b)
    .byte $34 ; manooki projectile (3c) / eggron (3d)
    .byte $30 ; bugger (3e) / earthquake (3f)
    .byte $60 ; winged soldier (40) / winged soldier generator (41)
    .byte $66 ; laser chandelier (42) / chandelier arm (43)
    .byte $04 ; chandelier arm laser (44) / wadder (45)
    .byte $53 ; red poisonous insect gel (46) / jameera (47)
    .byte $00 ; jameera projectile (48) / stomping ceiling (49)
    .byte $08 ; unused / fortress wall core (4b)
    .byte $00 ; unused / unused
    .byte $50 ; robot spider (4e) / unused
    .byte $77 ; fortress wall turret (50) / temple of terror (51)
    .byte $53 ; temple of terror core (52) / temple of terror acid drop generator (53)
    .byte $43 ; temple of terror fire ring projectile (54) / temple of terror poisonous insect gel (55)
    .byte $80 ; temple of terror poison drop (56) / unused
    .byte $06 ; storage room soldier generator (58) / krypto-crustacean (59)
    .byte $45 ; alien skull (5a) / rotating turret (5b)
    .byte $34 ; red soldier (5c) / mouth pit (5d)
    .byte $15 ; mouth pit generator (5e) / big faced one-eyed monster (5f)
    .byte $53 ; baby alien ladybug (60) / boss screen baby alien ladybug (61)
    .byte $04 ; intro helicopter (62) / overhead soldier (63)
    .byte $59 ; suspicious face (64) / jagger froid (65)
    .byte $35 ; alien serpent (66) / unused
    .byte $36 ; jagger froid projectile (68) / tank boss (69)
    .byte $34 ; tank gunner (6a) / ceiling vent bubbles (6b)
    .byte $38 ; ceiling vent (6c) / final boss kimkoh (6d)
    .byte $00 ; falling rubble (6e) / background storm (6f)
    .byte $32 ; suspicious face arm (70) / area 6 tile swapper (71)
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused
    .byte $00 ; unused / unused

; falcon weapon - destroy all enemies (with exceptions like for pill box sensor, weapon zeppelin)
; excludes enemies whose HP is #$f0
; also used when boss is defeated to remove all enemies
destroy_all_enemies:
    stx $08  ; store value of x register in $08 temporarily
    ldx #$0d ; looping through all #$0d enemies

@enemy_loop:
    lda ENEMY_ROUTINE,x             ; load the current enemy routine pointer
    beq @continue                   ; skip to next enemy when no routine set for enemy
    lda ENEMY_HP,x                  ; load enemy hp
    cmp #$f0                        ; special HP to not destroy enemy
    beq @continue                   ; skip to next enemy when enemy hp is #$f0
    jsr set_destroyed_enemy_routine ; regular enemy, set it to use its destroy routine

@continue:
    dex             ; go to next enemy (enemy logic starts high and goes to #$00)
    bpl @enemy_loop
    ldx $08         ; restore x attribute from before destroy_all_enemies call
    rts

unused_enemy_routine_ptr_tbl:
    .addr unused_enemy_routine_00
    .addr unused_enemy_routine_00

unused_enemy_routine_00:
    rts

enemy_routine_ptr_tbl:
    .addr weapon_item_routine_ptr_tbl-2                  ; #$00 - weapon item
    .addr flying_capsule_routine_ptr_tbl-2               ; #$01 - flying capsule
    .addr enemy_bullet_routine_ptr_tbl-2                 ; #$02 - enemy bullet
    .addr soldier_routine_ptr_tbl-2                      ; #$03 - soldier/humanoid alien
    .addr weapon_box_routine_ptr_tbl-2                   ; #$04 - weapon box
    .addr grenade_thrower_routine_ptr_tbl-2              ; #$05 - grenade throwing soldier
    .addr grenade_routine_ptr_tbl-2                      ; #$06 - grenade
    .addr sandbag_sniper_routine_ptr_tbl-2               ; #$07 - sandbag sniper
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$08 - unused
    .addr sniper_routine_ptr_tbl-2                       ; #$09 - sniper
    .addr rotating_gun_routine_ptr_tbl-2                 ; #$0a - rotating gun
    .addr gray_turret_routine_ptr_tbl-2                  ; #$0b - gray turret
    .addr enemy_explosion_routine_ptr_tbl-2              ; #$0c - enemy explosion animation
    .addr enemy_door_routine_ptr_tbl-2                   ; #$0d - enemy door
    .addr enemy_overhead_bullet_routine_ptr_tbl-2        ; #$0e - overhead level bullet
    .addr grenade_gen_routine_ptr_tbl-2                  ; #$0f - grenade launcher (not a soldier)
    .addr soldier_gen_routine_ptr_tbl-2                  ; #$10 - soldier generator
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$11 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$12 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$13 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$14 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$15 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$16 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$17 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$18 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$19 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$1a - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$1b - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$1c - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$1d - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$1e - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$1f - unused
    .addr wall_cannon_routine_ptr_tbl-2                  ; #$20 - wall cannon
    .addr helicopter_core_routine_ptr_tbl-2              ; #$21 - level 1 boss core
    .addr helicopter_turret_routine_ptr_tbl-2            ; #$22 - level 1 boss helicopter turret
    .addr helicopter_bay_routine_ptr_tbl-2               ; #$23 - level 1 boss helicopter bay (generates soldiers)
    .addr alien_ladybug_routine_ptr_tbl-2                ; #$24 - alien ladybug
    .addr rack_turret_routine_ptr_tbl-2                  ; #$25 - rack-mounted turret
    .addr crouching_sniper_routine_ptr_tbl-2             ; #$26 - crouching sniper
    .addr grass_covered_turret_routine_ptr_tbl-2         ; #$27 - grass-covered turret
    .addr ground_mortar_routine_ptr_tbl-2                ; #$28 - ground mortar
    .addr mortar_round_routine_ptr_tbl-2                 ; #$29 - mortar round
    .addr red_bubble_routine_ptr_tbl-2                   ; #$2a - red bubble
    .addr final_boss_red_blob_routine_ptr_tbl-2          ; #$2b - final boss red blob (poisonous insect gel)
    .addr collapsible_ceiling_routine_ptr_tbl-2          ; #$2c - area 4 vertical level ceiling tiles that fall after shooting
    .addr falling_ceiling_tile_routine_ptr_tbl-2         ; #$2d - area 4 vertical level single falling ceiling tile
    .addr overhead_tank_soldier_routine_ptr_tbl-2        ; #$2e - overhead non-mobile tank
    .addr falling_rock_routine_ptr_tbl-2                 ; #$2f - falling rock
    .addr overhead_tank_soldier_bullet_routine_ptr_tbl-2 ; #$30 - overhead tank soldier bullet
    .addr spinning_bubbles_routine_ptr_tbl-2             ; #$31 - spinning bubbles
    .addr metal_covered_turret_routine_ptr_tb-2          ; #$32 - metal-covered turret
    .addr metal_bullet_routine_ptr_tbl-2                 ; #$33 - turret metal bullet
    .addr jet_pack_soldier_routine_ptr_tbl-2             ; #$34 - jet pack soldier
    .addr red_blob_routine_ptr_tbl-2                     ; #$35 - poisonous insect gel
    .addr tank_boss_electrode_routine_ptr_tbl-2          ; #$36 - level 2 tank boss electrode
    .addr blue_blob_routine_ptr_tbl-2                    ; #$37 - final boss blue blob (poisonous insect gel)
    .addr enemy_38_routine_ptr_tbl-2                     ; #$38 - unknown/unused
    .addr elevator_routine_ptr_tbl-2                     ; #$39 - elevator
    .addr robot_spider_bullet_routine_ptr_tbl-2          ; #$3a - robot spider bullet
    .addr manooki_routine_ptr_tbl-2                      ; #$3b - manooki
    .addr manooki_projectile_routine_ptr_tbl-2           ; #$3c - manooki projectile
    .addr spider_spawn_routine_ptr_tbl-2                 ; #$3d - eggron
    .addr alien_spider_routine_ptr_tbl-2                 ; #$3e - alien spider
    .addr jungle_earthquake_routine_ptr_tbl-2            ; #$3f - earth shaking
    .addr winged_soldier_routine_ptr_tbl-2               ; #$40 - winged soldier
    .addr winged_soldier_gen_routine_ptr_tbl-2           ; #$41 - winged soldier generator
    .addr laser_chandelier_routine_ptr_tbl-2             ; #$42 - boss laser chandelier
    .addr chandelier_arm_routine_ptr_tbl-2               ; #$43 - chandelier arm
    .addr chandelier_arm_laser_routine_ptr_tbl-2         ; #$44 - chandelier arm laser
    .addr alien_mouth_routine_ptr_tbl-2                  ; #$45 - alien mouth (wadder)
    .addr final_stage_red_blob_routine_ptr_tbl-2         ; #$46 - final stage poisonous insect gel
    .addr alien_cyclops_routine_ptr_tbl-2                ; #$47 - alien cyclops
    .addr cyclops_projectile_routine_ptr_tbl-2           ; #$48 - alien cyclops projectile
    .addr stomping_ceiling_routine_ptr_tbl-2             ; #$49 - final stage moving ceiling
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$4a - unused
    .addr fortress_wall_core_routine_ptr_tbl-2           ; #$4b - jungle boss fortress wall core
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$4c - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$4d - unused
    .addr robot_spider_routine_ptr_tbl-2                 ; #$4e - jungle mini-boss robot spider
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$4f - unused
    .addr fortress_wall_turret_routine_ptr_tbl-2         ; #$50 - jungle boss fortress wall turret
    .addr temple_of_terror_skull_routine_ptr_tbl-2       ; #$51 - temple of terror skull
    .addr temple_of_terror_core_routine_ptr_tbl-2        ; #$52 - temple of terror core (fire ring projectile generator)
    .addr poison_drop_gen_routine_ptr_tbl-2              ; #$53 - temple of terror poison/acid drop generator
    .addr fire_ring_projectile_routine_ptr_tbl-2         ; #$54 - temple of terror core fire ring projectile
    .addr temple_of_terror_red_blob_routine_ptr_tbl-2    ; #$55 - temple of terror skull projectile (poisonous insect gel)
    .addr poison_drop_routine_ptr_tbl-2                  ; #$56 - poison drop
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$57 - unused
    .addr storage_bay_routine_ptr_tbl-2                  ; #$58 - storage soldier generator
    .addr krypto_crustacean_routine_ptr_tbl-2            ; #$59 - krypto-crustacean
    .addr alien_skull_routine_ptr_tbl-2                  ; #$5a - alien skull
    .addr overhead_rotating_turret_routine_ptr_tbl-2     ; #$5b - overhead rotating turret
    .addr stationary_red_soldier_routine_ptr_tbl-2       ; #$5c - overhead red soldier
    .addr mouth_pit_routine_ptr_tbl-2                    ; #$5d - mouth pit
    .addr mouth_pit_gen_routine_ptr_tbl-2                ; #$5e - invisible mouth pit generator
    .addr big_face_routine_ptr_tbl-2                     ; #$5f - big faced one-eyed monster
    .addr baby_alien_ladybug_routine_ptr_tbl-2           ; #$60 - baby alien ladybug
    .addr boss_baby_alien_ladybug_routine_ptr_tbl-2      ; #$61 - boss screen baby alien ladybug
    .addr intro_helicopter_routine_ptr_tbl-2             ; #$62 - screen 1 helicopter animation
    .addr overhead_soldier_routine_ptr_tbl-2             ; #$63 - overhead soldier
    .addr suspicious_face_routine_ptr_tbl-2              ; #$64 - suspicious face
    .addr jagger_froid_routine_ptr_tbl-2                 ; #$65 - jagger froid
    .addr alien_serpent_routine_ptr_tbl-2                ; #$66 - alien serpent
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$67 - unused
    .addr jagger_froid_projectile_routine_ptr_tbl-2      ; #$68 - jagger froid projectile
    .addr tank_boss_routine_ptr_tbl-2                    ; #$69 - level 2 tank boss
    .addr tank_gunner_routine_ptr_tbl-2                  ; #$6a - tank gunner
    .addr ceiling_vent_bubble_routine_ptr_tbl-2          ; #$6b - ceiling vent bubbles
    .addr ceiling_vent_routine_ptr_tbl-2                 ; #$6c - ceiling vent
    .addr final_boss_routine_ptr_tbl-2                   ; #$6d - kimkoh final boss
    .addr falling_rubble_routine_ptr_tbl-2               ; #$6e - final boss falling rubble
    .addr bg_storm_routine_ptr_tbl-2                     ; #$6f - level 1 background storm (bank 2)
    .addr suspicious_face_arm_routine_ptr_tbl-2          ; #$70 - suspicious face arm
    .addr area_6_chr_swap_routine_ptr_tbl-2              ; #$71 - invisible
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$72 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$73 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$74 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$75 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$76 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$77 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$78 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$79 - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$7a - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$7b - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$7c - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$7d - unused
    .addr unused_enemy_routine_ptr_tbl-2                 ; #$7e - unused
    .addr unused_enemy_routine_ptr_tbl                   ; doesn't subtract 2

; soldier_routine_00 - soldier animation delay hasn't elapsed
apply_y_scroll_to_y_pos:
    sec
    bcs apply_y_scroll ; always branch to apply vertical scroll to enemy Y position

; updates enemy position by applying scroll to keep enemy in same position while scrolling
; removes enemy if off-screen to left or below
; input
;  * x - enemy slot index
update_enemy_pos:
    lda ENEMY_X_POS,x    ; load enemy's X position
    sec                  ; set carry flag in preparation for subtraction
    sbc X_SCROLL_SPEED   ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    sta ENEMY_X_POS,x    ; set new X position incorporating scroll
    cmp #$06             ; compare enemy X position to extreme left edge of screen
    bcc remove_enemy_far ; branch if enemy is too far left on screen to remove them

; apply vertical scroll to enemy Y position
apply_y_scroll:
    lda ENEMY_Y_POS,x       ; load enemy's Y position
    sbc Y_SCROLL_SPEED
    sta ENEMY_Y_POS,x
    cmp #$f0
    bcc apply_velocity_exit

remove_enemy_far:
    jmp remove_enemy ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0

; applies the enemy's velocity to its position, removing enemy if off-screen
; input
;  * x - enemy slot index
apply_velocity:
    lda ENEMY_Y_VEL_ACCUM,x
    clc                          ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x
    sta ENEMY_Y_VEL_ACCUM,x
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    adc ENEMY_Y_VELOCITY_FAST,x
    sec                          ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED
    sta ENEMY_Y_POS,x
    cmp #$f0
    bcs remove_enemy_far

; applies the enemy's X velocity to its X position, removing enemy if off-screen
; input
;  * x - enemy slot index
apply_x_velocity:
    lda ENEMY_X_VEL_ACCUM,x
    adc ENEMY_X_VELOCITY_FRACT,x
    sta ENEMY_X_VEL_ACCUM,x
    lda ENEMY_X_POS,x            ; load enemy's X position
    adc ENEMY_X_VELOCITY_FAST,x
    sec                          ; set carry flag in preparation for subtraction
    sbc X_SCROLL_SPEED           ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    sta ENEMY_X_POS,x
    cmp #$06
    bcc remove_enemy_far

apply_velocity_exit:
    rts

; apply velocities, remove if off screen with no chance of coming back screen
; output
;  * ENEMY_VAR_6,x - whether or not enemy is vertically offscreen
;    (#$00 = on screen, #$ff = off screen above, #$01 = off screen below)
update_pos_check_offscreen:
    clc
    jsr apply_x_velocity
    ldy #$00                    ; enemy direction (#$00 = down, #$ff = up or no vertical direction)
    lda ENEMY_Y_VELOCITY_FAST,x ; load enemy's fast Y velocity
    bpl @continue               ; branch if going down
    dey                         ; going up, look for ground

@continue:
    sec                          ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED           ; subtract any vertical scroll for this frame from the enemy fast Y velocity
    sta $01                      ; store resulting fast Y velocity in $01
    tya                          ; transfer direction (#$00 = down, #$ff = up) to a
    sbc SCROLLING_UP_FLAG        ; (#$ff = scrolling up, #$00 = not scrolling (or scrolling down))
    tay                          ; backup direction - SCROLLING_UP_FLAG
    lda ENEMY_Y_VEL_ACCUM,x      ; load Y velocity fractional accumulator
    clc                          ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x ; add fractional Y velocity to fractional accumulator
    sta ENEMY_Y_VEL_ACCUM,x      ; update Y fractional accumulator
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    adc $01                      ; add Y fast velocity (adjusted for vertical scroll this frame)
                                 ; and any fractional accumulator overflow
    sta ENEMY_Y_POS,x            ; set new Y position
    tya                          ; transfer overall direction (including vertical scroll) to a
                                 ; tracks when Y position changes due to overflow adding accumulator
    adc ENEMY_VAR_6,x            ; will be set to #$ff when off screen above and moving up
    sta ENEMY_VAR_6,x            ; set whether off screen vertically
                                 ; if moving up and addition didn't result in carry, then must be off screen (above)
                                 ; if moving down and addition caused an overflow, then must be off screen (below)
                                 ; used to continue to allow a projectile to go offscreen and
                                 ; wait for gravity to bring it back on screen (e.g. enemy type #$29 mortar round)
    beq @check_off_screen_below  ; branch if still on screen
    cmp #$ff                     ; see if off screen above
    bne remove_enemy_far         ; branch if off screen below to remove
    lda ENEMY_Y_POS,x            ; off screen above, load enemy's Y position
    cmp #$80
    bcc remove_enemy_far         ; branch if enemy really far off screen above
    jmp clear_enemy_sprite       ; enemy may come back on screen, set sprite to #$00

@check_off_screen_below:
    lda ENEMY_Y_POS,x       ; load enemy's Y position
    cmp #$f0
    bcc apply_velocity_exit ; branch if in top 93.75% of screen
    jmp remove_enemy        ; at very bottom of screen
                            ; set the enemy type to #$7f (slot available), and sets the enemy sprite to 0

; used by helicopter core (enemy type #$21) and final boss (enemy type #$6d)
; shakes enemy as it rises/falls based on GLOBAL_TIMER
; cycle adjusting Y fast velocity by 1, 0, -1, 0 repeatedly
shake_enemy_pattern:
    lda GLOBAL_TIMER      ; load Y fast velocity adjustment cycle index
                          ; 00 -> #$01, 01 -> #$00, 10 -> #$ff, 11 => #$00
    lsr
    bcs bg_boss_apply_vel ; branch if timer ends in 1 (don't adjust Y vel)
    lsr
    lda #$01
    bcc push_enemy        ; branch if timer ends in 00 (adjust Y vel by 1)
    lda #$ff
    bcs push_enemy        ; always branch. timer ends in 10 (adjust Y vel by -1)

; apply enemy velocity for background enemy
; incorporating ENEMY_VAR_6,x (Y) and ENEMY_VAR_7,x (X)
; input
;  * x - enemy slot index
bg_boss_apply_vel:
    lda #$00

; push enemy either up or down based on a (0 = down, 1 = up)
; input
;  * a - amount to adjust Y velocity by (-1, 0, or 1)
;  * x - enemy slot offset
; output
;  * ENEMY_VAR_7,x - set to #$00 when move to new nametable horizontally
;  * ENEMY_VAR_6,x - set to #$00 when move to new nametable vertically
push_enemy:
    clc                         ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FAST,x
    sta $01                     ; set adjusted Y fast velocity
    asl                         ; push Y direction to carry
    lda #$0f                    ; assume going down, if too low will adjust Y position by 16
    bcc @continue_y_vel         ; branch if going down
    lda #$ef                    ; going up, if too low, adjust Y position by -16

@continue_y_vel:
    sta $00                      ; set further Y position adjustment
    lda ENEMY_Y_VEL_ACCUM,x
    clc                          ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x
    sta ENEMY_Y_VEL_ACCUM,x      ; add fractional amount to velocity accumulator
    lda ENEMY_Y_POS,x            ; load enemy's Y position
    adc $01                      ; add Y fast velocity adjustment (fast Y vel modified by -1, 0, or 1)
    ldy #$00
    cmp #$f0                     ; compare calculated enemy Y position to #$f0 (bottom of screen)
    bcc @continue_y_pos          ; branch if not at bottom of screen
    adc $00                      ; at bottom of screen, move back by 16 or -16
    asl $00                      ; push Y direction to carry (0 = down, 1 = up)
    ldy #$01
    bcc @continue_y_pos          ; branch if moving down
    ldy #$ff

@continue_y_pos:
    sta ENEMY_Y_POS,x      ; set new Y position based on Y velocity
    tya                    ; transfer Y fractional adjustment direction to a (1 = down, 0 = up)
    clc                    ; clear carry in preparation for addition
    adc ENEMY_VAR_6,x
    sta ENEMY_VAR_6,x
    beq @continue_x
    jsr clear_enemy_sprite

@continue_x:
    ldy #$00                    ; assume positive X velocity
    lda ENEMY_X_VELOCITY_FAST,x ; load enemy's fast X velocity
    bpl @apply_x_vel
    dey                         ; negative X velocity, use #$ff

@apply_x_vel:
    sty $00                      ; set X ENEMY_VAR_7 adjustment (0 = moving right, -1 = moving left)
    lda ENEMY_X_VELOCITY_FRACT,x ; load enemy's fractional X velocity
    clc                          ; clear carry in preparation for addition
    adc ENEMY_X_VEL_ACCUM,x
    sta ENEMY_X_VEL_ACCUM,x      ; set fractional velocity accumulator value
    lda ENEMY_X_VELOCITY_FAST,x  ; load enemy's fast X velocity
    adc ENEMY_X_POS,x            ; add fast X velocity plus any overflow from accumulator to X position
    sta ENEMY_X_POS,x            ; set new X position
    lda $00                      ; load direction (0 = moving right, -1 = moving left)
    adc ENEMY_VAR_7,x
    sta ENEMY_VAR_7,x            ; add direction (0 or -1) and any carry when adding X fast velocity to position
                                 ; effectively sets to #$00 when core when X position overflowed from #$ff to #$00
                                 ; moving from non-visible nametable to visible nametable
    beq shake_enemy_pattern_exit ; exit when core on screen (can still be hidden behind turrets)
    jmp clear_enemy_sprite       ; core not on screen, hide core sprite

; sets enemy X and Y velocity to 0
clear_enemy_vel:
    jsr clear_enemy_x_vel ; set enemy X velocity (fast and fractional) to 0

; sets enemy Y velocity to 0
clear_enemy_y_vel:
    lda #$00
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity

shake_enemy_pattern_exit:
    rts

; sets enemy X velocity (fast and fractional) to 0
; input
;  * x - enemy slot index
clear_enemy_x_vel:
    lda #$00
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    rts

; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25
; input
;  * x - enemy slot index
flip_enemy_x_dir:
    lda #$00
    sec                          ; set carry flag in preparation for subtraction
    sbc ENEMY_X_VELOCITY_FRACT,x
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda #$00
    sbc ENEMY_X_VELOCITY_FAST,x
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    rts

; flip the sign of the enemy's Y velocity, e.g. 1.25 becomes -1.25
; input
;  * x - enemy slot index
flip_enemy_y_dir:
    lda #$00
    sec                          ; set carry flag in preparation for subtraction
    sbc ENEMY_Y_VELOCITY_FRACT,x
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda #$00
    sbc ENEMY_Y_VELOCITY_FAST,x
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    rts

; determine enemy bg collision if ENEMY_VAR_6,x is zero (visible on screen)
; input
;  * a - amount to add to ENEMY_Y_POS
;  * x - enemy slot index
;  * ENEMY_VAR_6,x - 0 = enemy on-screen, #$ff enemy off-screen
;    when non-zero, no bg collision check is performed
; output
;  * zero flag - set when no bg collision
;  * a - bg collision code (see PLAYER_SURFACE)
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;  * $04 - bg collision offset, i.e. BG_COLLISION_DATA offset (same as y)
get_enemy_bg_collision_code_onscreen:
    ldy ENEMY_VAR_6,x           ; 0 = on-screen, #$ff = offscreen
    bne bg_collision_exit_clear ; exit with zero flag clear if off-screen
                                ; example enemy type #$29 mortar round when off-screen above

; determine enemy bg collision
; input
;  * a - amount to add to ENEMY_Y_POS
;  * x - enemy slot index
; output
;  * zero flag - set when no bg collision
;  * a - bg collision code (see PLAYER_SURFACE)
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;  * $04 - bg collision offset, i.e. BG_COLLISION_DATA offset (same as y)
get_falling_enemy_bg_collision_code:
    ldy ENEMY_Y_VELOCITY_FAST,x ; see if enemy is falling
    bmi bg_collision_exit_clear ; exit with zero flag clear if enemy going up

; input
;  * a - amount to add to ENEMY_Y_POS
; output
;  * zero flag - set when no bg collision
;  * a - bg collision type, i.e. surface kind (see PLAYER_SURFACE)
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;  * $04 - bg collision offset, i.e. BG_COLLISION_DATA offset (same as y)
get_enemy_bg_collision_code:
    sta $07                     ; enemy is falling, set Y offset amount
    clc                         ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x           ; add offset to enemy's Y position
    cmp #$10                    ; see if enemy is on top ~6% of screen
    bcc bg_collision_exit_clear ; branch if enemy is on top ~6% of screen
    cmp #$f0                    ; enemy not on very top of screen, see if on very bottom
    bcs bg_collision_exit_clear ; branch if enemy is on bottom ~6% of screen
    tay                         ; enemy not on very bottom, nor very top, transfer test Y position to y
    lda ENEMY_X_POS,x           ; load enemy's X position
    jmp get_bg_collision_code   ; determine background collision at point (a,y)

bg_collision_exit_clear:
    lda #$00
    rts

; input
;  * a - x offset from enemy for bg collision detection
;  * x - enemy slot index
; output
;  * a - 0 = no wall collision, 1 = wall collision
check_bg_wall_collision:
    sta $00                         ; store x offset from enemy X position
    clc
    adc ENEMY_X_POS,x               ; add X offset
    sta $01                         ; store X position to test for bg collision
    ror
    eor $00
    bmi @exit_no_collision          ; exit with no collision if off-screen
    lda $01                         ; load X position to test for bg collision
    ldy ENEMY_Y_POS,x               ; load enemy's Y position to test for bg collision
    jsr get_bg_collision            ; get background collision code for position (a,y)
    jmp is_wall_collision_use_y_reg ; determine if collision code can be considered a wall (horizontally)

@exit_no_collision:
    lda #$00
    rts

; set enemy Y position based on background collision
; input
;  * a - background collision code
;  * x - enemy slot index
;  * $01
;  * $06
;  * $09
set_y_pos_for_bg:
    sta $09               ; store bg collision code
    jsr get_landing_y_pos ; determine Y position from bg collision
    sta ENEMY_Y_POS,x     ; set Y position based on bg collision
    rts

; draws the 4 supertiles for the destroyed overhead tank
; input
;  * a - starting index of the 4 supertiles to draw (always 0)
; output
;  * carry - set when graphics buffer isn't empty, no supertiles were drawn, clear for success
overhead_tank_soldier_draw_destroyed:
    asl
    asl
    sta $0d                    ; set supertile_replace_index_tbl base offset
    lda ENEMY_Y_POS,x          ; load enemy's Y position
    sta $12
    lda ENEMY_X_POS,x          ; load enemy's X position
    sta $13
    lda GRAPHICS_BUFFER_OFFSET
    cmp #$01
    bcs @exit                  ; exit if graphics buffer isn't empty
    ldy #$03                   ; drawing 4 supertiles

@draw_supertile:
    sty $0f                                     ; set supertile draw count
    tya
    clc                                         ; clear carry in preparation for addition
    adc $0d
    tay
    lda supertile_replace_index_tbl,y           ; load index of supertile to draw
    sta $08                                     ; supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
    lda $12
    adc supertile_replace_y_offset_tbl,y
    sta $0c                                     ; set supertile Y position
    lda $13
    adc supertile_replace_x_offset_tbl,y        ; set supertile x position
    ldy $0c
    jsr load_banks_update_supertile_and_palette ; update nametable supertile (4x4 tile) and its palette at (a, y)
    txa                                         ; transfer graphics buffer write offset to a
    sec                                         ; set carry flag in preparation for subtraction
    sbc #$04                                    ; move back 4 bytes
                                                ; rewriting buffer to remove attribute table palette byte
    sta GRAPHICS_BUFFER_OFFSET
    tax
    lda #$ff
    sta CPU_GRAPHICS_BUFFER-1,x                 ; write graphics block end byte
                                                ; this marks the end before the palette byte
    ldy $0f                                     ; load supertile draw count
    dey                                         ; decrement remaining number of supertiles to draw
    bpl @draw_supertile                         ; branch if at least one more supertile to draw
    clc

@exit:
    ldx ENEMY_CURRENT_SLOT
    rts

; offset into level_2_supertile_data
supertile_replace_index_tbl:
    .byte $65,$66,$67,$68

supertile_replace_y_offset_tbl:
    .byte $f0,$f0,$10,$10

supertile_replace_x_offset_tbl:
    .byte $f0,$10,$f0,$10

; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
; redraws parts of the nametable for things like enemy animations
; input
;  * a - supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
;  * x - enemy slot index
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
update_enemy_nametable_supertile:
    sta $08                                             ; store supertile to draw
    lda ENEMY_X_POS,x                                   ; load enemy's X position
    ldy ENEMY_Y_POS,x                                   ; load the enemy's Y position
    jsr load_banks_update_supertile_and_palette_if_room ; update nametable supertile (4x4 tile) and its palette at (a, y)
    ldx ENEMY_CURRENT_SLOT                              ; restore x to the enemy offset
    rts

; updates 2 supertiles for enemy, the first at enemy position, the second at offset specified in y
; input
;  * a - first enemy supertile offset, e.g. index into level_5_supertile_data offset
;  * y - second supertile location adjustment amount byte (x and y)
;  * $0c - second supertile offset, e.g. index into level_5_supertile_data offset
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
load_banks_update_enemy_supertiles:
    sta $08           ; set first supertile offset, e.g. index into level_5_supertile_data offset
    sty $0d           ; set X and Y offset for second supertile
    lda ENEMY_X_POS,x ; load enemy's X position
    ldy ENEMY_Y_POS,x ; load enemy's Y position

; updates 2 supertiles
; the first ($08) at (a,y), the second ($0c) at offset encoded in $0d
; input
;  * a - X position
;  * y - Y position
;  * $08 - first supertile offset, e.g. index into level_5_supertile_data offset
;  * $0c - second supertile offset, e.g. index into level_5_supertile_data offset
;  * $0d - second supertile location adjustment amount byte
load_banks_update_supertiles:
    jsr load_banks_update_supertile_and_palette_if_room ; update nametable supertile (4x4 tile) and its palette at (a, y)
    bcs @exit                                           ; exit if unable to update nametable due to not enough room
    ldx ENEMY_CURRENT_SLOT                              ; restore enemy slot
    lda $0c                                             ; load second supertile offset, e.g. index into level_5_supertile_data offset
    sta $08                                             ; store in parameter for update supertile call
    lda $0d                                             ; load second supertile location adjustment amount byte
    asl
    asl
    asl
    asl                                                 ; shift low nibble to high (x offset)
    ora #$08                                            ; set bit 3
    sta $0e                                             ; set second supertile x offset
    lda $0d                                             ; load second supertile location adjustment amount byte
    and #$f0                                            ; keep high nibble (y offset)
    ora #$08                                            ; set bit 3
    clc                                                 ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x                                   ; add y adjustment to enemy's Y position
    tay                                                 ; transfer to y for parameter to update supertile call
    lda $0e                                             ; load second supertile x offset
    clc                                                 ; clear carry in preparation for addition
    adc ENEMY_X_POS,x                                   ; add x adjustment to enemy's X position
    jsr load_banks_update_supertile_and_palette         ; update second nametable supertile (4x4 tile) and its palette at (a, y)

@exit:
    ldx ENEMY_CURRENT_SLOT ; restore enemy slot
    rts

; !(UNUSED)
; update 2x2 nametable tiles at enemy position with tiles specified by a
; input
;  * a - index into nametable_square_update_tbl
;  * x - enemy slot index
; output
;  * carry - set when unable to update graphics buffer
update_nametable_square_for_enemy:
    sta $08
    lda ENEMY_X_POS,x ; load enemy's X position
    ldy ENEMY_Y_POS,x ; load enemy's Y position

; update 2x2 nametable tiles at position (a,y) with tiles specified by $08
; used by collapsible ceiling tile to set nametable to 2x2 black square
; input
;  * a - x offset
;  * y - y offset
;  * $08 - index into nametable_square_update_tbl
; output
;  * carry - set when unable to update graphics buffer
update_nametable_square_at_pos:
    jsr update_nametable_for_pos
    ldx ENEMY_CURRENT_SLOT       ; restore enemy slot
    rts

; write a row of tile 0 (black) to the nametable, moves PPU address to next row down
; input
;  * x - enemy slot index
;  * ENEMY_VAR_1 - PPU address high byte
;  * ENEMY_VAR_2 - PPU address low byte
clear_nametable_row:
    lda #$00

; write a row of tile a to the nametable, moves PPU address to next row down
; input
;  * a - the pattern table tile to write
;  * x - enemy slot index
;  * ENEMY_VAR_1 - PPU address high byte
;  * ENEMY_VAR_2 - PPU address low byte
set_nametable_row:
    sta $00
    ldy GRAPHICS_BUFFER_OFFSET
    lda #$03                   ; repeat mode
    sta CPU_GRAPHICS_BUFFER,y  ; write mode of operation to graphics buffer
    iny                        ; increment graphics buffer write offset
    lda ENEMY_VAR_1,x          ; load PPU address write high byte
    sta CPU_GRAPHICS_BUFFER,y  ; set PPU address write high byte
    iny                        ; increment graphics buffer write offset
    lda ENEMY_VAR_2,x          ; load PPU address write low byte
    sta CPU_GRAPHICS_BUFFER,y  ; write PPU address write low byte
    iny                        ; increment graphics buffer write offset
    lda #$20                   ; writing #$20 entries to graphics buffer
    sta CPU_GRAPHICS_BUFFER,y  ; store number of repetitions to write
    iny                        ; increment graphics buffer write offset
    lda $00                    ; load graphic byte
    sta CPU_GRAPHICS_BUFFER,y  ; set graphic byte (will be written #$20 times)
    iny                        ; increment graphics buffer write offset
    sty GRAPHICS_BUFFER_OFFSET ; update graphics buffer offset
    lda ENEMY_VAR_2,x          ; load current nametable row (PPU address low byte)
    clc                        ; clear carry in preparation for addition
    adc #$20                   ; move to next row in nametable
    sta ENEMY_VAR_2,x          ; update nametable row
    bcc @exit
    inc ENEMY_VAR_1,x          ; overflow, increment nametable address high byte

@exit:
    rts

; determine next aim direction to get closer to player index ($0a)
; and set enemy velocity to that target direction
; input
;  * $0a - player index of player to target (0 = p1, 1 = p2)
;  * x - enemy slot offset
;  * y - speed code
hone_to_player_set_enemy_vel:
    sty $06                   ; store speed code in $06
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr rotate_aim_dir_01     ; determine next aim direction [#$00-#$0b] ($0c)
                              ; adjusts ENEMY_VAR_1 to get closer to that value using quadrant_aim_dir_01

; use enemy aim direction (ENEMY_VAR_1,x) to set enemy's velocity
; input
;  * x - enemy slot offset
;  * $06 - speed adjust code [#$00-#$07], e.g. speed adjust code #$01 is .75 speed
set_enemy_vel_from_aim_dir:
    lda ENEMY_VAR_1,x ; load current aiming direction
    sta $0c           ; store aim direction in $0c
    ldy #$00          ; assume quadrant #$00 (quadrant IV)
    cmp #$0d          ; compare aiming direction to #$0d (9:30 o'clock)
    bcc @continue     ; branch if before 9:30 o'clock
    ldy #$01          ; after 9:30 o'clock, adjust quadrant to #$01 (quadrant I)
    lda #$18
    sbc $0c           ; calculate offset into quadrant (quadrant aim dir)
    sta $0c           ; set quadrant aim dir

@continue:
    cmp #$07          ; compare aim direction (or quadrant aim dir) to #$07
    bcc @set_velocity ; branch if aim direction (or quadrant aim dir) less than #$07
    tya               ; transfer quadrant to a
    ora #$02          ; set bit 1, which sets either quadrant #$02 (quadrant III), or quadrant #$03 (quadrant II)
    tay               ; transfer quadrant to a
    lda #$0c
    sbc $0c
    sta $0c           ; set quadrant aim direction

@set_velocity:
    sty $07                     ; set quadrant to aim into
    jsr calc_velocities_for_dir ; determine velocity based on quadrant aim dir (a), quadrant ($07), and speed adjust code ($06)
    jsr set_enemy_velocity      ; sets enemy velocity based on $05, $04, $0b, and $0a
    lda #$01
    rts

; set enemy velocity to target player index $0a with speed code y
; input
;  * x - enemy slot offset
;  * y - speed code
;  * $0a - the closest player index, #$00 for p1, #$01 for p2
set_vel_to_target_player:
    sty $06                     ; store speed code in $06 (see (see bullet_velocity_adjust_ptr_tbl))
    jsr copy_enemy_vars_to_zp   ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_01           ; set enemy aim direction ($0c) and rotation direction (a) targeting player $0a using quadrant_aim_dir_01
                                ; if no player to target, use $(0b, $0c)
    lda $0c                     ; load new aim direction
    sta ENEMY_VAR_1,x           ; store new aim direction
    lda $17                     ; load the quadrant aim direction, i.e. the index within the quadrant to aim
    jsr calc_velocities_for_dir ; determine velocity based on quadrant aim dir (a), quadrant ($07), and speed adjust code ($06)
    jmp set_enemy_velocity      ; sets enemy velocity based on $05, $04, $0b, and $0a

; !(HUH) unused
; modifies the y velocity based on ENEMY_Y_POS,x distance from ENEMY_VAR_1,x
modify_enemy_y_vel_by_dist:
    ldy #$00

; adjust Y velocity based on ENEMY_Y_POS,x distance from ENEMY_VAR_1,x
; used by robot spider bullet (enemy type #$3a), flying capsule (enemy type
; #$01), and krypto-crustacean (enemy type #$59)
; input
;  * y - control byte - number of bits to push onto velocity
;    * negative pushes to 1s to high bits
;    * positive pushes 0 to low bits
;    * zero to use distance a as fractional adjust amount
modify_enemy_y_vel:
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    sec                             ; set carry flag in preparation for subtraction
    sbc ENEMY_VAR_1,x
    jsr calc_vel_by_dist_from_point
    ldx ENEMY_CURRENT_SLOT
    lda ENEMY_Y_VELOCITY_FRACT,x    ; load enemy's fractional Y velocity
    sec                             ; set carry flag in preparation for subtraction
    sbc $01
    sta ENEMY_Y_VELOCITY_FRACT,x    ; store enemy's fractional Y velocity
    lda ENEMY_Y_VELOCITY_FAST,x     ; load enemy's fast Y velocity
    sbc $00
    sta ENEMY_Y_VELOCITY_FAST,x     ; store enemy's fast Y velocity
    rts

; !(UNUSED)
bank_3_unused_00:
    ldy #$00

; create swooping pattern. used by flying capsule (enemy type #$01),
; kryto-crustacean (enemy type #$59), and jet pack soldier (enemy type #$34)
; input
;  * x - enemy slot offset
;  * y - control byte - number of bits to push onto velocity
;    * negative pushes to 1s to high bits
;    * positive pushes 0 to low bits
;    * zero to use distance a as fractional adjust amount
modify_enemy_x_vel:
    lda ENEMY_X_POS,x               ; load enemy's X position
    sec                             ; set carry flag in preparation for subtraction
    sbc ENEMY_VAR_2,x
    jsr calc_vel_by_dist_from_point
    ldx ENEMY_CURRENT_SLOT
    lda ENEMY_X_VELOCITY_FRACT,x    ; load enemy's fractional X velocity
    sec                             ; set carry flag in preparation for subtraction
    sbc $01
    sta ENEMY_X_VELOCITY_FRACT,x    ; store enemy's fractional X velocity
    lda ENEMY_X_VELOCITY_FAST,x     ; load enemy's fast X velocity
    sbc $00
    sta ENEMY_X_VELOCITY_FAST,x     ; store enemy's fast X velocity
    rts

; !(UNUSED)
bank_3_unused_01:
    ldy #$00

; calculates fast and fractional velocity adjustment based on distance from
; point (a). Called for both x distances and y distances
; input
;  * a - distance from position point
;  * y - control byte - number of bits to push onto velocity
;    * negative pushes to 1s to high bits
;    * positive pushes 0 to low bits
;    * zero to use distance a as fractional adjust amount
;  * carry flag - set when enemy to left or above control point (no overflow when subtracted)
; output
;  * $00 - amount to subtract from fast velocity
;  * $01 - amount to subtract from fractional velocity
calc_vel_by_dist_from_point:
    sta $01       ; store distance from point in $01
                  ; point is stored in either ENEMY_VAR_2 or ENEMY_VAR_1
    ldx #$00      ; assume positive velocity, use #$00 for fast velocity
    bcs @continue ; branch if enemy is to right or below the position point
    dex           ; enemy is to the left or above position, use -1 for fast velocity

@continue:
    tya          ; transfer control byte to a
    bmi @t_loop2 ; branch if negative to push 1s to high bits
    beq @exit    ; exit if control byte is #$00 to use distance (a) as fractional velocity adjust
    txa          ; control byte positive, transfer fast velocity adjust to a

@loop:
    asl $01
    rol
    dey
    bne @loop ; shift distance left
    sta $00   ; set amount to be subtracted from fast velocity
    rts

@t_loop2:
    txa ; transfer fast velocity adjust to a

; push direction bit to the fractional velocity
@loop2:
    lsr
    ror $01
    iny
    bne @loop2

@exit:
    stx $00 ; store fast velocity adjust (either #$00 or #$ff (-1))
    rts

; calculate PRE_IRQ_Y_SCROLL_MAX, PRE_IRQ_X_SCROLL_MAX, and BG_BOSS_NAMETABLE
; initialize values for emulating sprite moving (see bg_enemy_set_scrolls)
init_bg_boss_pre_irq_max_scrolls:
    lda ENEMY_Y_POS,x     ; load enemy's Y position
    clc                   ; clear carry in preparation for addition
    adc Y_SCROLL          ; add vertical scroll
    bcs @handle_overflow  ; branch if overflow
    cmp #$f0
    bcc @set_irq_y_scroll ; branch if no overflow

@handle_overflow:
    adc #$0f
    sec

@set_irq_y_scroll:
    sta PRE_IRQ_Y_SCROLL_MAX ; for use in bg_enemy_set_scrolls
    php                      ; push status flags on to the stack
    lda ENEMY_VAR_6,x
    and #$01
    asl
    plp                      ; restore status flags from stack
    bcc @set_irq_x_scroll    ; branch if no overflow
    eor #$02

@set_irq_x_scroll:
    sta $00
    lda ENEMY_X_POS,x          ; load enemy's X position
    clc                        ; clear carry in preparation for addition
    adc X_SCROLL               ; add PPU horizontal scroll
    sta PRE_IRQ_X_SCROLL_MAX
    lda ENEMY_VAR_7,x
    and #$01
    bcc @set_bg_boss_nametable
    eor #$01

@set_bg_boss_nametable:
    ora $00
    eor PPUCTRL_SETTINGS
    and #$03
    sta BG_BOSS_NAMETABLE
    rts

; simulate moving ENEMY_X_POS and ENEMY_Y_POS by scrolling in the opposite
; direction.  these enemies are composed of bg tiles and stationary
; krypto-crustacean, helicopter core, fortress wall core, stomping ceiling
; sets vertical and horizontal scroll for values before the first IRQ
; i.e. the scrolls for showing the boss
bg_enemy_set_scrolls:
    lda PRE_IRQ_Y_SCROLL_MAX ; load highest value of PPU scroll
                             ; this is the lowest vertically scroll can occur
    sec                      ; set carry flag in preparation for subtraction
    sbc ENEMY_Y_POS,x        ; subtract enemy Y position
                             ; as enemy Y position increases, scroll moves up
                             ; to create moving down effect
    bcc @handle_underflow
    cmp #$f0
    bcc @continue

@handle_underflow:
    sec      ; set carry flag in preparation for subtraction
    sbc #$10
    sec

@continue:
    sta Y_SCROLL      ; set new pre-IRQ PPU vertical scroll
    php               ; push status flags on to the stack
    lda ENEMY_VAR_6,x
    asl
    and #$02
    plp               ; restore status flags from stack
    bcc @x_scroll
    eor #$02

@x_scroll:
    sta $00                  ; set base nametable
    lda PRE_IRQ_X_SCROLL_MAX ; load largest scroll horizontally
    sec                      ; set carry flag in preparation for subtraction
    sbc ENEMY_X_POS,x        ; subtract enemy X position
                             ; as enemy X position increases, scroll moves left
                             ; to create moving effect
    sta X_SCROLL             ; set new PPU horizontal scroll value
    lda ENEMY_VAR_7,x
    and #$01
    bcs @set_ppu_ctrl
    eor #$01

@set_ppu_ctrl:
    ora $00               ; merge with base nametable
    eor BG_BOSS_NAMETABLE
    and #$03
    sta $00
    lda PPUCTRL_SETTINGS  ; set non-boss bg nametable
    and #$fc
    ora $00
    sta PPUCTRL_SETTINGS
    rts

set_bg_boss_scroll_nt:
    lda PRE_IRQ_Y_SCROLL_MAX
    sec                      ; set carry flag in preparation for subtraction
    sbc ENEMY_Y_POS,x
    bcc @handle_y_overflow
    cmp #$f0
    bcc @set_irq_y_scroll

@handle_y_overflow:
    sec      ; set carry flag in preparation for subtraction
    sbc #$10
    sec

@set_irq_y_scroll:
    sta IRQ_Y_SCROLL
    php                   ; push status flags on to the stack
    lda ENEMY_VAR_6,x
    asl
    and #$02
    plp                   ; restore status flags from stack
    bcc @set_irq_x_scroll
    eor #$02

@set_irq_x_scroll:
    sta $00
    lda PRE_IRQ_X_SCROLL_MAX
    sec                      ; set carry flag in preparation for subtraction
    sbc ENEMY_X_POS,x
    sta IRQ_X_SCROLL
    lda ENEMY_VAR_7,x
    and #$01
    bcs @set_bg_boss_ppuctrl
    eor #$01

@set_bg_boss_ppuctrl:
    ora $00
    eor BG_BOSS_NAMETABLE
    and #$03
    sta $00
    lda IRQ_PPUCTRL_SETTINGS
    and #$fc
    ora $00
    sta IRQ_PPUCTRL_SETTINGS
    rts

; backup scroll and PPU control values
backup_scroll_and_ppuctrl:
    lda X_SCROLL                ; load PPU horizontal scroll
    sta BACKUP_X_SCROLL
    lda Y_SCROLL                ; load PPU vertical scroll
    sta BACKUP_Y_SCROLL
    lda PPUCTRL_SETTINGS
    sta BACKUP_PPUCTRL_SETTINGS
    rts

; restore scroll and PPU control values
restore_scroll_and_ppuctrl:
    lda BACKUP_X_SCROLL
    sta X_SCROLL                ; restore PPU horizontal scroll
    lda BACKUP_Y_SCROLL
    sta Y_SCROLL                ; restore PPU vertical scroll
    lda BACKUP_PPUCTRL_SETTINGS
    sta PPUCTRL_SETTINGS
    rts

; copies the current enemy's X and Y position to $09 and $08 respectively
; input
;  * x - enemy slot index
; output
;  * $08 - enemy Y position
;  * $09 - enemy X position
copy_enemy_vars_to_zp:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    sta $08
    lda ENEMY_X_POS,x ; load enemy's X position
    sta $09
    rts

; add a to enemy Y position and y to enemy X position
; input
;  * a - amount to add to ENEMY_Y_POS
;  * y - amount to add to ENEMY_X_POS
add_to_enemy_pos:
    clc               ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x
    sta ENEMY_Y_POS,x
    tya
    clc               ; clear carry in preparation for addition
    adc ENEMY_X_POS,x
    sta ENEMY_X_POS,x
    rts

; add .0625 to Y velocity
add_10_to_enemy_y_fract_vel:
    lda #$10

; add a to enemy Y fractional velocity, incorporating carry into fast Y velocity
; input
;  * a - amount to add to ENEMY_Y_VELOCITY_FRACT
add_a_to_enemy_y_fract_vel:
    clc                          ; clear carry in preparation for addition
    adc ENEMY_Y_VELOCITY_FRACT,x ; add a to enemy Y fractional velocity
    sta ENEMY_Y_VELOCITY_FRACT,x ; store updated result in enemy Y fractional velocity
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    adc #$00                     ; add any carry from adding to fractional velocity
    sta ENEMY_Y_VELOCITY_FAST,x  ; store updated fast velocity if any carry occurred
    rts

; determines which player to use to determine if gray turret should be activated
;  * if the same player is closest along both axes, use that player
;  * if different player is closest on each axis, use player closest along y axis
; !(BUG?) - this code is needlessly complex, and incorrectly compares distance with player indexes in some cases
; it also first computes farthest players and compares against that, which isn't necessary
; instead it could have just targeted the closest player along y axis because that's the ultimate outcome
; input
;  * x - the current enemy offset
; output
;  * y - the player index of the player to consider when determining whether to activate
gray_turret_find_activation_player:
    jsr player_enemy_x_dist ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                     ; transfer closest player index to a
    eor #$01                ; calculate farthest player
    sta $0c                 ; store farthest player index (x-axis) from enemy in $0c
    lda $08                 ; load p1 x distance
    sta $0a                 ; store p1 x distance to enemy in $0a
    lda $09                 ; load p2 x distance
    sta $0b                 ; store p2 x distance to enemy in $0b
    jsr player_enemy_y_dist ; a = closest y distance to enemy from players, y = closest player (#$00 or #$01)
    tya                     ; transfer closest player on y axis to a
    eor #$01                ; calculate farthest player (y-axis)
    cmp $0c                 ; compare farthest player index (y-axis) to farthest player index (x-axis)
    bne @continue           ; branch if the farthest player on one axis is different than another axis to use closest player along y axis
    tya                     ; farthest player is the same for both axes, transfer closest player to a (y-axis)
    sta $0c                 ; set closest player index (y-axis)

;  * a - tentative targeting player index, either
;    * farthest player along y axis (when different player is closer depending on axis)
;    * closest player along y axis (same player is closest for both axes)
;  * $08 - p1 y distance
;  * $09 - p2 y distance
;  * $0a - the closest player (y-axis), #$00 for p1, #$01 for p2
;  * $0b - p2 x distance to enemy
;  * $0c - depending on situation one of the two options below
;    * farthest player index (x-axis) (when different player is closer depending on axis)
;    * closest player index (y-axis) (same player is closest for both axes)
@continue:
    tax       ; transfer tentative targeting player index to x
    tay       ; transfer tentative targeting player index to y
    lda $08,x ; load tentative targeting player y distance
    ldx $0c   ; load player index
    cmp $0a,x ; compare targeting player y distance to farthest player x distance
              ; !(HUH) $0a contains closest player index (y-axis)
              ; when x = 0, this is comparing y distance to a player index
    bcc @exit ; branch if y distance is closer than x distance
    ldy $0c   ; x distance is closer than y (or target player index is p1)
              ; load targeted player index
    lda $0a,x ; load targeted player x distance

@exit:
    sty $0a                ; store targeted player index
    ldx ENEMY_CURRENT_SLOT ; restore x to enemy slot
    rts

; calculates x distance between p1 and the enemy and p2 and the enemy
; stores shortest distance in a, player number in y
; input
;  * x - the current enemy offset
; output
;  * a - the shortest x distance to current enemy (either p1 or p2)
;  * y - the closest player index, #$00 for p1, #$01 for p2
;  * $08 - p1 x distance
;  * $09 - p2 x distance
;  * $0a - the closest player index, #$00 for p1, #$01 for p2
; when player state is not #$01, #$fe is stored in $08 or #$ff in $09
player_enemy_x_dist:
    lda PLAYER_SPRITE_X_POS ; load player 1 X position
    sec                     ; set carry flag in preparation for subtraction
    sbc ENEMY_X_POS,x       ; enemy X position on screen
    bcs @continue_to_p2     ; branch if no overflow occurred
    eor #$ff                ; overflow occurred, flip bits and add one (two's compliment)
    adc #$01

@continue_to_p2:
    sta $08                   ; store distance between player and enemy in $08
    lda PLAYER_SPRITE_X_POS+1 ; load player 2 X position
    sec                       ; set carry flag in preparation for subtraction
    sbc ENEMY_X_POS,x         ; subtract enemy X position on screen
    jmp lda_closer_distance   ; jump to determine smallest of $08 (p1) and $09 (p2) store in a

; calculates y distance between p1 and the enemy and p2 and the enemy
; stores shortest distance in a, player number in y
; input
;  * x - the current enemy offset
; output
;  * a - the shortest y distance to current enemy (either p1 or p2)
;  * y - the closest player, #$00 for p1, #$01 for p2
;  * $08 - p1 y distance
;  * $09 - p2 y distance
;  * $0a - the closest player, #$00 for p1, #$01 for p2
; when player state is not #$01, #$fe is stored in $08 or #$ff in $09
player_enemy_y_dist:
    lda PLAYER_SPRITE_Y_POS ; load player 1 Y position
    sec                     ; set carry flag in preparation for subtraction
    sbc ENEMY_Y_POS,x       ; enemy Y position on screen
    bcs @continue_to_p2     ; branch if no overflow occurred
    eor #$ff                ; overflow occurred, flip bits and add one (two's compliment)
    adc #$01

@continue_to_p2:
    sta $08                   ; store distance between player and enemy in $08
    lda PLAYER_SPRITE_Y_POS+1 ; load player 2 Y position
    sec                       ; set carry flag in preparation for subtraction
    sbc ENEMY_Y_POS,x         ; enemy Y position on screen

; take the smallest of $08 (p1) and $09 (p2) and store in a accounting for overflow
; ignoring non-normal player state
; output
;  * a - the distance of the closest player to the enemy
;  * y - the closest player, #$00 for p1, #$01 for p2
;  * $08 - p1 distance
;  * $09 - p2 distance
;  * $0a - the closest player index, #$00 for p1, #$01 for p2
lda_closer_distance:
    bcs @continue
    eor #$ff      ; overflow occurred, flip bits and add one (two's compliment)
    adc #$01

@continue:
    sta $09          ; store player 2 X or Y distance from current enemy in $09
    ldy #$fe         ; y = #$fe
    lda PLAYER_STATE ; load player state (2 = normal state)
    cmp #$02
    beq @continue_p1 ; branch if player state is #$02 (normal)
    sty $08          ; player 1 state not normal, store #$fe in $08

@continue_p1:
    ldy #$ff           ; y = #$ff
    lda PLAYER_STATE+1 ; load 2nd player state (0 = not playing, 2 = normal state)
    cmp #$02
    beq @set_closest   ; branch if p2 state is normal
    sty $09            ; player 2 state not normal, set to #$ff

@set_closest:
    lda $09   ; load player 2 distance (or #$ff if not normal)
    ldy #$01  ; default assume p2 is closer
    cmp $08   ; compare player 2 distance to player 1 distance
    bcc @exit ; branch if $09 < $08 (p2 is closer)
    dey       ; p1 is closer, ensure player index specified is p1
    lda $08   ; load the closest distance (p1)

@exit:
    sty $0a ; store closest player index in $0a
    rts

; jungle level earthquake
; larger, vertical shake, no horizontal shake
; play sound_08 after every #$10 frames
; input
;  * x - enemy slot offset to use for timing playing of sound_08
jungle_earthquake:
    lda GLOBAL_TIMER
    lsr              ; vertical shake every 2 frames
    ldy #$02         ; larger vertical shake (shift 2 pixels)
    bne y_shake      ; always branch

; shake vertically and horizontally
; for vertical shake component, shift every 8 frames
; for horizontal shake component, depending on global timer, shift every 2 frames
; play sound_08 after every #$10 frames
; input
;  * x - enemy slot offset to use for timing playing of sound_08
earthquake_shake:
    jsr x_earthquake_shake ; scroll left/right for earthquake effect
                           ; a now contains GLOBAL_TIMER shifted right twice
    ldy #$01               ; vertical shake of 1 pixel

; input
;  * a - bit 0 specifies vertical scroll
;    * 0 = set Y_SCROLL to 0
;    * 1 = set Y_SCROLL to y
;  * y - how big the vertical shake is, i.e. the PPU vertical scroll to set when
;    bit 0 of a is non-zero
y_shake:
    lsr           ; shift vertical shift bit to carry
    bcs @continue
    ldy #$00      ; PPU vertical scroll to top of nametables (no scroll)

; set vertical shake, wait for delay, play sound
@continue:
    sty Y_SCROLL                ; set PPU vertical scroll
    dec ENEMY_ANIMATION_DELAY,x
    bne @exit
    lda #$10
    sta ENEMY_ANIMATION_DELAY,x ; reset sound delay
    lda #$08                    ; sound_08 - earthquake shake
    jmp play_sound              ; sound_08 - earthquake shake

@exit:
    rts

; shift left by 1 pixel for 2 frames, then shift right by 1 pixel for 2 frames
; output
;  * a - GLOBAL_TIMER shifted right twice
x_earthquake_shake:
    lda GLOBAL_TIMER ; load global timer
    lsr
    lsr
    ldy #$00         ; set scroll position back to left
    bcc @continue    ; scroll position is alternates every 2 frames
    iny              ; set scroll position to 1

@continue:
    sty X_SCROLL     ; set horizontal scroll position
    sty IRQ_X_SCROLL ; set horizontal scroll position used by some interrupt routines
    rts

; sets current ENEMY_HP using easiest base HP difficulty adjustments
; based on ENEMY_DIFFICULTY [0-2] and staring HP amount (register a)
; enemy HP set with this method will be lowest
; input
;  * a - starting HP amount to adjust
;  * x - enemy slot index
set_enemy_hp:
    ldy #$00                      ; set base ENEMY_HP amount index to easy mode (indexes into enemy_hp_adjust_tbl)
    beq set_enemy_hp_from_a_and_y ; always branch

; sets current ENEMY_HP using hardest base HP difficulty adjustments
; based on ENEMY_DIFFICULTY [0-2] and staring HP amount (register a)
; enemy HP set with this method will be highest
; input
;  * a - starting HP amount to adjust
;  * x - enemy slot index
set_enemy_hp_hard:
    ldy #$03 ; set base ENEMY_HP amount index (indexes into enemy_hp_adjust_tbl)

; sets current ENEMY_HP based on ENEMY_DIFFICULTY and provided a and y registers
; input
;  * a - starting HP amount to adjust
;  * y - difficulty level 0, 3, or 6
;  * x - enemy slot index
set_enemy_hp_from_a_and_y:
    sta $08
    sty $09              ; store base ENEMY_HP
    lda ENEMY_DIFFICULTY ; load base enemy hp difficulty
    cmp #$02             ; see if greater than max
    bcc @continue        ; branch if difficulty is not greater than max
    lda #$01             ; difficulty is greater than max, just set to max (2)
                         ; remember carry is already set

@continue:
    adc $09                   ; add base ENEMY_HP amount index (indexes into enemy_hp_adjust_tbl)
    tay                       ; transfer to offset register
    lda enemy_hp_adjust_tbl,y ; load base ENEMY_HP
    clc                       ; clear carry in preparation for addition
    adc $08                   ; add any additional ENEMY_HP
    sta ENEMY_HP,x            ; store new ENEMY_HP
    rts

; each row represents y register
; each column represents ENEMY_DIFFICULTY
; amount to add to HP
enemy_hp_adjust_tbl:
    .byte $00,$04,$07 ; easy difficulty (y register = 0)
    .byte $00,$10,$18 ; hard difficulty (y register = 3)
    .byte $00,$04,$08 ; medium difficulty (y register = 6)

; creates enemy of type y at position of enemy whose slot is stored in ENEMY_CURRENT_SLOT
; input
;  * y - enemy type of enemy to create
; output
;  * x - enemy slot of created enemy, if created
;  * zero flag - set when enemy created, clear when no slot available
create_enemy_from_existing_enemy:
    jsr try_create_enemy_from_existing
    bcs create_enemy_exit              ; exit if enemy was created
    jsr replace_bullet_with_enemy      ; wasn't able to create enemy, try to by replacing a bullet
    jmp copy_enemy_vars                ; set newly created enemy's position, and attributes to match existing enemy's

; creates enemy of type y at position of enemy whose slot is stored in ENEMY_CURRENT_SLOT
; input
;  * y - enemy type of enemy to create
; output
;  * carry flag - set when enemy created, clear when no slot available
;  * $11 - enemy slot of created enemy, if created
try_create_enemy_from_existing:
    sty $17
    jsr create_enemy_y ; create enemy of type y in first available slot

; input
;  * x - the destination enemy index
;  * y - the source enemy index
copy_enemy_vars:
    bne @exit_fail         ; exit if unable to create the enemy
    stx $11                ; enemy created, set $11 to enemy slot of created enemy
    ldy ENEMY_CURRENT_SLOT ; load enemy slot of existing enemy
    lda $17                ; load backed-up enemy type
    sta ENEMY_TYPE,x       ; set enemy type of newly created enemy
    lda ENEMY_Y_POS,y      ; load current enemy's Y position
    sta ENEMY_Y_POS,x      ; set new enemy's Y position
    lda ENEMY_VAR_6,y
    sta ENEMY_VAR_6,x
    lda ENEMY_X_POS,y
    sta ENEMY_X_POS,x
    lda ENEMY_VAR_7,y
    sta ENEMY_VAR_7,x
    lda ENEMY_ATTRIBUTES,y
    sta ENEMY_ATTRIBUTES,x
    sec                    ; set carry indicating success creating enemy
    bcs @exit              ; always branch

@exit_fail:
    clc

@exit:
    ldx ENEMY_CURRENT_SLOT ; restore x to enemy slot

create_enemy_exit:
    rts

; update sprite for enemy (x) based on animation (y) index and animation frame
; (ENEMY_FRAME) using ENEMY_ANIMATION_DELAY to time animation
; the first time this is called, the delay will elapse and the ENEMY_FRAME will
; be set based on enemy_anim_sprite_tbl
; input
;  * ENEMY_ANIMATION_DELAY - enemy animation delay
;  * ENEMY_FRAME - animation frame index
;  * x - enemy slot index
;  * y - specific animation number to run
set_enemy_animation_sprite:
    dec ENEMY_ANIMATION_DELAY,x      ; decrement enemy animation delay
    bpl @exit
    lda enemy_anim_delay_tbl,y       ; animation delay elapsed, change sprite
                                     ; load next animation delay
    sta ENEMY_ANIMATION_DELAY,x      ; set next animation delay
    lda ENEMY_FRAME,x                ; load animation frame index
    sec                              ; set carry flag in preparation for subtraction
    sbc #$01                         ; move down to next animation frame index for the enemy
    bcs @continue
    sec
    lda enemy_anim_frame_count_tbl,y ; initialize the number of animation frames for enemy

@continue:
    sta ENEMY_FRAME,x           ; store new animation frame index
    adc enemy_anim_offset_tbl,y ; add frame index to starting index for animation
    tay
    lda enemy_anim_sprite_tbl,y ; load sprite within the animation
    sta ENEMY_SPRITE,x          ; set new sprite in the animation

@exit:
    rts

enemy_anim_delay_tbl:
    .byte $06          ; #$00 - soldier running
    .byte $06          ; #$01 - soldier in water
    .byte $04          ; #$02 - bullet explosion
    .byte $04          ; #$03 - grenade
    .byte $04          ; #$04 - humanoid alien (orian)
    .byte $03          ; #$05 - spinning bubbles
    .byte $01          ; #$06 - spinning bubbles
    .byte $06          ; #$07 - winged soldier running (enemy type #$40)
    .byte $05          ; #$08 - falling rock
    .byte $05          ; #$09 - falling rock
    .byte $05          ; #$0a - red blob (generated by krytpo-crustacean)
    .byte $04          ; #$0b - alien skull
    .byte $03          ; #$0c - jetpack soldier
    .byte $04          ; #$0d - baby alien ladybug
    .byte $04          ; #$0e - manooki
    .byte $04          ; #$0f - manooki projectile
    .byte $04          ; #$10 - alien spider
    .byte $04          ; #$11 - fire ring projectile
    .byte $04          ; #$12 - temple of terror red blob
    .byte $06          ; #$13 - poison drop
    .byte $06          ; #$14 - poison drop crawl
    .byte $04          ; #$15 - large red bubble
    .byte $04          ; #$16 - red/blue blob (final boss screen)
    .byte $01          ; #$17 - falling rubble
    .byte $05          ; #$18 - fortress wall orb (enemy type #$50)
    .ifdef Probotector
        .byte $06      ; #$19 - Level 7 (Headquarters) soldier
    .else
        .byte $00
    .endif
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00

enemy_anim_frame_count_tbl:
    .byte $05          ; #$00 - soldier running
    .byte $05          ; #$01 - soldier in water
    .byte $03          ; #$02 - bullet explosion
    .byte $07          ; #$03 - grenade
    .byte $05          ; #$04 - humanoid alien (orian)
    .byte $05          ; #$05 - spinning bubbles
    .byte $05          ; #$06 - spinning bubbles
    .byte $05          ; #$07 - winged soldier running (enemy type #$40)
    .byte $03          ; #$08 - falling rock
    .byte $03          ; #$09 - falling rock
    .byte $01          ; #$0a - red blob
    .byte $03          ; #$0b - alien skull
    .byte $01          ; #$0c - jetpack soldier
    .byte $01          ; #$0d - baby alien ladybug
    .byte $01          ; #$0e - manooki
    .byte $01          ; #$0f - manooki projectile
    .byte $02          ; #$10 - alien spider
    .byte $03          ; #$11 - fire ring projectile
    .byte $01          ; #$12 - temple of terror red blob
    .byte $01          ; #$13 - poison drop
    .byte $01          ; #$14 - poison drop crawl
    .byte $01          ; #$15 - large red bubble
    .byte $01          ; #$16 - red/blue blob
    .byte $07          ; #$17 - falling rubble
    .byte $01          ; #$18 - fortress wall orb (enemy type #$50)
    .ifdef Probotector
        .byte $05      ; #$19 - Level 7 (Headquarters) soldier
    .else
        .byte $00
    .endif
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00

enemy_anim_sprite_tbl:
    .byte $1f,$1c,$1e,$1d,$1c,$1b         ; #$00 - soldier running
    .byte $38,$39,$39,$38,$37,$37         ; #$01 - soldier in water
    .byte $05,$02,$04,$03                 ; #$02 - bullet explosion
    .byte $30,$2f,$2e,$2d,$2c,$2b,$2a,$29 ; #$03 - grenade
    .byte $35,$32,$34,$33,$32,$31         ; #$04 - humanoid alien (orian)
    .byte $61,$60,$5f,$5e,$5d,$5c         ; #$05 - spinning bubbles
    .byte $61,$60,$5f,$5e,$5d,$5c         ; #$06 - spinning bubbles
    .byte $6a,$67,$69,$68,$67,$66         ; #$07 - winged soldier running (enemy type #$40)
    .byte $72,$71,$70,$6f                 ; #$08 - falling rock
    .byte $6f,$70,$71,$72                 ; #$09 - falling rock
    .byte $74,$73                         ; #$0a - red blob (generated by krytpo-crustacean)
    .byte $76,$77,$76,$75                 ; #$0b - alien skull
    .byte $6e,$6d                         ; #$0c - jetpack soldier
    .byte $83,$82                         ; #$0d - baby alien ladybug
    .byte $91,$90                         ; #$0e - manooki
    .byte $93,$92                         ; #$0f - manooki projectile
    .byte $a0,$9f,$9e                     ; #$10 - alien spider
    .byte $a4,$a3,$a2,$a1                 ; #$11 - fire ring projectile
    .byte $a6,$a5                         ; #$12 - temple of terror red blob
    .byte $99,$98                         ; #$13 - poison drop
    .byte $98,$9d                         ; #$14 - poison drop crawl
    .byte $a9,$a8                         ; #$15 - large red bubble
    .byte $ab,$aa                         ; #$16 - red/blue blob
    .byte $b3,$b2,$b1,$b0,$af,$ae,$ad,$ac ; #$17 - falling rubble
    .byte $b5,$b4                         ; #$18 - fortress wall orb (enemy type #$50)
    .ifdef Probotector
        .byte $c0,$bd,$bf,$be,$bd,$bc     ; #$19 - Level 7 (Headquarters) soldier
    .endif

; each byte is a starting offset into enemy_anim_sprite_tbl, which specifies the
; series of sprites for an animation
enemy_anim_offset_tbl:
    .byte $ff          ; #$00
    .byte $05          ; #$01 - soldier in water
    .byte $0b          ; #$02 - bullet explosion
    .byte $0f          ; #$03 - grenade
    .byte $17          ; #$04 - humanoid alien (orian)
    .byte $1d          ; #$05 - spinning bubbles
    .byte $23          ; #$06 - spinning bubbles
    .byte $29          ; #$07 - winged soldier running
    .byte $2f          ; #$08 - falling rock
    .byte $33          ; #$09 - falling rock
    .byte $37          ; #$0a - red blob
    .byte $39          ; #$0b - alien skull
    .byte $3d          ; #$0c - jetpack soldier
    .byte $3f          ; #$0d - baby alien ladybug
    .byte $41          ; #$0e - manooki
    .byte $43          ; #$0f - manooki projectile
    .byte $45          ; #$10 - alien spider
    .byte $48          ; #$11 - fire ring projectile
    .byte $4c          ; #$12 - poison drop
    .byte $4e          ; #$13 - poison drop crawl
    .byte $50          ; #$14 - poison drop crawl
    .byte $52          ; #$15 - large red bubble
    .byte $54          ; #$16 - red/blue blob
    .byte $56          ; #$17 - falling rubble
    .byte $5e          ; #$18 - fortress wall orb
    .ifdef Probotector
        .byte $60      ; #$19 - Level 7 (Headquarters) soldier
    .endif

; enemy type #$00
weapon_item_routine_ptr_tbl:
    .addr weapon_item_routine_00 ; initialize frame, animation delay, hp, and Y velocity
    .addr weapon_item_routine_01 ; continues path, until lands on surface or goes off screen
    .addr weapon_item_routine_02 ; flash until delay elapses, then remove

; initialize frame, animation delay, hp, and Y velocity
weapon_item_routine_00:
    lda #$00
    sta ENEMY_FRAME,x                  ; initialize frame to 0
    sta ENEMY_ANIMATION_DELAY,x        ; initialize animation delay to 0
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x          ; mark weapon item so bullets travel through it
    lda #$f0
    sta ENEMY_HP,x                     ; set special HP #$f0 not destroyed by destroy_all_enemies routine
    lda OVERHEAD_FLAG                  ; (0 = side view, 1 = overhead view)
    asl                                ; double since each entry is a #$02 byte memory address
    tay                                ; transfer to offset register
    lda weapon_item_init_y_vel_tbl,y   ; load fractional Y velocity
    sta ENEMY_Y_VELOCITY_FRACT,x       ; set weapon item fractional Y velocity
    lda weapon_item_init_y_vel_tbl+1,y ; load fast Y velocity
    sta ENEMY_Y_VELOCITY_FAST,x        ; set weapon item fast Y velocity
    jsr update_pos_check_offscreen     ; adjust position based on scroll (does not apply velocity)
    lda #$f0
    jmp set_delay_adv_enemy_routine    ; set delay to #$f0 and set routine to weapon_item_routine_01

weapon_item_init_y_vel_tbl:
    .byte $40,$fc ; outdoor (-3.75)
    .byte $00,$fd ; overhead view (-3.00)

; continues path, until lands on surface or goes off screen
weapon_item_routine_01:
    jsr set_weapon_item_sprite     ; set sprite, attributes (palette) and update palette for flashing effect
    jsr update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)
    lda OVERHEAD_FLAG              ; (0 = side view, 1 = overhead view)
    beq @weapon_item_outdoor       ; branch for outdoor level
    lda ENEMY_Y_VELOCITY_FAST,x    ; overhead level, rises for a bit, then falls on ground no special bg collision check
    bmi @continue                  ; branch if weapon item moving up to apply gravity and exit
    cmp #$03                       ; see if falling velocity is greater than or equal to 3
    bcc @continue                  ; continue if not falling really fast
    jmp advance_enemy_routine      ; falling pretty fast advance to next routine to stop velocity

@weapon_item_outdoor:
    lda ENEMY_Y_POS,x                      ; load enemy's Y position
    cmp #$08
    bcc @continue                          ; branch if int top 3% of screen
    ldy ENEMY_VAR_6,x                      ; load whether weapon item is off screen vertically (set in update_pos_check_offscreen)
    bne @continue                          ; branch if off screen to skip bg collision checking
    ldy ENEMY_Y_VELOCITY_FAST,x            ; load enemy's fast Y velocity
    bpl @test_bg_collision                 ; branch if weapon item is falling
    lda #$f8                               ; weapon item rising
                                           ; subtract #$08 from enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code        ; test background collision
    tay                                    ; transfer bg collision type to y
    lda bg_collision_test_no_incline_tbl,y ; determine if should stop velocity (landed on background)
    beq @continue                          ; branch to apply gravity if didn't land on background (ground, water, etc)
    bne @clear_enemy_vel                   ; landed on something, clear velocity (always branch)

@test_bg_collision:
    lda #$08                        ; add #$08 to enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
    beq @continue                   ; branch to apply gravity if didn't land on background (ground, water, etc)
    jsr set_y_pos_for_bg            ; landed on something, set player Y position

@clear_enemy_vel:
    jsr clear_enemy_vel          ; set enemy X and Y velocity to 0
    jmp weapon_item_check_remove ; decrement delay and remove if elapsed

@continue:
    lda #$18
    jsr add_a_to_enemy_y_fract_vel
    jmp weapon_item_check_remove   ; decrement delay and remove if elapsed

weapon_item_routine_02:
    jsr set_weapon_item_sprite ; set sprite, attributes (palette) and update palette for flashing effect
    jsr update_enemy_pos       ; adjust position based on scroll (does not apply velocity)

weapon_item_check_remove:
    lda GLOBAL_TIMER
    lsr
    bcs weapon_item_exit ; exit if odd frame
    dec ENEMY_DELAY,x
    bne weapon_item_exit ; exit if delay hasn't elapsed
    jmp remove_enemy     ; delay has elapsed, remove enemy

; sets sprite, attributes (palette) and updates palette for flashing effect
set_weapon_item_sprite:
    lda ENEMY_ATTRIBUTES,x  ; load enemy attributes
    and #$0f                ; keep weapon item type portion
    clc                     ; clear carry in preparation for addition
    adc #$08                ; add 8 to determine sprite code
    sta ENEMY_SPRITE,x      ; set weapon item sprite (sprite_08, sprite_09, ... sprite_0e)
    lda GLOBAL_TIMER        ; load global timer
    lsr                     ; sprite flashes every 2 frames
    and #$03                ; flashes among 4 different colors
    sta ENEMY_SPRITE_ATTR,x ; set current flash attribute palette

weapon_item_exit:
    rts

; enemy type #$01
flying_capsule_routine_ptr_tbl:
    .addr flying_capsule_routine_00  ; set initial position, velocity, sprite attribute, destroy attributes, advance routine
    .addr flying_capsule_routine_01  ; animate path
    .addr flying_capsule_routine_02  ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set initial position, velocity, sprite attribute, destroy attributes, advance routine
flying_capsule_routine_00:
    lda #$00
    sta ENEMY_FRAME,x
    sta ENEMY_ANIMATION_DELAY,x
    lda #$01
    sta ENEMY_SPRITE_ATTR,x
    lda #$1d
    sta ENEMY_DESTROY_ATTRS,x            ; enable bullet collision, disable player collision
                                         ; set spike explosion, set destroyed sound to sound_18 (APPEAR)
    lda ENEMY_ATTRIBUTES,x
    and #$30                             ; strip to the 2 bits that define initial position and velocity
    lsr
    lsr
    tay                                  ; transfer to offset register
    lda flying_capsule_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x         ; store enemy's fractional Y velocity
    lda flying_capsule_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x          ; store enemy's fast Y velocity
    lda flying_capsule_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x         ; store enemy's fractional X velocity
    lda flying_capsule_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x          ; store enemy's fast X velocity
    tya
    lsr                                  ; each entry is only 2 bytes, so half
    tay                                  ; transfer to offset register
    lda flying_capsule_initial_pos_tbl,y
    beq @continue                        ; skip setting if zero (not necessary since already 0)
    sta ENEMY_Y_POS,x                    ; set the Y position

@continue:
    lda flying_capsule_initial_pos_tbl+1,y
    beq @set_pos_adv_routine               ; skip setting if zero (not necessary since already 0)
    sta ENEMY_X_POS,x                      ; set the X position

@set_pos_adv_routine:
    lda ENEMY_Y_POS,x                ; load enemy's Y position
    sta ENEMY_VAR_1,x
    lda ENEMY_X_POS,x                ; load enemy's X position
    sta ENEMY_VAR_2,x
    jsr apply_velocity               ; apply enemy's velocity to its position, removing enemy if off-screen
    jsr flying_capsule_handle_scroll
    jmp advance_enemy_routine        ; advance to next routine

flying_capsule_initial_pos_tbl:
    .byte $50,$f8 ; pos = (-#$08, #$50)
    .byte $00,$08 ; pos = ( #$08, #$00)
    .byte $d0,$00 ; pos = ( #$00, #$d0)
    .byte $30,$08 ; pos = ( #$08, #$30)

flying_capsule_vel_tbl:
    .byte $00,$fe,$00,$fe ; x vel = -2, y vel = -2
    .byte $00,$fe,$00,$02 ; x vel =  2, y vel = -2
    .byte $00,$fe,$00,$02 ; x vel =  2, y vel = -2
    .byte $00,$fe,$00,$02 ; x vel =  2, y vel = -2

; #$00 - left to right
; #$01 - bottom to top (cliff level)
flying_capsule_dir_tbl:
    .byte $00,$00,$01,$00

; animate path
flying_capsule_routine_01:
    lda ENEMY_ATTRIBUTES,x
    and #$30                     ; strip to just 4 and 5 of attribute
    lsr
    lsr
    lsr
    lsr
    tay
    lda flying_capsule_dir_tbl,y ; determine direction (0 = left to right, 1 = bottom to top)
    bne @y_dir                   ; branch if moving from bottom to top (e.g. cliff level)
    ldy #$02                     ; controls swooping amplitude
    jsr modify_enemy_y_vel       ; adjust Y velocity based on ENEMY_Y_POS,x distance from ENEMY_VAR_1,x
    jmp set_sprite_and_velocity

@y_dir:
    ldy #$02               ; controls swooping amplitude
    jsr modify_enemy_x_vel ; adjust X velocity based on ENEMY_X_POS,x distance from ENEMY_VAR_2,x

set_sprite_and_velocity:
    lda #$07           ; a = #$07 (sprite_07)
    sta ENEMY_SPRITE,x ; write enemy sprite code to CPU buffer
    jsr apply_velocity ; apply enemy's velocity to its position, removing enemy if off-screen

; update ENEMY_VAR_1,x and ENEMY_VAR_2,x based on vertical and horizontal scroll
; respectively
flying_capsule_handle_scroll:
    lda ENEMY_VAR_1,x
    sec                ; set carry flag in preparation for subtraction
    sbc Y_SCROLL_SPEED ; subtract any vertical scrolling this frame
    sta ENEMY_VAR_1,x  ; set new value accounting for vertical scroll
    lda ENEMY_VAR_2,x
    clc                ; clear carry in preparation for addition
    adc X_SCROLL_SPEED ; how much to scroll horizontally the screen this frame (#00 - no scroll)
    sta ENEMY_VAR_2,x  ; set new value accounting for horizontal scroll
    rts

; enemy destroyed routine
flying_capsule_routine_02:
    ldy #$00                             ; weapon item enemy type
    jsr create_enemy_from_existing_enemy ; create weapon item at position of enemy whose slot is stored in ENEMY_CURRENT_SLOT
    jmp enemy_explosion_routine_00       ; set empty sprite, play optional enemy destroyed sound, disable collisions

; enemy type #$0e
enemy_overhead_bullet_routine_ptr_tbl:
    .addr enemy_bullet_routine_00          ; set sprite to muzzle flash, advance routine with delay
    .addr enemy_bullet_routine_01          ; wait for muzzle delay, set actual bullet sprite, update position
    .addr enemy_overhead_bullet_routine_02 ; flash palette, check for bg collision
    .addr enemy_overhead_bullet_routine_03 ; enemy destroyed routine, set animation delay before advance routine
    .addr enemy_overhead_bullet_routine_04 ; show bullet explosion, then remove enemy

; flash palette, check for bg collision
; very similar to enemy_bullet_routine_02, but doesn't have y offset for bg collision check
enemy_overhead_bullet_routine_02:
    inc ENEMY_DELAY,x
    lda ENEMY_DELAY,x
    lsr
    lsr
    and #$03
    sta ENEMY_SPRITE_ATTR,x         ; change palette every 4 frames
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_FIRING_DELAY,x        ; decrement bullet destroy delay
    beq @adv_routine                ; branch if delay elapsed to advance routine to enemy_overhead_bullet_routine_03 (destroy bullet)
    lda #$00                        ; destroy delay hasn't elapsed, check bullet bg collision with no y offset
    jsr get_enemy_bg_collision_code ; test background collision
    tay                             ; transfer collision code to y
    lda bg_collision_test_tbl,y     ; load whether or not collision code should destroy bullet
    beq enemy_overhead_bullet_exit  ; branch if collision code shouldn't destroy bullet

@adv_routine:
    jmp advance_enemy_routine ; advance to next routine to enemy_overhead_bullet_routine_03 (destroy bullet)

enemy_overhead_bullet_exit:
    rts

; enemy destroyed routine, set delay before advance routine
enemy_overhead_bullet_routine_03:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$08
    jmp set_delay_adv_enemy_routine ; set delay to #$08 and set routine to enemy_overhead_bullet_routine_04
                                    ; delay of #$08 is for the first frame of the explosion animation

; show bullet explosion, then remove enemy
enemy_overhead_bullet_routine_04:
    jsr update_enemy_pos           ; adjust position based on scroll (does not apply velocity)
    lda #$02
    sta ENEMY_SPRITE_ATTR,x        ; set palette
    lda #$04                       ; a = sprite_04 (bullet explosion frame 1)
    sta ENEMY_SPRITE,x             ; set bullet sprite to explosion
    dec ENEMY_DELAY,x              ; decrement animation delay
    bne enemy_overhead_bullet_exit ; exit if animation delay hasn't elapsed
    jmp remove_enemy               ; animation delay has elapsed, remove enemy

; enemy type #$02
; used by enemy type #$3a (robot spider bullet) as well
enemy_bullet_routine_ptr_tbl:
    .addr enemy_bullet_routine_00 ; set sprite to muzzle flash, advance routine with delay
    .addr enemy_bullet_routine_01 ; wait for muzzle delay, set actual bullet sprite, update position
    .addr enemy_bullet_routine_02 ; flash palette, check for bg collision
    .addr remove_enemy            ; enemy destroyed routine

; set sprite to muzzle flash, advance routine with delay
enemy_bullet_routine_00:
    lda #$80
    sta ENEMY_DESTROY_ATTRS,x       ; specify that bullets travel through each other
    lda #$02
    sta ENEMY_SPRITE_ATTR,x         ; set sprite palette
    lda #$02
    sta ENEMY_SPRITE,x              ; set to sprite_02 (muzzle flash)
    lda #$04
    jmp set_delay_adv_enemy_routine ; set delay to #$04 and set routine to enemy_bullet_routine_01

; used by both enemy bullet (enemy type = #$02) as well as overhead tank soldier bullet (enemy type = #$30)
; wait for muzzle delay, set actual bullet sprite, update position repeatedly
enemy_bullet_routine_01:
    jsr update_enemy_pos                 ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne bullet_routine_exit              ; exit if show muzzle  delay hasn't elapsed
    ldy ENEMY_VAR_1,x                    ; load bullet type
    lda enemy_bullet_sprite_tbl,y        ; load sprite for bullet type (sprite_05 or sprite_06)
    sta ENEMY_SPRITE,x                   ; set actual bullet sprite
    lda enemy_bullet_destroy_delay_tbl,y
    sta ENEMY_FIRING_DELAY,x             ; set delay before destroyed for overhead tank soldier bullets and overhead bullets
                                         ; not used for non-indoor level bullets

bullet_adv_enemy_routine:
    jmp advance_enemy_routine ; advance to next routine

; sprite_05 or sprite_06
enemy_bullet_sprite_tbl:
    .byte $05,$06,$06

; delay before bullet is destroyed (overhead tank soldier bullets and overhead bullets)
enemy_bullet_destroy_delay_tbl:
    .byte $50,$50,$38

; flash palette, check for bg collision
enemy_bullet_routine_02:
    inc ENEMY_DELAY,x
    lda ENEMY_DELAY,x
    lsr
    lsr
    and #$03
    sta ENEMY_SPRITE_ATTR,x                ; change palette every 4 frames
    jsr apply_velocity                     ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$f8                               ; subtract #$08 from enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code        ; test background collision
    tay                                    ; transfer bg collision type to y
    lda INCLINE_BG_COLLISION_FLAG          ; load whether or not bullets should collide with inclines
    bne @check_collision                   ; branch if bullets should collide with inclines
    lda bg_collision_test_no_incline_tbl,y ; bullets should not collide with inclines (level 5 or level 8)
                                           ; use this table instead of bg_collision_test_tbl for testing bg collisions
    bne bullet_adv_enemy_routine           ; branch to remove bullet if collided with background
    rts

; general collision check for all levels except 5 and 8
@check_collision:
    lda bg_collision_test_tbl,y  ; see if collision code counts for bullet
    bne bullet_adv_enemy_routine ; branch if collision counts to advance routine and destroy bullet

bullet_routine_exit:
    rts

; enemy type #$03
soldier_routine_ptr_tbl:
    .addr soldier_routine_00         ; wait for animation delay
    .addr soldier_routine_01         ; soldier animation delay elapsed, animate chasing player
    .addr soldier_routine_02         ; red soldier fire at player (level 4 only)
    .addr soldier_routine_03         ; enemy destroyed routine, set collision velocity
    .addr soldier_routine_04         ; enemy falling after collision until ENEMY_DELAY elapses
    .addr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; wait for animation delay
soldier_routine_00:
    lda ENEMY_DELAY,x           ; load enemy's animation delay
    beq @continue               ; branch if animation delay has elapsed
    dec ENEMY_DELAY,x           ; delay not elapsed, decrement delay
    jmp apply_y_scroll_to_y_pos ; apply Y_SCROLL_SPEED to soldier Y position and exit

; soldier animation delay elapsed, animate chasing player
@continue:
    ldy #$12                                 ; !(HUH) y is immediately overwritten
                                             ; developer probably meant lda #$12, which is done in many other locations
                                             ; to test bg collision with a Y offset of #$12
    jsr get_enemy_bg_collision_code_onscreen ; get bg collision code if on-screen
    beq @set_initial_x_pos
    jsr set_y_pos_for_bg                     ; set enemy Y position based on background collision

@set_initial_x_pos:
    lda ENEMY_ATTRIBUTES,x
    bmi @set_soldier_type  ; branch if orian (alien)
    lsr                    ; shift walking direction to carry
    bcc @set_soldier_type  ; branch if walking left
    lda #$10               ; waking right, set enemy X position to 16
    sta ENEMY_X_POS,x      ; set starting X position at #$10

@set_soldier_type:
    lda ENEMY_ATTRIBUTES,x ; load enemy attributes
    and #$7f               ; strip bit 7 (soldier or orian)
    ldy CURRENT_LEVEL      ; load current level
    cpy #$07               ; compare to the last level (The Final Stage)
    bne @set_anim_index    ; branch if not last level (The Final Stage)
    ora #$80               ; last level, set bit 7

@set_anim_index:
    sta ENEMY_ATTRIBUTES,x          ; update enemy attributes to include bit 7 based on level
                                    ; whether or not soldier is human or orian
    asl                             ; push whether or not soldier is human or orian to carry
    lda #$00                        ; soldier running animation index
    bcc @set_sprite_vel_adv_routine ; branch if not last level, i.e. bit 7 is clear
    lda #$04                        ; last level (orian soldier), set animation use be orian walking animation

@set_sprite_vel_adv_routine:
    sta ENEMY_VAR_3,x           ; set sprite animation (see set_enemy_animation_sprite)
    .ifdef Probotector
        cpy #$06                ; see if Level 7
        bne @set_vel_update_pos ; branch if not Level 7 (Headquarters)
        lda #$19                ; set sprite animation for Level 7 soldiers
        sta ENEMY_VAR_3,X       ; set sprite animation (see set_enemy_animation_sprite)
        lda ENEMY_ATTRIBUTES,x
        ora #$40
        sta ENEMY_ATTRIBUTES,x  ; set bit 6 to indicate this is a Level 7 soldier
                                ; used in soldier_routine_01
    .endif

@set_vel_update_pos:
    lda #$08
    sta ENEMY_COLLISION_INDEX,x      ; set enemy's collision index (specifies collision box dimensions)
    jsr set_soldier_x_vel            ; set X velocity based on soldier direction, and whether soldier is jumping
    jsr set_gravity_y_vel            ; set Y velocity to 1.0 (pulling down)
    lda ENEMY_ATTRIBUTES,x
    lsr
    lsr
    lsr
    and #$03
    sta ENEMY_VAR_4,x                ; set soldier sprite palette (see soldier_sprite_attr_tbl)
                                     ; when zero, soldier uses palette as specified by level
    jsr set_initial_facing_direction ; set initial facing direction of enemy based on attributes
    lda #$03
    sta ENEMY_VAR_2,x                ; !(UNUSED) this variable is never read
    lda #$10
    sta ENEMY_DELAY,x
    jsr update_enemy_pos             ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine        ; advance to next routine

set_soldier_x_vel_if_moving:
    lda ENEMY_X_VELOCITY_FRACT,x ; load enemy's fractional X velocity
    ora ENEMY_X_VELOCITY_FAST,x
    bne set_soldier_x_vel_exit   ; branch if there is any X velocity

; set X velocity based on ENEMY_ATTRIBUTES,x (direction), whether the soldier is
; jumping (ENEMY_VAR_1,x) and SCREEN_SCROLL_TYPE
set_soldier_x_vel:
    lda ENEMY_ATTRIBUTES,x
    lsr                     ; push facing direction into carry (0 = walking left, 1 = walking right)
    lda #$00
    ldy SCREEN_SCROLL_TYPE  ; 0 = horizontal, 1 = vertical/overhead
    beq @check_soldier_type
    lda #$04

@check_soldier_type:
    ldy ENEMY_ATTRIBUTES,x
    bpl @check_direction   ; branch if not last level (orian), i.e. bit 7 is set
    lda #$08               ; last level

@check_direction:
    bcc @continue ; branch if walking left
    adc #$01      ; walking right

@continue:
    ldy ENEMY_VAR_1,x ; see if jumping, i.e. is #$80
    bpl @set_x_vel
    clc               ; clear carry in preparation for addition
    adc #$0c          ; jumping, use jumping x velocities

@set_x_vel:
    tay
    lda soldier_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda soldier_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity

set_soldier_x_vel_exit:
    rts

; sets Y velocity to 1.0 (pulling down)
set_gravity_y_vel:
    lda #$00
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda #$01
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    rts

soldier_x_vel_tbl:
    .byte $00,$ff ; -1
    .byte $40,$01 ;  1.25
    .byte $00,$ff ; -1
    .byte $00,$01 ;  1
    .byte $c0,$fe ; -1.25
    .byte $80,$01 ;  1.5

; jumping x velocities
    .byte $20,$ff ; -0.875
    .byte $e0,$00 ;  0.875
    .byte $20,$ff ; -0.875
    .byte $e0,$00 ;  0.875
    .byte $c0,$fe ; -1.25
    .byte $80,$01 ;  1.5
    .byte $0c,$18 ; !(HUH) 24.046875

soldier_routine_01:
    .ifdef Probotector
        lda ENEMY_VAR_1,x     ; bit 7 set when jumping
        bmi @check_orian      ; branch if jumping
        jmp @dec_firing_delay ; not jumping, jump to decrement firing delay

    @check_orian:
        lda ENEMY_ATTRIBUTES,x
        asl
        ldy #$20               ; regular soldier, use sprite_20
        bcc @check_level_7     ; branch if not orian (alien)
        ldy #$36               ; orian, use sprite_36

    @check_level_7:
        asl           ; push bit 6 into carry
        bcc @continue ; branch if not Level 7 soldier
        ldy #$c1      ; level 7 soldier, use sprite_c1

    @continue:
        tya                    ; move sprite code to a
    .else
        lda ENEMY_VAR_1,x      ; bit 7 set when jumping
        bpl @dec_firing_delay  ; branch if not jumping to decrement firing delay if red firing soldier
                               ; and continue with bg collision checking
        lda #$20               ; sprite_20 (jumping soldier)
        ldy ENEMY_ATTRIBUTES,x ; see if orian or human soldier
        bpl @set_jumping_vars  ; branch if on last level (The Final Stage) to use orian instead of soldier
        lda #$36               ; sprite_36 (jumping orian)
    .endif

@set_jumping_vars:
    sta ENEMY_SPRITE,x     ; set soldier sprite
    lda #$14
    sta $0f                ; for use in get_bg_collision_y_range
    lda ENEMY_Y_POS,x      ; load enemy's Y position
    clc                    ; clear carry in preparation for addition
    adc #$06
    tay
    lda ENEMY_ATTRIBUTES,x
    lsr
    lda #$07               ; adding 8
    bcs @set_x             ; branch if walking right
    lda #$f8               ; walking left, subtracting 8

@set_x:
    sta $08                      ; set amount adding/subtracting
    adc ENEMY_X_POS,x
    sta $09                      ; set soldier new X position in $09
    ror                          ; shift any overflow to bit 7
    eor $08                      ; compare with intended change (adding or subtracting)
    bmi @set_x_if_moving         ; branch if overflow or underflow
    lda $09                      ; load new X position
    jsr get_bg_collision_y_range ; get bg collision over a vertical range for (a,y)
    bcc @set_x_if_moving         ; branch if no bg collision
    jsr clear_enemy_x_vel        ; collision, set enemy X velocity (fast and fractional) to 0
    jmp @pos_collision

@set_x_if_moving:
    jsr set_soldier_x_vel_if_moving

@pos_collision:
    jsr update_pos_check_offscreen           ; adjust position based on scroll (does not apply velocity)
    lda #$12
    jsr get_enemy_bg_collision_code_onscreen ; get bg collision code if on-screen
    beq @apply_gravity
    jsr set_y_pos_for_bg                     ; set enemy Y position based on background collision
    jsr set_gravity_y_vel                    ; set Y velocity to 1.0 (pulling down)
    lda #$00
    sta ENEMY_VAR_1,x
    sta ENEMY_ANIMATION_DELAY,x
    lda ENEMY_ATTRIBUTES,x
    and #$02
    beq @set_x_vel                           ; branch if soldier doesn't fire
    jsr player_enemy_x_dist                  ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_X_POS,x                        ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y
    lda ENEMY_ATTRIBUTES,x
    and #$fc
    bcs @set_attr
    ora #$01

@set_attr:
    sta ENEMY_ATTRIBUTES,x
    jsr set_initial_facing_direction ; set initial facing direction of enemy based on attributes

@set_x_vel:
    jsr set_soldier_x_vel ; set X velocity based on soldier direction, and whether soldier is jumping

@apply_gravity:
    lda #$1c
    jmp add_a_to_enemy_y_fract_vel

@dec_firing_delay:
    lda ENEMY_VAR_4,x       ; load soldier override palette
    beq @check_collision    ; branch if normal soldier (green)
    dec ENEMY_DELAY,x       ; red firing soldier, decrement delay to stay in soldier_routine_00
                            ; this is how long to run before pausing to fire
    beq soldier_adv_routine ; soldier should fire at player
                            ; set firing delay and advance routine

@check_collision:
    jsr soldier_check_collision
    bcs @update_pos                 ; branch if wall collision and it can't be jumped over
    jsr update_pos_check_offscreen  ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ROUTINE,x
    beq @exit
    lda #$12                        ; add #$12 to enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test background collision
                                    ; already tested in soldier_check_collision can confirmed that if there is a collision
                                    ; it can be jumped over.  checking to see if need to jump
    beq @jump
    jsr set_y_pos_for_bg            ; set enemy Y position based on background collision
    lda $09                         ; load bg collision code
    ldy ENEMY_VAR_3,x               ; load current animation to use
    cmp #$04                        ; see if in water
    bne @set_sprite_anim            ; branch if not water collision to use existing animation
    ldy #$01                        ; soldier is in water, use soldier in water animation

@set_sprite_anim:
    jmp set_enemy_animation_sprite ; animate sprite for soldier/orian based on ENEMY_FRAME and ENEMY_ANIM_DELAY
                                   ; using ENEMY_ANIMATION_DELAY for enemy animation delay
                                   ; and ENEMY_FRAME for animation frame index

@exit:
    rts

@jump:
    jsr player_enemy_x_dist   ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    sec                       ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_Y_POS,y
    ldy #$0c                  ; assume soldier below player (soldier_jump_vel_tbl offset)
    bcs @check_overflow       ; branch if no underflow (soldier below player)
    ldy #$08                  ; soldier above player, use middle row from soldier_jump_vel_tbl
    eor #$ff
    adc #$01                  ; convert distance to positive

@check_overflow:
    cmp #$10           ; see if soldier is farther than #$10 pixels from player vertically
    bcs @rand_jump_vel ; branch if player is much higher/lower than player
    ldy #$00           ; player and soldier are close vertically (use first row of soldier_jump_vel_tbl)

@rand_jump_vel:
    sty $08                    ; set initial soldier_jump_vel_tbl offset
    lda RANDOM_NUM             ; load random number
    and #$07                   ; random number between 0 and 7
    clc                        ; clear carry in preparation for addition
    adc $08                    ; add random offset from current row
    tay
    lda soldier_jump_vel_tbl,y ; load soldier_jmp_y_vel_tbl offset
    tay                        ; move random Y velocity index to offset register
    jmp @set_jump

@update_pos:
    jsr update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)
    ldy #$00                       ; -3.25 Y jump velocity

; set Y jump velocity to either -3.25 -2, or -0.75 depending on y
@set_jump:
    lda soldier_jmp_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x  ; store enemy's fractional Y velocity
    lda soldier_jmp_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x   ; store enemy's fast Y velocity
    lda #$80
    sta ENEMY_VAR_1,x             ; mark soldier jumping
    lda ENEMY_SPRITE_ATTR,x
    and #$df                      ; strip bit 5 (bg priority)
    sta ENEMY_SPRITE_ATTR,x
    jmp set_soldier_x_vel         ; set X velocity based on soldier direction, and whether soldier is jumping

soldier_routine_exit:
    rts

; soldier is red, set firing delay and advance routine
soldier_adv_routine:
    lda #$04
    sta ENEMY_FIRING_DELAY,x        ; set firing delay
    lda #$0e
    jmp set_delay_adv_enemy_routine ; set delay to #$0e and set routine to soldier_routine_02

; references indexes into soldier_jmp_y_vel_tbl
soldier_jump_vel_tbl:
    .byte $02,$04,$02,$02,$04,$04,$02,$04 ; soldier above player vertically
    .byte $04,$04,$04,$04,$04,$04,$04,$04 ; player and soldier close vertically
    .byte $02,$02,$02,$02,$02,$02,$02,$02 ; soldier below player vertically

soldier_jmp_y_vel_tbl:
    .byte $c0,$fc ; -3.25
    .byte $00,$fe ; -2
    .byte $40,$ff ; -0.75

; red soldier fire at player
soldier_routine_02:
    jsr player_enemy_x_dist     ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda PLAYER_SPRITE_X_POS,y   ; load the X position of the closest player
    cmp ENEMY_X_POS,x           ; compare to soldier's current position
    jsr set_soldier_sprite_attr
    lda ENEMY_VAR_4,x           ; see if soldier should crouch to fire
    lsr
    lda #$21                    ; sprite_21 (red soldier)
    bcs @continue               ; branch if soldier fires standing up
                                ; !(UNUSED) soldier will always be standing
    lda #$10                    ; soldier should crouch to fire
    sta ENEMY_COLLISION_INDEX,x ; set enemy's collision index (specifies collision box dimensions)
    lda #$22                    ; sprite_22 (unused crouching fire red soldier)

@continue:
    sta ENEMY_SPRITE,x       ; set soldier sprite (sprite_21)
    jsr update_enemy_pos     ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_FIRING_DELAY,x ; load animation delay before firing
    beq @set_chase_routine   ; branch if fired already to continue process of changing to soldier_routine_01
    dec ENEMY_FIRING_DELAY,x ; decrement animation delay before firing
    bne soldier_routine_exit ; exit if animation delay before firing hasn't elapsed
    lda #$06                 ; ready to fire
    sta ENEMY_VAR_5,x        ; initialize sprite recoil effect timer
    jmp soldier_fire_bullet  ; fire bullet

@set_chase_routine:
    dec ENEMY_DELAY,x                ; decrement delay before going back to chasing player
    bne soldier_routine_exit         ; exit if delay hasn't elapsed
    lda #$08                         ; delay elapsed
    sta ENEMY_COLLISION_INDEX,x      ; set enemy's collision index (specifies collision box dimensions)
    jsr set_initial_facing_direction ; set initial facing direction of enemy based on attributes
    lda #$50
    sta ENEMY_DELAY,x                ; set how long to stay in soldier_routine_01
    lda #$02
    jmp set_enemy_routine            ; set routine to soldier_routine_01
                                     ; to walk towards player

; soldier and sandbag soldier enemy destroyed routine
soldier_routine_03:
    lda ENEMY_ATTRIBUTES,x
    bpl @continue          ; branch if orian
    lda #$15               ; regular soldier
    jsr play_sound         ; play sound_15 (A OUT) - soldier destroyed

@continue:
    jsr get_soldier_start_sprite ; get the sprite: either a running soldier, or a running orian

; sets enemy sprite and velocity as it is being destroyed
; input
;  * a - enemy sprite
set_enemy_destroy_sprite_and_vel:
    sta ENEMY_SPRITE,x

; sets enemy destroyed X velocity to +/-.375 and Y velocity to -3.5
set_enemy_destroy_dir_and_vel:
    lda #$83
    sta ENEMY_DESTROY_ATTRS,x    ; allow bullets to pass, don't test player collision, play explosion
    lda #$80
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda #$fc
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda #$60
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda #$00
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    lda ENEMY_X_POS,x            ; load enemy's X position
    cmp #$10
    bcc @stop_x_vel
    cmp #$f0
    bcc @check_enemy_dir

@stop_x_vel:
    jsr clear_enemy_x_vel ; set enemy X velocity (fast and fractional) to 0

@check_enemy_dir:
    lda ENEMY_SPRITE_ATTR,x
    asl
    bmi @update_pos_adv_routine ; branch if sprite is flipped horizontally
    jsr flip_enemy_x_dir        ; flip the sign of the enemy's X velocity for falling back, e.g. 1.25 becomes -1.25

@update_pos_adv_routine:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$10
    jmp set_delay_adv_enemy_routine ; set delay to #$10 and advance routine

; enemy falling after collision until ENEMY_DELAY elapses
soldier_routine_04:
    jsr get_soldier_start_sprite ; get the sprite: either a running soldier, or a running orian

; set sprite for fall, add to enemy falling velocity, and apply velocity to enemy
; if ENEMY_DELAY has elapsed, advance the enemy routine
; input
;  * a - sprite number to use
apply_gravity_adv_routine_after_delay:
    sta ENEMY_SPRITE,x ; set sprite to use for fall

; adds .1875 to Y velocity (simulate gravity), applies velocity
; if ENEMY_DELAY has elapsed, advance the enemy routine
apply_destroy_vel_adv_routine_after_delay:
    lda #$30
    jsr add_a_to_enemy_y_fract_vel
    lda ENEMY_Y_POS,x                 ; load enemy's Y position
    cmp #$08
    bcc @advance_routine_exit         ; exit if enemy too close to top of screen (skip applying velocity)
    jsr apply_velocity                ; apply enemy's velocity to its position, removing enemy if off-screen
    dec ENEMY_DELAY,x
    .ifdef Probotector
        bne soldier_start_sprite_exit
    .else
        bne soldier_sprite_exit
    .endif

@advance_routine_exit:
    jmp advance_enemy_routine ; advance to next routine

; gets the soldier sprite: either a running soldier, or a running orian
; input
;  * x - enemy slot index
; output
;  * a - soldier sprite code
get_soldier_start_sprite:
    .ifdef Probotector
        lda ENEMY_ATTRIBUTES,x
        asl
        ldy #$1c               ; human soldier, sprite_1c
        bcc @continue
        ldy #$32               ; orian (alien), use sprite_32

    @continue:
        asl                           ; check bit 6 (Level 7 soldier)
        bcc soldier_start_sprite_exit
        ldy #$bd                      ; level 7 soldier, use sprite_bd

    soldier_start_sprite_exit:
        tya
    .else
        lda #$1c                ; human soldier sprite start
        ldy ENEMY_ATTRIBUTES,x
        bpl soldier_sprite_exit
        lda #$32                ; orian (alien) sprite start, use sprite_32
    .endif

soldier_sprite_exit:
    rts

; determines X velocity based on if there is a wall collision
; input
;  * x - enemy slot offset
; output
;  * carry flag - clear when no wall collision, or clear when wall collision,
;    but soldier can jump over collision, set when soldier has turned around
soldier_check_collision:
    lda #$10                    ; assume soldier moving to the right
    ldy ENEMY_X_VELOCITY_FAST,x ; load enemy's fast X velocity
    bpl @check_collision        ; branch if moving to the right
    lda #$f0                    ; moving left, check in fro

@check_collision:
    jsr check_bg_wall_collision
    bne @wall_collision         ; branch if wall collision
    jsr set_soldier_x_vel       ; no wall collision
                                ; set X velocity based on soldier direction, and whether soldier is jumping
    clc                         ; exit with no wall collision flag
    rts

@wall_collision:
    jsr clear_enemy_x_vel ; set enemy X velocity (fast and fractional) to 0
    lda $04               ; load bg collision offset, i.e. BG_COLLISION_DATA offset
    sec
    sbc #$10              ; move up one super-tile location on the nametable
    sta $08               ; store new background collision index in $08
    eor $04
    asl
    lda $08
    bcc @check_above
    eor #$80              ; flip bit if over/underflow into different nametable
                          ; to calculate correct BG_COLLISION_DATA offset

@check_above:
    tay
    jsr check_bg_collision_at_y      ; check bg collision at BG_COLLISION_DATA,y
    jsr is_wall_collision_use_y_reg  ; determine if collision code can be considered a wall (horizontally)
    beq @exit_collision              ; exit if no collision above, so soldier can jump over collision
    lda ENEMY_ATTRIBUTES,x           ; soldier can't jump, must turn around
    eor #$01                         ; flipping bit 0 (walk direction)
    sta ENEMY_ATTRIBUTES,x           ; flip walking direction
    jsr set_initial_facing_direction ; set new facing direction based on attributes
    jsr set_soldier_x_vel            ; set X velocity based on soldier direction, and whether soldier is jumping
    dec ENEMY_VAR_2,x                ; decrement number of collisions before auto-destroying soldier
    bne @exit_no_collision           ; exit if more bg collisions before auto-destruction
    lda #$06
    jsr set_enemy_routine            ; set routine to enemy_explosion_routine_00

@exit_no_collision:
    clc
    rts

@exit_collision:
    sec
    rts

; sets initial facing direction of enemy based on attributes bit 0
; 0 = walking left, 1 = walking right
set_initial_facing_direction:
  lda ENEMY_ATTRIBUTES,x
  lsr                    ; shift facing direction to carry flag
                         ; 0 = walking left, 1 = walking right

; set sprite attribute, based on level and facing direction of soldier
; input
;  * ENEMY_VAR_5,x - recoil timer
;  * carry flag - clear when enemy to left of player, clear otherwise
set_soldier_sprite_attr:
  php               ; push status flags on to the stack
  lda CURRENT_LEVEL ; load current level
  ldy ENEMY_VAR_4,x ; load value indicating which set of palettes to use
  beq @continue
  clc
  adc #$08          ; using secondary set of palettes

@continue:
  tay                           ; transfer current level (or current level + 8) to y
  lda soldier_sprite_attr_tbl,y ; load appropriate sprite attribute (palette)
  plp                           ; restore status flags from stack (where enemy is in relation to player)

; sets sniper, sandbag sniper, and grenade thrower sprite attribute
; input
;  * a - the sprite attribute to start with, before flipping horizontally and adding recoil
;  * x - enemy slot index
;  * ENEMY_VAR_5,x - recoil timer
;  * carry flag - set to flip sprite horizontally, clear for no horizontal flip
set_soldier_sprite_attr_with_recoil:
    bcs set_soldier_recoil
    ora #$40               ; flip sprite horizontally

; sets sandbag sniper sprite attribute
; input
;  * a - the sprite attribute to start with, before flipping horizontally and adding recoil
;  * x - enemy slot index
;  * ENEMY_VAR_5,x - recoil timer
;  * carry flag - set to flip sprite horizontally, clear for no horizontal flip
set_soldier_recoil:
    ldy ENEMY_VAR_5,x ; load the recoil timer value
    beq @continue     ; branch if no recoil
    dec ENEMY_VAR_5,x ; decrement recoil timer
    ora #$04          ; adjust sprite location for recoil effect

@continue:
    sta $00                 ; set new sprite attribute with recoil
    lda ENEMY_SPRITE_ATTR,x ; load existing sprite attribute
    and #$20                ; keep background priority
    ora $00                 ; merge new sprite attribute with recoil with existing bg priority
    sta ENEMY_SPRITE_ATTR,x ; set new sprite attribute
    rts

; initial soldier sprite attribute before any horizontal flipping or recoil
; comes from bits 3 and 4 of ENEMY_ATTRIBUTES,x (stored in ENEMY_VAR_4,x)
; when ENEMY_VAR_4,x is zero, use palette as defined by level on first row
; when ENEMY_VAR_4,x is non-zero, use palette as indexed by ENEMY_VAR_4,X from second row
soldier_sprite_attr_tbl:
    .byte $03,$03,$03,$03,$03,$03,$01,$03 ; ENEMY_VAR_4 is 0, use this row offset by level index
    .byte $01,$01,$01,$01,$01,$01,$01,$01 ; ENEMY_VAR_4 is non-zero, use value from this row

soldier_fire_bullet:
    ldy #$02                           ; enemy type = enemy bullet
    jsr try_create_enemy_from_existing ; create enemy bullet
    bcc @exit                          ; exit if unable to create bullet
    ldy ENEMY_VAR_4,x                  ; load whether soldier is crouching or not
                                       ; 1 = soldier fires standing up, 0 = crouches to fire
    lda soldier_bullet_offset_tbl+1,y  ; load y offset of soldier's bullet
    clc                                ; clear carry in preparation for addition
    adc ENEMY_Y_POS,x                  ; add to soldier y location
    sta $08                            ; set bullet initial Y position
                                       ; !(HUH) why not just set ENEMY_Y_POS,x immediately
    lda ENEMY_SPRITE_ATTR,x
    rol
    rol
    rol
    and #$01
    tay
    lda ENEMY_X_POS,x                  ; load enemy's X position
    ldx $11                            ; load created bullet enemy slot
    clc                                ; clear carry in preparation for addition
    adc soldier_bullet_offset_tbl,y    ; add bullet initial x offset from soldier
    sta ENEMY_X_POS,x                  ; set bullet X position
    lda $08
    sta ENEMY_Y_POS,x                  ; set bullet Y position
    tya
    asl
    tay
    lda soldier_bullet_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x       ; store enemy's fractional X velocity
    lda soldier_bullet_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x        ; store enemy's fast X velocity
    ldx ENEMY_CURRENT_SLOT             ; restore x to enemy slot

@exit:
    rts

soldier_bullet_x_vel_tbl:
    .byte $00,$02 ; 2
    .byte $00,$fe ; -2

soldier_bullet_offset_tbl:
    .byte $08,$f8 ; x = 8, y = -8 (standing)
    .byte $fc,$06 ; x = -4, y = 6 (crouched)

; enemy type #$04
weapon_box_routine_ptr_tbl:
    .addr weapon_box_routine_00      ; disable collision, set y offset, set frame, set delay
    .addr weapon_box_routine_01      ; animate opening and closing
    .addr weapon_box_routine_02      ; enemy destroyed routine, set destroyed supertile, create weapon item
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; disable collision, set y offset, set frame, set delay
weapon_box_routine_00:
    lda #$9d
    sta ENEMY_DESTROY_ATTRS,x ; set explosion type, collision box, disable collision
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite (uses bg tiles instead)
    lda ENEMY_ATTRIBUTES,x
    asl
    asl
    asl
    asl                       ; bit 4 specifies whether to add #$04 or #$14 to Y position
    lda #$04                  ; add #$04 to enemy Y position
    bcc @continue
    lda #$14                  ; add #$14 to enemy Y position

@continue:
    ldy #$08                  ; add #$08 to enemy X position
    jsr add_to_enemy_pos      ; add #$08 to enemy X position and y (#$04 or #$14) to enemy Y position
    lda #$20
    sta ENEMY_DELAY,x         ; set delay until being opening
    lda #$01
    sta ENEMY_FRAME,x         ; set frame to slightly open (see weapon_box_frame_tbl)
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; animate opening and closing
weapon_box_routine_01:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    cmp #$d0          ; compare Y position to bottom ~18% of screen
    bcc @continue
    jmp remove_enemy  ; remove enemy if at bottom of ~18% screen

@continue:
    jsr update_enemy_pos                 ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                    ; decrement delay until next animation frame
    bne weapon_box_exit                  ; exit if delay hasn't elapsed
    ldy ENEMY_FRAME,x                    ; delay elapsed, go to next animation frame
    lda weapon_box_frame_tbl,y           ; load correct supertile offset (see level_5_supertile_data)
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    ldy ENEMY_FRAME,x                    ; load frame index
    lda #$01                             ; set to retry updating nametable the next frame if unable to this frame
    bcs @set_delay_frame_exit            ; branch if unable to update nametable supertile
    lda ENEMY_VAR_1,x                    ; load whether or not the weapon box is open
    bne @disable_collision               ; branch if open (vulnerable) to begin closing animation
    iny                                  ; weapon box not fully open, increment frame index
    cpy #$04                             ; see if finished animating opening
    bcc @set_small_delay_frame_exit      ; branch if still opening
    dey                                  ; fully open, set frame offset to 2 (more open)
    dey
    lda #$1d
    sta ENEMY_DESTROY_ATTRS,x            ; enable bullet collision, disable player collision
                                         ; set spike explosion, set destroyed sound to sound_18 (APPEAR)
    lda #$40
    bne @toggle_status                   ; always branch to toggle open/closed flag
                                         ; and set duration of staying open

@disable_collision:
    lda #$9d                        ; starting to close, disable collision
    sta ENEMY_DESTROY_ATTRS,x       ; disable bullet collisions
    lda #$50
    dey
    bpl @set_small_delay_frame_exit
    iny
    iny

@toggle_status:
    sta ENEMY_DELAY,x   ; set delay until next animation
    lda ENEMY_VAR_1,x   ; load open/closed flag
    eor #$01
    sta ENEMY_VAR_1,x   ; slip open/closed flag
    jmp @set_frame_exit ; jump to set frame to y

@set_small_delay_frame_exit:
    lda #$08 ; opening/closing sequence, set short delay until next animation

@set_delay_frame_exit:
    sta ENEMY_DELAY,x ; set delay until next animation

@set_frame_exit:
    tya               ; transfer frame index to a
    sta ENEMY_FRAME,x ; set enemy frame index

weapon_box_exit:
    rts

; supertile offset for weapon box (see level_5_supertile_data)
weapon_box_frame_tbl:
    .byte $0e ; frame 0 - closed
    .byte $0f ; frame 1 - slightly open
    .byte $11 ; frame 2 - more open
    .byte $12 ; frame 3 - fully open

; enemy destroyed routine, set destroyed supertile, create weapon item
weapon_box_routine_02:
    lda #$0d                             ; weapon box destroyed (see level_5_supertile_data)
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    bcs weapon_box_exit
    ldy #$00                             ; enemy type = 0, weapon item
    jsr create_enemy_from_existing_enemy ; create weapon item (specific weapon item based on ENEMY_ATTRIBUTES,x)
    jmp enemy_explosion_routine_00       ; set empty sprite, play optional enemy destroyed sound, disable collisions

; enemy type #$05
grenade_thrower_routine_ptr_tbl:
    .addr grenade_thrower_routine_00 ; initialize enemy properties from attributes
    .addr grenade_thrower_routine_01 ; throw n grenades at closest enemy, delay, loop
    .addr grenade_thrower_routine_02 ; enemy destroyed routine
    .addr grenade_thrower_routine_03 ; set enemy hit animation
    .addr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize enemy properties from attributes
grenade_thrower_routine_00:
    lda ENEMY_ATTRIBUTES,x
    lsr
    lsr
    and #$03                  ; bits 2 and 3 specify number of grenades to throw
    sta ENEMY_VAR_1,x         ; set number of grenades to throw in a row
    sta ENEMY_VAR_2,x         ; initialize remaining grenades to throw
    lda #$01
    sta ENEMY_FIRING_DELAY,x  ; set throw grenade delay timer
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; throw n grenades at closest enemy, delay, loop
; every throw targets closest enemy
grenade_thrower_routine_01:
    jsr player_enemy_x_dist                 ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_X_POS,x                       ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y               ; set carry flag when enemy facing left
    .ifdef Probotector
        lda #$03                            ; set palette
    .else
        lda #$01                            ; set palette
    .endif
    jsr set_soldier_sprite_attr_with_recoil ; set whether to flip horizontally, and sprite palette
    lda ENEMY_ATTRIBUTES,x
    lsr
    lda #$27                                ; sprite_27
    bcc @set_anim_delay
    lda #$27                                ; sprite_27

@set_anim_delay:
    ldy ENEMY_DELAY,x
    beq @continue     ; don't adjust sprite if animation delay hasn't elapsed
    dec ENEMY_DELAY,x
    clc               ; clear carry in preparation for addition
    adc #$01          ; throwing grenade, set sprite to sprite_28

@continue:
    sta ENEMY_SPRITE,x                 ; either sprite_27 or sprite_28
    jsr update_enemy_pos               ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x           ; decrement throw grenade delay timer
    bne @exit                          ; exit if timer hasn't elapsed
                                       ; timer has elapsed, throw grenade
    ldy #$06                           ; enemy type = grenade
    jsr try_create_enemy_from_existing ; create grenade
    bcc @set_grenade_throw_delay       ; branch if unable to throw grenade
    lda #$0c
    sta ENEMY_DELAY,x                  ; set animation delay for grenade throw
    ldy $11                            ; load enemy slot of created grenade
    lda ENEMY_SPRITE_ATTR,x
    asl
    and #$80
    eor #$80
    ora ENEMY_ATTRIBUTES,y             ; merge with grenade attributes
    sta ENEMY_ATTRIBUTES,y             ; set grenade attributes incorporating horizontal flip from grenade thrower
    lda ENEMY_Y_POS,y                  ; load grenade position
    clc                                ; clear carry in preparation for addition
    adc #$eb                           ; subtract #$15
    sta ENEMY_Y_POS,y                  ; move grenade left of grenade thrower

@set_grenade_throw_delay:
    lda #$18                  ; delay before throwing next grenade
    dec ENEMY_VAR_2,x         ; decrement number of grenades to throw
    bpl @set_throw_delay_exit
    lda ENEMY_VAR_1,x         ; threw all grenades in group
    sta ENEMY_VAR_2,x         ; re-initialize number of grenades to throw (always 3)
    lda #$51                  ; load longer delay after throwing group of grenades

@set_throw_delay_exit:
    sta ENEMY_FIRING_DELAY,x ; set throw grenade delay timer

@exit:
    rts

; enemy destroyed routine
grenade_thrower_routine_02:
    lda #$27                             ; sprite_27 grenade thrower
    jmp set_enemy_destroy_sprite_and_vel ; set enemy sprite to sprite_27 and set destroyed velocity

; set enemy hit animation
grenade_thrower_routine_03:
    lda #$27                                  ; grenade thrower sprite
    jmp apply_gravity_adv_routine_after_delay ; set sprite to sprite_27 for fall, apply gravity
                                              ; advance the enemy routine if ENEMY_DELAY has elapsed

; enemy type #$06
grenade_routine_ptr_tbl:
    .addr grenade_routine_00         ; set grenade's collision code, and velocity based on attributes
    .addr grenade_routine_01         ; apply velocity until fall to ground
    .addr grenade_routine_02         ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set grenade's collision code, and velocity based on attributes
grenade_routine_00:
    ldy #$98               ; test player-grenade collision
    lda ENEMY_ATTRIBUTES,x
    and #$03               ; strip to grenade type
    beq @set_props         ; if bits 0 and 1 are clear test player-grenade collision
    ldy #$99               ; bits 0 and 1 are not clear, skip player-grenade collision

@set_props:
    tya
    sta ENEMY_DESTROY_ATTRS,x    ; set collision box, allow bullets to travel through grenade
                                 ; and set whether to test player-grenade collision
    lda ENEMY_ATTRIBUTES,x
    and #$03
    asl
    asl                          ; quadruple since each entry is #$04 bytes
    tay                          ; transfer to offset register
    lda grenade_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda grenade_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda grenade_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda grenade_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    lda ENEMY_ATTRIBUTES,x       ; see x direction grenade should fall
    bmi @continue                ; branch if bit 7 set, indicating the grande should fall to the left
    jsr flip_enemy_x_dir         ; grenade should fall to the right
                                 ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

@continue:
    jsr update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)

grenade_adv_routine_exit:
    jmp advance_enemy_routine ; advance to next routine

grenade_vel_tbl:
    .byte $00,$fc,$00,$ff ; y vel = -4, x vel = -1 !(UNUSED)?
    .byte $00,$ff,$c0,$fe ; y vel = -1, x vel = -1.25 (grenade thrower grenade)
    .byte $00,$fd,$c0,$ff ; y vel = -3, x vel = -0.25 (grenade generator grenade)
    .byte $00,$ff,$c0,$ff ; y vel = -1, x vel = -0.25 !(UNUSED)?

; apply velocity until fall to ground
grenade_routine_01:
    ldy #$03                       ; animation index for grenade
    jsr set_enemy_animation_sprite ; animate sprite for grenade based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr update_pos_check_offscreen ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_VELOCITY_FRACT,x   ; load enemy's fractional Y velocity
    clc                            ; clear carry in preparation for addition
    adc #$14                       ; add .078125 to Y velocity (gravity)
    sta ENEMY_Y_VELOCITY_FRACT,x   ; apply a gravity effect
    lda ENEMY_Y_VELOCITY_FAST,x    ; load enemy's fast Y velocity
    adc #$00
    sta ENEMY_Y_VELOCITY_FAST,x    ; add any overflow from fractional gravity effect
    lda ENEMY_ATTRIBUTES,x
    and #$03                       ; strip to grenade type
    tay
    lda grenade_sprite_attr_tbl,y
    ldy ENEMY_Y_VELOCITY_FAST,x    ; load enemy's fast Y velocity
    bmi @continue
    lda #$98                       ; falling down, enable player-grenade collision
    sta ENEMY_DESTROY_ATTRS,x      ; set grenade collision code, explosion type, and player-grenade collision check
    lda #$01                       ; set sprite palette, and ensure bullets travel through grenade

@continue:
    sta ENEMY_SPRITE_ATTR,x
    lda #$00
    jsr get_enemy_bg_collision_code_onscreen ; check if collided with ground (should explode)
    bne grenade_adv_routine_exit             ; advance routine if collided with ground
    rts

; grenade ENEMY_SPRITE_ATTR, specifies bg priority and palette for grenade
grenade_sprite_attr_tbl:
    .byte $01 ; !(UNUSED)?
    .byte $01 ; (grenade thrower grenade)
    .byte $21 ; (grenade generator grenade)
    .byte $01 ; !(UNUSED)?

; enemy type #$07
sandbag_sniper_routine_ptr_tbl:
    .addr sandbag_sniper_routine_00  ; initializes hp, sprite, sprite attr, and sets delay
    .addr sandbag_sniper_routine_01  ; wait and fire weapon 2 bullets every #$80 frames
    .ifdef Superc
        .addr soldier_routine_03     ; enemy destroyed routine, set collision velocity
        .addr soldier_routine_04     ; enemy falling after collision until ENEMY_DELAY elapses
    .endif
    .addr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initializes hp, sprite, sprite attr, and sets delay
sandbag_sniper_routine_00:
    lda #$05                                ; set base ENEMY_HP to #$05
    ldy #$06                                ; set enemy_hp_adjust_tbl offset to #$06
    jsr set_enemy_hp_from_a_and_y           ; set ENEMY_HP calculated using medium HP difficulty (adjusted by ENEMY_DIFFICULTY)
                                            ; sets enemy HP = #$05, #$09, or #$0d
    lda #$01
    sta ENEMY_VAR_2,x                       ; initialize bullet counter, keeps track of bullet fired to know which delay to set
    lda #$26
    sta ENEMY_SPRITE,x                      ; set enemy sprite to sprite_26
    .ifdef Probotector
        lda #$00
    .else
        lda #$01
    .endif
    jsr set_soldier_sprite_attr_with_recoil ; set whether to flip horizontally, and sprite palette
    lda #$67
    sta ENEMY_DELAY,x                       ; set animation delay
    jsr update_enemy_pos                    ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine               ; advance to next routine

; wait and fire weapon 2 bullets every #$80 frames
sandbag_sniper_routine_01:
    lda #$41               ; enemy sprite attribute value (flip sprite horizontally and set palette to 1)
    jsr set_soldier_recoil ; set sprite attribute to take into account recoil timer (ENEMY_VAR_5)
    jsr update_enemy_pos   ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x      ; decrement timer
    bne @exit              ; exit if timer has not elapsed
    lda ENEMY_X_POS,x      ; load sandbag sniper's X position
    cmp #$40               ; see if sandbag sniper is on the left 25% of the screen
    bcc @exit              ; do nothing if sandbag sniper is on left 25% of the screen
    lda #$06               ; sandbag sniper not on left edge
    sta ENEMY_VAR_5,x      ; set recoil effect timer
    jsr @fire_weapon       ; fire bullet
    lda #$0a               ; load long delay between the two bullets as default
    dec ENEMY_VAR_2,x      ; decrement variable keeping track of which bullet has fired
                           ; when decrementing from 1 to 0, fired first bullet, set short delay
                           ; when decrementing from 0 to #$ff, fired second bullet, set longer delay
    bpl @continue          ; branch if decremented from 1 to 0 to use short delay (#$0a)
    lda #$01
    sta ENEMY_VAR_2,x      ; reset bullet counter
    lda #$80               ; fired both bullets, set longer delay before next attack

@continue:
    sta ENEMY_DELAY,x ; set next attack delay

@exit:
    rts

; sandbag sniper fire weapon with velocity of -1.75 pixels/frame
@fire_weapon:
    ldy #$02                           ; enemy type = enemy bullet
    jsr try_create_enemy_from_existing ; create bullet
    bcc @fire_exit                     ; exit if unable to create bullet
    ldy $11                            ; load enemy slot of created bullet
    lda ENEMY_Y_POS,x                  ; load sandbag sniper's current Y position
    adc #$03
    sta ENEMY_Y_POS,y                  ; shift bullet location 3 pixels lower to match sniper gun location
    lda ENEMY_X_POS,x                  ; load enemy's X position
    sec                                ; set carry flag in preparation for subtraction
    sbc #$10
    sta ENEMY_X_POS,y                  ; shift bullet location left #$10 pixels to match sniper gun location
                                       ; set velocity to -1.75
    lda #$40
    sta ENEMY_X_VELOCITY_FRACT,y
    lda #$fe
    sta ENEMY_X_VELOCITY_FAST,y

@fire_exit:
    rts

; enemy type #$09
sniper_routine_ptr_tbl:
    .addr sniper_routine_00          ; initialize number of bullets, and delay timer to start firing
    .addr sniper_routine_01          ; aim to player, firing two shots with a delay between each attack
    .addr sniper_routine_02          ; enemy destroyed routine, set sprite and enemy destroyed velocity
    .addr sniper_routine_03          ; apply gravity, wait for #$10 frame delay before advancing routine
    .addr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize number of bullets, and delay timer to start firing
sniper_routine_00:
    lda ENEMY_ATTRIBUTES,x          ; load enemy attributes
    and #$03                        ; grab bits 0 and 1
    sta ENEMY_VAR_3,x               ; initialize reset value for number of bullets to fire per round of attack
    sta ENEMY_VAR_4,x               ; set remaining number of bullets to fire in current round
                                    ; note the value is one more than what is actually fired (2 bullets per attack)
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$01
    sta ENEMY_FIRING_DELAY,x        ; set delay until next firing
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to sniper_routine_01

; aim to player, firing two shots with a delay between each attack
sniper_routine_01:
    jsr player_enemy_x_dist                 ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda PLAYER_SPRITE_X_POS,y               ; load closest player X position
    cmp ENEMY_X_POS,x                       ; compare to enemy X position
    lda ENEMY_ATTRIBUTES,x                  ; load enemy attributes
    and #$20                                ; strip to just desired sprite bg priority
    ora #$01                                ; set sprite palette
    jsr set_soldier_sprite_attr_with_recoil ; set whether to flip horizontally, recoil, and sprite palette
    ldy $0a                                 ; load the closest player to the enemy (0 = p1, 1 = p2)
    lda ENEMY_Y_POS,x                       ; load sniper's Y position
    sec                                     ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_Y_POS,y               ; subtract target player's Y position
    php                                     ; push status flags on to the stack
    bcs @continue                           ; branch if player above sniper
    eor #$ff                                ; player above sniper
    adc #$01                                ; overflow, flip all bits and add one

@continue:
    cmp #$10                  ; see if target player is more than 10 pixels away vertically
    lda #$01                  ; default to using sniper firing at same level (sprite_24)
    bcc @set_frame_check_fire ; branch if player is close vertically to sniper to use horizontal firing sprite
    plp                       ; restore status flags from stack
    lda #$00                  ; assume player above sniper (sprite_23)
    bcs @save_status          ; branch if sniper below player
    lda #$02                  ; sniper above player, set sprite to #$02 (sprite_25)

@save_status:
    php ; push status flags on to the stack

@set_frame_check_fire:
    plp                       ; restore status flags from stack
    sta ENEMY_FRAME,x         ; set enemy frame (offsets from sprite_23)
    clc                       ; clear carry in preparation for addition
    adc #$23                  ; add #$23 to get actual sprite code
    sta ENEMY_SPRITE,x        ; set enemy sprite code
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_FIRING_DELAY,x  ; decrement firing delay
    bne @exit                 ; exit if firing delay hasn't elapsed
    jsr copy_enemy_vars_to_zp ; firing delay elapsed, fire bullet
                              ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$03                  ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr sniper_set_bullet_pos ; sets the position of the sniper's bullet after getting created
    lda #$0c                  ; just fired bullet, load short delay for bullets during attack
    dec ENEMY_VAR_4,x         ; decrement remaining bullets to fire
    bne @set_delay_exit       ; branch if more bullets to fire to use shorter delay
    lda ENEMY_VAR_3,x         ; fired all bullets this attack round, reset for next round
    sta ENEMY_VAR_4,x         ; set one more than number of bullets to fire per attack (always 3)
    lda #$60                  ; current attack finished, load longer delay until next attack round

@set_delay_exit:
    sta ENEMY_FIRING_DELAY,x ; set delay until next bullet is fired

@exit:
    rts

; enemy destroyed routine, set sprite and enemy destroyed velocity
sniper_routine_02:
    lda #$23                             ; standing sniper sprite
    jmp set_enemy_destroy_sprite_and_vel ; set enemy sprite to sprite_23 and set destroyed velocity

; apply gravity, wait for #$10 frame delay before advancing routine
sniper_routine_03:
    lda #$23                                  ; standing sniper sprite
    jmp apply_gravity_adv_routine_after_delay ; set sprite to sprite_23 for fall, apply gravity
                                              ; advance the enemy routine if ENEMY_DELAY has elapsed

; sets the position of the sniper's bullet after getting created
; input
;  * x - sniper enemy slot index
;  * $11 - bullet enemy slot
;  * zero flag - whether or not bullet was created (set when bullet created, clear when unable to create)
sniper_set_bullet_pos:
    bne @exit               ; exit if bullet not created
    lda #$06                ; bullet created, load recoil timer
    sta ENEMY_VAR_5,x       ; set recoil timer to #$06
    lda ENEMY_SPRITE_ATTR,x ; load sniper sprite attribute
    asl
    asl                     ; push horizontal sprite flip flag to carry
    lda ENEMY_FRAME,x       ; load which sprite the sniper is
    bcc @continue           ; branch if not flipping sprite horizontally
    adc #$02                ; sniper sprite flipped horizontally, add 3 to bullet y offset amount

@continue:
    tay                              ; transfer to offset register
    ldx $11                          ; load newly created bullet enemy slot
    lda ENEMY_Y_POS,x                ; load bullet Y position
    clc                              ; clear carry in preparation for addition
    adc sniper_bullet_y_offset_tbl,y ; add y offset based on sniper facing direction
    sta ENEMY_Y_POS,x                ; update bullet Y position
    lda ENEMY_X_POS,x                ; load enemy's X position
    clc                              ; clear carry in preparation for addition
    adc sniper_bullet_x_offset_tbl,y ; add x offset based on sniper facing direction
    sta ENEMY_X_POS,x                ; update bullet X position
    ldx ENEMY_CURRENT_SLOT           ; restore x to sniper enemy slot index

@exit:
    rts

; y amount to adjust from sniper position based on sniper facing direction
; ENEMY_FRAME [0-3] and whether sniper is facing left or right
sniper_bullet_y_offset_tbl:
    .byte $f0,$fb,$00 ; facing right
    .byte $f0,$fb,$00 ; facing left

; x amount to adjust from sniper position based on sniper facing direction
; ENEMY_FRAME [0-3] and whether sniper is facing left or right
sniper_bullet_x_offset_tbl:
    .byte $06,$07,$05 ; facing right
    .byte $fa,$f9,$fb ; facing left

; enemy type #$0a
rotating_gun_routine_ptr_tbl:
    .addr rotating_gun_routine_00    ; set to ignore player collision, set hp, set number of bullets
    .addr rotating_gun_routine_01    ; wait until within range before activating
    .addr rotating_gun_routine_02    ; run through activation animation (if level 5)
    .addr rotating_gun_routine_03    ; aim if aim delay elapsed (advances routine), if not fire if fire delay elapsed and aiming at player
    .addr rotating_gun_routine_04    ; update supertile based on aiming direction, then go back to rotating_gun_routine_03
    .addr rotating_gun_routine_05    ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set to ignore player collision, set hp, set number of bullets
rotating_gun_routine_00:
    lda #$01
    sta ENEMY_DESTROY_ATTRS,x ; disable player enemy collision
    lda #$07                  ; HP = #$07, #$0b, or #$0e
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$04
    ldy #$08
    jsr add_to_enemy_pos      ; add #$08 to enemy X position and #$04 to enemy Y position
    lda ENEMY_ATTRIBUTES,x
    and #$03
    sta ENEMY_VAR_3,x         ; set number of bullets to fire per attack
    sta ENEMY_VAR_4,x         ; initialize remaining number of bullets to fire per attack
    lda #$03
    sta ENEMY_VAR_1,x
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait until within range before activating
rotating_gun_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    cmp #$18                        ; see if on top ~9% of screen
    bcc rotating_gun_exit           ; exit if in top ~9% of screen
    lda #$08                        ; entered bottom 91% of screen, activate by advancing to next routine
    jmp set_delay_adv_enemy_routine ; set delay to #$08 and set routine to rotating_gun_routine_02

; run through activation animation (if level 5)
rotating_gun_routine_02:
    jsr update_enemy_pos                    ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ATTRIBUTES,x
    and #$08                                ; keep bit 3 (whether or not has activation animation)
    beq @set_sprite_adv_routine             ; branch if no activation animation (level 4 rotating turrets)
    dec ENEMY_DELAY,x                       ; level 5 rotating turrets, has activation animation
    bne rotating_gun_exit                   ; exit if delay for frame of activation animation hasn't elapsed
    ldy ENEMY_FRAME,x                       ; delay for updating frame elapsed, load next frame
    lda rotating_gun_activating_frame_tbl,y ; load supertile offset for activating animation
    jsr update_enemy_nametable_supertile    ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    lda #$01
    bcs @set_delay_exit
    inc ENEMY_FRAME,x                       ; move to next supertile index
    lda ENEMY_FRAME,x                       ; load supertile index
    cmp #$03                                ; see if showed all frames of animation
    lda #$0c                                ; load activated initial supertile index
    bcc @set_delay_exit                     ; exit if more frames to show in activating animation

@set_sprite_adv_routine:
    lda #$01
    sta ENEMY_SPRITE,x              ; set invisible sprite (uses bg tiles instead)
    lda #$04
    sta ENEMY_FIRING_DELAY,x        ; set delay until next bullet is fired
    lda #$20
    jmp set_delay_adv_enemy_routine ; set delay to #$20 and set routine to rotating_gun_routine_03

@set_delay_exit:
    sta ENEMY_DELAY,x

rotating_gun_exit:
    rts

; update pos, see if should remove based on position
; check aim delay (aim if delay elapsed)
; if aim delay hasn't elapsed, check firing delay (fire if elapsed and aiming at player)
rotating_gun_routine_03:
    jsr update_enemy_pos   ; adjust position based on scroll (does not apply velocity)
    ldy ENEMY_Y_POS,x      ; load enemy's Y position
    cpy #$18               ; see if enemy in top ~9% of the screen
    bcc rotating_gun_exit  ; exit if enemy in top portion of screen
    cpy #$d8               ; see if in bottom ~16% of screen
    bcs @remove_enemy      ; branch if enemy has scrolled to bottom to remove
    lda ENEMY_ATTRIBUTES,x ; enemy not in bottom portion, see if should consider 'early removal'
    and #$10               ; see if enemy configured to remove when in bottom 37.5% instead of bottom ~16%
    beq @continue          ; branch if attribute bit 4 is clear
    cpy #$a0               ; attribute bit 4 is set 'early removal', compare Y position to bottom 62.5% of screen
    bcs @remove_enemy      ; remove enemy if in bottom 37.5% of screen

@continue:
    jsr player_enemy_x_dist      ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    sty $12                      ; store closest player index in $12
    dec ENEMY_DELAY,x            ; decrement aiming delay
    bne rotating_gun_fire        ; branch if firing delay elapsed to fire bullet at player (if already aiming at player)
    lda #$14                     ; aiming delay has elapsed, aim towards player
    sta ENEMY_DELAY,x            ; set next aiming delay to #$14
    jsr rotate_aim_dir_00_zp     ; increment or decrement ENEMY_VAR_1 by 1 to aim towards the player
                                 ; using quadrant_aim_dir_00
    bcs rotating_gun_fire_bullet ; if no need to rotate, branch to fire bullet if firing delay has elapsed
    jmp advance_enemy_routine    ; need to rotate to aim to player, advance routine to rotating_gun_routine_04
                                 ; to update supertile the next frame

@remove_enemy:
    jmp remove_enemy

; update supertile based on aiming direction
rotating_gun_routine_04:
    jsr update_enemy_pos                      ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x                         ; load enemy's Y position
    cmp #$18                                  ; see if in top ~9% of screen
    bcc rotating_gun_exit                     ; exit if in top ~9% of screen
    cmp #$c0                                  ; see if in bottom 75% of screen
    bcs rotating_gun_exit                     ; exit if in bottom 75% of screen
    jsr rotating_gun_update_supertile_for_dir ; in main part of screen (10%-75%)
                                              ; set appropriate supertile based on aiming direction
    bcs rotating_gun_exit2                    ; branch to exit if unable to update supertile to try again next frame
    lda #$04                                  ; updated supertile
    jmp set_enemy_routine                     ; set routine to rotating_gun_routine_03

; enemy destroyed routine
rotating_gun_routine_05:
    ldy #$0c                          ; rotating gun destroyed supertile (see level_4_supertile_data)
    jsr rotating_gun_update_supertile
    bcs @update_enemy_pos             ; branch if unable to update supertile to try again next frame
    jmp enemy_explosion_routine_00    ; begin explosion animation

@update_enemy_pos:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; fire bullet at player if aiming at player and firing delay elapsed
rotating_gun_fire:
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_00         ; get enemy aim direction and rotation direction using quadrant_aim_dir_00
    bpl rotating_gun_exit2    ; branch to exit if not aiming at player

; decrements delay and fires bullet if delay has elapsed, sets next delay after firing
rotating_gun_fire_bullet:
    dec ENEMY_FIRING_DELAY,x           ; decrement firing delay
    bne rotating_gun_exit2             ; exit if firing delay hasn't elapsed
    ldy $12                            ; load closest player index
    sty $0a                            ; store closest player to rotating gun in $0a
    jsr copy_enemy_vars_to_zp          ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$02                           ; set bullet speed code to normal 1x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player          ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr rotating_gun_adjust_bullet_pos ; adjust created bullet's initial position based on direction
    lda #$10                           ; shorter delay between bullet firings is #$10 frames
    dec ENEMY_VAR_4,x                  ; decrement remaining number of bullets to fire this round of attack
    bne @set_delay_exit                ; branch to set short delay and exit if more bullets to fire
    lda ENEMY_VAR_3,x                  ; fired all bullets this round of attack, load number of bullets to fire next attack round
    sta ENEMY_VAR_4,x                  ; set remaining number of bullets to fire for next round of attack
    lda RANDOM_NUM                     ; load random number
    and #$0f                           ; strip random number to be between #$00 and #$0f
    adc #$47                           ; add random number [#$00-#$ff] to #$47 for longer attack delay

@set_delay_exit:
    sta ENEMY_FIRING_DELAY,x ; set delay until next bullet is fired

rotating_gun_exit2:
    rts

; adjust newly created bullet's X and Y position based on its firing direction
; used by gray turret and rotating gun
; input
;  * $11 - created bullet enemy slot index
;  * zero flag - set when new bullet created, clear when unable to create
rotating_gun_adjust_bullet_pos:
    bne @exit                             ; exit if rotating gun/gray turret unable to create bullet
    ldy ENEMY_VAR_1,x
    ldx $11                               ; load created bullet slot index
    lda ENEMY_Y_POS,x                     ; load bullet's Y position
    clc                                   ; clear carry in preparation for addition
    adc rotating_enemy_bullet_y_adj_tbl,y ; add bullet adjustment
    sta ENEMY_Y_POS,x                     ; update bullet's Y position
    lda ENEMY_X_POS,x                     ; load bullet's X position
    clc                                   ; clear carry in preparation for addition
    adc rotating_enemy_bullet_x_adj_tbl,y ; add bullet adjustment
    sta ENEMY_X_POS,x                     ; update bullet's X position
    ldx ENEMY_CURRENT_SLOT                ; restore x to the current enemy slot index

@exit:
    rts

rotating_enemy_bullet_y_adj_tbl:
    .byte $00,$07,$0e,$10,$0e,$07,$00,$f9,$f2,$f0,$f2,$f9

rotating_enemy_bullet_x_adj_tbl:
    .byte $10,$0e,$07,$00,$f9,$f2,$f0,$f2,$f9,$00,$07,$0e

; updates the nametable supertile for the rotating gun based on aim direction
rotating_gun_update_supertile_for_dir:
    ldy ENEMY_VAR_1,x ; load enemy aim direction

; updates the supertile for the rotating gun
; input
;  * y - rotating_gun_dir_supertile_tbl offset, specifies nametable to update (see level_4_supertile_data)
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
rotating_gun_update_supertile:
    lda rotating_gun_dir_supertile_tbl,y
    jmp update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)

; used for animating the activation of rotating gun (level 5)
rotating_gun_activating_frame_tbl:
    .byte $0f,$10,$01

; offset into see level_4_supertile_data
rotating_gun_dir_supertile_tbl:
    .byte $0a,$0b,$0c,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0d

; enemy type #$0b
gray_turret_routine_ptr_tbl:
    .addr gray_turret_routine_00     ; initialize variables HP, collision, number of bullets to fire, aim direction, etc.
    .addr gray_turret_routine_01     ; wait for player proximity to activate
    .addr gray_turret_routine_02     ; animate activation
    .addr gray_turret_routine_03     ; aim if aim delay elapsed (advances routine), if not fire if fire delay elapsed and aiming at player
    .addr gray_turret_routine_04     ; update supertile based on aiming direction, then go back to gray_turret_routine_03
    .addr gray_turret_routine_05     ; enemy destroyed routine, update supertiles, begin explosion sequence (enemy_explosion_routine_00)
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize variables HP, collision, number of bullets to fire, aim direction, etc.
gray_turret_routine_00:
    lda #$08                  ; HP = #$08, #$0c, or #$0f
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$04
    ldy #$08
    jsr add_to_enemy_pos      ; add #$08 to enemy X position and #$04 to enemy Y position
    lda ENEMY_ATTRIBUTES,x
    lsr                       ; push facing direction to carry (0 = right, 1 = left)
    and #$03                  ; strip to just the number of bullets
    sta ENEMY_VAR_3,x         ; set number of bullets to fire per attack
    sta ENEMY_VAR_4,x         ; set remaining number of bullets to fire per attack
    lda #$00                  ; set initial aim direction to #$06 (3 o'clock)
    bcc @continue             ; branch if facing right
    lda #$06                  ; facing left, set initial aim direction to #$06 (9 o'clock)

@continue:
    sta ENEMY_VAR_1,x         ; set initial aim direction
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for player proximity to activate
gray_turret_routine_01:
    lda #$01
    sta ENEMY_SPRITE,x                     ; set invisible sprite
    jsr update_enemy_pos                   ; adjust position based on scroll (does not apply velocity)
    jsr gray_turret_find_activation_player ; y = targeted player (#$00 or #$01)
    lda ENEMY_X_POS,x                      ; load enemy's X position
    sec                                    ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_X_POS,y              ; compare enemy X position to target player X position
    bcs @continue                          ; branch if player to left of enemy
    eor #$ff                               ; player to right of gray turret
    adc #$01

@continue:
    cmp #$60                   ; see if player is within #$60 pixels of enemy horizontally
    bcs gray_turret_exit       ; branch to not activate if player is far away
    lda ENEMY_Y_POS,x          ; load enemy's Y position
    sec                        ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_Y_POS,y  ; compare enemy Y position to target player Y position
    bcs @set_delay_adv_routine ; branch if player above enemy
    eor #$ff                   ; player below enemy
    adc #$01

@set_delay_adv_routine:
    cmp #$28                        ; see if player is within #$28 of enemy vertically
    bcs gray_turret_exit            ; branch to not activate if player is far away
    lda #$01                        ; turret should activate, advance routine
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to gray_turret_routine_02

gray_turret_exit:
    rts

; animate activation
gray_turret_routine_02:
    jsr update_enemy_pos   ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x      ; decrement animation delay
    bne gray_turret_exit   ; exit if animation delay hasn't elapsed
    lda ENEMY_ATTRIBUTES,x ; animation delay elapsed, load attributes
    lsr
    lda #$00               ; assume facing right
    ldy #$4d               ; load second supertile offset #$4d (rotating gun ground supertile)
    bcc @continue          ; branch if facing right
    lda #$02               ; facing left, use different rotating gun ground supertile
    ldy #$44               ; load second supertile offset #$44 (rotating gun ground supertile)

@continue:
    sty $0c                                    ; set second supertile offset, e.g. index into level_5_supertile_data offset
    adc ENEMY_FRAME,x                          ; add base index (0 = right or 2 = left) with frame offset
    tay                                        ; transfer to offset register
    lda gray_turret_activation_supertile_tbl,y ; load activating rotating gun supertile
    ldy #$10                                   ; x offset = #$08, y offset = #$18 (one half supertile below)
    jsr load_banks_update_enemy_supertiles     ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y
    lda #$01
    bcs @set_delay_exit                        ; exit if unable to update supertile to try again next frame
    inc ENEMY_FRAME,x                          ; updated supertile, increment frame index
    lda ENEMY_FRAME,x                          ; load frame index
    cmp #$03                                   ; see if past last animation frame
    lda #$0c                                   ; assume not finished animating, set next frame update to #$0c frames
    bcc @set_delay_exit                        ; branch if more frames to animate
    lda #$01                                   ; drawn all frames
    sta ENEMY_DESTROY_ATTRS,x                  ; disable player-enemy collision
    jsr advance_enemy_routine                  ; move to next routine
    lda #$20
    sta ENEMY_FIRING_DELAY,x                   ; set delay until first bullet is fired
    lda #$08

@set_delay_exit:
    sta ENEMY_DELAY,x ; set animation delay

gray_turret_exit2:
    rts

; aim if aim delay elapsed (advances routine), if not fire if fire delay elapsed and aiming at player
gray_turret_routine_03:
    lda ENEMY_Y_POS,x ; load enemy's Y position
    cmp #$d0          ; compare enemy position to bottom 18.75% of screen
    bcc @continue     ; branch if not in bottom bottom 18.75% of screen
    jmp remove_enemy  ; enemy in bottom 18.75% of screen, remove it

@continue:
    jsr update_enemy_pos                  ; adjust position based on scroll (does not apply velocity)
    jsr player_enemy_y_dist               ; a = closest y distance to enemy from players, y = closest player (#$00 or #$01)
    sty $12                               ; store closest player index in $12
    dec ENEMY_DELAY,x                     ; decrement aiming delay
    bne gray_turret_fire_if_ready         ; branch if aiming delay not elapsed
                                          ; to fire if already aiming at closest player ($0a) and firing delay has elapsed
    lda #$15                              ; aiming delay has elapsed, aim towards closest player
    sta ENEMY_DELAY,x                     ; store delay until aim again
    jsr copy_enemy_vars_to_zp             ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_00                     ; get enemy aim direction and rotation direction using quadrant_aim_dir_00
    bmi gray_turret_fire_if_delay_elapsed ; branch if already aiming at player to fire if delay has elapsed
                                          ; not aiming at player
    php                                   ; push status flags on to the stack
    lda ENEMY_ATTRIBUTES,x                ; load enemy attributes
    and #$01                              ; strip to facing direction (0 = right, 1 = left)
    tay                                   ; transfer to offset register
    lda ENEMY_VAR_1,x                     ; load aim direction (0 = 3 o'clock, #$0b = 2 o'clock, ascends clockwise)
    plp                                   ; restore status flags from stack
    bne @aim_counterclockwise             ; branch if aim direction is counterclockwise (a = #$01)
    cmp gray_turret_c_aim_limit_tbl,y     ; aim direction is clockwise, see if can increase aim dir
    beq gray_turret_exit2                 ; branch if gray turret can't aim any more clockwise
    inc ENEMY_VAR_1,x                     ; can update aim, increment aim direction (clockwise)
    lda ENEMY_VAR_1,x                     ; load aim direction (0 = 3 o'clock, #$0b = 2 o'clock, ascends clockwise)
    cmp #$0c                              ; see if out of bounds of valid aiming direction [0-#$0b]
    bcc @set_dir_adv_routine              ; branch if aiming direction is valid to set dir and advance routine
    lda #$00                              ; aiming direction invalid, just set 0 (3 o'clock)

@set_dir_adv_routine:
    jmp @set_aim_dir_adv_routine

@aim_counterclockwise:
    cmp gray_turret_cc_aim_limit_tbl,y ; aim direction is counterclockwise, see if can increase aim dir
    beq gray_turret_exit2              ; branch if gray turret can't aim any more counterclockwise
    dec ENEMY_VAR_1,x                  ; decrement aim direction
    bpl @advance_routine               ; branch if valid aim direction [00-#$0b]
    lda #$0b                           ; aiming direction invalid, just set #$0b (2 o'clock)

@set_aim_dir_adv_routine:
    sta ENEMY_VAR_1,x ; set aim direction (0 = 3 o'clock, #$0b = 2 o'clock, ascends clockwise)

@advance_routine:
    jmp advance_enemy_routine ; advance to next routine

; most extreme direction for clockwise aim changes
gray_turret_c_aim_limit_tbl:
    .byte $00 ; facing right (3 o'clock)
    .byte $08 ; facing left (11 o'clock)

; most extreme direction for counter-clockwise aim changes
gray_turret_cc_aim_limit_tbl:
    .byte $0a ; facing right (1 o'clock)
    .byte $06 ; facing left (9 o'clock)

; updates supertile based on aim direction
gray_turret_routine_04:
    jsr update_enemy_pos                 ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    cmp #$18                             ; compare to top ~9% of screen
    bcc gray_turret_exit3                ; exit if in top ~9% of screen
    cmp #$c0                             ; compare to bottom 25% of screen
    bcs gray_turret_exit3                ; exit if in bottom 25% of screen
    ldy ENEMY_VAR_1,x                    ; load aim direction (0 = 3 o'clock, #$0b = 2 o'clock, ascends clockwise)
    lda gray_turret_supertile_tbl,y      ; load correct supertile based on aiming direction
    jsr update_enemy_nametable_supertile ; draw supertile a at current enemy position (ENEMY_X_POS,ENEMY_Y_POS)
    bcs gray_turret_exit3                ; exit if unable to update supertile to try again next frame
    lda #$04
    jmp set_enemy_routine                ; set routine back to gray_turret_routine_03

; enemy destroyed routine, update supertiles, begin explosion sequence (enemy_explosion_routine_00)
gray_turret_routine_05:
    lda ENEMY_ATTRIBUTES,x
    lsr                    ; push facing direction to carry
    lda #$4e               ; gray turret ground destroyed supertile
    bcc @set_supertiles    ; branch if facing right
    lda #$45               ; facing left, use gray turret ground destroyed supertile

@set_supertiles:
    sta $0c                                ; set second supertile offset (ground destroyed)
    lda #$00                               ; load blank supertile (gray turret destroyed)
    ldy #$10                               ; x offset = #$08, y offset = #$18 (one half supertile below)
    jsr load_banks_update_enemy_supertiles ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y
    bcs @exit                              ; branch if unable to set supertile to try again next frame
    jmp enemy_explosion_routine_00         ; set empty sprite, play optional enemy destroyed sound, disable collisions

@exit:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; fire if aiming at closest player ($0a) and firing delay has elapsed
gray_turret_fire_if_ready:
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_00         ; get enemy aim direction and rotation direction using quadrant_aim_dir_00
    bpl gray_turret_exit3     ; branch if need to rotate

gray_turret_fire_if_delay_elapsed:
    dec ENEMY_FIRING_DELAY,x           ; decrement firing delay
    bne gray_turret_exit3              ; exit if firing delay hasn't elapsed
    ldy $12                            ; load closest player index
    sty $0a                            ; store in $0a to be fired at
    jsr copy_enemy_vars_to_zp          ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$02                           ; set bullet speed code to normal 1x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player          ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    jsr rotating_gun_adjust_bullet_pos ; adjust created bullet's initial position based on direction
    lda #$10
    dec ENEMY_VAR_4,x                  ; decrement remaining number of bullets to fire this round of attack
    bne @set_fire_delay_exit           ; branch if more bullets to fire to use shorter delay
    lda ENEMY_VAR_3,x                  ; fired all bullets, load number of bullets to fire per attack
    sta ENEMY_VAR_4,x                  ; set remaining number of bullets to fire this round of attack
    lda #$60                           ; fired all bullets, load longer delay until next bullet is fired

@set_fire_delay_exit:
    sta ENEMY_FIRING_DELAY,x ; set delay until next bullet is fired

gray_turret_exit3:
    rts

; level_5_supertile_data offsets for rotating gun activating
gray_turret_activation_supertile_tbl:
    .byte $4f ; frame 0 (facing right)
    .byte $50 ; frame 1 (facing right)
    .byte $51 ; frame 2 (facing right)
    .byte $48 ; frame 0 (facing left)
    .byte $49 ; frame 1 (facing left)
    .byte $4a ; frame 2 (facing left)

; level_5_supertile_data offsets for rotating gun depending on aiming direction
gray_turret_supertile_tbl:
    .byte $51 ; 3 o'clock
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $4a ; 9 o'clock
    .byte $4b ; 10 o'clock
    .byte $4c ; 11 o'clock
    .byte $00
    .byte $53 ; 1 o'clock
    .byte $52 ; 2 o'clock

; enemy type #$0c
enemy_explosion_routine_ptr_tbl:
    .addr enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; enemy type #$0d
enemy_door_routine_ptr_tbl:
    .addr enemy_door_routine_00      ; initialize sprite, hp, destroyed sound, and position
    .addr enemy_door_routine_01      ; wait for activation, then enable bullet collisions
    .addr enemy_door_routine_02      ; update position based on scroll
    .addr enemy_door_routine_03      ; enemy destroyed routine - initialize ENEMY_VAR_4 for 5 explosions
    .addr enemy_routine_explosions   ; generate 5 explosions, with an #$08 frame delay between each one
    .addr enemy_door_routine_05
    .addr enemy_door_routine_06
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; initialize sprite, hp, destroyed sound, and position
enemy_door_routine_00:
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite (uses bg tiles instead)
    lda #$10
    sta ENEMY_HP,x            ; set door HP to #$10
    lda #$89                  ; disable player collision and bullet collision
    sta ENEMY_DESTROY_ATTRS,x ; set sound code when destroyed to sound_1c
    lda ENEMY_ATTRIBUTES,x
    and #$01                  ; strip to just the level bit
    sta ENEMY_ATTRIBUTES,x
    lda ENEMY_X_POS,x         ; load enemy's X position
    adc #$08
    sta ENEMY_X_POS,x         ; add #$08 to door X position
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for activation, then enable bullet collisions
enemy_door_routine_01:
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_Y_POS,x         ; load enemy's Y position
    cmp #$20
    bcc @exit                 ; exit if still in top 12.5% of screen
    lda #$09                  ; door scrolled into main part of screen, enable collisions
    sta ENEMY_DESTROY_ATTRS,x ; enable bullet-enemy collision
                              ; keeping sound code when destroyed as sound_1c
    jmp advance_enemy_routine ; advance to next routine

@exit:
    rts

; update position based on scroll
enemy_door_routine_02:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

enemy_door_routine_05:
    jsr update_enemy_pos                 ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_ATTRIBUTES,x               ; load which level the door is on
    bne @level_6_door                    ; branch if level 6 (Entry to HQ)
    .ifdef Superc
        lda #$ff                         ; level 2 (base area 2)
        sta SUPERTILE_ATTRIBUTE_OVERRIDE ; set door destroyed flag
        ldy #$05
        lda #$55

    ; overwrite supertiles #$89,#$8d,#$ac,#$af,#$c1,#$c2 attribute bytes with #$55
    @loop:
        sta ATTRIBUTE_OVERRIDES,y
        dey
        bpl @loop
    .endif
    ldy #$20

@bg_collision_loop:
    lda #$00
    sta BG_COLLISION_DATA+3,y
    sta BG_COLLISION_DATA+4,y
    tya
    sec                       ; set carry flag in preparation for subtraction
    sbc #$08
    tay
    bpl @bg_collision_loop
    bmi @continue             ; always branch

@level_6_door:
    ldy #$10

@level_6_loop:
    lda #$10
    sta $044b,y
    lda #$01
    sta $044c,y
    tya
    sec               ; set carry flag in preparation for subtraction
    sbc #$08
    tay
    bpl @level_6_loop

@continue:
    lda ENEMY_ATTRIBUTES,x ; load which level the door is on
    lsr                    ; push level bit to the carry
    bcs @update_x_pos      ; branch if level 6 (Entry to HQ) to only adjust X position
    lda ENEMY_Y_POS,x      ; level 2 (base area 2), load enemy's Y position
    adc #$10
    sta ENEMY_Y_POS,x      ; push door #$10 pixels down

@update_x_pos:
    lda ENEMY_X_POS,x         ; load enemy's X position
    sbc #$10
    sta ENEMY_X_POS,x         ; subtract #$10 from X position
    lda #$02
    sta ENEMY_VAR_1,x         ; initialize supertile replace index for destruction, #$02 down to #$00
    jmp advance_enemy_routine ; advance to next routine

; updates supertiles to be destroyed door, #$02 at a time, then creates explosions
enemy_door_routine_06:
    lda ENEMY_ATTRIBUTES,x ; load which level the door is on
    beq @continue          ; branch if level 2 (base area 2)
    lda #$03

@continue:
    clc                                    ; clear carry in preparation for addition
    adc ENEMY_VAR_1,x                      ; add current supertile to update [#$00-#$02]
    tay                                    ; transfer to offset register
    lda door_destroyed_supertile_tbl,y     ; load supertile to be used for destroyed door
    beq @begin_explosions                  ; branch if finished updated background supertiles
    sta $0c                                ; store supertile index for load_banks_update_enemy_supertiles
    lda door_destroyed_supertile_tbl2,y    ; load second supertile to be used for destroyed door
    ldy #$01                               ; x offset = #$18, y offset = #$08 (supertile to the right)
    jsr load_banks_update_enemy_supertiles ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y
    bcs @exit

@begin_explosions:
    lda ENEMY_Y_POS,x              ; load enemy's Y position
    sbc #$1f
    sta ENEMY_Y_POS,x
    dec ENEMY_VAR_1,x
    bpl @exit
    lda ENEMY_Y_POS,x              ; load enemy's Y position
    adc #$50
    sta ENEMY_Y_POS,x
    lda ENEMY_X_POS,x              ; load enemy's X position
    adc #$10
    sta ENEMY_X_POS,x
    jmp enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

@exit:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

door_destroyed_supertile_tbl2:
    .byte $6c,$6d,$c3 ; level 2 (indexes into level_2_supertile_data)
    .byte $00,$70,$74 ; level 6 (indexes into level_6_supertile_data)

door_destroyed_supertile_tbl:
    .byte $c5,$c6,$c4 ; level 2 (indexes into level_2_supertile_data)
    .byte $00,$71,$75 ; level 6 (indexes into level_6_supertile_data)

; enemy type #$0f
grenade_gen_routine_ptr_tbl:
    .addr grenade_gen_routine_00
    .addr grenade_gen_routine_01 ; enemy destroyed routine (doesn't destroy enemy), wait to active
    .addr grenade_gen_routine_02 ; wait for delay, launch grenade, determine next delay, remove if far to the left

grenade_gen_routine_00:
    lda #$01
    sta ENEMY_SPRITE,x        ; set sprite to sprite_01 (blank)
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$f0                  ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda ENEMY_ATTRIBUTES,x
    and #$03
    sta ENEMY_VAR_1,x         ; set delay after launching set of grenades
    sta ENEMY_VAR_2,x         ; set delay between grenade creation
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; enemy destroyed routine (doesn't actually destroy enemy)
; wait to activate, then advance routine
grenade_gen_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x               ; load enemy's X position
    cmp #$e0
    bcs grenade_gen_exit            ; do nothing if generator far to right of screen
    lda #$01                        ; grenade generator in range, activate by going to next routine
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to grenade_gen_routine_02

; wait for delay, launch grenade, determine next delay, remove if far to the left
grenade_gen_routine_02:
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x    ; load enemy's X position
    cmp #$20             ; compare to left ~12.5% of screen
    bcs @continue
    jmp remove_enemy     ; remove enemy if on far left of screen

@continue:
    dec ENEMY_DELAY,x                  ; decrement grenade generation delay
    bne grenade_gen_exit               ; exit if grenade generation delay hasn't elapsed
    ldy #$06                           ; enemy type = grenade
    jsr try_create_enemy_from_existing ; create grenade
    bcc @determine_delay               ; branch if unable to create grenade
    ldy $11                            ; created grenade, load slot where grenade was created
    lda ENEMY_ATTRIBUTES,x             ; load grenade generator attributes
    asl
    asl
    and #$80                           ; set grenade fall direction (left or right)
                                       ; 1 = fall to the left, 0 = fall to the right
    ora #$02                           ; set grenade type to #$02
    sta ENEMY_ATTRIBUTES,y             ; set grenade's enemy fall direction and grenade type

@determine_delay:
    lda #$18            ; load delay between grenade launches
                        ; when multiple grenades are launch per attack
    dec ENEMY_VAR_2,x   ; decrement number of grenades to generate in attack
    bpl @set_delay_exit ; branch if more grenades to launch in attack
    lda ENEMY_VAR_1,x   ; launched all grenades in current attack,
                        ; load number of grenades to launch per attack
    sta ENEMY_VAR_2,x   ; reinitialize number of grenades to launch for next attack
    lda #$63            ; set longer delay after having launched all grenades in attack

@set_delay_exit:
    sta ENEMY_DELAY,x ; set delay before next grenade generation

grenade_gen_exit:
    rts

; enemy type #$10
soldier_gen_routine_ptr_tbl:
    .addr soldier_gen_routine_00 ; initialize ENEMY_HP, adjust X position
    .addr soldier_gen_routine_01
    .addr soldier_gen_routine_02 ; repeatedly generate 2 soldiers per generation round with longer delay in between
    .addr remove_enemy           ; enemy destroyed routine

; initialize ENEMY_HP, adjust X position
soldier_gen_routine_00:
    lda #$f0                  ; a = #$f0 (special HP)
    sta ENEMY_HP,x            ; set special HP not destroyed by destroy_all_enemies routine
    lda ENEMY_X_POS,x         ; load enemy's X position
    sec                       ; set carry flag in preparation for subtraction
    sbc #$04                  ; subtract 4 from enemy soldier generator location
    sta ENEMY_X_POS,x         ; update soldier generator location
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

soldier_gen_routine_01:
    jsr update_enemy_pos                 ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x                    ; load enemy's X position
    cmp #$e8                             ; compare to rightmost ~90% of the screen
    bcs soldier_gen_exit                 ; branch to exit if generator is far to the right (not yet enabled)
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    clc                                  ; clear carry in preparation for addition
    adc #$12                             ; calculate generator position Y position offset by #$12
    tay
    lda ENEMY_X_POS,x                    ; load enemy's X position
    sbc #$10
    jsr get_bg_collision                 ; get background collision code for position (a,y)
    lda $03
    and #$01
    asl
    tax
    lda soldier_gen_bg_collision_tbl,x
    sta BG_COLLISION_DATA,y
    lda soldier_gen_bg_collision_tbl+1,x
    sta BG_COLLISION_DATA+1,y
    ldx ENEMY_CURRENT_SLOT
    lda #$01
    sta ENEMY_VAR_1,x
    lda #$01
    jmp set_delay_adv_enemy_routine      ; set delay to #$01 and set routine to soldier_gen_routine_02

soldier_gen_bg_collision_tbl:
    .byte $11,$10,$01,$11

; if player not too close, repeatedly generate 2 soldiers per generation round with longer delay in between
soldier_gen_routine_02:
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne soldier_gen_exit      ; exit if soldier creation delay hasn't elapsed
    jsr player_enemy_x_dist   ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    ldy $0a                   ; load closest player index
    lda ENEMY_X_POS,x         ; load enemy's X position
    sec                       ; set carry flag in preparation for subtraction
    sbc PLAYER_SPRITE_X_POS,y ; subtract player X position from enemy X position
    bcs @get_enemy_attr       ; branch if no overflow, player to left of soldier generator
    eor #$ff
    adc #$01                  ; overflow, flip all bits and add 1
    clc

@get_enemy_attr:
    ldy #$81      ; set facing direction to walk to the right
    bcc @continue ; branch if player to right of soldier generator
    ldy #$00      ; player to left of soldier generator, set facing direction to walk to the left

@continue:
    sty $12                            ; set generated soldier enemy attribute (facing direction)
    cmp #$18                           ; see if player within #$18 of the soldier generator
                                       ; don't generate soldier when close to generator horizontally
    bcc @set_delay                     ; branch if player close to generator
    ldy #$03                           ; player not super close to generator, create soldier enemy type = soldier
    jsr try_create_enemy_from_existing ; create soldier
    bcc @set_delay                     ; branch if unable to create soldier
    lda ENEMY_ATTRIBUTES,x             ; created soldier, load generator attributes
    and #$20                           ; keep bit 5 (bg priority)
    ldy $11                            ; load enemy slot where soldier was created
    sta ENEMY_SPRITE_ATTR,y            ; set soldier sprite attribute (bg priority)
    lda $12                            ; load generated soldier's enemy attributes (walking direction)
    sta ENEMY_ATTRIBUTES,y             ; set generated soldier's walking direction

@set_delay:
    lda #$20            ; set delay between soldier generation
    dec ENEMY_VAR_1,x   ; decrement remaining number of soldiers to generate
    bpl @set_delay_exit ; branch if still another soldier to generate to use a #$20 frame delay
    lda #$01            ; finished generating all soldiers this attack
                        ; load value to indicate generate 2 soldiers for the next round
    sta ENEMY_VAR_1,x   ; set one less than number of soldiers to generate for next round of generation
    lda #$80            ; set longer delay, this is the delay before next round of soldier generation

@set_delay_exit:
    sta ENEMY_DELAY,x ; set delay until next soldier is generated

soldier_gen_exit:
    rts

; !(UNUSED)
; get PPU position for enemy X position
bank_3_unused_02:
    ldy ENEMY_Y_POS,x ; load enemy's Y position
    lda ENEMY_X_POS,x ; load enemy's X position

; robot spider
; input
;  * a - X position
;  * y - Y position
; output
;  * $(00) - 2 byte PPU address
robot_spider_get_ppu_addr:
    sta $03       ; set X position
    lda #$08
    sta $01
    tya           ; transfer Y position to a
    clc           ; clear carry in preparation for addition
    adc Y_SCROLL  ; add vertical scroll
    bcs @overflow
    cmp #$f0
    bcc @continue

@overflow:
    adc #$0f

@continue:
    and #$f8
    asl
    rol $01
    asl
    rol $01
    sta $02
    lda $03
    clc          ; clear carry in preparation for addition
    adc X_SCROLL ; add PPU horizontal scroll
    lsr
    lsr
    lsr
    tay
    ora $02
    sta $00      ; set PPU address low byte
    rts

; determine background collision at point (a,y)
; accounts for incline allowing player to walk up/down incline
; input
;  * a - X position
;  * y - Y position
; output
;  * a - background collision code
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;  * $04 - bg collision offset, i.e. BG_COLLISION_DATA offset (same as y)
;  * zero flag - set when no collision, clear otherwise
;  * carry - set when collision
get_bg_collision_code:
    jsr get_bg_collision              ; get background collision code for position (a,y)
    cmp #$06
    tay                               ; transfer collision code to y
    bcc @exit                         ; exit if not an incline bg collision
    lda $01                           ; incline bg collision
    and #$0f
    cmp incline_collision_pos_tbl-6,y
                                      ; includes how many bytes is between @get_collision_val and incline_collision_pos_tbl (#$06)
    bcs @continue
    lda #$00                          ; no collision
    rts

@continue:
    tya

@exit:
    rts

; determines background collision code for position (a,y)
; this method is IRQ-aware and will handle using correct tiles for calculations
; input
;  * a - X position
;  * y - Y position
; output
;  * a - background collision code
;  * y - bg collision offset, i.e. BG_COLLISION_DATA offset (same as $04)
;  * $00 - input X test position
;  * $01 - Y offset within nametable accounting for Y scroll
;  * $02
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;  * $04 - bg collision offset, i.e. BG_COLLISION_DATA offset (same as y)
;  * $06 - input Y test position
;  * zero flag - set when no collision, clear otherwise
get_bg_collision:
    sta $00                                ; store X position in $00
    sty $06                                ; store Y position in $06
    .ifdef Probotector
        lda IRQ_TYPE
        beq @continue_pre_irq
        cpy SCANLINE_IRQ_2                 ; compare Y position to scanline where 2nd irq happens
    .else
        cpy SCANLINE_IRQ_1                 ; compare Y position to scanline where 1st irq happens
    .endif
    bcc @continue_pre_irq                  ; branch if enemy Y position is above where 1st scanline irq occurs
    cpy SCANLINE_IRQ_3                     ; compare Y position to scanline where 3rd irq happens
    bcs @continue_pre_irq                  ; branch if enemy Y position is below where 3rd scanline irq occurs
    lda IRQ_X_SCROLL                       ; in between 1st and 3rd scanline irqs
                                           ; use post-IRQ bg scroll values
                                           ; load how far scrolled horizontally
    sta $04                                ; store bg horizontal scroll in $04
    lda IRQ_Y_SCROLL                       ; load how far scrolled vertically
    cmp #$ff
    beq get_bg_collision_exit_no_collision
    ldy IRQ_PPUCTRL_SETTINGS               ; past IRQ, use post-IRQ PPUCTRL
    jmp @continue

; use normal scroll values before 1st irq and after 3rd irq
@continue_pre_irq:
    lda X_SCROLL         ; load PPU horizontal scroll
    sta $04              ; store horizontal bg scroll in $04
    lda Y_SCROLL         ; load PPU vertical scroll
    ldy PPUCTRL_SETTINGS ; load PPUCTRL

@continue:
    sty $05          ; store base nametable information in $05
    ldy #$00
    clc              ; clear carry in preparation for addition
    adc $06          ; add Y position to PPU vertical scroll to get absolute y location
    bcs @nametable_2
    cmp #$f0         ; didn't overflow, see if result is at very bottom of nametable
    bcc @calc_x      ; no overflow, using one of the top nametables

; overflow or sum is greater than #$f0, move to bottom nametable
@nametable_2:
    adc #$0f ; add #$10
    ldy #$02 ; set nametable index to #$02 (bottom left - $2800)

@calc_x:
    sta $01        ; set Y offset within nametable accounting for Y scroll
    and #$f0       ; strip to just high nibble
    sta $02        ; set BG_COLLISION_DATA offset high nibble (vertical component)
    lda $04        ; load horizontal scroll
    clc            ; clear carry in preparation for addition
    adc $00        ; add X position
    sta $00        ; store result in X position
    bcc @continue2
    iny            ; nametable position to right of current nametable index
                   ; move to next nametable, i.e. #$00 -> #$01 (top right $2400)
                   ; or #$02 -> #$03 (bottom right $2c00)

@continue2:
    lsr
    lsr
    lsr
    lsr
    sta $03             ; set BG_COLLISION_DATA offset low nibble (horizontal component)
    ora $02             ; calculate full BG_COLLISION_DATA offset by merging with high nibble
    sta $04             ; set BG_COLLISION_DATA offset
    tya                 ; transfer nametable index to a (#$00 = top left, #$01 = top right, #$02 = bottom left, #$03 - bottom right)
    eor $05             ; use base nametable and calculated nametable to get actual nametable
                        ; to know resulting nametable (whether to set bit 7 of BG_COLLISION_DATA offset)
    lsr                 ; push calculated nametable index to carry
    ldy NT_MIRRORING    ; load nametable mirroring (0: vertical; 1: horizontal)
    beq @load_bg_offset ; branch if vertical mirroring (AB|AB) (horizontal arrangement)
    lsr

@load_bg_offset:
    ror $04
    ldy $04 ; load bg collision offset, i.e. supertile offset

; check bg collision at BG_COLLISION_DATA,y
; input
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;    odd for low nibble, even for high nibble
;  * y - BG_COLLISION_DATA offset
check_bg_collision_at_y:
    lda $03                 ; increments as you walk horizontally
    lsr                     ; push bit 0 to carry
    lda BG_COLLISION_DATA,y ; load bg collision byte
    bcs @get_collision_val
    lsr
    lsr
    lsr
    lsr
    rts

@get_collision_val:
    and #$0f
    rts

get_bg_collision_exit_no_collision:
    lda #$00
    rts

; offset based on incline bg collision code [#$06-#$0b]
incline_collision_pos_tbl:
    .byte $00,$00,$08 ; positive incline
    .byte $00,$00,$08 ; negative incline

; check for bg collision at (a,y) as well as 2 tiles to the left if necessary
; overhead level, when player changes position vertically, used to see if going
; to collide with a wall when walking up or down
; input
;  * a - X position to test for bg collisions
;  * x - player index (0 = p1, 1 = p2)
;  * y - Y position to test for bg collisions
;  * $0f - how close horizontally to tile to the left the player needs to
;    be before testing that tile's bg collision, always set to #$0c in game
; output
;  * carry - set when bg collision found, clear otherwise
overhead_player_get_bg_collision:
    stx $17                  ; store player index (0 = p1, 1 = p2) in $17
    sta $00                  ; store X position to test in $00
    lda IRQ_Y_SCROLL         ; load vertical scroll used after 1st irq until before 3rd irq
    cmp #$ff
    bne @continue            ; continue if no irq interrupt
    cpy SCANLINE_IRQ_1       ; compare to the scanline where the 1st irq interrupt triggers
    bcs exit_no_bg_collision ; exit if past

@continue:
    lda $00                         ; load X position to test
    jsr get_bg_collision            ; get background collision code for position (a,y)
    jsr is_wall_collision_use_x_reg ; determine if collision code can be considered a wall
    bne exit_with_bg_collision      ; exit if wall bg collision
    lda $00                         ; no collision, load X position to test
    and #$0f
    sta $00
    lda $0f
    sec
    sbc $00
    bcc exit_no_bg_collision        ; branch if not near super-tile to left
    beq exit_no_bg_collision        ; branch if not near super-tile to left
    lsr
    lsr
    lsr
    lsr
    sta $0e                         ; !(OBS) in game always set to #$00 since only #$0c is passed in for $0f
                                    ; so, max value $0f - $00 is #$0c

@loop:
    lda $03              ; load whether testing high nibble or low nibble of BG_COLLISION_DATA,y
    eor #$01
    sta $03              ; move from high nibble to low nibble or vice versa
    lsr
    bcc @check_collision ; branch if testing high nibble
    dey                  ; move to previous (to the left) BG_COLLISION_DATA offset
                         ; every byte of BG_COLLISION_DATA is 32 pixels wide
                         ; (4 nametable tiles, or 1/2 of a super-tile)


@check_collision:
    jsr check_bg_collision_at_y     ; check bg collision at BG_COLLISION_DATA,y
    jsr is_wall_collision_use_x_reg
    bne exit_with_bg_collision      ; exit if bg collision with wall
    dec $0e
    bpl @loop

exit_no_bg_collision:
    ldx $17 ; load player index (0 = p1, 1 = p2)
    clc
    rts

exit_with_bg_collision:
    ldx $17 ; load player index (0 = p1, 1 = p2)
    sec     ; set carry flag
    rts

; side level get collision over a range of Y
; input
;  * a - X position
;  * y - Y position
;  * $0f - how many vertical pixels to search, e.g. set to #$14 for soldiers
; output
;  * carry - set when bg collision, clear otherwise
get_bg_collision_y_range:
    stx $17                         ; backup x in $17
    jsr get_bg_collision            ; get background collision code for position (a,y)
    jsr is_wall_collision_use_x_reg ; determine if collision code can be considered a wall (horizontally)
    bne exit_with_bg_collision      ; exit if wall collision
    lda $01                         ; no background wall collision, load Y offset within nametable accounting for Y scroll
    and #$0f
    sta $01                         ; strip to just #$00 or #$08 (alternates every nametable row)
    lda $0f                         ;
    sec                             ; set carry flag in preparation for subtraction
    sbc $01
    bcc @exit
    beq @exit
    lsr
    lsr
    lsr
    lsr
    sta $0e                         ; set number of times to loop (how many rows to check)

@loop:
    tya              ; transfer BG_COLLISION_DATA offset to a
    and #$7f         ; strip bit 7
    cmp #$08
    tya              ; transfer BG_COLLISION_DATA offset to a
    bcs @check_above ; branch if bit 7 of collision data set, i.e. left tile of collision byte is collision
    adc #$70
    ldy NT_MIRRORING ; set nametable mirroring (0: vertical; 1: horizontal)
    beq @continue
    eor #$80

@continue:
    bcc @check_collision

@check_above:
    sbc #$08 ; move up nametable 2 rows for collision byte

@check_collision:
    tay                             ; transfer BG_COLLISION_DATA offset to y
    jsr check_bg_collision_at_y     ; check bg collision at BG_COLLISION_DATA,y
    jsr is_wall_collision_use_x_reg
    bne exit_with_bg_collision
    dec $0e
    bpl @loop                       ; branch if more rows to check

@exit:
    ldx $17 ; restore x from backup
    clc
    rts

; determine if collision code can be considered a wall
; same as is_wall_collision_use_y_reg, but does not modify y register
; input
;  * a - bg collision code
; output
;  * a - 0 = no wall collision, 1 = wall collision
is_wall_collision_use_x_reg:
    tax                           ; transfer collision code to x
    lda OVERHEAD_FLAG             ; (0 = side view, 1 = overhead view)
    bne @overhead_bg_collision    ; branch for overhead level
    lda bg_ground_collision_tbl,x ; load whether solid/ground collision
    rts

@overhead_bg_collision:
    lda overhead_bg_ground_collision_tbl,x
    rts

; determine if collision code can be considered a wall
; same as is_wall_collision_use_x_reg, but does not modify x register
; input
;  * a - bg collision code
; output
;  * a - 0 = no wall collision, 1 = wall collision
is_wall_collision_use_y_reg:
    tay                           ; transfer collision code to y
    lda OVERHEAD_FLAG             ; (0 = side view, 1 = overhead view)
    bne @overhead_bg_collision    ; branch for overhead level
    lda bg_ground_collision_tbl,y ; load whether solid/ground collision
    rts

@overhead_bg_collision:
    lda overhead_bg_ground_collision_tbl,y ; load whether solid/ground collision
    rts

; table specifying land collisions (ground, or eggshells) on side-view levels
bg_ground_collision_tbl:
    .byte $00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00

overhead_bg_ground_collision_tbl:
    .byte $00,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00

; see if (a,y) is on ground, water, or incline
; input
;  * a - X position
;  * y - Y position
; output
;  * carry - set when bg collision is ground or incline, clear otherwise
;  * $03 - BG_COLLISION_DATA offset low nibble (horizontal component)
;  * $04 - bg collision offset, i.e. BG_COLLISION_DATA offset (same as y)
is_on_ground_water:
    jsr get_bg_collision_code     ; determine background collision at point (a,y)
    cmp #$06
    bcs update_nametable_exit
    tay                           ; collision code less than #$06, see if ground/water collision
    lda bg_ground_collision_tbl,y
    lsr

update_nametable_exit:
    rts

; update 2x2 nametable tiles at position (a,y) with tiles specified by $08
; input
;  * a - x offset
;  * y - y offset
;  * $08 - index into nametable_square_update_tbl
; output
;  * carry - set when unable to update graphics buffer
update_nametable_for_pos:
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$50
    bcs update_nametable_exit  ; exit with carry set if graphics buffer full
    clc                        ; clear carry in preparation for addition
    adc X_SCROLL               ; add PPU horizontal scroll
    sta $00
    lda PPUCTRL_SETTINGS
    bcc @continue
    eor #$01

@continue:
    and #$01
    ora #$08
    sta $01
    lda $00
    and #$f8
    lsr
    lsr
    lsr
    sta $00
    tya
    clc               ; clear carry in preparation for addition
    adc Y_SCROLL      ; add vertical scroll
    bcs @adjust_high  ; branch if overflow
    cmp #$f0
    bcc @calc_address

@adjust_high:
    clc      ; clear carry in preparation for addition
    adc #$10

@calc_address:
    and #$f8
    asl
    rol $01
    asl
    rol $01  ; set PPU address high byte
    ora $00
    sta $00  ; set PPU address low byte

; updates 2 rows of 2 nametable tiles based on $08 offset
; input
;  * $08 - index into nametable_square_update_tbl
;  * $00 - PPU address low byte
;  * $01 - PPU address high byte
update_nametable_square:
    lda #$06                  ; byte 0 = #$06 (block mode)
                              ; byte 1 and 2 will be PPU address
                              ; byte 3 is length, and bytes 4 to (byte 4 + length) are written to PPU
    sta CPU_GRAPHICS_BUFFER,x ; set graphics format to block mode
    inx                       ; increment graphics buffer offset
    lda #$02                  ; writing two rows of graphics
    sta $04                   ; set number of rows to write to buffer
    lda $08
    asl
    asl                       ; quadruple since each entry is #$04 bytes
    tay                       ; set starting nametable_square_update_tbl index

@write_graphics_row:
    lda $01                   ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address high byte
    inx                       ; increment graphics buffer offset
    lda $00                   ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address low byte
    inx                       ; increment graphics buffer offset
    lda #$02                  ; length is #$02 graphic bytes
    sta $05                   ; initialize graphic byte counter
    sta CPU_GRAPHICS_BUFFER,x ; store length in graphics buffer
    inx                       ; increment graphics buffer offset

@loop:
    lda nametable_square_update_tbl,y
    sta CPU_GRAPHICS_BUFFER,x         ; store graphics tile
    iny                               ; increment read offset
    inx                               ; increment graphics buffer offset
    dec $05                           ; decrement graphic bytes to draw
    bne @loop                         ; branch if another graphic byte to write
    lda $00                           ; written all graphic bytes, load PPU address low byte
    clc                               ; clear carry in preparation for addition
    adc #$20                          ; move down to next nametable row
    sta $00                           ; update PPU address low byte
    bcc @continue                     ; branch if no overflow
    inc $01                           ; overflow on low address, add one to PPU address high byte

@continue:
    dec $04                    ; decrement number of rows to write to graphics buffer
    bne @write_graphics_row    ; branch if more rows to draw
    lda #$ff                   ; finished writing all graphic data, write end of data byte
    sta CPU_GRAPHICS_BUFFER,x  ; write end of data byte
    inx                        ; increment graphics buffer offset
    stx GRAPHICS_BUFFER_OFFSET ; store graphics buffer offset
    clc                        ; wrote all successfully, clear carry
    rts

nametable_square_update_tbl:
    .ifdef Probotector
        .byte $00,$00,$00,$00 ; #$00 - blank square (used for falling ceiling tiles)
        .byte $32,$33,$38,$39 ; #$01 - helicopter turret facing right
        .byte $30,$31,$36,$37 ; #$02 - helicopter turret facing straight out
        .byte $20,$21,$29,$2a ; #$03 - helicopter turret facing left
        .byte $34,$35,$3a,$3b ; #$04 - helicopter turret destroyed
    .else
        .byte $00,$00,$00,$00 ; #$00 - blank square (used for falling ceiling tiles)
        .byte $be,$bf,$c0,$c1 ; #$01 - helicopter turret facing right
        .byte $52,$53,$5e,$5f ; #$02 - helicopter turret facing straight out
        .byte $ba,$bb,$bc,$bd ; #$03 - helicopter turret facing left
        .byte $c2,$c3,$c4,$c5 ; #$04 - helicopter turret destroyed
    .endif

; !(UNUSED) - almost exact copy of update_nametable_square
; updates 2 rows of 2 nametable tiles based on $08 offset
; input
;  * $08 - index into nametable_square_update_tbl
;  * $00 - PPU address low byte
;  * $01 - PPU address high byte
update_nametable_rect:
    lda #$06                  ; byte 0 = #$06 (block mode)
                              ; byte 1 and 2 will be PPU address
                              ; byte 3 is length, and bytes 4 to (byte 4 + length) are written to PPU
    sta CPU_GRAPHICS_BUFFER,x ; set graphics format to block mode
    inx                       ; increment graphics buffer offset
    lda #$04                  ; writing 4 rows of graphics
    sta $04                   ; set number of rows to write to buffer
    lda $08
    asl
    asl
    asl                       ; multiply by #$08 since each entry is #$08 bytes
    tay                       ; set starting nametable_rect_update_tbl index

@write_graphics_row:
    lda $01                   ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address high byte
    inx                       ; increment graphics buffer offset
    lda $00                   ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address low byte
    inx                       ; increment graphics buffer offset
    lda #$02                  ; length is #$02 graphic bytes
    sta $05                   ; initialize graphic byte counter
    sta CPU_GRAPHICS_BUFFER,x ; store length in graphics buffer
    inx                       ; increment graphics buffer offset

@loop:
    lda nametable_rect_update_tbl,y
    sta CPU_GRAPHICS_BUFFER,x       ; store graphics tile
    iny                             ; increment read offset
    inx                             ; increment graphics buffer offset
    dec $05                         ; decrement graphic bytes to draw
    bne @loop                       ; branch if another graphic byte to write
    lda $00                         ; written all graphic bytes, load PPU address low byte
    clc                             ; clear carry in preparation for addition
    adc #$20                        ; move down to next nametable row
    sta $00                         ; update PPU address low byte
    bcc @continue                   ; branch if no overflow
    inc $01                         ; overflow on low address, add one to PPU address high byte

@continue:
    dec $04                    ; decrement number of rows to write to graphics buffer
    bne @write_graphics_row    ; branch if more rows to draw
    lda #$ff                   ; finished writing all graphic data, write end of data byte
    sta CPU_GRAPHICS_BUFFER,x  ; write end of data byte
    inx                        ; increment graphics buffer offset
    stx GRAPHICS_BUFFER_OFFSET ; store graphics buffer offset
    clc                        ; wrote all successfully, clear carry
    rts

; !(UNUSED) - probably feature that was removed or never fully implemented
nametable_rect_update_tbl:
    .byte $a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad
    .byte $ae,$af,$b0,$b1,$b2,$b3,$b4,$b5
    .byte $b6,$b6,$b7,$b8,$b9,$ba,$bb,$bc
    .byte $b6,$b6,$bd,$be,$bf,$c0,$c1,$c2
    .byte $d5,$d6,$d7,$d8,$d9,$da,$db,$dc
    .byte $dd,$de,$df,$e0,$e1,$e2,$db,$dc

; set entire BG_COLLISION_DATA to #$00
clear_bg_collision_data:
    ldy #$7f

; clear the first y bytes of BG_COLLISION_DATA
; input
;  * y - amount of bytes to clear
clear_partial_bg_collision_data:
    lda #$00

@loop:
    sta BG_COLLISION_DATA,y
    dey
    bpl @loop
    rts

clear_irq_bg_collision_data:
    ldy #$7f

; clear the first y bytes of SECOND_BG_COLLISION_DATA
; input
;  * y - amount of bytes to clear
clear_partial_irq_collision_data:
    lda #$00

@loop:
    sta SECOND_BG_COLLISION_DATA,y
    dey
    bpl @loop
    rts

.ifdef Probotector
    ; !(UNUSED) duplicated level_8_palette_data from bank a
    ; probably a leftover artifact of the build system
    ; can be safely removed and used for other purposes
    .incbin "assets/chr_rom/unused_remnant_01.bin"
.endif

; unused #$2d9 bytes out of #$2,000 bytes total (91.10% full)
; unused 729 bytes out of 8,192 bytes total (91.10% full)
; filled with 729 #$ff bytes by superc.cfg configuration
bank_3_unused_space:

.segment "BANK3_ID"

; bank byte
; see load_sound_banks_init_sound
    .byte $33