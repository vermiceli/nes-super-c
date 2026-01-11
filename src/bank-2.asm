; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank 2 contains the enemy routines for Level 1 (Fort Firestorn) and Level 3
; (Jungle).
; * Enemy Type #$20 - Wall Cannon
; * Enemy Type #$22 - Helicopter Turret
; * Enemy Type #$23 - Helicopter Bay
; * Enemy Type #$62 - Level 1 Intro Helicopter
; * Enemy Type #$6f - Level 1 Background Storm
; * Enemy Type #$21 - Helicopter Core
; * Enemy Type #$22 = Helicopter Turret
; * Enemy Type #$23 = Helicopter Bay
; * Enemy Type #$26 - Hiding Sniper
; * Enemy Type #$27 - Raised Grass-Covered Turret
; * Enemy Type #$28 - Cannon Turret
; * Enemy Type #$29 - Mortar Round
; * Enemy Type #$3f - Earthquake
; * Enemy Type #$3a - Robot Spider Bullet
; * Enemy Type #$4e - Robot Spider
; * Enemy Type #$4b - Fortress Wall Core
; * Enemy Type #$50 - Fortress Wall Turret

; 8 KiB PRG ROM
.segment "BANK2"

.include "constants.asm"

; import from bank 3
.import set_delay_adv_enemy_routine
.import update_enemy_pos
.import bg_enemy_explosion_routine_01
.import set_enemy_hp_hard
.import init_bg_boss_pre_irq_max_scrolls
.import try_create_enemy_from_existing
.import advance_enemy_routine
.import clear_irq_bg_collision_data
.import clear_partial_irq_collision_data
.import clear_nametable_row
.import flip_enemy_y_dir
.import flip_enemy_x_dir
.import bg_enemy_set_scrolls
.import shake_enemy_pattern
.import enemy_routine_boss_defeated_00
.import enemy_routine_boss_defeated_01
.import bg_enemy_explosion_routine_00
.import set_boss_defeated_remove_enemy
.import remove_enemy
.import set_enemy_routine
.import clear_enemy_sprite
.import enemy_routine_explosions
.import enemy_explosion_routine_00
.import enemy_explosion_routine_01
.import enemy_explosion_routine_03
.import copy_enemy_vars_to_zp
.import player_enemy_x_dist
.import set_enemy_hp
.import update_nametable_square
.import enemy_routine_init_explosions
.import apply_gravity_adv_routine_after_delay
.import set_enemy_destroy_sprite_and_vel
.import load_banks_update_enemy_supertiles
.import grenade_routine_02
.import get_enemy_bg_collision_code_onscreen
.import update_pos_check_offscreen
.import robot_spider_routine_04
.import set_nametable_row
.import robot_spider_get_ppu_addr
.import get_enemy_bg_collision_code
.import apply_velocity
.import modify_enemy_y_vel
.import enemy_bullet_routine_00
.import jungle_earthquake
.import clear_enemy_vel
.import x_earthquake_shake
.import bg_boss_apply_vel
.import clear_partial_bg_collision_data
.import set_enemy_animation_sprite

; import from bank f
.import play_sound
.import set_level_palette
.import write_bg_palette_to_graphics_buffer
.import run_routine_from_tbl_below
.import reset_scroll_draw_point
.import set_nmi_noop_irq
.import fire_bullet_at_player
.import rotate_aim_dir_01
.import bg_collision_test_tbl
.import clear_y_scroll

; export for bank 3
.export wall_cannon_routine_ptr_tbl
.export bg_storm_routine_ptr_tbl
.export helicopter_core_routine_ptr_tbl
.export helicopter_bay_routine_ptr_tbl
.export helicopter_turret_routine_ptr_tbl
.export crouching_sniper_routine_ptr_tbl
.export grass_covered_turret_routine_ptr_tbl
.export ground_mortar_routine_ptr_tbl
.export mortar_round_routine_ptr_tbl
.export robot_spider_routine_ptr_tbl
.export robot_spider_bullet_routine_ptr_tbl
.export jungle_earthquake_routine_ptr_tbl
.export fortress_wall_core_routine_ptr_tbl
.export fortress_wall_turret_routine_ptr_tbl
.export intro_helicopter_routine_ptr_tbl

.byte $32 ; bank byte

; enemy type #$20
wall_cannon_routine_ptr_tbl:
    .addr wall_cannon_routine_00        ; set hp, destroyed attributes, position, advance routine
    .addr wall_cannon_routine_01        ; wait for activation, advance routine
    .addr wall_cannon_routine_02        ; animate activation rising from the ground, enable collisions, advance routine
    .addr wall_cannon_routine_03        ; fire single bullet at player repeatedly after delay
    .addr enemy_routine_init_explosions ; enemy destroyed routine - configure for 5 explosions
    .addr enemy_routine_explosions      ; generate 5 explosions, with an #$08 frame delay between each one
    .addr wall_cannon_routine_06        ; update supertiles to destroyed supertiles, enemy_explosion_routine_00
    .addr enemy_explosion_routine_01    ; animate explosion sequence
    .addr enemy_explosion_routine_03    ; mark destroyed, remove enemy

; set hp, destroyed attributes, position, advance routine
wall_cannon_routine_00:
    lda #$01
    sta ENEMY_SPRITE,x        ; set invisible sprite (uses bg tiles instead)
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$0c                  ; HP = #$0c, #$10, or #$13
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda ENEMY_Y_POS,x
    clc
    adc #$08
    sta ENEMY_Y_POS,x         ; adjust Y position by #$08 pixels down
    lda ENEMY_X_POS,x
    sec
    sbc #$08
    sta ENEMY_X_POS,x         ; adjust X position by #$08 pixels to the left
    jsr update_enemy_pos
    jmp advance_enemy_routine

; wait for activation, advance routine
wall_cannon_routine_01:
    jsr update_enemy_pos
    lda ENEMY_X_POS,x
    cmp #$e8                        ; see if wall cannon is in left ~90% of screen
    bcs wall_cannon_exit            ; exit if not at activate point
    lda #$01                        ; activate wall cannon
    jmp set_delay_adv_enemy_routine

; animate activation rising from the ground, enable collisions, advance routine
wall_cannon_routine_02:
    jsr update_enemy_pos
    dec ENEMY_DELAY,x               ; decrement animation delay
    bne wall_cannon_exit            ; branch if animation delay hasn't elapsed
    jsr wall_cannon_draw            ; draw wall cannon supertiles based on ENEMY_FRAME,x
    bcs wall_cannon_set_delay_exit  ; exit if unable to update supertiles to try again next frame
    inc ENEMY_FRAME,x               ; drawn supertiles, move to next frame
    lda ENEMY_FRAME,x
    cmp #$02                        ; see if drawn fully risen wall cannon
    lda #$0c                        ; delay of #$0c frames
    bcc wall_cannon_set_delay_exit  ; exit with delay if more frames to draw
    lda #$00                        ; drawn fully risen wall cannon
    sta ENEMY_DESTROY_ATTRS,x       ; enable collisions
    lda #$20
    jmp set_delay_adv_enemy_routine ; set firing delay and advance routine

; fire single bullet at player repeatedly after delay
wall_cannon_routine_03:
    jsr update_enemy_pos
    dec ENEMY_DELAY,x      ; decrement firing delay
    bne wall_cannon_exit   ; exit if firing delay hasn't elapsed
    lda GAME_COMPLETED     ; load whether or not the game has been completed
    bne @fire_bullet       ; branch if previously beat the game
                           ; (harder mode - ignore firing suppression flag)
    lda ENEMY_ATTRIBUTES,x ; game not previously completed
    lsr
    bcs wall_cannon_exit   ; exit if fire suppression flag is set

; either game has been completed, or the firing suppression flag is not set
@fire_bullet:
    lda ENEMY_X_POS,x
    cmp #$30
    bcc wall_cannon_exit               ; exit if in left 18.75% of screen
    ldy #$02                           ; enemy type #$02 (bullet)
    jsr try_create_enemy_from_existing ; fire bullet
    bcc wall_cannon_exit               ; exit if no bullet was able to be created
    ldy $11                            ; load newly-created bullet slot index
    lda #$00
    sta ENEMY_X_VELOCITY_FRACT,y
    lda #$fd
    sta ENEMY_X_VELOCITY_FAST,y        ; set bullet X velocity to -3
    lda #$01                           ; enemy_bullet_sprite_tbl offset
    sta ENEMY_VAR_1,y                  ; set enemy bullet type (sprite_06 - silver metal bullet)
    lda ENEMY_X_POS,y
    sbc #$10
    sta ENEMY_X_POS,y                  ; set initial bullet X position to be #$10 to left of wall cannon
    lda ENEMY_Y_POS,y
    adc #$02
    sta ENEMY_Y_POS,y                  ; set initial bullet Y position to be #$02 above center of wall cannon
    lda #$60

wall_cannon_set_delay_exit:
    sta ENEMY_DELAY,x ; set animation or firing delay depending on current routine

wall_cannon_exit:
    rts

; update supertiles to destroyed supertiles, enemy_explosion_routine_00
wall_cannon_routine_06:
    ldy #$02                       ; destroyed/removed wall cannon supertile indexes
    jsr wall_cannon_set_supertiles ; update the 2 supertiles for wall cannon to the destroyed tiles
    bcs @continue                  ; branch if unable to update the nametable to try again next frame
    jmp enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

@continue:
    jmp update_enemy_pos

; update the 2 supertiles for wall cannon to supertiles based on ENEMY_FRAME,x
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
wall_cannon_draw:
    ldy ENEMY_FRAME,x

; update the 2 supertiles for wall cannon to supertiles based on y
; input
;  * y - frame of wall cannon (0-2)
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space, clear when updated
wall_cannon_set_supertiles:
    lda wall_cannon_2nd_supertile_tbl,y    ; load second supertile offset
    sta $0c                                ; store second supertile offset
    lda wall_cannon_1st_supertile_tbl,y    ; load first supertile offset
    ldy #$10                               ; x offset = #$08, y offset = #$18 (one half supertile below)
    jsr load_banks_update_enemy_supertiles ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y
    lda #$01
    rts

wall_cannon_1st_supertile_tbl:
    .byte $f2 ; frame #$00 - wall cannon rising top
    .byte $f3 ; frame #$01 - wall cannon fully risen top
    .byte $01 ; frame #$02 - wall cannon destroyed top

wall_cannon_2nd_supertile_tbl:
    .byte $f1 ; frame #$00 - wall cannon rising bottom
    .byte $f0 ; frame #$01 - wall cannon fully risen bottom
    .byte $03 ; frame #$02 - wall cannon destroyed bottom

; enemy type #$22
helicopter_turret_routine_ptr_tbl:
    .addr helicopter_turret_routine_00  ; initialize HP, number of bullets to fire, aim direction, and firing delay
    .addr helicopter_turret_routine_01  ; target closest player and fire at player repeatedly
    .addr enemy_routine_init_explosions ; enemy destroyed routine - configure for 5 explosions
    .addr enemy_routine_explosions      ; generate 5 explosions, with an #$08 frame delay between each one
    .addr helicopter_turret_routine_05  ; decrement helicopter core's total number of children enemies
    .addr enemy_explosion_routine_01    ; animate explosion sequence
    .addr helicopter_turret_routine_07  ; just jumps to enemy_explosion_routine_03 (mark destroyed, remove enemy)

; initialize HP, number of bullets to fire, aim direction, and firing delay
helicopter_turret_routine_00:
    lda #$08                                 ; HP = #$08, #$0c, or #$0f
    jsr set_enemy_hp                         ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$02
    sta ENEMY_VAR_3,x                        ; initialize number of bullets to fire per attack
    lda #$03
    sta ENEMY_VAR_1,x                        ; set initial targeting direction (facing right)
    ldy ENEMY_ATTRIBUTES,x                   ; load turret number #$00 to #$03
    lda helicopter_turret_bullet_delay_tbl,y ; load initial delay before firing bullet timer
                                             ; based on the helicopter turret index
    sta ENEMY_FIRING_DELAY,x                 ; set bullet fire delay timer
    lda #$01
    jmp set_delay_adv_enemy_routine          ; set delay to #$01 and routine to helicopter_turret_routine_01

helicopter_turret_bullet_delay_tbl:
    .byte $6b,$95,$c1,$f8

; target closest player and fire at player repeatedly
helicopter_turret_routine_01:
    lda #$01
    sta ENEMY_SPRITE,x             ; turret is background tile, use invisible sprite
    jsr set_helicopter_turret_vars ; set helicopter turret X and Y position
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x      ; bullets travel through enemy and player can collide
    lda ENEMY_VAR_7,x              ; load horizontal screen indicator
                                   ; #$ff left of screen, #$00 on screen, #$01 right of screen
    bne helicopter_turret_exit     ; branch if off screen
    lda ENEMY_X_POS,x              ; load enemy's X position
    cmp #$08
    bcc helicopter_turret_exit     ; exit if turret is almost off screen to left
    cmp #$10
    bcc @continue
    cmp #$fc
    bcs helicopter_turret_exit     ; exit if turret is almost off screen to right
    cmp #$f0
    bcs @continue
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x      ; enable player-enemy and player bullet-enemy collision

@continue:
    ldy ENEMY_VAR_5,x            ; load helicopter core enemy slot index
    lda ENEMY_Y_VELOCITY_FAST,y  ; load helicopter core's fast Y velocity
    ora ENEMY_Y_VELOCITY_FRACT,y ; merge with helicopter core's fractional Y velocity
    beq @aim_fire                ; branch if helicopter is not moving vertically
    lda ENEMY_Y_VELOCITY_FAST,y  ; helicopter moving vertically, load helicopter core's fast Y velocity
    bpl @aim                     ; branch if moving down

; helicopter is moving up, or not moving vertically
@aim_fire:
    dec ENEMY_FIRING_DELAY,x  ; decrement bullet fire delay timer
    bne @aim                  ; branch if turret firing delay timer has not elapsed
    lda ENEMY_VAR_4,x         ; load targeted player
    sta $0a                   ; store targeted player in $0a for fire_bullet_at_player
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$02                  ; set bullet speed code to normal 1x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    lda #$10                  ; more bullets to fire firing delay
    dec ENEMY_VAR_3,x         ; decrement number of bullets to fire
    bne @set_delay            ; branch if more bullets to fire to use shorter delay
    lda #$02                  ; fired all bullets, reset number of bullets to fire
    sta ENEMY_VAR_3,x         ; set remaining number of bullets to fire
    lda #$89                  ; load firing delay

@set_delay:
    sta ENEMY_FIRING_DELAY,x ; set delay until next firing

@aim:
    dec ENEMY_DELAY,x          ; decrement re-targeting delay
    bne helicopter_turret_exit ; exit if re-targeting delay has not elapsed
    lda #$0d                   ; targeting delay elapsed, initialize delay for next time to target again
    sta ENEMY_DELAY,x          ; set re-targeting delay
    jsr player_enemy_x_dist    ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    tya                        ; transfer closest player index to a
    sta ENEMY_VAR_4,x          ; set player to target (0 = p1, 1 = p2)
    jsr copy_enemy_vars_to_zp  ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr rotate_aim_dir_01      ; determine next aim direction [#$00-#$0b] ($0c), adjusts ENEMY_VAR_1 to get closer to that value using quadrant_aim_dir_01
                               ; now calculate turret tile index based on direction
                               ; facing right tiles for directions between [#$00-#$04] or [#$12-#$0a]
                               ; facing straight tiles for directions between [#$05-#$07]
                               ; facing left tiles for directions between [#$08-#$11]
    ldy #$00
    lda ENEMY_VAR_1,x          ; load targeting direction [#$00-#$0b]
    cmp #$05
    bcc @update_nametable      ; branch if target direction is less than #$05 to use #$00 (right)
    cmp #$12
    bcs @update_nametable      ; branch if target direction >= #$12 to use #$00 (right)
    iny                        ; targeting direction >= #$05 and < #$12, set aim to #$01 (straight)
    cmp #$08
    bcc @update_nametable      ; branch if aim direction is straight, i.e. targeting direction >= #$05 and < #$08
    iny                        ; targeting direction >= #$08 and < #$12, set aim to #$02 (left)

@update_nametable:
    tya                             ; transfer aim direction to a
                                    ; 0 = right, 1 = straight out, 2 = left
    clc                             ; clear carry in preparation for addition
    adc #$01
    jmp helicopter_turret_update_ui ; update nametable tiles based on aim direction

helicopter_turret_exit:
    rts

; decrements helicopter core's total number of non-destroyed turrets
helicopter_turret_routine_05:
    jsr set_helicopter_turret_vars  ; set helicopter turret X and Y position
    lda #$04                        ; helicopter turret destroyed tiles
    jsr helicopter_turret_update_ui ; update nametable tiles to be the turret destroyed tiles
    bcs helicopter_turret_exit      ; exit if unable to update nametable
    lda ENEMY_VAR_5,x               ; updated nametable, load helicopter core enemy slot index
    tax                             ; transfer to offset register
    dec ENEMY_VAR_3,x               ; decrement remaining number helicopter sub-enemies, e.g. number of turrets and helicopter bay
    ldx ENEMY_CURRENT_SLOT          ; load current enemy slot
    jmp enemy_explosion_routine_00  ; set empty sprite, play optional enemy destroyed sound, disable collisions

; just jumps to enemy_explosion_routine_03
; mark destroyed, remove enemy
helicopter_turret_routine_07:
    jmp enemy_explosion_routine_03 ; !(HUH) why not just set this address in helicopter_turret_routine_ptr_tbl

; update helicopter turret nametable tiles based on aiming direction a
; input
;  * a - turret state/direction (0 = facing right, 1 = facing straight out, 2 = facing left, 3 = destroyed)
;  * x - enemy slot index
; output
;  * carry - set when unable to update nametable due to graphics buffer full
helicopter_turret_update_ui:
    sta $08                                ; store aim direction in $08
    lda ENEMY_X_POS,x                      ; load enemy's X position
    cmp #$08                               ; see if turret is close to left edge of screen
    bcc @no_action_exit                    ; exit success when close to left edge (do nothing)
    cmp #$fc                               ; see if turret is close to right edge of screen
    bcs @no_action_exit                    ; exit success when close to right edge (do nothing)
    lda ENEMY_ATTRIBUTES,x                 ; load helicopter turret index
    ldx GRAPHICS_BUFFER_OFFSET             ; load current graphics buffer offset
    cpx #$50                               ; see if graphics buffer full
    bcs @exit                              ; exit with carry set if unable to write to nametable
    and #$07                               ; strip to just the helicopter turret index
    asl                                    ; double since each entry is a #$02 byte PPU address
    tay                                    ; transfer to offset register
    lda helicopter_turret_ppu_addr_tbl,y   ; load PPU address low byte
    sta $00                                ; set PPU address low byte
    lda helicopter_turret_ppu_addr_tbl+1,y ; load PPU address high byte
    sta $01                                ; set PPU address high byte
    jsr update_nametable_square            ; update nametable for helicopter turret

@exit:
    ldx ENEMY_CURRENT_SLOT
    rts

@no_action_exit:
    clc
    rts

helicopter_turret_ppu_addr_tbl:
    .ifdef Probotector
        .byte $8f,$2e  ; turret 0
        .byte $92,$2e  ; turret 1
        .byte $95,$2e  ; turret 2
        .byte $98,$2e  ; turret 3
    .else
        .byte $8d,$2e  ; turret 0
        .byte $90,$2e  ; turret 1
        .byte $93,$2e  ; turret 2
        .byte $96,$2e  ; turret 3
    .endif

; set helicopter bay X and Y position
set_helicopter_bay_vars:
    ldy #$08 ; !(HUH) not used

; set helicopter turret or helicopter bay X and Y position
; input
;  * x - helicopter/turret enemy slot
set_helicopter_turret_vars:
    lda ENEMY_ATTRIBUTES,x             ; load turret/bay attributes
    and #$07                           ; strip to just the 'offset' that defines the turret or bay
    asl                                ; double since each entry is #$02 bytes
    tay                                ; transfer to offset register
    lda helicopter_turret_bay_offset,y ; load turret/bay y offset
    sta $08                            ; store turret/bay y offset in $08
    asl                                ; push offset direction bit to carry
    lda #$00                           ; assume y offset is downward
    bcc @continue_x                    ; branch if y offset is down
    lda #$ff                           ; y offset upward (I don't think ever happens)

@continue_x:
    sta $0a                              ; store offset direction in $0a (0 = down, #$ff = up)
    lda helicopter_turret_bay_offset+1,y ; load turret/bay x offset
    sta $09                              ; store turret/bay x offset
    asl                                  ; push offset direction bit to carry
    lda #$00                             ; assume x offset is to the right
    bcc @continue                        ; branch if x offset is to the right
    lda #$ff                             ; offset direction to left

@continue:
    sta $0b           ; store offset direction (0 = right, #$ff = left)
    ldy ENEMY_VAR_5,x ; load helicopter core slot index
    lda $09           ; load turret/bay x offset
    clc               ; clear carry in preparation for addition
    adc ENEMY_X_POS,y ; add to helicopter core X position
    sta ENEMY_X_POS,x ; update turret/bay X position based on offset from core location
    lda $0b           ; load offset direction (0 = right, #$ff = left)
                      ; dir + num_offscreen_turrets + carry
    adc ENEMY_VAR_7,y ; add 1 if direction is right and off screen to the right (#$00 + #$00 + carry == 1)
                      ; adds 1 if direction is left and off screen the left (#$ff + #$00 + no carry == 1)
    sta ENEMY_VAR_7,x ; set number of off-screen turrets (x-axis)
    bne @clear_sprite ; branch turret is off-screen to hide sprite
    lda $08           ; turret on screen, load turret/bay y offset
    clc               ; clear carry in preparation for addition
    adc ENEMY_Y_POS,y ; add to helicopter core Y position
    sta ENEMY_Y_POS,x ; update turret/bay Y position based on offset from core location
    lda $0a           ; load y offset direction (0 = down, 1 = up)
                      ; dir + num_offscreen_turrets + carry
    adc ENEMY_VAR_6,y ; add 1 if direction is down and off screen to the bottom (#$00 + #$00 + carry == 1)
                      ; adds 1 if direction is up and off screen to the top (#$ff + #$01 + no carry == 1)
    sta ENEMY_VAR_6,x ; set number of off-screen turrets (y-axis)
    bne @clear_sprite
    rts

@clear_sprite:
    jmp clear_enemy_sprite

; y offset, then x offset from helicopter core position
helicopter_turret_bay_offset:
    .ifdef Probotector
        .byte $02,$e0  ; helicopter turret 0 (-48, 2)
        .byte $02,$f8  ; helicopter turret 1 (-24, 2)
        .byte $02,$10  ; helicopter turret 2 (0, 2)
        .byte $02,$28  ; helicopter turret 3 (24, 2)
        .byte $0a,$d0  ; helicopter bay (-80, 10)
    .else
        .byte $02,$d0  ; helicopter turret 0 (-48, 2)
        .byte $02,$e8  ; helicopter turret 1 (-24, 2)
        .byte $02,$00  ; helicopter turret 2 (0, 2)
        .byte $02,$18  ; helicopter turret 3 (24, 2)
        .byte $0a,$b0  ; helicopter bay (-80, 10)
    .endif

; enemy type #$23
helicopter_bay_routine_ptr_tbl:
    .addr helicopter_bay_routine_00 ; set no bullet collision, and delay
    .addr helicopter_bay_routine_01 ; wait for delay * 4 then set frame to start opening bay door
    .addr helicopter_bay_routine_02 ; wait for delay, animate opening bay door fully, set open duration, advance routine
    .addr helicopter_bay_routine_03 ; generate soldiers, one at a time, until delay elapses
    .addr helicopter_bay_routine_04 ; animate closing bay door
    .addr helicopter_bay_routine_05 ; bay closed, check core HP, set delay go back to helicopter_bay_routine_01
    .addr remove_enemy              ; enemy destroyed routine

; set no bullet collision, and delay
helicopter_bay_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x       ; bullets travel through enemy and player can collide
    lda #$80
    jmp set_delay_adv_enemy_routine ; set delay to #$80 and routine to helicopter_bay_routine_01

; wait for delay * 4 then set frame to start opening bay door
helicopter_bay_routine_01:
    jsr set_helicopter_bay_vars     ; set helicopter bay X and Y position
    lda ENEMY_VAR_7,x               ; load nametable which core is in (#$ff - off-screen left, #$00 - visible, #$01 - off-screen right)
    bne helicopter_bay_exit         ; exit if helicopter is off the screen to the left/right
    lda GLOBAL_TIMER                ; helicopter bay is on screen, load global timer
    and #$03
    bne helicopter_bay_exit         ; exit if timer is not a multiple of #$04
    dec ENEMY_DELAY,x               ; decrement delay timer
    bne helicopter_bay_exit         ; exit if delay not elapsed
    lda #$01
    sta ENEMY_FRAME,x               ; partially open
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and routine to helicopter_bay_routine_02

; wait for delay, animate opening bay door fully, set open duration, advance routine
helicopter_bay_routine_02:
    jsr set_helicopter_bay_vars       ; set helicopter bay X and Y position
    dec ENEMY_DELAY,x                 ; decrement animation delay
    bne helicopter_bay_exit           ; exit if animation delay hasn't elapsed
    jsr helicopter_bay_set_tiles      ; write pattern tiles to CPU_GRAPHICS_BUFFER based on ENEMY_FRAME
    lda #$01
    bcs helicopter_bay_set_delay_exit ; exit if unable to update nametable tiles to try again next frame
    inc ENEMY_FRAME,x                 ; move to next frame (more open)
    lda ENEMY_FRAME,x                 ; load new frame index
    cmp #$04                          ; see if past fully open bay frame
    bcs helicopter_bay_bay_open       ; branch if bay is open to set open duration
                                      ; and advance routine
helicopter_bay_set_delay_10_exit:
    lda #$10

helicopter_bay_set_delay_exit:
    sta ENEMY_DELAY,x

helicopter_bay_exit:
    rts

helicopter_bay_bay_open:
    dec ENEMY_FRAME,x                      ; since incremented before realizing went to far
    dec ENEMY_FRAME,x                      ; decrement twice
    lda #$20
    sta ENEMY_FIRING_DELAY,x               ; delay between soldier generation
    ldy ENEMY_VAR_2,x                      ; load helicopter bay opening duration index
    lda helicopter_bay_open_duration_tbl,y ; load bay opening duration
    jmp set_delay_adv_enemy_routine        ; set routine to helicopter_bay_routine_03

helicopter_bay_open_duration_tbl:
    .byte $b0,$a0

; generate soldiers, one at a time, until delay elapses
helicopter_bay_routine_03:
    jsr set_helicopter_bay_vars     ; set helicopter bay X and Y position
    lda GLOBAL_TIMER
    and #$03
    bne @continue
    dec ENEMY_DELAY,x               ; decrement delay every #$04 ticks of the global timer
    bne @continue
    lda #$01                        ; delay elapsed, move to next routine to close bay door
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and routine to helicopter_bay_routine_04

@continue:
    dec ENEMY_FIRING_DELAY,x           ; decrement soldier generation delay
    bne helicopter_bay_exit
    ldy #$03                           ; enemy type = soldier
    jsr try_create_enemy_from_existing ; create soldier
    bcc @set_delay_exit                ; branch if unable to create soldier
    ldy $11                            ; load enemy slot of created soldier
    inc ENEMY_VAR_1,x                  ; increment soldier generation count
    lda ENEMY_VAR_1,x                  ; load soldier generation count
    cmp #$06                           ; see if generated #$06 soldiers
    lda #$02                           ; assume non-firing green soldier
    bcc @set_attr_delay_exit           ; branch if non-firing green soldier
    lda #$00                           ; red firing soldier, use #$0a for enemy attributes
    sta ENEMY_VAR_1,x                  ; reset soldier generation count (goes from #$00 to #$06)
    lda #$0a                           ; enemy attributes for red firing soldier

@set_attr_delay_exit:
    sta ENEMY_ATTRIBUTES,y ; set soldier attributes

@set_delay_exit:
    lda #$30
    sta ENEMY_FIRING_DELAY,x ; set delay before next soldier is generated

helicopter_bay_exit2:
    rts

; animate closing bay door
helicopter_bay_routine_04:
    jsr set_helicopter_bay_vars          ; set helicopter bay X and Y position
    dec ENEMY_DELAY,x
    bne helicopter_bay_exit              ; exit if delay hasn't elapsed
    jsr helicopter_bay_set_tiles         ; write pattern tiles to CPU_GRAPHICS_BUFFER based on ENEMY_FRAME
    lda #$01
    bcs helicopter_bay_set_delay_exit    ; branch if unable to update nametable to try again next frame
    dec ENEMY_FRAME,x                    ; continue closing the bay door
    bpl helicopter_bay_set_delay_10_exit ; branch to set delay to continue closing door
                                         ; if more frames to animate
    jmp advance_enemy_routine            ; animated closing door, advance to next routine

; bay closed, check core HP, set delay go back to helicopter_bay_routine_01
helicopter_bay_routine_05:
    jsr set_helicopter_bay_vars ; set helicopter bay X and Y position
    ldy ENEMY_VAR_5,x           ; load helicopter core slot index
    lda ENEMY_VAR_5,y           ; load whether helicopter core wants to move to opening bay door
                                ; (see helicopter_core_subroutine_0a)
    beq helicopter_bay_exit2    ; exit if shouldn't open bay door yet
    lda #$01
    sta ENEMY_VAR_2,x           ; use shorter bay door open duration
    lda #$10
    sta ENEMY_DELAY,x           ; set delay before opening bay door (will decrement every 4 frames)
    lda #$02
    jmp set_enemy_routine       ; set routine to helicopter_bay_routine_01 to start opening

; write appropriate pattern tiles to CPU_GRAPHICS_BUFFER based on ENEMY_FRAME
; output
;  * carry flag - set when graphics buffer full, clear when updated graphics buffer
helicopter_bay_set_tiles:
    lda ENEMY_FRAME,x                    ; load enemy frame (how open the bay is)
    asl                                  ; double since each entry is a #$02 byte memory address
    tay                                  ; transfer to offset register
    ldx GRAPHICS_BUFFER_OFFSET           ; load current graphics buffer offset
    cpx #$40                             ; compare to a full graphics buffer
    bcs @exit                            ; exit if graphics buffer is full to try again next frame
    lda helicopter_bay_frame_ptr_tbl,y   ; load animation frame data address low byte
    sta $08                              ; store animation frame data address low byte
    lda helicopter_bay_frame_ptr_tbl+1,y ; load animation frame data address high byte
    sta $09                              ; store animation frame data address high byte
    .ifdef Probotector
        lda #$ac
    .else
        lda #$a8
    .endif
    sta $00                              ; set initial PPU write address low byte
    lda #$2a
    sta $01                              ; set initial PPU write address high byte
    lda #$06                             ; byte 0 = #$06 (block mode)
                                         ; byte 1 and 2 will be PPU address
                                         ; byte 3 is length, and bytes 4 to (byte 4 + length) are written to PPU
    sta CPU_GRAPHICS_BUFFER,x            ; set graphics format to block mode
    inx                                  ; increment graphics buffer offset
    ldy #$00                             ; initialize graphics read offset to 0
    lda #$04
    sta $0a                              ; set to draw #$04 rows of #$05 tiles each row

@write_header:
    lda $01                   ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address high byte
    inx                       ; increment graphics buffer offset
    lda $00                   ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address low byte
    inx                       ; increment graphics buffer offset
    .ifdef Probotector
        lda #$04              ; writing #$04 tiles to graphics buffer
    .else
        lda #$05              ; writing #$05 tiles to graphics buffer
    .endif
    sta CPU_GRAPHICS_BUFFER,x ; store group size in graphics buffer
    sta $0b                   ; store group size in $0b
    inx                       ; increment graphics buffer offset

@loop:
    lda ($08),y               ; load pattern table tile
    sta CPU_GRAPHICS_BUFFER,x ; write pattern table tile to buffer
    iny                       ; increment read offset
    inx                       ; increment graphics buffer offset
    dec $0b                   ; decrement number of bytes to write in the row
    bne @loop                 ; branch if more bytes to write
    lda $00                   ; written all bytes for row, move down to next nametable row
    clc
    adc #$20                  ; add #$20 to PPU address low byte (moving down one row)
    sta $00                   ; update PPU address low byte
    bcc @continue             ; branch if no overflow
    inc $01                   ; add 1 to PPU address high byte if overflow

@continue:
    dec $0a                    ; decrement number of rows to write
    bne @write_header          ; branch to write next row of tiles if more rows to draw
    lda #$ff                   ; finished drawing all row, write end of data byte
    sta CPU_GRAPHICS_BUFFER,x  ; write end of data byte
    inx                        ; increment graphics buffer offset
    stx GRAPHICS_BUFFER_OFFSET ; save offset in memory

@exit:
    ldx ENEMY_CURRENT_SLOT ; restore x to helicopter bay enemy slot
    rts

helicopter_bay_frame_ptr_tbl:
    .addr helicopter_bay_frame_00 ; frame 0 - closed
    .addr helicopter_bay_frame_01 ; frame 1 - partially open
    .addr helicopter_bay_frame_02 ; frame 2 - more open
    .addr helicopter_bay_frame_03 ; frame 3 - fully open

.ifdef Probotector
    helicopter_bay_frame_00:
        .byte $26,$27,$28,$3a
        .byte $00,$00,$2b,$2c
        .byte $00,$00,$00,$2e
        .byte $00,$00,$00,$00

    helicopter_bay_frame_01:
        .byte $98,$99,$28,$3a
        .byte $9a,$9b,$9c,$2c
        .byte $00,$00,$9a,$9d
        .byte $00,$00,$00,$00

    helicopter_bay_frame_02:
        .byte $26,$27,$28,$3a
        .byte $00,$a4,$2b,$2c
        .byte $a7,$a8,$a9,$aa
        .byte $00,$00,$00,$00

    helicopter_bay_frame_03:
        .byte $26,$27,$28,$3a
        .byte $00,$a5,$a6,$2c
        .byte $00,$ab,$ac,$ad
        .byte $ae,$af,$b0,$00
.else
    helicopter_bay_frame_00:
        .byte $00,$00,$5b,$5c,$5d
        .byte $00,$00,$00,$68,$69
        .byte $00,$00,$00,$00,$7c
        .byte $00,$00,$00,$00,$00

    helicopter_bay_frame_01:
        .byte $93,$94,$95,$5c,$5d
        .byte $00,$96,$97,$98,$69
        .byte $00,$00,$00,$96,$7c
        .byte $00,$00,$00,$00,$00

    helicopter_bay_frame_02:
        .byte $00,$00,$99,$5c,$5d
        .byte $00,$00,$9a,$68,$69
        .byte $9b,$9c,$9d,$9e,$9f
        .byte $00,$00,$00,$00,$00

    helicopter_bay_frame_03:
        .byte $00,$00,$5b,$5c,$5d
        .byte $00,$00,$9a,$68,$69
        .byte $00,$00,$a0,$a1,$a2
        .byte $a3,$a4,$a5,$a6,$00
.endif

; enemy type #$62
intro_helicopter_routine_ptr_tbl:
    .addr intro_helicopter_routine_00 ; set collision attributes, set delay, advance routine
    .addr intro_helicopter_routine_01 ; animate helicopter rotor blade

; set collision attributes, set delay, advance routine
intro_helicopter_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x       ; bullets travel through enemy and player can collide
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$0c
    jmp set_delay_adv_enemy_routine ; set delay to #$0c and routine to intro_helicopter_routine_01

; animate helicopter rotor blade
intro_helicopter_routine_01:
    lda #$00                           ; do not play sound_1d (thunder)
    jsr animate_bg_thunder_effect      ; determine which bg palette to set lightning effect
    jsr update_enemy_pos               ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_FRAME,x                  ; load helicopter frame
    asl                                ; double since each entry is a #$02 byte address
    tay                                ; transfer to offset register
    lda intro_helicopter_frame_tbl,y   ; load address low byte
    sta $08                            ; store low byte in $08
    lda intro_helicopter_frame_tbl+1,y ; load address high byte
    sta $09                            ; store high byte in $09
    lda GRAPHICS_BUFFER_OFFSET         ; load current graphics buffer position
    cmp #$20                           ; see if too full
    bcs @exit                          ; exit if graphics buffer too full to try again next frame
    tax                                ; can update tiles, transfer graphics buffer offset to x
    ldy #$00                           ; initialize read offset

; update nametable tiles to animate rotor blades
; uses CHR bank #$00 (ROM address $20010)
@graphic_update_loop:
    lda ($08),y                ; load graphic byte
    sta CPU_GRAPHICS_BUFFER,x  ; write to graphics buffer
    inx                        ; increment graphics buffer write offset
    iny                        ; increment graphics read offset
    cmp #$ff                   ; see if read end of data marker
    bne @graphic_update_loop   ; branch to write next graphic byte to buffer if not end of data
    stx GRAPHICS_BUFFER_OFFSET ; read all data, update the graphics buffer offset
    ldx ENEMY_CURRENT_SLOT     ; restore x to the enemy offset
    lda ENEMY_FRAME,x          ; load current animation frame for helicopter
    clc                        ; clear carry in preparation for addition
    adc #$01                   ; advance the animation frame
    cmp #$06                   ; see if past the last animation frame
    bcc @continue              ; branch if more frames to animation
    lda #$00                   ; animated entire animation sequence, reset to 0 to loop

@continue:
    sta ENEMY_FRAME,x ; update animation frame

@exit:
    rts

; !(UNUSED)
; most likely originally was used to determine graphics data frame address
; e.g. ENEMY_FRAME #$03 would got to intro_helicopter_frame_tbl+6
bank_2_unused_00:
    .byte $00,$02,$04,$06,$04,$02

; helicopter rotor blade animation
; all referenced tiles are in CHR bank #$00 (ROM address $20010)
intro_helicopter_frame_tbl:
    .addr intro_helicopter_frame_00
    .addr intro_helicopter_frame_01
    .addr intro_helicopter_frame_02
    .addr intro_helicopter_frame_03
    .addr intro_helicopter_frame_04
    .addr intro_helicopter_frame_05 ; same as intro_helicopter_frame_01

intro_helicopter_frame_00:
    .byte $06                             ; block mode
    .byte $20,$84                         ; PPU address $2084
    .byte $10                             ; #$10 tiles
    .byte $11,$11,$12,$11,$12,$12,$13,$75 ; pattern tile indexes
    .byte $14,$76,$12,$12,$11,$12,$11,$11
    .byte $ff                             ; end of data marker

intro_helicopter_frame_01:
intro_helicopter_frame_05:
    .byte $06                             ; block mode
    .byte $20,$84                         ; PPU address $2084
    .byte $10                             ; #$10 tiles
    .byte $00,$00,$11,$12,$11,$12,$13,$75 ; pattern tile indexes
    .byte $14,$76,$12,$12,$11,$11,$00,$00
    .byte $ff                             ; end of data marker

intro_helicopter_frame_02:
intro_helicopter_frame_04:
    .byte $06                         ; block mode
    .byte $20,$86                     ; PPU address $2086
    .byte $0c                         ; #$0c tiles
    .byte $00,$00,$11,$11,$12,$75,$14 ; pattern tile indexes
    .byte $76,$12,$12,$00,$00
    .byte $ff                         ; end of data marker

intro_helicopter_frame_03:
    .byte $06                             ; block mode
    .byte $20,$88                         ; PPU address $2088
    .byte $08                             ; #$08 tiles
    .byte $00,$00,$00,$77,$78,$00,$00,$00 ; pattern tile indexes
    .byte $ff                             ; end of data marker

; enemy type #$6f
bg_storm_routine_ptr_tbl:
    .addr bg_storm_routine_00 ; set collision attributes, set delay, advance routine
    .addr bg_storm_routine_01 ; update position, and create lightning flash and thunder sound effect

; set collision attributes, set delay, advance routine
bg_storm_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x       ; bullets travel through enemy and player can collide
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and routine to bg_storm_routine_01

; update position, and create lightning flash and thunder sound effect
bg_storm_routine_01:
    jsr update_enemy_pos ; adjust position based on scroll (does not apply velocity)
    lda #$01

; determine which bg palette to set lightning effect
; used by both background storm (enemy type = #$6f)
; and intro helicopter (enemy type = #$62)
; input
;  * a - whether or not to play sound_1d (0 = do not play, 1 = play)
;  * x - enemy slot index
;  * ENEMY_VAR_1,x - current bg effect (0 = lightning, 1 = normal)
;  * ENEMY_VAR_2,x - number of lightning flashes triggered by enemy
animate_bg_thunder_effect:
    sta $08           ; store whether or not to play storm thunder sound
    lda ENEMY_VAR_1,x ; load whether or not lightning is flashing
    bne @continue     ; branch if lightning is flashing
    lda ENEMY_X_POS,x ; load enemy's X position
    cmp #$20
    bcc @exit         ; exit if intro helicopter or bg storm in leftmost 12.5% of screen

@continue:
    dec ENEMY_DELAY,x        ; decrement lightning flash delay
    bne @exit                ; exit if lightning flash delay hasn't elapsed
    lda ENEMY_VAR_1,x        ; lightning flash delay elapsed
                             ; load bg effect (0 = lightning, 1 = normal)
    bne @bg_palette_continue ; branch if normal background
    lda $08                  ; load whether or not to play sound_1d
    beq @bg_palette_continue ; branch if shouldn't play thunder sound
    lda #$1d                 ; sound_1d - level 1 storm thunder
    jsr play_sound           ; play sound_1d - level 1 storm thunder

@bg_palette_continue:
    ldy ENEMY_VAR_1,x   ; re-load bg effect (0 = lightning, 1 = normal)
    beq @set_bg_palette ; branch if lightning effect
    ldy #$03            ; normal bg effect

@set_bg_palette:
    lda bg_lightning_palette_tbl,y
    sta PALETTE_CPU_BUFFER+9           ; set bg palette 2 color 1
    lda bg_lightning_palette_tbl+1,y
    sta PALETTE_CPU_BUFFER+10          ; set bg palette 2 color 2
    lda bg_lightning_palette_tbl+2,y
    sta PALETTE_CPU_BUFFER+11          ; set bg palette 2 color 3
    lda ENEMY_VAR_1,x                  ; re-load bg effect (0 = lightning, 1 = normal)
    eor #$01                           ; swap to other effect
    sta ENEMY_VAR_1,x                  ; update bg effect
    lsr
    lda #$05
    bcs @set_delay_exit                ; branch if just switched from lightning to normal bg
    ldy ENEMY_VAR_2,x                  ; switching to lightning effect
    inc ENEMY_VAR_2,x                  ; increment number of lightning flashes
    lda ENEMY_VAR_2,x
    and #$03
    sta ENEMY_VAR_2,x                  ; ensure number is between #$00 and #$03
                                       ; for intro helicopter this isn't necessary, but for background storm enemy
                                       ; this is needed
    lda bg_storm_animation_delay_tbl,y

@set_delay_exit:
    sta ENEMY_DELAY,x ; set delay until next flash based on number of lightning flashes

@exit:
    rts

bg_storm_animation_delay_tbl:
    .byte $0e,$2a,$1e,$c0

; level 1 lightning effect
bg_lightning_palette_tbl:
    .byte COLOR_LT_MAGENTA_24, COLOR_LT_RED_26, COLOR_WHITE_20        ; lightning effect
    .byte COLOR_LT_ORANGE_27, COLOR_MED_RED_16, COLOR_DARK_MAGENTA_04 ; normal bg

; enemy type #$21
helicopter_core_routine_ptr_tbl:
    .addr helicopter_core_routine_00    ; disable player-enemy collision, set bullet collision box, set hp
    .addr helicopter_core_routine_01    ; create turrets and helicopter bay, set core position, init irq
    .addr helicopter_core_routine_02    ; writes black tile for entire nametable $2800
    .addr helicopter_core_routine_03    ; update helicopter palette, initialize to helicopter_core_subroutine_00, set velocity, advance routine
    .addr helicopter_core_routine_04    ; animate, play rotor noise, enable soldier generation when bay open, execute helicopter core mode routine
    .addr helicopter_core_routine_05    ; enemy destroyed routine
    .addr helicopter_core_routine_06    ; wait for helicopter to fall to ground
    .addr helicopter_core_routine_07    ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr bg_enemy_explosion_routine_01 ; animate sequence of explosions
    .addr helicopter_core_routine_09    ; remove scanline interrupts, set correct left pattern table (bg) tiles, remove boss

; disable player-enemy collision, set bullet collision box, set hp
helicopter_core_routine_00:
    lda #$11
    sta ENEMY_DESTROY_ATTRS,x ; set bullet collision box and disable player-enemy collision
    lda #$20                  ; HP = #$20, #$30, or #$38
    jsr set_enemy_hp_hard     ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    jmp advance_enemy_routine ; advance to next routine

helicopter_core_exit:
    rts

; create turrets and helicopter bay, set core position, init irq
helicopter_core_routine_01:
    lda SCREEN_SCROLL_TYPE          ; 0 = horizontal, 1 = vertical/overhead
    beq helicopter_core_exit
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne helicopter_core_exit        ; exit if still drawing
    lda #$a0
    sta ENEMY_X_POS,x               ; set core initial X position
    lda #$b8
    sta ENEMY_Y_POS,x               ; set core initial Y position
    lda #$ff
    sta ENEMY_VAR_6,x               ; initialize turrets as being off screen
    sta ENEMY_VAR_7,x               ; initialize horizontal screen as off screen to the left
    lda #$e0
    sta Y_SCROLL                    ; set PPU vertical scroll to show bottom nametable
    lda #$00
    sta X_SCROLL                    ; set PPU horizontal scroll to no scroll
    lda #$ab
    sta PPUCTRL_SETTINGS            ; enable nmi for vertical blank, 8x16 sprites, base nametable $2c00
    lda PPUMASK_SETTINGS
    and #$fd
    sta PPUMASK_SETTINGS
    jsr clear_irq_bg_collision_data ; set entire BG_COLLISION_DATA to #$00
    lda #$04                        ; set pre-irq left pattern table tiles to helicopter tiles
    sta LEFT_TOP_HALF_CHR_BANK      ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$06                        ; helicopter tiles
    sta LEFT_BOTTOM_CHR_HALF_BANK   ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    ldy #$04                        ; helicopter enemies creation counter (4 turrets and 1 helicopter bay)

; create helicotper turrets and helicopter bay
@loop:
    sty $08
    lda helicopter_enemy_tbl,y         ; enemy type = #$22 or #$23 (helicopter turret or helicopter bay door)
    tay
    jsr try_create_enemy_from_existing ; create helicopter turret or helicopter bay door
    bcc @continue                      ; branch if unable to create enemy
    ldy $08                            ; created enemy, re-load offset
    lda helicopter_enemy_tbl,y         ; see what enemy was just created
    cmp #$22                           ; see if a helicopter turret
    bne @set_attrs                     ; continue to enemy configuration if not helicopter bay
    inc ENEMY_VAR_3,x                  ; increment number of turrets and helicopter bay

; initialize helicotper turrets and helicopter bay
@set_attrs:
    tya                    ; transfer offset number to a
    ldy $11                ; load enemy slot of created enemy
    sta ENEMY_ATTRIBUTES,y ; set enemy attribute to its offset index
    txa                    ; transfer helicopter core enemy offset to a
    sta ENEMY_VAR_5,y      ; set variable of turret/bay to the enemy slot of helicopter core
    ldy $08                ; re-load loop counter/enemy to create index
    dey                    ; decrement loop counter
    bpl @loop              ; branch if more turrets to create

@continue:
    jsr init_bg_boss_pre_irq_max_scrolls    ; init boss screen pre-irq scroll max values
    lda #$01
    sta NT_MIRRORING                        ; set horizontal nametable mirroring
    lda #$01
    sta IRQ_TYPE                            ; set irq routine type to irq_handler_01_ptr_tbl
                                            ; level 1 fort firestorm boss screen
    .ifdef Probotector
        lda #$94
    .else
        lda #$a2
    .endif
    sta SCANLINE_IRQ_1                      ; set scanline irq to be right where the ground starts
    lda Y_SCROLL                            ; load PPU vertical scroll
    sta IRQ_Y_SCROLL                        ; use same vertical scroll for both screens
    lda X_SCROLL                            ; load PPU horizontal scroll
    sta IRQ_X_SCROLL                        ; use same horizontal scroll for both screens
    lda PPUCTRL_SETTINGS
    and #$fe
    sta IRQ_PPUCTRL_SETTINGS
    lda #$10
    sta ENEMY_Y_POS,x                       ; initialize helicopter core Y position
    lda #$00
    sta ENEMY_VAR_6,x                       ; initialize number of off screen turrets (y-axis) to 0
    lda #$28
    sta ENEMY_VAR_1,x                       ; set PPU address high byte
    lda #$00
    sta ENEMY_VAR_2,x                       ; set PPU address low byte
                                            ; sets PPU address to bottom left nametable (PPU address $2800)
    lda #$1e                                ; number of nametable rows to clear (set to black)
    sta ENEMY_VAR_4,x                       ; clear entire nametable (all #$1e rows)
    ldy #$00                                ; white, med olive, dark olive
    jsr set_level_palette                   ; set background palette 2 if boss not defeated
    ldy #$04                                ; dark green, med olive, dark olive
    jsr set_level_palette                   ; set background palette 3 if boss not defeated
    jsr write_bg_palette_to_graphics_buffer ; write background palette colors to graphics buffer
    ldx ENEMY_CURRENT_SLOT                  ; restore x to enemy current slot
    lda #$30                                ; sound_30 (GREAT) - Great Heli - Ruined Base (Boss 1)
    jsr play_sound                          ; play sound_30 - Great Heli - Ruined Base (Boss 1)
    jmp advance_enemy_routine               ; advance to next routine

; enemy type #$22 = helicopter turret
; enemy type #$23 = helicopter bay
helicopter_enemy_tbl:
    .byte $22,$22,$22,$22,$23

; writes black tile for entire nametable $2800
helicopter_core_routine_02:
    jsr clear_nametable_row   ; write a row of tile 0 (black) to the nametable
                              ; moves PPU address to next row down
                              ; ENEMY_VAR_1,x - PPU address high byte, ENEMY_VAR_2,x - PPU address low byte
    dec ENEMY_VAR_4,x         ; decrement number of rows to clear
    bpl @exit
    jmp advance_enemy_routine ; advance to next routine if done clearing nametable

@exit:
    rts

; update helicopter palette, initialize to helicopter_core_subroutine_00, set velocity, advance routine
helicopter_core_routine_03:
    jsr helicopter_write_attr       ; write helicopter attribute data for coloring
    bcs helicopter_core_exit2       ; exit if unable to update attribute table
    ldx ENEMY_CURRENT_SLOT
    lda #$00
    sta ENEMY_VAR_1,x               ; initialize to helicopter_core_subroutine_00
    jsr set_helicopter_velocity     ; set velocity to x = .3125, y = 0 based on ENEMY_VAR_1,x
    jmp set_delay_adv_enemy_routine ; set delay and set routine to helicopter_core_routine_04

; animate, play rotor noise, enable soldier generation when bay open, execute helicopter core mode routine
helicopter_core_routine_04:
    dec ENEMY_DELAY,x
    bne @continue
    lda #$04
    sta ENEMY_DELAY,x
    lda #$09          ; sound_09 (helicopter rotor noise)
    jsr play_sound    ; play sound_09 (helicopter rotor noise)

@continue:
    lda ENEMY_VAR_3,x
    bne @exe_core_mode
    lda ENEMY_GEN_CTRL ; helicopter bay open, initialize
    and #$bf           ; clear bit 6 (resume random enemy generation)
    sta ENEMY_GEN_CTRL
    lda #$a7           ; sprite_a7
    sta ENEMY_SPRITE,x ; set helicopter core sprite

@exe_core_mode:
    lda GLOBAL_TIMER
    lsr
    lsr
    and #$03                           ; flash helicopter core palette every 8 frames
    sta ENEMY_SPRITE_ATTR,x            ; flash helicopter core spite palette
    jsr run_helicopter_core_subroutine ; execute core mode routine based on ENEMY_VAR_1
    jmp helicopter_core_shake_anim

helicopter_core_exit2:
    rts

; enemy destroyed routine
helicopter_core_routine_05:
    lda #$0b
    sta ENEMY_VAR_1,x                  ; setting velocity to x vel = 0, y vel = .3125
    jsr set_helicopter_velocity        ; set velocity based on ENEMY_VAR_1,x
    jsr helicopter_core_shake_anim
    jmp enemy_routine_boss_defeated_00

; wait for helicopter to fall to ground
helicopter_core_routine_06:
    lda ENEMY_Y_POS,x       ; load enemy's Y position
    cmp #$a0
    bcs @continue
    jsr shake_enemy_pattern ; shake enemy as it rises/falls based on GLOBAL_TIMER

@continue:
    jsr helicopter_core_set_scroll_anim
    jmp enemy_routine_boss_defeated_01

; set empty sprite, play optional enemy destroyed sound, disable collisions
helicopter_core_routine_07:
  jmp bg_enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions
                                    ; !(HUH) why not just set this address in helicopter_core_routine_ptr_tbl

; remove scanline interrupts, set correct left pattern table (bg) tiles, remove boss
helicopter_core_routine_09:
    jsr set_nmi_noop_irq               ; remove any scanline interrupts
    lda #$aa
    sta PPUCTRL_SETTINGS               ; enable nmi for vertical blank, 8x16 sprites, base nametable $2800
    lda PPUMASK_SETTINGS
    ora #$02
    sta PPUMASK_SETTINGS
    lda #$e0
    sta Y_SCROLL                       ; set PPU vertical scroll to show bottom nametable
    jsr reset_scroll_draw_point
    lda #$38
    sta LEFT_TOP_HALF_CHR_BANK         ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$3a
    sta LEFT_BOTTOM_CHR_HALF_BANK      ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    jmp set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy

helicopter_core_shake_anim:
    jsr shake_enemy_pattern ; shake enemy as it rises/falls based on GLOBAL_TIMER

helicopter_core_set_scroll_anim:
    jsr bg_enemy_set_scrolls            ; simulate moving (ENEMY_X_POS, ENEMY_Y_POS) by scrolling in the opposite direction
    jsr helicopter_core_draw_helicopter
    jmp helicopter_core_animate

; set helicopter velocity based on ENEMY_VAR_1,x (helicopter subroutine)
; input
;  * x - enemy slot index
;  * ENEMY_VAR_1 ; offset into helicopter_vel_tbl
set_helicopter_velocity:
    lda ENEMY_VAR_1,x
    asl
    asl                          ; quadruple since each entry is #$04 bytes
    tay
    lda helicopter_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda helicopter_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda helicopter_vel_tbl+2,y
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda helicopter_vel_tbl+3,y
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    rts

helicopter_vel_tbl:
    .byte $00,$00,$50,$00 ; subroutine = #$00, x vel = .3125, y vel = 0
    .byte $50,$00,$50,$00 ; subroutine = #$01, x vel = .3125, y vel = .3125
    .byte $00,$00,$50,$00 ; subroutine = #$02, x vel = .3125, y vel = 0
    .byte $50,$00,$50,$00 ; subroutine = #$03, x vel = .3125, y vel = .3125
    .byte $00,$00,$50,$00 ; subroutine = #$04, x vel = .3125, y vel = 0
    .byte $00,$00,$b0,$ff ; subroutine = #$05, x vel = -.3125, y vel = 0
    .byte $b0,$ff,$b0,$ff ; subroutine = #$06, x vel = -.3125, y vel = -.3125
    .byte $00,$00,$b0,$ff ; subroutine = #$07, x vel = -.3125, y vel = 0
    .byte $50,$00,$50,$00 ; subroutine = #$08, x vel = .3125, y vel = .3125
    .byte $00,$00,$50,$00 ; subroutine = #$09, x vel = .3125, y vel = 0
    .byte $b0,$ff,$b0,$ff ; subroutine = #$0a, x vel = -.3125, y vel = -.3125
    .byte $50,$00,$00,$00 ; subroutine = #$0b, x vel = 0, y vel = .3125 (helicopter destroyed)

run_helicopter_core_subroutine:
    lda ENEMY_VAR_1,x
    jsr run_routine_from_tbl_below

helicopter_core_subroutine_tbl:
    .addr helicopter_core_subroutine_00 ; wait for core to be on screen and helicopter X position to be greater than 1
    .addr helicopter_core_subroutine_01 ; wait for helicopter to descend for first time, and disable soldier generation
    .addr helicopter_core_subroutine_02 ; wait for helicopter to get to right 37% of screen
    .addr helicopter_core_subroutine_03 ; wait for helicopter to get to bottom 47% of screen
    .addr helicopter_core_subroutine_04 ; wait for core to move off-screen to the right by 19%
    .addr helicopter_core_subroutine_05 ; core moving left to be on screen, wait for core to be on left 87.5% of screen
    .addr helicopter_core_subroutine_06 ; wait for core to get high (top 31.25%) enough
    .addr helicopter_core_subroutine_07 ; wait for core to get far enough into left nametable (non-visible)
    .addr helicopter_core_subroutine_08 ; wait for helicopter to get low enough (bottom 47% of screen), enable bay opening
    .addr helicopter_core_subroutine_09 ; wait for helicopter core to get off screen to right, enable bay open (fast soldier gen)
    .addr helicopter_core_subroutine_0a ; bouncing dvd video logo pattern (reverse direction when encounter edge collision)

; wait for core to be on screen and helicopter X position to be greater than 1
helicopter_core_subroutine_00:
    lda ENEMY_VAR_7,x                    ; see if core visible on screen (#$ff = not visible, #$00 = visible)
                                         ; set in push_enemy
    bne helicopter_core_subroutine_exit2 ; exit if core not visible on screen
    lda ENEMY_X_POS,x                    ; core visible on screen (can be hidden behind turrets)
                                         ; load enemy's X position
    cmp #$01
    bcc helicopter_core_subroutine_exit2 ; branch if core still at X position 0

; advance subroutine, set velocity based on new subroutine
adv_helicopter_core_subroutine:
    inc ENEMY_VAR_1,x           ; helicopter core on screen (can be hidden behind turrets)
                                ; move to next helicopter core mode routine
    jmp set_helicopter_velocity ; set helicopter velocity based on subroutine set in ENEMY_VAR_1,x

; wait for helicopter to descend for first time, and disable soldier generation
helicopter_core_subroutine_01:
    lda ENEMY_VAR_6,x
    bne helicopter_core_subroutine_exit2
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    cmp #$40
    bcc helicopter_core_subroutine_exit  ; branch if
    lda ENEMY_GEN_CTRL
    ora #$40                             ; set bit 6 to pause random enemy generation
    sta ENEMY_GEN_CTRL                   ; pause enemy generation now that helicopter is in level
                                         ; and not at top of screen
    bcs adv_helicopter_core_subroutine

; wait for helicopter to get to right 37% of screen
helicopter_core_subroutine_02:
    lda ENEMY_X_POS,x                  ; load enemy's X position
    cmp #$a0
    bcs adv_helicopter_core_subroutine ; branch to advance subroutine if on right 37% of screen

helicopter_core_subroutine_exit:
    rts

; wait for helicopter to get to bottom 47% of screen
helicopter_core_subroutine_03:
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    cmp #$78
    bcs adv_helicopter_core_subroutine ; branch to advance subroutine if on bottom 47% of screen
    rts

; wait for core to move off-screen to the right by 19%
helicopter_core_subroutine_04:
    lda ENEMY_VAR_7,x
    cmp #$01
    bne helicopter_core_subroutine_exit2 ; branch if core not yet moved off-screen to right
    lda ENEMY_X_POS,x                    ; helicopter core moved off screen to right
                                         ; load enemy's X position
    .ifdef Probotector
        cmp #$20
    .else
        cmp #$30
    .endif
    bcs adv_helicopter_core_subroutine   ; branch to advance routine if core far off to the right
    rts                                  ; core not yet far enough to the right, exit

; core moving left to be on screen, wait for core to be on left 87.5% of screen
helicopter_core_subroutine_05:
    lda ENEMY_VAR_7,x
    bne helicopter_core_subroutine_exit2 ; branch to exit if core not yet visible
    lda ENEMY_X_POS,x                    ; load enemy's X position
    cmp #$e0
    bcc adv_helicopter_core_subroutine   ; advance routine if core far enough left
    rts

; wait for core to get high (top 31.25%) enough
helicopter_core_subroutine_06:
    lda ENEMY_Y_POS,x                  ; load enemy's Y position
    cmp #$50
    bcc adv_helicopter_core_subroutine ; branch if helicopter core high enough

helicopter_core_subroutine_exit2:
    rts

; wait for core to get far enough (88%) into left screen (non-visible)
helicopter_core_subroutine_07:
    lda ENEMY_VAR_7,x                    ; load visible screen indicator
    bpl helicopter_core_subroutine_exit2 ; exit if core not yet on left (non-visible) nametable
    lda ENEMY_X_POS,x                    ; load enemy's X position
    cmp #$e0
    bcc adv_helicopter_core_subroutine
    rts

; wait for helicopter to get low enough (bottom 47% of screen), enable bay opening
helicopter_core_subroutine_08:
    lda ENEMY_Y_POS,x                            ; load enemy's Y position
    cmp #$78
    bcc helicopter_core_subroutine_exit          ; branch if enemy still too high
    lda #$01
    bne helicopter_core_subroutine_set_bay_state ; always branch to allow helicopter bay to open

; wait for helicopter core to get off screen to right, enable bay open (fast soldier gen)
helicopter_core_subroutine_09:
    lda ENEMY_VAR_7,x
    cmp #$01
    bne helicopter_core_subroutine_exit2 ; branch if helicopter isn't off the screen to the right
    lda ENEMY_X_POS,x                    ; load enemy's X position
    cmp #$10
    bcc helicopter_core_subroutine_exit  ; exit if helicopter isn't far enough off screen to the right
    lda #$00                             ; helicopter off screen to right, prevent helicopter bay from opening

helicopter_core_subroutine_set_bay_state:
    sta ENEMY_VAR_5,x                  ; when 1, enables fast enemy gen for open bay
    bcs adv_helicopter_core_subroutine

; bouncing dvd video logo pattern (reverse direction when encounter edge collision)
; when encounter X collision (6.25% and 87.5%), reverse X direction
; when encounter Y collision (12.5% and 46.88%), reverse Y direction
helicopter_core_subroutine_0a:
    lda ENEMY_Y_VELOCITY_FAST,x            ; load enemy's fast Y velocity
    rol
    rol
    and #$01
    tay                                    ; transfer Y velocity direction to y (0 = down, 1 = up)
    lda ENEMY_Y_POS,x                      ; load enemy's Y position
    cmp helicopter_core_y_dir_change_tbl,y ; compare Y position to #$78 (down) or #$40 (up)
    ror                                    ; transfer carry to bit 7
    eor ENEMY_Y_VELOCITY_FAST,x
    bpl @continue                          ; branch if moving down and Y position less than #$78 or
                                           ; if moving up and Y position greater than #$40
    jsr flip_enemy_y_dir                   ; flip the sign of the enemy's Y velocity, e.g. 1.25 becomes -1.25

@continue:
    lda ENEMY_VAR_7,x
    bne helicopter_core_subroutine_exit2         ; exit if helicopter is off the screen to the left/right
    lda ENEMY_X_VELOCITY_FAST,x                  ; load enemy's fast X velocity
    rol
    rol
    and #$01
    tay                                          ; transfer X velocity direction to y (0 = right, 1 = left)
    lda ENEMY_X_POS,x                            ; load enemy's X position
    cmp helicopter_core_x_dir_change_tbl,y       ; compare X position to #$e0 (right) or #$10 (left)
    ror
    eor ENEMY_X_VELOCITY_FAST,x
    bpl helicopter_core_subroutine_exit2         ; branch if moving right and X position less than #$e0 or
                                                 ; if moving left and X position greater than #$10
    lda helicopter_core_change_bay_routine_tbl,y ; load whether or not to start opening bay door
                                                 ; starting to move right = update bay routine to open door
                                                 ; starting to move left = do not update
    sta ENEMY_VAR_5,x                            ; when #$01 moves helicopter bay from helicopter_bay_routine_05 to helicopter_bay_routine_01
                                                 ; (!OBS) could have been optimized to just do sty ENEMY_VAR_5,x
                                                 ; starts opening bay door and sets the shorter bay door open duration
    jmp flip_enemy_x_dir                         ; flip the sign of the enemy's X velocity, e.g. 1.25 becomes -1.25

helicopter_core_y_dir_change_tbl:
    .byte $78,$40

helicopter_core_x_dir_change_tbl:
    .byte $e0,$10

helicopter_core_change_bay_routine_tbl:
    .byte $00,$01

; output
;  * carry flag - set when graphics buffer full, clear otherwise
helicopter_write_attr:
    .ifdef Probotector
        lda #$21
    .else
        lda #$14
    .endif
    sta $08                    ; writing #$14 bytes
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$40
    bcs @exit                  ; exit if graphics buffer full

@attribute_loop:
    lda helicopter_attribute_data_tbl,y
    sta CPU_GRAPHICS_BUFFER,x
    inx
    iny
    dec $08
    bne @attribute_loop
    stx GRAPHICS_BUFFER_OFFSET
    clc

@exit:
    ldx ENEMY_CURRENT_SLOT ; restore x to enemy current slot
    rts

helicopter_attribute_data_tbl:
    .ifdef Probotector
        .byte 03                                                              ; repeat mode
        .byte $2b,$d8                                                         ; PPU address $2bd8 (bottom left nametable attribute table)
        .byte $09                                                             ; number of repetitions
        .byte $aa                                                             ; byte to write (palette)
        .byte $06                                                             ; block mode
        .byte $2b,$e1                                                         ; PPU address $2be1
        .byte $17                                                             ; length of data to write
        .byte $ea,$fa,$ea,$fa,$fa,$ba,$ea,$fb,$fb,$0f,$eb,$fa,$aa,$ea,$fb,$00
        .byte $00,$00,$ef,$aa,$aa,$aa,$aa
        .byte $ff
    .else
        .byte $03                                                             ; repeat mode
        .byte $2b,$d8                                                         ; PPU address $2bd8 (bottom left nametable attribute table)
        .byte $12                                                             ; number of repetitions
        .byte $aa                                                             ; byte to write (palette)
        .byte $06                                                             ; block mode
        .byte $2b,$ea                                                         ; PPU address $2bea (bottom left nametable attribute table)
        .byte $05                                                             ; length of data to write
        .byte $ae,$fa,$fa,$fa,$fa                                             ; graphic bytes to write
        .byte $ff                                                             ; end block mode
        .byte $03                                                             ; repeat mode
        .byte $2b,$ef                                                         ; PPU address $2bef (bottom left nametable attribute table)
        .byte $08                                                             ; number of repetitions
        .byte $aa                                                             ; byte to write (palette)
    .endif

helicopter_core_exit3:
    rts

; re-draws/blacks out parts of the helicopter so scrolling doesn't show
; off-screen portion
helicopter_core_draw_helicopter:
    lda X_SCROLL                ; load PPU horizontal scroll
    and #$07
    cmp ENEMY_VAR_2,x
    beq helicopter_core_exit3
    sta ENEMY_VAR_2,x
    lda ENEMY_X_VELOCITY_FAST,x ; load enemy's fast X velocity
    rol
    rol
    and #$01
    cmp ENEMY_VAR_2,x
    bne helicopter_core_exit3
    lda X_SCROLL                ; load PPU horizontal scroll
    lsr
    lsr
    lsr
    sta $0b
    lda ENEMY_X_VELOCITY_FAST,x ; load enemy's fast X velocity
    asl
    lda PPUCTRL_SETTINGS
    bcc @calc_offset
    eor #$01                    ; switch to other horizontal nametable

@calc_offset:
    lsr
    ldy #$00
    .ifdef Probotector
        lda $0b              ; load X scroll
        bcc @continue        ; branch if base nametable is $2000 or $2800
        cmp #$01
        bcs @write_block
        adc #$20
        clc
        jmp @sub_write_block
    @continue:
        cmp #$03
        bcc @write_block
        cmp #$20
    .else
        bcs @write_block
        lda $0b
    @continue:
        cmp #$03
        bcc @write_block
        cmp #$1f
    .endif
    bcs @write_block

@sub_write_block:
    sbc #$02
    tay      ; transfer offset graphics read offset to y

@write_block:
    tya
    clc                           ; clear carry in preparation for addition
    adc helicopter_draw_ptr_tbl
    sta $08
    lda #$00
    adc helicopter_draw_ptr_tbl+1
    sta $09
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$07
    sta CPU_GRAPHICS_BUFFER,x     ; block mode
    inx                           ; increment graphics buffer write offset
    lda $0b
    clc                           ; clear carry in preparation for addition
    adc #$e0
    sta CPU_GRAPHICS_BUFFER+1,x   ; write PPU address low byte
    lda #$00
    adc #$29
    sta CPU_GRAPHICS_BUFFER,x     ; write PPU address high byte
    inx                           ; increment graphics buffer write offset
    inx                           ; increment graphics buffer write offset
    .ifdef Probotector
        lda #$0a
    .else
        lda #$0b
    .endif
    sta CPU_GRAPHICS_BUFFER,x     ; writing #$0b bytes
    sta $0a                       ; set remaining bytes to draw
    inx                           ; increment graphics buffer write offset
    ldy #$00

@write_loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    inx                        ; increment graphics buffer write offset
    lda $08
    clc                        ; clear carry in preparation for addition
    .ifdef Probotector
        adc #$1e
    .else
        adc #$1c
    .endif
    sta $08
    lda $09
    adc #$00
    sta $09                    ; move to next 'row'
    dec $0a                    ; decrement remaining bytes to draw
    bne @write_loop
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x  ; write end of block mode byte
    inx                        ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET ; update graphics buffer write offset
    ldx ENEMY_CURRENT_SLOT     ; restore x to the enemy offset
    rts

helicopter_draw_ptr_tbl:
    .addr helicopter_draw_00

helicopter_draw_00:
    .ifdef Probotector
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$3c,$3d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3e
        .byte $3f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$41,$42,$42,$42,$42
        .byte $43,$44,$45,$46,$00,$00,$00,$00,$00,$00,$00,$0b,$08,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$0c,$0d,$0e,$47,$48,$49,$49,$49,$4a,$4b,$4c
        .byte $4d,$4e,$4f,$50,$51,$00,$00,$00,$00,$10,$11,$12,$12,$12,$12,$12
        .byte $13,$14,$15,$16,$16,$17,$52,$53,$53,$53,$53,$54,$55,$56,$57,$58
        .byte $59,$5a,$5b,$5c,$00,$00,$00,$18,$19,$1a,$1b,$1c,$1d,$1d,$1e,$1e
        .byte $1f,$1e,$34,$35,$5d,$34,$35,$5d,$34,$35,$5d,$34,$35,$5e,$5f,$60
        .byte $61,$62,$63,$64,$00,$00,$22,$23,$23,$24,$25,$25,$26,$26,$27,$28
        .byte $3a,$3b,$65,$3a,$3b,$65,$3a,$3b,$65,$3a,$3b,$66,$1a,$67,$68,$69
        .byte $6a,$6b,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2b,$2c,$2d
        .byte $6c,$2d,$6d,$6e,$6f,$70,$71,$70,$72,$73,$74,$75,$76,$77,$78,$79
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2e,$2f,$7a,$7b
        .byte $7c,$7d,$7e,$7e,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$87,$87,$87,$88,$87
        .byte $87,$87,$87,$87,$89,$87,$87,$8a,$00,$00,$00,$00
    .else
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $04,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0b,$0c,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$08,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$00
        .byte $00,$00,$00,$00,$00,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$23,$24,$25
        .byte $26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$00,$00,$00
        .byte $00,$33,$34,$35,$36,$37,$38,$39,$3a,$3a,$3a,$3b,$3c,$3d,$3e,$3f
        .byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$00,$00,$00,$00,$4a,$4b
        .byte $4c,$4d,$4e,$4f,$50,$51,$c2,$c3,$51,$c2,$c3,$51,$c2,$c3,$51,$c2
        .byte $c3,$54,$55,$56,$57,$58,$59,$5a,$00,$00,$00,$00,$00,$00,$00,$5b
        .byte $5c,$5d,$c4,$c5,$60,$c4,$c5,$60,$c4,$c5,$60,$c4,$c5,$61,$62,$63
        .byte $64,$65,$66,$67,$00,$00,$00,$00,$00,$00,$00,$00,$68,$69,$6a,$6b
        .byte $6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$7c,$7d,$7e,$7f,$80,$81,$82
        .byte $83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8d,$8e,$00,$00,$00,$00
        .byte $00,$00,$00,$8f,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$91,$92,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00
    .endif

; draw helicopter as scroll reveals and hides parts of it
; due to horizontal mirroring (vertical arrangement) the entire helicopter
; cannot be drawn at once as it's larger than a single nametable
helicopter_core_animate:
    lda GLOBAL_TIMER
    and #$00
    beq @draw_heli                     ; continue if global timer non-zero
    jmp helicopter_core_restore_x_exit

@draw_heli:
    lda ENEMY_FRAME,x
    asl                                     ; double since each entry is a #$02 byte memory address
    tay                                     ; transfer to offset register
    lda helicopter_core_bg_tile_ptr_tbl,y
    sta $08
    lda helicopter_core_bg_tile_ptr_tbl+1,y
    sta $09
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                                ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x               ; set block mode
    inx                                     ; increment graphics buffer write offset
    ldy #$00                                ; initialize read offset
    sty $06                                 ; initialize number of graphics blocks written

@load_bg_loop:
    sty $07             ; backup graphics read offset
    lda ($08),y         ; read helicopter_core_bg_tile_xx byte
    bpl @load_bg
    jmp @write_byte_adv ; negative byte, write end of data
                        ; and advance ENEMY_FRAME,x

@load_bg:
    and #$1f             ; strip to just tile offset within nametable (0-31)
    sta $05              ; set horizontal tile offset within a nametable
    lda PPUCTRL_SETTINGS
    ror
    ror
    ror
    ror
    and #$20             ; strip value to just PPUCTRL bit 1 now in bit 6
    ora #$01
    sta $00              ; set nametable horizontal tile base offset
                         ; either #$01 for left nametables, or #21 for right nametables
    lda X_SCROLL         ; load PPU horizontal scroll
    lsr
    lsr
    lsr                  ; divide by 8 since each tile is 8 pixels wide
    clc                  ; clear carry in preparation for addition
    adc $00              ; add horizontal tiles scrolled to base nametable offset
                         ; to get tile offset from left of left nametables for start of X PPU scroll
    and #$3f             ; strip to maximum number of tiles in a row (63 in decimal)
    sta $00              ; set number of tiles from left of left nametable where X scroll starts
    eor ($08),y          ; see if same as original value read from table
    iny
    iny
    and #$20
    bne @read            ; branch if tile offset doesn't match originally read value
    lda $00              ; value was the same, load horizontal tile offset
    and #$1f             ; strip to tile offset within a nametable
    sec                  ; set carry flag in preparation for subtraction
    sbc $05              ; subtract starting tile offset for drawing helicopter
    bcs @continue        ; branch if no underflow
    lda #$00             ; underflow

@continue:
    sta $02                  ; store number of tiles left of visible scroll that aren't drawn
    clc                      ; clear carry in preparation for addition
    adc $05                  ; add back starting tile offset
    sta $03                  ; set horizontal tile start offset
                             ; (amount to add to PPU address low byte)
    lda ($08),y              ; read helicopter_core_bg_tile_xx byte
    sec                      ; set carry flag in preparation for subtraction
    sbc $02                  ; subtract block size adjust amount
    sta $04                  ; set graphic block size
    jmp @write_ppu_and_block

@read:
    lda #$ff
    sta $04
    lda $00
    and #$1f
    sec                 ; set carry flag in preparation for subtraction
    sbc $05
    beq @prep
    bcc @prep
    cmp ($08),y
    bcc @set_block_size
    lda ($08),y

@set_block_size:
    sta $04 ; set graphic block size, when 0 or negative, unable to write

@prep:
    lda $05
    sta $03  ; set PPU address low byte offset
    lda #$00
    sta $02  ; added to read offset

; input
;  * y  - helicopter_core_bg_tile_xx read offset
@write_ppu_and_block:
    dey                         ; decrement read offset
    lda #$00
    sta $00                     ; initialize PPU address high byte to #$00
    lda ($08),y                 ; read number of rows down from top of nametable along with nametable info
    asl
    asl
    asl
    asl
    rol $00
    asl                         ; calculate PPU address low byte by number of rows to offset
                                ; by multiplying the row offset count by #$20
    rol $00                     ; a now stores the PPU address high low byte
                                ; $00 now contains bits 4 and 3 from read byte in bits 1 and 0 respectively
                                ; this is nametable overflow and will be used to set PPU address high byte
    clc                         ; clear carry in preparation for addition
    adc $03                     ; the row offset is added to $03 to get the PPU address low byte
    sta CPU_GRAPHICS_BUFFER+1,x ; write PPU address low byte
    lda $00                     ; load ppu address high byte component
    ora #$28                    ; always drawing in bottom left nametable
    sta CPU_GRAPHICS_BUFFER,x   ; write PPU address high byte
    iny                         ; increment read offset
    lda $04                     ; load graphics block size, 0 or negative when unable to write
    beq @move_read_offset
    bmi @move_read_offset       ; branch if unable to write
    inx                         ; block size greater than #$00, skip PPU address
    inx                         ; move pass PPU write address
    sta CPU_GRAPHICS_BUFFER,x   ; write graphics block size
    inx                         ; increment graphics buffer write offset
    iny                         ; increment graphics buffer read offset
    tya                         ; transfer read offset to a for addition
    clc                         ; clear carry in preparation for addition
    adc $02                     ; add amount to skip
    tay

; write the actual graphics bytes
@graphics_loop:
    lda ($08),y               ; read helicopter_core_bg_tile_xx byte
    sta CPU_GRAPHICS_BUFFER,x
    iny                       ; increment read offset
    inx                       ; increment buffer write offset
    dec $04                   ; decrement graphics block size counter
    bne @graphics_loop        ; loop if more graphics bytes to write
    inc $06                   ; increment number of graphics blocks written

@move_read_offset:
    ldy $07           ; read previous graphics data read offset
    iny
    iny               ; move forward 2 bytes to skip over ppu address
    sty $07           ; backup graphics read data offset
    lda ($08),y       ; read graphics block size
    clc               ; clear carry in preparation for addition
    adc $07           ; move pointer to end of graphics
                      ; for use after writing graphics block to buffer
    tay               ; set new graphics data read offset
    iny               ; increment graphics read offset
    jmp @load_bg_loop

; input
;  * a - graphic byte to write, always #$ff to signal end of data
@write_byte_adv:
    sta CPU_GRAPHICS_BUFFER,x  ; write end graphics block byte
    inx                        ; increment graphics buffer write offset
    lda $06                    ; load number of graphics blocks written
    beq @next_frame
    stx GRAPHICS_BUFFER_OFFSET ; update graphics buffer offset

@next_frame:
    ldx ENEMY_CURRENT_SLOT
    lda ENEMY_FRAME,x
    clc                    ; clear carry in preparation for addition
    adc #$01
    cmp #$06               ; see if passed last frame of animation
    bcc @set_frame_exit    ; exit if not passed
    lda #$00               ; reset animation counter

@set_frame_exit:
    sta ENEMY_FRAME,x
    rts

helicopter_core_restore_x_exit:
    ldx ENEMY_CURRENT_SLOT
    rts

; tiles written in block mode
helicopter_core_bg_tile_ptr_tbl:
    .addr helicopter_core_bg_tile_00
    .addr helicopter_core_bg_tile_01
    .addr helicopter_core_bg_tile_02
    .addr helicopter_core_bg_tile_03
    .addr helicopter_core_bg_tile_04
    .addr helicopter_core_bg_tile_05

; negative number signals end of stream
.ifdef Probotector
    helicopter_core_bg_tile_00:
        .byte $21,$0f,$04,$00,$00,$00,$00,$20,$10,$04,$04,$04,$04,$00,$20,$11
        .byte $06,$00,$00,$00,$00,$00,$00,$21,$12,$05,$00,$00,$00,$00,$00,$21
        .byte $13,$04,$00,$00,$00,$00,$07,$10,$08,$04,$04,$04,$05,$04,$05,$04
        .byte $05,$1b,$10,$05,$05,$04,$05,$04,$05,$27,$10,$19,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$03,$0f,$02,$00,$00,$03,$10,$02,$00,$00,$00
        .byte $11,$06,$9e,$9f,$a0,$a1,$a2,$a3,$01,$12,$04,$00,$09,$93,$94,$01
        .byte $13,$04,$00,$00,$18,$97,$ff

    helicopter_core_bg_tile_01:
        .byte $20,$10,$03,$00,$00,$00,$07,$10,$08,$00,$00,$00,$00,$04,$04,$05
        .byte $04,$1b,$10,$05,$04,$05,$04,$04,$00,$01,$0f,$01,$01,$01,$10,$02
        .byte $02,$03,$00,$11,$06,$00,$00,$06,$07,$08,$00,$03,$12,$03,$0a,$0b
        .byte $08,$03,$13,$03,$0f,$10,$11,$ff

    helicopter_core_bg_tile_02:
        .byte $0b,$10,$14,$00,$00,$00,$00,$00,$04,$04,$05,$05,$b1,$3f,$05,$05
        .byte $04,$04,$00,$00,$00,$00,$00,$01,$0f,$04,$00,$00,$8b,$8c,$01,$10
        .byte $04,$00,$00,$8d,$8e,$02,$11,$02,$8f,$90,$01,$12,$04,$91,$92,$93
        .byte $94,$01,$13,$04,$95,$96,$18,$97,$ff

    helicopter_core_bg_tile_03:
        .byte $10,$10,$0a,$00,$00,$00,$00,$b3,$b4,$00,$00,$00,$00,$03,$0f,$02
        .byte $00,$00,$03,$10,$02,$00,$00,$00,$11,$06,$9e,$9f,$a0,$a1,$a2,$a3
        .byte $01,$12,$04,$00,$09,$93,$94,$01,$13,$04,$00,$00,$18,$97,$ff

    helicopter_core_bg_tile_04:
        .byte $10,$10,$0a,$04,$04,$05,$05,$3e,$b2,$05,$05,$04,$04,$01,$0f,$01
        .byte $01,$01,$10,$02,$02,$03,$00,$11,$06,$00,$00,$06,$07,$08,$00,$03
        .byte $12,$03,$0a,$0b,$08,$03,$13,$03,$0f,$10,$11,$ff

    helicopter_core_bg_tile_05:
        .byte $0b,$10,$14,$04,$04,$05,$04,$05,$05,$05,$05,$05,$47,$48,$05,$05
        .byte $05,$05,$05,$04,$05,$04,$04,$01,$0f,$04,$00,$00,$8b,$8c,$01,$10
        .byte $04,$00,$00,$8d,$8e,$02,$11,$02,$8f,$90,$01,$12,$04,$91,$92,$93
        .byte $94,$01,$13,$04,$95,$96,$18,$97,$ff
.else
    helicopter_core_bg_tile_00:
        .byte $21,$0e,$04,$00,$00,$00,$00,$21,$0f,$04,$00,$00,$00,$00,$20,$10
        .byte $06,$09,$09,$00,$00,$00,$00,$21,$11,$03,$00,$00,$00,$21,$12,$02
        .byte $00,$00,$06,$10,$08,$09,$09,$09,$0a,$09,$0a,$09,$0a,$1a,$10,$06
        .byte $0a,$09,$0a,$09,$0a,$09,$26,$10,$1a,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$03,$0e,$02,$00,$00,$03,$0f,$02,$00,$00,$00,$10,$06
        .byte $b4,$b5,$b6,$b7,$b8,$b9,$01,$11,$03,$00,$0d,$af,$01,$12,$02,$00
        .byte $00,$ff

    helicopter_core_bg_tile_01:
        .byte $20,$10,$02,$00,$00,$06,$10,$08,$00,$00,$00,$00,$09,$09,$0a,$09
        .byte $1a,$10,$06,$09,$0a,$09,$09,$00,$00,$01,$0e,$01,$01,$01,$0f,$02
        .byte $02,$03,$00,$10,$06,$00,$00,$06,$07,$08,$00,$03,$11,$02,$0e,$0f
        .byte $03,$12,$02,$1b,$1c,$ff

    helicopter_core_bg_tile_02:
        .byte $0a,$10,$14,$00,$00,$00,$00,$00,$09,$09,$0a,$0a,$c6,$0c,$0a,$0a
        .byte $09,$09,$00,$00,$00,$00,$00,$01,$0e,$04,$00,$00,$a7,$a8,$01,$0f
        .byte $04,$00,$00,$a9,$aa,$02,$10,$02,$ab,$ac,$01,$11,$04,$ad,$ae,$af
        .byte $b0,$01,$12,$04,$b1,$b2,$b3,$5d,$ff

    helicopter_core_bg_tile_03:
        .byte $0f,$10,$0a,$00,$00,$00,$00,$c8,$c9,$00,$00,$00,$00,$03,$0e,$02
        .byte $00,$00,$03,$0f,$02,$00,$00,$00,$10,$06,$b4,$b5,$b6,$b7,$b8,$b9
        .byte $01,$11,$03,$00,$0d,$af,$01,$12,$02,$00,$00,$ff

    helicopter_core_bg_tile_04:
        .byte $0f,$10,$0a,$09,$09,$0a,$0a,$0b,$c7,$0a,$0a,$09,$09,$01,$0e,$01
        .byte $01,$01,$0f,$02,$02,$03,$00,$10,$06,$00,$00,$06,$07,$08,$00,$03
        .byte $11,$02,$0e,$0f,$03,$12,$02,$1b,$1c,$ff

    helicopter_core_bg_tile_05:
        .byte $0a,$10,$14,$09,$09,$0a,$09,$0a,$0a,$0a,$0a,$0a,$0b,$0c,$0a,$0a
        .byte $0a,$0a,$0a,$09,$0a,$09,$09,$01,$0e,$04,$00,$00,$a7,$a8,$01,$0f
        .byte $04,$00,$00,$a9,$aa,$02,$10,$02,$ab,$ac,$01,$11,$04,$ad,$ae,$af
        .byte $b0,$01,$12,$04,$b1,$b2,$b3,$5d,$ff
.endif

; enemy type #$26
crouching_sniper_routine_ptr_tbl:
    .addr crouching_sniper_routine_00 ; set collision, set Y position, set delay
    .addr crouching_sniper_routine_01 ; animate standing up
    .addr crouching_sniper_routine_02 ; fire single bullet after firing delay
    .addr crouching_sniper_routine_03 ; animate crouching and set crouch duration, go to crouching_sniper_routine_01
    .addr crouching_sniper_routine_04 ; enemy destroyed routine - set sprite and destroyed velocity
    .addr crouching_sniper_routine_05 ; apply gravity for delay, before advance routine
    .addr enemy_explosion_routine_00  ; set empty sprite, play optional enemy destroyed sound, disable collisions
    .addr enemy_explosion_routine_01  ; animate explosion sequence
    .addr enemy_explosion_routine_03  ; mark destroyed, remove enemy

; set collision, set Y position, set delay
crouching_sniper_routine_00:
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x       ; bullets travel through enemy and player can collide
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    clc                             ; clear carry in preparation for addition
    adc #$05
    sta ENEMY_Y_POS,x               ; adjust enemy's actual Y position by #$05 (since crouching)
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda #$20
    jmp set_delay_adv_enemy_routine ; set delay to #$20 and set routine to crouching_sniper_routine_01

crouching_sniper_routine_01:
    dec ENEMY_DELAY,x                     ; check animation delay
    bne crouching_sniper_exit             ; branch to update position and exit if delay hasn't elapsed
    inc ENEMY_FRAME,x                     ; move to next animation frame
    jsr crouching_sniper_set_delay_sprite ; set crouching sniper animation delay, sprite and direction
    cpy #$02                              ; compare sniper frame index to #$02
    bcc crouching_sniper_exit             ; branch to continue if not done with animation loop
    lda #$01                              ; completed standing up animation loop
    jsr set_delay_adv_enemy_routine       ; set delay to #$01 and set routine to crouching_sniper_routine_02

crouching_sniper_exit:
    jmp update_enemy_pos ; adjust position based on scroll (does not apply velocity)

; fire single bullet after firing delay
crouching_sniper_routine_02:
    jsr crouching_sniper_set_sprite ; set crouching sniper sprite and direction based on ENEMY_FRAME and player position
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x               ; decrement firing delay
    bne @exit                       ; exit if delay hasn't elapsed
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x       ; enable player-enemy and player bullet-enemy collision
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    sec                             ; set carry flag in preparation for subtraction
    sbc #$05                        ; calculate Y position for generated bullet now that sniper is standing
    sta $08                         ; store Y position in $08
    lda ENEMY_SPRITE_ATTR,x
    asl
    asl
    lda #$0b                        ; assume facing right to add #$0b to sniper X position
    bcs @continue                   ; branch if sniper facing right (horizontal flip sprite)
    lda #$f4                        ; facing left, subtract #$0b from X position

@continue:
    adc ENEMY_X_POS,x               ; calculate X position for generated bullet
    sta $09                         ; store generated bullet Y position in $09
    ldy #$03                        ; set bullet speed code to 1.25x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player       ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)
    lda #$30
    jmp set_delay_adv_enemy_routine ; set delay to #$30 and set routine to crouching_sniper_routine_03
    sta ENEMY_DELAY,x               ; !(UNUSED) unreachable code, jmp just line before

@exit:
    rts

; animate crouching and set crouch duration, go to crouching_sniper_routine_01
crouching_sniper_routine_03:
    dec ENEMY_DELAY,x
    bne crouching_sniper_exit             ; exit if standing after firing delay hasn't elapsed
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x             ; bullets travel through enemy and player can collide
    dec ENEMY_FRAME,x                     ; decrement frame (going from standing to crouching)
    jsr crouching_sniper_set_delay_sprite ; set crouching sniper animation delay, sprite and direction
    cpy #$00                              ; compare sniper frame index to #$00
    bne crouching_sniper_exit             ; exit if not finished crouching
    lda ENEMY_X_POS,x                     ; load enemy's X position
    cmp #$30
    bcc @remove_enemy                     ; remove enemy if the enemy is in the left most ~19% of screen
    lda #$60                              ; enemy still active, set crouching duration
    sta ENEMY_DELAY,x                     ; set crouching duration to #$60
    lda #$02
    jmp set_enemy_routine                 ; set routine to crouching_sniper_routine_01

@remove_enemy:
    jmp remove_enemy

; enemy destroyed routine - set sprite and destroyed velocity
crouching_sniper_routine_04:
    lda ENEMY_SPRITE_ATTR,x
    eor #$40                             ; swapping value of horizontal flip flag
    sta ENEMY_SPRITE_ATTR,x              ; flip sprite horizontally
    lda #$23                             ; sprite_23 (standing sniper)
    jmp set_enemy_destroy_sprite_and_vel ; set enemy sprite to sprite_23 and set destroyed velocity

; apply gravity for delay, before advance routine
crouching_sniper_routine_05:
    lda #$23                                  ; standing sniper sprite
    jmp apply_gravity_adv_routine_after_delay ; set sprite to sprite_23 for fall, apply gravity
                                              ; advance the enemy routine if ENEMY_DELAY has elapsed

; set crouching sniper animation delay, sprite and direction based on ENEMY_FRAME and player position
; input
;  * x - enemy slot index
crouching_sniper_set_delay_sprite:
    lda #$08
    sta ENEMY_DELAY,x

; set crouching sniper sprite and direction based on ENEMY_FRAME and player position
; input
;  * x - enemy slot index
; output
;  * y - current ENEMY_FRAME index
crouching_sniper_set_sprite:
    ldy ENEMY_FRAME,x                 ; load sprite index
    lda crouching_sniper_sprite_tbl,y ; load crouching sniper sprite
    sta ENEMY_SPRITE,x                ; update crouching sniper sprite
    jsr player_enemy_x_dist           ; a = closest x distance to enemy from players, y = closest player (#$00 or #$01)
    lda ENEMY_X_POS,x                 ; load enemy's X position
    cmp PLAYER_SPRITE_X_POS,y         ; compare enemy X position to player X position
    lda #$01                          ; assume player to left of enemy, sniper facing left
    bcs @continue                     ; branch if player to left of enemy
    lda #$41                          ; player is to right of sniper (flip sprite horizontally so sniper faces right)

@continue:
    sta ENEMY_SPRITE_ATTR,x ; set sprite attributes (facing direction)
    ldy ENEMY_FRAME,x       ; load enemy frame index
    rts

; sprites used for crouching sniper
crouching_sniper_sprite_tbl:
    .byte $59 ; sprite_59 (fully crouched)
    .byte $5a ; sprite_5a (partially crouched)
    .byte $5b ; sprite_5b (fully standing)

; enemy type #$27
grass_covered_turret_routine_ptr_tbl:
    .addr grass_covered_turret_routine_00 ; initialize enemy HP, position, shots per attack, and set collision so bullets travel through
    .addr grass_covered_turret_routine_01 ; wait for activation
    .addr grass_covered_turret_routine_02 ; animate activation, set vulnerable to bullets
    .addr grass_covered_turret_routine_03 ; fire 2 shots in short duration, wait long duration, repeat.  remove (still visible) if scrolled too far to left
    .addr grass_covered_turret_routine_04 ; enemy destroyed routine
    .addr enemy_explosion_routine_01      ; animate explosion sequence
    .addr enemy_explosion_routine_03      ; mark destroyed, remove enemy

; initialize enemy HP, position, shots per attack, and set collision so bullets travel through
grass_covered_turret_routine_00:
    lda #$08                  ; HP = #$08, #$0c, or #$0f
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda #$b0
    sta ENEMY_Y_POS,x         ; hard-code Y position to #$b0
    lda ENEMY_X_POS,x         ; load enemy's X position
    sec                       ; set carry flag in preparation for subtraction
    sbc #$04
    sta ENEMY_X_POS,x         ; adjust X position
    lda ENEMY_ATTRIBUTES,x    ; bits 1 and 2 specify one less than number of bullets to fire per attack
    lsr
    and #$03                  ; strip to just one less than number of bullets to fire per attack
    sta ENEMY_VAR_1,x         ; set number of bullets to use when resetting for next attack
    sta ENEMY_VAR_2,x         ; set number of bullets that is decremented when attacking
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for activation
grass_covered_turret_routine_01:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x               ; load enemy's X position
    cmp #$d8
    bcs grass_covered_turret_exit   ; exit if not yet scrolled to activation position (~85% horizontally)
    lda #$01                        ; initial animation delay
    jmp set_delay_adv_enemy_routine ; set animation delay to #$01 and set routine to grass_covered_turret_routine_02

grass_covered_turret_exit:
    rts

; animate activation, set vulnerable to bullets
grass_covered_turret_routine_02:
    lda #$01
    sta ENEMY_SPRITE,x                      ; set invisible sprite, enemy is composed of background tiles
    jsr update_enemy_pos                    ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                       ; decrement animation delay
    bne grass_covered_turret_exit           ; exit if animation delay hasn't elapsed
    jsr grass_covered_turret_anim           ; set super-tiles for turret based on ENEMY_FRAME,x
    bcs grass_covered_turret_set_delay_exit ; exit if unable to draw super-tiles
    inc ENEMY_FRAME,x                       ; updated super-tiles, move to next animation frame for activation
    lda ENEMY_FRAME,x
    cmp #$03
    lda #$0c
    bcc grass_covered_turret_set_delay_exit ; exit if not past last frame of animation
    lda #$01                                ; finished animating activation
    sta ENEMY_FRAME,x                       ; set frame, when firing alternates between #$01 and #$02 to create recoil
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x               ; enable player-enemy and player bullet-enemy collision
    jsr advance_enemy_routine
    lda #$38                                ; set initial firing delay

grass_covered_turret_set_delay_exit:
    sta ENEMY_DELAY,x

grass_covered_turret_exit2:
    rts

; fire 2 shots in short duration, wait long duration, repeat.  remove (still visible) if scrolled too far to left
grass_covered_turret_routine_03:
    lda ENEMY_X_POS,x ; load enemy's X position
    cmp #$40
    bcs @active_enemy
    jmp remove_enemy

@active_enemy:
    jsr update_enemy_pos                    ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x                       ; decrement firing delay
    bne grass_covered_turret_exit2
    jsr grass_covered_turret_anim           ; set super-tiles for turret based on ENEMY_FRAME,x
    bcs grass_covered_turret_set_delay_exit
    lda ENEMY_FRAME,x                       ; ready to fire, swap ENEMY_FRAME,x between #$01 and #$02
    lsr
    lda #$01
    bcc @continue                           ; branch if frame #$02 to switch to frame #$01
    lda #$02                                ; switch from frame #$01 to frame #$02

@continue:
    sta ENEMY_FRAME,x                       ; set firing/not firing frame
    bcs @fire                               ; branch if odd frame
    lda #$08                                ; even frame
    dec ENEMY_VAR_2,x                       ; decrement remaining number of bullets to fire
    bpl grass_covered_turret_set_delay_exit ; exit if more bullets to fire
    lda ENEMY_VAR_1,x                       ; finished firing all shots for attack
    sta ENEMY_VAR_2,x                       ; set one less than number of shots for next round of attack
    lda #$80                                ; load long delay before next attack
    bne grass_covered_turret_set_delay_exit ; always exit

@fire:
    ldy #$02                                ; enemy type = enemy bullet
    jsr try_create_enemy_from_existing      ; create enemy bullet
    bcc grass_covered_turret_exit2          ; branch if unable to create bullet
    ldy $11                                 ; load slot of created bullet
    lda #$00
    sta ENEMY_X_VELOCITY_FRACT,y
    lda #$fe
    sta ENEMY_X_VELOCITY_FAST,y             ; set velocity to -2
    lda ENEMY_Y_POS,y
    clc                                     ; clear carry in preparation for addition
    adc #$02
    sta ENEMY_Y_POS,y                       ; adjust bullet Y position by 2
    lda ENEMY_X_POS,y
    adc #$fe
    sta ENEMY_X_POS,y                       ; adjust bullet X position by -2
    lda #$04                                ; set short firing delay for bullets fired during attack
    jmp grass_covered_turret_set_delay_exit

; enemy destroyed routine
grass_covered_turret_routine_04:
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    ldy #$06
    jsr grass_covered_turret_anim_y ; draw turret as destroyed (frame #$06)
    bcs grass_covered_turret_exit2
    lda ENEMY_X_POS,x               ; load enemy's X position
    clc                             ; clear carry in preparation for addition
    adc #$10
    sta ENEMY_X_POS,x
    jmp enemy_explosion_routine_00  ; set empty sprite, play optional enemy destroyed sound, disable collisions

; set super-tiles for turret based on ENEMY_FRAME,x
; output
;  * carry flag - set when unable to update nametable supertile due to not
;    enough space or not activated, clear when updated
grass_covered_turret_anim:
    lda ENEMY_FRAME,x
    asl
    tay

; set super-tiles for turret based on y
; input
;  * y - frame to set tiles for
; output
;  * carry flag - set when unable to update nametable supertile due to not
;    enough space or not activated, clear when updated
grass_covered_turret_anim_y:
    lda grass_covered_turret_supertiles_tbl+1,y
    sta $0c                                     ; level_3_supertile_data row index (super-tile to draw)
    lda ENEMY_X_POS,x                           ; load enemy's X position
    cmp #$d8
    bcs @exit                                   ; don't animate if not yet in activation position
                                                ; !(OBS) this should never happen because this method is never called until
                                                ; activated, and player can't scroll left, only right
    lda grass_covered_turret_supertiles_tbl,y
    ldy #$01                                    ; x offset = #$18, y offset = #$08 (supertile to the right)
    jsr load_banks_update_enemy_supertiles      ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y
    lda #$01
    rts

@exit:
    clc
    rts

grass_covered_turret_supertiles_tbl:
    .byte $49,$4a ; frame #$00 - barely open
    .byte $4b,$4c ; frame #$01 - fully showing
    .byte $4d,$4e ; frame #$02 - locked into position (frame #$01 shifted slightly left)
    .byte $4f,$50 ; frame #$03 - destroyed

; enemy type #$28
ground_mortar_routine_ptr_tbl:
    .addr ground_mortar_routine_00      ; set hp, destroyed attributes, position, advance routine
    .addr ground_mortar_routine_01      ; wait for activation, pause soldier generation, enable bullet collision,
    .addr ground_mortar_routine_02      ; check for de-activation, fire mortar round based on ENEMY_DELAY,x
    .addr enemy_routine_init_explosions ; enemy destroyed routine - configure for 5 explosions
    .addr enemy_routine_explosions      ; generate 5 explosions, with an #$08 frame delay between each one
    .addr ground_mortar_routine_05      ; update supertiles to destroyed ground mortar
    .addr enemy_explosion_routine_01    ; animate explosion sequence
    .addr enemy_explosion_routine_03    ; mark destroyed, remove enemy

; set hp, destroyed attributes, position, advance routine
ground_mortar_routine_00:
    lda #$20                  ; HP = #$20, #$24, or #$27
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x ; bullets travel through enemy and player can collide
    lda ENEMY_X_POS,x         ; load enemy's X position
    sec                       ; set carry flag in preparation for subtraction
    sbc #$02
    sta ENEMY_X_POS,x
    jsr update_enemy_pos      ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine ; advance to next routine

; wait for activation, pause soldier generation, enable bullet collision,
ground_mortar_routine_01:
    lda #$01
    sta ENEMY_SPRITE,x              ; set invisible sprite
    jsr update_enemy_pos            ; adjust position based on scroll (does not apply velocity)
    lda ENEMY_X_POS,x               ; load enemy's X position
    cmp #$e0
    bcs ground_mortar_exit          ; exit if not yet activated
    lda ENEMY_GEN_CTRL              ; activated, pause random soldier generation
    ora #$40                        ; set bit 6 to pause random enemy generation
    sta ENEMY_GEN_CTRL
    lda ENEMY_Y_POS,x               ; load enemy's Y position
    clc                             ; clear carry in preparation for addition
    adc #$16
    sta ENEMY_Y_POS,x
    lda ENEMY_X_POS,x               ; load enemy's X position
    clc                             ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_X_POS,x
    lda #$00
    sta ENEMY_DESTROY_ATTRS,x       ; enable player-enemy and player bullet-enemy collision
    lda #$07
    sta ENEMY_VAR_1,x               ; set number of mortar rounds to fire per attack round
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to ground_mortar_routine_02

ground_mortar_exit:
    rts

; check for de-activation, fire mortar round based on ENEMY_DELAY,x
ground_mortar_routine_02:
    lda ENEMY_X_POS,x                   ; load enemy's X position
    cmp #$30
    bcc ground_mortar_unpause_enemy_gen ; branch if far to the left to deactivate
    jsr update_enemy_pos                ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne ground_mortar_exit              ; exit if firing delay hasn't elapsed
    ldy #$00
    lda ENEMY_VAR_1,x                   ; load number of mortar rounds remaining to fire this attack
    beq @continue                       ; branch if no more to fire
    ldy #$02                            ; set supertiles for firing ground mortar with recoil
    cmp #$07
    bne @create_mortar_round            ; branch if firing first mortar round

@continue:
    jsr ground_mortar_anim_y
    bcs ground_mortar_exit

@create_mortar_round:
    ldy #$29                           ; enemy type = mortar round
    jsr try_create_enemy_from_existing ; create mortar round
    bcc @dec_mortar_round              ; branch if unable to create mortar round
    ldy $11                            ; load slot of created enemy
    lda ENEMY_VAR_1,x                  ; load ground mortar round initial velocity index
    sta ENEMY_VAR_1,y                  ; set mortar round initial velocity index (indexes into mortar_round_vel_tbl)
    lda ENEMY_Y_POS,y
    sec                                ; set carry flag in preparation for subtraction
    sbc #$0e
    sta ENEMY_Y_POS,y
    lda ENEMY_X_POS,y
    sec                                ; set carry flag in preparation for subtraction
    sbc #$08
    sta ENEMY_X_POS,y

@dec_mortar_round:
    lda #$02
    dec ENEMY_VAR_1,x   ; decrement number of mortars fired
    bpl @set_delay_exit ; branch if more to fire
    lda #$07            ; fired all mortars, reset counter and use longer delay
    sta ENEMY_VAR_1,x
    lda #$80

@set_delay_exit:
    sta ENEMY_DELAY,x

ground_mortar_exit2:
    rts

ground_mortar_unpause_enemy_gen:
    lda ENEMY_GEN_CTRL ; load various control flags for enemy generation
    and #$bf           ; clear bit 6
    sta ENEMY_GEN_CTRL ; unpause enemy generation
    jmp remove_enemy

; update supertiles to destroyed ground mortar
ground_mortar_routine_05:
    ldy #$04                       ; set ground mortar destroyed supertiles
    jsr ground_mortar_anim_y       ; set destroyed supertiles
    bcs ground_mortar_exit2        ; exit if unable to update supertiles
    lda ENEMY_GEN_CTRL
    and #$bf                       ; clear bit 6 (enable random soldier generation)
    sta ENEMY_GEN_CTRL
    jmp enemy_explosion_routine_00 ; set empty sprite, play optional enemy destroyed sound, disable collisions

ground_mortar_anim_y:
    lda ground_mortar_supertiles_tbl+1,y
    sta $0c
    lda ground_mortar_supertiles_tbl,y
    ldy #$10                               ; x offset = #$08, y offset = #$18 (one half supertile below)
    jmp load_banks_update_enemy_supertiles ; update 2 supertiles (a and $0c) at enemy position and location offset encoded in y

ground_mortar_supertiles_tbl:
    .byte $3e,$40 ; not firing ground mortar at rest
    .byte $46,$47 ; firing mortar recoil
    .byte $11,$48 ; destroyed

; enemy type #$29
mortar_round_routine_ptr_tbl:
    .addr mortar_round_routine_00    ; set hp, destroy attributes, sprite palette, and initial velocity based on ENEMY_VAR_1,x
    .addr mortar_round_routine_01    ; apply gravity, check for apex of arc, if so play sound_17, check bg collision, if so, advance to destroy routine
    .addr grenade_routine_02         ; enemy destroyed routine
    .addr enemy_explosion_routine_01 ; animate explosion sequence
    .addr enemy_explosion_routine_03 ; mark destroyed, remove enemy

; set hp, destroy attributes, sprite palette, and initial velocity based on ENEMY_VAR_1,x
mortar_round_routine_00:
    lda #$02
    sta ENEMY_HP,x               ; set hp to #$02
    lda #$18                     ; enable collision, spike explosion, sound_12 (HARETSU) destroy sound
    sta ENEMY_DESTROY_ATTRS,x    ; set enemy destroyed attribute
    lda #$01
    sta ENEMY_SPRITE_ATTR,x      ; set sprite palette
    lda ENEMY_VAR_1,x            ; load initial velocity index (set in ground_mortar_routine_02)
    asl
    asl                          ; quadruple since each entry is #$04 bytes
    tay                          ; transfer initial velocity index to offset register
    lda mortar_round_vel_tbl,y   ; load enemy's fractional Y velocity
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda mortar_round_vel_tbl+1,y ; load enemy's fast Y velocity
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda mortar_round_vel_tbl+2,y ; load enemy's fractional X velocity
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    lda mortar_round_vel_tbl+3,y ; load enemy's fast X velocity
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    jsr update_enemy_pos         ; adjust position based on scroll (does not apply velocity)
    jmp advance_enemy_routine    ; advance to next routine

; initial mortar round velocity
mortar_round_vel_tbl:
    .byte $00,$fd,$90,$ff ; x vel = -0.4375, y vel = -3.000
    .byte $20,$fd,$a0,$ff ; x vel = -0.3750, y vel = -2.875
    .byte $e0,$fc,$c0,$ff ; x vel = -0.2500, y vel = -3.125
    .byte $20,$fd,$d0,$ff ; x vel = -0.1875, y vel = -2.875
    .byte $c0,$fc,$e0,$ff ; x vel = -0.1250, y vel = -3.250
    .byte $80,$fd,$80,$ff ; x vel = -0.5000, y vel = -2.500
    .byte $a0,$fc,$60,$ff ; x vel = -0.6250, y vel = -3.375
    .byte $40,$fc,$50,$ff ; x vel = -0.6875, y vel = -3.750

; apply gravity, check for apex of arc, if so play sound_17, check bg collision, if so, advance to destroy routine
mortar_round_routine_01:
    lda #$3a                     ; sprite_3a
    sta ENEMY_SPRITE,x           ; set sprite to sprite_3a
    lda ENEMY_Y_VELOCITY_FRACT,x ; load enemy's fractional Y velocity
    clc                          ; clear carry in preparation for addition
    adc #$08
    sta ENEMY_Y_VELOCITY_FRACT,x ; apply gravity
    lda ENEMY_Y_VELOCITY_FAST,x  ; load enemy's fast Y velocity
    sta $08
    adc #$00                     ; add any carry
    sta ENEMY_Y_VELOCITY_FAST,x  ; add any accumulated fractional velocity to fast velocity
    tay
    eor $08                      ; check to see if direction changed
                                 ; bit 7 will only be set if previous fast velocity was negative
                                 ; and new velocity is zero (or positive)
                                 ; technically, going from positive to zero would also work,
                                 ; but that's not applicable here
    bpl @check_collision         ; branch if direction hasn't changed
    tya                          ; mortar round is starting to fall, transfer fast velocity to a
    bmi @check_collision         ; branch if new direction is negative (upward)
                                 ; !(OBS) I don't think this ever could happen
    lda #$17                     ; sound_17 (mortar round) - silent (no actual audio for this sound)
    jsr play_sound               ; play sound_17 (mortar round) - silent
                                 ; !(UNUSED) there is no sound for sound_17 so no audio is heard

@check_collision:
    jsr update_pos_check_offscreen           ; adjust position based on scroll (does not apply velocity)
    lda #$00                                 ; don't adjust mortar round Y position when calculating bg collision
    jsr get_enemy_bg_collision_code_onscreen ; get bg collision code if on-screen
    tay                                      ; transfer collision code to y
    lda bg_collision_test_tbl,y              ; see if collision should destroy mortar round
    beq @exit                                ; exit if collision doesn't destroy mortar round
    jmp advance_enemy_routine                ; mortar round collided with bg, advance to next routine to destroy

@exit:
    rts

; enemy type #$3f
jungle_earthquake_routine_ptr_tbl:
    .addr jungle_earthquake_routine_00 ; backup Y scroll, disable player bullet collision, set animation delay, advance routine
    .addr jungle_earthquake_routine_01 ; shake screen with sound, until player at correct screen on level
    .addr jungle_earthquake_routine_02 ; collapse floor shaking screen until collapsed, go to jungle_earthquake_routine_01

; backup Y scroll, disable player bullet collision, set animation delay, advance routine
jungle_earthquake_routine_00:
    lda Y_SCROLL                ; load PPU vertical scroll
    sta BACKUP_Y_SCROLL
    lda #$81
    sta ENEMY_DESTROY_ATTRS,x   ; bullets travel through enemy and player can collide
    lda #$01
    sta ENEMY_ANIMATION_DELAY,x
    jmp advance_enemy_routine   ; advance to next routine

; shake screen with sound, until player at correct screen on level
jungle_earthquake_routine_01:
    jsr jungle_earthquake           ; vertical shake and play earthquake shake (sound_08)
    lda ENEMY_VAR_1,x
    asl
    tay
    lda X_SCREEN                    ; load number of horizontal nametable screens scrolled
    cmp jungle_earthquake_x_tbl,y
    bcc jungle_earthquake_exit      ; exit if not yet at screen
    bne @continue
    lda X_SCROLL                    ; load PPU horizontal scroll
    cmp jungle_earthquake_x_tbl+1,y
    bcc jungle_earthquake_exit      ; exit if not at at position within screen

@continue:
    lda #$20                        ; sound_20 (JIWARE)
    jsr play_sound                  ; play sound_20 (JIWARE)
    lda #$00
    sta ENEMY_VAR_2,x
    lda #$14
    sta ENEMY_VAR_3,x               ; load how low the collapsing floor sinks to
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to jungle_earthquake_routine_02

; collapse floor shaking screen until collapsed, go to jungle_earthquake_routine_01
jungle_earthquake_routine_02:
    jsr jungle_earthquake                 ; vertical shake and play earthquake shake (sound_08)
    dec ENEMY_DELAY,x
    bne jungle_earthquake_exit
    jsr jungle_earthquake_write_graphics
    lda #$01
    bcs @continue
    lda ENEMY_VAR_3,x
    cmp $0d
    beq jungle_earthquake_next_checkpoint
    inc ENEMY_VAR_3,x
    lda #$08

@continue:
    sta ENEMY_DELAY,x

jungle_earthquake_exit:
    rts

jungle_earthquake_next_checkpoint:
    inc ENEMY_VAR_1,x     ; move to next checkpoint position
    lda ENEMY_VAR_1,x
    cmp #$0f
    bcs @restore_y_scroll ; branch if finished all checkpoints to remove enemy
    lda #$02              ; go back one routine to create effect based on current checkpoint
    jmp set_enemy_routine ; set routine to jungle_earthquake_routine_01

@restore_y_scroll:
    lda BACKUP_Y_SCROLL
    sta Y_SCROLL        ; restore PPU vertical scroll value
    jmp remove_enemy

jungle_earthquake_write_graphics:
    lda ENEMY_VAR_1,x                         ; load checkpoint index
    asl
    asl                                       ; quadruple since each entry is #$04 bytes
    tay                                       ; transfer to offset register for jungle_earthquake_ctrl_tbl
    lda jungle_earthquake_ctrl_tbl,y
    sta $0a
    lda jungle_earthquake_ctrl_tbl+1,y
    sta $0d
    lda #$04
    sta $01
    lda jungle_earthquake_ctrl_tbl+2,y
    asl
    sta $00
    rol $01
    lda ENEMY_VAR_3,x
    asl
    asl
    asl
    asl
    rol $01
    asl
    rol $01
    clc                                       ; clear carry in preparation for addition
    adc $00
    sta $00
    lda jungle_earthquake_ctrl_tbl+3,y
    tay
    lda jungle_quake_column_tiles_ptr_tbl,y
    sta $08
    lda jungle_quake_column_tiles_ptr_tbl+1,y
    sta $09
    lda ENEMY_VAR_3,x
    lsr
    bcc @write_graphics_to_buffer
    lda $01                                   ; clearing some bg collisions
    sta $02
    lda $00
    asl
    rol $02
    asl
    rol $02
    asl
    lsr $02
    ror
    lsr $02
    ror
    lsr $02
    ror
    lsr $02
    ror
    lsr $02
    ror
    tay
    lda $0a                                   ; load number of bg collisions to update
    lsr
    lsr                                       ; each bg collision is represented by 1/4 of a byte
                                              ; so divide by 4
    sta $02                                   ; set number of bg collision bytes to write
    lda #$00                                  ; clearing bg collision data

@set_bg_collision:
    sta BG_COLLISION_DATA,y
    iny                     ; increment collision data write offset
    dec $02                 ; decrement write counter
    bne @set_bg_collision

@write_graphics_to_buffer:
    lda #$00
    ldy ENEMY_VAR_2,x
    beq @calc_ppu_addr
    clc

@calc_offset:
    adc $0a
    dey
    bne @calc_offset

@calc_ppu_addr:
    tay                       ; set graphics data read offset (jungle_quake_column_tiles_xx offset)
    lda #$1b
    sec                       ; set carry flag in preparation for subtraction
    sbc ENEMY_VAR_3,x
    bcs @write_graphic_blocks
    lda #$00

@write_graphic_blocks:
    sta $0c                      ; set PPU address high byte
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$20
    bcs @restore_x_exit
    lda #$03
    sta CPU_GRAPHICS_BUFFER,x    ; set repeat mode
    inx                          ; increment graphics buffer write offset
    jsr @write_block_mode_header
    lda #$00
    sta CPU_GRAPHICS_BUFFER,x
    inx                          ; increment graphics buffer write offset
    lda $0c
    beq @set_buffer_offset_exit
    lda #$06                     ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x
    inx                          ; increment graphics buffer write offset

@write_block:
    jsr @write_block_mode_header
    sta $0b                      ; set graphics block size

@graphics_write_loop:
    lda ($08),y               ; load graphic byte from jungle_quake_column_tiles_xx
    sta CPU_GRAPHICS_BUFFER,x
    iny                       ; increment graphics data read offset
    inx                       ; increment graphics buffer write offset
    dec $0b                   ; decrement total number of bytes to write
    bne @graphics_write_loop
    dec $0c
    bne @write_block
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x
    inx

@set_buffer_offset_exit:
    stx GRAPHICS_BUFFER_OFFSET
    clc

@restore_x_exit:
    ldx ENEMY_CURRENT_SLOT
    rts

; input
;  * $00 - PPU address low byte
;  * $01 - PPU address high byte
;  * $0a - graphics block size
; output
;  * a - graphics block size
;  * $00 - next nametable row PPU address low byte
;  * $01 - next nametable row PPU address high byte
@write_block_mode_header:
    lda $01
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address high byte
    inx                       ; increment graphics buffer write offset
    lda $00
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address low byte
    inx                       ; increment graphics buffer write offset
    lda $00
    clc                       ; clear carry in preparation for addition
    adc #$20
    sta $00                   ; move down one nametable row
    bcc @block_size
    inc $01                   ; handle overflow by moving to next PPU address high byte

@block_size:
    lda $0a                   ; load block size (number of graphics bytes in block)
    sta CPU_GRAPHICS_BUFFER,x ; set graphics block size
    inx                       ; increment graphics buffer write offset
    rts

jungle_earthquake_x_tbl:
    .byte $0e,$e0 ; X_SCREEN = #$0e, X_SCROLL = #$e0
    .byte $0f,$00 ; X_SCREEN = #$0f, X_SCROLL = #$00
    .byte $0f,$20 ; X_SCREEN = #$0f, X_SCROLL = #$20
    .byte $0f,$70 ; X_SCREEN = #$0f, X_SCROLL = #$70
    .byte $0f,$b0 ; X_SCREEN = #$0f, X_SCROLL = #$b0
    .byte $0f,$d0 ; X_SCREEN = #$0f, X_SCROLL = #$d0
    .byte $10,$60 ; X_SCREEN = #$10, X_SCROLL = #$60
    .byte $10,$a0 ; X_SCREEN = #$10, X_SCROLL = #$a0
    .byte $10,$e0 ; X_SCREEN = #$10, X_SCROLL = #$e0
    .byte $11,$40 ; X_SCREEN = #$11, X_SCROLL = #$40
    .byte $11,$80 ; X_SCREEN = #$11, X_SCROLL = #$80
    .byte $11,$c0 ; X_SCREEN = #$11, X_SCROLL = #$c0
    .byte $11,$e0 ; X_SCREEN = #$11, X_SCROLL = #$e0
    .byte $12,$00 ; X_SCREEN = #$12, X_SCROLL = #$00
    .byte $12,$20 ; X_SCREEN = #$12, X_SCROLL = #$20

; byte 4 - jungle_quake_column_tiles_ptr_tbl offset
jungle_earthquake_ctrl_tbl:
    .byte $04,$17,$88,$00 ; ENEMY_VAR_1,x = #$00
    .byte $04,$1d,$8a,$02 ; ENEMY_VAR_1,x = #$01
    .byte $08,$15,$8c,$04 ; ENEMY_VAR_1,x = #$02
    .byte $08,$19,$00,$04 ; ENEMY_VAR_1,x = #$03
    .byte $08,$15,$04,$04 ; ENEMY_VAR_1,x = #$04
    .byte $04,$1d,$08,$00 ; ENEMY_VAR_1,x = #$05
    .byte $04,$1d,$80,$00 ; ENEMY_VAR_1,x = #$06
    .byte $04,$1d,$84,$00 ; ENEMY_VAR_1,x = #$07
    .byte $04,$1d,$88,$00 ; ENEMY_VAR_1,x = #$08
    .byte $04,$1d,$8c,$00 ; ENEMY_VAR_1,x = #$09
    .byte $08,$19,$00,$04 ; ENEMY_VAR_1,x = #$0a
    .byte $08,$15,$04,$04 ; ENEMY_VAR_1,x = #$0b
    .byte $04,$1d,$08,$00 ; ENEMY_VAR_1,x = #$0c
    .byte $04,$19,$0a,$02 ; ENEMY_VAR_1,x = #$0d
    .byte $04,$17,$0c,$00 ; ENEMY_VAR_1,x = #$0e

jungle_quake_column_tiles_ptr_tbl:
    .addr jungle_quake_column_tiles_00
    .addr jungle_quake_column_tiles_01
    .addr jungle_quake_column_tiles_02

jungle_quake_column_tiles_00:
    .byte $2a,$2a,$2b,$2b ; grass top
    .byte $c5,$c6,$c5,$c6 ; top of rocks
    .byte $2e,$c9,$30,$cf ; rocks
    .byte $cb,$cc,$d1,$d2
    .byte $30,$d0,$2f,$ca
    .byte $cb,$cc,$cd,$ce
    .byte $30,$cf,$31,$d0
    .byte $d1,$d2,$d3,$d4
    .byte $2f,$ca,$2e,$c9
    .byte $cd,$ce,$cb,$cc

jungle_quake_column_tiles_01:
    .byte $2c,$2b,$2b,$2c ; grass top
    .byte $c5,$c6,$c7,$c8 ; top of rocks
    .byte $2e,$c9,$2f,$ca ; rocks
    .byte $cb,$cc,$cd,$ce
    .byte $30,$cf,$31,$d0
    .byte $d1,$d2,$d3,$d4
    .byte $2f,$ca,$2e,$c9
    .byte $cd,$ce,$cb,$cc
    .byte $31,$d0,$30,$d0
    .byte $d3,$d4,$cb,$cc

jungle_quake_column_tiles_02:
    .byte $2a,$2a,$2b,$2b ; grass top
    .byte $2c,$2b,$2b,$2c ; top of rocks
    .byte $c5,$c6,$c5,$c6 ; rocks
    .byte $c5,$c6,$c7,$c8
    .byte $2e,$c9,$30,$cf
    .byte $2e,$c9,$2f,$ca
    .byte $cb,$cc,$d1,$d2
    .byte $cb,$cc,$cd,$ce
    .byte $30,$d0,$2f,$ca
    .byte $30,$cf,$31,$d0
    .byte $cb,$cc,$cd,$ce
    .byte $d1,$d2,$d3,$d4
    .byte $30,$cf,$31,$d0
    .byte $2f,$ca,$2e,$c9
    .byte $d1,$d2,$d3,$d4
    .byte $cd,$ce,$cb,$cc
    .byte $2f,$ca,$2e,$c9
    .byte $31,$d0,$30,$d0
    .byte $cd,$ce,$cb,$cc
    .byte $d3,$d4,$cb,$cc

; enemy type #$3a
robot_spider_bullet_routine_ptr_tbl:
    .addr robot_spider_bullet_routine_00 ; backup Y position, set sprite to muzzle flash, advance routine
    .addr robot_spider_bullet_routine_01 ; wait for muzzle flash delay, set metal bullet sprite, advance routine
    .addr robot_spider_bullet_routine_02 ; animate bullet sprite palette, set bullet path
    .addr remove_enemy                   ; enemy destroyed routine

; backup Y position, set sprite to muzzle flash, advance routine
robot_spider_bullet_routine_00:
    lda ENEMY_Y_POS,x           ; load enemy's Y position
    sta ENEMY_VAR_1,x           ; set bullet's initial Y position
    jmp enemy_bullet_routine_00 ; set sprite to muzzle flash, advance routine

; wait for muzzle flash delay, set metal bullet sprite, advance routine
robot_spider_bullet_routine_01:
    jsr update_enemy_pos         ; adjust position based on scroll (does not apply velocity)
    dec ENEMY_DELAY,x
    bne robot_spider_bullet_exit ; exit if delay hasn't elapsed
    lda #$06                     ; sprite_06 (metal bullet sprite)
    sta ENEMY_SPRITE,x           ; set sprite_06
    jmp advance_enemy_routine    ; advance to next routine

; animate bullet sprite palette, set bullet path
robot_spider_bullet_routine_02:
    inc ENEMY_DELAY,x
    lda ENEMY_DELAY,x
    lsr
    lsr
    and #$03
    sta ENEMY_SPRITE_ATTR,x ; set sprite palette
    lda ENEMY_VAR_3,x       ; see if bullet straight or wobbly
    beq @continue           ; branch if bullet proceeds along straight line
    ldy #$05                ; wobbly bullet
    jsr modify_enemy_y_vel  ; adjust Y velocity based on ENEMY_Y_POS,x distance from ENEMY_VAR_1,x

@continue:
    jsr apply_velocity              ; apply enemy's velocity to its position, removing enemy if off-screen
    lda #$f8                        ; add -8 to enemy Y position when determining background collision
    jsr get_enemy_bg_collision_code ; test enemy bg collision
    beq robot_spider_bullet_exit
    jmp remove_enemy

robot_spider_bullet_exit:
    rts

; enemy type #$4e
robot_spider_routine_ptr_tbl:
    .addr robot_spider_routine_00
    .addr robot_spider_routine_01
    .addr robot_spider_routine_02
    .addr robot_spider_routine_03
    .addr robot_spider_routine_04        ; enemy destroyed routine
    .addr enemy_routine_boss_defeated_01
    .addr robot_spider_routine_06
    .addr enemy_explosion_routine_01     ; animate explosion sequence
    .addr robot_spider_routine_08

robot_spider_routine_00:
    lda #$8d
    sta ENEMY_DESTROY_ATTRS,x
    lda #$f0                    ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    lda #$08
    sta ENEMY_ANIMATION_DELAY,x
    jmp advance_enemy_routine   ; advance to next routine

robot_spider_routine_01:
    lda SCREEN_SCROLL_TYPE          ; 0 = horizontal, 1 = vertical/overhead
    beq robot_spider_exit
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne robot_spider_exit
    lda #$01
    sta NT_MIRRORING                ; set nametable mirroring (0: vertical; 1: horizontal)
    lda #$9c
    sta ENEMY_Y_POS,x
    lda #$60
    sta ENEMY_X_POS,x
    lda #$01
    sta ENEMY_VAR_7,x
    lda #$2c
    sta ENEMY_VAR_1,x
    lda #$00
    sta ENEMY_VAR_2,x
    lda #$1e
    sta ENEMY_VAR_4,x
    lda #$0c
    sta $0a
    ldx GRAPHICS_BUFFER_OFFSET
    jsr robot_spider_set_floor_attr ; set the attributes for the bottom of the screen
    lda #$01
    jmp set_delay_adv_enemy_routine ; set delay to #$01 and set routine to robot_spider_routine_02

robot_spider_exit:
    rts

; drawing floor pattern table tiles to the nametable
robot_spider_routine_02:
    lda #$00
    ldy ENEMY_VAR_4,x
    cpy #$08
    bcs @continue
    lda #$ac          ; pattern tile index for floor tile
    cpy #$07
    bcs @continue
    lda #$34          ; pattern tile index for solid gray (under floor tile)
    cpy #$06
    bcs @continue
    lda #$b2          ; pattern tile index for solid gray with light gray line
                      ; (under floor tile)
    cpy #$05
    bcs @continue
    lda #$34

@continue:
    jsr set_nametable_row                ; write a row of tile specified in register a to the nametable
                                         ; ENEMY_VAR_1,x - PPU address high byte, ENEMY_VAR_2,x - PPU address low byte
    dec ENEMY_VAR_4,x
    bpl robot_spider_exit
    ldy #$57
    jsr clear_partial_irq_collision_data ; clear the first #$58 bytes of SECOND_BG_COLLISION_DATA
    lda #$32                             ; sound_32 (BOSS2BGM) Creature from Outer Space (Boss 3)
    jsr play_sound                       ; play sound_32 (BOSS2BGM)
    lda #$00
    sta ENEMY_VAR_2,x
    sta ENEMY_VAR_3,x
    sta ENEMY_VAR_1,x
    lda #$01
    sta ENEMY_VAR_4,x
    jmp advance_enemy_routine            ; advance to next routine

robot_spider_routine_03:
    lda #$01          ; invisible sprite
    ldy ENEMY_VAR_7,x
    bne @continue
    lda #$06          ; sprite_06

@continue:
    sta ENEMY_SPRITE,x
    lda GLOBAL_TIMER
    lsr
    lsr
    and #$03
    ora #$20
    sta ENEMY_SPRITE_ATTR,x
    jsr robot_spider_draw
    jsr run_robot_spider_subroutine
    lda #$0c
    ldy ENEMY_VAR_7,x
    beq @set_destroy_attrs
    lda #$8d

@set_destroy_attrs:
    sta ENEMY_DESTROY_ATTRS,x

robot_spider_set_nametable:
    lda PPUCTRL_SETTINGS
    and #$fd             ; strip previous nametable setting
    ora ENEMY_VAR_2,x    ; merge with new nametable to use (0 = $2000, 2 = $2800)
    sta PPUCTRL_SETTINGS
    rts

robot_spider_routine_06:
    lda #$05
    sta ENEMY_VAR_3,x
    jmp enemy_explosion_routine_00

robot_spider_routine_08:
    lda ENEMY_VAR_3,x
    cmp #$03
    bcc @continue
    jmp robot_spider_clear_bg_draw_floor ; draw black background or floor depending on ENEMY_VAR_3,x

@continue:
    lda ENEMY_VAR_2,x
    eor #$02
    sta ENEMY_VAR_2,x
    and #$02
    beq @prep_destroy
    lda #$05
    sta ENEMY_VAR_3,x
    jmp robot_spider_set_nametable

@prep_destroy:
    ldy #$57
    jsr clear_partial_bg_collision_data
    ldy #$57
    jsr clear_partial_irq_collision_data
    jsr clear_y_scroll
    jmp enemy_explosion_routine_03

run_robot_spider_subroutine:
    lda ENEMY_VAR_1,x
    jsr run_routine_from_tbl_below

robot_spider_subroutine_ptr_tbl:
    .addr robot_spider_subroutine_00
    .addr robot_spider_subroutine_01
    .addr robot_spider_subroutine_02

robot_spider_subroutine_00:
    jsr robot_spider_walk
    lda ENEMY_VAR_7,x
    bne robot_spider_subroutine_exit
    lda ENEMY_X_POS,x                ; load enemy's X position
    cmp #$c0
    bcs robot_spider_subroutine_exit
    lda ENEMY_FRAME,x
    bne robot_spider_subroutine_exit
    lda #$30                         ; HP = #$30, #$40, or #$48
    jsr set_enemy_hp_hard            ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)

robot_spider_set_18_delay_adv_subroutine:
    lda #$18

robot_spider_set_delay_adv_subroutine:
    sta ENEMY_DELAY,x
    inc ENEMY_VAR_1,x

robot_spider_subroutine_exit:
    rts

robot_spider_subroutine_01:
    lda ENEMY_DELAY,x
    cmp #$10
    bne @continue
    jsr robot_spider_create_bullets

@continue:
    dec ENEMY_DELAY,x
    bne robot_spider_subroutine_exit
    ldy ENEMY_X_POS,x                ; load enemy's X position
    cpy PLAYER_SPRITE_X_POS
    lda #$00
    bcc @set_dir_hp
    lda #$01

@set_dir_hp:
    sta ENEMY_VAR_4,x            ; set direction (0 = to the right, 1 = to the left)
    lda #$60
    ldy ENEMY_HP,x
    cpy #$10
    bcs @randomly_adv_subroutine
    lda #$40

@randomly_adv_subroutine:
    sta $00
    lda RANDOM_NUM                            ; load random number
    and #$1f
    adc $00
    bne robot_spider_set_delay_adv_subroutine

robot_spider_subroutine_02:
    jsr robot_spider_walk
    dec ENEMY_DELAY,x
    bne @exit
    lda ENEMY_FRAME,x
    beq @continue
    inc ENEMY_DELAY,x
    rts

@continue:
    jsr robot_spider_set_18_delay_adv_subroutine
    lda #$01
    sta ENEMY_VAR_1,x

@exit:
    rts

robot_spider_create_bullets:
    lda #$05
    sta $08  ;  creating #$06 robot spider bullets

@spider_bullet_create_loop:
    ldy #$3a                                ; enemy type = robot spider bullet
    jsr try_create_enemy_from_existing      ; create robot spider bullet
    bcc @restore_slot_exit                  ; branch if unable to create spider bullet
    lda ENEMY_Y_POS,x                       ; load robot spider's Y position
    sta $0a
    lda ENEMY_X_POS,x                       ; load robot spider's X position
    sta $0b
    ldx $11                                 ; load enemy slot of created robot spider bullet
    lda $08                                 ; load remaining number of bullets to create
    asl                                     ; double since each entry is #$02 bytes
    tay                                     ; transfer to offset register
    lda robot_spider_bullet_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x            ; store bullet's fractional Y velocity
    lda robot_spider_bullet_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x             ; store bullet's fast Y velocity
    lda robot_spider_bullet_x_vel_tbl,y
    sta ENEMY_X_VELOCITY_FRACT,x            ; store bullet's fractional X velocity
    lda robot_spider_bullet_x_vel_tbl+1,y
    sta ENEMY_X_VELOCITY_FAST,x             ; store bullet's fast X velocity
    ldy $08                                 ; load bullet index
    lda $0a                                 ; load robot spider's Y position
    clc                                     ; clear carry in preparation for addition
    adc robot_spider_bullet_y_pos_adj_tbl,y
    sta ENEMY_Y_POS,x                       ; set robot spider bullet's initial Y position
    lda robot_spider_bullet_x_pos_adj_tbl,y
    clc                                     ; clear carry in preparation for addition
    adc $0b                                 ; add robot spider's X position
    sta ENEMY_X_POS,x                       ; set robot spider bullet's initial X position
    lda robot_spider_bullet_behavior_tbl,y
    sta ENEMY_VAR_3,x                       ; set bullet behavior (0 = straight, 1 = wobbly)
    ldx ENEMY_CURRENT_SLOT                  ; restore x to enemy current slot
    dec $08                                 ; decrement number of bullets to create
    bpl @spider_bullet_create_loop

@restore_slot_exit:
    ldx ENEMY_CURRENT_SLOT ; restore x to enemy current slot

robot_spider_exit2:
    rts

robot_spider_bullet_y_vel_tbl:
    .byte $96,$fe ; y vel = -1.41
    .byte $00,$04 ; y vel =  4.00
    .byte $6a,$01 ; y vel =  1.41
    .byte $96,$fe ; y vel = -1.41
    .byte $00,$04 ; y vel =  4.00
    .byte $6a,$01 ; y vel =  1.41

robot_spider_bullet_x_vel_tbl:
    .byte $6a,$01 ; x vel =  1.41
    .byte $00,$02 ; x vel =  2.00
    .byte $6a,$01 ; x vel =  1.41
    .byte $96,$fe ; x vel = -1.41
    .byte $00,$fe ; x vel = -2.00
    .byte $96,$fe ; x vel = -1.41

robot_spider_bullet_y_pos_adj_tbl:
    .byte $e6,$f2,$02,$e6,$f2,$02

robot_spider_bullet_x_pos_adj_tbl:
    .byte $1e,$22,$1e,$e2,$de,$e2

robot_spider_bullet_behavior_tbl:
    .byte $00,$01,$00,$00,$01,$00

robot_spider_walk:
    dec ENEMY_ANIMATION_DELAY,x
    bne robot_spider_exit2
    lda ENEMY_VAR_7,x
    bne @calc_pace_from_hp
    jsr player_enemy_x_dist
    lda ENEMY_X_POS,x           ; load enemy's X position
    sec
    sbc PLAYER_SPRITE_X_POS,y
    bcs @calc_pace              ; branch if robot spider to right of player
    eor #$ff
    adc #$01

@calc_pace:
    cmp #$20
    lda #$05
    bcc @set_pace ; branch if player is close to robot spider

@calc_pace_from_hp:
    lda #$07       ; robot spider slow pace
    ldy ENEMY_HP,x
    cpy #$10
    bcs @set_pace  ; branch if robot spider HP is still high
    lda #$06       ; robot spider medium pace
    cpy #$08
    bcs @set_pace  ; branch if robot spider HP is #$09
    lda #$05       ; robot spider HP is less than #$08, quicken pace

@set_pace:
    sta ENEMY_ANIMATION_DELAY,x
    lda ENEMY_VAR_4,x           ; load robot spider direction (0 = to the right, 1 = to the left)
    lsr
    lda #$ff
    ldy #$08
    bcc @next_frame             ; branch if robot spider moving to the right
    lda #$01
    ldy #$00

; add one to ENEMY_FRAME,x
@next_frame:
    clc
    adc ENEMY_FRAME,x
    cmp #$09
    bcc @set_frame    ; branch if didn't pass last frame
    tya               ; reset the frame index to #$00

@set_frame:
    sta ENEMY_FRAME,x                      ; set next frame in loop
    jsr robot_spider_x_pos_adj_for_frame
    lda robot_spider_x_pos_adj_tbl,y
    clc
    adc ENEMY_X_POS,x
    sta ENEMY_X_POS,x                      ; move robot spider to new position
    lda ENEMY_VAR_7,x                      ; load previous location offscreen indicator
    adc robot_spider_overflow_offset_tbl,y ; add overflow offset
                                           ; e.g. if previously off-screen to left (#$ff) and overflow when determining X position
                                           ; then the robot spider is now on the screen
                                           ; this offsets for when overflow is expected like walking left
    sta ENEMY_VAR_7,x                      ; set nametable indicator
                                           ; #$ff off screen to left, #$00 on screen, #$01 off screen to right
    lda robot_spider_sound_for_frame,y
    beq @set_nt_dir_delay                  ; branch if not playing sound for frame
    lda #$1f                               ; sound_1f (ARUKU)
    jsr play_sound                         ; play sound_1f (ARUKU)

@set_nt_dir_delay:
    lda ENEMY_VAR_2,x               ; load current nametable being used (0 = $2000, 2 = $2800)
    eor #$02
    sta ENEMY_VAR_2,x               ; move to other nametable (0 = $2000, 2 = $2800)
    lda #$05
    sta ENEMY_VAR_3,x
    lda ENEMY_VAR_7,x
    bne robot_spider_restore_x_exit ; exit if not on screen
    lda ENEMY_VAR_4,x               ; robot on scree
                                    ; load robot spider direction (0 = to the right, 1 = to the left)
    lsr
    lda #$e0                        ; right edge of screen
    bcc @set_dir                    ; branch if moving right
    lda #$28                        ; left edge of screen

@set_dir:
    cmp ENEMY_X_POS,x
    rol                             ; push carry into bit 0
    eor ENEMY_VAR_4,x
    lsr
    bcs robot_spider_restore_x_exit ; branch if not past edge, and no need to turn around
    lda ENEMY_VAR_4,x               ; at edge and walking toward edge, turn around
    eor #$01
    sta ENEMY_VAR_4,x

robot_spider_restore_x_exit:
    ldx ENEMY_CURRENT_SLOT
    rts

; get x position adjustment based on current ENEMY_FRAME,x and direction
; input
;  * x - enemy slot index
; output
;  * y - robot_spider_x_pos_adj_tbl offset
robot_spider_x_pos_adj_for_frame:
    lda ENEMY_FRAME,x ; #$00 to #$08

; get x position adjustment based on a register and direction
; input
;  * a - initial X position index (offset into robot_spider_x_pos_adj_tbl)
;  * x - enemy slot index
; output
;  * y - robot_spider_x_pos_adj_tbl and robot_spider_sound_for_frame offset
robot_spider_x_pos_adj:
    sta $00
    lda ENEMY_VAR_4,x ; load robot spider direction (0 = to the right, 1 = to the left)
    lsr
    lda #$00          ; use first row when walking right
    bcc @continue     ; branch if walking to the right
    lda #$08          ; use second row when walking left

@continue:
    adc $00 ; load specific value based on frame
    tay     ; transfer X adjustment offset index to y
    rts

; each column in the row is based on the enemy frame #$00-#$08
robot_spider_x_pos_adj_tbl:
    .byte $00,$08,$00,$08,$00,$08,$00,$08,$00 ; walking right
    .byte $00,$00,$f8,$00,$f8,$00,$f8,$00,$f8 ; walking left

; used in conjunction with robot_spider_x_pos_adj_tbl to offset any overflow
; together can be used to determine if resulting X position is off screen
robot_spider_overflow_offset_tbl:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00 ; walking right
    .byte $00,$00,$ff,$00,$ff,$00,$ff,$00,$ff ; walking left

; whether or not to play sound_1f (ARUKU) for the frame and direction
robot_spider_sound_for_frame:
    .byte $01,$00,$00,$00,$00,$01,$00,$00,$00 ; walking left
    .byte $00,$01,$00,$00,$00,$00,$00,$01,$00 ; walking right

; run logic based on ENEMY_VAR_3
robot_spider_draw:
    lda #$00
    sta PAUSE_PALETTE_UPDATES      ; un-pause palette updates
    lda ENEMY_VAR_3,x
    cmp #$01
    bne @check_early_exit
    jsr robot_spider_set_collision ; value is #$01, update bg collision data

@check_early_exit:
    lda ENEMY_VAR_3,x
    bne @check_clear_bg
    jmp robot_spider_restore_x_exit2 ; value is #$00, exit

@check_clear_bg:
    cmp #$03
    bcc @start_draw_next_frame           ; branch if ENEMY_VAR_3,x is less than #$03
    jmp robot_spider_clear_bg_draw_floor ; draw black background or floor depending on ENEMY_VAR_3,x
                                         ; ENEMY_VAR_3,x is #$03, #$04, or #$05

; ENEMY_VAR_3,x is #$01, or #$02
@start_draw_next_frame:
    asl
    sta $08           ; store ENEMY_VAR_3,x * 2
    lda ENEMY_VAR_2,x ; load base nametable being used (#$00 = $2000, #$02 = $2800)
    asl
    asl
    eor #$08          ; swap to the other nametable
    sta $0a           ; store nametable being used (#$00 = $2000, #$08 = $2800)
    lda ENEMY_VAR_4,x ; load direction (0 = to the right, 1 = to the left)
    lsr
    lda #$01          ; go up a frame
    ldy #$09          ; used for overflow use 0
    bcs @new_frame    ; branch if moving left
    lda #$ff          ; moving right, go down a frame
    ldy #$f7          ; (-9) used for underflow to go to last frame #$08

@new_frame:
    sty $09
    clc                ; clear carry in preparation for addition
    adc ENEMY_FRAME,x
    cmp #$09           ; see if past last valid ENEMY_FRAME,x (#$00-#$08)
    bcc @calc_draw_pos
    sbc $09            ; overflow, go  ENEMY_FRAME,x

@calc_draw_pos:
    sta $0b                                ; store future frame index value
    jsr robot_spider_x_pos_adj             ; get x position adjustment based on a register and direction
    lda robot_spider_x_pos_adj_tbl,y
    clc                                    ; clear carry in preparation for addition
    adc ENEMY_X_POS,x
    sta $0e
    lda ENEMY_VAR_7,x                      ; load previous location offscreen indicator
                                           ; #$ff left of screen, #$00 on-screen, #$01 right of screen
    adc robot_spider_overflow_offset_tbl,y ; add overflow offset
                                           ; e.g. if previously off-screen to left (#$ff) and overflow when determining X position
                                           ; then the robot spider is now on the screen
                                           ; this offsets for when overflow is expected like walking left
    sta $0f
    lda ENEMY_Y_POS,x                      ; load enemy's Y position
    sta $0d
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$20
    bcc @load_bg_tiles_addr
    jmp robot_spider_restore_x_exit2

@load_bg_tiles_addr:
    lda $0b                               ; load frame index
    asl
    asl
    adc $08                               ; add either #$02 or #$04
    tay                                   ; transfer to offset register
    lda robot_spider_bg_tiles_ptr_tbl-2,y
    sta $08
    lda robot_spider_bg_tiles_ptr_tbl-1,y
    sta $09
    ldy #$00                              ; initialize graphics data read offset
    sty $0c                               ; two rounds of writing, start at first round
    lda #$07
    sta CPU_GRAPHICS_BUFFER,x             ; block mode
    inx                                   ; increment graphics buffer write offset

@process_bg_tiles:
    lda #$00
    sta $02
    lda ($08),y
    bpl @parse_byte
    dec $02         ; set $02 to #$ff

@parse_byte:
    cmp #$80
    bne @read_graphic_byte
    jmp @check_end_buffer  ; graphic byte is #$80, see if have finished writing all data

@read_graphic_byte:
    clc              ; clear carry in preparation for addition
    adc $0e          ; add to enemy X position with adjustment
    sta $05          ; set X position in $05
    iny
    iny              ; skip PPU address
    lda ($08),y      ; load graphics byte
    bmi @change_addr
    lda $02
    adc $0f          ; add robot_spider_overflow_offset_tbl value based on ENEMY_VAR_7,x
    bne @next_count
    lda $05
    cmp #$04
    bcc @next_count

@change_addr:
    dey
    sty $12
    lda $0d
    clc                           ; clear carry in preparation for addition
    adc ($08),y
    tay                           ; transfer Y position to y
    lda $05                       ; load X position
    jsr robot_spider_get_ppu_addr ; get 2-byte ppu address $(00) based on (a,y)
    ldy $12
    iny
    lda $01
    ora $0a
    sta CPU_GRAPHICS_BUFFER,x     ; write PPU address high byte
    inx                           ; increment graphics buffer write offset
    lda $00
    sta CPU_GRAPHICS_BUFFER,x     ; write PPU address low byte
    inx                           ; increment graphics buffer write offset
    lda ($08),y
    and #$7f
    sta CPU_GRAPHICS_BUFFER,x     ; write mode (flush, or block for example)
    sta $0b
    inx                           ; increment graphics buffer write offset
    iny                           ; increment graphics data read offset

@write_bg_tile_loop:
    lda ($08),y
    sta CPU_GRAPHICS_BUFFER,x
    sty $12
    ldy $12
    inx
    iny
    dec $0b
    bne @write_bg_tile_loop
    inc $0c
    jmp @process_bg_tiles

@next_count:
    lda ($08),y
    and #$7f
    sta $02
    iny
    tya
    clc                   ; clear carry in preparation for addition
    adc $02
    tay
    jmp @process_bg_tiles

@check_end_buffer:
    lda $0c
    beq @dec_anim_delay
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x
    inx
    stx GRAPHICS_BUFFER_OFFSET

@dec_anim_delay:
    ldx ENEMY_CURRENT_SLOT
    dec ENEMY_VAR_3,x
    clc

robot_spider_restore_x_exit2:
    ldx ENEMY_CURRENT_SLOT
    rts

; input
;  * a - ENEMY_VAR_3,x (3, 4, or 5 only)
;  * x - enemy slot index
robot_spider_clear_bg_draw_floor:
    asl
    tay
    lda ENEMY_VAR_2,x
    asl
    asl
    eor #$08
    sta $0a                                 ; set amount to add to PPU address high byte
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$20
    bcs robot_spider_restore_x_exit3
    lda #$03
    sta CPU_GRAPHICS_BUFFER,x               ; set repeat mode
    inx                                     ; increment graphics buffer write offset
    lda robot_spider_clear_bg_floor_tbl-2,y ; load PPU address high byte
    clc                                     ; clear carry in preparation for addition
    adc $0a
    sta CPU_GRAPHICS_BUFFER,x               ; set PPU address high byte
    inx                                     ; increment graphics buffer write offset
    lda robot_spider_clear_bg_floor_tbl-3,y ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x               ; set PPU address low byte
    inx                                     ; increment graphics buffer write offset
    tya                                     ; now pulling number of times to repeat the byte
    lsr
    tay
    lda robot_spider_clear_bg_floor_tbl-3,y ; load number of times to repeatedly write the graphics byte
    sta CPU_GRAPHICS_BUFFER,x               ; set number of times to repeat
    lsr
    lsr
    sta $0b                                 ; quarter repeat count (#$70 -> #$1c, #$20 -> #$08)
    inx                                     ; increment graphics buffer write offset
    lda #$00                                ; black blank tile
    cpy #$05                                ; see if ENEMY_VAR_3,x is #$05 (drawing top of floor)
    bne @continue
    lda #$ac                                ; striped gray tile (top of floor)

@continue:
    sta CPU_GRAPHICS_BUFFER,x       ; set graphics byte to write repeatedly
                                    ; either floor tile (striped) or black background
    inx                             ; increment graphics buffer write offset
    cpy #$03                        ; when ENEMY_VAR_3,x is #$03, the floor tile attributes are set
    bne robot_spider_dec_delay_exit

; sets the attribute data for the floor tiles
robot_spider_set_floor_attr:
    lda #$03
    sta CPU_GRAPHICS_BUFFER,x ; set repeat mode
    inx
    lda #$23                  ; PPU address high byte
    clc
    adc $0a
    sta CPU_GRAPHICS_BUFFER,x ; set PPU write address high byte
    inx
    lda #$e0
    sta CPU_GRAPHICS_BUFFER,x ; set PPU write address low byte
    inx
    lda #$20                  ; writing #$20 bytes (1 nametable row)
    sta CPU_GRAPHICS_BUFFER,x
    inx
    lda #$ff                  ; tile index #$ff (floor tile)
    sta CPU_GRAPHICS_BUFFER,x
    inx

robot_spider_dec_delay_exit:
    stx GRAPHICS_BUFFER_OFFSET
    inc PAUSE_PALETTE_UPDATES  ; pause palette updates
    ldx ENEMY_CURRENT_SLOT
    dec ENEMY_VAR_3,x
    clc

robot_spider_restore_x_exit3:
    ldx ENEMY_CURRENT_SLOT
    rts

robot_spider_clear_bg_floor_tbl:
    .byte $70     ; ENEMY_VAR_3,x = 3 (repeat #$70 times)
    .byte $70     ; ENEMY_VAR_3,x = 4 (repeat #$70 times)
    .byte $20     ; ENEMY_VAR_3,x = 5 (repeat #$20 times)
    .byte $00,$22 ; ENEMY_VAR_3,x = 3 (PPU address $2200) (black background)
    .byte $70,$22 ; ENEMY_VAR_3,x = 4 (PPU address $2270) (black background)
    .byte $e0,$22 ; ENEMY_VAR_3,x = 5 (PPU address $22e0) (striped gray floor tile)

; !(UNUSED)
    .byte $80,$04
    .byte $9c,$04
    .byte $b8,$04

robot_spider_set_collision:
    lda ENEMY_VAR_2,x
    ror
    ror
    ror
    eor #$80
    and #$80
    ora #$40
    sta $08
    tay
    ldx #$08
    lda #$00

@collision_loop:
    sta BG_COLLISION_DATA,y
    iny
    dex
    bne @collision_loop
    ldx ENEMY_CURRENT_SLOT
    lda ENEMY_VAR_7,x
    bne @exit
    lda ENEMY_X_POS,x       ; load enemy's X position
    sec                     ; set carry flag in preparation for subtraction
    sbc #$18
    lsr
    lsr
    lsr
    lsr
    lsr
    ora $08
    tay
    ldx #$00
    bcc @start_draw_loop
    ldx #$03

@start_draw_loop:
    lda #$03
    sta $08

@write_loop:
    lda robot_spider_collision_data_tbl,x
    sta BG_COLLISION_DATA,y
    inx
    iny
    dec $08
    bne @write_loop
    ldx ENEMY_CURRENT_SLOT

@exit:
    rts

robot_spider_collision_data_tbl:
    .byte $22,$22,$00,$02,$22,$20

robot_spider_bg_tiles_ptr_tbl:
    .addr robot_spider_bg_tiles_00 ; frame #$00
    .addr robot_spider_bg_tiles_01 ; frame #$00
    .addr robot_spider_bg_tiles_00 ; frame #$01
    .addr robot_spider_bg_tiles_03 ; frame #$01
    .addr robot_spider_bg_tiles_04 ; frame #$02
    .addr robot_spider_bg_tiles_05 ; frame #$02
    .addr robot_spider_bg_tiles_06 ; frame #$03
    .addr robot_spider_bg_tiles_07 ; frame #$03
    .addr robot_spider_bg_tiles_08 ; frame #$04
    .addr robot_spider_bg_tiles_09 ; frame #$04
    .addr robot_spider_bg_tiles_0a ; frame #$05
    .addr robot_spider_bg_tiles_0b ; frame #$05
    .addr robot_spider_bg_tiles_0c ; frame #$06
    .addr robot_spider_bg_tiles_0d ; frame #$06
    .addr robot_spider_bg_tiles_0e ; frame #$07
    .addr robot_spider_bg_tiles_0f ; frame #$07
    .addr robot_spider_bg_tiles_00 ; frame #$08
    .addr robot_spider_bg_tiles_11 ; frame #$08

; byte 0 = x offset
robot_spider_bg_tiles_00:
    .byte $d8,$f8,$05,$0d,$16,$20,$36,$3a,$e0,$e8,$07,$01,$05,$0e,$17,$21
    .byte $37,$3b,$e8,$e8,$08,$02,$06,$0f,$18,$22,$38,$3c,$3e,$f0,$f0,$07
    .byte $07,$10,$19,$23,$39,$3d,$3f,$f8,$f0,$04,$08,$11,$1a,$24,$80

robot_spider_bg_tiles_01:
    .byte $00,$f0,$04,$09,$12,$1b,$25,$08,$f0,$06,$0a,$13,$1c,$26,$36,$3a
    .byte $10,$e8,$07,$03,$0b,$14,$1d,$32,$37,$3b,$18,$e8,$08,$04,$0c,$15
    .byte $1e,$33,$38,$3c,$3e,$20,$00,$05,$1f,$35,$39,$3d,$3f,$80

robot_spider_bg_tiles_03:
    .byte $00,$f0,$04,$09,$12,$42,$45,$08,$f0,$06,$0a,$13,$43,$46,$36,$3a
    .byte $10,$e8,$07,$03,$0b,$40,$44,$38,$3c,$3b,$18,$e8,$06,$04,$0c,$41
    .byte $35,$39,$3d,$80

robot_spider_bg_tiles_04:
    .byte $d8,$f0,$05,$49,$4b,$20,$36,$3a,$e0,$e8,$06,$01,$05,$4c,$52,$37
    .byte $3b,$e8,$e8,$05,$02,$06,$4d,$53,$59,$f0,$f0,$07,$07,$4e,$54,$5a
    .byte $38,$3c,$3e,$f8,$f0,$07,$08,$4f,$55,$35,$39,$3d,$3f,$80

robot_spider_bg_tiles_05:
    .byte $00,$f0,$04,$09,$12,$56,$5b,$08,$f0,$04,$0a,$50,$57,$5c,$10,$e8
    .byte $07,$47,$4a,$51,$58,$3c,$5d,$3a,$18,$e8,$07,$48,$1f,$35,$39,$3d
    .byte $5e,$3b,$80

robot_spider_bg_tiles_06:
    .byte $d0,$e8,$05,$5f,$61,$20,$36,$3a,$d8,$e8,$05,$60,$62,$64,$37,$3b
    .byte $e0,$e8,$04,$01,$63,$65,$68,$e8,$e8,$04,$02,$06,$4d,$69,$f0,$f0
    .byte $07,$07,$4e,$6a,$22,$38,$3c,$3e,$80

robot_spider_bg_tiles_07:
    .byte $f8,$f0,$07,$08,$4f,$55,$35,$39,$3d,$3f,$00,$f0,$04,$09,$12,$6b
    .byte $6e,$08,$f0,$05,$0a,$66,$6c,$6f,$3c,$10,$e8,$07,$03,$0b,$67,$35
    .byte $39,$3d,$3a,$18,$e8,$07,$04,$0c,$15,$6d,$21,$37,$3b,$80

robot_spider_bg_tiles_08:
    .byte $d0,$f0,$05,$70,$72,$20,$36,$3a,$d8,$f0,$05,$71,$73,$77,$37,$3b
    .byte $e0,$e8,$04,$01,$05,$74,$78,$e8,$e8,$05,$02,$06,$4d,$79,$81,$f0
    .byte $f0,$04,$07,$10,$7a,$82,$f8,$f0,$07,$08,$11,$7b,$83,$38,$3c,$3e
    .byte $80

robot_spider_bg_tiles_09:
    .byte $00,$f0,$07,$09,$12,$7c,$84,$39,$3d,$3f,$08,$f0,$07,$0a,$13,$7d
    .byte $85,$38,$3c,$3e,$10,$e8,$08,$03,$0b,$14,$7e,$35,$39,$3d,$3f,$18
    .byte $e8,$07,$04,$0c,$75,$7f,$20,$36,$3a,$20,$f8,$05,$76,$80,$21,$37
    .byte $3b,$80

robot_spider_bg_tiles_0a:
    .byte $c8,$f8,$05,$86,$8b,$20,$36,$3a,$d0,$f8,$05,$87,$8c,$21,$37,$3b
    .byte $d8,$00,$02,$8d,$93,$e0,$e8,$05,$01,$05,$88,$8e,$94,$e8,$e8,$05
    .byte $02,$06,$4d,$8f,$95,$f0,$f0,$05,$07,$89,$22,$38,$3c,$80

robot_spider_bg_tiles_0b:
    .byte $f8,$f0,$05,$08,$8a,$90,$39,$3d,$00,$f0,$04,$09,$12,$91,$96,$08
    .byte $f0,$07,$0a,$13,$92,$97,$38,$3c,$3e,$10,$e8,$08,$03,$0b,$14,$7e
    .byte $35,$39,$3d,$3f,$18,$e8,$07,$04,$0c,$75,$7f,$20,$36,$3a,$20,$f8
    .byte $05,$76,$80,$21,$37,$3b,$80

robot_spider_bg_tiles_0c:
    .byte $d0,$f8,$05,$5f,$9f,$20,$36,$3a,$d8,$f8,$05,$71,$a0,$21,$37,$3b
    .byte $e0,$e8,$05,$01,$05,$88,$a1,$a8,$e8,$e8,$05,$02,$06,$4d,$a2,$a9
    .byte $f0,$f0,$04,$98,$9b,$38,$3c,$f8,$f0,$04,$99,$9c,$a3,$3d,$80

robot_spider_bg_tiles_0d:
    .byte $00,$f0,$04,$09,$12,$a4,$aa,$08,$f0,$04,$0a,$13,$a5,$ab,$10,$e8
    .byte $08,$03,$0b,$14,$a6,$ae,$38,$3c,$3e,$18,$e8,$08,$04,$0c,$9d,$a7
    .byte $35,$39,$3d,$3f,$20,$f0,$05,$9a,$9e,$21,$37,$3b,$80

robot_spider_bg_tiles_0e:
    .byte $d0,$f8,$05,$5f,$9f,$20,$36,$3a,$d8,$f8,$05,$71,$a0,$21,$37,$3b
    .byte $e0,$e8,$05,$01,$05,$88,$a1,$a8,$e8,$e8,$06,$02,$06,$af,$b1,$b5
    .byte $3c,$f0,$f0,$05,$07,$b0,$b3,$b6,$3d,$80

robot_spider_bg_tiles_0f:
    .byte $f8,$f0,$04,$08,$11,$b4,$b7,$00,$f0,$04,$09,$12,$a4,$aa,$08,$f0
    .byte $04,$0a,$13,$a5,$ab,$10,$e8,$08,$03,$0b,$14,$a6,$ae,$38,$3c,$3e
    .byte $18,$e8,$08,$04,$0c,$15,$1f,$35,$39,$3d,$3f,$80

robot_spider_bg_tiles_11:
    .byte $00,$f0,$04,$09,$12,$1b,$ba,$08,$f0,$04,$0a,$13,$b8,$bb,$10,$e8
    .byte $06,$03,$0b,$14,$b9,$bc,$3a,$18,$e8,$08,$04,$0c,$15,$1e,$33,$38
    .byte $3c,$3e,$20,$00,$05,$1f,$35,$39,$3d,$3f,$80

; enemy type #$4b
fortress_wall_core_routine_ptr_tbl:
    .addr fortress_wall_core_routine_00
    .addr fortress_wall_core_routine_01
    .addr fortress_wall_core_routine_02
    .addr fortress_wall_core_routine_03
    .addr fortress_wall_core_routine_04
    .addr fortress_wall_core_routine_05
    .addr fortress_wall_core_routine_06
    .addr fortress_wall_core_routine_07 ; enemy destroyed routine
    .addr fortress_wall_core_routine_08
    .addr fortress_wall_core_routine_09
    .addr fortress_wall_core_routine_0a
    .addr fortress_wall_core_routine_0b

fortress_wall_core_routine_00:
    lda #$11
    sta ENEMY_DESTROY_ATTRS,x ; set bullet collision box and disable player-enemy collision
    lda #$f0                  ; set special HP not destroyed by destroy_all_enemies routine
    sta ENEMY_HP,x
    jmp advance_enemy_routine ; advance to next routine

fortress_wall_core_routine_01:
    lda SCREEN_SCROLL_TYPE              ; 0 = horizontal, 1 = vertical/overhead
    beq fortress_wall_core_routine_exit
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne fortress_wall_core_routine_exit
    lda #$31                            ; sound_31 (BOSS BGM) - Ruined Base
    jsr play_sound                      ; play boss background music
    lda #$07
    sta ENEMY_VAR_1,x                   ; 7 frame delay to draw fortress wall core on non-visible nametable
    jmp advance_enemy_routine           ; advance to next routine

fortress_wall_core_routine_02:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne fortress_wall_core_routine_exit
    dec ENEMY_VAR_1,x                   ; decrement drawing delay
    beq fortress_wall_core_init_irq     ; branch if drawing of fortress wall core
                                        ; in non-visible nametable is complete
    lda #$00
    sta X_SCROLL_DRAW_POINT

fortress_wall_core_routine_exit:
    rts

; initializes enemy position, and sets up IRQ
fortress_wall_core_init_irq:
    lda #$80
    sta ENEMY_X_POS,x
    lda #$18
    sta ENEMY_Y_POS,x
    lda #$01
    sta ENEMY_VAR_6,x
    lda #$00
    sta ENEMY_VAR_7,x
    lda #$01
    sta NT_MIRRORING                               ; set nametable mirroring (0: vertical; 1: horizontal)
    lda #$00                                       ; moving to top left nametable where fortress wall core is drawn
    sta Y_SCROLL                                   ; set PPU vertical scroll to top of nametables (no scroll)
    sta IRQ_Y_SCROLL
    sta X_SCROLL                                   ; set PPU horizontal scroll to no scroll
    sta IRQ_X_SCROLL
    lda #$aa
    sta PPUCTRL_SETTINGS                           ; enable nmi for vertical blank, 8x16 sprites, base nametable $2800
    lda #$a8                                       ; settings for post IRQ
    sta IRQ_PPUCTRL_SETTINGS                       ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
    jsr init_bg_boss_pre_irq_max_scrolls           ; init boss screen pre-irq scroll max values
    lda #$08
    sta IRQ_TYPE                                   ; set irq routine type to irq_handler_07_ptr_tbl
                                                   ; level 3 jungle boss screen (fortress wall)
    .ifdef Probotector
        lda #$95
        sta SCANLINE_IRQ_1                         ; set initial scanline where interrupt will occur
                                                   ; interrupt occurs when drawing the ground
        jsr probotector_set_fortress_wall_irq_diff
    .else
        lda #$a2
        sta SCANLINE_IRQ_1                         ; set initial scanline where interrupt will occur
                                                   ; interrupt occurs when drawing the ground
    .endif
    ldy #$4f                                       ; number of bytes to clear
    jsr clear_partial_bg_collision_data            ; clear the first #$50 bytes of BG_COLLISION_DATA
    lda #$09
    sta ENEMY_VAR_1,x                              ; 9 frames to draw the fortress wall on the bottom of the nametable (top of scroll)
    jmp advance_enemy_routine                      ; advance to next routine

fortress_wall_core_routine_03:
    lda X_DRAW_ROUTINE
    ora Y_DRAW_ROUTINE
    bne fortress_wall_core_exit
    dec ENEMY_VAR_1,x
    beq fortress_wall_core_clear_bg_collision
    lda #$00
    sta X_SCROLL_DRAW_POINT

fortress_wall_core_exit:
    rts

fortress_wall_core_clear_bg_collision:
    jsr clear_irq_bg_collision_data ; set entire BG_COLLISION_DATA to #$00
    jmp advance_enemy_routine       ; advance to next routine

; fortress wall destroyed
fortress_wall_core_routine_04:
    lda #$aa
    sta IRQ_PPUCTRL_SETTINGS
    inc SCANLINE_IRQ_1
    jsr fortress_wall_y_scroll
    .ifdef Probotector
        jsr probotector_set_fortress_wall_irq_diff
    .endif
    lda SCANLINE_IRQ_1                             ; load scanline where interrupt will occur
    .ifdef Probotector
        cmp #$bc
    .else
        cmp #$c8
    .endif
    bcc fortress_wall_core_exit
    lda #$16
    sta LEFT_TOP_HALF_CHR_BANK                     ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$18
    sta LEFT_BOTTOM_CHR_HALF_BANK                  ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda #$10
    jmp set_delay_adv_enemy_routine                ; set delay to #$10 and set routine to fortress_wall_core_routine_05

fortress_wall_core_routine_05:
    dec ENEMY_DELAY,x
    bne fortress_wall_core_exit
    lda #$00
    sta ENEMY_VAR_1,x
    lda #$01
    sta ENEMY_ANIMATION_DELAY,x
    ldy #$00                         ; Y velocity = -1.00
    jsr fortress_wall_core_set_y_vel
    lda #$21
    jsr play_sound
    jmp advance_enemy_routine        ; advance to next routine

fortress_wall_core_routine_06:
    lda #$01
    sta ENEMY_SPRITE,x                    ; invisible sprite
    jmp run_fortress_wall_core_subroutine

; enemy destroyed routine
fortress_wall_core_routine_07:
    ldy #$02                              ; Y velocity = 0.25
    jsr fortress_wall_core_set_y_vel      ; begin descent
    lda #$04
    sta ENEMY_VAR_1,x                     ; fortress_wall_core_subroutine_04
    jsr run_fortress_wall_core_subroutine
    jmp enemy_routine_boss_defeated_00

fortress_wall_core_routine_08:
    jsr run_fortress_wall_core_subroutine
    jmp enemy_routine_boss_defeated_01

fortress_wall_core_routine_09:
    jsr run_fortress_wall_core_subroutine
    jmp bg_enemy_explosion_routine_00     ; set empty sprite, play optional enemy destroyed sound, disable collisions

fortress_wall_core_routine_0a:
    jsr run_fortress_wall_core_subroutine
    jmp bg_enemy_explosion_routine_01

fortress_wall_core_routine_0b:
    lda #$00
    sta Y_SCROLL                       ; set PPU vertical scroll to top of nametables (no scroll)
    sta X_SCROLL                       ; set PPU horizontal scroll to no scroll
    lda #$aa
    sta PPUCTRL_SETTINGS               ; enable nmi for vertical blank, 8x16 sprites, base nametable $2800
    jmp set_boss_defeated_remove_enemy ; set boss defeated flag, strip alive attribute bit, and remove enemy

.ifdef Probotector
    probotector_set_fortress_wall_irq_diff:
        lda #$db
        sec
        sbc SCANLINE_IRQ_1
        sta SCANLINE_IRQ_2_DIFF
        rts
.endif

; input
;  * y - fortress_wall_core_y_vel_tbl offset (0 = -1, 2 = 0.25)
;  * x - enemy slot offset
fortress_wall_core_set_y_vel:
    lda fortress_wall_core_y_vel_tbl,y
    sta ENEMY_Y_VELOCITY_FRACT,x         ; store enemy's fractional Y velocity
    lda fortress_wall_core_y_vel_tbl+1,y
    sta ENEMY_Y_VELOCITY_FAST,x          ; store enemy's fast Y velocity
    rts

fortress_wall_core_y_vel_tbl:
    .byte $00,$ff ; Y velocity = -1.00
    .byte $40,$00 ; Y velocity =  0.25

run_fortress_wall_core_subroutine:
    jsr bg_boss_apply_vel
    jsr bg_enemy_set_scrolls       ; simulate moving (ENEMY_X_POS, ENEMY_Y_POS) by scrolling in the opposite direction
    lda ENEMY_VAR_1,x              ; load fortress wall core subroutine index
    jsr run_routine_from_tbl_below

fortress_wall_core_subroutine_tbl:
    .addr fortress_wall_core_subroutine_00
    .addr fortress_wall_core_subroutine_01
    .addr fortress_wall_core_subroutine_02 ; wait for delay, signal turrets can rotate/move
    .addr fortress_wall_core_subroutine_03
    .addr fortress_wall_core_subroutine_04

; repeatedly play sound_08 and shake the screen
fortress_wall_core_subroutine_00:
    jsr x_earthquake_shake      ; scroll left/right for earthquake effect
    dec ENEMY_ANIMATION_DELAY,x
    bne @continue
    lda #$10
    sta ENEMY_ANIMATION_DELAY,x
    lda #$08
    jsr play_sound              ; play sound_08

@continue:
    lda ENEMY_VAR_6,x
    bne fortress_wall_core_subroutine_exit
    lda ENEMY_Y_POS,x                      ; load enemy's Y position
    cmp #$48
    bcs fortress_wall_core_subroutine_exit
    lda #$00
    sta X_SCROLL                           ; set PPU horizontal scroll to no scroll
    sta IRQ_X_SCROLL
    jsr clear_enemy_vel                    ; set enemy X and Y velocity to 0
    lda #$10
    sta ENEMY_DELAY,x
    bne fortress_wall_advance_subroutine   ; always branch to move to fortress_wall_core_subroutine_01

fortress_wall_core_subroutine_01:
    dec ENEMY_DELAY,x
    bne fortress_wall_core_subroutine_exit
    ldy #$03                               ; #$04 turrets to create

@create_turret_loop:
    sty $08                                 ; store turret index in $08
    ldy #$50                                ; enemy type = jungle boss fortress wall turret
    jsr try_create_enemy_from_existing      ; create jungle boss fortress wall turret
    bcc @continue                           ; branch if unable to create fortress wall turret
    ldy $08                                 ; load turret index
    lda fortress_wall_turret_init_pos_tbl,y
    ldy $11                                 ; load created turret enemy slot index
    sta ENEMY_ATTRIBUTES,y                  ; set initial position index of created turret
    txa
    sta ENEMY_VAR_5,y
    ldy $08                                 ; load turret index
    dey                                     ; move to next turret
    bpl @create_turret_loop

; set core HP and enabling palette cycling, set delay and advance subroutine
@continue:
    lda #$28                      ; HP = #$28, #$38, or #$40
    jsr set_enemy_hp_hard         ; set ENEMY_HP calculated using hardest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    lda #$00
    sta PALETTE_CYCLE_INDEX       ; enable palette cycling for red core
    lda #$40
    sta ENEMY_DELAY,x
    lda #$1a
    sta RIGHT_FOURTH_QTR_CHR_BANK ; set bank number of PPU $1c00-$1fff (last quarter of right pattern table)

fortress_wall_advance_subroutine:
    inc ENEMY_VAR_1,x

fortress_wall_core_subroutine_exit:
    rts

; initial position index for fortress wall turret, moved into turret's ENEMY_VAR_1
fortress_wall_turret_init_pos_tbl:
    .byte $00 ; above core
    .byte $03 ; right of core
    .byte $06 ; below core
    .byte $09 ; left of core

; wait for delay, signal turrets can rotate/move
fortress_wall_core_subroutine_02:
    lda GLOBAL_TIMER
    and #$01
    bne fortress_wall_core_subroutine_exit
    dec ENEMY_DELAY,x                      ; decrement every 2 frames
    bne fortress_wall_core_subroutine_exit
    lda #$01
    sta ENEMY_VAR_2,x                      ; indicate ready for turrets surrounding core to move
    bne fortress_wall_advance_subroutine   ; always branch to move to fortress_wall_core_subroutine_03

fortress_wall_core_subroutine_03:
    lda #$00
    sta ENEMY_VAR_2,x ; signal turrets to wait before moving again
    lda #$40
    sta ENEMY_DELAY,x
    lda #$02
    sta ENEMY_VAR_1,x ; set subroutine to fortress_wall_core_subroutine_02
    rts

fortress_wall_core_subroutine_04:
    jsr x_earthquake_shake                         ; scroll left/right for earthquake effect
    lda SCANLINE_IRQ_1                             ; load scanline where interrupt will occur
    .ifdef Probotector
        cmp #$96
    .else
        cmp #$a3
    .endif
    beq fortress_wall_y_scroll                     ; branch if scanline interrupt for ground split occurs at 63% of the screen
    lda GLOBAL_TIMER                               ; ground not yet risen to 63% location
    lsr
    bcc fortress_wall_y_scroll                     ; branch if even timer value
    dec SCANLINE_IRQ_1                             ; move scanline up one line every other frame
    .ifdef Probotector
        jsr probotector_set_fortress_wall_irq_diff
    .endif

; scroll pre-irq vertically up to cause the fortress wall to descend into the ground
; offsets rising ground
fortress_wall_y_scroll:
    lda SCANLINE_IRQ_1 ; load scanline where ground interrupt will occur
    sec                ; set carry flag in preparation for subtraction
    .ifdef Probotector
        sbc #$95       ; subtract scanline irq line for where ground stops at highest point
    .else
        sbc #$a2       ; subtract scanline irq line for where ground stops at highest point
    .endif
    sta $00            ; store number of scanlines above ground will be at its highest point
    lda #$f0           ; -16
    sec                ; set carry flag in preparation for subtraction
    sbc $00            ; adjust by additional 16 scanlines
    sta IRQ_Y_SCROLL   ; set new vertical scroll
                       ; this keeps the fortress wall 'stationary' while ground is rising
    rts

; !(UNUSED)
; update the attribute table palettes for the top of the fortress wall
fortress_wall_core_set_attr_palettes:
    lda #$14
    sta $08
    ldy #$00
    ldx GRAPHICS_BUFFER_OFFSET
    cpx #$40
    bcs @exit

@loop:
    lda unused_fortress_wall_core_attr_tbl,y
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

unused_fortress_wall_core_attr_tbl:
    .byte $03                 ; repeat mode
    .byte $2b,$d8             ; PPU address (nametable 2 (bottom left) attribute table)
    .byte $12,$aa             ; repeat #$aa #$12 times (2.25 rows of palettes)
    .byte $06                 ; block mode
    .byte $2b,$ea             ; PPU address (nametable 2 (bottom left) attribute table)
    .byte $05                 ; block size is #$05 bytes
    .byte $ae,$fa,$fa,$fa,$fa ; attribute palette data
    .byte $ff                 ; block mode end byte
    .byte $03                 ; repeat mode
    .byte $2b,$ef             ; PPU address (nametable 2 (bottom left) attribute table)
    .byte $08,$aa             ; repeat #$aa #$08 times (1 row of palettes)

; enemy type #$50
fortress_wall_turret_routine_ptr_tbl:
    .addr fortress_wall_turret_routine_00 ; initialize HP, set invincible, and position index
    .addr fortress_wall_turret_routine_01 ; set purple sprite, set X,Y position, set delay and advance routine
    .addr fortress_wall_turret_routine_02 ; set sprite and velocity, once delay elapses change to purple, advance position, advance routine
    .addr fortress_wall_turret_routine_03 ; set X,Y position, wait for delay, then make vulnerable, set delays, advance routine
    .addr fortress_wall_turret_routine_04 ; wait for delay, fire bullet, wait for delay make purple (invincible), advance routine
    .addr fortress_wall_turret_routine_05 ; wait for delay, advance to fortress_wall_turret_routine_01
    .addr enemy_explosion_routine_00      ; enemy destroyed routine - set empty sprite, play optional sound, disable collisions
    .addr enemy_explosion_routine_01      ; animate explosion sequence
    .addr enemy_explosion_routine_03      ; mark destroyed, remove enemy

; initialize HP, set invincible, and position index
fortress_wall_turret_routine_00:
    lda #$10                  ; HP = #$10, #$14, or #$17
    jsr set_enemy_hp          ; set ENEMY_HP calculated using easiest HP difficulty (adjusted by ENEMY_DIFFICULTY)
    sta ENEMY_VAR_2,x         ; also set ENEMY_VAR_2 to have the enemy HP
    lda #$f1                  ; special invincible HP
    sta ENEMY_HP,x            ; any HP greater than #$f0 is invincible
                              ; bullet-enemy collision sound still plays (sound_11)
    lda ENEMY_ATTRIBUTES,x
    sta ENEMY_VAR_1,x         ; set position index of where turret is located
    jmp advance_enemy_routine ; advance to next routine

; set purple sprite, set X,Y position, set delay and advance routine
fortress_wall_turret_routine_01:
    jsr fortress_wall_turret_set_purple_sprite ; set purple slightly glowing orb
    jsr fortress_wall_turret_set_pos           ; set position using ENEMY_VAR_1, which specifies offset from fortress wall core
    ldy ENEMY_VAR_5,x                          ; load enemy slot index of fortress wall core
    lda ENEMY_VAR_2,y
    beq fortress_wall_turret_exit              ; exit if not all turrets are ready to move
    ldy ENEMY_VAR_1,x                          ; all turrets ready to move, being moving
                                               ; load position index where turret is located
    lda fortress_wall_turret_anim_delay_tbl,y  ; load stop delay delay based on position
                                               ; set to cause turret to stop at right spot
    jmp set_delay_adv_enemy_routine            ; set delay and set routine to fortress_wall_turret_routine_01

fortress_wall_turret_anim_delay_tbl:
    .byte $34,$2f,$2f ; turret above core
    .byte $34,$10,$10 ; turret right of core
    .byte $34,$2f,$2f ; turret below core
    .byte $34,$08,$08 ; turret left of core

; set sprite and velocity, once delay elapses change to purple, advance position, advance routine
fortress_wall_turret_routine_02:
    jsr fortress_wall_turret_animate           ; set sprite and attribute based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    jsr fortress_wall_turret_set_vel           ; set X and Y velocities based on position index ENEMY_VAR_1
    dec ENEMY_DELAY,x                          ; decrement stop delay
    bne fortress_wall_turret_exit              ; exit if stop delay hasn't elapsed
    jsr fortress_wall_turret_set_purple_sprite ; set purple slightly glowing orb
    inc ENEMY_VAR_1,x                          ; move to next position index where turret is located
    lda ENEMY_VAR_1,x                          ; load position index where turret is now located
    cmp #$0c
    bcc @continue
    lda #$00                                   ; went past last position, wrap around

@continue:
    sta ENEMY_VAR_1,x               ; set new position index where turret is located
    lda #$0c
    jmp set_delay_adv_enemy_routine ; set delay to #$0c and set routine to fortress_wall_turret_routine_03

fortress_wall_turret_exit:
    rts

; set X,Y position, wait for delay, then set teal sprite, set hp (make vulnerable),
; set animation and firing delay, advance routine
fortress_wall_turret_routine_03:
    jsr fortress_wall_turret_set_pos         ; set position using ENEMY_VAR_1, which specifies offset from fortress wall core
    dec ENEMY_DELAY,x                        ; decrement start moving delay
    bne fortress_wall_turret_exit            ; exit if delay hasn't elapsed
    jsr fortress_wall_turret_set_teal_sprite ; set slightly glowing teal sprite
    lda ENEMY_VAR_2,x                        ; load saved HP from fortress_wall_turret_routine_04 when make vulnerable
    sta ENEMY_HP,x                           ; restore HP now that turret is vulnerable (no longer purple)
    jsr advance_enemy_routine                ; advance enemy routine
    lda #$10
    sta ENEMY_FIRING_DELAY,x                 ; set firing delay
    lda #$30
    sta ENEMY_DELAY,x                        ; set delay before turning purple (become invincible)
                                             ; and advancing to fortress_wall_turret_routine_05
    rts

; wait for delay, fire bullet, wait for delay make purple (invincible), advance routine
fortress_wall_turret_routine_04:
    lda ENEMY_FIRING_DELAY,x  ; load firing delay
    beq @continue             ; branch to skip firing if already fired (already 0)
    dec ENEMY_FIRING_DELAY,x  ; decrement firing delay
    bne @continue             ; branch if delay not elapsed to skip firing
    jsr copy_enemy_vars_to_zp ; firing delay elapsed and just turned 0
                              ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    ldy #$04                  ; set bullet speed code to 1.5x speed (see bullet_velocity_adjust_ptr_tbl)
    jsr fire_bullet_at_player ; fire bullet from ($09, $08) at player index specified in $0a (0 = p1, 1 = p2)

@continue:
    dec ENEMY_DELAY,x                          ; decrement advance routine delay
    bne fortress_wall_turret_exit              ; exit if delay hasn't elapsed
    jsr fortress_wall_turret_set_purple_sprite ; delay elapsed, set purple slightly glowing orb
                                               ; while purple, the turret is invincible
    lda ENEMY_HP,x
    sta ENEMY_VAR_2,x                          ; store current HP for use when no longer invincible
    lda #$f1                                   ; special invincible HP
    sta ENEMY_HP,x                             ; any HP greater than #$f0 is invincible
                                               ; bullet-enemy collision sound still plays (sound_11)
    lda #$04
    jmp set_delay_adv_enemy_routine            ; set delay to #$04 and set routine to fortress_wall_turret_routine_05

; wait for delay, advance to fortress_wall_turret_routine_01
fortress_wall_turret_routine_05:
    dec ENEMY_DELAY,x
    bne fortress_wall_turret_exit ; exit if delay hasn't elapsed
    lda #$02
    jmp set_enemy_routine         ; set routine to fortress_wall_turret_routine_01

fortress_wall_turret_animate:
    ldy #$18                                 ; animation index for fortress wall orb
    jsr set_enemy_animation_sprite           ; animate sprite for fortress wall orb based on ENEMY_FRAME and ENEMY_ANIM_DELAY
    ldy #$03
    bne fortress_wall_turret_set_sprite_attr

; invincible
fortress_wall_turret_set_purple_sprite:
    lda #$b6                            ; sprite_b6 (fortress wall turret small center glow)
    ldy #$03                            ; set palette
    bne fortress_wall_turret_set_sprite

; vulnerable
fortress_wall_turret_set_teal_sprite:
    ldy #$00
    lda #$b7 ; sprite_b7 (fortress wall turret no center glow)

fortress_wall_turret_set_sprite:
    sta ENEMY_SPRITE,x

fortress_wall_turret_set_sprite_attr:
    tya
    sta ENEMY_SPRITE_ATTR,x
    rts

; sets the turrets X and Y velocities based on position index ENEMY_VAR_1
fortress_wall_turret_set_vel:
    lda ENEMY_VAR_1,x                    ; load position index where turret is located
    asl
    asl                                  ; quadruple since each position has 4 velocity values
    tay                                  ; transfer to offset register
    lda ENEMY_Y_VEL_ACCUM,x              ; load Y fractional velocity accumulator
    clc                                  ; clear carry in preparation for addition
    adc fortress_wall_turret_vel_tbl,y   ; add Y fractional velocity to accumulator
    sta ENEMY_Y_VEL_ACCUM,x              ; update Y fractional accumulator
    lda ENEMY_Y_POS,x                    ; load enemy's Y position
    adc fortress_wall_turret_vel_tbl+1,y ; add Y fast velocity (plus any fractional overflow)
    sta ENEMY_Y_POS,x                    ; set new Y position
    lda ENEMY_X_VEL_ACCUM,x              ; load X fractional velocity accumulator
    clc                                  ; clear carry in preparation for addition
    adc fortress_wall_turret_vel_tbl+2,y ; add X fractional velocity to accumulator
    sta ENEMY_X_VEL_ACCUM,x              ; update X fractional accumulator
    lda ENEMY_X_POS,x                    ; load enemy's X position
    adc fortress_wall_turret_vel_tbl+3,y ; add X fast velocity (plus any fractional overflow)
    sta ENEMY_X_POS,x                    ; set new X position
    rts

; Y fractional vel, Y fast, X fractional, X fast
; 0.7070 is approximately sqrt(2)/2, which is slope of hypotenuse of path
fortress_wall_turret_vel_tbl:
    .byte $b5,$00,$b5,$00 ; turret above core (x vel = 0.7070, y vel = 0.7070)
    .byte $00,$00,$00,$01 ; turret above core (x vel = 0.0000, y vel = 1.0000)
    .byte $00,$00,$00,$ff ; turret above core (x vel = 0.0000, y vel = -1.0000)
    .byte $b5,$00,$4b,$ff ; turret right of core (x vel = 0.7070, y vel = -0.7070)
    .byte $00,$01,$00,$00 ; turret right of core (x vel = 1.0000, y vel = 0.0000)
    .byte $00,$ff,$00,$00 ; turret right of core (x vel = -1.0000, y vel = 0.0000)
    .byte $4b,$ff,$4b,$ff ; turret below core (x vel = -0.7070, y vel = -0.7070)
    .byte $00,$00,$00,$ff ; turret below core (x vel = 0.0000, y vel = -1.0000)
    .byte $00,$00,$00,$01 ; turret below core (x vel = 0.0000, y vel = 1.0000)
    .byte $4b,$ff,$b5,$00 ; turret left of core (x vel = -0.7070, y vel = 0.7070)
    .byte $00,$ff,$00,$00 ; turret left of core (x vel = -1.0000, y vel = 0.0000)
    .byte $00,$01,$00,$00 ; turret left of core (x vel = 1.0000, y vel = 0.0000)

; sets the position using ENEMY_VAR_1, which specifies offset from fortress wall core
; input
;  * x - enemy slot index
fortress_wall_turret_set_pos:
    ldy ENEMY_VAR_1,x                    ; load position index where the core is location
    lda fortress_wall_turret_y_adj_tbl,y ; load amount to add to core Y position
    sta $00                              ; set y adjustment from core amount
    lda fortress_wall_turret_x_adj_tbl,y ; load amount to add to core X position
    ldy ENEMY_VAR_5,x                    ; load fortress wall core enemy slot
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_X_POS,y                    ; add x adjustment amount to core's X position
    sta ENEMY_X_POS,x                    ; set turret's X position
    lda $00                              ; load y adjustment from core amount
    clc                                  ; clear carry in preparation for addition
    adc ENEMY_Y_POS,y                    ; add y adjustment amount to core's Y position
    sta ENEMY_Y_POS,x                    ; set turret's Y position
    rts

fortress_wall_turret_y_adj_tbl:
    .byte $db,$00,$00 ; turret above core
    .byte $00,$25,$35 ; turret right of core
    .byte $25,$00,$00 ; turret below core
    .byte $00,$db,$d3 ; turret left of core

fortress_wall_turret_x_adj_tbl:
    .byte $00,$25,$54 ; turret above core
    .byte $25,$00,$00 ; turret right of core
    .byte $00,$db,$ac ; turret below core
    .byte $db,$00,$00 ; turret left of core

.ifdef Probotector
    ; !(UNUSED) duplicated level_7_supertile_data from bank a
    ; probably a leftover artifact of the build system
    ; can be safely removed and used for other purposes
    .incbin "assets/chr_rom/unused_remnant_00.bin"
.endif

; end of bank
; unused #$2b2 bytes out of #$2,000 bytes total (97.68% full)
; unused 690 bytes out of 8,192 bytes total (97.68% full)
bank_2_unused_space: