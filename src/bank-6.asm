; NES Super C Disassembly - v1.00
; https://github.com/vermiceli/nes-super-c/
; Bank 6 contains the sprite logic and all of the sprite data, including player
; sprites.  Then bank 6 contains the 'tile routines'.  These are routines that
; are executed as the player navigates the level and do things like change the
; pattern tiles, palette, enable auto-scroll, among other things.  Bank 6 then
; contains Level 4 (Inner Base) screen layout and the table of supertiles per
; screen for Level 4. Finally, the end of the bank contains the start of the
; assignment of pattern table tiles that make up Level 4 supertiles, which is
; continued in Bank 7.

; 8 KiB PRG ROM
.segment "BANK6"

.include "constants.asm"

; import from bank f
.import run_routine_from_tbl_below
.import set_y_autoscroll_stop
.import set_level_palette
.import set_palette
.import reset_draw_point
.import load_alt_collision_code_indices

; export for bank f
.export load_sprites_to_oam_buffer
.export level_4_supertiles_screen_ptr_table
.export level_4_supertile_data
.export level_4_screen_layout_tbl
.export level_1_run_tile_routines
.export level_2_run_tile_routines
.export level_3_run_tile_routines
.export level_4_run_tile_routines
.export level_5_run_tile_routines
.export level_6_run_tile_routines
.export level_7_run_tile_routines
.export level_8_run_tile_routines

.byte $36 ; bank byte

; copy the player HUD sprites to OAM CPU buffer
; draw the player number of lives medals on the top of the screen
; if player in game over, or in demo mode, display text "GAME OVER" instead of medals
; input
;  * x - OAMDMA_CPU_BUFFER write offset
load_hud_sprites_to_oam_buffer:
    lda PLAYER_HUD   ; load whether or not to display HUD sprites (0 = no display, 1 = display)
    beq @exit        ; exit if not displaying HUD
    ldy PLAYER_COUNT ; displaying hud, load number of players (0 = 1 player, 1 = 2 player)

@draw_player_hud:
    sty $02                       ; store current player index in $02, used for player distinction sprite palette
    lda PLAYER_GAME_OVER_STATUS,y ; load player game over status (0 = not game over, 1 = game over)
    ldy #$04                      ; set initial hud_sprites offset to show GAME OVER
    ora DEMO_MODE                 ; merge player game over status with whether or not in demo
    bne @four_sprites             ; branch if in game over or in demo to draw GAME OVER sprites
    ldy $02                       ; not in game over nor demo, load player index
    lda PLAYER_NUM_LIVES,y        ; load player y's number of medals in HUD (number of lives minus 1)
    ldy #$00                      ; player not in game over, use the medals hud_sprites offset
    cmp #$04
    bcc @draw_sprites             ; branch if player has #$04 lives
                                  ; (#$03 medals in HUD) or fewer

@four_sprites:
    lda #$04 ; set number of sprites to draw
             ; either player #$04 or more lives and just showing 4 medals
             ; or player is in game over and needs to show the #$04 sprites spelling GAME OVER

@draw_sprites:
    sta $01 ; set number of sprites to draw

@draw_p_sprite:
    dec $01                     ; decrement number of sprites to draw
    bmi @move_to_next_player    ; if done drawing all sprites, move to next player
    lda #$10                    ; setting sprite Y position #$10
    sta OAMDMA_CPU_BUFFER,x     ; set Y position of medal/game over hud sprite to #$10
    lda hud_sprites,y           ; load HUD sprite (either medal, or game over text sprite)
    sta OAMDMA_CPU_BUFFER+1,x   ; write tile number to OAM
    lda $02                     ; load sprite attributes
    sta OAMDMA_CPU_BUFFER+2,x   ; write the tile attributes (blue palette for p1, red palette for p2)
    lsr                         ; set carry flag if on player 2
    lda sprite_medal_x_offset,y ; load x offset based on sprite number
    bcc @continue               ; branch for player 1
    adc #$af                    ; add #$af to sprite x offset if on player 2 (see previous lsr)
    clc

@continue:
    sta OAMDMA_CPU_BUFFER+3,x ; set X position of sprite
    txa                       ; move current OAM CPU buffer write offset to a
    adc #$c4                  ; add #$c4 to current write offset (next sprite location)
    tax                       ; move new OAM CPU buffer offset back to x
    iny                       ; increment sprite read offset
    bne @draw_p_sprite        ; always branch

@move_to_next_player:
    ldy $02              ; load current player index
    dey                  ; decrement current player index
    bpl @draw_player_hud ; display hud sprite for player

@exit:
    rts

hud_sprites:
    .byte $8d,$8d,$8d,$8d ; medals
    .byte $83,$85,$87,$89 ; game over text

; x offsets to where each sprite should go
sprite_medal_x_offset:
    .byte $10,$1c,$28,$34
    .byte $10,$1c,$28,$34

; write player meta-sprite sprites to OAMDMA_CPU_BUFFER
; NOTE: in most documentation, a sprite actually refers to a meta-sprite, which
; is a group of sprites combined to make a single, larger sprite, e.g. Bill and
; Lance are composed multiple sprites.  In this method, documentation is careful
; to refer to sprites as an individual 8x16 NES sprite and not the meta-sprite.
; input
;  * a - meta-sprite code, offset into player_sprite_ptr_tbl
;  * $00 - meta-sprite attribute (overrides any attribute specified in sprite data)
;  * $01 - meta-sprite Y position on screen
;  * $02 - meta-sprite attribute
;  * $0a - meta-sprite horizontal and vertical flip bits
;  * $0b - bit 0 stores meta-sprite recoil effect flag
load_player_metasprite_to_oam_buffer:
    asl                           ; double since each entry in player_sprite_ptr_tbl is 2 bytes
    tay
    lda player_sprite_ptr_tbl,y   ; load low byte of meta-sprite address into a
    sta $08                       ; store low byte of meta-sprite address into $08
    lda player_sprite_ptr_tbl+1,y ; load high byte of meta-sprite address into a
    sta $09                       ; store high byte of meta-sprite address into $09
    ldy #$00                      ; initialize meta-sprite read offset
    lda ($08),y                   ; load first byte from meta-sprite
    sta $03                       ; store number of bytes in meta-sprite into $03
    iny                           ; increment player_sprite_xx read offset

@handle_sprite:
    lda ($08),y       ; load first byte of sprite (Y position), or #$80 for shared sprite
    cmp #$80          ; see if using shared sprite
    bne @write_sprite ; branch if not shared sprite, to write the sprite to the OAM cpu buffer
    iny               ; using shared meta-sprite, need to update cpu read addresses
                      ; increment player_sprite_xx read offset
    lda ($08),y       ; load low byte of new meta-sprite read address
    sta $04           ; store low byte into $04
    iny               ; increment player_sprite_xx read offset
    lda ($08),y       ; load high byte of new meta-sprite read address
    sta $09           ; replace existing read high byte in $09 with new address
    lda $04           ; load low byte
    sta $08           ; replace existing read low byte in $08 with new address
    ldy #$00          ; clear player_sprite_xx read offset since starting at new address
    lda ($08),y       ; read meta-sprite relative Y position (Byte 0)

@write_sprite:
    clc                       ; clear carry in preparation for addition
    adc $01                   ; add SPRITE_Y_POS to sprite y offset (sets tile's absolute position)
    sta OAMDMA_CPU_BUFFER,x   ; write Y position to CPU buffer
    iny                       ; increment player_sprite_xx read offset
    lda ($08),y
    sta OAMDMA_CPU_BUFFER+1,x ; write pattern tile number to CPU buffer
    iny                       ; increment player_sprite_xx read offset
    lda ($08),y               ; read 3rd byte (sprite tile attributes) (Byte 2)
    and $0b                   ; keep/strip sprite palette from sprite tile byte 2, based on bit 2 of SPRITE_ATTR
    ora $00                   ; merge sprite code tile byte 2 with attribute data from SPRITE_ATTR
    eor $0a                   ; exclusive or to handle flipping a flipped sprite tile, e.g. flip + flip = no flip
    sta OAMDMA_CPU_BUFFER+2,x ; write sprite attributes to CPU buffer
    iny                       ; increment player_sprite_xx read offset
    bit $0a                   ; set overflow flag when $0a specifies to flip sprite horizontally
    lda ($08),y               ; load sprite relative X position within meta-sprite
    bvc @continue             ; branch if not flipping sprite horizontally
    eor #$ff                  ; flipping sprite horizontally, flip all bits and subtract #$07
    clc                       ; clear carry in preparation for addition
    adc #$f9                  ; subtract 7 to get the reflected relative X position

@continue:
    clc                       ; clear carry in preparation for addition
    adc $02                   ; add meta-sprite X position to relative X position within meta-sprite
    sta OAMDMA_CPU_BUFFER+3,x ; set X position of sprite
    txa                       ; transfer OAM buffer offset to a
    clc                       ; clear carry in preparation for addition
    adc #$c4                  ; move to next sprite location within the buffer
    tax                       ; transfer new OAM buffer offset back to x
    iny                       ; increment player_sprite_xx read offset
    dec $03                   ; decrement number of sprites to draw
    bne @handle_sprite        ; branch to load the next sprite within the meta-sprite
                              ; if there is at least one more sprite to draw
    rts

; writes the hud, player, and enemy sprites to the OAM CPU buffer
load_sprites_to_oam_buffer:
    lda OAMDMA_CPU_BUFFER_OFFSET       ; OAMDMA_CPU_BUFFER write offset
    clc                                ; clear carry in preparation for addition
    adc #$4c                           ; add #$4c to OAMDMA_CPU_BUFFER, this is to support "sprite cycling"/"sprite flickering"
                                       ; since the NES can draw a maximum of 8 sprites per scan line, Super C adjusts the starting locations
                                       ; so that sprites move around in PPU memory. This will allow different sprites to load each frame
                                       ; on the same scan line.
    sta OAMDMA_CPU_BUFFER_OFFSET       ; store OAMDMA_CPU_BUFFER write offset
    tax                                ; move new OAMDMA_CPU_BUFFER offset back to x
    sec                                ; set carry flag in preparation for subtraction
    sbc #$c4                           ; move back one sprite location
                                       ; (each sprite is written #$c4 positions apart from each other)
    tay                                ; transfer OAMDMA CPU buffer offset to y
    sty $07                            ; store offset in $07
    lda #$00
    sta OAMDMA_CPU_BUFFER+1,y          ; store tile number #$00 in previous sprite location
                                       ; this clears the last sprite that won't be cleaned up in fill_unused_OAM
    jsr load_hud_sprites_to_oam_buffer ; load hud sprites (number of lives medals or "GAME OVER" text) into OAM CPU buffer
    lda #$ff
    ldy INVINCIBILITY_TIMER            ; load player 1 invincibility timer
    beq @player_1_sprite               ; branch if no timer
    lda #$fc                           ; timer present

@player_1_sprite:
    sta $0b                                  ; set sprite code attribute override
    lda PLAYER_SPRITE_ATTR
    and #$03                                 ; keep palette only
    sta $00                                  ; set sprite palette
                                             ; overwrites sprite code default palette
    lda PLAYER_SPRITE_ATTR
    and #$e0                                 ; keep flip bits, and bg priority
    sta $0a                                  ; set set vertical and horizontal sprite bit, including bg priority
    lda PLAYER_SPRITE_Y_POS
    sta $01                                  ; set sprite Y position
    lda PLAYER_SPRITE_X_POS
    sta $02                                  ; set sprite X position
    lda PLAYER_SPRITE                        ; load sprite code
    beq @adv_player_2                        ; branch if player 1 isn't on screen to move to player 2
    jsr load_player_metasprite_to_oam_buffer ; load player metasprite to OAM CPU buffer

@adv_player_2:
    lda #$ff
    ldy INVINCIBILITY_TIMER+1 ; load player 2 invincibility timer
    beq @player_2_sprite
    lda #$fc

@player_2_sprite:
    sta $0b                                  ; set sprite code attribute override
    lda PLAYER_SPRITE_ATTR+1
    and #$03                                 ; keep palette only
    sta $00                                  ; set sprite palette
    lda PLAYER_SPRITE_ATTR+1
    and #$e0                                 ; keep flip bits, and bg priority
    sta $0a                                  ; set set vertical and horizontal sprite bit, including bg priority
    lda PLAYER_SPRITE_Y_POS+1
    sta $01                                  ; set sprite Y position
    lda PLAYER_SPRITE_X_POS+1
    sta $02                                  ; set X position
    lda PLAYER_SPRITE+1                      ; load sprite code
    beq @continue
    jsr load_player_metasprite_to_oam_buffer ; load player 2 metasprite to OAM CPU buffer

@continue:
    ldy #$17 ; number of player bullet sprites plus enemy sprites

@load_enemy_metasprite:
    lda SPRITE_ATTR,y
    and #$3f                                ; strip background priority flag
    sta $00                                 ; store sprite attributes without bg priority
                                            ; enemy sprites are always in the foreground
    lsr
    lsr
    sta $0b                                 ; store sprite recoil effect in bit 0
                                            ; (allows enemies that fire weapons to animate as they fire)
    lda SPRITE_ATTR,y
    and #$c0                                ; keep sprite flip flags
    sta $0a                                 ; store whether to flip sprite vertically and/or horizontally
    lda SPRITE_Y_POS,y
    sta $01                                 ; store sprite Y position
    lda SPRITE_X_POS,y
    sta $02                                 ; store sprite X position
    lda SPRITES,y
    beq @adv_sprite                         ; branch if no sprite to draw
    sty $05                                 ; store meta-sprite code
    jsr load_enemy_metasprite_to_oam_buffer ; loads the meta-sprite to the OAMDMA buffer
    ldy $05                                 ; load meta-sprite code

; move to next sprite code if room available in OAM
@adv_sprite:
    dey                        ; move to next enemy
    bpl @load_enemy_metasprite ; loop to draw next enemy meta-sprite if more to draw
    ldy $07                    ; loaded all enemies, load index into OAM of sprite slot
    lda OAMDMA_CPU_BUFFER+1,y  ; load pattern tile index
    bne draw_sprites_exit      ; exit if sprite already at location, indicating OAM buffer is full

; take the remaining sprite tiles that are available in the OAMDMA and blank them
; by setting their Y position to #$f4, which isn't drawn
@fill_unused_OAM:
    lda #$f4                     ; hide any sprite whose byte 0 is $ef-$ff is not displayed
    sta OAMDMA_CPU_BUFFER,x      ; write Y position as #$f4 (hidden)
    txa                          ; move current write offset to a
    clc                          ; clear carry in preparation for addition
    adc #$c4                     ; add #$c4 to current write offset
    tax                          ; move new offset back to x
    cpx OAMDMA_CPU_BUFFER_OFFSET
    bne @fill_unused_OAM         ; branch if more of the buffer to 'clear'

draw_sprites_exit:
    rts

; write enemy meta-sprite data (OAM) to OAMDMA_CPU_BUFFER
; NOTE: in most documentation, a sprite actually refers to a meta-sprite, which
; is a group of sprites combined to make a single, larger sprite, e.g. Bill and
; Lance are composed multiple sprites.  In this method, documentation is careful
; to refer to sprites as an individual 8x16 NES sprite and not the meta-sprite.
; input
;  * a - meta-sprite code, offset into sprite_ptr_tbl_0 or sprite_ptr_tbl_1
;  * $00 - meta-sprite attribute (overrides any attribute specified in sprite data)
;  * $01 - meta-sprite Y position on screen
;  * $02 - meta-sprite X position
;  * $0a - meta-sprite horizontal and vertical flip bits
;  * $0b - bit 0 stores meta-sprite recoil effect flag
load_enemy_metasprite_to_oam_buffer:
    asl                      ; double since each entry in sprite_ptr_tbl_x is 2 bytes
    tay
    bcc @load_sprite_tbl_0   ; branch if bit 7 of meta-sprite code is clear to use sprite_ptr_tbl_0
    lda sprite_ptr_tbl_1,y   ; offset is >= #$80, use second meta-sprite table sprite_ptr_tbl_1
    sta $08                  ; store low byte of meta-sprite address into a
    lda sprite_ptr_tbl_1+1,y ; load high byte of meta-sprite address into a
    bcs @continue            ; always branch

@load_sprite_tbl_0:
    lda sprite_ptr_tbl_0,y   ; load low byte of meta-sprite address into a
    sta $08                  ; store low byte of meta-sprite address into a
    lda sprite_ptr_tbl_0+1,y ; load high byte of meta-sprite address into a

@continue:
    sta $09                 ; store high byte of meta-sprite address into $09
    ldy #$00                ; initialize meta-sprite read offset
    lda ($08),y             ; load first byte from meta-sprite
    bmi @write_small_sprite ; branch if first byte is negative, indicating a small sprite (single 8x16 tile)
                            ; small sprite writes the sprite tile specified in a at position ($02-#$04, $01-#$08)
    tay                     ; normal meta-sprite, transfer number of tiles to y
                            ; to start reading from the end of the meta-sprite data
    bne @handle_sprite      ; branch if number of tiles is more than 0 to process sprite within meta-sprite
                            ; always branches as no meta-sprite starts with a #$00 byte

@load_sprite:
    lda ($08),y
    cmp #$80           ; see if the byte is the 'stop recoil effect' byte
    bne @handle_sprite
    sta $0b            ; store #$80 to stop the rest of the sprites within the meta-sprite from being impacted by recoil
                       ; e.g. an enemy's meta-sprite top-half is affected, but not the legs.
                       ; only bit 0 is important in this assignment
    dey                ; decrement meta-sprite read offset

; process a single sprite within the meta-sprite
@handle_sprite:
    bit $0a         ; set overflow flag when $0a specifies to flip sprite horizontally
    lda ($08),y     ; load sprite relative X position within meta-sprite
    bvc @calc_x_pos ; branch if not flipping sprite horizontally
    eor #$ff        ; flipping sprite horizontally, flip all bits and subtract #$07
    clc             ; clear carry in preparation for addition
    adc #$f9        ; subtract 7 to get the reflected relative X position

@calc_x_pos:
    clc
    bpl @pos_rel_calc_x ; branch if relative X position within meta-sprite is positive
                        ; to calculate X position accounting for positive overflow
    adc $02             ; relative X position within meta-sprite is negative, add meta-sprite X position
    bcc @skip_sprite    ; branch to not draw sprite if carry clear
                        ; this is because the resulting x location is negative, i.e. off screen to the left

@write_to_buffer:
    sta OAMDMA_CPU_BUFFER+3,x ; set X position of sprite
    dey                       ; decrement meta-sprite read offset
    lda ($08),y               ; read sprite attribute
    ora $00                   ; merge sprite attribute override
    eor $0a                   ; merge sprite flip flags (handles flipping a flipped sprite byte)
    sta OAMDMA_CPU_BUFFER+2,x ; set sprite attributes
    dey                       ; decrement meta-sprite code read offset
    lda ($08),y               ; read sprite pattern table tile offset
    sta OAMDMA_CPU_BUFFER+1,x ; set sprite pattern table tile offset
    dey                       ; decrement meta-sprite code read offset
    lda $0b                   ; load sprite recoil effect flag
    lsr                       ; push whether or not recoil effect flat set
    lda ($08),y
    bpl @add_y
    adc $01                   ; relative Y position within meta-sprite is negative
                              ; add meta-sprite Y position (plus any recoil effect of 1px)
    bcc @next_byte            ; branch if no carry occurred, indicating sprite is off screen above
                              ; this will cause the y value to stay #$f4, which isn't drawn
                              ; (any y value between #$ef-#$ff are not drawn)
    clc                       ; sprite on the screen, clear carry for use in calculating next OAM CPU buffer offset

@write_y_pos_next:
    sta OAMDMA_CPU_BUFFER,x ; set sprite Y position
    txa                     ; transfer OAM buffer offset to a
    adc #$c4                ; move to next sprite location within the buffer
    tax                     ; transfer new OAM buffer offset back to x

@next_byte:
    dey
    bne @load_sprite ; branch to load the next sprite within the meta-sprite
                     ; if there is at least one more sprite to draw

@exit:
    rts

@add_y:
    adc $01               ; add sprite Y position
    bcc @write_y_pos_next ; branch if no overflow to write sprite Y position to OAM CPU buffer
    dey                   ; resulting Y position had an overflow, don't set Y position
                          ; this will cause the y value to stay #$f4, which isn't drawn
                          ; (any y value between #$ef-#$ff are not drawn)
    bne @load_sprite      ; branch to load the next sprite within the meta-sprite
                          ; if there is at least one more sprite to draw
    rts                   ; exit if drawn all sprites for meta-sprite

; determine X position on screen for sprite whose relative X position within a meta-sprite is positive
; doesn't draw if off screen to the right
@pos_rel_calc_x:
    adc $02              ; add sprite X position
    bcc @write_to_buffer ; branch if no overflow
                         ; if overflow, skip writing sprite to OAM CPU buffer
                         ; this is because the resulting x location has overflown,
                         ; i.e. off screen to the right

; skip drawing current sprite to OAM CPU buffer, since it is off screen
@skip_sprite:
    dey
    dey
    dey
    bne @next_byte

; writes the sprite tile specified in a at position ($02-#$04, $01-#$08)
; input
;  * a - sprite tile offset, indexes into right pattern table
;  * $00 - sprite attribute
;  * $01 - sprite Y position on screen
;  * $02 - sprite X position
@write_small_sprite:
    cmp #$ff
    beq @exit                 ; exit if end of sprite data
    sec                       ; using right pattern table
    rol                       ; double sprite tile offset
                              ; clears bit 7 and sets to use right pattern table
    sta OAMDMA_CPU_BUFFER+1,x ; store sprite tile index (byte 0)
    lda $00                   ; load sprite attributes
    sta OAMDMA_CPU_BUFFER+2,x ; store sprite attributes (byte 2)
    lda $01                   ; load sprite Y position
    sbc #$08                  ; subtract #$08 from sprite Y position
    sta OAMDMA_CPU_BUFFER,x   ; store sprite Y position
    lda $02                   ; load sprite X position
    sbc #$04                  ; subtract #$04 from sprite X position
    sta OAMDMA_CPU_BUFFER+3,x ; store sprite X position (byte 3)
    txa                       ; transfer OAM offset to a
    clc                       ; clear carry in preparation for addition
    adc #$c4                  ; add #$c4 to current write offset
                              ; when filling the OAMDMA data, the sprite tiles aren't written sequently, but
                              ; rather spaced every 49 sprite tiles apart (#$c4 bytes) wrapping around.
                              ; I'm not sure why this was done. Perhaps it has to do with concern about dynamic RAM
                              ; decay that OAM has.
    tax                       ; transfer OAM offset to x
    rts

; pointer table for all non-player sprites: enemies, explosions, bullets, etc.
sprite_ptr_tbl_0:
    .addr sprite_00
    .addr sprite_00
    .addr sprite_02 ; sprite_02
    .addr sprite_03 ; small ball explosion
    .addr sprite_04 ; small open ball explosion
    .addr sprite_05 ; red bullet
    .addr sprite_06 ; metal bullet
    .addr sprite_07 ; flying capsule
    .addr sprite_08 ; M weapon
    .addr sprite_09 ; S weapon
    .addr sprite_0a ; L weapon
    .addr sprite_0b ; F weapon
    .addr sprite_0c ; R weapon
    .addr sprite_0d ; B weapon
    .addr sprite_0e ; falcon weapon
    .addr sprite_0f ; spike explosion
    .addr sprite_10 ; large explosion
    .addr sprite_11 ; large explosion broken up
    .addr sprite_12 ; large ball explosion
    .addr sprite_13 ; large open ball explosion
    .addr sprite_14 ; large open ball explosion decay
    .addr sprite_15 ; vertical laser
    .addr sprite_16 ; horizontal laser
    .addr sprite_17 ; up-left, up-right laser
    .addr sprite_18 ; down-left, down-right laser
    .addr sprite_19 ; red bullet
    .addr sprite_1a ; ball explosion
    .addr sprite_1b ; soldier running
    .addr sprite_1c ; soldier running
    .addr sprite_1d ; soldier running
    .addr sprite_1e ; soldier running
    .addr sprite_1f ; soldier running
    .addr sprite_20 ; soldier jumping
    .addr sprite_21 ; soldier with weapon
    .addr sprite_22 ; unused soldier prone
    .addr sprite_23 ; soldier aiming angled up
    .addr sprite_24 ; soldier with weapon
    .addr sprite_25 ; soldier aiming angled down
    .addr sprite_26 ; sandbag sniper
    .addr sprite_27 ; grenade thrower
    .addr sprite_28 ; grenade thrower
    .addr sprite_29 ; grenade
    .addr sprite_2a ; grenade
    .addr sprite_2b ; grenade
    .addr sprite_2c ; grenade
    .addr sprite_2d ; grenade
    .addr sprite_2e ; grenade
    .addr sprite_2f ; grenade
    .addr sprite_30 ; grenade
    .addr sprite_31 ; orian
    .addr sprite_32 ; orian
    .addr sprite_33 ; orian
    .addr sprite_34 ; orian
    .addr sprite_35 ; orian
    .addr sprite_36 ; orian
    .addr sprite_37 ; soldier in water
    .addr sprite_38 ; soldier in water
    .addr sprite_39 ; soldier in water
    .addr sprite_3a ; mortar round
    .addr sprite_3b ; overhead soldier
    .addr sprite_3b ; overhead soldier
    .addr sprite_3d ; overhead soldier
    .addr sprite_3e ; overhead soldier
    .addr sprite_3f ; overhead soldier
    .addr sprite_40 ; overhead soldier
    .addr sprite_41 ; overhead soldier
    .addr sprite_42 ; overhead soldier
    .addr sprite_43 ; overhead soldier
    .addr sprite_44 ; overhead soldier
    .addr sprite_45 ; overhead soldier
    .addr sprite_46 ; overhead soldier
    .addr sprite_47 ; overhead soldier
    .addr sprite_48 ; overhead soldier
    .addr sprite_49 ; overhead soldier
    .addr sprite_4a ; overhead soldier
    .addr sprite_4b ; overhead tank soldier
    .addr sprite_4c ; overhead tank soldier
    .addr sprite_4d ; overhead tank soldier
    .addr sprite_4e ; overhead tank soldier
    .addr sprite_4f ; overhead tank soldier
    .addr sprite_50 ; overhead tank soldier
    .addr sprite_51 ; overhead tank soldier
    .addr sprite_52 ; overhead tank soldier
    .addr sprite_53 ; overhead tank soldier
    .addr sprite_54 ; tank boss electrode
    .addr sprite_55 ; tank boss electrode
    .addr sprite_56 ; tank boss electrode
    .addr sprite_57 ; tank boss electrode
    .addr sprite_58 ; tank boss electrode
    .addr sprite_59 ; crouching soldier
    .addr sprite_5a ; crouching soldier
    .addr sprite_5b ; crouching soldier
    .addr sprite_5c ; spinning bubbles
    .addr sprite_5d ; spinning bubbles
    .addr sprite_5e ; spinning bubbles
    .addr sprite_5f ; spinning bubbles
    .addr sprite_60 ; spinning bubbles
    .addr sprite_61 ; spinning bubbles
    .addr sprite_62 ; falling ceiling tile
    .addr sprite_63 ; chandelier laser
    .addr sprite_64 ; rack turret
    .addr sprite_65 ; rack turret
    .addr sprite_66 ; winged soldier
    .addr sprite_67 ; winged soldier
    .addr sprite_68 ; winged soldier
    .addr sprite_69 ; winged soldier
    .addr sprite_6a ; winged soldier
    .addr sprite_6b ; winged soldier
    .addr sprite_6c ; winged soldier
    .addr sprite_6d ; winged soldier
    .addr sprite_6e ; winged soldier
    .addr sprite_6f ; falling rock
    .addr sprite_70 ; falling rock
    .addr sprite_71 ; falling rock
    .addr sprite_72 ; falling rock
    .addr sprite_73 ; red blob
    .addr sprite_74 ; red blob
    .addr sprite_75 ; alien skull
    .addr sprite_76 ; alien skull
    .addr sprite_77 ; alien skull
    .addr sprite_78 ; alien ladybug
    .addr sprite_79 ; alien ladybug
    .addr sprite_7a ; alien ladybug
    .addr sprite_7b ; alien ladybug
    .addr sprite_7c ; alien ladybug
    .addr sprite_7d ; alien ladybug
    .addr sprite_7e ; alien ladybug
    .addr sprite_7f ; alien ladybug

sprite_ptr_tbl_1:
    .addr sprite_80 ; alien ladybug
    .addr sprite_81 ; alien ladybug
    .addr sprite_82 ; baby alien ladybug
    .addr sprite_83 ; baby alien ladybug
    .addr sprite_84 ; big faced one-eyed monster
    .addr sprite_85 ; big faced one-eyed monster
    .addr sprite_86 ; big faced one-eyed monster
    .addr sprite_87 ; big faced one-eyed monster
    .addr sprite_88 ; alien serpent head
    .addr sprite_89 ; alien serpent head
    .addr sprite_8a ; alien serpent head
    .addr sprite_8b ; alien serpent head
    .addr sprite_8c ; alien serpent head
    .addr sprite_8d ; alien serpent head
    .addr sprite_8e ; alien serpent body
    .addr sprite_8f ; jagger froid projectile
    .addr sprite_90 ; manooki
    .addr sprite_91 ; manooki
    .addr sprite_92 ; manooki projectile
    .addr sprite_93 ; manooki projectile
    .addr sprite_94 ; poison drop
    .addr sprite_95 ; poison drop
    .addr sprite_96 ; poison drop
    .addr sprite_97 ; poison drop
    .addr sprite_98 ; poison drop
    .addr sprite_99 ; poison drop
    .addr sprite_9a ; poison drop
    .addr sprite_9b ; poison drop
    .addr sprite_9c ; poison drop
    .addr sprite_9d ; poison drop
    .addr sprite_9e ; alien spider
    .addr sprite_9f ; alien spider
    .addr sprite_a0 ; alien spider
    .addr sprite_a1 ; fire ring projectile
    .addr sprite_a2 ; fire ring projectile
    .addr sprite_a3 ; fire ring projectile
    .addr sprite_a4 ; fire ring projectile
    .addr sprite_a5 ; temple of terror red blob
    .addr sprite_a6 ; temple of terror red blob
    .addr sprite_a7 ; level 1 boss helicopter heart
    .addr sprite_a8 ; red bubble
    .addr sprite_a9 ; red bubble
    .addr sprite_aa ; blob
    .addr sprite_ab ; blob
    .addr sprite_ac ; crumbling rock
    .addr sprite_ad ; crumbling rock
    .addr sprite_ae ; crumbling rock
    .addr sprite_af ; crumbling rock
    .addr sprite_b0 ; crumbling rock
    .addr sprite_b1 ; crumbling rock
    .addr sprite_b2 ; crumbling rock
    .addr sprite_b3 ; crumbling rock
    .addr sprite_b4 ; fortress wall turret
    .addr sprite_b5 ; fortress wall turret
    .addr sprite_b6 ; fortress wall turret
    .addr sprite_b7 ; fortress wall turret
    .addr sprite_b8 ; intro screen logo red c bottom
    .addr sprite_b9 ; intro screen cursor
    .addr sprite_ba ; ending credits helicopter
    .addr sprite_bb ; ending credits helicopter
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc
    .addr sprite_bc

sprite_00:
    .byte $ff

; spike explosion
sprite_02:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$59,$00,$f8 ; 8x16 tile #$59 at location (-8,-8)
    .byte $f8,$59,$c0,$00 ; 8x16 tile #$59 at location (0,-8)with sprite attribute #$c0

; small ball explosion
sprite_03:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$7d,$00,$f8 ; 8x16 tile #$7d at location (-8,-8)
    .byte $f8,$7d,$c0,$00 ; 8x16 tile #$7d at location (0,-8)with sprite attribute #$c0

; small open ball explosion
sprite_04:
    .byte $c7 ; 8x16 tile #$8e from right pattern table

; red bullet
sprite_05:
    .byte $80 ; 8x16 tile #$00 from right pattern table (small sprite)

; metal bullet
sprite_06:
    .byte $d1 ; 8x16 tile #$a2 from right pattern table (small sprite)

; flying capsule
sprite_07:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$55,$00,$f4 ; 8x16 tile #$55 at location (-12,-8)
    .byte $f8,$57,$00,$fc ; 8x16 tile #$57 at location (-4,-8)
    .byte $f8,$55,$40,$04 ; 8x16 tile #$55 at location (4,-8) (horizontal flip, palette 0)

; M weapon
sprite_08:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$93,$00,$fc ; 8x16 tile #$93 at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; S weapon
sprite_09:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$95,$00,$fc ; 8x16 tile #$95 at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; L weapon
sprite_0a:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$97,$00,$fc ; 8x16 tile #$97 at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; F weapon
sprite_0b:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$99,$00,$fc ; 8x16 tile #$99 at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; R weapon
sprite_0c:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$9b,$00,$fc ; 8x16 tile #$9b at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; B weapon
sprite_0d:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$9d,$00,$fc ; 8x16 tile #$9d at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; falcon weapon
sprite_0e:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$9f,$00,$fc ; 8x16 tile #$9f at location (-4,-8)
    .byte $f8,$91,$00,$f4 ; 8x16 tile #$91 at location (-12,-8)
    .byte $f8,$91,$40,$04 ; 8x16 tile #$91 at location (4,-8) (horizontal flip, palette 0)

; spike explosion
sprite_0f:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f4,$5d,$00,$f0 ; 8x16 tile #$5d at location (-16,-12)
    .byte $f0,$5f,$00,$f8 ; 8x16 tile #$5f at location (-8,-16)
    .byte $f0,$61,$c0,$00 ; 8x16 tile #$61 at location (0,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$61,$00,$f8 ; 8x16 tile #$61 at location (-8,0)
    .byte $00,$5f,$c0,$00 ; 8x16 tile #$5f at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $fc,$5d,$c0,$08 ; 8x16 tile #$5d at location (8,-4) (vertical flip, horizontal flip, palette 0)

; large explosion
sprite_10:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$63,$00,$f0 ; 8x16 tile #$63 at location (-16,-16)
    .byte $f0,$65,$00,$f8 ; 8x16 tile #$65 at location (-8,-16)
    .byte $f0,$69,$c0,$00 ; 8x16 tile #$69 at location (0,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f0,$67,$c0,$08 ; 8x16 tile #$67 at location (8,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$67,$00,$f0 ; 8x16 tile #$67 at location (-16,0)
    .byte $00,$69,$00,$f8 ; 8x16 tile #$69 at location (-8,0)
    .byte $00,$65,$c0,$00 ; 8x16 tile #$65 at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$63,$c0,$08 ; 8x16 tile #$63 at location (8,0) (vertical flip, horizontal flip, palette 0)

; large explosion broken up
sprite_11:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f8,$6b,$00,$f0 ; 8x16 tile #$6b at location (-16,-8)
    .byte $f0,$6d,$00,$f8 ; 8x16 tile #$6d at location (-8,-16)
    .byte $f0,$6b,$c0,$00 ; 8x16 tile #$6b at location (0,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f8,$6d,$40,$08 ; 8x16 tile #$6d at location (8,-8) (horizontal flip, palette 0)
    .byte $00,$6b,$00,$f8 ; 8x16 tile #$6b at location (-8,0)
    .byte $00,$6d,$c0,$08 ; 8x16 tile #$6d at location (8,0) (vertical flip, horizontal flip, palette 0)

; large ball explosion
sprite_12:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$7f,$00,$f4 ; 8x16 tile #$7f at location (-12,-16)
    .byte $f0,$81,$00,$fc ; 8x16 tile #$81 at location (-4,-16)
    .byte $f0,$7f,$40,$04 ; 8x16 tile #$7f at location (4,-16) (horizontal flip, palette 0)
    .byte $00,$7f,$80,$f4 ; 8x16 tile #$7f at location (-12,0) (vertical flip, palette 0)
    .byte $00,$81,$c0,$fc ; 8x16 tile #$81 at location (-4,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$7f,$c0,$04 ; 8x16 tile #$7f at location (4,0) (vertical flip, horizontal flip, palette 0)

; large open ball explosion
sprite_13:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$71,$00,$f0 ; 8x16 tile #$71 at location (-16,-16)
    .byte $f0,$73,$00,$f8 ; 8x16 tile #$73 at location (-8,-16)
    .byte $f0,$73,$40,$00 ; 8x16 tile #$73 at location (0,-16) (horizontal flip, palette 0)
    .byte $f0,$71,$40,$08 ; 8x16 tile #$71 at location (8,-16) (horizontal flip, palette 0)
    .byte $00,$71,$80,$f0 ; 8x16 tile #$71 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$73,$80,$f8 ; 8x16 tile #$73 at location (-8,0) (vertical flip, palette 0)
    .byte $00,$73,$c0,$00 ; 8x16 tile #$73 at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$71,$c0,$08 ; 8x16 tile #$71 at location (8,0) (vertical flip, horizontal flip, palette 0)

; large open ball explosion decay
sprite_14:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$75,$00,$f0 ; 8x16 tile #$75 at location (-16,-16)
    .byte $f0,$77,$00,$f8 ; 8x16 tile #$77 at location (-8,-16)
    .byte $f0,$77,$40,$00 ; 8x16 tile #$77 at location (0,-16) (horizontal flip, palette 0)
    .byte $f0,$75,$40,$08 ; 8x16 tile #$75 at location (8,-16) (horizontal flip, palette 0)
    .byte $00,$75,$80,$f0 ; 8x16 tile #$75 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$77,$80,$f8 ; 8x16 tile #$77 at location (-8,0) (vertical flip, palette 0)
    .byte $00,$77,$c0,$00 ; 8x16 tile #$77 at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$75,$c0,$08 ; 8x16 tile #$75 at location (8,0) (vertical flip, horizontal flip, palette 0)

; vertical laser
sprite_15:
    .byte $c5 ; 8x16 tile #$8a from right pattern table (small sprite)

; horizontal laser
sprite_16:
    .byte $bc ; 8x16 tile #$78 from right pattern table (small sprite)

; up-left, up-right laser
sprite_17:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$7b,$00,$fc ; 8x16 tile #$7b at location (-4,-8)

; down-left, down-right laser
sprite_18:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$7b,$80,$fc ; 8x16 tile #$7b at location (-4,-8) (vertical flip, palette 0)

; red bullet
sprite_19:
    .byte $d0 ; 8x16 tile #$a0 from right pattern table (small sprite)

; ball explosion
sprite_1a:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$7f,$00,$f4 ; 8x16 tile #$7f at location (-12,-16)
    .byte $f0,$81,$00,$fc ; 8x16 tile #$81 at location (-4,-16)
    .byte $f0,$7f,$40,$04 ; 8x16 tile #$7f at location (4,-16) (horizontal flip, palette 0)
    .byte $00,$7f,$80,$f4 ; 8x16 tile #$7f at location (-12,0) (vertical flip, palette 0)
    .byte $00,$81,$c0,$fc ; 8x16 tile #$81 at location (-4,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$7f,$c0,$04 ; 8x16 tile #$7f at location (4,0) (vertical flip, horizontal flip, palette 0)

; soldier running
sprite_1b:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f0,$a9,$00,$f8 ; 8x16 tile #$a9 at location (-8,-16)
    .byte $ee,$ab,$00,$00 ; 8x16 tile #$ab at location (0,-18)
    .byte $00,$ad,$00,$f4 ; 8x16 tile #$ad at location (-12,0)
    .byte $fe,$af,$00,$fc ; 8x16 tile #$af at location (-4,-2)

; soldier running
sprite_1c:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f1,$b1,$00,$fc ; 8x16 tile #$b1 at location (-4,-15)
    .byte $f8,$b3,$00,$f5 ; 8x16 tile #$b3 at location (-11,-8)
    .byte $01,$b5,$00,$fd ; 8x16 tile #$b5 at location (-3,1)

; soldier running
sprite_1d:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f0,$b9,$00,$ff ; 8x16 tile #$b9 at location (-1,-16)
    .byte $f0,$b7,$00,$f7 ; 8x16 tile #$b7 at location (-9,-16)
    .byte $00,$bb,$00,$fa ; 8x16 tile #$bb at location (-6,0)

; soldier running
sprite_1e:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f0,$b9,$00,$ff ; 8x16 tile #$b9 at location (-1,-16)
    .byte $f0,$b7,$00,$f7 ; 8x16 tile #$b7 at location (-9,-16)
    .byte $00,$ad,$00,$f4 ; 8x16 tile #$ad at location (-12,0)
    .byte $fe,$af,$00,$fc ; 8x16 tile #$af at location (-4,-2)

; soldier running
sprite_1f:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f0,$a9,$00,$f8 ; 8x16 tile #$a9 at location (-8,-16)
    .byte $ee,$ab,$00,$00 ; 8x16 tile #$ab at location (0,-18)
    .byte $00,$bb,$00,$fa ; 8x16 tile #$bb at location (-6,0)

; soldier jumping
sprite_20:
    .byte $10             ; number of bytes in meta-sprite
    .byte $ee,$bd,$00,$f8 ; 8x16 tile #$bd at location (-8,-18)
    .byte $ee,$bf,$00,$00 ; 8x16 tile #$bf at location (0,-18)
    .byte $fe,$c1,$00,$f6 ; 8x16 tile #$c1 at location (-10,-2)
    .byte $fe,$c3,$00,$fe ; 8x16 tile #$c3 at location (-2,-2)

; soldier with weapon
sprite_21:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $00,$bb,$00,$fa ; 8x16 tile #$bb at location (-6,0)
    .byte $80             ; stop re-coil effect
    .byte $f0,$c7,$00,$00 ; 8x16 tile #$c7 at location (0,-16)
    .byte $f0,$c5,$00,$f8 ; 8x16 tile #$c5 at location (-8,-16)

; unused soldier prone
sprite_22:
    .byte $10             ; number of bytes in meta-sprite
    .byte $00,$dd,$00,$f0 ; 8x16 tile #$dd at location (-16,0)
    .byte $00,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,0)
    .byte $00,$d9,$00,$00 ; 8x16 tile #$d9 at location (0,0)
    .byte $00,$db,$00,$08 ; 8x16 tile #$db at location (8,0)

; soldier aiming angled up
sprite_23:
    .byte $11             ; number of bytes in meta-sprite
    .byte $00,$d3,$00,$f7 ; 8x16 tile #$d3 at location (-9,0)
    .byte $00,$d5,$00,$ff ; 8x16 tile #$d5 at location (-1,0)
    .byte $80             ; stop re-coil effect
    .byte $f0,$cb,$00,$00 ; 8x16 tile #$cb at location (0,-16)
    .byte $f0,$c9,$00,$f8 ; 8x16 tile #$c9 at location (-8,-16)

; soldier with weapon
sprite_24:
    .byte $11             ; number of bytes in meta-sprite
    .byte $00,$d3,$00,$f7 ; 8x16 tile #$d3 at location (-9,0)
    .byte $00,$d5,$00,$ff ; 8x16 tile #$d5 at location (-1,0)
    .byte $80             ; stop re-coil effect
    .byte $f0,$c7,$00,$00 ; 8x16 tile #$c7 at location (0,-16)
    .byte $f0,$cd,$00,$f8 ; 8x16 tile #$cd at location (-8,-16)

; soldier aiming angled down
sprite_25:
    .byte $11             ; number of bytes in meta-sprite
    .byte $00,$d3,$00,$f7 ; tile #$d3 at location (-9,0)
    .byte $00,$d5,$00,$ff ; tile #$d5 at location (-1,0)
    .byte $80             ; stop re-coil effect
    .byte $f2,$d1,$00,$00 ; tile #$d1 at location (0,-14)
    .byte $f0,$cf,$00,$f8 ; tile #$cf at location (-8,-16)

; sandbag sniper
sprite_26:
    .byte $10             ; number of bytes in meta-sprite
    .byte $00,$dd,$00,$f0 ; 8x16 tile #$dd at location (-16,0)
    .byte $00,$df,$00,$f8 ; 8x16 tile #$df at location (-8,0)
    .byte $ff,$e1,$00,$00 ; 8x16 tile #$e1 at location (0,-1)
    .byte $00,$e3,$00,$08 ; 8x16 tile #$e3 at location (8,0)

; grenade thrower
sprite_27:
    .byte $14             ; number of bytes in meta-sprite
    .byte $f0,$e9,$00,$f6 ; 8x16 tile #$e9 at location (-10,-16)
    .byte $f0,$eb,$00,$fe ; 8x16 tile #$eb at location (-2,-16)
    .byte $f1,$ed,$00,$06 ; 8x16 tile #$ed at location (6,-15)
    .byte $00,$d3,$00,$f7 ; 8x16 tile #$d3 at location (-9,0)
    .byte $00,$d5,$00,$ff ; 8x16 tile #$d5 at location (-1,0)

; grenade thrower
sprite_28:
    .byte $14             ; number of bytes in meta-sprite
    .byte $e0,$ef,$00,$fb ; 8x16 tile #$ef at location (-5,-32)
    .byte $f0,$f1,$00,$f6 ; 8x16 tile #$f1 at location (-10,-16)
    .byte $f0,$f3,$00,$fe ; 8x16 tile #$f3 at location (-2,-16)
    .byte $00,$d3,$00,$f7 ; 8x16 tile #$d3 at location (-9,0)
    .byte $00,$d5,$00,$ff ; 8x16 tile #$d5 at location (-1,0)

; grenade
sprite_29:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$f5,$00,$fb ; 8x16 tile #$f5 at location (-5,-8)

; grenade
sprite_2a:
    .byte $fb ; 8x16 tile #$f6 from right pattern table (small sprite)

; grenade
sprite_2b:
    .byte $fc ; 8x16 tile #$f8 from right pattern table (small sprite)

; grenade
sprite_2c:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$f7,$c0,$fc ; 8x16 tile #$f7 at location (-4,-8) (vertical flip, horizontal flip, palette 0)

; grenade
sprite_2d:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$f5,$c0,$fd ; 8x16 tile #$f5 at location (-3,-8) (vertical flip, horizontal flip, palette 0)

; grenade
sprite_2e:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$f7,$c0,$fc ; 8x16 tile #$f7 at location (-4,-8) (vertical flip, horizontal flip, palette 0)

; grenade
sprite_2f:
    .byte $fc ; 8x16 tile #$f8 from right pattern table (small sprite)

; grenade
sprite_30:
    .byte $fb ; 8x16 tile #$f6 from right pattern table (small sprite)

; orian
sprite_31:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$a9,$03,$f9 ; 8x16 tile #$a9 at location (-7,-16) (palette 3)
    .byte $ef,$ab,$03,$fe ; 8x16 tile #$ab at location (-2,-17) (palette 3)
    .byte $ef,$ad,$03,$06 ; 8x16 tile #$ad at location (6,-17) (palette 3)
    .byte $02,$af,$03,$f0 ; 8x16 tile #$af at location (-16,2) (palette 3)
    .byte $00,$b1,$03,$f8 ; 8x16 tile #$b1 at location (-8,0) (palette 3)
    .byte $ff,$b3,$03,$00 ; 8x16 tile #$b3 at location (0,-1) (palette 3)

; orian
sprite_32:
    .byte $14             ; number of bytes in meta-sprite
    .byte $ef,$b5,$03,$fa ; 8x16 tile #$b5 at location (-6,-17) (palette 3)
    .byte $ef,$b7,$03,$02 ; 8x16 tile #$b7 at location (2,-17) (palette 3)
    .byte $ff,$b9,$03,$f4 ; 8x16 tile #$b9 at location (-12,-1) (palette 3)
    .byte $ff,$bb,$03,$fc ; 8x16 tile #$bb at location (-4,-1) (palette 3)
    .byte $ff,$bd,$03,$04 ; 8x16 tile #$bd at location (4,-1) (palette 3)

; orian
sprite_33:
    .byte $14             ; number of bytes in meta-sprite
    .byte $f0,$bf,$03,$f9 ; 8x16 tile #$bf at location (-7,-16) (palette 3)
    .byte $f0,$c1,$03,$01 ; 8x16 tile #$c1 at location (1,-16) (palette 3)
    .byte $04,$c3,$03,$ee ; 8x16 tile #$c3 at location (-18,4) (palette 3)
    .byte $00,$c5,$03,$f4 ; 8x16 tile #$c5 at location (-12,0) (palette 3)
    .byte $00,$c7,$03,$fc ; 8x16 tile #$c7 at location (-4,0) (palette 3)

; orian
sprite_34:
    .byte $14             ; number of bytes in meta-sprite
    .byte $f0,$bf,$03,$f9 ; 8x16 tile #$bf at location (-7,-16) (palette 3)
    .byte $f0,$c1,$03,$01 ; 8x16 tile #$c1 at location (1,-16) (palette 3)
    .byte $02,$c3,$03,$ef ; 8x16 tile #$c3 at location (-17,2) (palette 3)
    .byte $00,$c9,$03,$f7 ; 8x16 tile #$c9 at location (-9,0) (palette 3)
    .byte $00,$cb,$03,$ff ; 8x16 tile #$cb at location (-1,0) (palette 3)

; orian
sprite_35:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$a9,$03,$f9 ; 8x16 tile #$a9 at location (-7,-16) (palette 3)
    .byte $ef,$ab,$03,$fe ; 8x16 tile #$ab at location (-2,-17) (palette 3)
    .byte $ef,$ad,$03,$06 ; 8x16 tile #$ad at location (6,-17) (palette 3)
    .byte $04,$c3,$03,$ef ; 8x16 tile #$c3 at location (-17,4) (palette 3)
    .byte $00,$c5,$03,$f5 ; 8x16 tile #$c5 at location (-11,0) (palette 3)
    .byte $00,$cd,$03,$fd ; 8x16 tile #$cd at location (-3,0) (palette 3)

; orian
sprite_36:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$cf,$03,$f9 ; 8x16 tile #$cf at location (-7,-16) (palette 3)
    .byte $ed,$d1,$03,$fe ; 8x16 tile #$d1 at location (-2,-19) (palette 3)
    .byte $eb,$d3,$03,$06 ; 8x16 tile #$d3 at location (6,-21) (palette 3)
    .byte $02,$af,$03,$f0 ; 8x16 tile #$af at location (-16,2) (palette 3)
    .byte $00,$b1,$03,$f8 ; 8x16 tile #$b1 at location (-8,0) (palette 3)
    .byte $ff,$b3,$03,$00 ; 8x16 tile #$b3 at location (0,-1) (palette 3)

; soldier in water
sprite_37:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f0,$d9,$00,$f8 ; 8x16 tile #$d9 at location (-8,-16)
    .byte $f0,$db,$00,$00 ; 8x16 tile #$db at location (0,-16)

; soldier in water
sprite_38:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f1,$e9,$00,$f8 ; 8x16 tile #$e9 at location (-8,-15)
    .byte $f1,$eb,$00,$00 ; 8x16 tile #$eb at location (0,-15)

; soldier in water
sprite_39:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f0,$ed,$00,$f8 ; 8x16 tile #$ed at location (-8,-16)
    .byte $f0,$ef,$00,$00 ; 8x16 tile #$ef at location (0,-16)

; mortar round
sprite_3a:
    .byte $f8 ; 8x16 tile #$f0 from right pattern table (small sprite)

; overhead soldier
sprite_3b:
    .byte $11             ; number of bytes in meta-sprite
    .byte $03,$07,$01,$f7 ; 8x16 tile #$07 at location (-9,3) (palette 1)
    .byte $00,$09,$01,$ff ; 8x16 tile #$09 at location (-1,0) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f3,$ab,$01,$f9 ; 8x16 tile #$ab at location (-7,-13) (palette 1)
    .byte $f0,$ad,$01,$ff ; 8x16 tile #$ad at location (-1,-16) (palette 1)

; overhead soldier
sprite_3d:
    .byte $11             ; number of bytes in meta-sprite
    .byte $04,$0b,$01,$f7 ; 8x16 tile #$0b at location (-9,4) (palette 1)
    .byte $01,$0d,$01,$ff ; 8x16 tile #$0d at location (-1,1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f4,$ab,$01,$f9 ; 8x16 tile #$ab at location (-7,-12) (palette 1)
    .byte $f1,$ad,$01,$ff ; 8x16 tile #$ad at location (-1,-15) (palette 1)

; overhead soldier
sprite_3e:
    .byte $11             ; number of bytes in meta-sprite
    .byte $03,$0f,$01,$f7 ; 8x16 tile #$0f at location (-9,3) (palette 1)
    .byte $00,$11,$01,$ff ; 8x16 tile #$11 at location (-1,0) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f3,$ab,$01,$f9 ; 8x16 tile #$ab at location (-7,-13) (palette 1)
    .byte $f0,$ad,$01,$ff ; 8x16 tile #$ad at location (-1,-16) (palette 1)

; overhead soldier
sprite_3f:
    .byte $11             ; number of bytes in meta-sprite
    .byte $02,$17,$01,$f7 ; 8x16 tile #$17 at location (-9,2) (palette 1)
    .byte $01,$19,$01,$ff ; 8x16 tile #$19 at location (-1,1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f2,$af,$01,$f7 ; 8x16 tile #$af at location (-9,-14) (palette 1)
    .byte $f1,$b1,$01,$ff ; 8x16 tile #$b1 at location (-1,-15) (palette 1)

; overhead soldier
sprite_40:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $02,$1b,$01,$fd ; 8x16 tile #$1b at location (-3,2) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f3,$af,$01,$f7 ; 8x16 tile #$af at location (-9,-13) (palette 1)
    .byte $f2,$b1,$01,$ff ; 8x16 tile #$b1 at location (-1,-14) (palette 1)

; overhead soldier
sprite_41:
    .byte $11             ; number of bytes in meta-sprite
    .byte $02,$1d,$01,$f7 ; 8x16 tile #$1d at location (-9,2) (palette 1)
    .byte $01,$1f,$01,$ff ; 8x16 tile #$1f at location (-1,1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f2,$af,$01,$f7 ; 8x16 tile #$af at location (-9,-14) (palette 1)
    .byte $f1,$b1,$01,$ff ; 8x16 tile #$b1 at location (-1,-15) (palette 1)

; overhead soldier
sprite_42:
    .byte $11             ; number of bytes in meta-sprite
    .byte $ff,$35,$01,$f8 ; 8x16 tile #$35 at location (-8,-1) (palette 1)
    .byte $ff,$37,$01,$00 ; 8x16 tile #$37 at location (0,-1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $ef,$b7,$01,$f8 ; 8x16 tile #$b7 at location (-8,-17) (palette 1)
    .byte $ef,$b9,$01,$00 ; 8x16 tile #$b9 at location (0,-17) (palette 1)

; overhead soldier
sprite_43:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $00,$39,$01,$fd ; 8x16 tile #$39 at location (-3,0) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f0,$b7,$01,$f8 ; 8x16 tile #$b7 at location (-8,-16) (palette 1)
    .byte $f0,$b9,$01,$00 ; 8x16 tile #$b9 at location (0,-16) (palette 1)

; overhead soldier
sprite_44:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $ff,$3b,$01,$fe ; 8x16 tile #$3b at location (-2,-1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $ef,$b7,$01,$f8 ; 8x16 tile #$b7 at location (-8,-17) (palette 1)
    .byte $ef,$b9,$01,$00 ; 8x16 tile #$b9 at location (0,-17) (palette 1)

; overhead soldier
sprite_45:
    .byte $11             ; number of bytes in meta-sprite
    .byte $ff,$25,$01,$f9 ; 8x16 tile #$25 at location (-7,-1) (palette 1)
    .byte $ff,$27,$01,$01 ; 8x16 tile #$27 at location (1,-1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $ef,$b3,$01,$f7 ; 8x16 tile #$b3 at location (-9,-17) (palette 1)
    .byte $ef,$b5,$01,$ff ; 8x16 tile #$b5 at location (-1,-17) (palette 1)

; overhead soldier
sprite_46:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $00,$29,$01,$fd ; 8x16 tile #$29 at location (-3,0) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f0,$b3,$01,$f7 ; 8x16 tile #$b3 at location (-9,-16) (palette 1)
    .byte $f0,$b5,$01,$ff ; 8x16 tile #$b5 at location (-1,-16) (palette 1)

; overhead soldier
sprite_47:
    .byte $11             ; number of bytes in meta-sprite
    .byte $ff,$2b,$01,$fa ; 8x16 tile #$2b at location (-6,-1) (palette 1)
    .byte $ff,$2d,$01,$02 ; 8x16 tile #$2d at location (2,-1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $ef,$b3,$01,$f7 ; 8x16 tile #$b3 at location (-9,-17) (palette 1)
    .byte $ef,$b5,$01,$ff ; 8x16 tile #$b5 at location (-1,-17) (palette 1)

; overhead soldier
sprite_48:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $00,$3d,$01,$fc ; 8x16 tile #$3d at location (-4,0) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f0,$bb,$01,$f9 ; 8x16 tile #$bb at location (-7,-16) (palette 1)
    .byte $f0,$bd,$01,$01 ; 8x16 tile #$bd at location (1,-16) (palette 1)

; overhead soldier
sprite_49:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $01,$43,$01,$fc ; 8x16 tile #$43 at location (-4,1) (palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f1,$bb,$01,$f9 ; 8x16 tile #$bb at location (-7,-15) (palette 1)
    .byte $f1,$bd,$01,$01 ; 8x16 tile #$bd at location (1,-15) (palette 1)

; overhead soldier
sprite_4a:
    .byte $0d             ; number of bytes in meta-sprite
    .byte $00,$3d,$41,$fc ; 8x16 tile #$3d at location (-4,0) (horizontal flip, palette 1)
    .byte $80             ; stop re-coil effect
    .byte $f0,$bb,$01,$f9 ; 8x16 tile #$bb at location (-7,-16) (palette 1)
    .byte $f0,$bd,$01,$01 ; 8x16 tile #$bd at location (1,-16) (palette 1)

; overhead tank soldier
sprite_4b:
    .byte $14             ; number of bytes in meta-sprite
    .byte $ea,$e5,$00,$f9 ; 8x16 tile #$e5 at location (-7,-22)
    .byte $ea,$e5,$40,$00 ; 8x16 tile #$e5 at location (0,-22) (horizontal flip, palette 0)
    .byte $fa,$e7,$00,$f9 ; 8x16 tile #$e7 at location (-7,-6)
    .byte $fa,$e7,$40,$00 ; 8x16 tile #$e7 at location (0,-6) (horizontal flip, palette 0)
    .byte $0a,$e9,$00,$fd ; 8x16 tile #$e9 at location (-3,10)

; overhead tank soldier
sprite_4c:
    .byte $14             ; number of bytes in meta-sprite
    .byte $ea,$ed,$00,$02 ; 8x16 tile #$ed at location (2,-22)
    .byte $f1,$eb,$00,$fa ; 8x16 tile #$eb at location (-6,-15)
    .byte $fa,$f1,$00,$02 ; 8x16 tile #$f1 at location (2,-6)
    .byte $01,$ef,$00,$fa ; 8x16 tile #$ef at location (-6,1)
    .byte $0a,$f3,$00,$f3 ; 8x16 tile #$f3 at location (-13,10)

; overhead tank soldier
sprite_4d:
    .byte $14             ; number of bytes in meta-sprite
    .byte $f2,$f5,$00,$fc ; 8x16 tile #$f5 at location (-4,-14)
    .byte $f2,$f7,$00,$04 ; 8x16 tile #$f7 at location (4,-14)
    .byte $02,$f9,$00,$f3 ; 8x16 tile #$f9 at location (-13,2)
    .byte $02,$fb,$00,$fb ; 8x16 tile #$fb at location (-5,2)
    .byte $02,$fd,$00,$03 ; 8x16 tile #$fd at location (3,2)

; overhead tank soldier
sprite_4e:
    .byte $14             ; number of bytes in meta-sprite
    .byte $d2,$e5,$00,$f9 ; 8x16 tile #$e5 at location (-7,-46)
    .byte $d2,$e5,$40,$00 ; 8x16 tile #$e5 at location (0,-46) (horizontal flip, palette 0)
    .byte $e2,$e7,$00,$f9 ; 8x16 tile #$e7 at location (-7,-30)
    .byte $e2,$e7,$40,$00 ; 8x16 tile #$e7 at location (0,-30) (horizontal flip, palette 0)
    .byte $f2,$e9,$00,$fd ; 8x16 tile #$e9 at location (-3,-14)

; overhead tank soldier
sprite_4f:
    .byte $14             ; number of bytes in meta-sprite
    .byte $d2,$ed,$00,$02 ; 8x16 tile #$ed at location (2,-46)
    .byte $d9,$eb,$00,$fa ; 8x16 tile #$eb at location (-6,-39)
    .byte $e2,$f1,$00,$02 ; 8x16 tile #$f1 at location (2,-30)
    .byte $e9,$ef,$00,$fa ; 8x16 tile #$ef at location (-6,-23)
    .byte $f2,$f3,$00,$f3 ; 8x16 tile #$f3 at location (-13,-14)

; overhead tank soldier
sprite_50:
    .byte $14             ; number of bytes in meta-sprite
    .byte $da,$f5,$00,$fc ; 8x16 tile #$f5 at location (-4,-38)
    .byte $da,$f7,$00,$04 ; 8x16 tile #$f7 at location (4,-38)
    .byte $ea,$f9,$00,$f3 ; 8x16 tile #$f9 at location (-13,-22)
    .byte $ea,$fb,$00,$fb ; 8x16 tile #$fb at location (-5,-22)
    .byte $ea,$fd,$00,$03 ; 8x16 tile #$fd at location (3,-22)

; overhead tank soldier
sprite_51:
    .byte $14             ; number of bytes in meta-sprite
    .byte $d8,$e5,$00,$f9 ; 8x16 tile #$e5 at location (-7,-40)
    .byte $d8,$e5,$40,$00 ; 8x16 tile #$e5 at location (0,-40) (horizontal flip, palette 0)
    .byte $e8,$e7,$00,$f9 ; 8x16 tile #$e7 at location (-7,-24)
    .byte $e8,$e7,$40,$00 ; 8x16 tile #$e7 at location (0,-24) (horizontal flip, palette 0)
    .byte $f8,$e9,$00,$fd ; 8x16 tile #$e9 at location (-3,-8)

; overhead tank soldier
sprite_52:
    .byte $14             ; number of bytes in meta-sprite
    .byte $d8,$ed,$00,$02 ; 8x16 tile #$ed at location (2,-40)
    .byte $df,$eb,$00,$fa ; 8x16 tile #$eb at location (-6,-33)
    .byte $e8,$f1,$00,$02 ; 8x16 tile #$f1 at location (2,-24)
    .byte $ef,$ef,$00,$fa ; 8x16 tile #$ef at location (-6,-17)
    .byte $f8,$f3,$00,$f3 ; 8x16 tile #$f3 at location (-13,-8)

; overhead tank soldier
sprite_53:
    .byte $14             ; number of bytes in meta-sprite
    .byte $e0,$f5,$00,$fc ; 8x16 tile #$f5 at location (-4,-32)
    .byte $e0,$f7,$00,$04 ; 8x16 tile #$f7 at location (4,-32)
    .byte $f0,$f9,$00,$f3 ; 8x16 tile #$f9 at location (-13,-16)
    .byte $f0,$fb,$00,$fb ; 8x16 tile #$fb at location (-5,-16)
    .byte $f0,$fd,$00,$03 ; 8x16 tile #$fd at location (3,-16)

; tank boss electrode
sprite_54:
    .byte $08             ; number of bytes in meta-sprite
    .byte $1f,$e1,$00,$e0 ; 8x16 tile #$e1 at location (-32,31)
    .byte $1f,$e1,$00,$18 ; 8x16 tile #$e1 at location (24,31)

; tank boss electrode
sprite_55:
    .byte $1c             ; number of bytes in meta-sprite
    .byte $1f,$e1,$00,$e0 ; 8x16 tile #$e1 at location (-32,31)
    .byte $1f,$e1,$00,$18 ; 8x16 tile #$e1 at location (24,31)
    .byte $2f,$e3,$00,$e0 ; 8x16 tile #$e3 at location (-32,47)
    .byte $2f,$df,$00,$e8 ; 8x16 tile #$df at location (-24,47)
    .byte $2f,$df,$00,$f8 ; 8x16 tile #$df at location (-8,47)
    .byte $2f,$df,$00,$08 ; 8x16 tile #$df at location (8,47)
    .byte $2f,$e3,$00,$18 ; 8x16 tile #$e3 at location (24,47)

; tank boss electrode
sprite_56:
    .byte $1c             ; number of bytes in meta-sprite
    .byte $1f,$e1,$00,$e0 ; 8x16 tile #$e1 at location (-32,31)
    .byte $1f,$e1,$00,$18 ; 8x16 tile #$e1 at location (24,31)
    .byte $2f,$e3,$00,$e0 ; 8x16 tile #$e3 at location (-32,47)
    .byte $2f,$df,$00,$f0 ; 8x16 tile #$df at location (-16,47)
    .byte $2f,$df,$00,$00 ; 8x16 tile #$df at location (0,47)
    .byte $2f,$df,$00,$10 ; 8x16 tile #$df at location (16,47)
    .byte $2f,$e3,$00,$18 ; 8x16 tile #$e3 at location (24,47)

; tank boss electrode
sprite_57:
    .byte $1c             ; number of bytes in meta-sprite
    .byte $1f,$e1,$00,$e0 ; 8x16 tile #$e1 at location (-32,31)
    .byte $1f,$e1,$00,$18 ; 8x16 tile #$e1 at location (24,31)
    .byte $2f,$e3,$00,$e0 ; 8x16 tile #$e3 at location (-32,47)
    .byte $2f,$df,$80,$e8 ; 8x16 tile #$df at location (-24,47) (vertical flip, palette 0)
    .byte $2f,$df,$80,$f8 ; 8x16 tile #$df at location (-8,47) (vertical flip, palette 0)
    .byte $2f,$df,$80,$08 ; 8x16 tile #$df at location (8,47) (vertical flip, palette 0)
    .byte $2f,$e3,$00,$18 ; 8x16 tile #$e3 at location (24,47)

; tank boss electrode
sprite_58:
    .byte $1c             ; number of bytes in meta-sprite
    .byte $1f,$e1,$00,$e0 ; 8x16 tile #$e1 at location (-32,31)
    .byte $1f,$e1,$00,$18 ; 8x16 tile #$e1 at location (24,31)
    .byte $2f,$e3,$00,$e0 ; 8x16 tile #$e3 at location (-32,47)
    .byte $2f,$df,$80,$f0 ; 8x16 tile #$df at location (-16,47) (vertical flip, palette 0)
    .byte $2f,$df,$80,$00 ; 8x16 tile #$df at location (0,47) (vertical flip, palette 0)
    .byte $2f,$df,$80,$10 ; 8x16 tile #$df at location (16,47) (vertical flip, palette 0)
    .byte $2f,$e3,$00,$18 ; 8x16 tile #$e3 at location (24,47)

; crouching soldier
sprite_59:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f0,$f3,$00,$00 ; 8x16 tile #$f3 at location (0,-16)

; crouching soldier
sprite_5a:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $fe,$f9,$00,$f1 ; 8x16 tile #$f9 at location (-15,-2)
    .byte $f1,$f5,$00,$f8 ; 8x16 tile #$f5 at location (-8,-15)
    .byte $f1,$f7,$00,$00 ; 8x16 tile #$f7 at location (0,-15)

; crouching soldier
sprite_5b:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f0,$fb,$00,$f8 ; 8x16 tile #$fb at location (-8,-16)
    .byte $f0,$fd,$00,$00 ; 8x16 tile #$fd at location (0,-16)

; spinning bubbles
sprite_5c:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$f9,$00,$f8 ; 8x16 tile #$f9 at location (-8,-8)
    .byte $f8,$fd,$01,$fe ; 8x16 tile #$fd at location (-2,-8) (palette 1)

; spinning bubbles
sprite_5d:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$fd,$00,$fa ; 8x16 tile #$fd at location (-6,-8)
    .byte $f8,$f7,$01,$00 ; 8x16 tile #$f7 at location (0,-8) (palette 1)

; spinning bubbles
sprite_5e:
    .byte $fe ; 8x16 tile #$fc from right pattern table (small sprite)

; spinning bubbles
sprite_5f:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$fd,$01,$fc ; 8x16 tile #$fd at location (-4,-8) (palette 1)

; spinning bubbles
sprite_60:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$fd,$01,$fa ; 8x16 tile #$fd at location (-6,-8) (palette 1)
    .byte $f8,$f7,$00,$00 ; 8x16 tile #$f7 at location (0,-8)

; spinning bubbles
sprite_61:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$f9,$01,$f8 ; 8x16 tile #$f9 at location (-8,-8) (palette 1)
    .byte $f8,$fd,$00,$fe ; 8x16 tile #$fd at location (-2,-8)

; falling ceiling tile
sprite_62:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$fb,$03,$f8 ; 8x16 tile #$fb at location (-8,-8) (palette 3)
    .byte $f8,$fb,$43,$00 ; 8x16 tile #$fb at location (0,-8) (horizontal flip, palette 3)

; chandelier laser
sprite_63:
    .byte $fa ; 8x16 tile #$f4 from right pattern table (small sprite)

; rack turret
sprite_64:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$f1,$03,$f8 ; 8x16 tile #$f1 at location (-8,-8) (palette 3)
    .byte $f8,$f3,$03,$00 ; 8x16 tile #$f3 at location (0,-8) (palette 3)
    .byte $05,$ef,$03,$f3 ; 8x16 tile #$ef at location (-13,5) (palette 3)

; rack turret
sprite_65:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$c5,$03,$f8 ; 8x16 tile #$c5 at location (-8,-8) (palette 3)
    .byte $f8,$c5,$43,$00 ; 8x16 tile #$c5 at location (0,-8) (horizontal flip, palette 3)
    .byte $08,$db,$03,$fc ; 8x16 tile #$db at location (-4,8) (palette 3)

; winged soldier
sprite_66:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f0,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,-16)
    .byte $ee,$d9,$00,$00 ; 8x16 tile #$d9 at location (0,-18)
    .byte $00,$ad,$00,$f4 ; 8x16 tile #$ad at location (-12,0)
    .byte $fe,$af,$00,$fc ; 8x16 tile #$af at location (-4,-2)

; winged soldier
sprite_67:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f1,$dd,$00,$fc ; 8x16 tile #$dd at location (-4,-15)
    .byte $f8,$b3,$00,$f5 ; 8x16 tile #$b3 at location (-11,-8)
    .byte $01,$b5,$00,$fd ; 8x16 tile #$b5 at location (-3,1)

; winged soldier
sprite_68:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f0,$e1,$00,$ff ; 8x16 tile #$e1 at location (-1,-16)
    .byte $f0,$df,$00,$f7 ; 8x16 tile #$df at location (-9,-16)
    .byte $00,$bb,$00,$fa ; 8x16 tile #$bb at location (-6,0)

; winged soldier
sprite_69:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f0,$e1,$00,$ff ; 8x16 tile #$e1 at location (-1,-16)
    .byte $f0,$df,$00,$f7 ; 8x16 tile #$df at location (-9,-16)
    .byte $00,$ad,$00,$f4 ; 8x16 tile #$ad at location (-12,0)
    .byte $fe,$af,$00,$fc ; 8x16 tile #$af at location (-4,-2)

; winged soldier
sprite_6a:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f0,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,-16)
    .byte $ee,$d9,$00,$00 ; 8x16 tile #$d9 at location (0,-18)
    .byte $00,$bb,$00,$fa ; 8x16 tile #$bb at location (-6,0)

; winged soldier
sprite_6b:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f0,$e3,$00,$f8 ; 8x16 tile #$e3 at location (-8,-16)
    .byte $f0,$e3,$40,$00 ; 8x16 tile #$e3 at location (0,-16) (horizontal flip, palette 0)
    .byte $00,$e5,$00,$f8 ; 8x16 tile #$e5 at location (-8,0)
    .byte $00,$e5,$40,$00 ; 8x16 tile #$e5 at location (0,0) (horizontal flip, palette 0)

; winged soldier
sprite_6c:
    .byte $14             ; number of bytes in meta-sprite
    .byte $ee,$e7,$00,$f4 ; 8x16 tile #$e7 at location (-12,-18)
    .byte $ee,$e7,$40,$04 ; 8x16 tile #$e7 at location (4,-18) (horizontal flip, palette 0)
    .byte $f5,$e9,$00,$fc ; 8x16 tile #$e9 at location (-4,-11)
    .byte $fe,$eb,$00,$f7 ; 8x16 tile #$eb at location (-9,-2)
    .byte $05,$ed,$00,$00 ; 8x16 tile #$ed at location (0,5)

; winged soldier
sprite_6d:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f1,$f7,$00,$f7 ; 8x16 tile #$f7 at location (-9,-15)
    .byte $f1,$f9,$00,$ff ; 8x16 tile #$f9 at location (-1,-15)
    .byte $f5,$fb,$00,$07 ; 8x16 tile #$fb at location (7,-11)
    .byte $01,$fd,$00,$fe ; 8x16 tile #$fd at location (-2,1)

; winged soldier
sprite_6e:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f1,$f7,$00,$f7 ; 8x16 tile #$f7 at location (-9,-15)
    .byte $f1,$f9,$00,$ff ; 8x16 tile #$f9 at location (-1,-15)
    .byte $f5,$eb,$00,$07 ; 8x16 tile #$eb at location (7,-11)
    .byte $01,$fd,$00,$fe ; 8x16 tile #$fd at location (-2,1)

; falling rock
sprite_6f:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$ef,$00,$f4 ; 8x16 tile #$ef at location (-12,-16)
    .byte $f0,$f1,$00,$fc ; 8x16 tile #$f1 at location (-4,-16)
    .byte $f0,$f3,$c0,$04 ; 8x16 tile #$f3 at location (4,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$f3,$00,$f4 ; 8x16 tile #$f3 at location (-12,0)
    .byte $00,$f5,$00,$fc ; 8x16 tile #$f5 at location (-4,0)
    .byte $00,$ef,$c0,$04 ; 8x16 tile #$ef at location (4,0) (vertical flip, horizontal flip, palette 0)

; falling rock
sprite_70:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$f3,$80,$f4 ; 8x16 tile #$f3 at location (-12,-16) (vertical flip, palette 0)
    .byte $f0,$f1,$40,$fc ; 8x16 tile #$f1 at location (-4,-16) (horizontal flip, palette 0)
    .byte $f0,$ef,$40,$04 ; 8x16 tile #$ef at location (4,-16) (horizontal flip, palette 0)
    .byte $00,$ef,$80,$f4 ; 8x16 tile #$ef at location (-12,0) (vertical flip, palette 0)
    .byte $00,$f5,$40,$fc ; 8x16 tile #$f5 at location (-4,0) (horizontal flip, palette 0)
    .byte $00,$f3,$40,$04 ; 8x16 tile #$f3 at location (4,0) (horizontal flip, palette 0)

; falling rock
sprite_71:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$ef,$00,$f4 ; 8x16 tile #$ef at location (-12,-16)
    .byte $f0,$f5,$c0,$fc ; 8x16 tile #$f5 at location (-4,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f0,$f3,$c0,$04 ; 8x16 tile #$f3 at location (4,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$f3,$00,$f4 ; 8x16 tile #$f3 at location (-12,0)
    .byte $00,$f1,$c0,$fc ; 8x16 tile #$f1 at location (-4,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$ef,$c0,$04 ; 8x16 tile #$ef at location (4,0) (vertical flip, horizontal flip, palette 0)

; falling rock
sprite_72:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$f3,$80,$f4 ; 8x16 tile #$f3 at location (-12,-16) (vertical flip, palette 0)
    .byte $f0,$f5,$80,$fc ; 8x16 tile #$f5 at location (-4,-16) (vertical flip, palette 0)
    .byte $f0,$ef,$40,$04 ; 8x16 tile #$ef at location (4,-16) (horizontal flip, palette 0)
    .byte $00,$ef,$80,$f4 ; 8x16 tile #$ef at location (-12,0) (vertical flip, palette 0)
    .byte $00,$f1,$80,$fc ; 8x16 tile #$f1 at location (-4,0) (vertical flip, palette 0)
    .byte $00,$f3,$40,$04 ; 8x16 tile #$f3 at location (4,0) (horizontal flip, palette 0)

; red blob
sprite_73:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,-8)
    .byte $f8,$d7,$40,$00 ; 8x16 tile #$d7 at location (0,-8) (horizontal flip, palette 0)

; red blob
sprite_74:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$d9,$00,$f8 ; 8x16 tile #$d9 at location (-8,-8)
    .byte $f8,$d9,$40,$00 ; 8x16 tile #$d9 at location (0,-8) (horizontal flip, palette 0)

; alien skull
sprite_75:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$db,$00,$f8 ; 8x16 tile #$db at location (-8,-8)
    .byte $f8,$dd,$00,$00 ; 8x16 tile #$dd at location (0,-8)

; alien skull
sprite_76:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f7,$df,$00,$f8 ; 8x16 tile #$df at location (-8,-9)
    .byte $f7,$e1,$00,$00 ; 8x16 tile #$e1 at location (0,-9)

; alien skull
sprite_77:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f7,$e3,$00,$f8 ; 8x16 tile #$e3 at location (-8,-9)
    .byte $f7,$e9,$00,$00 ; 8x16 tile #$e9 at location (0,-9)

; alien ladybug
sprite_78:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f9,$bb,$00,$f4 ; 8x16 tile #$bb at location (-12,-7)
    .byte $f8,$bd,$00,$fc ; 8x16 tile #$bd at location (-4,-8)
    .byte $f6,$bf,$00,$04 ; 8x16 tile #$bf at location (4,-10)

; alien ladybug
sprite_79:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f6,$bf,$40,$f4 ; 8x16 tile #$bf at location (-12,-10) (horizontal flip, palette 0)
    .byte $f8,$ed,$00,$fc ; 8x16 tile #$ed at location (-4,-8)
    .byte $f9,$bb,$40,$04 ; 8x16 tile #$bb at location (4,-7) (horizontal flip, palette 0)

; alien ladybug
sprite_7a:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f0,$ef,$00,$f3 ; 8x16 tile #$ef at location (-13,-16)
    .byte $f7,$f1,$00,$f8 ; 8x16 tile #$f1 at location (-8,-9)
    .byte $f8,$f3,$00,$00 ; 8x16 tile #$f3 at location (0,-8)
    .byte $00,$f5,$00,$07 ; 8x16 tile #$f5 at location (7,0)

; alien ladybug
sprite_7b:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f2,$f7,$00,$f6 ; 8x16 tile #$f7 at location (-10,-14)
    .byte $f2,$f9,$00,$fe ; 8x16 tile #$f9 at location (-2,-14)
    .byte $02,$fb,$00,$f9 ; 8x16 tile #$fb at location (-7,2)
    .byte $fc,$fd,$00,$01 ; 8x16 tile #$fd at location (1,-4)

; alien ladybug
sprite_7c:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f4,$ab,$00,$f7 ; 8x16 tile #$ab at location (-9,-12)
    .byte $ec,$ad,$00,$ff ; 8x16 tile #$ad at location (-1,-20)
    .byte $fc,$af,$00,$fa ; 8x16 tile #$af at location (-6,-4)
    .byte $fc,$b1,$00,$02 ; 8x16 tile #$b1 at location (2,-4)

; alien ladybug
sprite_7d:
    .byte $10             ; number of bytes in meta-sprite
    .byte $f4,$b3,$00,$fa ; 8x16 tile #$b3 at location (-6,-12)
    .byte $f4,$b5,$00,$02 ; 8x16 tile #$b5 at location (2,-12)
    .byte $fc,$b7,$00,$f7 ; 8x16 tile #$b7 at location (-9,-4)
    .byte $04,$b9,$00,$ff ; 8x16 tile #$b9 at location (-1,4)

; alien ladybug
sprite_7e:
    .byte $10             ; number of bytes in meta-sprite
    .byte $00,$ef,$80,$f3 ; 8x16 tile #$ef at location (-13,0) (vertical flip, palette 0)
    .byte $f9,$f1,$80,$f7 ; 8x16 tile #$f1 at location (-9,-7) (vertical flip, palette 0)
    .byte $f8,$f3,$80,$ff ; 8x16 tile #$f3 at location (-1,-8) (vertical flip, palette 0)
    .byte $f0,$f5,$80,$07 ; 8x16 tile #$f5 at location (7,-16) (vertical flip, palette 0)

; alien ladybug
sprite_7f:
    .byte $10             ; number of bytes in meta-sprite
    .byte $fe,$f7,$80,$f6 ; 8x16 tile #$f7 at location (-10,-2) (vertical flip, palette 0)
    .byte $fe,$f9,$80,$fe ; 8x16 tile #$f9 at location (-2,-2) (vertical flip, palette 0)
    .byte $ee,$fb,$80,$f9 ; 8x16 tile #$fb at location (-7,-18) (vertical flip, palette 0)
    .byte $f4,$fd,$80,$01 ; 8x16 tile #$fd at location (1,-12) (vertical flip, palette 0)

; alien ladybug
sprite_80:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f7,$bb,$80,$f4 ; 8x16 tile #$bb at location (-12,-9) (vertical flip, palette 0)
    .byte $f8,$bd,$80,$fc ; 8x16 tile #$bd at location (-4,-8) (vertical flip, palette 0)
    .byte $fa,$bf,$80,$04 ; 8x16 tile #$bf at location (4,-6) (vertical flip, palette 0)

; alien ladybug
sprite_81:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $fa,$bf,$c0,$f4 ; 8x16 tile #$bf at location (-12,-6) (vertical flip, horizontal flip, palette 0)
    .byte $f8,$ed,$80,$fc ; 8x16 tile #$ed at location (-4,-8) (vertical flip, palette 0)
    .byte $f7,$bb,$c0,$04 ; 8x16 tile #$bb at location (4,-9) (vertical flip, horizontal flip, palette 0)

; baby alien ladybug
sprite_82:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$e5,$00,$f8 ; 8x16 tile #$e5 at location (-8,-8)
    .byte $f8,$e7,$00,$00 ; 8x16 tile #$e7 at location (0,-8)

; baby alien ladybug
sprite_83:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$e9,$00,$f8 ; 8x16 tile #$e9 at location (-8,-8)
    .byte $f8,$eb,$00,$00 ; 8x16 tile #$eb at location (0,-8)

; big faced one-eyed monster
sprite_84:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$c1,$00,$f0 ; 8x16 tile #$c1 at location (-16,-16)
    .byte $f0,$c3,$00,$f8 ; 8x16 tile #$c3 at location (-8,-16)
    .byte $f0,$c3,$40,$00 ; 8x16 tile #$c3 at location (0,-16) (horizontal flip, palette 0)
    .byte $f0,$c1,$40,$08 ; 8x16 tile #$c1 at location (8,-16) (horizontal flip, palette 0)
    .byte $00,$c5,$00,$f0 ; 8x16 tile #$c5 at location (-16,0)
    .byte $00,$c7,$00,$f8 ; 8x16 tile #$c7 at location (-8,0)
    .byte $00,$c7,$40,$00 ; 8x16 tile #$c7 at location (0,0) (horizontal flip, palette 0)
    .byte $00,$c5,$40,$08 ; 8x16 tile #$c5 at location (8,0) (horizontal flip, palette 0)

; big faced one-eyed monster
sprite_85:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$c9,$00,$f0 ; 8x16 tile #$c9 at location (-16,-16)
    .byte $f0,$cb,$00,$f8 ; 8x16 tile #$cb at location (-8,-16)
    .byte $f0,$cd,$00,$00 ; 8x16 tile #$cd at location (0,-16)
    .byte $f0,$cf,$00,$08 ; 8x16 tile #$cf at location (8,-16)
    .byte $00,$d1,$00,$f0 ; 8x16 tile #$d1 at location (-16,0)
    .byte $00,$d3,$00,$f8 ; 8x16 tile #$d3 at location (-8,0)
    .byte $00,$d5,$00,$00 ; 8x16 tile #$d5 at location (0,0)
    .byte $00,$d7,$00,$08 ; 8x16 tile #$d7 at location (8,0)

; big faced one-eyed monster
sprite_86:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$d9,$00,$f0 ; 8x16 tile #$d9 at location (-16,-16)
    .byte $f0,$db,$00,$f8 ; 8x16 tile #$db at location (-8,-16)
    .byte $f0,$dd,$00,$00 ; 8x16 tile #$dd at location (0,-16)
    .byte $f0,$df,$00,$08 ; 8x16 tile #$df at location (8,-16)
    .byte $00,$d9,$80,$f0 ; 8x16 tile #$d9 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$db,$80,$f8 ; 8x16 tile #$db at location (-8,0) (vertical flip, palette 0)
    .byte $00,$dd,$80,$00 ; 8x16 tile #$dd at location (0,0) (vertical flip, palette 0)
    .byte $00,$df,$80,$08 ; 8x16 tile #$df at location (8,0) (vertical flip, palette 0)

; big faced one-eyed monster
sprite_87:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$d1,$80,$f0 ; 8x16 tile #$d1 at location (-16,-16) (vertical flip, palette 0)
    .byte $f0,$d3,$80,$f8 ; 8x16 tile #$d3 at location (-8,-16) (vertical flip, palette 0)
    .byte $f0,$d5,$80,$00 ; 8x16 tile #$d5 at location (0,-16) (vertical flip, palette 0)
    .byte $f0,$d7,$80,$08 ; 8x16 tile #$d7 at location (8,-16) (vertical flip, palette 0)
    .byte $00,$c9,$80,$f0 ; 8x16 tile #$c9 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$cb,$80,$f8 ; 8x16 tile #$cb at location (-8,0) (vertical flip, palette 0)
    .byte $00,$cd,$80,$00 ; 8x16 tile #$cd at location (0,0) (vertical flip, palette 0)
    .byte $00,$cf,$80,$08 ; 8x16 tile #$cf at location (8,0) (vertical flip, palette 0)

; alien serpent head
sprite_88:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$c5,$80,$f0 ; 8x16 tile #$c5 at location (-16,-16) (vertical flip, palette 0)
    .byte $f0,$c7,$80,$f8 ; 8x16 tile #$c7 at location (-8,-16) (vertical flip, palette 0)
    .byte $f0,$c7,$c0,$00 ; 8x16 tile #$c7 at location (0,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f0,$c5,$c0,$08 ; 8x16 tile #$c5 at location (8,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$c1,$80,$f0 ; 8x16 tile #$c1 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$c3,$80,$f8 ; 8x16 tile #$c3 at location (-8,0) (vertical flip, palette 0)
    .byte $00,$c3,$c0,$00 ; 8x16 tile #$c3 at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$c1,$c0,$08 ; 8x16 tile #$c1 at location (8,0) (vertical flip, horizontal flip, palette 0)

; alien serpent head
sprite_89:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$d9,$00,$f0 ; 8x16 tile #$d9 at location (-16,-16)
    .byte $f0,$db,$00,$f8 ; 8x16 tile #$db at location (-8,-16)
    .byte $f0,$db,$40,$00 ; 8x16 tile #$db at location (0,-16) (horizontal flip, palette 0)
    .byte $f0,$d9,$40,$08 ; 8x16 tile #$d9 at location (8,-16) (horizontal flip, palette 0)
    .byte $00,$dd,$00,$f0 ; 8x16 tile #$dd at location (-16,0)
    .byte $00,$df,$00,$f8 ; 8x16 tile #$df at location (-8,0)
    .byte $00,$df,$40,$00 ; 8x16 tile #$df at location (0,0) (horizontal flip, palette 0)
    .byte $00,$dd,$40,$08 ; 8x16 tile #$dd at location (8,0) (horizontal flip, palette 0)

; alien serpent head
sprite_8a:
    .byte $20             ; number of bytes in meta-sprite
    .byte $ee,$e1,$00,$f2 ; 8x16 tile #$e1 at location (-14,-18)
    .byte $ee,$e3,$00,$fa ; 8x16 tile #$e3 at location (-6,-18)
    .byte $ee,$e5,$00,$02 ; 8x16 tile #$e5 at location (2,-18)
    .byte $ee,$e7,$00,$0a ; 8x16 tile #$e7 at location (10,-18)
    .byte $fe,$e9,$00,$f2 ; 8x16 tile #$e9 at location (-14,-2)
    .byte $fe,$eb,$00,$fa ; 8x16 tile #$eb at location (-6,-2)
    .byte $fe,$ed,$00,$02 ; 8x16 tile #$ed at location (2,-2)
    .byte $fe,$ef,$00,$0a ; 8x16 tile #$ef at location (10,-2)

; alien serpent head
sprite_8b:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$f1,$00,$f0 ; 8x16 tile #$f1 at location (-16,-16)
    .byte $f0,$f3,$00,$f8 ; 8x16 tile #$f3 at location (-8,-16)
    .byte $f0,$f5,$00,$00 ; 8x16 tile #$f5 at location (0,-16)
    .byte $f0,$f7,$00,$08 ; 8x16 tile #$f7 at location (8,-16)
    .byte $00,$f1,$80,$f0 ; 8x16 tile #$f1 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$f3,$80,$f8 ; 8x16 tile #$f3 at location (-8,0) (vertical flip, palette 0)
    .byte $00,$f5,$80,$00 ; 8x16 tile #$f5 at location (0,0) (vertical flip, palette 0)
    .byte $00,$f7,$80,$08 ; 8x16 tile #$f7 at location (8,0) (vertical flip, palette 0)

; alien serpent head
sprite_8c:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f2,$e9,$80,$f2 ; 8x16 tile #$e9 at location (-14,-14) (vertical flip, palette 0)
    .byte $f2,$eb,$80,$fa ; 8x16 tile #$eb at location (-6,-14) (vertical flip, palette 0)
    .byte $f2,$ed,$80,$02 ; 8x16 tile #$ed at location (2,-14) (vertical flip, palette 0)
    .byte $f2,$ef,$80,$0a ; 8x16 tile #$ef at location (10,-14) (vertical flip, palette 0)
    .byte $02,$e1,$80,$f2 ; 8x16 tile #$e1 at location (-14,2) (vertical flip, palette 0)
    .byte $02,$e3,$80,$fa ; 8x16 tile #$e3 at location (-6,2) (vertical flip, palette 0)
    .byte $02,$e5,$80,$02 ; 8x16 tile #$e5 at location (2,2) (vertical flip, palette 0)
    .byte $02,$e7,$80,$0a ; 8x16 tile #$e7 at location (10,2) (vertical flip, palette 0)

; alien serpent head
sprite_8d:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$dd,$80,$f0 ; 8x16 tile #$dd at location (-16,-16) (vertical flip, palette 0)
    .byte $f0,$df,$80,$f8 ; 8x16 tile #$df at location (-8,-16) (vertical flip, palette 0)
    .byte $f0,$df,$c0,$00 ; 8x16 tile #$df at location (0,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f0,$dd,$c0,$08 ; 8x16 tile #$dd at location (8,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$d9,$80,$f0 ; 8x16 tile #$d9 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$db,$80,$f8 ; 8x16 tile #$db at location (-8,0) (vertical flip, palette 0)
    .byte $00,$db,$c0,$00 ; 8x16 tile #$db at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$d9,$c0,$08 ; 8x16 tile #$d9 at location (8,0) (vertical flip, horizontal flip, palette 0)

; alien serpent body
sprite_8e:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$f9,$00,$f4 ; 8x16 tile #$f9 at location (-12,-16)
    .byte $f0,$fb,$00,$fc ; 8x16 tile #$fb at location (-4,-16)
    .byte $f0,$fd,$00,$04 ; 8x16 tile #$fd at location (4,-16)
    .byte $00,$fd,$c0,$f4 ; 8x16 tile #$fd at location (-12,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$fb,$c0,$fc ; 8x16 tile #$fb at location (-4,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$f9,$c0,$04 ; 8x16 tile #$f9 at location (4,0) (vertical flip, horizontal flip, palette 0)

; jagger froid projectile
sprite_8f:
    .byte $eb ; 8x16 tile #$d6 from right pattern table (small sprite)

; manooki
sprite_90:
    .byte $38             ; number of bytes in meta-sprite
    .byte $e0,$c5,$00,$f0 ; 8x16 tile #$c5 at location (-16,-32)
    .byte $e0,$c7,$00,$f8 ; 8x16 tile #$c7 at location (-8,-32)
    .byte $e0,$c9,$00,$00 ; 8x16 tile #$c9 at location (0,-32)
    .byte $e0,$cb,$00,$08 ; 8x16 tile #$cb at location (8,-32)
    .byte $f0,$cd,$00,$f1 ; 8x16 tile #$cd at location (-15,-16)
    .byte $f0,$cf,$00,$f9 ; 8x16 tile #$cf at location (-7,-16)
    .byte $f0,$d1,$00,$01 ; 8x16 tile #$d1 at location (1,-16)
    .byte $f0,$d3,$00,$07 ; 8x16 tile #$d3 at location (7,-16)
    .byte $00,$d5,$00,$f0 ; 8x16 tile #$d5 at location (-16,0)
    .byte $00,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,0)
    .byte $00,$d9,$00,$00 ; 8x16 tile #$d9 at location (0,0)
    .byte $00,$db,$00,$08 ; 8x16 tile #$db at location (8,0)
    .byte $10,$dd,$00,$f2 ; 8x16 tile #$dd at location (-14,16)
    .byte $10,$df,$00,$fa ; 8x16 tile #$df at location (-6,16)

; manooki
sprite_91:
    .byte $38             ; number of bytes in meta-sprite
    .byte $e0,$e1,$00,$ef ; 8x16 tile #$e1 at location (-17,-32)
    .byte $e0,$e3,$00,$f7 ; 8x16 tile #$e3 at location (-9,-32)
    .byte $e0,$e9,$00,$ff ; 8x16 tile #$e9 at location (-1,-32)
    .byte $e0,$eb,$00,$07 ; 8x16 tile #$eb at location (7,-32)
    .byte $f0,$ed,$00,$f0 ; 8x16 tile #$ed at location (-16,-16)
    .byte $f0,$ef,$00,$f8 ; 8x16 tile #$ef at location (-8,-16)
    .byte $f0,$f1,$00,$00 ; 8x16 tile #$f1 at location (0,-16)
    .byte $f0,$f3,$00,$08 ; 8x16 tile #$f3 at location (8,-16)
    .byte $00,$d5,$00,$ef ; 8x16 tile #$d5 at location (-17,0)
    .byte $00,$d7,$00,$f7 ; 8x16 tile #$d7 at location (-9,0)
    .byte $00,$f5,$00,$ff ; 8x16 tile #$f5 at location (-1,0)
    .byte $00,$f7,$00,$07 ; 8x16 tile #$f7 at location (7,0)
    .byte $10,$f9,$00,$f2 ; 8x16 tile #$f9 at location (-14,16)
    .byte $10,$df,$00,$fa ; 8x16 tile #$df at location (-6,16)

; manooki projectile
sprite_92:
    .byte $fd ; 8x16 tile #$fa from right pattern table (small sprite)

; manooki projectile
sprite_93:
    .byte $fe ; 8x16 tile #$fc from right pattern table (small sprite)

; poison drop
sprite_94:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$dd,$00,$fc ; 8x16 tile #$dd at location (-4,-8)

; poison drop
sprite_95:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$df,$00,$fc ; 8x16 tile #$df at location (-4,-8)

; poison drop
sprite_96:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$e1,$00,$fc ; 8x16 tile #$e1 at location (-4,-8)

; poison drop
sprite_97:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$e3,$00,$fc ; 8x16 tile #$e3 at location (-4,-8)

; poison drop
sprite_98:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$e9,$00,$f8 ; 8x16 tile #$e9 at location (-8,-8)
    .byte $f8,$eb,$00,$00 ; 8x16 tile #$eb at location (0,-8)

; poison drop
sprite_99:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$ed,$00,$f8 ; 8x16 tile #$ed at location (-8,-8)
    .byte $f8,$ef,$00,$00 ; 8x16 tile #$ef at location (0,-8)

; poison drop
sprite_9a:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$f1,$00,$f8 ; 8x16 tile #$f1 at location (-8,-8)
    .byte $f8,$f3,$00,$00 ; 8x16 tile #$f3 at location (0,-8)

; poison drop
sprite_9b:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$f5,$00,$f8 ; 8x16 tile #$f5 at location (-8,-8)
    .byte $f8,$f7,$00,$00 ; 8x16 tile #$f7 at location (0,-8)

; poison drop
sprite_9c:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$f9,$00,$fc ; 8x16 tile #$f9 at location (-4,-8)

; poison drop
sprite_9d:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$fb,$00,$f8 ; 8x16 tile #$fb at location (-8,-8)
    .byte $f8,$fd,$00,$00 ; 8x16 tile #$fd at location (0,-8)

; alien spider
sprite_9e:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$cb,$00,$f5 ; 8x16 tile #$cb at location (-11,-8)
    .byte $f8,$cd,$00,$fd ; 8x16 tile #$cd at location (-3,-8)
    .byte $f0,$cf,$00,$04 ; 8x16 tile #$cf at location (4,-16)

; alien spider
sprite_9f:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$d1,$00,$f7 ; 8x16 tile #$d1 at location (-9,-8)
    .byte $f9,$d3,$00,$ff ; 8x16 tile #$d3 at location (-1,-7)
    .byte $f0,$d5,$00,$05 ; 8x16 tile #$d5 at location (5,-16)

; alien spider
sprite_a0:
    .byte $0c             ; number of bytes in meta-sprite
    .byte $f8,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,-8)
    .byte $f8,$d9,$00,$00 ; 8x16 tile #$d9 at location (0,-8)
    .byte $f4,$db,$00,$08 ; 8x16 tile #$db at location (8,-12)

; fire ring projectile
sprite_a1:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$b9,$00,$f0 ; 8x16 tile #$b9 at location (-16,-16)
    .byte $f0,$bb,$00,$f8 ; 8x16 tile #$bb at location (-8,-16)
    .byte $f0,$bd,$00,$00 ; 8x16 tile #$bd at location (0,-16)
    .byte $f0,$bf,$00,$08 ; 8x16 tile #$bf at location (8,-16)
    .byte $00,$c5,$00,$f0 ; 8x16 tile #$c5 at location (-16,0)
    .byte $00,$c7,$00,$f8 ; 8x16 tile #$c7 at location (-8,0)
    .byte $00,$bb,$c0,$00 ; 8x16 tile #$bb at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$c9,$00,$08 ; 8x16 tile #$c9 at location (8,0)

; fire ring projectile
sprite_a2:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$c5,$80,$f0 ; 8x16 tile #$c5 at location (-16,-16) (vertical flip, palette 0)
    .byte $f0,$c7,$80,$f8 ; 8x16 tile #$c7 at location (-8,-16) (vertical flip, palette 0)
    .byte $f0,$bb,$40,$00 ; 8x16 tile #$bb at location (0,-16) (horizontal flip, palette 0)
    .byte $f0,$c9,$80,$08 ; 8x16 tile #$c9 at location (8,-16) (vertical flip, palette 0)
    .byte $00,$b9,$80,$f0 ; 8x16 tile #$b9 at location (-16,0) (vertical flip, palette 0)
    .byte $00,$bb,$80,$f8 ; 8x16 tile #$bb at location (-8,0) (vertical flip, palette 0)
    .byte $00,$bd,$80,$00 ; 8x16 tile #$bd at location (0,0) (vertical flip, palette 0)
    .byte $00,$bf,$80,$08 ; 8x16 tile #$bf at location (8,0) (vertical flip, palette 0)

; fire ring projectile
sprite_a3:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$c9,$c0,$f0 ; 8x16 tile #$c9 at location (-16,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f0,$bb,$00,$f8 ; 8x16 tile #$bb at location (-8,-16)
    .byte $f0,$c7,$c0,$00 ; 8x16 tile #$c7 at location (0,-16) (vertical flip, horizontal flip, palette 0)
    .byte $f0,$c5,$c0,$08 ; 8x16 tile #$c5 at location (8,-16) (vertical flip, horizontal flip, palette 0)
    .byte $00,$bf,$c0,$f0 ; 8x16 tile #$bf at location (-16,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$bd,$c0,$f8 ; 8x16 tile #$bd at location (-8,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$bb,$c0,$00 ; 8x16 tile #$bb at location (0,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$b9,$c0,$08 ; 8x16 tile #$b9 at location (8,0) (vertical flip, horizontal flip, palette 0)

; fire ring projectile
sprite_a4:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f0,$bf,$40,$f0 ; 8x16 tile #$bf at location (-16,-16) (horizontal flip, palette 0)
    .byte $f0,$bd,$40,$f8 ; 8x16 tile #$bd at location (-8,-16) (horizontal flip, palette 0)
    .byte $f0,$bb,$40,$00 ; 8x16 tile #$bb at location (0,-16) (horizontal flip, palette 0)
    .byte $f0,$b9,$40,$08 ; 8x16 tile #$b9 at location (8,-16) (horizontal flip, palette 0)
    .byte $00,$c9,$40,$f0 ; 8x16 tile #$c9 at location (-16,0) (horizontal flip, palette 0)
    .byte $00,$bb,$80,$f8 ; 8x16 tile #$bb at location (-8,0) (vertical flip, palette 0)
    .byte $00,$c7,$40,$00 ; 8x16 tile #$c7 at location (0,0) (horizontal flip, palette 0)
    .byte $00,$c5,$40,$08 ; 8x16 tile #$c5 at location (8,0) (horizontal flip, palette 0)

; temple of terror red blob
sprite_a5:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$b5,$00,$f8 ; 8x16 tile #$b5 at location (-8,-8)
    .byte $f8,$b5,$40,$00 ; 8x16 tile #$b5 at location (0,-8) (horizontal flip, palette 0)

; temple of terror red blob
sprite_a6:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$b7,$00,$f8 ; 8x16 tile #$b7 at location (-8,-8)
    .byte $f8,$b7,$40,$00 ; 8x16 tile #$b7 at location (0,-8) (horizontal flip, palette 0)

; level 1 boss helicopter heart
sprite_a7:
    .byte $18             ; number of bytes in meta-sprite
    .byte $f0,$d7,$00,$f4 ; 8x16 tile #$d7 at location (-12,-16)
    .byte $f0,$d9,$00,$fc ; 8x16 tile #$d9 at location (-4,-16)
    .byte $f0,$db,$00,$04 ; 8x16 tile #$db at location (4,-16)
    .byte $00,$db,$c0,$f4 ; 8x16 tile #$db at location (-12,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$d9,$c0,$fc ; 8x16 tile #$d9 at location (-4,0) (vertical flip, horizontal flip, palette 0)
    .byte $00,$d7,$c0,$04 ; 8x16 tile #$d7 at location (4,0) (vertical flip, horizontal flip, palette 0)

; red bubble
sprite_a8:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$d5,$00,$f8 ; 8x16 tile #$d5 at location (-8,-8)
    .byte $f8,$d7,$00,$00 ; 8x16 tile #$d7 at location (0,-8)

; red bubble
sprite_a9:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$d9,$00,$f8 ; 8x16 tile #$d9 at location (-8,-8)
    .byte $f8,$db,$00,$00 ; 8x16 tile #$db at location (0,-8)

; blob
sprite_aa:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$dd,$00,$f8 ; 8x16 tile #$dd at location (-8,-8)
    .byte $f8,$dd,$40,$00 ; 8x16 tile #$dd at location (0,-8) (horizontal flip, palette 0)

; blob
sprite_ab:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$df,$00,$f8 ; 8x16 tile #$df at location (-8,-8)
    .byte $f8,$df,$40,$00 ; 8x16 tile #$df at location (0,-8) (horizontal flip, palette 0)

; crumbling rock
sprite_ac:
    .byte $f0 ; 8x16 tile #$e0 from right pattern table (small sprite)

; crumbling rock
sprite_ad:
    .byte $f1 ; 8x16 tile #$e2 from right pattern table (small sprite)

; crumbling rock
sprite_ae:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$e3,$80,$fc ; 8x16 tile #$e3 at location (-4,-8) (vertical flip, palette 0)

; crumbling rock
sprite_af:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$e1,$80,$fc ; 8x16 tile #$e1 at location (-4,-8) (vertical flip, palette 0)

; crumbling rock
sprite_b0:
    .byte $f4 ; 8x16 tile #$e8 from right pattern table (small sprite)

; crumbling rock
sprite_b1:
    .byte $f5 ; 8x16 tile #$ea from right pattern table (small sprite)

; crumbling rock
sprite_b2:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$eb,$80,$fc ; 8x16 tile #$eb at location (-4,-8) (vertical flip, palette 0)

; crumbling rock
sprite_b3:
    .byte $04             ; number of bytes in meta-sprite
    .byte $f8,$e9,$80,$fc ; 8x16 tile #$e9 at location (-4,-8) (vertical flip, palette 0)

; fortress wall turret
sprite_b4:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$d7,$00,$f8 ; 8x16 tile #$d7 at location (-8,-8)
    .byte $f8,$d7,$40,$00 ; 8x16 tile #$d7 at location (0,-8) (horizontal flip, palette 0)

; fortress wall turret
sprite_b5:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$d9,$00,$f8 ; 8x16 tile #$d9 at location (-8,-8)
    .byte $f8,$d9,$40,$00 ; 8x16 tile #$d9 at location (0,-8) (horizontal flip, palette 0)

; fortress wall turret
sprite_b6:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$db,$00,$f8 ; 8x16 tile #$db at location (-8,-8)
    .byte $f8,$db,$40,$00 ; 8x16 tile #$db at location (0,-8) (horizontal flip, palette 0)

; fortress wall turret
sprite_b7:
    .byte $08             ; number of bytes in meta-sprite
    .byte $f8,$dd,$00,$f8 ; 8x16 tile #$dd at location (-8,-8)
    .byte $f8,$dd,$40,$00 ; 8x16 tile #$dd at location (0,-8) (horizontal flip, palette 0)

; intro screen logo red c bottom
sprite_b8:
    .byte $2c             ; number of bytes in meta-sprite
    .byte $00,$cb,$00,$00 ; 8x16 tile #$cb at location (0,0)
    .byte $10,$cd,$00,$00 ; 8x16 tile #$cd at location (0,16)
    .byte $20,$cf,$00,$00 ; 8x16 tile #$cf at location (0,32)
    .byte $2e,$d1,$00,$08 ; 8x16 tile #$d1 at location (8,46)
    .byte $2e,$d3,$00,$10 ; 8x16 tile #$d3 at location (16,46)
    .byte $2e,$d5,$00,$18 ; 8x16 tile #$d5 at location (24,46)
    .byte $2e,$d7,$00,$20 ; 8x16 tile #$d7 at location (32,46)
    .byte $30,$db,$00,$28 ; 8x16 tile #$db at location (40,48)
    .byte $30,$dd,$00,$30 ; 8x16 tile #$dd at location (48,48)
    .byte $30,$df,$00,$38 ; 8x16 tile #$df at location (56,48)
    .byte $2e,$e1,$00,$40 ; 8x16 tile #$e1 at location (64,46)

; intro screen cursor
sprite_b9:
    .byte $04             ; number of bytes in meta-sprite
    .byte $00,$d9,$00,$00 ; 8x16 tile #$d9 at location (0,0)

; ending credits helicopter
sprite_ba:
    .byte $24             ; number of bytes in meta-sprite
    .byte $f5,$c1,$00,$e7 ; 8x16 tile #$c1 at location (-25,-11)
    .byte $f0,$c3,$00,$ef ; 8x16 tile #$c3 at location (-17,-16)
    .byte $f0,$c5,$00,$f7 ; 8x16 tile #$c5 at location (-9,-16)
    .byte $f0,$c7,$00,$ff ; 8x16 tile #$c7 at location (-1,-16)
    .byte $f0,$c9,$00,$07 ; 8x16 tile #$c9 at location (7,-16)
    .byte $00,$cb,$00,$ef ; 8x16 tile #$cb at location (-17,0)
    .byte $00,$cd,$00,$f7 ; 8x16 tile #$cd at location (-9,0)
    .byte $00,$cf,$00,$ff ; 8x16 tile #$cf at location (-1,0)
    .byte $00,$d1,$00,$07 ; 8x16 tile #$d1 at location (7,0)

; ending credits helicopter
sprite_bb:
    .byte $20             ; number of bytes in meta-sprite
    .byte $f8,$d3,$00,$e5 ; 8x16 tile #$d3 at location (-27,-8)
    .byte $f8,$d5,$00,$ed ; 8x16 tile #$d5 at location (-19,-8)
    .byte $f0,$d7,$00,$f5 ; 8x16 tile #$d7 at location (-11,-16)
    .byte $f0,$d9,$00,$fd ; 8x16 tile #$d9 at location (-3,-16)
    .byte $f0,$db,$00,$05 ; 8x16 tile #$db at location (5,-16)
    .byte $00,$dd,$00,$f5 ; 8x16 tile #$dd at location (-11,0)
    .byte $00,$df,$00,$fd ; 8x16 tile #$df at location (-3,0)
    .byte $00,$e1,$00,$05 ; 8x16 tile #$e1 at location (5,0)

sprite_bc:
    .byte $ff

; pointer table for sprites
player_sprite_ptr_tbl:
    .addr player_sprite_00
    .addr player_sprite_00
    .addr player_sprite_02
    .addr player_sprite_03
    .addr player_sprite_04
    .addr player_sprite_05
    .addr player_sprite_06
    .addr player_sprite_07
    .addr player_sprite_08
    .addr player_sprite_09
    .addr player_sprite_0a
    .addr player_sprite_0b
    .addr player_sprite_0c
    .addr player_sprite_0d
    .addr player_sprite_0e
    .addr player_sprite_0f
    .addr player_sprite_10
    .addr player_sprite_11
    .addr player_sprite_12
    .addr player_sprite_13
    .addr player_sprite_14
    .addr player_sprite_15
    .addr player_sprite_16
    .addr player_sprite_17
    .addr player_sprite_18
    .addr player_sprite_19
    .addr player_sprite_1a
    .addr player_sprite_1b
    .addr player_sprite_1c
    .addr player_sprite_1d
    .addr player_sprite_1e
    .addr player_sprite_1f
    .addr player_sprite_20
    .addr player_sprite_21
    .addr player_sprite_22
    .addr player_sprite_23
    .addr player_sprite_24
    .addr player_sprite_25
    .addr player_sprite_26
    .addr player_sprite_26
    .addr player_sprite_28
    .addr player_sprite_28
    .addr player_sprite_2a
    .addr player_sprite_2b
    .addr player_sprite_2c
    .addr player_sprite_2d
    .addr player_sprite_2e
    .addr player_sprite_2f
    .addr player_sprite_30
    .addr player_sprite_31
    .addr player_sprite_32
    .addr player_sprite_33
    .addr player_sprite_34
    .addr player_sprite_35
    .addr player_sprite_36
    .addr player_sprite_37
    .addr player_sprite_38
    .addr player_sprite_39
    .addr player_sprite_3a
    .addr player_sprite_3b
    .addr player_sprite_3c
    .addr player_sprite_3d
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e
    .addr player_sprite_3e

player_sprite_00:
    .byte $05             ; number of sprites in meta-sprite
    .byte $f0,$03,$01,$fa ; 8x16 tile #$03 at location (-6,-16) (palette 1)
    .byte $ef,$05,$01,$02 ; 8x16 tile #$05 at location (2,-17) (palette 1)

player_running_bottom:
    .byte $ff,$0f,$00,$f6 ; 8x16 tile #$0f at location (-10,-1)
    .byte $00,$11,$00,$fe ; 8x16 tile #$11 at location (-2,0)
    .byte $0f,$13,$00,$f4 ; 8x16 tile #$13 at location (-12,15)

player_sprite_02:
    .byte $05             ; number of sprites in meta-sprite
    .byte $f1,$07,$01,$fc ; 8x16 tile #$07 at location (-4,-15) (palette 1)
    .byte $f0,$09,$01,$04 ; 8x16 tile #$09 at location (4,-16) (palette 1)

player_large_run_bottom:
    .byte $00,$15,$00,$f6 ; 8x16 tile #$15 at location (-10,0)
    .byte $01,$17,$00,$fe ; 8x16 tile #$17 at location (-2,1)
    .byte $11,$13,$00,$01 ; 8x16 tile #$13 at location (1,17)

player_sprite_03:
    .byte $05             ; number of sprites in meta-sprite
    .byte $ef,$0b,$01,$f8 ; 8x16 tile #$0b at location (-8,-17) (palette 1)
    .byte $f0,$0d,$01,$00 ; 8x16 tile #$0d at location (0,-16) (palette 1)

player_standing_bottom:
    .byte $ff,$19,$00,$f9 ; 8x16 tile #$19 at location (-7,-1)
    .byte $00,$1b,$00,$01 ; 8x16 tile #$1b at location (1,0)
    .byte $0f,$13,$00,$fb ; 8x16 tile #$13 at location (-5,15)

player_sprite_04:
    .byte $05                   ; number of sprites in meta-sprite
    .byte $ef,$0b,$01,$f8       ; 8x16 tile #$0b at location (-8,-17) (palette 1)
    .byte $f0,$0d,$01,$00       ; 8x16 tile #$0d at location (0,-16) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_05:
    .byte $05                    ; number of sprites in meta-sprite
    .byte $f0,$03,$01,$f9        ; 8x16 tile #$03 at location (-7,-16) (palette 1)
    .byte $ef,$05,$01,$01        ; 8x16 tile #$05 at location (1,-17) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_06:
    .byte $06                   ; number of sprites in meta-sprite
    .byte $f0,$1d,$01,$f7       ; 8x16 tile #$1d at location (-9,-16) (palette 1)
    .byte $f0,$1f,$01,$ff       ; 8x16 tile #$1f at location (-1,-16) (palette 1)
    .byte $ef,$21,$01,$07       ; 8x16 tile #$21 at location (7,-17) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_07:
    .byte $06                     ; number of sprites in meta-sprite
    .byte $f1,$1d,$01,$f7         ; 8x16 tile #$1d at location (-9,-15) (palette 1)
    .byte $f1,$1f,$01,$ff         ; 8x16 tile #$1f at location (-1,-15) (palette 1)
    .byte $f0,$21,$01,$07         ; 8x16 tile #$21 at location (7,-16) (palette 1)
    .byte $80                     ; shared sprite byte
    .addr player_large_run_bottom ; continue meta-sprite at player_large_run_bottom

player_sprite_08:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $f0,$1d,$01,$f7        ; 8x16 tile #$1d at location (-9,-16) (palette 1)
    .byte $f0,$1f,$01,$ff        ; 8x16 tile #$1f at location (-1,-16) (palette 1)
    .byte $ef,$21,$01,$07        ; 8x16 tile #$21 at location (7,-17) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_09:
    .byte $06                   ; number of sprites in meta-sprite
    .byte $f0,$1d,$01,$f7       ; 8x16 tile #$1d at location (-9,-16) (palette 1)
    .byte $f0,$1f,$01,$ff       ; 8x16 tile #$1f at location (-1,-16) (palette 1)
    .byte $ef,$21,$01,$07       ; 8x16 tile #$21 at location (7,-17) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_0a:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $f0,$1d,$01,$f7        ; 8x16 tile #$1d at location (-9,-16) (palette 1)
    .byte $f0,$1f,$01,$ff        ; 8x16 tile #$1f at location (-1,-16) (palette 1)
    .byte $ef,$21,$01,$07        ; 8x16 tile #$21 at location (7,-17) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_0b:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $f1,$1d,$01,$f7        ; 8x16 tile #$1d at location (-9,-15) (palette 1)
    .byte $f1,$1f,$01,$ff        ; 8x16 tile #$1f at location (-1,-15) (palette 1)
    .byte $f0,$21,$01,$07        ; 8x16 tile #$21 at location (7,-16) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_0c:
    .byte $05                   ; number of sprites in meta-sprite
    .byte $f0,$2b,$01,$fa       ; 8x16 tile #$2b at location (-6,-16) (palette 1)
    .byte $ef,$2d,$01,$02       ; 8x16 tile #$2d at location (2,-17) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_0d:
    .byte $05                     ; number of sprites in meta-sprite
    .byte $f1,$2b,$01,$fa         ; 8x16 tile #$2b at location (-6,-15) (palette 1)
    .byte $f0,$2d,$01,$02         ; 8x16 tile #$2d at location (2,-16) (palette 1)
    .byte $80                     ; shared sprite byte
    .addr player_large_run_bottom ; continue meta-sprite at player_large_run_bottom

player_sprite_0e:
    .byte $05                    ; number of sprites in meta-sprite
    .byte $f0,$2b,$01,$fa        ; 8x16 tile #$2b at location (-6,-16) (palette 1)
    .byte $ef,$2d,$01,$02        ; 8x16 tile #$2d at location (2,-17) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_0f:
    .byte $05                   ; number of sprites in meta-sprite
    .byte $f0,$2b,$01,$fa       ; 8x16 tile #$2b at location (-6,-16) (palette 1)
    .byte $ef,$2d,$01,$02       ; 8x16 tile #$2d at location (2,-17) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_10:
    .byte $05                    ; number of sprites in meta-sprite
    .byte $f0,$2b,$01,$fa        ; 8x16 tile #$2b at location (-6,-16) (palette 1)
    .byte $ef,$2d,$01,$02        ; 8x16 tile #$2d at location (2,-17) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_11:
    .byte $06                   ; number of sprites in meta-sprite
    .byte $ef,$2f,$01,$f6       ; 8x16 tile #$2f at location (-10,-17) (palette 1)
    .byte $f0,$31,$01,$fe       ; 8x16 tile #$31 at location (-2,-16) (palette 1)
    .byte $f8,$33,$01,$03       ; 8x16 tile #$33 at location (3,-8) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_12:
    .byte $06                     ; number of sprites in meta-sprite
    .byte $f0,$2f,$01,$f6         ; 8x16 tile #$2f at location (-10,-16) (palette 1)
    .byte $f1,$31,$01,$fe         ; 8x16 tile #$31 at location (-2,-15) (palette 1)
    .byte $f9,$33,$01,$03         ; 8x16 tile #$33 at location (3,-7) (palette 1)
    .byte $80                     ; shared sprite byte
    .addr player_large_run_bottom ; continue meta-sprite at player_large_run_bottom

player_sprite_13:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $ef,$2f,$01,$f6        ; 8x16 tile #$2f at location (-10,-17) (palette 1)
    .byte $f0,$31,$01,$fe        ; 8x16 tile #$31 at location (-2,-16) (palette 1)
    .byte $f8,$33,$01,$03        ; 8x16 tile #$33 at location (3,-8) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_14:
    .byte $06                   ; number of sprites in meta-sprite
    .byte $ef,$2f,$01,$f6       ; 8x16 tile #$2f at location (-10,-17) (palette 1)
    .byte $f0,$31,$01,$fe       ; 8x16 tile #$31 at location (-2,-16) (palette 1)
    .byte $f8,$33,$01,$03       ; 8x16 tile #$33 at location (3,-8) (palette 1)
    .byte $80                   ; shared sprite byte
    .addr player_running_bottom ; continue meta-sprite at player_running_bottom

player_sprite_15:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $ef,$2f,$01,$f6        ; 8x16 tile #$2f at location (-10,-17) (palette 1)
    .byte $f0,$31,$01,$fe        ; 8x16 tile #$31 at location (-2,-16) (palette 1)
    .byte $f8,$33,$01,$03        ; 8x16 tile #$33 at location (3,-8) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_16:
    .byte $04             ; number of sprites in meta-sprite
    .byte $02,$23,$00,$f0 ; 8x16 tile #$23 at location (-16,2)
    .byte $02,$25,$00,$f8 ; 8x16 tile #$25 at location (-8,2)
    .byte $02,$27,$01,$00 ; 8x16 tile #$27 at location (0,2) (palette 1)
    .byte $02,$29,$01,$08 ; 8x16 tile #$29 at location (8,2) (palette 1)

player_sprite_17:
    .byte $04             ; number of sprites in meta-sprite
    .byte $03,$23,$00,$f0 ; 8x16 tile #$23 at location (-16,3)
    .byte $03,$25,$00,$f8 ; 8x16 tile #$25 at location (-8,3)
    .byte $03,$27,$01,$00 ; 8x16 tile #$27 at location (0,3) (palette 1)
    .byte $03,$29,$01,$08 ; 8x16 tile #$29 at location (8,3) (palette 1)

player_sprite_18:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $e0,$35,$01,$ff        ; 8x16 tile #$35 at location (-1,-32) (palette 1)
    .byte $ef,$37,$01,$f6        ; 8x16 tile #$37 at location (-10,-17) (palette 1)
    .byte $f0,$39,$01,$fe        ; 8x16 tile #$39 at location (-2,-16) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_19:
    .byte $06                    ; number of sprites in meta-sprite
    .byte $e1,$35,$01,$ff        ; 8x16 tile #$35 at location (-1,-31) (palette 1)
    .byte $f0,$37,$01,$f6        ; 8x16 tile #$37 at location (-10,-16) (palette 1)
    .byte $f1,$39,$01,$fe        ; 8x16 tile #$39 at location (-2,-15) (palette 1)
    .byte $80                    ; shared sprite byte
    .addr player_standing_bottom ; continue meta-sprite at player_standing_bottom

player_sprite_1a:
    .byte $04             ; number of sprites in meta-sprite
    .byte $f1,$3b,$01,$f8 ; 8x16 tile #$3b at location (-8,-15) (palette 1)
    .byte $f1,$3d,$01,$00 ; 8x16 tile #$3d at location (0,-15) (palette 1)
    .byte $01,$3f,$00,$f8 ; 8x16 tile #$3f at location (-8,1)
    .byte $01,$41,$00,$00 ; 8x16 tile #$41 at location (0,1)

player_sprite_1b:
    .byte $02             ; number of sprites in meta-sprite
    .byte $f8,$43,$00,$f7 ; 8x16 tile #$43 at location (-9,-8)
    .byte $f8,$45,$01,$ff ; 8x16 tile #$45 at location (-1,-8) (palette 1)

player_sprite_1c:
    .byte $04             ; number of sprites in meta-sprite
    .byte $ef,$41,$c0,$f8 ; 8x16 tile #$41 at location (-8,-17) (vertical flip, horizontal flip, palette 0)
    .byte $ef,$3f,$c0,$00 ; 8x16 tile #$3f at location (0,-17) (vertical flip, horizontal flip, palette 0)
    .byte $ff,$3d,$c1,$f8 ; 8x16 tile #$3d at location (-8,-1) (vertical flip, horizontal flip, palette 1)
    .byte $ff,$3b,$c1,$00 ; 8x16 tile #$3b at location (0,-1) (vertical flip, horizontal flip, palette 1)

player_sprite_1d:
    .byte $02             ; number of sprites in meta-sprite
    .byte $f8,$45,$c1,$f9 ; 8x16 tile #$45 at location (-7,-8) (vertical flip, horizontal flip, palette 1)
    .byte $f8,$43,$c0,$01 ; 8x16 tile #$43 at location (1,-8) (vertical flip, horizontal flip, palette 0)

player_sprite_1e:
    .byte $04             ; number of sprites in meta-sprite
    .byte $f2,$47,$01,$f6 ; 8x16 tile #$47 at location (-10,-14) (palette 1)
    .byte $f2,$49,$01,$fe ; 8x16 tile #$49 at location (-2,-14) (palette 1)
    .byte $02,$4b,$00,$f8 ; 8x16 tile #$4b at location (-8,2)
    .byte $02,$4d,$00,$00 ; 8x16 tile #$4d at location (0,2)

player_sprite_1f:
    .byte $03             ; number of sprites in meta-sprite
    .byte $fb,$4f,$01,$f5 ; 8x16 tile #$4f at location (-11,-5) (palette 1)
    .byte $fb,$51,$00,$fd ; 8x16 tile #$51 at location (-3,-5)
    .byte $f9,$53,$00,$05 ; 8x16 tile #$53 at location (5,-7)

player_sprite_20:
    .byte $04             ; number of sprites in meta-sprite
    .byte $f4,$4d,$c0,$f8 ; 8x16 tile #$4d at location (-8,-12) (vertical flip, horizontal flip, palette 0)
    .byte $f4,$4b,$c0,$00 ; 8x16 tile #$4b at location (0,-12) (vertical flip, horizontal flip, palette 0)
    .byte $04,$49,$c1,$fa ; 8x16 tile #$49 at location (-6,4) (vertical flip, horizontal flip, palette 1)
    .byte $04,$47,$c1,$02 ; 8x16 tile #$47 at location (2,4) (vertical flip, horizontal flip, palette 1)

player_sprite_21:
    .byte $03             ; number of sprites in meta-sprite
    .byte $fd,$53,$c0,$f3 ; 8x16 tile #$53 at location (-13,-3) (vertical flip, horizontal flip, palette 0)
    .byte $fb,$51,$c0,$fb ; 8x16 tile #$51 at location (-5,-5) (vertical flip, horizontal flip, palette 0)
    .byte $fb,$4f,$c1,$03 ; 8x16 tile #$4f at location (3,-5) (vertical flip, horizontal flip, palette 1)

player_sprite_22:
    .byte $04             ; number of sprites in meta-sprite
    .byte $03,$a5,$01,$f0 ; 8x16 tile #$a5 at location (-16,3) (palette 1)
    .byte $03,$a7,$01,$f8 ; 8x16 tile #$a7 at location (-8,3) (palette 1)
    .byte $03,$5b,$00,$00 ; 8x16 tile #$5b at location (0,3)
    .byte $03,$6f,$00,$08 ; 8x16 tile #$6f at location (8,3)

player_sprite_23:
    .byte $05             ; number of sprites in meta-sprite
    .byte $f8,$1d,$01,$f7 ; 8x16 tile #$1d at location (-9,-8) (palette 1)
    .byte $f8,$1f,$01,$ff ; 8x16 tile #$1f at location (-1,-8) (palette 1)
    .byte $f7,$21,$01,$07 ; 8x16 tile #$21 at location (7,-9) (palette 1)
    .byte $08,$e5,$00,$f7 ; 8x16 tile #$e5 at location (-9,8)
    .byte $07,$e7,$00,$ff ; 8x16 tile #$e7 at location (-1,7)

player_sprite_24:
    .byte $05             ; number of sprites in meta-sprite
    .byte $f9,$1d,$01,$f7 ; 8x16 tile #$1d at location (-9,-7) (palette 1)
    .byte $f9,$1f,$01,$ff ; 8x16 tile #$1f at location (-1,-7) (palette 1)
    .byte $f8,$21,$01,$07 ; 8x16 tile #$21 at location (7,-8) (palette 1)
    .byte $08,$e5,$00,$f7 ; 8x16 tile #$e5 at location (-9,8)
    .byte $07,$e7,$00,$ff ; 8x16 tile #$e7 at location (-1,7)

player_sprite_25:
    .byte $04             ; number of sprites in meta-sprite
    .byte $f0,$07,$01,$fc ; 8x16 tile #$07 at location (-4,-16) (palette 1)
    .byte $ef,$09,$01,$04 ; 8x16 tile #$09 at location (4,-17) (palette 1)

player_water_splash:
    .byte $00,$e5,$01,$f8 ; 8x16 tile #$e5 at location (-8,0) (palette 1)
    .byte $ff,$e7,$01,$00 ; 8x16 tile #$e7 at location (0,-1) (palette 1)

; same as player_water_splash above
player_sprite_26:
    .byte $02             ; number of sprites in meta-sprite
    .byte $00,$e5,$01,$f8 ; 8x16 tile #$e5 at location (-8,0) (palette 1)
    .byte $ff,$e7,$01,$00 ; 8x16 tile #$e7 at location (0,-1) (palette 1)

player_sprite_28:
    .byte $05                 ; number of sprites in meta-sprite
    .byte $f0,$1d,$01,$f7     ; 8x16 tile #$1d at location (-9,-16) (palette 1)
    .byte $f0,$1f,$01,$ff     ; 8x16 tile #$1f at location (-1,-16) (palette 1)
    .byte $ef,$21,$01,$07     ; 8x16 tile #$21 at location (7,-17) (palette 1)
    .byte $80                 ; shared sprite byte
    .addr player_water_splash ; continue meta-sprite at player_water_splash

player_sprite_2a:
    .byte $04                 ; number of sprites in meta-sprite
    .byte $f0,$2b,$01,$fa     ; 8x16 tile #$2b at location (-6,-16) (palette 1)
    .byte $ef,$2d,$01,$02     ; 8x16 tile #$2d at location (2,-17) (palette 1)
    .byte $80                 ; shared sprite byte
    .addr player_water_splash ; continue meta-sprite at player_water_splash

player_sprite_2b:
    .byte $05                 ; number of sprites in meta-sprite
    .byte $e0,$35,$01,$ff     ; 8x16 tile #$35 at location (-1,-32) (palette 1)
    .byte $ef,$37,$01,$f6     ; 8x16 tile #$37 at location (-10,-17) (palette 1)
    .byte $f0,$39,$01,$fe     ; 8x16 tile #$39 at location (-2,-16) (palette 1)
    .byte $80                 ; shared sprite byte
    .addr player_water_splash ; continue meta-sprite at player_water_splash

player_sprite_2c:
    .byte $04             ; number of sprites in meta-sprite
    .byte $03,$07,$00,$f7 ; 8x16 tile #$07 at location (-9,3)
    .byte $00,$09,$00,$ff ; 8x16 tile #$09 at location (-1,0)

player_overhead_top:
    .byte $f3,$03,$01,$f9 ; 8x16 tile #$03 at location (-7,-13) (palette 1)
    .byte $f0,$05,$01,$ff ; 8x16 tile #$05 at location (-1,-16) (palette 1)

player_sprite_2d:
    .byte $04             ; number of sprites in meta-sprite
    .byte $f4,$03,$01,$f9 ; 8x16 tile #$03 at location (-7,-12) (palette 1)
    .byte $f1,$05,$01,$ff ; 8x16 tile #$05 at location (-1,-15) (palette 1)
    .byte $04,$0b,$00,$f7 ; 8x16 tile #$0b at location (-9,4)
    .byte $01,$0d,$00,$ff ; 8x16 tile #$0d at location (-1,1)

player_sprite_2e:
    .byte $04                 ; number of sprites in meta-sprite
    .byte $03,$0f,$00,$f7     ; 8x16 tile #$0f at location (-9,3)
    .byte $00,$11,$00,$ff     ; 8x16 tile #$11 at location (-1,0)
    .byte $80                 ; shared sprite byte
    .addr player_overhead_top ; continue meta-sprite at player_overhead_top

player_sprite_2f:
    .byte $04             ; number of sprites in meta-sprite
    .byte $02,$17,$00,$f7 ; 8x16 tile #$17 at location (-9,2)
    .byte $01,$19,$00,$ff ; 8x16 tile #$19 at location (-1,1)

overhead_down_angle_top:
    .byte $f2,$13,$01,$f7 ; 8x16 tile #$13 at location (-9,-14) (palette 1)
    .byte $f1,$15,$01,$ff ; 8x16 tile #$15 at location (-1,-15) (palette 1)

player_sprite_30:
    .byte $03             ; number of sprites in meta-sprite
    .byte $02,$1b,$00,$fd ; 8x16 tile #$1b at location (-3,2)
    .byte $f3,$13,$01,$f7 ; 8x16 tile #$13 at location (-9,-13) (palette 1)
    .byte $f2,$15,$01,$ff ; 8x16 tile #$15 at location (-1,-14) (palette 1)

player_sprite_31:
    .byte $04                     ; number of sprites in meta-sprite
    .byte $02,$1d,$00,$f7         ; 8x16 tile #$1d at location (-9,2)
    .byte $01,$1f,$00,$ff         ; 8x16 tile #$1f at location (-1,1)
    .byte $80                     ; shared sprite byte
    .addr overhead_down_angle_top ; continue meta-sprite at overhead_down_angle_top

player_sprite_32:
    .byte $04             ; number of sprites in meta-sprite
    .byte $ff,$35,$00,$f8 ; 8x16 tile #$35 at location (-8,-1)
    .byte $ff,$37,$00,$00 ; 8x16 tile #$37 at location (0,-1)

overhead_up_angle_top:
    .byte $ef,$31,$01,$f8 ; 8x16 tile #$31 at location (-8,-17) (palette 1)
    .byte $ef,$33,$01,$00 ; 8x16 tile #$33 at location (0,-17) (palette 1)

player_sprite_33:
    .byte $03             ; number of sprites in meta-sprite
    .byte $00,$39,$00,$fd ; 8x16 tile #$39 at location (-3,0)
    .byte $f0,$31,$01,$f8 ; 8x16 tile #$31 at location (-8,-16) (palette 1)
    .byte $f0,$33,$01,$00 ; 8x16 tile #$33 at location (0,-16) (palette 1)

player_sprite_34:
    .byte $03                   ; number of sprites in meta-sprite
    .byte $ff,$3b,$00,$fe       ; 8x16 tile #$3b at location (-2,-1)
    .byte $80                   ; shared sprite byte
    .addr overhead_up_angle_top ; continue meta-sprite at overhead_up_angle_top

player_sprite_35:
    .byte $04             ; number of sprites in meta-sprite
    .byte $ff,$25,$00,$f9 ; 8x16 tile #$25 at location (-7,-1)
    .byte $ff,$27,$00,$01 ; 8x16 tile #$27 at location (1,-1)

overhead_x_top:
    .byte $ef,$21,$01,$f7 ; 8x16 tile #$21 at location (-9,-17) (palette 1)
    .byte $ef,$23,$01,$ff ; 8x16 tile #$23 at location (-1,-17) (palette 1)

player_sprite_36:
    .byte $03             ; number of sprites in meta-sprite
    .byte $00,$29,$00,$fd ; 8x16 tile #$29 at location (-3,0)
    .byte $f0,$21,$01,$f7 ; 8x16 tile #$21 at location (-9,-16) (palette 1)
    .byte $f0,$23,$01,$ff ; 8x16 tile #$23 at location (-1,-16) (palette 1)

player_sprite_37:
    .byte $04             ; number of sprites in meta-sprite
    .byte $ff,$2b,$00,$fa ; 8x16 tile #$2b at location (-6,-1)
    .byte $ff,$2d,$00,$02 ; 8x16 tile #$2d at location (2,-1)
    .byte $80             ; shared sprite byte
    .addr overhead_x_top  ; continue meta-sprite at overhead_x_top

player_sprite_38:
    .byte $03             ; number of sprites in meta-sprite
    .byte $00,$3d,$00,$fc ; 8x16 tile #$3d at location (-4,0)

overhead_y_top:
    .byte $f0,$3f,$01,$f9 ; 8x16 tile #$3f at location (-7,-16) (palette 1)
    .byte $f0,$41,$01,$01 ; 8x16 tile #$41 at location (1,-16) (palette 1)

player_sprite_39:
    .byte $03             ; number of sprites in meta-sprite
    .byte $01,$43,$00,$fc ; 8x16 tile #$43 at location (-4,1)
    .byte $f1,$3f,$01,$f9 ; 8x16 tile #$3f at location (-7,-15) (palette 1)
    .byte $f1,$41,$01,$01 ; 8x16 tile #$41 at location (1,-15) (palette 1)

player_sprite_3a:
    .byte $03             ; number of sprites in meta-sprite
    .byte $00,$3d,$40,$fc ; 8x16 tile #$3d at location (-4,0) (horizontal flip, palette 0)
    .byte $80             ; shared sprite byte
    .addr overhead_y_top  ; continue meta-sprite at overhead_y_top

player_sprite_3b:
    .byte $04             ; number of sprites in meta-sprite
    .byte $f0,$4b,$01,$f6 ; 8x16 tile #$4b at location (-10,-16) (palette 1)
    .byte $f0,$4d,$01,$fe ; 8x16 tile #$4d at location (-2,-16) (palette 1)
    .byte $f3,$4f,$01,$06 ; 8x16 tile #$4f at location (6,-13) (palette 1)
    .byte $00,$51,$00,$fc ; 8x16 tile #$51 at location (-4,0)

player_sprite_3c:
    .byte $04             ; number of sprites in meta-sprite
    .byte $fb,$53,$01,$f4 ; 8x16 tile #$53 at location (-12,-5) (palette 1)
    .byte $f9,$5b,$01,$fa ; 8x16 tile #$5b at location (-6,-7) (palette 1)
    .byte $f2,$6f,$00,$fe ; 8x16 tile #$6f at location (-2,-14)
    .byte $02,$a5,$01,$02 ; 8x16 tile #$a5 at location (2,2) (palette 1)

player_sprite_3d:
    .byte $05             ; number of sprites in meta-sprite
    .byte $ef,$a7,$00,$fa ; 8x16 tile #$a7 at location (-6,-17)
    .byte $f3,$45,$00,$02 ; 8x16 tile #$45 at location (2,-13)
    .byte $00,$2f,$01,$f2 ; 8x16 tile #$2f at location (-14,0) (palette 1)
    .byte $ff,$47,$01,$fa ; 8x16 tile #$47 at location (-6,-1) (palette 1)
    .byte $03,$49,$01,$02 ; 8x16 tile #$49 at location (2,3) (palette 1)

; shared unused sprite
player_sprite_3e:
    .byte $80,$01,$00

level_1_run_tile_routines:
    lda PAUSE_STATE ; (0 = not paused, 1 = paused)
    beq @continue   ; continue if not paused
    rts             ; exit if paused

@continue:
    jsr level_1_check_run_y_tile_routine   ; run tile routine for vertical checkpoint
    lda SCREEN_SCROLL_TYPE                 ; 0 = horizontal, 1 = vertical/overhead
    bne @exit
    lda X_TILE_ROUTINE
    asl
    tay
    lda level_1_x_tile_routine_pts_tbl,y
    cmp X_SCREEN                           ; compare to amount of horizontal nametable screens scrolled
    bcc @run_x_routine                     ; branch if not on screen yet
    bne @exit                              ; exit if past screen
    lda level_1_x_tile_routine_pts_tbl+1,y ; on target screen, see if at correct scroll
    cmp X_SCROLL                           ; compare to PPU vertical scroll
    beq @run_x_routine
    bcs @exit

@run_x_routine:
    jsr level_1_run_x_tile_routine
    clc

@exit:
    rts

level_1_x_tile_routine_pts_tbl:
    .byte $03,$00 ; X_TILE_ROUTINE #$00, horizontal screen #$03, horizontal scroll #00
    .byte $03,$80 ; X_TILE_ROUTINE #$01, horizontal screen #$03, horizontal scroll #80
    .byte $03,$c0 ; X_TILE_ROUTINE #$02, horizontal screen #$03, horizontal scroll #c0
    .byte $05,$70 ; X_TILE_ROUTINE #$03, horizontal screen #$05, horizontal scroll #70
    .byte $05,$b0 ; X_TILE_ROUTINE #$04, horizontal screen #$05, horizontal scroll #b0
    .byte $07,$80 ; X_TILE_ROUTINE #$05, horizontal screen #$07, horizontal scroll #80
    .byte $07,$c0 ; X_TILE_ROUTINE #$06, horizontal screen #$07, horizontal scroll #c0
    .byte $0a,$70 ; X_TILE_ROUTINE #$07, horizontal screen #$0a, horizontal scroll #70
    .byte $0a,$b0 ; X_TILE_ROUTINE #$08, horizontal screen #$0a, horizontal scroll #b0
    .byte $0c,$00 ; X_TILE_ROUTINE #$09, horizontal screen #$0c, horizontal scroll #00
    .byte $0c,$02 ; X_TILE_ROUTINE #$0a, horizontal screen #$0c, horizontal scroll #02
    .byte $0c,$fe ; X_TILE_ROUTINE #$0b, horizontal screen #$0c, horizontal scroll #fe
    .byte $ff

level_1_run_x_tile_routine:
    lda X_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_1_x_tile_routine_ptr_tbl:
    .addr level_1_x_tile_routine_00 ; set LEFT_BOTTOM_CHR_HALF_BANK to #$3a
    .addr level_1_x_tile_routine_01 ; (almost at top of 1st hill) set auto-scroll up to vertical screen 2 at 56.25% of screen
    .addr level_1_x_tile_routine_02 ; (passed 1st hill apex) set auto-scroll up to vertical screen 2 at 37.5% of screen
    .addr level_1_x_tile_routine_03 ; (almost at top of 2nd hill) set auto-scroll up to vertical screen 2 at 12.5% of screen
    .addr level_1_x_tile_routine_04 ; (passed 2nd hill apex) set auto-scroll up to vertical screen 1 at 87.5% of screen
    .addr level_1_x_tile_routine_05 ; (almost at top of 3rd hill) set auto-scroll up to vertical screen 1 at 56.25% of screen
    .addr level_1_x_tile_routine_06 ; (passed 3rd hill apex) set auto-scroll up to vertical screen 1 at 37.5% of screen
    .addr level_1_x_tile_routine_07 ; (almost at top of 4th hill) set auto-scroll up to vertical screen 1 at 12.5% of screen
    .addr level_1_x_tile_routine_08 ; (passed 4th hill apex) set auto-scroll up to vertical screen 0 at 87.5% of screen
    .addr level_1_x_tile_routine_09
    .addr level_1_x_tile_routine_0a ; set background palettes 2 and 3
    .addr level_1_x_tile_routine_0b
    .addr level_1_x_tile_routine_0c

; set LEFT_BOTTOM_CHR_HALF_BANK to #$3a
level_1_x_tile_routine_00:
    lda #$3a
    sta LEFT_BOTTOM_CHR_HALF_BANK  ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    bne level_1_adv_x_tile_routine ; always branch to exit

; set auto-scroll up to vertical screen 2 at 56.25% of screen
level_1_x_tile_routine_01:
    ldy #$02                  ; vertical screen 2
    bne level_1_set_y_stop_56 ; always branch

; set auto-scroll up to vertical screen 1 at 56.25% of screen
level_1_x_tile_routine_05:
    ldy #$01 ; vertical screen 1

level_1_set_y_stop_56:
    lda #$90                           ; stop auto-scrolling up at 56.25% vertically
    bne level_1_set_y_scroll_stop_exit ; always branch

; set auto-scroll up to vertical screen 2 at 37.5% of screen
level_1_x_tile_routine_02:
    ldy #$02                  ; vertical screen 2
    bne level_1_set_y_stop_37 ; always branch

; set auto-scroll up to vertical screen 1 at 37.5% of screen
level_1_x_tile_routine_06:
    ldy #$01 ; vertical screen 1

level_1_set_y_stop_37:
    lda #$60                           ; stop auto scrolling up at 37.5% vertically
    bne level_1_set_y_scroll_stop_exit

; set auto-scroll up to vertical screen 2 at 12.5% of screen
level_1_x_tile_routine_03:
    ldy #$02                ; vertical screen 2
    bne level_set_y_stop_12

; set auto-scroll up to vertical screen 1 at 12.5% of screen
level_1_x_tile_routine_07:
    ldy #$01 ; vertical screen 1

level_set_y_stop_12:
    lda #$20                           ; stop auto-scrolling up at 12.5% vertically
    bne level_1_set_y_scroll_stop_exit

; set auto-scroll up to vertical screen 1 at 87.5% of screen
level_1_x_tile_routine_04:
    ldy #$01                ; vertical screen 1
    bne level_set_y_stop_87

; set auto-scroll up to vertical screen 0 at 87.5% of screen
level_1_x_tile_routine_08:
    ldy #$00

level_set_y_stop_87:
    lda #$e0 ; stop auto-scrolling up at 87.5% vertically

level_1_set_y_scroll_stop_exit:
    jsr set_y_autoscroll_stop
    jmp level_1_adv_x_tile_routine

level_1_x_tile_routine_09:
    lda #$c0
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS

level_1_x_tile_routine_0b:
    lda #$01
    sta SCREEN_SCROLL_TYPE         ; set vertical/overhead scrolling
    bne level_1_adv_x_tile_routine ; always branch

; set background palettes 2 and 3
level_1_x_tile_routine_0a:
    ldy #$80        ; background palette 2
                    ; COLOR_LT_ORANGE_27,COLOR_MED_RED_16,COLOR_DARK_MAGENTA_04
    jsr set_palette ; set background palette 2 to orange, medium red, and dark magenta
    ldy #$84        ; background palette 3
                    ; COLOR_MED_RED_16,COLOR_DARK_RED_06,COLOR_DARK_GRAY_00
    jsr set_palette ; set background palette 3 to medium red, dark red, and dark gray

level_1_adv_x_tile_routine:
    inc X_TILE_ROUTINE
    rts

level_1_x_tile_routine_0c:
    rts

level_1_check_run_y_tile_routine:
    lda Y_TILE_ROUTINE                     ; load tile routine index, used to update tile banks and pallettes
    asl                                    ; double since each entry is a #$02 byte memory address
    tay                                    ; transfer to offset register
    lda level_1_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN                           ; compare to vertical screen index
    bcc @continue
    bne @exit
    lda level_1_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcs @exit

@continue:
    jsr level_1_run_y_tile_routine
    clc

@exit:
    rts

level_1_y_tile_routine_pts_tbl:
    .byte $02,$80 ; Y_TILE_ROUTINE #$00, vertical screen #$02, vertical scroll #$80
    .byte $02,$e0 ; Y_TILE_ROUTINE #$01, vertical screen #$02, vertical scroll #$e0
    .byte $ff

level_1_run_y_tile_routine:
    lda Y_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_1_y_tile_routine_ptr_tbl:
    .addr level_1_y_tile_routine_00
    .addr level_1_y_tile_routine_01
    .addr level_1_y_tile_routine_02

level_1_y_tile_routine_00:
    ldy #$64              ; dark teal, light gray, dark gray
    jsr set_level_palette ; set background palette 1

level_1_adv_y_tile_routine:
    inc Y_TILE_ROUTINE
    rts

level_1_y_tile_routine_01:
    lda #$38
    sta LEFT_TOP_HALF_CHR_BANK     ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$40
    sta Y_SCROLL_FLAGS
    bne level_1_adv_y_tile_routine ; always branch

level_1_y_tile_routine_02:
    rts

level_2_run_tile_routines:
    lda PAUSE_STATE ; (0 = not paused, 1 = paused)
    beq @continue
    rts

@continue:
    lda Y_TILE_ROUTINE
    asl
    tay
    lda level_2_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN
    bcc @exit
    beq @scroll_check
    bcs @run_y_tile_routine

@scroll_check:
    lda level_2_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcc @exit

@run_y_tile_routine:
    jsr level_2_run_y_tile_routine
    clc

@exit:
    rts

level_2_y_tile_routine_pts_tbl:
    .byte $0a,$00 ; Y_TILE_ROUTINE #$00, vertical screen #$0a, vertical scroll #$00
    .byte $09,$e0 ; Y_TILE_ROUTINE #$01, vertical screen #$09, vertical scroll #$e0
    .byte $09,$8e ; Y_TILE_ROUTINE #$02, vertical screen #$09, vertical scroll #$8e
    .byte $08,$d0 ; Y_TILE_ROUTINE #$03, vertical screen #$08, vertical scroll #$d0
    .byte $08,$8e ; Y_TILE_ROUTINE #$04, vertical screen #$08, vertical scroll #$e8
    .byte $08,$80 ; Y_TILE_ROUTINE #$05, vertical screen #$08, vertical scroll #$80
    .byte $07,$90 ; Y_TILE_ROUTINE #$06, vertical screen #$07, vertical scroll #$90
    .byte $03,$00 ; Y_TILE_ROUTINE #$07, vertical screen #$03, vertical scroll #$00
    .byte $02,$18 ; Y_TILE_ROUTINE #$08, vertical screen #$02, vertical scroll #$18
    .byte $02,$00 ; Y_TILE_ROUTINE #$09, vertical screen #$02, vertical scroll #$00
    .byte $01,$60 ; Y_TILE_ROUTINE #$0a, vertical screen #$01, vertical scroll #$60
    .byte $01,$00 ; Y_TILE_ROUTINE #$0b, vertical screen #$01, vertical scroll #$00
    .byte $00,$80 ; Y_TILE_ROUTINE #$0c, vertical screen #$00, vertical scroll #$80
    .byte $ff

level_2_run_y_tile_routine:
    lda Y_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_2_y_tile_routine_ptr_tbl:
    .addr level_2_y_tile_routine_00 ; right before tile color change, change palettes
    .addr level_2_y_tile_routine_01 ; initialize number of tanks, set Y auto-stop position (revealing first overhead tank)
    .addr level_2_y_tile_routine_02 ; clear Y auto-stop, set Y scroll flags
    .addr level_2_y_tile_routine_01
    .addr level_2_y_tile_routine_02 ; clear Y auto-stop, set Y scroll flags
    .addr level_2_y_tile_routine_05
    .addr level_2_y_tile_routine_06 ; level 2 (mid-level) collision code indices
    .addr level_2_y_tile_routine_07
    .addr level_2_y_tile_routine_08
    .addr level_2_y_tile_routine_09
    .addr level_2_y_tile_routine_08
    .addr level_2_y_tile_routine_02 ; clear Y auto-stop, set Y scroll flags
    .addr level_2_y_tile_routine_02 ; clear Y auto-stop, set Y scroll flags
    .addr level_2_y_tile_routine_0d

; right before tile color change, change palettes
level_2_y_tile_routine_00:
    ldy #$68                            ; dark olive, light gray, dark gray
    jsr set_level_palette               ; set background palette 1
    ldy #$6c                            ; dark olive, light gray, medium olive
    jsr set_level_palette               ; set background palette 2
    ldy #$70                            ; background palette 3 (white, light gray, dark gray)
    bne level_2_set_palette_adv_routine ; always branch

level_2_y_tile_routine_05:
    ldy #$74 ; background palette 2 (white, light gray, dark gray)

level_2_set_palette_adv_routine:
    jsr set_level_palette ; set background palette 2 or 3

level_2_adv_tile_routine:
    inc Y_TILE_ROUTINE
    rts

level_2_y_tile_routine_06:
    ldy #$78                            ; light orange, medium orange, dark orange
    jsr set_level_palette               ; set background palette 0
    ldy #$20                            ; level 2 (mid-level) collision code indices
    jsr load_alt_collision_code_indices ; load collision code indices for level 2 (mid-level)
    ldy #$7c
    lda #$5c
    ldx #$5e
    sta LEFT_TOP_HALF_CHR_BANK          ; set bank number of PPU $0000-$07ff (top half of left pattern table)

level_2_set_bottom_half_adv_routine:
    stx LEFT_BOTTOM_CHR_HALF_BANK
    bne level_2_set_palette_adv_routine ; always branch

level_2_y_tile_routine_07:
    ldy #$88
    bne level_2_set_palette_adv_routine

level_2_y_tile_routine_09:
    ldy #$8c                                ; white, light gray, medium olive
    jsr set_level_palette                   ; set background palette 0
    ldy #$90
    ldx #$32
    bne level_2_set_bottom_half_adv_routine

; initialize number of tanks, set Y auto-stop position (revealing first overhead tank)
level_2_y_tile_routine_01:
    lda #$88
    ldy #$00
    sty NUM_TANKS                        ; clear number of tanks on screen
    beq level_2_set_stop_pos_adv_routine ; always branch

level_2_y_tile_routine_08:
    lda #$ee

level_2_set_stop_pos_adv_routine:
    sta Y_AUTOSCROLL_STOP_POS
    jmp level_2_adv_tile_routine

; clear Y auto-stop, set Y scroll flags
level_2_y_tile_routine_02:
    lda #$00
    sta Y_AUTOSCROLL_STOP_POS
    lda #$c0
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    bne level_2_adv_tile_routine

level_2_y_tile_routine_0d:
    rts

level_3_run_tile_routines:
    lda PALETTE_CYCLE_INDEX     ; load background palette cycle index
    bmi @check_run_tile_routine ; branch if no background palette cycle enabled
    lda FRAME_COUNTER           ; load frame counter
    and #$07
    bne @check_run_tile_routine ; branch if not the #$08th frame
    inc PALETTE_CYCLE_INDEX     ; increment every #$08 frames
    ldy PALETTE_CYCLE_INDEX     ; load background palette cycle index
    cpy #$09                    ; see if past last palette color index to cycle through
    bcc @continue
    ldy #$00                    ; reset every #$40 frames

@continue:
    sty PALETTE_CYCLE_INDEX              ; set cycle index
    lda level_3_boss_palette_color_tbl,y ; load color for background palette 0 color 3
    sta PALETTE_CPU_BUFFER+3             ; set correct color for black-red-black palette cycle for boss

@check_run_tile_routine:
    lda PAUSE_STATE                      ; (0 = not paused, 1 = paused)
    beq level_3_check_run_x_tile_routine ; run X_TILE_ROUTINE if at checkpoint in level
    rts                                  ; exit if paused

; the 3rd color to use in background palette 0 for level 3 boss to cycle
; goes from black to red then back to black
level_3_boss_palette_color_tbl:
    .byte COLOR_BLACK_0f,COLOR_BLACK_0f,COLOR_DARK_RED_06
    .byte COLOR_MED_RED_16,COLOR_LT_RED_26,COLOR_MED_RED_16
    .byte COLOR_DARK_RED_06,COLOR_BLACK_0f,COLOR_BLACK_0f

; run X_TILE_ROUTINE if at checkpoint in level
level_3_check_run_x_tile_routine:
    lda X_TILE_ROUTINE
    asl
    tay
    lda level_3_x_tile_routine_pts_tbl,y
    cmp X_SCREEN
    bcc @run_tile_routine
    bne @exit
    lda level_3_x_tile_routine_pts_tbl+1,y
    cmp X_SCROLL                           ; compare to PPU vertical scroll
    beq @run_tile_routine
    bcs @exit

@run_tile_routine:
    jsr level_3_run_tile_routine ; update palette, scroll, and/or background tiles based on X_TILE_ROUTINE
    clc

@exit:
    rts

; breakpoints where the horizontal tile routine needs to be run
level_3_x_tile_routine_pts_tbl:
    .byte $06,$00 ; X_TILE_ROUTINE #$00, screen #$06, scroll #$00
    .byte $0c,$00 ; X_TILE_ROUTINE #$01, screen #$0c, scroll #$00
    .byte $0e,$00 ; X_TILE_ROUTINE #$02, screen #$0e, scroll #$00
    .byte $0e,$02 ; X_TILE_ROUTINE #$03, screen #$0e, scroll #$02
    .byte $10,$00 ; X_TILE_ROUTINE #$04, screen #$10, scroll #$00
    .byte $13,$00 ; X_TILE_ROUTINE #$05, screen #$13, scroll #$00
    .byte $ff

; level 3 - update palette, scroll, and/or background tiles based on X_TILE_ROUTINE
; !(OBS) interesting that this was implemented without a jump table
level_3_run_tile_routine:
    ldx X_TILE_ROUTINE            ; load horizontal tile routine
    bne @tile_routine_01          ; branch if X_TILE_ROUTINE at least #$01
    lda #$10                      ; X_TILE_ROUTINE is #$00
    sta LEFT_BOTTOM_CHR_HALF_BANK ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    ldy #$08                      ; dark violet, medium violet, light violet
    jsr set_level_palette         ; set background palette 2
    ldy #$0c                      ; dark violet, medium violet, dark green
    bne @set_palette_adv_routine  ; always branch to set background palette 3
                                  ; and move to next X_TILE_ROUTINE

@tile_routine_01:
    dex
    bne @tile_routine_02  ; branch if X_TILE_ROUTINE at least #$02
    ldy #$10              ; X_TILE_ROUTINE is #$01
                          ; medium olive, medium green, dark green
    jsr set_level_palette ; set background palette 2
    ldy #$14              ; white, light gray, dark gray

@set_palette_adv_routine:
    jsr set_level_palette ; set background palette to y

@adv_routine:
    inc X_TILE_ROUTINE
    rts

@tile_routine_02:
    dex
    bne @tile_routine_03          ; branch if X_TILE_ROUTINE at least #$03
    lda #$01                      ; X_TILE_ROUTINE is #$02
    sta SCREEN_SCROLL_TYPE        ; set vertical/overhead scrolling
    lda #$12
    sta LEFT_TOP_HALF_CHR_BANK    ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$14
    sta LEFT_BOTTOM_CHR_HALF_BANK ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    ldy #$18                      ; medium olive, medium green, dark green
    jsr set_level_palette         ; set background palette 0
    ldy #$1c                      ; white, light gray, dark gray
    bne @set_palette_adv_routine  ; always branch to set background palette 1
                                  ; and move to next X_TILE_ROUTINE

@tile_routine_03:
    dex
    bne @tile_routine_04          ; branch if X_TILE_ROUTINE at least #$04
    lda #$10                      ; X_TILE_ROUTINE is #$03
    sta LEFT_BOTTOM_CHR_HALF_BANK ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    ldy #$20                      ; light olive, medium olive, dark olive
    jsr set_level_palette         ; set background palette 1
    ldy #$24                      ; dark olive, light gray, dark gray
    bne @set_palette_adv_routine  ; always branch to set background palette 2
                                  ; and move to next X_TILE_ROUTINE

@tile_routine_04:
    dex
    bne @tile_routine_05         ; branch if X_TILE_ROUTINE at least #$05
    ldy #$44                     ; X_TILE_ROUTINE is #$04
                                 ; white, light gray, black
    jsr set_level_palette        ; set background palette 0
    ldy #$48                     ; pale teal, medium teal, dark teal
    bne @set_palette_adv_routine ; always branch to set background palette 2
                                 ; and move to next X_TILE_ROUTINE

@tile_routine_05:
    dex
    bne @exit              ; exit if X_TILE_ROUTINE is more than #$05
    lda #$02               ; X_TILE_ROUTINE is #$05
    sta SCREEN_SCROLL_TYPE ; set final level mid level change scroll type
    bne @adv_routine       ; always branch to increment X_TILE_ROUTINE

@exit:
    rts

level_4_run_tile_routines:
    lda FRAME_COUNTER           ; load frame counter
    and #$07
    bne @check_run_tile_routine ; branch if not the #$08th frame
    inc PALETTE_CYCLE_INDEX     ; increment every #$08 frames
    ldy PALETTE_CYCLE_INDEX     ; load background palette cycle index
    cpy #$09                    ; see if past last palette color index to cycle through
    bcc @continue
    ldy #$00                    ; reset every #$40 frames

@continue:
    sty PALETTE_CYCLE_INDEX         ; set cycle index
    lda level_4_palette_color_tbl,y ; load color for background palette 3 color 3
    sta PALETTE_CPU_BUFFER+15       ; bg palette 3 color 3

@check_run_tile_routine:
    lda PAUSE_STATE                     ; (0 = not paused, 1 = paused)
    beq level_4_check_run_tile_routines
    rts

; bg palette 3 color 3
level_4_palette_color_tbl:
    .byte COLOR_BLACK_0f,COLOR_BLACK_0f,COLOR_DARK_RED_06
    .byte COLOR_MED_RED_16,COLOR_LT_RED_26,COLOR_MED_RED_16
    .byte COLOR_DARK_RED_06,COLOR_BLACK_0f,COLOR_BLACK_0f

; run horizontal and vertical tile routines
level_4_check_run_tile_routines:
    jsr level_4_check_y_run_tile_routine
    lda X_TILE_ROUTINE
    asl
    tay
    lda level_4_x_tile_routine_pts_tbl,y
    cmp X_SCREEN
    bcc @run_x_routine
    bne @exit
    lda level_4_x_tile_routine_pts_tbl+1,y
    cmp X_SCROLL                           ; compare to PPU vertical scroll
    beq @run_x_routine
    bcs @exit

@run_x_routine:
    jsr level_4_run_x_tile_routine
    clc

@exit:
    rts

level_4_x_tile_routine_pts_tbl:
    .byte $02,$a0 ; X_TILE_ROUTINE #$00, screen #$02, scroll #$a0
    .byte $03,$00 ; X_TILE_ROUTINE #$01, screen #$03, scroll #$00
    .byte $03,$c0 ; X_TILE_ROUTINE #$02, screen #$03, scroll #$c0
    .byte $ff

level_4_run_x_tile_routine:
    lda X_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_4_x_tile_routine_ptr_tbl:
    .addr level_4_x_tile_routine_00
    .addr level_4_x_tile_routine_01
    .addr level_4_x_tile_routine_02
    .addr level_4_x_tile_routine_03

level_4_x_tile_routine_00:
    lda #$01                       ; X auto-scroll mode 1
    sta X_AUTOSCROLL               ; set to auto-scroll right to reveal next nametable
    bne level_4_adv_x_tile_routine ; always branch

level_4_x_tile_routine_01:
    lda #$01
    sta SCREEN_SCROLL_TYPE ; set vertical/overhead scrolling

level_4_adv_x_tile_routine:
    inc X_TILE_ROUTINE
    rts

level_4_x_tile_routine_02:
    lda #$01
    sta SCREEN_SCROLL_TYPE         ; set vertical/overhead scrolling
    bne level_4_adv_x_tile_routine

level_4_x_tile_routine_03:
    rts

level_4_check_y_run_tile_routine:
    lda Y_TILE_ROUTINE
    asl
    tay
    lda level_4_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN
    bcc @exit
    beq @continue
    bcs @run_y_tile_routine

@continue:
    lda level_4_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcc @exit

@run_y_tile_routine:
    jsr level_4_run_y_tile_routine
    clc

@exit:
    rts

level_4_y_tile_routine_pts_tbl:
    .byte $0b,$e0 ; Y_TILE_ROUTINE #$00, screen #$0b, scroll #$e0
    .byte $09,$30 ; Y_TILE_ROUTINE #$01, screen #$09, scroll #$30
    .byte $09,$00 ; Y_TILE_ROUTINE #$02, screen #$09, scroll #$00
    .byte $08,$40 ; Y_TILE_ROUTINE #$03, screen #$08, scroll #$40
    .byte $06,$00 ; Y_TILE_ROUTINE #$04, screen #$06, scroll #$00
    .byte $05,$40 ; Y_TILE_ROUTINE #$05, screen #$05, scroll #$40
    .byte $04,$c0 ; Y_TILE_ROUTINE #$06, screen #$04, scroll #$c0
    .byte $02,$d0 ; Y_TILE_ROUTINE #$07, screen #$02, scroll #$d0
    .byte $01,$00 ; Y_TILE_ROUTINE #$08, screen #$01, scroll #$00
    .byte $ff

level_4_run_y_tile_routine:
    lda Y_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_4_y_tile_routine_ptr_tbl:
    .addr level_4_y_tile_routine_00 ; swap out 1/4 of sprite tiles for the elevator portion of level, advance routine
    .addr level_4_y_tile_routine_01 ; set Y auto scroll stop position to reveal ceiling tiles (barrels), advance routine
    .addr level_4_y_tile_routine_02 ; set Y auto-scroll stop position, advance routine
    .addr level_4_y_tile_routine_03 ; enable elevator with Y velocity of -1.00, advance routine
    .addr level_4_y_tile_routine_04 ; set Y auto-scroll stop position, disable elevator, advance routine
    .addr level_4_y_tile_routine_03 ; enable elevator with Y velocity of -1.00, advance routine
    .addr level_4_y_tile_routine_06 ; set elevator checkpoint to #$01, advance routine
    .addr level_4_y_tile_routine_07 ; set elevator checkpoint to #$00, advance routine
    .addr level_4_y_tile_routine_04 ; set Y auto-scroll stop position, disable elevator, advance routine
    .addr level_4_y_tile_routine_08 ; no op

; swap out 1/4 of sprite tiles for the elevator portion of level
level_4_y_tile_routine_00:
    lda #$1b                       ; replacing sniper and grenade thrower sprites with hawkman and turret sprites
    sta RIGHT_FOURTH_QTR_CHR_BANK  ; set bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    bne level_4_adv_y_tile_routine

; set Y auto scroll stop position to reveal ceiling tiles (barrels), advance routine
level_4_y_tile_routine_01:
    lda #$ee
    sta Y_AUTOSCROLL_STOP_POS      ; set Y auto scroll stop position to reveal ceiling tiles (barrels)
    bne level_4_adv_y_tile_routine

; set Y auto-scroll stop position, advance routine
level_4_y_tile_routine_02:
    lda #$00
    sta Y_AUTOSCROLL_STOP_POS      ; stop when revealed next nametable (elevator)
    lda #$c0
    sta Y_SCROLL_FLAGS             ; prevent player from causing screen scroll
    sta LEVEL_Y_SCROLL_FLAGS       ; prevent player from causing screen scroll
    bne level_4_adv_y_tile_routine

; enable elevator with Y velocity of -1.00
level_4_y_tile_routine_03:
    lda #$80
    sta ELEVATOR_ENABLED           ; enable elevator
    lda #$00
    sta ELEVATOR_FRACT_VEL
    lda #$ff
    sta ELEVATOR_FAST_VEL
    bne level_4_adv_y_tile_routine

; set Y auto-scroll stop position, disable elevator
level_4_y_tile_routine_04:
    lda #$c0
    sta LEVEL_Y_SCROLL_FLAGS       ; set Y auto scroll stop Y position
    lda #$00
    sta ELEVATOR_ENABLED           ; disable elevator
    beq level_4_adv_y_tile_routine

; set elevator checkpoint to #$01, advance routine
level_4_y_tile_routine_06:
    lda #$01
    bne level_4_set_elevator_checkpoint ; always branch

; set elevator checkpoint to #$00, advance routine
level_4_y_tile_routine_07:
    lda #$00

level_4_set_elevator_checkpoint:
    sta ELEVATOR_CHECKPOINT

level_4_adv_y_tile_routine:
    inc Y_TILE_ROUTINE
    rts

; no op
level_4_y_tile_routine_08:
    rts

level_5_run_tile_routines:
    lda PALETTE_CYCLE_INDEX   ; load background palette cycle index
    bmi @second_palette_cycle ; branch if no background palette cycle enabled
    lda FRAME_COUNTER         ; load frame counter
    and #$07
    bne @second_palette_cycle ; branch if not the #$08th frame
    inc PALETTE_CYCLE_INDEX   ; increment every #$08 frames
    ldy PALETTE_CYCLE_INDEX   ; load background palette cycle index
    cpy #$06                  ; see if past last palette color index to cycle through
    bcc @set_palette
    ldy #$00                  ; reset every #$30 frames

@set_palette:
    sty PALETTE_CYCLE_INDEX         ; set cycle index
    lda level_5_palette_cycle_tbl,y ; load background palette 2 to replace
    tay                             ; transfer to method parameter
    jsr set_level_palette           ; set background palette

@second_palette_cycle:
    lda PALETTE_CYCLE_INDEX_2 ; load second background palette cycle index
    bmi @continue             ; branch if no second background palette cycle enabled
    lda FRAME_COUNTER         ; load frame counter
    and #$03
    bne @continue             ; branch if not the #$04th frame
    inc PALETTE_CYCLE_INDEX_2 ; increment every #$04 frames
    lda PALETTE_CYCLE_INDEX_2 ; load second background palette cycle index
    and #$40                  ; keep track of bit 6
    sta $00                   ; (1 = update bg palette 2 color 3, 0 = update bg 3 color 3)
                              ; set for krypto-crustacean, clear for cliff
    lda PALETTE_CYCLE_INDEX_2 ; load second background palette cycle index
    and #$3f                  ; strip bit 7 (no need to do this as it's already non-negative)
    cmp #$09
    bcc @set_cycle_2_color    ; see if past last palette color index to cycle through
    lda #$00                  ; reset every #$24 frames

@set_cycle_2_color:
    tay                               ; transfer palette cycle index value to y offset
    ora $00                           ; merge palette cycle index with saved bit 6
    sta PALETTE_CYCLE_INDEX_2         ; set cycle index
    asl
    asl                               ; push bit 6 to carry (bg palette flag)
    lda level_5_palette_cycle_2_tbl,y ; load palette color
    bcs @set_palette_2_color_3        ; if bit 6 set, branch to update color 3 for bg palette 2
    sta PALETTE_CPU_BUFFER+15         ; set bg palette 3 color 3 (hill climb)
    bcc @continue                     ; always branch

@set_palette_2_color_3:
    sta PALETTE_CPU_BUFFER+11 ; set bg palette 2 color 3 (krypto-crustacean)

@continue:
    lda PAUSE_STATE                     ; (0 = not paused, 1 = paused)
    beq level_5_check_run_tile_routines
    rts

; background palette indices, indexes into alternate_palettes_tbl
; used to cycle cloud colors for cliff background
level_5_palette_cycle_tbl:
    .byte $38 ; background palette 2 (light purple, medium purple, dark purple)
    .byte $3c ; background palette 2 (light teal, medium purple, dark teal)
    .byte $40 ; background palette 2 (light teal, medium teal, dark teal)
    .byte $40 ; background palette 2 (light teal, medium teal, dark teal)
    .byte $3c ; background palette 2 (light teal, medium purple, dark teal)
    .byte $38 ; background palette 2 (light purple, medium purple, dark purple)

; colors for color 3 of bg palette 2 or bg palette 3
level_5_palette_cycle_2_tbl:
    .byte COLOR_BLACK_0f,COLOR_BLACK_0f,COLOR_DARK_RED_06
    .byte COLOR_MED_RED_16,COLOR_LT_RED_26,COLOR_MED_RED_16
    .byte COLOR_DARK_RED_06,COLOR_BLACK_0f,COLOR_BLACK_0f

level_5_check_run_tile_routines:
    jsr level_5_check_y_run_tile_routine
    lda SCREEN_SCROLL_TYPE                 ; 0 = horizontal, 1 = vertical/overhead
    bne @exit
    lda X_TILE_ROUTINE
    asl
    tay
    lda level_5_x_tile_routine_pts_tbl,y
    cmp X_SCREEN
    bcc @run_x_routine
    bne @exit
    lda level_5_x_tile_routine_pts_tbl+1,y
    cmp X_SCROLL                           ; compare to PPU vertical scroll
    beq @run_x_routine
    bcs @exit

@run_x_routine:
    jsr level_5_run_x_tile_routine
    clc

@exit:
    rts

level_5_x_tile_routine_pts_tbl:
    .byte $00,$02 ; X_TILE_ROUTINE #$00, screen #$00, scroll #$02
    .byte $01,$00 ; X_TILE_ROUTINE #$01, screen #$01, scroll #$00
    .byte $01,$80 ; X_TILE_ROUTINE #$02, screen #$01, scroll #$80
    .byte $05,$00 ; X_TILE_ROUTINE #$03, screen #$05, scroll #$00
    .byte $05,$e0 ; X_TILE_ROUTINE #$04, screen #$05, scroll #$e0
    .byte $ff

level_5_run_x_tile_routine:
    lda X_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_5_x_tile_routine_ptr_tbl:
    .addr level_5_x_tile_routine_00 ; level 5 top of cliff
    .addr level_5_x_tile_routine_01
    .addr level_5_x_tile_routine_02
    .addr level_5_x_tile_routine_03
    .addr level_5_x_tile_routine_04
    .addr level_5_x_tile_routine_05

level_5_x_tile_routine_00:
    ldy #$00                            ; level 5 top of cliff collision code indices
    jsr load_alt_collision_code_indices ; load collision code indices for level 5 top of cliff
    lda #$20
    sta LEFT_TOP_HALF_CHR_BANK          ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$22
    sta LEFT_BOTTOM_CHR_HALF_BANK       ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    ldy #$28                            ; dark gray, medium olive, dark green

level_5_set_palette_adv_routine:
    jsr set_level_palette ; set background palette

level_5_adv_x_tile_routine:
    inc X_TILE_ROUTINE
    rts

level_5_x_tile_routine_01:
    lda #$ff
    sta PALETTE_CYCLE_INDEX
    ldy #$2c
    bne level_5_set_palette_adv_routine

level_5_x_tile_routine_02:
    lda #$80

; input
;  * a
level_5_set_scroll_flags_adv_tile_routine:
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    bne level_5_adv_x_tile_routine

level_5_x_tile_routine_03:
    lda #$40
    sta PALETTE_CYCLE_INDEX_2                     ; start boss screen for krypto-crustacean palette cycling (bg palette 2 color 3)
    ldy #$a4                                      ; light magenta, medium magenta, dark magenta
    jsr set_level_palette                         ; set background palette 1
    ldy #$a8                                      ; white, light gray, dark red
    jsr set_level_palette                         ; set background palette 2
    lda #$02
    sta SCREEN_SCROLL_TYPE
    lda #$c0
    bne level_5_set_scroll_flags_adv_tile_routine ; always branch

level_5_x_tile_routine_04:
    lda #$01
    sta SCREEN_SCROLL_TYPE         ; set vertical/overhead scrolling
    bne level_5_adv_x_tile_routine

level_5_x_tile_routine_05:
    rts

level_5_check_y_run_tile_routine:
    lda Y_TILE_ROUTINE
    asl
    tay
    lda level_5_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN
    bcc @exit
    beq @continue
    bcs @run_tile_routine

@continue:
    lda level_5_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcc @exit

@run_tile_routine:
    jsr level_5_run_y_tile_routine
    clc

@exit:
    rts

level_5_y_tile_routine_pts_tbl:
    .byte $01,$00 ; Y_TILE_ROUTINE #$00, screen #$01, scroll #$00
    .byte $00,$80 ; Y_TILE_ROUTINE #$01, screen #$00, scroll #$80
    .byte $ff

level_5_run_y_tile_routine:
    ldx Y_TILE_ROUTINE
    bne @y_tile_routine_01    ; branch if Y_TILE_ROUTINE #$01
    lda #$ff                  ; Y_TILE_ROUTINE = 0, disable palette cycling
    sta PALETTE_CYCLE_INDEX_2 ; (top of hill) disable second palette cycling
    bne @adv_routine          ; branch to advance routine and exit

@y_tile_routine_01:
    dex
    bne @exit              ; simply exit if somehow past vertical tile routine #$01
    jsr reset_draw_point
    lda #$00
    sta SCREEN_SCROLL_TYPE ; set horizontal scrolling

@adv_routine:
    inc Y_TILE_ROUTINE
    rts                ; !(OBS) unneeded since next line is also an rts

@exit:
    rts

level_6_run_tile_routines:
    ldy PALETTE_CYCLE_INDEX           ; load background palette cycle index
    bmi @second_palette_cycle         ; branch if no background palette cycle enabled
    lda FRAME_COUNTER                 ; transitioning floor color from blue to red
                                      ; load frame counter
    and #$0f
    bne @second_palette_cycle         ; branch if not the #$10th frame
    inc PALETTE_CYCLE_INDEX           ; increment every #$10 frames
    ldy PALETTE_CYCLE_INDEX           ; load background palette cycle index
    lda level_6_palette_color_tbl-1,y ; load color for background palette 1 color 3
    sta PALETTE_CPU_BUFFER+7          ; set correct color for palette cycle
    cpy #$04                          ; see if past last palette color index to cycle through
    bcc @second_palette_cycle
    lda #$ff                          ; finished cycle, disabling palette cycling
    sta PALETTE_CYCLE_INDEX           ; disable palette cycling

; palette cycling for red skull eyes
@second_palette_cycle:
    lda PALETTE_CYCLE_INDEX_2 ; load second background palette cycle index
    bmi @continue             ; branch if no second background palette cycle enabled
    lda FRAME_COUNTER         ; load frame counter
    and #$07
    bne @continue             ; branch if not the #$08th frame
    inc PALETTE_CYCLE_INDEX_2 ; increment every #$08 frames
    ldy PALETTE_CYCLE_INDEX_2 ; load second background palette cycle index
    cpy #$07
    bcc @set_cycle_2_color    ; see if past last palette color index to cycle through
    ldy #$00                  ; reset every #$64 frames

@set_cycle_2_color:
    sty PALETTE_CYCLE_INDEX_2         ; set cycle index
    lda level_6_palette_cycle_2_tbl,y ; load bg palette 3 color 3
    sta PALETTE_CPU_BUFFER+15         ; set bg palette 3 color 3

@continue:
    lda PAUSE_STATE                    ; (0 = not paused, 1 = paused)
    beq level_6_check_run_tile_routine
    rts

; colors used for transitioning the floor color from blue to red
level_6_palette_color_tbl:
    .byte COLOR_DARK_PURPLE_03,COLOR_DARK_MAGENTA_04,COLOR_DARK_PINK_05,COLOR_DARK_RED_06

; colors for color 3 of bg palette 3
level_6_palette_cycle_2_tbl:
    .byte COLOR_BLACK_0f,COLOR_DARK_RED_06,COLOR_MED_RED_16
    .byte COLOR_LT_RED_26,COLOR_MED_RED_16,COLOR_DARK_RED_06
    .byte COLOR_BLACK_0f

level_6_check_run_tile_routine:
    lda Y_TILE_ROUTINE
    asl
    tay
    lda level_6_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN
    bcc @exit
    beq @continue
    bcs @run_y_tile_routine

@continue:
    lda level_6_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcc @exit

@run_y_tile_routine:
    jsr level_6_run_y_tile_routine
    clc

@exit:
    rts

level_6_y_tile_routine_pts_tbl:
    .byte $09,$00 ; Y_TILE_ROUTINE #$00, screen #$09, scroll #$00
    .byte $06,$00 ; Y_TILE_ROUTINE #$01, screen #$06, scroll #$00
    .byte $04,$00 ; Y_TILE_ROUTINE #$02, screen #$04, scroll #$00
    .byte $03,$c0 ; Y_TILE_ROUTINE #$03, screen #$03, scroll #$c0
    .byte $02,$c0 ; Y_TILE_ROUTINE #$04, screen #$02, scroll #$c0
    .byte $01,$60 ; Y_TILE_ROUTINE #$05, screen #$01, scroll #$60
    .byte $01,$00 ; Y_TILE_ROUTINE #$06, screen #$01, scroll #$00
    .byte $ff

level_6_run_y_tile_routine:
    lda Y_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_6_y_tile_routine_ptr_tbl:
    .addr level_6_y_tile_routine_00
    .addr level_6_y_tile_routine_01
    .addr level_6_y_tile_routine_02 ; level 6 before door
    .addr level_6_y_tile_routine_03
    .addr level_6_y_tile_routine_04
    .addr level_6_y_tile_routine_05
    .addr level_6_y_tile_routine_06
    .addr level_6_y_tile_routine_07

level_6_y_tile_routine_00:
    ldy #$94              ; white, light gray, dark violet
    jsr set_level_palette ; set background palette 1

level_6_adv_y_tile_routine:
    inc Y_TILE_ROUTINE
    rts

level_6_y_tile_routine_01:
    lda #$52
    sta LEFT_BOTTOM_CHR_HALF_BANK  ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    bne level_6_adv_y_tile_routine

level_6_y_tile_routine_02:
    ldy #$30                            ; level 6 before door collision code indices
    jsr load_alt_collision_code_indices ; load collision code indices for level 6 before door
    jmp level_6_adv_y_tile_routine

level_6_y_tile_routine_03:
    lda #$00
    sta PALETTE_CYCLE_INDEX
    beq level_6_adv_y_tile_routine

level_6_y_tile_routine_04:
    lda #$70
    bne level_6_set_stop_adv_y_routine ; always branch

level_6_y_tile_routine_05:
    lda #$ee

level_6_set_stop_adv_y_routine:
    sta Y_AUTOSCROLL_STOP_POS
    bne level_6_adv_y_tile_routine

level_6_y_tile_routine_06:
    ldy #$98                       ; light gray, dark gray, dark red
    jsr set_level_palette          ; set background palette 2
    lda #$00
    sta Y_AUTOSCROLL_STOP_POS
    lda #$c0
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    bne level_6_adv_y_tile_routine

level_6_y_tile_routine_07:
    rts

level_7_run_tile_routines:
    lda PAUSE_STATE ; (0 = not paused, 1 = paused)
    beq @continue
    rts

@continue:
    lda Y_TILE_ROUTINE
    asl
    tay
    lda level_7_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN
    bcc @run_y_tile_routine
    bne @exit
    lda level_7_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcs @exit

@run_y_tile_routine:
    jsr level_7_run_y_tile_routine
    clc

@exit:
    rts

level_7_y_tile_routine_pts_tbl:
    .byte $00,$10 ; Y_TILE_ROUTINE #$00, screen #$00, scroll #$10
    .byte $05,$00 ; Y_TILE_ROUTINE #$01, screen #$05, scroll #$00
    .byte $08,$00 ; Y_TILE_ROUTINE #$02, screen #$08, scroll #$00
    .byte $0c,$00 ; Y_TILE_ROUTINE #$03, screen #$0c, scroll #$00
    .byte $0c,$d8 ; Y_TILE_ROUTINE #$04, screen #$0c, scroll #$d8
    .byte $ff

level_7_run_y_tile_routine:
    ldx Y_TILE_ROUTINE
    bne @y_tile_routine_01
    lda #$33
    ldy #$7f

@y_tile_routine_00:
    sta SECOND_BG_COLLISION_DATA,y
    dey
    bpl @y_tile_routine_00
    bmi @adv_y_tile_routine

@y_tile_routine_01:
    dex
    bne @y_tile_routine_02
    lda #$28                   ; dark gray, medium olive, dark green
    sta LEFT_TOP_HALF_CHR_BANK ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    ldy #$30                   ; light green, medium blue green, dark blue green

@set_palette_adv_routine:
    jsr set_level_palette ; set background palette

@adv_y_tile_routine:
    inc Y_TILE_ROUTINE
    rts

@y_tile_routine_02:
    dex
    bne @y_tile_routine_03
    lda #$2a
    sta LEFT_BOTTOM_CHR_HALF_BANK ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda #$65
    sta RIGHT_FOURTH_QTR_CHR_BANK ; set bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    ldy #$34
    bne @set_palette_adv_routine

; level 7 boss (temple of terror)
@y_tile_routine_03:
    dex
    bne @y_tile_routine_04
    ldy #$10                            ; level 7 boss (temple of terror) collision code indices
    jsr load_alt_collision_code_indices ; load collision code indices for level 7 boss (temple of terror)
    lda #$2c
    sta LEFT_TOP_HALF_CHR_BANK          ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$2e
    sta LEFT_BOTTOM_CHR_HALF_BANK       ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda #$66
    sta RIGHT_THIRD_QTR_CHR_BANK        ; set bank number of PPU $1800-$1bff (third quarter of right pattern table)
    ldy #$50                            ; white, light gray, dark gray
    jsr set_level_palette               ; set background palette 1
    ldy #$54
    bne @set_palette_adv_routine

@y_tile_routine_04:
    dex
    bne @exit               ; simply exit if somehow past vertical tile routine #$04
    lda #$c0                ; vertical tile routine #$04
    sta Y_SCROLL_FLAGS
    bne @adv_y_tile_routine

@exit:
    rts

level_8_run_tile_routines:
    lda FRAME_COUNTER           ; load frame counter
    and #$07
    bne @check_run_tile_routine ; branch if not the #$08th frame
    inc PALETTE_CYCLE_INDEX     ; increment every #$08 frames
    ldy PALETTE_CYCLE_INDEX     ; load background palette cycle index
    cpy #$06                    ; see if past last palette index to cycle through
    bcc @set_palette
    ldy #$00                    ; reset every #$30 frames

@set_palette:
    sty PALETTE_CYCLE_INDEX           ; set cycle index
    lda lvl_8_alt_palette_index_tbl,y ; load background palette 1 to replace
    tay                               ; transfer to method parameter
    jsr set_level_palette             ; set background palette

@check_run_tile_routine:
    lda PAUSE_STATE                    ; (0 = not paused, 1 = paused)
    beq level_8_check_run_tile_routine
    rts

; background palette indices, indexes into alternate_palettes_tbl
lvl_8_alt_palette_index_tbl:
    .byte $58 ; background palette 1 (black, medium purple, dark purple)
    .byte $5c ; background palette 1 (black, medium green, dark green)
    .byte $60 ; background palette 1 (black, medium blue green, dark blue green)
    .byte $60 ; background palette 1 (black, medium blue green, dark blue green)
    .byte $5c ; background palette 1 (black, medium green, dark green)
    .byte $58 ; background palette 1 (black, medium purple, dark purple)

level_8_check_run_tile_routine:
    jsr level_8_check_y_run_tile_routine
    lda X_TILE_ROUTINE
    asl
    tay
    lda level_8_x_tile_routine_pts_tbl,y
    cmp X_SCREEN
    bcc @run_x_routine
    bne @exit
    lda level_8_x_tile_routine_pts_tbl+1,y
    cmp X_SCROLL                           ; compare to PPU vertical scroll
    beq @run_x_routine
    bcs @exit

@run_x_routine:
    jsr level_8_run_x_tile_routine
    clc

@exit:
    rts

level_8_x_tile_routine_pts_tbl:
    .byte $03,$80 ; X_TILE_ROUTINE #$00, screen #$03, scroll #$80
    .byte $05,$01 ; X_TILE_ROUTINE #$01, screen #$05, scroll #$01
    .byte $05,$20 ; X_TILE_ROUTINE #$02, screen #$05, scroll #$20
    .byte $05,$b0 ; X_TILE_ROUTINE #$03, screen #$05, scroll #$b0
    .byte $05,$e0 ; X_TILE_ROUTINE #$04, screen #$05, scroll #$e0
    .byte $06,$90 ; X_TILE_ROUTINE #$05, screen #$06, scroll #$90
    .byte $07,$80 ; X_TILE_ROUTINE #$06, screen #$07, scroll #$80
    .byte $0b,$c0 ; X_TILE_ROUTINE #$07, screen #$0b, scroll #$c0
    .byte $0c,$80 ; X_TILE_ROUTINE #$08, screen #$0c, scroll #$80
    .byte $0c,$80 ; X_TILE_ROUTINE #$09, screen #$0c, scroll #$80
    .byte $0d,$a0 ; X_TILE_ROUTINE #$0a, screen #$0d, scroll #$a0
    .byte $0e,$00 ; X_TILE_ROUTINE #$0b, screen #$0e, scroll #$00
    .byte $ff

level_8_run_x_tile_routine:
    lda X_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_8_x_tile_routine_ptr_tbl:
    .addr level_8_x_tile_routine_00
    .addr level_8_x_tile_routine_01
    .addr level_8_x_tile_routine_02
    .addr level_8_x_tile_routine_01
    .addr level_8_x_tile_routine_02
    .addr level_8_x_tile_routine_05
    .addr level_8_x_tile_routine_06 ; level 8 stomping ceiling
    .addr level_8_x_tile_routine_07
    .addr level_8_x_tile_routine_05
    .addr level_8_x_tile_routine_09
    .addr level_8_x_tile_routine_01
    .addr level_8_x_tile_routine_0b
    .addr level_8_x_tile_routine_0c

level_8_x_tile_routine_00:
    lda #$40
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    bne level_8_adv_x_tile_routine

level_8_x_tile_routine_02:
    lda #$02
    sta SCREEN_SCROLL_TYPE
    lda #$00
    sta X_AUTOSCROLL       ; stop any horizontal auto-scroll

level_8_adv_x_tile_routine:
    inc X_TILE_ROUTINE
    rts

level_8_x_tile_routine_05:
    lda #$c0
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    lda #$01
    ldy Y_SCROLL             ; load PPU vertical scroll
    bmi @continue
    lda #$ee

@continue:
    sta Y_AUTOSCROLL_STOP_POS
    jmp level_8_adv_x_tile_routine

level_8_x_tile_routine_06:
    ldy #$40                            ; level 8 stomping ceiling collision code indices
    jsr load_alt_collision_code_indices ; load collision code indices for level 8 stomping ceiling
    lda #$6c
    sta LEFT_TOP_HALF_CHR_BANK          ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$6e
    sta LEFT_BOTTOM_CHR_HALF_BANK       ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    bne level_8_adv_x_tile_routine

level_8_x_tile_routine_07:
    lda #$80
    sta Y_SCROLL_FLAGS
    sta LEVEL_Y_SCROLL_FLAGS
    bne level_8_adv_x_tile_routine

level_8_x_tile_routine_09:
    lda Y_SCROLL                   ; load PPU vertical scroll
    bne level_8_x_tile_routine_0c
    lda #$00
    sta Y_AUTOSCROLL_STOP_POS
    beq level_8_adv_x_tile_routine

level_8_x_tile_routine_01:
    lda #$01                       ; X auto-scroll mode 1
    sta X_AUTOSCROLL               ; set to auto-scroll right to reveal next nametable
    bne level_8_adv_x_tile_routine

level_8_x_tile_routine_0b:
    lda #$01
    sta SCREEN_SCROLL_TYPE         ; set vertical/overhead scrolling
    bne level_8_adv_x_tile_routine

level_8_x_tile_routine_0c:
    rts

level_8_check_y_run_tile_routine:
    lda Y_TILE_ROUTINE
    asl
    tay
    lda level_8_y_tile_routine_pts_tbl,y
    cmp Y_SCREEN
    bcc @exit
    beq @continue
    bcs @run_y_tile_routine

@continue:
    lda level_8_y_tile_routine_pts_tbl+1,y
    cmp Y_SCROLL                           ; compare to PPU vertical scroll
    bcc @exit

@run_y_tile_routine:
    jsr level_8_run_y_tile_routine
    clc

@exit:
    rts

level_8_y_tile_routine_pts_tbl:
    .byte $01,$d0 ; Y_TILE_ROUTINE #$00, screen #$01, scroll #$d0
    .byte $01,$20 ; Y_TILE_ROUTINE #$01, screen #$01, scroll #$20
    .byte $00,$c0 ; Y_TILE_ROUTINE #$02, screen #$00, scroll #$c0
    .byte $ff

level_8_run_y_tile_routine:
    lda Y_TILE_ROUTINE
    jsr run_routine_from_tbl_below

level_8_y_tile_routine_ptr_tbl:
    .addr level_8_y_tile_routine_00
    .addr level_8_y_tile_routine_00
    .addr level_8_y_tile_routine_02
    .addr level_8_y_tile_routine_03

level_8_y_tile_routine_00:
    lda #$00
    sta SCREEN_SCROLL_TYPE ; set horizontal scrolling

level_8_adv_y_tile_routine:
    inc Y_TILE_ROUTINE
    rts

level_8_y_tile_routine_02:
    lda #$c0
    sta LEVEL_Y_SCROLL_FLAGS
    bne level_8_adv_y_tile_routine

level_8_y_tile_routine_03:
    rts

level_4_screen_layout_tbl:
    .byte $07                         ; LEVEL_WIDTH
    .byte $0d                         ; LEVEL_HEIGHT
    .byte $00,$00,$00,$0e,$12,$00,$10
    .byte $00,$00,$00,$0d,$11,$00,$0f
    .byte $00,$00,$00,$0c,$00,$00,$00
    .byte $00,$00,$00,$0b,$00,$00,$00
    .byte $00,$00,$00,$0a,$00,$00,$00
    .byte $00,$00,$00,$09,$00,$00,$00
    .byte $00,$00,$00,$08,$00,$00,$00
    .byte $00,$00,$00,$07,$00,$00,$00
    .byte $00,$00,$00,$06,$00,$00,$00
    .byte $00,$00,$00,$05,$00,$00,$00
    .byte $00,$00,$00,$04,$00,$00,$00
    .byte $00,$00,$00,$03,$00,$00,$00
    .byte $01,$01,$01,$02,$00,$00,$00

level_4_supertiles_screen_ptr_table:
    .addr level_4_supertiles_screen_00
    .addr level_4_supertiles_screen_01
    .addr level_4_supertiles_screen_02
    .addr level_4_supertiles_screen_03
    .addr level_4_supertiles_screen_04
    .addr level_4_supertiles_screen_05
    .addr level_4_supertiles_screen_06
    .addr level_4_supertiles_screen_07
    .addr level_4_supertiles_screen_08
    .addr level_4_supertiles_screen_09
    .addr level_4_supertiles_screen_0a
    .addr level_4_supertiles_screen_0b
    .addr level_4_supertiles_screen_0c
    .addr level_4_supertiles_screen_0d
    .addr level_4_supertiles_screen_0e
    .addr level_4_supertiles_screen_0f
    .addr level_4_supertiles_screen_10
    .addr level_4_supertiles_screen_11
    .addr level_4_supertiles_screen_12

level_4_supertiles_screen_00:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_4_supertiles_screen_01:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$8b,$12,$8b,$12,$8b,$12,$8b,$12
    .byte $78,$79,$7a,$7b,$7a,$7c,$7a,$88,$81,$82,$00,$86,$86,$86,$86,$2b
    .byte $62,$27,$00,$87,$87,$87,$87,$00,$7d,$7e,$7f,$80,$7d,$7e,$7f,$80
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_4_supertiles_screen_02:
    .byte $30,$41,$10,$10,$10,$10,$42,$31,$18,$40,$00,$81,$82,$00,$32,$1b
    .byte $89,$2b,$3c,$62,$27,$3c,$2b,$1a,$00,$2b,$00,$00,$00,$00,$2b,$1b
    .byte $00,$00,$15,$15,$15,$15,$00,$19,$7d,$7e,$7f,$80,$8a,$7e,$8a,$13
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_4_supertiles_screen_03:
    .byte $5e,$36,$37,$00,$63,$68,$15,$23,$4d,$2e,$15,$68,$66,$15,$15,$19
    .byte $44,$01,$63,$64,$4f,$01,$00,$1b,$45,$26,$66,$65,$2b,$3c,$63,$84
    .byte $16,$26,$14,$14,$63,$68,$6a,$5f,$20,$14,$3c,$67,$66,$15,$6b,$23
    .byte $1c,$68,$64,$3c,$3c,$3c,$67,$19,$1e,$00,$00,$00,$00,$00,$00,$1f

level_4_supertiles_screen_04:
    .byte $16,$00,$3c,$8c,$01,$15,$32,$19,$50,$4f,$01,$8d,$8e,$68,$15,$23
    .byte $49,$6c,$68,$8f,$11,$64,$2b,$5d,$44,$26,$15,$0e,$0f,$3c,$3c,$5b
    .byte $20,$14,$33,$2b,$67,$68,$68,$1d,$50,$3c,$72,$73,$71,$2c,$2d,$4e
    .byte $3a,$2f,$2f,$73,$70,$14,$14,$21,$34,$15,$15,$00,$00,$00,$00,$1d

level_4_supertiles_screen_05:
    .byte $30,$41,$10,$10,$10,$10,$42,$31,$83,$40,$00,$00,$00,$00,$32,$19
    .byte $5a,$3c,$3c,$71,$01,$38,$00,$1a,$49,$6c,$68,$6e,$69,$4f,$63,$1b
    .byte $44,$26,$68,$15,$6a,$69,$65,$19,$45,$26,$15,$15,$6b,$67,$6a,$1b
    .byte $20,$14,$14,$14,$67,$14,$14,$21,$1c,$00,$00,$00,$00,$00,$00,$1d

level_4_supertiles_screen_06:
    .byte $3a,$2f,$65,$00,$72,$71,$6f,$19,$4d,$2e,$66,$63,$68,$6e,$6e,$1b
    .byte $44,$26,$69,$65,$63,$64,$14,$21,$45,$26,$6a,$6a,$66,$67,$6d,$4a
    .byte $43,$26,$66,$67,$68,$68,$24,$46,$75,$76,$76,$76,$76,$76,$76,$77
    .byte $20,$14,$14,$3c,$3c,$3c,$14,$21,$1e,$00,$00,$00,$00,$00,$00,$1f

level_4_supertiles_screen_07:
    .byte $83,$73,$70,$65,$67,$6e,$68,$5f,$5e,$68,$68,$66,$2b,$72,$71,$1b
    .byte $18,$71,$01,$00,$00,$01,$74,$1a,$83,$6e,$68,$68,$69,$63,$6e,$84
    .byte $5e,$72,$73,$71,$67,$66,$74,$5f,$17,$73,$73,$70,$00,$00,$72,$1a
    .byte $18,$68,$68,$69,$6f,$73,$73,$1b,$1e,$68,$69,$67,$6e,$68,$68,$1f

level_4_supertiles_screen_08:
    .byte $5e,$68,$68,$69,$15,$15,$15,$61,$22,$15,$15,$6b,$6b,$15,$68,$5f
    .byte $17,$73,$71,$65,$67,$69,$14,$21,$18,$68,$6e,$14,$00,$67,$68,$1d
    .byte $17,$00,$74,$00,$00,$15,$00,$19,$20,$14,$72,$73,$73,$71,$2b,$1b
    .byte $16,$00,$71,$63,$68,$6e,$68,$1a,$1e,$68,$6e,$6a,$69,$74,$00,$1f

level_4_supertiles_screen_09:
    .byte $16,$68,$68,$68,$68,$68,$68,$19,$56,$59,$59,$59,$59,$59,$59,$57
    .byte $17,$73,$71,$63,$69,$63,$68,$1a,$18,$68,$6e,$66,$67,$6a,$68,$1b
    .byte $16,$73,$70,$2b,$2b,$67,$68,$19,$75,$76,$76,$76,$76,$76,$76,$77
    .byte $18,$2b,$3c,$00,$00,$00,$00,$1b,$85,$00,$00,$00,$00,$00,$00,$1f

level_4_supertiles_screen_0a:
    .byte $16,$00,$00,$00,$00,$00,$00,$19,$54,$58,$58,$58,$58,$58,$58,$55
    .byte $17,$68,$69,$00,$00,$00,$00,$1a,$18,$01,$65,$00,$00,$00,$01,$1b
    .byte $16,$00,$67,$68,$68,$68,$68,$19,$54,$58,$58,$58,$58,$58,$58,$55
    .byte $17,$68,$68,$68,$68,$68,$68,$1a,$54,$58,$58,$58,$58,$58,$58,$55

level_4_supertiles_screen_0b:
    .byte $16,$73,$71,$2b,$00,$63,$68,$19,$18,$68,$6e,$68,$69,$65,$63,$1b
    .byte $17,$71,$70,$63,$6a,$6a,$66,$1a,$18,$6e,$68,$6a,$6a,$66,$2b,$1b
    .byte $16,$70,$00,$65,$67,$69,$00,$19,$18,$68,$68,$66,$00,$67,$68,$1b
    .byte $54,$58,$58,$58,$58,$58,$58,$55,$54,$58,$58,$58,$58,$58,$58,$55

level_4_supertiles_screen_0c:
    .byte $3a,$2f,$2f,$2f,$2f,$2f,$00,$19,$60,$15,$15,$15,$15,$15,$00,$1b
    .byte $5a,$3c,$3c,$00,$15,$15,$15,$23,$3a,$2f,$2f,$2f,$00,$15,$2f,$3b
    .byte $22,$15,$15,$15,$00,$00,$15,$61,$20,$14,$14,$14,$14,$3c,$3c,$5b
    .byte $1c,$00,$00,$00,$00,$00,$00,$1d,$1e,$00,$00,$00,$00,$00,$00,$1f

level_4_supertiles_screen_0d:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$17,$00,$00,$00,$00,$00,$00,$1a
    .byte $18,$00,$00,$00,$00,$00,$00,$1b,$1e,$00,$00,$00,$00,$00,$00,$1f

level_4_supertiles_screen_0e:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $90,$91,$92,$90,$91,$92,$90,$91,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0

level_4_supertiles_screen_0f:
    .byte $00,$00,$00,$00,$94,$ad,$ad,$ad,$00,$00,$00,$00,$95,$ae,$ae,$ae
    .byte $00,$00,$00,$00,$96,$ac,$ac,$ac,$00,$00,$00,$00,$97,$af,$af,$af
    .byte $00,$00,$00,$00,$9f,$af,$af,$af,$98,$99,$99,$9a,$9b,$ac,$ac,$ac
    .byte $9c,$7b,$9e,$79,$7a,$9c,$9d,$9c,$00,$00,$00,$00,$00,$00,$00,$00

level_4_supertiles_screen_10:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $92,$90,$91,$92,$93,$90,$91,$92,$a0,$a0,$a0,$a0,$a1,$ae,$ae,$ae

level_4_supertiles_screen_11:
    .byte $00,$a5,$a6,$a7,$a8,$a9,$aa,$00,$00,$ab,$a2,$a2,$a2,$a2,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

level_4_supertiles_screen_12:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $90,$91,$92,$90,$91,$92,$90,$91,$a0,$a0,$a0,$a3,$a4,$a0,$a0,$a0

level_4_supertile_data:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $12,$75,$13,$76,$77,$78,$79,$7a,$14,$7b,$15,$7c,$7d,$7e,$7f,$80 ; #$01 - rotating gun
    .byte $81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90 ; #$02 - rotating gun
    .byte $91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0 ; #$03 - rotating gun
    .byte $12,$75,$13,$76,$77,$78                                         ; #$04 - rotating gun
                                                                          ; continues in bank 7

; end of bank
; unused #$0 bytes out of #$2,000 bytes total (100% full)
; unused 0 bytes out of 8,192 bytes total (100% full)
bank_6_unused_space: