; Bank F is the core of the game's programming. Reset, NMI, and IRQ vectors are
; in this bank and is the entry point to the game.  Bank F is always loaded in
; memory unlike other banks, which are memory-mapped and can be swapped out.
; Bank F contains the code for flushing graphics data from the CPU to the PPU,
; bank switching, controller input, score calculation, game routine execution,
; level transition logic, showing demo (attract mode), calculating bullet angles
; and speeds, and the assembly date, among other things.

; last 8 KiB PRG ROM bank fixed in memory at $e000-$ffff
.segment "BANKF"

.include "constants.asm"

; import from bank 0
.import run_player_state_routines
.import write_text_to_mem
.import handle_player_bullets
.import cp_bullet_to_sprite_buffers
.import create_f_child_flames_or_destroy
.import enemy_gen_routine
.import write_graphic_data_to_ppu
.import init_level_vars
.import set_level_scroll_screen
.import run_demo_input

; import from bank 1
.import run_end_level_anim_routine
.import extra_lives_cheat_input_check
.import level_select
.import run_intro_animation_routine
.import run_scroll_credits_routine
.import level_2_supertiles_screen_ptr_table
.import level_2_supertile_data
.import level_2_palette_data
.import level_2_screen_layout_tbl

; import from bank 3
.import exe_enemy_routines
.import copy_enemy_vars_to_zp

; import from bank 4
.import run_sound_menu_routine
.import lvl_1_create_intro_heli
.import create_screen_enemies
.import level_1_supertiles_screen_ptr_table
.import level_1_enemy_screen_ptr_tbl
.import level_2_enemy_screen_ptr_tbl
.import level_3_enemy_screen_ptr_tbl
.import level_4_enemy_screen_ptr_tbl
.import level_5_enemy_screen_ptr_tbl
.import level_6_enemy_screen_ptr_tbl
.import level_7_enemy_screen_ptr_tbl
.import level_8_enemy_screen_ptr_tbl
.import level_1_screen_layout_tbl

; import from bank 5
.import level_1_supertile_data
.import level_1_palette_data

; import from bank 6
.import load_sprites_to_oam_buffer
.import level_4_supertiles_screen_ptr_table
.import level_4_supertile_data
.import level_4_screen_layout_tbl
.import level_1_run_tile_routines
.import level_2_run_tile_routines
.import level_3_run_tile_routines
.import level_4_run_tile_routines
.import level_5_run_tile_routines
.import level_6_run_tile_routines
.import level_7_run_tile_routines
.import level_8_run_tile_routines

; import from bank 7
.import level_3_supertiles_screen_ptr_table
.import set_palettes
.import level_3_supertile_data
.import level_3_palette_data
.import level_4_palette_data
.import level_3_screen_layout_tbl
.import set_palettes_for_current_level

; import from bank 8
.import level_5_supertiles_screen_ptr_table
.import level_5_screen_layout_tbl

; import from bank 9
.import level_6_supertiles_screen_ptr_table
.import level_5_supertile_data
.import level_6_supertile_data
.import level_5_palette_data
.import level_6_palette_data
.import level_6_screen_layout_tbl

; import from bank a
.import level_7_supertiles_screen_ptr_table
.import level_7_supertile_data
.import level_7_screen_layout_tbl

; import from bank b
.import level_8_supertiles_screen_ptr_table
.import level_8_supertile_data
.import level_7_palette_data
.import level_8_palette_data
.import level_8_screen_layout_tbl

; import from bank c
.import init_sound_code_vars
.import init_pulse_and_noise_channels
.import handle_sound_slots

; export for bank 0
.export two_byte_add
.export play_game_over_sound
.export kill_player_x
.export kill_player
.export clear_bullet_sprite
.export enable_nmi_for_vblank
.export disable_nmi_set_ppumask
.export create_enemy_slot_0_to_3

; export for bank 1
.export increment_game_routine
.export set_intro_screen_bg_sprite
.export clear_mid_high_ram
.export write_palette_to_graphics_buffer
.export set_menu_pattern_tiles

; export for bank 3
.export init_player_for_level_a
.export add_single_bcd_to_score
.export add_score_to_player
.export clear_enemy_type
.export load_sound_banks_init_channels
.export load_banks_update_supertile_and_palette_if_room
.export bg_collision_test_no_incline_tbl
.export get_rotate_00
.export rotate_aim_dir_00_zp
.export calc_velocities_for_dir
.export set_enemy_velocity

; export for bank 4
.export level_enemy_screen_ptr_ptr_tbl
.export replace_bullet_with_enemy
.export initialize_enemy
.export clear_y_scroll

; export for bank 6
.export set_y_autoscroll_stop
.export set_palette

; export for bank a
.export load_banks_update_supertile_if_room
.export rotate_aim_dir_01_zp
.export fire_bullet_at_player_1x_speed

; export for bank c
.export load_high_prg_sound_bank
.export load_sound_banks_init_sound

; export for multiple banks
.export run_routine_from_tbl_below                  ; bank 0, bank 2, bank 3, bank 6, bank 8, bank a
.export create_enemy_y                              ; bank 0, bank 3, bank 4
.export get_landing_y_pos                           ; bank 0, bank 3
.export load_banks_create_f_child_flames_or_destroy ; bank 0, bank 3
.export play_sound                                  ; bank 0, bank 2, bank 3, bank 4, bank 8, bank a
.export bg_collision_test_tbl                       ; bank 0, bank 2, bank 3
.export set_enemy_collision_and_type                ; bank 0, bank 4, bank 8
.export write_bg_palette_to_graphics_buffer         ; bank 1, bank 2
.export set_level_palette                           ; bank 2, bank 4, bank 6, bank 8
.export set_nmi_noop_irq                            ; bank 2, bank 4, bank8, bank a
.export get_rotate_01                               ; bank 3, bank 4, bank 8, bank a
.export fire_bullet_at_player                       ; bank 2, bank 3, bank 8, bank a
.export rotate_aim_dir_01                           ; bank 2, bank 3
.export reset_scroll_draw_point                     ; bank 2, bank 8
.export load_alt_collision_code_indices             ; bank 4, bank 6
.export reset_draw_point                            ; bank 6, bank 8, bank a
.export init_irq_scroll                             ; bank 8, bank 4, bank a
.export overhead_quad_aim_dir_01                    ; bank 8, bank a
.export convert_dir_to_overhead_dir                 ; bank 8, bank a
.export fire_near_player                            ; bank 8, bank a
.export dir_to_overhead_dir_tbl                     ; bank 8, bank a
.export load_banks_update_supertile_and_palette     ; bank 3, bank 4
.export create_and_init_enemy                       ; bank 4, bank 8

; all PPUCTRL settings are the same except for vram address increment
; #$28 - add 1 (going across), #$2c - add 32 (going down)
ppuctrl_tbl:
    .byte $28,$2c,$28,$28,$28,$28,$2c

; sets first byte of CPU_GRAPHICS_BUFFER to #$00 so no drawing takes place for frame
; input
;  * x - graphics buffer offset to set
reset_graphics_buffer:
    stx CPU_GRAPHICS_BUFFER
    stx GRAPHICS_BUFFER_OFFSET
    lda PPUCTRL_SETTINGS
    sta PPUCTRL
    rts

; reads the cpu graphics buffer and decodes the data to write to the PPU
; input
;  * y - graphics buffer read offset
write_cpu_graphics_buffer_to_ppu:
    ldx CPU_GRAPHICS_BUFFER,y ; read VRAM offset
    beq reset_graphics_buffer ; branch if nothing to draw, exit
    lda ppuctrl_tbl-1,x       ; load PPUCTRL (either #$28 or #$2c)
    sta PPUCTRL               ; set background, sprite pattern table addresses, vram increment, etc.
    iny                       ; increment graphics buffer read offset
    lda PPUSTATUS             ; clear bit 7 and address latch used by PPUSCROLL and PPUADDR
    lda CPU_GRAPHICS_BUFFER,y
    sta PPUADDR               ; store high byte of PPU address
    iny                       ; increment graphics buffer read offset
    lda CPU_GRAPHICS_BUFFER,y
    sta PPUADDR               ; store low byte of PPU data write address
    iny                       ; increment graphics buffer read offset
    cpx #$06                  ; see if ppuctrl_tbl offset value is one of the last 2 (#$06 or #$07)
    bcs @block_mode           ; branch if "block mode"
                              ; BLOCK_SIZE, BYTES, PPU ADDR PPU ADDR
    cpx #$03                  ; compare ppuctrl_tbl offset offset value is 3
    bne @flush_mode           ; if not "repeat mode", write data directly from graphics buffer to PPUDATA
    ldx CPU_GRAPHICS_BUFFER,y ; "repeat mode", first byte is number of times to write next byte
                              ; read number of times to repeat graphic byte
    iny                       ; increment graphics buffer read offset
    lda CPU_GRAPHICS_BUFFER,y ; read graphics byte
    iny                       ; increment graphics buffer read offset

; writes graphics byte a to the PPU x times
@repeat_mode_loop:
    sta PPUDATA                          ; write graphics data byte
    dex                                  ; decrement loop counter
    bne @repeat_mode_loop                ; loop if still repeating the graphics byte
    beq write_cpu_graphics_buffer_to_ppu ; repeated graphic byte x times
                                         ; go to next group of graphic data

@write_ff_to_ppu:
    lda #$ff

@flush_byte:
    sta PPUDATA

; write to PPU until #$ff is encountered
@flush_mode:
    lda CPU_GRAPHICS_BUFFER,y            ; read next byte to write to PPU
    iny                                  ; increment CPU_GRAPHICS_BUFFER read offset
    cmp #$ff                             ; compare to end of data byte #$ff
    bne @flush_byte                      ; if not #$ff, then write to PPU
    lda CPU_GRAPHICS_BUFFER,y            ; byte was #$ff, see what next graphic byte is
    cmp #$08
    bcs @write_ff_to_ppu                 ; branch if graphic byte is greater than or equal to #$08
                                         ; generally always #$ff
    bcc write_cpu_graphics_buffer_to_ppu ; otherwise, loop back to write the next set of data

; set the PPU write address based on the CPU_GRAPHICS_BUFFER
@next_block:
    sta PPUADDR
    iny                       ; increment graphics buffer read offset
    lda CPU_GRAPHICS_BUFFER,y
    sta PPUADDR
    iny                       ; increment graphics buffer read offset

; reads the next byte from the cpu graphics buffer
; then reads that many graphic bytes from the buffer into ppu
; once finished, if the next byte isn't #$ff, then read the next two bytes as
; a PPU address, and then repeat
@block_mode:
    ldx CPU_GRAPHICS_BUFFER,y ; read number of graphic bytes to read
    iny                       ; increment graphics buffer read offset

@write_block_bytes:
    lda CPU_GRAPHICS_BUFFER,y            ; read graphics byte
    sta PPUDATA                          ; store in PPU
    iny                                  ; increment graphics buffer read offset
    dex                                  ; decrement remaining graphic group size counter
    bne @write_block_bytes               ; loop if more graphics bytes to write
    lda CPU_GRAPHICS_BUFFER,y            ; finished writing graphics block, read next byte
                                         ; from the cpu graphics buffer
    bpl @next_block                      ; if not #$ff, loop to write the next block of graphics
                                         ; at the appropriate PPU address
    iny                                  ; increment graphics buffer read offset
    jmp write_cpu_graphics_buffer_to_ppu ; loop to read next set of graphic data

; handler for scanline interrupts, runs appropriate irq_handler_xx_yy routine
; based on IRQ_ROUTINE and IRQ_PTR_TBL
irq:
    pha                    ; backup the a register
    txa
    pha                    ; backup the x register
    tya
    pha                    ; backup the y register
    sta $e000              ; push y to $e000
                           ; this disables MMC3 interrupts
                           ; and acknowledges current interrupt
    sta $e001              ; enable interrupts
    lda IRQ_ROUTINE        ; load the specific irq routine index, e.g. irq_handler_xx_ptr_tbl
    asl                    ; double since each entry in irq_handler_xx_ptr_tbl is a #$02 byte address
    tay                    ; transfer to offset register
    lda (IRQ_PTR_TBL),y    ; load specific irq routine handler low byte (irq_handler_xx_yy)
    sta IRQ_ROUTINE_ADDR
    iny
    lda (IRQ_PTR_TBL),y    ; load specific irq routine handler high byte (irq_handler_xx_yy)
    sta IRQ_ROUTINE_ADDR+1
    jmp (IRQ_ROUTINE_ADDR) ; jump to irq handler routine, e.g. irq_handler_xx_yy

irq_handler_ptr_tbl:
    .addr irq_handler_00_ptr_tbl ; noop
    .addr irq_handler_01_ptr_tbl ; level 1 fort firestorm boss screen (helicopter)
    .addr irq_handler_02_ptr_tbl ; level 4 elevator
    .addr irq_handler_03_ptr_tbl ; end credits
    .addr irq_handler_04_ptr_tbl ; level 4 laser chandelier
    .addr irq_handler_04_ptr_tbl ; level 4 laser chandelier
    .addr irq_handler_06_ptr_tbl ; level 8 stomping ceiling
    .addr irq_handler_07_ptr_tbl ; level 3 jungle boss screen (fortress wall)
    .addr irq_handler_07_ptr_tbl ; level 3 jungle boss screen (fortress wall)
    .addr irq_handler_09_ptr_tbl ; level 7 headquarters boss (temple of terror)
    .addr irq_handler_09_ptr_tbl ; level 7 headquarters boss (temple of terror)
    .addr irq_handler_0b_ptr_tbl ; level 5 cliff boss (krypto-crustacean)
    .addr irq_handler_0c_ptr_tbl ; level 2 boss screen
    .addr irq_handler_0d_ptr_tbl ; level 8 final boss
    .addr irq_handler_0e_ptr_tbl ; level 6 before first door
    .addr irq_handler_0f_ptr_tbl ; level 6 miniboss (suspicious face) and boss (jagger froid)

irq_handler_00_ptr_tbl:
    .addr remove_registers_from_stack_and_rti

; level 1 fort firestorm boss screen, switches from helicopter view to ground view
irq_handler_01_ptr_tbl:
    .addr irq_handler_01_00 ; sets scroll and tiles so the ground is at the top of the view

; level 4 elevator
irq_handler_02_ptr_tbl:
    .addr irq_handler_02_00 ; top of elevator floor
    .addr irq_handler_02_01 ; bottom of elevator floor
    .addr irq_handler_02_02 ; bottom of black bar under elevator floor

; end credits
irq_handler_03_ptr_tbl:
    .addr irq_handler_03_00 ; scanline #$38 - set next scanline irq, horizontally scroll clouds
    .addr irq_handler_03_01 ; scanline #$59 - horizontally scroll mountains (parallax effect)
    .addr irq_handler_03_02 ; scanline #$7d - clear left pattern table, set PPU address and scroll, set vertical mirroring
    .addr irq_handler_03_03 ; scanline #$85 - restore left pattern table tiles (contains text tiles)
    .addr irq_handler_03_04 ; scanline #$c6 - set left pattern table to all black

; level 4 laser chandelier
irq_handler_04_ptr_tbl:
    .addr irq_handler_04_00 ; scanline #$18 - set next irq, base nametable, X scroll, pattern table tiles, and advance routine
    .addr irq_handler_04_01 ; scanline #$1a - set next irq to #$a8 scanlines later
    .addr irq_handler_04_02 ; scanline #$c6 - set base nametable $2400, clear X scroll, set pattern table tiles, acknowledge interrupt

; level 8 stomping ceiling
irq_handler_06_ptr_tbl:
    .addr irq_handler_06_00

; level 3 jungle boss screen (fortress wall)
irq_handler_07_ptr_tbl:
    .addr irq_handler_07_00 ; update horizontal scroll for earthquake effect, and swap background tiles

; level 7 headquarters boss (temple of terror)
irq_handler_09_ptr_tbl:
    .addr irq_handler_09_00 ; change left top pattern table tiles from #$2c to #$30

; level 5 cliff boss (krypto-crustacean)
irq_handler_0b_ptr_tbl:
    .addr irq_handler_0b_00 ; sets PPU address with scroll to be at $26e0 to show ground, updates left pattern table tiles

; level 2 boss screen
irq_handler_0c_ptr_tbl:
    .addr irq_handler_0c_00 ; sets next scanline interrupt between #$01 and #$08 for fine-grain Y scroll
    .addr irq_handler_0c_01 ; main tank boss region
    .addr irq_handler_0c_02 ; set PPU address to render bottom of screen with alley and 2 walls

; level 8 final boss
irq_handler_0d_ptr_tbl:
    .addr irq_handler_0d_00
    .addr irq_handler_0d_01
    .addr irq_handler_0d_02

; level 6 before first door
irq_handler_0e_ptr_tbl:
    .addr irq_handler_0e_00 ; set left pattern table banks to #$4c and #$52

; level 6 miniboss (suspicious face) and boss (jagger froid)
irq_handler_0f_ptr_tbl:
    .addr irq_handler_0f_00 ; sets left pattern table banks to #$50 and #$52

; level 1 fort firestorm boss screen
; sets scroll and tiles so the ground is at the top of the view
; ground tiles are in center of nametable, helicopter tiles are at bottom and
; wrap around to top
irq_handler_01_00:
    ldy #$01

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    ldx #$22
    lda #$40
    ldy #$00
    stx PPUADDR
    sta PPUADDR                    ; set PPUADDR to $2240
    sty PPUSCROLL                  ; set PPUSCROLL to 0 (horizontal scroll)
    sty PPUSCROLL                  ; set PPUSCROLL to 0 (y scroll)
                                   ; positioning view so that the ground tiles are at the top
    lda #$a8
    sta PPUCTRL                    ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
                                   ; not sure why bit 3 is set, it is ignored for 8x16 sprites
    lda #$38                       ; left top half pattern table bank (level 1 ground tiles)
    ldx #$3a                       ; left bottom half pattern table bank (level 1 ground tiles)
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    jmp acknowledge_irq_rti        ; acknowledge the interrupt, restore registers, and rti

; top of elevator floor - first scanline interrupt at top of elevator floor
; where player stands on.
; * Sets next scanline interrupt based on `SPLIT_SCANLINE_IRQ_2` to allow
;   fine-grain Y scrolling.  Creates a small region of few black scanlines under
;   elevator of varying height to allow nametable alignment.
; * Sets PPU address to $2280 to show elevator floor
; * if ELEVATOR_CHECKPOINT is set, blank out bottom bit of pattern table for
;   hiding rack-mounted turret so it isn't shown over elevator floor
irq_handler_02_00:
    lda SPLIT_SCANLINE_IRQ_2 ; load number of scanlines before the next interrupt is triggered
                             ; the next interrupt will be irq_handler_02_01
                             ; creates a varying height of elevator floor from #$18 to #$1f
                             ; for smooth Y scrolling between nametable rows
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$1e

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    ldx #$22
    lda #$80
    ldy #$00
    stx PPUADDR
    sta PPUADDR               ; set PPU address to $2280
    sty PPUSCROLL             ; clear any horizontal scroll
    sty PPUSCROLL             ; updating scroll during rendering does not update vertical scroll
                              ; because changes to PPU t register will be ignored at the end of the line
                              ; however, the second write does clear the write toggle
                              ; for more information, see split horizontal scroll
                              ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    lda ELEVATOR_CHECKPOINT   ; see if should change pattern table tiles
    beq @adv_irq_routine_exit ; exit if shouldn't change tiles yet
    ldy #$05                  ; R5: Select 1 KiB CHR bank at PPU $1C00-$1FFF
    lda #$42                  ; black out the right pattern table bottom quarter tiles
    sty $8000                 ; removes pattern tiles for rack-mounted turret so it's hidden behind elevator
    sta $8001
    lda BANK_SELECT           ; load last backed-up bank select command
    sta $8000                 ; set bank select command

@adv_irq_routine_exit:
    jmp adv_irq_routine_rti

; bottom of elevator floor
irq_handler_02_01:
    lda SPLIT_SCANLINE_IRQ_3 ; load next scanline interrupt
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$14

@delay_loop:
    dey
    bne @delay_loop
    jsr clear_left_pattern_tbl              ; set left pattern table to all black tiles
    lda PPUSTATUS
    ldy #$00
    ldx IRQ_HANDLER_PPUADDR+1
    lda IRQ_HANDLER_PPUADDR
    stx PPUADDR
    sta PPUADDR
    sty PPUSCROLL                           ; clear any horizontal scroll
    sty PPUSCROLL                           ; updating scroll during rendering does not update vertical scroll
                                            ; because changes to PPU t register will be ignored at the end of the line
                                            ; however, the second write does clear the write toggle
                                            ; for more information, see split horizontal scroll
                                            ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    inc IRQ_ROUTINE
    lda SPLIT_SCANLINE_IRQ_3
    beq irq_handler_02_restore_tiles        ; restore the right 1/4th tiles and all of the left pattern table tiles
    jmp remove_registers_from_stack_and_rti

; bottom of black bar under elevator floor
irq_handler_02_02:
    ldy #$0f

@delay_loop:
    dey
    bne @delay_loop

; restore the right 1/4th tiles and all of the left pattern table tiles
irq_handler_02_restore_tiles:
    ldy #$05                       ; R5: Select 1 KiB CHR bank at PPU $1C00-$1FFF
    lda RIGHT_FOURTH_QTR_CHR_BANK  ; load bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    sty $8000
    sta $8001
    ldy #$00
    lda LEFT_TOP_HALF_CHR_BANK     ; left top half pattern table bank
    ldx LEFT_BOTTOM_CHR_HALF_BANK  ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    jmp acknowledge_irq_rti        ; acknowledge the interrupt, restore registers, and rti

; end credits - scanline #$38 - set next scanline irq, horizontally scroll clouds
irq_handler_03_00:
    lda #$20
    sta $c000 ; set number of scanlines until trigger scanline IRQ
    ldy #$08

@delay_loop:
    dey
    bne @delay_loop         ; delay 8 iterations of loop
    lda NT_ROW_SCROLL       ; load horizontal scroll of clouds part
    sta PPUSCROLL           ; set horizontal scroll
    sta PPUSCROLL           ; updating scroll during rendering does not update vertical scroll
                            ; because changes to PPU t register will be ignored at the end of the line
                            ; however, the second write does clear the write toggle
                            ; for more information, see split horizontal scroll
                            ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    jmp adv_irq_routine_rti

; end credits - scanline #$59 - horizontally scroll mountains (parallax effect)
irq_handler_03_01:
    lda SPLIT_SCANLINE_IRQ_2 ; about #$25 scanlines
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$08

@delay_loop:
    dey
    bne @delay_loop
    lda LEVEL_Y_SCREEN
    sta PPUSCROLL           ; set horizontal scroll
    sta PPUSCROLL           ; updating scroll during rendering does not update vertical scroll
                            ; because changes to PPU t register will be ignored at the end of the line
                            ; however, the second write does clear the write toggle
                            ; for more information, see split horizontal scroll
                            ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    jmp adv_irq_routine_rti

; end credits - scanline #$7d - clear left pattern table, set PPU address and scroll, set vertical mirroring
irq_handler_03_02:
    lda SPLIT_SCANLINE_IRQ_3   ; about #$08 scanlines
    sta $c000                  ; set number of scanlines until trigger scanline IRQ
    jsr clear_left_pattern_tbl ; set left pattern table to all black tiles
    ldy #$14

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    ldx IRQ_HANDLER_PPUADDR+1
    lda IRQ_HANDLER_PPUADDR
    ldy SPLIT_X_SCROLL
    stx PPUADDR
    sta PPUADDR
    sty PPUSCROLL
    sty PPUSCROLL             ; updating scroll during rendering does not update vertical scroll
                              ; because changes to PPU t register will be ignored at the end of the line
                              ; however, the second write does clear the write toggle
                              ; for more information, see split horizontal scroll
                              ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    lda #$00
    sta $a000                 ; set vertical mirroring (horizontal arrangement)
                              ; A B
                              ; A B
    jmp adv_irq_routine_rti

; end credits - scanline #$85 - restore left pattern table tiles (contains text tiles)
irq_handler_03_03:
    lda #$40  ; #$40 scanlines until irq_handler_03_04
    sta $c000 ; set number of scanlines until trigger scanline IRQ
    ldy #$02

@delay_loop:
    dey
    bne @delay_loop
    lda LEFT_TOP_HALF_CHR_BANK     ; left top half pattern table bank
    ldx LEFT_BOTTOM_CHR_HALF_BANK  ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    jmp adv_irq_routine_rti

; scanline #$c6 - set left pattern table to all black
irq_handler_03_04:
    ldy #$08

@delay_loop:
    dey
    bne @delay_loop
    jsr clear_left_pattern_tbl ; set left pattern table to all black tiles
    jmp acknowledge_irq_rti    ; acknowledge the interrupt, restore registers, and rti

; level 4 laser chandelier
; set next irq, base nametable, X scroll, pattern table tiles, and advance routine
irq_handler_04_00:
    lda SPLIT_SCANLINE_IRQ_2 ; load number of scanlines until next IRQ (always #$01)
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$09

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    lda SPLIT_PPUCTRL              ; load PPU control to use after current scanline
    sta PPUCTRL                    ; changes base nametable (either $2000 (top left), or $2400 (top right))
    lda SPLIT_X_SCROLL
    sta PPUSCROLL
    sta PPUSCROLL                  ; updating scroll during rendering does not update vertical scroll
                                   ; because changes to PPU t register will be ignored at the end of the line
                                   ; however, the second write does clear the write toggle
                                   ; for more information, see split horizontal scroll
                                   ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    lda #$08                       ; left top half pattern table bank
    ldx #$14                       ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks

adv_irq_routine_rti:
    inc IRQ_ROUTINE
    jmp remove_registers_from_stack_and_rti

; level 4 laser chandelier
; set next irq to #$a8 scanlines later
irq_handler_04_01:
    lda SPLIT_SCANLINE_IRQ_3 ; always #$ab
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    jmp adv_irq_routine_rti

; level 4 laser chandelier - set PPU to draw floor bg tiles
; set base nametable $2400, clear X scroll, set pattern table tiles, acknowledge interrupt
irq_handler_04_02:
    ldy #$09

@delay_loop:
    dey
    bne @delay_loop
    lda PPUCTRL_SETTINGS
    sta PPUCTRL                    ; set original base nametable
    lda #$00
    sta PPUSCROLL
    sta PPUSCROLL                  ; updating scroll during rendering does not update vertical scroll
                                   ; because changes to PPU t register will be ignored at the end of the line
                                   ; however, the second write does clear the write toggle
                                   ; for more information, see split horizontal scroll
                                   ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    lda #$08                       ; left top half pattern table bank
    ldx #$0a                       ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks

; acknowledge the interrupt, restore registers, and rti
acknowledge_irq_rti:
    sta $e000                               ; disable/acknowledge the IRQ
    jmp remove_registers_from_stack_and_rti

; level 8 stomping ceiling - set PPUADDR to show ground portion for screen
; renders after stomping ceiling has rendered triggered at ~72% of screen
; always sets PPUADDR to same position plus horizontal scroll to show ground
irq_handler_06_00:
    ldy #$09

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS             ; prepare PPU registers for writing PPUADDR (clear w latch)
    ldx IRQ_HANDLER_PPUADDR+1 ; split values loaded from init_scanline_irq
    lda IRQ_HANDLER_PPUADDR   ; from IRQ_* variables
    ldy SPLIT_X_SCROLL        ; which were set in stomping_ceiling_routine_01
    stx PPUADDR
    sta PPUADDR               ; either $22e0 or $2ee0
    sty PPUSCROLL
    sty PPUSCROLL             ; updating scroll during rendering does not update vertical scroll
                              ; because changes to PPU t register will be ignored at the end of the line
                              ; however, the second write does clear the write toggle
                              ; for more information, see split horizontal scroll
                              ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    jmp acknowledge_irq_rti   ; acknowledge the interrupt, restore registers, and rti

; level 3 jungle boss screen (fortress wall) - updates horizontal scroll for earthquake effect, and swaps background tiles
irq_handler_07_00:
    ldy #$04

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS           ; prepare PPU registers for writing PPUADDR (clear w latch)
    ldx #$22
    lda #$80
    ldy SPLIT_X_SCROLL
    stx PPUADDR
    sta PPUADDR
    sty PPUSCROLL           ; update horizontal scroll to match pre irq scroll (for earthquake effect)
    sty PPUSCROLL           ; updating scroll during rendering does not update vertical scroll
                            ; because changes to PPU t register will be ignored at the end of the line
                            ; however, the second write does clear the write toggle
                            ; for more information, see split horizontal scroll
                            ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    ldy #$00
    lda #$12
    ldx #$10
    sty $8000               ; R0: Select 2 KiB CHR bank at PPU $0000-$07FF (left top half chr bank)
    sta $8001               ; set left top half character bank to bank #$12
    iny
    sty $8000               ; R1: Select 2 KiB CHR bank at PPU $0800-$0FFF (left bottom half chr bank)
    stx $8001               ; set left bottom half character bank to bank #$10
    lda BANK_SELECT         ; load last backed-up bank select command
    sta $8000               ; set bank select command
    jmp acknowledge_irq_rti ; acknowledge the interrupt, restore registers, and rti

; change left top pattern table tiles from #$2c to #$30
; bottom left tiles are always #$2e
irq_handler_09_00:
    ldy #$0d

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    lda #$30                       ; left top half pattern table bank
    ldx #$2e                       ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    jmp acknowledge_irq_rti        ; acknowledge the interrupt, restore registers, and rti

; sets PPU address with scroll to be at $26e0 to show ground, updates left pattern table tiles
irq_handler_0b_00:
    ldy #$07

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    ldx #$2e
    lda #$20
    ldy #$00
    stx PPUADDR
    sta PPUADDR                    ; set PPU address to $202e
    sty PPUSCROLL                  ; clear any horizontal scroll
    sty PPUSCROLL                  ; updating scroll during rendering does not update vertical scroll
                                   ; because changes to PPU t register will be ignored at the end of the line
                                   ; however, the second write does clear the write toggle
                                   ; for more information, see split horizontal scroll
                                   ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    lda #$20                       ; left top half pattern table bank
    ldx #$22                       ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    jmp acknowledge_irq_rti        ; acknowledge the interrupt, restore registers, and rti

; level 2 boss screen - first scanline interrupt after top of screen has been
; rendered.  The top of the screen is the exit alley and the two walls.
; * Sets next scanline interrupt based on `SPLIT_SCANLINE_IRQ_2` to allow
; fine-grain Y scrolling.  Creates a small region of few scanlines (ranges from
; #$01 to #$08).
; * Sets PPU address to create tank boss vertical movement
;   $2c80, $2c60, $2c40, $2c20 (as tank goes down toward bottom)
irq_handler_0c_00:
    lda SPLIT_SCANLINE_IRQ_2 ; load number of scanlines before the next interrupt is triggered
                             ; the next interrupt will be irq_handler_0c_01
                             ; creates a varying height from #$08 to #$01
                             ; for smooth Y scrolling between nametable rows
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$06

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    ldx IRQ_HANDLER_PPUADDR+1
    lda IRQ_HANDLER_PPUADDR   ; load PPU address appropriate for tank boss
    ldy SPLIT_X_SCROLL        ; load horizontal scroll to use
    stx PPUADDR
    sta PPUADDR               ; set new PPU address for the next #$08 scanlines to $2c40
    sty PPUSCROLL             ; set horizontal scroll for use
    sty PPUSCROLL             ; updating scroll during rendering does not update vertical scroll
                              ; because changes to PPU t register will be ignored at the end of the line
                              ; however, the second write does clear the write toggle
                              ; for more information, see split horizontal scroll
                              ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    jmp adv_irq_routine_rti

; level 2 boss screen - main tank boss region
; sets next interrupt to occur after #$97 scanlines
; this interrupt is is always triggered on scanline #$29
irq_handler_0c_01:
    lda SPLIT_SCANLINE_IRQ_3 ; load main screen area number of scanlines
                             ; this area contains the tank boss
                             ; always #$97 scanlines
    sta $c000                ; set #$98 scanlines until trigger scanline IRQ
    ldy #$02

@delay_loop:
    dey
    bne @delay_loop
    jmp adv_irq_routine_rti

; level 2 boss screen
; set PPU address to render bottom of screen with alley and 2 walls
irq_handler_0c_02:
    ldy #$08

@delay_loop:
    dey
    bne @delay_loop
    lda PPUSTATUS
    ldx #$23
    lda #$00
    stx PPUADDR
    sta PPUADDR             ; set PPU address to $2300
    sta PPUSCROLL           ; clears any horizontal scroll
    sta PPUSCROLL           ; updating scroll during rendering does not update vertical scroll
                            ; because changes to PPU t register will be ignored at the end of the line
                            ; however, the second write does clear the write toggle
                            ; for more information, see split horizontal scroll
                            ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    jmp acknowledge_irq_rti ; acknowledge the interrupt, restore registers, and rti

; final boss - first scanline interrupt after stalactites have been rendered
; area gets smaller as boss rises up
; * Swaps out the pattern table bank (left pattern table) to #$70 and #$72
; * Sets PPU address to $2cc0
; * Sets next scanline interrupt to `SPLIT_SCANLINE_IRQ_2`.  This sets the
; interrupt to occur in #$05 scanlines.
irq_handler_0d_00:
    lda SPLIT_SCANLINE_IRQ_2
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$09

@delay_loop:
    dey
    bne @delay_loop
    lda #$70                       ; left top half pattern table bank
    ldx #$72                       ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    lda PPUSTATUS
    ldx IRQ_HANDLER_PPUADDR+1
    lda IRQ_HANDLER_PPUADDR
    ldy SPLIT_X_SCROLL             ; always #$00 for final boss
    stx PPUADDR
    sta PPUADDR                    ; set new PPU address for the next #$05 scanlines to $2cc0
    sty PPUSCROLL
    sty PPUSCROLL                  ; set X scroll to #$00
    jmp adv_irq_routine_rti

; final boss - set next scanline interrupt based on how much of the boss is
; visible as it rises.  Between this IRQ and the next IRQ is the boss bg
irq_handler_0d_01:
    lda SPLIT_SCANLINE_IRQ_3 ; load how many scanlines until next scanline IRQ
                             ; gets larger and larger as the boss is rising
    sta $c000                ; set number of scanlines until trigger scanline IRQ
    ldy #$03

@delay_loop:
    dey
    bne @delay_loop
    jmp adv_irq_routine_rti

; final boss - interrupt handler after boss is shown, always scanline #$c1.
; Show the ground
irq_handler_0d_02:
    ldy #$07

@delay_loop:
    dey
    bne @delay_loop
    lda LEFT_TOP_HALF_CHR_BANK     ; left top half pattern table bank
    ldx LEFT_BOTTOM_CHR_HALF_BANK  ; left bottom half pattern table bank
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    ldy #$0e

@delay_loop2:
    dey
    bne @delay_loop2
    lda PPUSTATUS
    ldx #$23
    lda #$00
    ldy X_SCROLL            ; load PPU horizontal scroll
    stx PPUADDR
    sta PPUADDR             ; set PPU address to $2300
    sty PPUSCROLL
    sty PPUSCROLL           ; updating scroll during rendering does not update vertical scroll
                            ; because changes to PPU t register will be ignored at the end of the line
                            ; however, the second write does clear the write toggle
                            ; for more information, see split horizontal scroll
                            ; https://www.nesdev.org/wiki/PPU_scrolling#Split_X_scroll
    jmp acknowledge_irq_rti ; acknowledge the interrupt, restore registers, and rti

; sets left pattern table banks to #$4c and #$52
; allows the area above the scanline to have the tiles for showing the gray
; alien wall (#50 and #52), and the area below the scanline to have the tiles
; to show the baby alien ladybug generator skulls (#$4c and #$52)
irq_handler_0e_00:
    lda #$4c                 ; left top half pattern table bank
    ldx #$52                 ; left bottom half pattern table bank
    bne set_left_chr_tbl_rti

; sets left pattern table banks to #$50 and #$52
; allows the area above the scanline to have the tiles for showing the miniboss
; (#$54 and #$56) or boss (#$54 and #$58), and the area below the scanline to
; have the tiles to show the gray alien wall (#50 and #52)
irq_handler_0f_00:
    lda #$50 ; left top half pattern table bank
    ldx #$52 ; left bottom half pattern table bank

; input
;  * a - 2 KiB left top half character bank
;  * x - 2 KiB left bottom half character bank
set_left_chr_tbl_rti:
    ldy #$05

@delay_loop:
    dey
    bne @delay_loop
    jsr set_left_pattern_tbl_banks ; set the left ($0000-$0fff) pattern table tile banks
    jmp acknowledge_irq_rti        ; acknowledge the interrupt, restore registers, and rti

exe_game_routine:
    inc FRAME_COUNTER      ; increment frame counter
    lda GAME_ROUTINE_INDEX ; index into game_routine_pointer_table
    cmp #$04               ; see if game_routine_04
    bne run_game_routine   ; run game routine from table based on index if not game_routine_04
    jmp game_routine_04    ; !(HUH), would run game_routine_04 anyway

run_game_routine:
    jsr run_routine_from_tbl_below

game_routine_pointer_table:
    .addr game_routine_00 ; animate intro, read controller for cheat code, sound menu, start game, player select
    .addr game_routine_01 ; initialize and run demo, checking for controller start/select interrupt
    .addr game_routine_02 ; player mode selected, initialize level 1, advance game routine
    .addr game_routine_03 ; set level_routine_00, advance routine
    .addr game_routine_04 ; play the level
    .addr game_routine_05 ; hidden sound menu ui

; animate intro, read controller for cheat code, sound menu, start game, player select
game_routine_00:
    ldx INTRO_SCREEN_STATE               ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    bne @continue
    jsr show_intro_menu
    lda #$09
    jsr load_banks_set_palette           ; load the palette colors for intro screen
    jsr write_palette_to_graphics_buffer ; write bg and sprite palette colors from palette buffer to graphics buffer
    inc INTRO_SCREEN_STATE               ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection

@exit:
    rts

@continue:
    dex                                          ; decrement INTRO_SCREEN_STATE
    bne show_intro_handle_state                  ; branch if showing intro (player select) screen, but not made a player selection
                                                 ; to either flash player selection, handle input for intro (player select) screen
    jsr load_banks_extra_lives_cheat_input_check ; still animating intro, check for 10 extra lives cheat
    jsr load_banks_run_intro_animation_routine   ; animate reveal of logo and showing of the player selection and license text
    bcs show_intro_reset_delay                   ; animation complete, show the intro (player select) screen
    lda CONTROLLER_STATE_DIFF_B                  ; load which buttons are pressed on the controller
    and #$30                                     ; see if start or select button is pressed
    beq @exit                                    ; exit if neither start nor select is pressed

; player interrupted intro animation, show the intro (player select) screen
show_intro_menu_init_vars:
    jsr show_intro_menu ; start pressed, cancel intro animation and show intro menu

; show the intro (player select) screen, and reset the delay timer
show_intro_reset_delay:
    jsr show_intro_screen
    jsr reset_delay_timer
    lda #$00
    sta GAME_ROUTINE_INDEX
    lda #$02               ; set flag specifying that the player select is visible
    sta INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    rts

; flash player selection if selection made
; otherwise, handle input for intro (player select) screen
; that is check for and handle extra lives cheat, player select change, show sound menu input, or player selection
show_intro_handle_state:
    dex                                          ; decrement already decremented INTRO_SCREEN_STATE value
    bne flash_player_select_check_delay          ; branch if selected player count
                                                 ; to flash the "1 PLAYER"/"2 PLAYER" selection until timer elapses, then advance game routine
    jsr load_banks_extra_lives_cheat_input_check ; haven't made player selection
                                                 ; check for 10 extra lives cheat
    jsr decrement_delay_timer                    ; decrement intro (player select) screen delay timer
    bne @check_input                             ; branch if start demo delay timer hasn't elapsed
    jmp increment_game_routine                   ; timer elapsed, go to demo (GAME_ROUTINE_INDEX = 1)

; demo delay timer hasn't elapsed, see if player has pressed start or select
@check_input:
    ldx PLAYER_MODE                  ; load number of players (0 = 1 player)
    lda #$a7                         ; load Y position of cursor for player select
    ldy player_select_cursor_x_pos,x ; load X position of cursor for player select based on player mode
    jsr set_cursor_sprite            ; draw player select sprite (sprite_b9) at position (y,a) with palette #$01
    lda CONTROLLER_STATE_DIFF_B      ; load which buttons are pressed on the controller
    and #$20                         ; see if select button is pressed
    beq @check_for_start             ; branch if select button wasn't pressed
    lda PLAYER_MODE                  ; player mode change, swap player mode from 1 PLAYER (#$00) to 2 PLAYER (#$01) or vice versa
    eor #$01                         ; swap player mode
    sta PLAYER_MODE                  ; set new player mode (00 = single player, 01 = 2 player)
    jsr reset_delay_timer

@check_for_start:
    lda CONTROLLER_STATE_DIFF_B
    and #$10                    ; see if start button is pressed
    beq intro_input_exit        ; exit if start button isn't pressed
    lda CTRL_KNOWN_GOOD         ; start button pressed, check if sound menu hidden menu input is being pressed
    and #$c0                    ; strip to just A and B inputs
    cmp #$c0                    ; see if A and B button are both pressed
    beq show_sound_menu         ; start button, A, and B are pressed, show sound menu
    lda #$80                    ; start button pressed by itself, starting a game
    sta DELAY_TIME_LOW_BYTE
    inc INTRO_SCREEN_STATE      ; move INTRO_SCREEN_STATE to #$03 (made intro player selection)
                                ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection

intro_input_exit:
    rts

show_sound_menu:
    lda #$05
    jmp set_game_routine_index_to_a ; set routine to game_routine_05

flash_player_select_check_delay:
    lda DELAY_TIME_LOW_BYTE
    and #$08                          ; keep bit 3, used to flash every #$08 frames
    asl
    asl
    asl
    asl                               ; pushing bit 3 to bit 7
                                      ; when set signifies write blank/space instead of text character
                                      ; used for flashing effect every #$08 frames
    adc PLAYER_MODE                   ; add 1 if 2 player game
                                      ; text index for "1 PLAYER" or "2 PLAYERS"
    jsr load_bank_0_write_text_to_mem ; store "1 PLAYER" or "2 PLAYERS" in CPU_GRAPHICS_BUFFER
    dec DELAY_TIME_LOW_BYTE
    bne intro_input_exit
    lda #$02
    jmp set_game_routine_index_to_a   ; set routine to game_routine_02

; draws player select sprite (sprite_b9) at position (y,a) with palette #$01
; input
;  * y - sprite X position
;  * a - sprite Y position
set_cursor_sprite:
    sta SPRITE_Y_POS ; store player select cursor sprite Y position
    sty SPRITE_X_POS ; store player select cursor sprite X position
    lda #$b9         ; sprite_b9 (player select cursor)
    sta SPRITES      ; store sprite in sprite buffer
    lda #$01         ; sprite palette 1
    sta SPRITE_ATTR  ; store sprite attribute
    rts

player_select_cursor_x_pos:
    .byte $20,$80

; initialize and run demo, checking for controller start/select interrupt
game_routine_01:
    ldx INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    bne show_demo          ; branch if in demo to run demo and check for controller select or start to break out of demo
    jsr init_demo_vars     ; not yet in demo, initialize variables necessary
    inc INTRO_SCREEN_STATE ; increment state to indicate ready to demo for next frame
    rts

; show demo, check for controller select or start to break out of demo
show_demo:
    lda CONTROLLER_STATE_DIFF
    and #$30
    beq @continue                 ; branch if neither start nor select button is pressed
    jsr adv_demo_level            ; select or start button is pressed, advance to next level, then show intro (player select) screen
    jmp show_intro_menu_init_vars ; !(OBS) can adjust demo level by pressing start and waiting for demo again

@continue:
    jsr run_demo            ; loads banks to press demo input, check if demo level finished, if so go back to show intro screen
                            ; otherwise, run level
    lda DEMO_LEVEL_END_FLAG
    beq intro_input_exit

; advance the current demo level, and restart game
adv_demo_level_restart_game:
    jsr adv_demo_level ; move to next demo level. If demo level is #$02, advance to #$00

set_game_routine_00:
    lda #$00
    beq set_game_routine_index_to_a ; set routine to game_routine_00

; move to next demo level. If demo level is #$02, advance to #$00
adv_demo_level:
    inc DEMO_LEVEL
    lda DEMO_LEVEL
    cmp #$03
    bcc @continue
    lda #$00

@continue:
    sta DEMO_LEVEL
    rts

; player mode selected, initialize level 1, advance game routine
game_routine_02:
    lda #$00
    sta DEMO_MODE          ; set flag indicating not in demo
    ldx INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    bne @next_screen_state
    inc INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    rts

@next_screen_state:
    dex                        ; decrement INTRO_SCREEN_STATE
    bne @level_select          ; !(OBS) unused level select interface that exists in the Japanese version
    jsr clear_mid_high_ram
    jsr init_player_level_1    ; set level 1, initialize player state, number of lives, player count, player mode, etc.
    jmp increment_game_routine

; !(UNUSED) this label brings up the level select interface
; and handles reading the controller d-pad to select the level
; down moves to the next level, up moves to the previous level
; since this isn't used, instead the level title screen's "AREA" and Level
; number, e.g. "AREA 2" is drawn in draw_title_level
@level_select:
    lda #$0c                          ; text index for AREA 1
    jsr load_bank_0_write_text_to_mem ; store "AREA 1" in CPU_GRAPHICS_BUFFER
    jmp load_banks_level_select

; set level_routine_00, advance routine
game_routine_03:
    lda #$00
    sta LEVEL_ROUTINE_INDEX    ; initialize to level_routine_00
    jmp increment_game_routine

; hidden sound menu ui
game_routine_05:
    ldx INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    bne @continue
    lda #$00
    sta SPRITES
    inc INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    lda #$00
    sta CURRENT_LEVEL
    sta GAME_COMPLETED
    rts

@continue:
    jsr load_bank_4_run_sound_menu_routine
    lda CONTROLLER_STATE_DIFF_B
    and #$10
    bne adv_demo_level_restart_game        ; restart game to show animation and regular menu
                                           ; !(HUH) why advance demo level, maybe hidden feature to control the 1st demo level
                                           ; if unintentional, developers could have just branched to set_game_routine_00
    rts

increment_game_routine:
    inc GAME_ROUTINE_INDEX

; called every time the game_routine index is incremented
init_game_routine_flags:
    lda #$00
    sta INTRO_SCREEN_STATE ; 01=scrolling or in demo, 02=showing player select, 03=made intro player selection
    rts

; update GAME_ROUTINE_INDEX to A
set_game_routine_index_to_a:
    sta GAME_ROUTINE_INDEX
    jmp init_game_routine_flags

; decrement delay timer
; zero flag set (checked) when the timer has elapsed, otherwise zero flag will not be set
decrement_delay_timer:
    lda DELAY_TIME_LOW_BYTE  ; load the low byte of the delay timer
    ora DELAY_TIME_HIGH_BYTE ; OR it together with high byte
    beq @exit                ; all bits both high and low byte are #$0, exit with #$0 in a register, zero flag set
    lda DELAY_TIME_LOW_BYTE  ; decrease delay (loops below #$00 to #$ff)
    bne @exit_z_flag_clear   ; low byte isn't #$0, exit with zero flag clear
    dec DELAY_TIME_HIGH_BYTE ; high byte wasn't #$0, subtract 1 from it

@exit_z_flag_clear:
    dec DELAY_TIME_LOW_BYTE
    lda #$01                ; ensures the zero flag is clear

@exit:
    rts

; set or reset delay before demo begins
; only if the intro screen is forced by pressing start/select
; NTSC is about #3c frames per second
; PAL is close to #$32 frames per second
reset_delay_timer:
    lda #$00
    ldy #$01
    sta DELAY_TIME_LOW_BYTE
    sty DELAY_TIME_HIGH_BYTE
    rts

; show intro screen after animation
show_intro_menu:
    jsr load_sound_banks_init_channels       ; load the sound banks (bank c and d), and init pulse and noise channels
    jsr zero_out_nametables                  ; clear all nametables (set pattern tile to #$00)
    ldx #$02
    jsr load_banks_write_graphic_data_to_ppu ; write graphic_data_01 (intro screen nametable tiles) to PPU
    lda #$00
    sta DEMO_MODE                            ; set flag indicating not in demo
    sta CURRENT_LEVEL
    jsr clear_mid_high_ram                   ; clears $38-$e0, $0400-$06ff, $03b0-$03ff, and $0700-$07df
    jsr set_menu_pattern_tiles               ; set the tiles used between levels to show high score, level name, etc
    jsr set_nmi_noop_irq                     ; remove any scanline interrupts
    lda #$1e
    sta PPUMASK_SETTINGS                     ; disable nmi at start of vertical blanking interval
    rts

show_intro_screen:
    lda PPUCTRL_SETTINGS
    and #$fc
    sta PPUCTRL_SETTINGS
    lda #$08
    jsr load_banks_set_palette           ; load the palette colors for flashing intro screen
    jsr write_palette_to_graphics_buffer ; write bg and sprite palette colors from palette buffer to graphics buffer
    lda #$00                             ; text index for "1 PLAYER"
    jsr load_bank_0_write_text_to_mem    ; store "1 PLAYER" in CPU_GRAPHICS_BUFFER
    lda #$01                             ; text index for "2 PLAYERS"
    jsr load_bank_0_write_text_to_mem    ; store "2 PLAYERS" in CPU_GRAPHICS_BUFFER

; sets the large sprite for the intro screen
set_intro_screen_bg_sprite:
    lda #$b8           ; sprite_b8
    sta SPRITES+1      ; set super c intro screen background sprite
    lda #$00
    sta SPRITE_ATTR+1  ; set sprite attribute for background tile
    lda #$47
    sta SPRITE_Y_POS+1 ; set Y position of sprite
    lda #$88
    sta SPRITE_X_POS+1 ; set X position of sprite
    rts

zero_out_nametables:
    ldx #$00
    jmp load_banks_write_graphic_data_to_ppu ; zero out nametables (write blank_nametables to PPU)

init_demo_vars:
    jsr clear_mid_high_ram
    lda #$01
    sta DEMO_MODE                 ; set flag indicating in demo
    sta PLAYER_COUNT              ; 0 = 1 player game, 1 = 2 player game
    lda #$01
    sta PLAYER_STATE
    sta PLAYER_STATE+1
    lda #$00
    sta PLAYER_GAME_OVER_STATUS   ; clear p1 game over status (not in game over)
    sta PLAYER_GAME_OVER_STATUS+1 ; clear p2 game over status (not in game over)
    sta FRAME_COUNTER
    sta GLOBAL_TIMER
    sta RANDOM_NUM                ; seed random number
    lda #$10
    sta PLAYER_NUM_LIVES
    sta PLAYER_2_NUM_LIVES
    lda DEMO_LEVEL
    sta CURRENT_LEVEL
    rts

; set level 1, initialize player state, number of lives, player count, player mode, etc.
init_player_level_1:
    lda #$00

; initialize level a, player state, number of lives, player count, player mode, etc.
; input
;  * a - level number
init_player_for_level_a:
    sta CURRENT_LEVEL
    lda #$00
    sta PLAYER_STATE
    sta PLAYER_STATE+1
    lda #$02
    sta NUM_CONTINUES
    lda PLAYER_MODE
    sta PLAYER_COUNT   ; 0 = 1 player game, 1 = 2 player game

init_player_vars:
    ldx PLAYER_COUNT ; 0 = 1 player game, 1 = 2 player game

@player_loop:
    lda #$01
    sta PLAYER_STATE,x
    lda #$00
    sta PLAYER_GAME_OVER_STATUS,x      ; load player game over status (0 = not game over, 1 = game over)
    sta NEW_LIFE_INVINCIBILITY_TIMER,x
    sta PLAYER_CURRENT_WEAPON,x
    lda #$02                           ; default to 3 lives
    ldy CHEAT_CODE_STATUS              ; check if 10 lives cheat entered
    beq @continue                      ; branch if not entered
    lda #$09                           ; 10 lives cheat entered, set number of lives to 10

; sets initial score to get an extra life and clears both player scores
@continue:
    sta PLAYER_NUM_LIVES,x    ; set player number of lives
    lda #$30
    sta NEXT_1_UP_SCORE_MID,x
    lda #$00
    sta NEXT_1_UP_SCORE_HI,x  ; sets initial score to obtain to get an extra life to 0030000
    dex                       ; decrement player index
    bpl @player_loop          ; branch if need to set player 1
    lda #$00
    ldy #$05                  ; the players' score is stored in 6 bytes

@clear_score_byte_loop:
    sta P1_SCORE,y             ; clear player score
    dey
    bpl @clear_score_byte_loop
    rts

; clears $5b-$9f, $0400-$06ff, $03b0-$03ff, and $0700-$07df
clear_memory:
    ldx #$5b
    lda #$00

@clear_5b_to_9f:
    sta $00,x
    inx
    cpx #$9f
    bne @clear_5b_to_9f
    beq clear_high_ram

; clears either $38-$e0 or $50-$e0 based on zero flag
; also clears $0400-$06ff, $03b0-$03ff, and $0700-$07df
; input
;  * zero flag - set to clear $38-$e0, clear to clear $50-$e0
clear_mid_high_ram:
    ldx #$38
    bne @continue
    ldx #$50

@continue:
    lda #$00

; clear $38-$e0 or $50-$e0
; input
;  * x - starting clearing address
clear_38_to_e0:
    sta $00,x
    inx
    cpx #$e0
    bne clear_38_to_e0

; clears $0400-$06ff, $03b0-$03ff, and $0700-$07df
clear_high_ram:
    ldx #$00

@set_0400_to_06ff:
    sta BG_COLLISION_DATA,x
    sta SPRITES,x
    sta $0600,x
    inx
    bne @set_0400_to_06ff
    ldx #$b0

; clears $03b0-$03ff
@clear_03b0_to_03ff:
    sta CPU_GRAPHICS_BUFFER,x
    inx
    bne @clear_03b0_to_03ff
    ldx #$e0

; clears $0700-$07df
@clear_0700_07df:
    dex
    sta $0700,x
    bne @clear_0700_07df
    rts

; clears [$500-$568]
; clears player sprites (including bullets), enemy sprites, sprite locations, and sprite attributes
clear_sprites:
    ldy #$68
    lda #$00

@loop:
    dey
    sta SPRITES,y
    bne @loop
    rts

; execute the code at offset A from the pointer table underneath the jsr opcode that called this method
; this is done by reading with offset from the value of the stack before this call and adding 1
; which effectively allows this method to read from the pointer table below the calling code
; input
;  * a - offset from pointer table
run_routine_from_tbl_below:
    asl         ; double A since each entry is a 2-byte label address
    sty $03     ; store y into $03 temporarily since this method overrides y
    tay         ; store offset into y
    iny         ; add one since the stack pointer points to one byte before table to offset into
    pla         ; pull the low byte of the stack pointer into a
    sta $00     ; store low byte of stack pointer address into $00
    pla         ; pull the high byte of the stack pointer memory address into a
    sta $01     ; store high byte into $01
    lda ($00),y ; read low byte of address of code to execute (offset into table)
    sta $02     ; store the low byte into $02
    iny         ; increment offset so high byte can be read
    lda ($00),y ; read the high byte of the address of the code to execute (offset into table)
    ldy $03     ; restore y register to what it was before the call to run_routine_from_tbl_below
    sta $03     ; store high byte into $03
    jmp ($0002) ; jump to the code specified by the address at offset A into the pointer table table

; adds a register to value at $00,x, if any overflow, then increments $01,x
; input
;  * a - amount to add to value in $00,x
;  * x - offset from $00 where value is stored
two_byte_add:
    clc       ; clear carry in preparation for addition
    adc $00,x
    sta $00,x
    bcc @exit
    inc $01,x

@exit:
    rts

; !(UNUSED)
; subtracts a from two byte value stored in $00,$01
; input
;  * a - amount to subtract from 2-byte value $00 $01
;  * $00 - low byte of 2-byte value
;  * $01 - high byte of 2-byte value
bank_f_unused_00:
two_byte_subtract:
    sec
    eor #$ff
    adc $00,x
    sta $00,x
    bcs @exit
    dec $01,x

@exit:
    rts

; adds score, stored as binary-coded decimal, to player's score
; if appropriate, handles giving 1-up, and/or setting new high score
; input
;  * $01 - binary-coded decimal (2 digits) added to the player's score.
;          This byte represents the 100s and 10s place
;  * PLAYER_INDEX - player index (0 = p1, 1 = p2)
add_single_bcd_to_score:
    lda #$00
    sta $02

; adds score, stored as binary-coded decimal, to player's score
; if appropriate, handles giving 1-up, and/or setting new high score
; input
;  * $01 - binary-coded decimal (2 digits) added to the player's score.
;          This byte represents the 100s and 10s place
;  * $02 - binary-coded decimal (2 digits) added to the player's score.
;          This byte represents the 10,000s and 1,000s place
;  * PLAYER_INDEX - player index (0 = p1, 1 = p2)
add_score_to_player:
    lda #$00
    sta $03       ; set 1,000,000s and 100,00s place to add to score to zero
    lda DEMO_MODE ; #$00 not in demo mode, #$01 demo mode on
    beq @continue ; continue if not in demo
    rts           ; exit if in demo mode
                  ; if player somehow collided with enemy, don't kill enemy

@continue:
    lda PLAYER_INDEX      ; load player index (0 = p1, 1 = p2)
    beq @player_score_low ; branch if player 1
    lda #$01              ; player 2
                          ; !(HUH) no way to have player that isn't 0 nor 1
                          ; no need to ensure $21 isn't 0 nor 1

@player_score_low:
    sta PLAYER_INDEX         ; store player index back into $21
    lda #.LOBYTE(P1_SCORE)   ; assume player 1 score
    ldy PLAYER_INDEX         ; load player index
    beq @init_score_addition ; branch for player 1
    lda #.LOBYTE(P2_SCORE)   ; player 2, use player 2's score address

@init_score_addition:
    sta $04                ; store low byte of address to score
    ldy #.HIBYTE(P2_SCORE) ; load high byte of 2 byte address to score
    sty $05                ; stores 2 byte address to lowest byte of score
                           ; (largest memory address of player's 3-byte score)
    ldx #$01               ; start by adding address $01 (100s and 10s place) as binary-coded decimal
    ldy #$00               ; initialize score read byte index
    lda #$03               ; score is 3 bytes wide
    sta $00                ; loop 3 times
    clc

; add 3-byte amount to 3-byte player score
; the 3 bytes to add are in $01, $02, and $03 and are encoded in binary-coded decimal (BCD)
; where $01 is the smallest part of the number, e.g. #$58 #$47 #$00 = 0047580
@add_to_score:
    lda ($04),y       ; P1_SCORE or P2_SCORE byte
    jsr bcd_add       ; add a and $00,x (both values are binary-coded decimals)
    sta ($04),y       ; update player score binary-coded decimal byte
    iny               ; increment score read counter (#$00, #$01 or #$02)
    inx               ; increment add to score read index
    dec $00           ; decrement byte loop counter
    bne @add_to_score ; branch if more binary-coded decimals to add to score
    bcc @check_1_up   ; finished add the 6 bytes together
                      ; branch if there was no overflow in adding to highest place in the score (done)
    ldx #$02          ; overflow in adding to highest place in the score
                      ; just set high score to max, which is #$99 #$99 #$99 (displayed as 9999990)
    lda #$99

; set high score to maximum value of 9999990
@max_high_score_loop:
    sta HI_SCORE,x
    dex                      ; move to next high score byte
    bpl @max_high_score_loop
    rts

@check_1_up:
    ldx PLAYER_INDEX          ; load player index
    ldy #$02
    lda ($04),y               ; read player's highest 2 decimal digits (1,000,000s and 100,00s place)
    cmp NEXT_1_UP_SCORE_HI,x  ; compare to the next 1-up score middle decimal places
    bcc @compare_hi_score     ; branch if player score isn't high enough for an extra life to see if new hi score
    bne @give_1_up            ; branch if high digits are larger than next 1-up score high digits
                              ; to skip over the low digits check
    dey                       ; NEXT_1_UP_SCORE_HI is equal to player's score
                              ; for the 1,000,000s and 100,00s places
                              ; need to compare 10,000s and 1,000s places
    lda ($04),y
    cmp NEXT_1_UP_SCORE_MID,x
    bcc @compare_hi_score     ; branch if player score isn't high enough for an extra life to see if new hi score

@give_1_up:
    ldy PLAYER_INDEX          ; load player index
    ldx #$20
    lda NEXT_1_UP_SCORE_MID,y
    clc                       ; adding 0020000 to the required score to get the next 1-up
    jsr bcd_add_a_x           ; add a and 20 where both values are encoded as binary-coded decimals
    sta NEXT_1_UP_SCORE_MID,y ; adding 20 to middle 2 digits of score (10,000s and 1,000s place)
    bcc @check_max_num_lives  ; branch if no overflow
    ldx #$01                  ; overflow, add 1 to high byte
    lda NEXT_1_UP_SCORE_HI,y
    clc
    jsr bcd_add_a_x           ; add a and 01 where both values are encoded as binary-coded decimals
    bcc @set_1_up_hi
    lda #$ff                  ; set impossible score for next 1-up

@set_1_up_hi:
    sta NEXT_1_UP_SCORE_HI,y ; adding 01 to middle 2 digits of score (1,000,000s and 100,000s place)

@check_max_num_lives:
    ldx PLAYER_INDEX       ; load player index
    lda PLAYER_NUM_LIVES,x
    cmp #$63               ; see if at max number of lives (99 in decimal)
    bcc @inc_num_lives
    lda #$63
    sta PLAYER_NUM_LIVES,x ; set number of lives to #$63 if #$63 or greater
                           ; 99 in decimal, which is the highest number that can be shown correctly
                           ; on the player score screen
    bne @compare_hi_score  ; always branch

@inc_num_lives:
    inc PLAYER_NUM_LIVES,x
    lda #$23
    jsr play_sound         ; play sound_23 (P 1UP) - extra life sound

@compare_hi_score:
    ldy #$02

@compare_hi_score_loop:
    lda HI_SCORE,y
    cmp ($04),y
    bcc @set_hi_score
    bne @exit
    dey
    bpl @compare_hi_score_loop

@set_hi_score:
    ldy #$02

@set_hi_score_loop:
    lda ($04),y
    sta HI_SCORE,y
    dey
    bpl @set_hi_score_loop

@exit:
    rts

; add a and x where both values are encoded as binary-coded decimals
; input
;  * a - original amount (binary-coded decimal)
;  * x - amount to add to a (binary-coded decimal)
; output
;  * a - the binary-coded decimal representation of the result
;  * carry flag - whether or not the result has any overflow
bcd_add_a_x:
    stx $00
    ldx #$00

; add a and $00,x where both values are encoded as binary-coded decimals
; result is in a and also encoded as a binary-coded decimal
; input
;  * a - byte value to add to from player score (binary-coded decimal)
;  * $00,x - amount being to add to a (binary-coded decimal)
;  * carry flag - whether or not there was any overflow from any previous digit
; output
;  * a - the binary-coded decimal representation of the result
;  * carry flag - whether or not the result has any overflow
bcd_add:
    sta $07       ; set initial value to add to
    and #$f0
    sta $06       ; set high nibble to single binary-coded decimal
    eor $07
    sta $07       ; set low nibble to next binary-coded decimal
                  ; e.g. if a = #$42, then $06 = #$40, $07 = #$02
    lda $00,x
    and #$0f
    adc $07       ; add to smallest binary encoded digit
    cmp #$0a      ; see if overflow (#$09 is the last decimal)
    bcc @continue ; branch if no overflow
    adc #$05      ; overflow, adding 6 will create a 2 digit binary-coded decimal
                  ; e.g. #$0e -> #$14 (14 in decimal)

@continue:
    adc $06       ; add result to 2nd binary-coded decimal digit
    sta $06       ; set binary-coded decimal result in $06
                  ; e.g. 42 + 09 = $51
    lda $00,x     ; at this point, only added 1s place, now adding 10s place
    and #$f0      ; adding 10s place
    adc $06       ; add 10s place to result
                  ; e.g. 42 + 19 = 61
    bcs @overflow ; branch if overflow
    cmp #$a0      ; see if result 10s digit is larger than 9
    bcc @exit     ; exit with carry clear indicating no overflow

; correct 10s place and set carry indicating overflow
@overflow:
    sbc #$a0 ; result 10s place is larger than 9
             ; subtract 10 to get 10s place
    sec      ; set carry indicating an overflow

@exit:
    rts

; !(UNUSED)
; not sure what the exact use could be. It looks like it would calculate
; $00 = a * number_of_set_bits(y)
; but there is a ror that modifies a between every bit and I don't see what this
; could be used for
; input
;  * a - the amount to add for every set bit in y
;  * y - the number to evaluate set bits on
;  output
;  * $00
;  * carry
bank_f_unused_01:
    sty $02
    sta $00
    lda #$00
    ldy #$08 ; looping through all bits in $02

@loop:
    lsr $02
    bcc @continue
    clc           ; clear carry in preparation for addition
    adc $00       ; bit was 1, add $00

@continue:
    ror
    dey       ; decrement loop counter
    bne @loop
    sta $00
    rts

; draws the level title screen's "AREA" and level number, e.g. "AREA 2"
; input
;  * x - graphics buffer write offset
draw_title_level:
    lda #$02                          ; text index for "AREA"
    jsr load_bank_0_write_text_to_mem ; store "AREA" in CPU_GRAPHICS_BUFFER
    lda CURRENT_LEVEL                 ; load current level
    clc                               ; clear carry in preparation for addition
    adc #$02                          ; convert from level to tile character, e.g. 0 -> '1', 1 -> '2', 2 -> '3', etc.
                                      ; level numbers are 0 indexed, but displayed as 1 indexed
    sta CPU_GRAPHICS_BUFFER-2,x       ; write level number
    rts

; draws the "1P SCORE", "2P SCORE", "HI SCORE", "REST" labels and their values
draw_scores_num_lives:
    lda #$05
    sta PPU_READY
    lda #$08
    jsr load_banks_set_palette           ; load the palette colors for flashing intro screen
    ldy #$4c                             ; background palette 0
                                         ; COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00
    jsr set_palette                      ; set background palette 0 to white, gray, dark gray
    jsr write_palette_to_graphics_buffer ; write bg and sprite palette colors from palette buffer to graphics buffer
    lda #$a8
    sta PPUCTRL_SETTINGS                 ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
    lda #$00
    sta X_SCROLL                         ; set PPU horizontal scroll to no scroll
    sta Y_SCROLL                         ; set PPU vertical scroll to top of nametables (no scroll)
    lda #$06                             ; text index for "REST"
    jsr load_bank_0_write_text_to_mem    ; store "REST" in CPU_GRAPHICS_BUFFER
    ldy #$00                             ; 1st player index
    jsr draw_player_num_lives            ; draw 1st player number of lives
    lda #$07                             ; text index for "REST"
    jsr load_bank_0_write_text_to_mem    ; store "REST" in CPU_GRAPHICS_BUFFER
    ldy #$01                             ; 2nd player index
    jsr draw_player_num_lives            ; draw 2nd player number of lives
    lda #$03                             ; text index for "1P SCORE"
    jsr load_bank_0_write_text_to_mem    ; store "1P SCORE" in CPU_GRAPHICS_BUFFER
    lda #.LOBYTE(P1_SCORE)               ; $07e3
    jsr draw_score
    lda #$04                             ; text index for "2P SCORE"
    jsr load_bank_0_write_text_to_mem    ; store "2P SCORE" in CPU_GRAPHICS_BUFFER
    lda #.LOBYTE(P2_SCORE)               ; $07e6
    jsr draw_score
    lda #$05                             ; text index for "HI SCORE"
    jsr load_bank_0_write_text_to_mem    ; store "HI SCORE" in CPU_GRAPHICS_BUFFER
    lda #.LOBYTE(HI_SCORE)               ; $07e0

; draw player 1, player 2, or hi score
; score is encoded in little endian binary-coded decimal
; e.g. a score of 0047580 is #$58 #$47 #$00
; note that a 0 is appended to the end to make the score larger
; input
;  * a - low byte of address to player score's first byte (score low byte)
draw_score:
    sta $00  ; store low byte of score address
    lda #$07
    sta $01  ; store high byte of score address
    txa      ; transfer graphics buffer offset to a
    sec      ; set carry flag in preparation for subtraction
    sbc #$08 ; subtract #$08 from graphics buffer offset
             ; 7 digits in the score to draw
    tax      ; update graphics buffer offset
    ldy #$02 ; 3 bytes make up the score

@write_score_byte_digits:
    lda ($00),y                  ; load score nibble
                                 ; contains 2 nibbles of binary-encoded score digits
    lsr
    lsr
    lsr
    lsr                          ; push high nibble to low nibble
    jsr @write_digit             ; write digit encoded in score high nibble
    lda ($00),y                  ; read low nibble
    jsr @write_digit             ; write digit encoded in score low nibble
    dey                          ; move to next score byte
    bpl @write_score_byte_digits ; branch if more score digits to draw
    rts

@write_digit:
    and #$0f                  ; the binary-encoded nibble is in low nibble
    clc                       ; clear carry in preparation for addition
    adc #$01                  ; converting the ones digit to pattern tile offset
                              ; digit 0 = tile offset 1, digit 1 = tile offset 2, etc.
    sta CPU_GRAPHICS_BUFFER,x
    inx                       ; increment graphics buffer write offset
    rts

; draws the level screen number of lives (the part after "REST")
; !(BUG) can only correctly render number of lives up to #$63 (99)
; input
;  * x - graphics buffer offset
;  * y - player index (0 = p1, 1 = p2)
draw_player_num_lives:
    lda PLAYER_NUM_LIVES,y ; load number of lives
    ldy #$00

@loop:
    cmp #$0a      ; see if number of lives is greater than 10 (more than one digit)
    bcc @continue ; branch if new amount is less than 10 lives to just draw the digit
    sbc #$0a      ; subtract 10 from remaining lives
    iny           ; increment number of times #$0a has been subtracted
                  ; this will be the 10s digit
    bne @loop     ; continue subtracting 10 if number is larger than 10

@continue:
                                ; a now contains the 1s digit
    clc                         ; clear carry in preparation for addition
    adc #$01                    ; covert the ones digit to pattern tile offset
                                ; digit 0 = tile offset 1, digit 1 = tile offset 2, etc.
    sta CPU_GRAPHICS_BUFFER-2,x ; write
    tya                         ; transfer number of times #$0a fit into player's number of lives to a
    clc                         ; clear carry in preparation for addition
    adc #$01                    ; convert the ones digit to pattern tile offset
                                ; if a is greater than 9, then incorrectly starts rendering letters starting at A
    sta CPU_GRAPHICS_BUFFER-3,x ; write 10s digit
    rts

; set the tiles used between levels to show high score, level name, etc
set_menu_pattern_tiles:
    lda #$40
    sta LEFT_TOP_HALF_CHR_BANK    ; set bank number of PPU $0000-$07ff (top half of left pattern table)
    lda #$6a
    sta LEFT_BOTTOM_CHR_HALF_BANK ; set bank number of PPU $0800-$0fff (bottom half of left pattern table)
    lda #$44
    sta RIGHT_FIRST_QTR_CHR_BANK  ; set bank number of PPU $1000-$13ff (first quarter of right pattern table)
    lda #$45
    sta RIGHT_SECOND_QTR_CHR_BANK ; set bank number of PPU $1400-$17ff (second quarter of right pattern table)
    lda #$46
    sta RIGHT_THIRD_QTR_CHR_BANK  ; set bank number of PPU $1800-$1bff (third quarter of right pattern table)
    lda #$07
    sta RIGHT_FOURTH_QTR_CHR_BANK ; set bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    rts

; loads banks to press demo input, check if demo level finished, if so go back to show intro screen, otherwise, run level
run_demo:
    jsr load_banks_run_demo_input ; run player input simulation for demo
    lda DEMO_LEVEL_END_FLAG       ; whether or not the demo for the level is complete
    beq game_routine_04           ; still in demo, run demo
    jmp jmp_set_game_routine_00   ; demo level complete, set game routine to #$00 to show intro (player select) screen

game_routine_04:
    lda LEVEL_ROUTINE_INDEX ; load current level routine
    cmp #$03
    bne run_level_routine   ; jump if not level routine 3 (this code is called from level_routine_04 and level_routine_09)
    jmp level_routine_03    ; !(HUH) this conditional and manual jump aren't necessary
                            ; in original Contra, this logic would go to simulate input for demo, perhaps it was dropped

run_level_routine:
    jsr run_routine_from_tbl_below

level_routine_ptr_tbl:
    .addr level_routine_00
    .addr level_routine_01 ; draw level title card with player scores, number of lives, hi score, and level number, advance routine
    .addr level_routine_02 ; draw first screen of nametable, collision, and attribute data, play level music
    .addr level_routine_03 ; normal state in the level
    .addr level_routine_04 ; runs the end of level routine based on LEVEL_ROUTINE_VAR
    .addr level_routine_05
    .addr level_routine_06 ; wait for delay and replace "GAME OVER" text with "CONTINUE"/"END" text, initialize cursor location, advance routine
    .addr level_routine_07 ; game over, continue/end cursor
    .addr level_routine_08 ; run end credits and play music

level_routine_00:
    jsr load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels
    jsr clear_sprites                  ; clear player sprites (including bullets), enemy sprites, sprite locations, and sprite attributes
    jsr clear_memory                   ; clear $5b-$9f, $0400-$06ff, $03b0-$03ff, and $0700-$07df
    jsr zero_out_nametables            ; clear all nametables (set pattern tile to #$00)
    lda #$1e                           ; normal colors, show sprites (everywhere), show background (everywhere)
    sta PPUMASK_SETTINGS               ; set PPU mask variable, used to set PPUMASK later
    jsr clear_all_enemy_data           ; clear all enemy data (enemy routine, hp, attributes, etc)
    ldx #$01

@player_loop:
    lda #$00
    ldy CURRENT_LEVEL            ; load current level
    bne @not_level_1
    lda #$60                     ; only executed for the level 1 (fort firestorm)
                                 ; set initial sprite location
    sta PLAYER_SPRITE_Y_POS,x    ; set level 1 (Base Area 1) player initial Y position
    lda #$77
    sta PLAYER_SPRITE_X_POS,x    ; set level 1 (Base Area 1) player initial X position
    lda player_state_timer_tbl,x
    ldy PLAYER_STATE,x           ; load player state
    beq @continue                ; branch to keep existing player state when non-zero
    ldy #$06                     ; player state zero, initialize to 6

@continue:
    jmp @continue2

; executed for every level except level 1 (for firestorm)
@not_level_1:
    ldy PLAYER_STATE,x
    beq @continue2
    ldy #$01

@continue2:
    sta PLAYER_STATE_TIMER,x ; mark player death to begin lying on ground timer
    tya
    sta PLAYER_STATE,x
    beq @continue3
    inc PLAYER_NUM_LIVES,x

@continue3:
    dex                              ; decrement player index
    bpl @player_loop
    jsr load_banks_load_level_header
    jsr load_banks_init_level_vars
    jsr set_nmi_noop_irq             ; remove any scanline interrupts
    lda #$01
    ldy DEMO_MODE                    ; #$00 not in demo mode, #$01 demo mode on
    bne @continue4                   ; if in demo, skip the level title screen
    jsr draw_title_level             ; draw the level title screen's "AREA" and level number, e.g. "AREA 2"
    jsr draw_scores_num_lives        ; draw the level title card screen with the player scores and num lives
    jsr set_menu_pattern_tiles       ; set the tiles used between levels to show high score, level name, etc
    lda #$80

@continue4:
    jmp set_delay_adv_level_routine ; set level 1 routine delay, advance level routine
                                    ; will be #$01 when in demo, and #$80 when not in demo

; controls delay before player drops from helicopter to slide down rope
; used so p2 comes out after p1
player_state_timer_tbl:
    .byte $60 ; player 1
    .byte $78 ; player 2

; draw level title card with player scores, number of lives, hi score, and level number, advance routine
level_routine_01:
    dec LEVEL_ROUTINE_DELAY                      ; decrement level routine delay
    bne level_routine_exit                       ; exit if delay hasn't elapsed
    jsr zero_out_nametables                      ; clear all nametables (set pattern tile to #$00)
    jsr load_level_pattern_tiles                 ; load the initial #$06 pattern table tile banks for the level
    jsr load_banks_set_palette_for_current_level ; load the palette colors for intro screen
    jsr write_palette_to_graphics_buffer         ; write bg and sprite palette colors from palette buffer to graphics buffer
    jmp adv_level_routine

; draw first screen of nametable, collision, and attribute data, play level music
level_routine_02:
    lda #$02
    sta PPU_READY
    ldx LEVEL_ROUTINE_VAR
    bne level_routine_02_continue       ; branch if finished drawing column of supertiles
    jsr load_banks_x_nt_draw_routine_02 ; set start location for drawing of supertile column
                                        ; then write 1 or 2 supertiles to the CPU_GRAPHICS_BUFFER

; draw rest of the column of supertiles
@loop:
    jsr load_banks_write_supertile_to_buffer
    lda SUPERTILE_DRAW_COUNT                 ; load number of supertiles remaining to draw
    cmp #$03
    bne @loop
    inc LEVEL_ROUTINE_VAR                    ; finished drawing column, increment level-specific variable

level_routine_exit:
    rts

level_routine_02_continue:
    jsr load_banks_write_supertile_to_buffer
    lda X_DRAW_ROUTINE
    bne level_routine_02_continue            ; draw next chunk if finished drawing full column
                                             ; X_DRAW_ROUTINE is set to #$00 once SUPERTILE_DRAW_COUNT is #$00
                                             ; which is when entire column of supertiles is written to graphics buffer
    lda DRAW_X_SCREEN                        ; load horizontal index of screen to draw
    cmp #$01                                 ; see if on the finished writing entire first screen to graphics buffer
    bne reset_level_routine_var_exit         ; branch if more of the screen needs to be written to the graphics buffer
    lda NT_CURRENT_COLUMN                    ; finished writing first screen
    cmp #$04
    bcc reset_level_routine_var_exit         ; branch if haven't written a full additional column past the first screen
    lda #$00
    sta X_SCROLL_DRAW_POINT
    sta SCREENS_DRAWN
    jsr load_banks_lvl_1_create_intro_heli   ; if level 1 (fort firestorm), create the intro helicopter enemy
                                             ; otherwise, just load banks 4 and 5 into memory
    jsr play_bg_music                        ; play background music and if level 1, play thunderclap as well

adv_level_routine:
    inc LEVEL_ROUTINE_INDEX

; resets the generic variable for use within a level routine
reset_level_routine_var_exit:
    lda #$00
    sta LEVEL_ROUTINE_VAR ; init the generic variable for use within a level routine

play_bg_music_exit:
    rts

; play background and for level 1 play thunderclap
play_bg_music:
    lda DEMO_MODE            ; #$00 not in demo mode, #$01 demo mode on
    bne @lvl_1_thunder_sound ; don't play level music/song when in demo mode
    ldy CURRENT_LEVEL        ; load current level
    lda lvl_bgm_tbl,y
    jsr play_sound           ; play level background music

@lvl_1_thunder_sound:
    lda CURRENT_LEVEL      ; load current level
    bne play_bg_music_exit ; exit if not level 1
    lda #$0a               ; level 1, play thunder sound as player drop in from helicopter
    jmp play_sound         ; play sound_0a (THUNDER) - level one intro thunder

; sound_28 (BGM1), sound_2b (BGM2), sound_2a (BGM3), sound_29 (BGM4)
; sound_2c (BGM5), sound_2d (BGM6), sound_2e (level 7 BGM), sound_2f (BGM7)
lvl_bgm_tbl:
    .byte $28,$2b,$2a,$29,$2c,$2d,$2e,$2f

; normal state in the level
level_routine_03:
    lda #$01
    sta PLAYER_HUD                             ; enable player hud
    jsr check_for_pause
    jsr load_banks_level_run_tile_routines     ; load appropriate tiles, pallettes, auto-scroll for position in level
    lda PAUSE_STATE                            ; #$00 for un-paused #$01 for paused
    beq @run_level_logic
    jsr load_banks_cp_bullet_to_sprite_buffers ; copy bullet sprite, sprite attr, location to sprite buffers to draw
    jmp safe_write_palette_to_graphics_buffer  ; write palette colors from palette buffer to graphics buffer
                                               ; when PAUSE_PALETTE_UPDATES is zero, and enough space in graphics buffer

@run_level_logic:
    jsr run_level_logic
    lda BOSS_DEFEATED_FLAGS
    bmi @boss_removed       ; branch if level boss is defeated and removed
    rts

@boss_removed:
    lda #$04                 ; moving to level_routine_04
    bne init_level_routine_a ; always branch to move to level_routine_04
                             ; and initialize LEVEL_ROUTINE_VAR to #$00

; plays game over sound and set level routine to level_routine_05
play_game_over_sound:
    lda #$35
    jsr play_sound          ; play sound_35 (OVER) - game over
    ldy #$01
    lda #$05                ; setting routine to level_routine_05
    sty LEVEL_ROUTINE_DELAY

; move to level routine specified in a, initialize LEVEL_ROUTINE_VAR to #$00
; input
;  * a - level routine to move to
init_level_routine_a:
    sta LEVEL_ROUTINE_INDEX          ; set routine (index into level_routine_ptr_tbl)
    jmp reset_level_routine_var_exit ; reset the generic variable for use within a level routine

run_level_logic:
    inc GLOBAL_TIMER
    jsr set_irq_scanlines                     ; set 2nd and 3rd scanline locations based on diff from 1st irq
    jsr load_banks_run_player_state_routines
    jsr load_banks_set_level_scroll_screen
    jsr load_banks_run_draw_routines          ; write off-screen nametable tile data to CPU_GRAPHICS_BUFFER in response to scroll
    jsr load_banks_handle_player_bullets      ; clear destroyed bullets, copy bullet data to general sprite buffers
    jsr load_bank_4_create_screen_enemies
    jsr load_banks_enemy_gen_routine          ; run timer logic for ENEMY_GEN_DELAY and if delay elapsed, generate random enemy
    jsr load_banks_exe_enemy_routines         ; load bank offset $50 and bank 3, execute exe_enemy_routines
    jmp safe_write_palette_to_graphics_buffer ; write palette colors from palette buffer to graphics buffer
                                              ; when PAUSE_PALETTE_UPDATES is zero, and enough space in graphics buffer

; runs the end of level routine based on LEVEL_ROUTINE_VAR
; for level 3 and higher, block controller input
level_routine_04:
    lda LEVEL_ROUTINE_VAR       ; load level routine-specific routine var
                                ; for level_routine_04 this is the end of level routine index
    cmp #$02
    bcc @continue
    ldy #$00                    ; level 3 or higher, prevent player input
    sty CONTROLLER_STATE_DIFF
    sty CONTROLLER_STATE_DIFF+1
    sty CONTROLLER_STATE
    sty CONTROLLER_STATE+1

@continue:
    jsr run_routine_from_tbl_below

end_level_routine_ptr_tbl:
    .addr end_level_routine_00 ; run_level_logic and set delay and advance routine
    .addr end_level_routine_01 ; level cleared, wait for delay, play level clear music
    .addr end_level_routine_02 ; wait for auto-move animation
    .addr end_level_routine_03 ; wait for delay, initialize fade to black timer, advance routine
    .addr end_level_routine_04 ; fade to black
    .addr end_level_routine_05 ; set CURRENT_LEVEL and LEVEL_ROUTINE_INDEX based on whether beat the game

end_level_routine_00:
    jsr load_banks_level_run_tile_routines ; load appropriate tiles, pallettes, auto-scroll for position in level
    jsr run_level_logic
    lda #$5e
    bne end_level_set_delay_adv_routine    ; always branch to set delay to #$5e and move to next level routine

; level cleared, wait for delay, play level clear music
end_level_routine_01:
    jsr load_banks_level_run_tile_routines ; load appropriate tiles, pallettes, auto-scroll for position in level
    jsr run_level_logic
    lda FRAME_COUNTER                      ; load frame counter
    lsr
    bcs end_level_exit                     ; exit if odd frame
    dec LEVEL_ROUTINE_DELAY                ; even frame, decrement delay
    bne end_level_exit                     ; exit if delay hasn't elapsed
    lda #$33                               ; sound_33 (PCLEAR) - pattern clear
    ldy CURRENT_LEVEL                      ; load current level
    cpy #$07
    bne @play_level_clear                  ; use regular level clear sound if not on final level
    lda #$34                               ; final level, use sound_34 (A CLEAR) - final boss defeated sound

@play_level_clear:
    jsr play_sound          ; play level clear music
    lda #$00
    sta END_LEVEL_ROUTINE   ; initialize end level routine index
    lda BOSS_DEFEATED_FLAGS
    ora #$40
    sta BOSS_DEFEATED_FLAGS ; mark playing the level clear music (bit 6)
    lda #$90

end_level_set_delay_adv_routine:
    sta LEVEL_ROUTINE_DELAY

end_level_adv_routine_exit:
    inc LEVEL_ROUTINE_VAR

end_level_exit:
    rts

; wait for auto-move animation
end_level_routine_02:
    jsr load_banks_run_end_level_anim_routine ; run the end of level animation (auto-scroll, auto-move)
    jsr load_banks_level_run_tile_routines    ; load appropriate tiles, pallettes, auto-scroll for position in level
    jsr run_level_logic
    lda FRAME_COUNTER                         ; load frame counter
    and #$03
    bne @continue
    dec LEVEL_ROUTINE_DELAY                   ; decrement delay every 3 frames

@continue:
    beq @set_delay_adv_routine  ; branch if delay elapsed
    lda LEVEL_ROUTINE_DELAY
    cmp #$30
    bcs end_level_routine_exit2 ; exit if delay is greater than or equal to #$30
    ldy #$00                    ; delay not elapsed and less than #$30
                                ; initialize advance level routine flag (0 = advance level routine, non-zero = stay on current routine)
    lda PLAYER_GAME_OVER_STATUS ; load p1 game over status (0 = not game over, 1 = game over)
    bne @check_p2               ; move to p2 if p1 in game over state
    lda PLAYER_STATE
    cmp #$08
    beq @check_p2               ; move to p2 if p1 is in end-of-level auto movement, don't advance routine
    iny                         ; p1 not yet in auto-movement, do not advance level routine

@check_p2:
    lda PLAYER_GAME_OVER_STATUS+1 ; load p2 game over status (0 = not game over, 1 = game over/1 player game)
    bne @check_auto_move_start    ; branch if p2 is game over (or 1 player game)
    lda PLAYER_STATE+1            ; p2 not in game over, load p2 player state
    cmp #$08
    beq @check_auto_move_start    ; branch if p2 is in end-of-level auto movement state
    iny                           ; p2 not yet in auto movement, don't advance level routine

@check_auto_move_start:
    tya                         ; non-zero when waiting for player to begin auto-movement animation at end of level
                                ; (0 = advance level routine, non-zero = stay on current routine)
    bne end_level_routine_exit2 ; exit if waiting

@set_delay_adv_routine:
    lda #$08
    bne end_level_set_delay_adv_routine ; always branch to set delay to #$08 and advance level routine

end_level_routine_exit2:
    rts

; wait for delay, initialize fade to black timer, advance routine
end_level_routine_03:
    dec LEVEL_ROUTINE_DELAY
    bne end_level_routine_exit2    ; exit if delay hasn't elapsed
    lda #$01
    sta END_LEVEL_FADE_DELAY       ; initialize fade to black animation to start next frame
    bne end_level_adv_routine_exit ; always branch to advance the level routine

; fade to black
end_level_routine_04:
    jsr fade_to_black                         ; animate end of level fade to black after player walks out of frame
    php                                       ; push status flags on to the stack
                                              ; carry flag contains whether or not the screen is fully dark
    jsr safe_write_palette_to_graphics_buffer ; write palette colors from palette buffer to graphics buffer
                                              ; when PAUSE_PALETTE_UPDATES is zero, and enough space in graphics buffer
    plp                                       ; restore status flags from stack
                                              ; carry flag contains whether or not the screen is fully dark
    bcc end_level_routine_exit2               ; branch if screen not yet fully dark
    lda #$30                                  ; screen fully black, move to next level routine with delay set to #$30
    sta LEVEL_ROUTINE_DELAY
    bne end_level_adv_routine_exit

; set CURRENT_LEVEL and LEVEL_ROUTINE_INDEX based on whether beat the game
; if beat the game, set GAME_COMPLETED to #$01, and LEVEL_ROUTINE_INDEX to level_routine_08 (run end credits)
; otherwise, set LEVEL_ROUTINE_INDEX to level_routine_00
end_level_routine_05:
    dec LEVEL_ROUTINE_DELAY
    bne end_level_routine_exit2 ; branch if delay hasn't elapsed
    jsr zero_out_nametables     ; clear all nametables (set pattern tile to #$00)
    inc CURRENT_LEVEL           ; increment current level
    lda CURRENT_LEVEL           ; load current level
    cmp #$08                    ; compare to level after defeating final boss
                                ; note, this is not level 8 (The Final Stage)
                                ; it is a special routine after defeating the final boss
    bcc @reset_level_routine    ; branch if not past last level
    lda #$00                    ; beat the last level, reset back to first level
    sta CURRENT_LEVEL
    lda GAME_COMPLETED          ; load whether or not the game has been completed
    adc #$00                    ; increment game completion count (carry is set)
    cmp #$02                    ; see if game already completed
    bcc @continue               ; branch if first time completing game to set value to #$01
    lda #$01                    ; game already beaten at least once, keep value of #$01 (don't set to #$02)
                                ; !(OBS) at some point, the developers must have wanted to have support for tracking
                                ; the number of times the game has been defeated, similar to Contra
                                ; otherwise, they could have just done lda #$01, sta GAME_COMPLETED

@continue:
    sta GAME_COMPLETED     ; set game defeated flag to #01
    jsr clear_memory
    lda #$08
    bne @set_level_routine ; set LEVEL_ROUTINE_INDEX to level_routine_08
                           ; (run end credits)

@reset_level_routine:
    lda #$00

@set_level_routine:
    jmp init_level_routine_a ; move to level routine specified in a
                             ; either level_routine_00 (moving to next level) or level_routine_08 (beat game)
                             ; also initializes LEVEL_ROUTINE_VAR to #$00

level_routine_05:
    jsr set_nmi_noop_irq              ; remove any scanline interrupts
    jsr clear_sprites                 ; clear player sprites (including bullets), enemy sprites, sprite locations, and sprite attributes
    jsr zero_out_nametables           ; clear all nametables (set pattern tile to #$00)
    lda #$00
    sta PLAYER_HUD                    ; showing game over screen with player scores and high score, disable player hud
    lda #$09                          ; text input for "GAME OVER"
    jsr load_bank_0_write_text_to_mem ; store "GAME OVER" in CPU_GRAPHICS_BUFFER
    jsr draw_scores_num_lives
    jsr set_menu_pattern_tiles        ; set the tiles used between levels to show high score, level name, etc
    lda #$c0

; set level routine delay to a, advance routine
; input
;  * a - the level routine delay
set_delay_adv_level_routine:
    sta LEVEL_ROUTINE_DELAY ; set level routine delay
    jmp adv_level_routine   ; increment level routine index

; wait for delay and replace "GAME OVER" text with "CONTINUE"/"END" text, initialize cursor location, advance routine
level_routine_06:
    dec LEVEL_ROUTINE_DELAY           ; decrement level routine delay
    bne game_over_exit                ; exit if level delay hasn't elapsed
    dec NUM_CONTINUES                 ; decrement number of continues
    bmi restart_game                  ; branch if no more continues to reset the game
    lda #$0a                          ; store "CONTINUE END" in CPU_GRAPHICS_BUFFER
    jsr load_bank_0_write_text_to_mem ; store "CONTINUE END" in CPU_GRAPHICS_BUFFER
    lda #$00                          ; set selection to CONTINUE
    sta CONT_END_SELECTION            ; #$00 when "CONTINUE" is selected, #$01 when "END" is selected
    jmp adv_level_routine

; game over, continue/end cursor
level_routine_07:
    lda CONTROLLER_STATE_DIFF
    and #$2c
    beq @set_cursor_sprite_and_scores ; branch if neither select, up, nor down are pressed
    lda CONT_END_SELECTION            ; select, up, or down pressed, change selection between "CONTINUE" and "END"
                                      ; #$00 when "CONTINUE" is selected, #$01 when "END" is selected
    eor #$01                          ; swap to other selection
    sta CONT_END_SELECTION            ; set new selection

@set_cursor_sprite_and_scores:
    ldy CONT_END_SELECTION
    lda cursor_sprite_y_pos,y ; load player select cursor sprite Y position
    ldy #$50                  ; cursor X position
    jsr set_cursor_sprite     ; draw player select sprite (sprite_b9) at position (y,a) with palette #$01
    lda CONTROLLER_STATE_DIFF
    and #$10
    beq game_over_exit
    lda CONT_END_SELECTION
    bne restart_game          ; branch if END is selected
    jsr init_player_vars      ; CONTINUE is selected
    lda #$00
    jmp init_level_routine_a  ; move to level_routine_00, initialize LEVEL_ROUTINE_VAR to #$00

restart_game:
    lda #$00
    sta DEMO_LEVEL ; reset demo level

jmp_set_game_routine_00:
    jmp set_game_routine_00

game_over_exit:
    rts

cursor_sprite_y_pos:
    .byte $90,$a8

; run end credits and play music
level_routine_08:
    lda LEVEL_ROUTINE_VAR
    bne @run_credits                         ; branch if already updated PPU and started end credits music
    jsr zero_out_nametables                  ; clear all nametables (set pattern tile to #$00)
    ldx #$04
    jsr load_banks_write_graphic_data_to_ppu ; write graphic_data_02
                                             ; (end of game credits mountains and clouds nametable data)
    lda #$0a
    jsr load_banks_set_palette               ; load the palette colors for the ending credits
    jsr write_palette_to_graphics_buffer     ; write bg and sprite palette colors from palette buffer to graphics buffer
    lda #$36                                 ; sound_36 Free World (ENDING) - background music for credits
    jsr play_sound                           ; play sound_36 (ENDING)
    jmp end_level_adv_routine_exit           ; updated PPU and started end credits music, advance level routine var

@run_credits:
    jsr load_banks_run_scroll_credits_routine ; draw and scroll credits
    lda X_DRAW_ROUTINE
    beq game_over_exit
    lda #$00
    sta PLAYER_CURRENT_WEAPON
    sta PLAYER_CURRENT_WEAPON+1               ; reset players' weapon to default with no rapid fire
    lda #$00                                  ; level_routine_00
    jmp init_level_routine_a                  ; move to level_routine_00, initialize LEVEL_ROUTINE_VAR to #$00

; checks for start button and sets pause status as appropriate
; plays sound if entering pause
check_for_pause:
    lda DEMO_MODE               ; #$00 not in demo mode, #$01 demo mode on
    ora UNUSED_PAUSE_FLAG       ; always #$00
    ora PPU_READY               ; #$00 when PPU is ready, > #$00 otherwise
    bne pause_exit_00           ; if in demo, PPU isn't ready, or $26 > 0, then exit
    lda CONTROLLER_STATE_DIFF_B ; controller 1 buttons pressed
    ldy PAUSE_STATE             ; #$00 for un-paused #$01 for paused
    bne @game_paused            ; if game paused, jump
    and #$10                    ; keep bits ...x .... (check for start button)
    beq pause_exit_00           ; exit if start button isn't pressed
    lda #$01                    ; a = #$01
    sta PAUSE_STATE             ; #$01 for paused, #$00 for not paused
    lda #$27                    ; a = #$27 (27 = game pausing jingle sound)
    jmp play_sound              ; play sound_27 - pause jingle sound
    rts                         ; unused/unreachable

; handle game paused state
; un-pauses if necessary
@game_paused:
    and #$10          ; keep bits ...x .... (check for start button)
    beq pause_exit_01 ; exit if start button isn't pressed
    lda #$00          ; a = #$00
    sta PAUSE_STATE   ; set game state to not paused

pause_exit_00:
    rts

; !(HUH) interesting, same double rts as Probotector that wasn't in Contra
pause_exit_01:
    rts

; loads the initial #$06 pattern table tile banks for the level
load_level_pattern_tiles:
    lda CURRENT_LEVEL ; load current level
    asl
    sta $00
    asl               ; multiply the level index by #$06 ((CURRENT_LEVEL * 4) + CURRENT_LEVEL)
    adc $00           ; each entry in table is #$06 bytes
    tay               ; transfer to offset register
    ldx #$00

@pattern_tiles_loop:
    lda level_pattern_tile_banks_tbl,y
    sta CHR_BANKS,x                    ; store the chr rom bank in
    iny
    inx
    cpx #$06                           ; see if written all 6 chr rom banks
    bne @pattern_tiles_loop            ; branch to continue if not complete
    rts

; contains the initial CHR ROM banks banks for the level
; each entry is #$06 bytes: 2 for left pattern table, 4 for right pattern table
level_pattern_tile_banks_tbl:
    .byte $00,$02,$44,$45,$46,$47 ; level 1 Fort Firestorm
    .byte $3c,$3e,$49,$4a,$4b,$1a ; level 2 First Base
    .byte $0c,$0e,$44,$45,$46,$48 ; level 3 Jungle
    .byte $08,$0a,$44,$45,$46,$47 ; level 4 Inner Base
    .byte $1c,$1e,$44,$45,$46,$60 ; level 5 The Cliff
    .byte $4c,$4e,$49,$4a,$61,$62 ; level 6 Entry to HQ
    .byte $24,$26,$44,$45,$63,$64 ; level 7 Headquarters
    .byte $34,$36,$44,$45,$67,$68 ; level 8 The Final Stage

; write off-screen nametable tile data to CPU_GRAPHICS_BUFFER in response to scroll
run_draw_routines:
    jsr run_y_draw_routine
    lda X_DRAW_ROUTINE
    jsr run_routine_from_tbl_below

x_supertile_draw_routine_tbl:
    .addr x_nt_draw_exit            ; noop routine, just exit (don't advance)
    .addr x_nt_draw_routine_01      ; noop routine, advance to next routine
    .addr x_nt_draw_routine_02      ; determine start location for drawing of supertile column
                                    ; then writes 1 or 2 supertiles to the CPU_GRAPHICS_BUFFER
    .addr write_supertile_to_buffer ; then writes 1 or 2 supertiles to the CPU_GRAPHICS_BUFFER

; noop routine, advance to next routine
x_nt_draw_routine_01:
    inc X_DRAW_ROUTINE ; advance routine
    rts

; determines start location for drawing of supertile column based on values below
; then writes 1 or 2 supertiles to the CPU_GRAPHICS_BUFFER
; input
;  * LEVEL_Y_SCREEN
;  * NT_CURRENT_ROW
;  * DRAW_X_SCREEN
; not used on level 2 (except for boss screen)
x_nt_draw_routine_02:
    inc X_DRAW_ROUTINE              ; advance routine
    lda #$00
    sta HAS_MARKED_SUPERTILE_BACKUP
    lda #$07                        ; #$07 supertiles per column of nametable
    sta SUPERTILE_DRAW_COUNT        ; starting new column, initialize number of supertiles to draw
    lda LEVEL_Y_SCREEN
    sta DRAW_Y_SCREEN               ; set current draw screen
    lda NT_ROW_SCROLL               ; load number of nametable rows scrolled
    ldy Y_SCROLL_DIR                ; 0 = vertically scrolling up, 1 = vertically scrolling down
    beq @continue                   ; branch if at top of nametable
    clc                             ; not at top of nametable
                                    ; clear carry in preparation for addition
    adc #$02
    cmp #$1e
    bcc @continue                   ; branch if not in bottom nametables
    inc DRAW_Y_SCREEN               ; scrolled into bottom nametables
    lda #$00

@continue:
    sta $00               ; calculating PPU address, store NT_ROW_SCROLL in $00
    and #$fc
    asl
    sta NT_CURRENT_ROW
    ldy #$00
    sty $01
    lda $00
    asl
    rol $01
    asl
    rol $01
    asl
    rol $01
    asl
    rol $01
    asl
    rol $01
    clc                   ; clear carry in preparation for addition
    adc NT_CURRENT_COLUMN
    sta PPU_WRITE_ADDR
    lda DRAW_X_SCREEN     ; load current level screen number (how many screens into the level)
    lsr
    lda #$20              ; $2c00 nametable (top left)
    bcc @set_high_addr
    lda #$2c              ; $2c00 nametable (bottom right)

@set_high_addr:
    ora $01
    sta PPU_WRITE_ADDR+1

; writes a supertile to the CPU_GRAPHICS_BUFFER, including its palette based on
; (DRAW_X_SCREEN, DRAW_Y_SCREEN) and (NT_CURRENT_ROW, NT_CURRENT_COLUMN)
; also writes the background collision data into BG_COLLISION_DATA
; only for drawing levels, not intro screen, nor hi score screens
; input
;  * DRAW_X_SCREEN - horizontal position into LEVEL_WIDTH*LEVEL_HEIGHT grid of screens for level
;  * DRAW_Y_SCREEN  - vertical position into LEVEL_WIDTH*LEVEL_HEIGHT grid of screens for level
;  * NT_CURRENT_ROW - row index into screen specifying where on the screen to write to buffer
;  * NT_CURRENT_COLUMN - column index into screen specifying where on the screen to write to buffer
write_supertile_to_buffer:
    jsr @write_supertile
    lda X_DRAW_ROUTINE   ; check routine index (usually #$00 or #$03)
    bne @write_supertile
    rts

@write_supertile:
    lda #$00                    ; start at left-most portion of level layout grid
    ldy DRAW_Y_SCREEN           ; load which screen vertically is being drawn
    beq @calc_supertile_address ; if first screen, no need to multiply by width get to get offset

; a = DRAW_Y_SCREEN * LEVEL_WIDTH
@loop:
    clc             ; clear carry in preparation for addition
    adc LEVEL_WIDTH ; add number of horizontal screens in level layout
    dey             ; decrement multiplier
    bne @loop

; a now contains the offset into the correct row into level_x_screen_layout_tbl
@calc_supertile_address:
    clc                                 ; clear carry in preparation for addition
    adc DRAW_X_SCREEN
    tay                                 ; y = (DRAW_Y_SCREEN * LEVEL_WIDTH) + DRAW_X_SCREEN
    lda (LEVEL_SCREEN_LAYOUT),y         ; load offset to supertiles for screen y (level_x_screen_layout_tbl offset)
    asl                                 ; double since byte is an offset into level_x_supertiles_screen_ptr_table
                                        ; and each entry in that is a #$02 byte memory address
    tay                                 ; transfer to offset register
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab low-byte to specific level_x_supertiles_screen_xx
    sta $08
    iny                                 ; increment LEVEL_SCREEN_SUPERTILES_PTR read offset
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab high-byte to specific level_x_supertiles_screen_xx
    sta $09                             ; ($08) now stores address to specific level_x_supertiles_screen_xx
                                        ; this is the list of all supertiles for the screen
                                        ; now load specific supertile to draw
    lda NT_CURRENT_COLUMN               ; load current nametable column to draw
    lsr                                 ; every time a column is drawn it is 4 nametable columns
    lsr                                 ; divide by 4 to get the draw column number
    clc                                 ; clear carry in preparation for addition
    adc NT_CURRENT_ROW
    tay                                 ; y = (NT_CURRENT_COLUMN / 4) + NT_CURRENT_COLUMN
                                        ; y now stores the index into the screen's supertiles
    lda #$00
    sta $0c                             ; initialize 2 byte offset into level_x_supertile_data
    lda ($08),y                         ; load supertile index (level_x_supertiles_screen_xx offset)
    sta $0d                             ; store supertile number in $0d
    asl
    rol $0c
    asl
    rol $0c
    asl
    rol $0c
    asl
    rol $0c                             ; multiply by #$10 since each supertile is #$10 bytes (tiles)
    clc                                 ; clear carry in preparation for addition
    adc LEVEL_SUPERTILE_DATA_PTR
    sta $08
    lda $0c
    adc LEVEL_SUPERTILE_DATA_PTR+1
    sta $09                             ; set level_x_supertile_data read offset
                                        ; ($08) now points to the beginning of
                                        ; the supertile bytes for super tile number $0d
    lda NT_CURRENT_ROW                  ; now know current supertile to draw, calculate next draw row
    clc                                 ; clear carry in preparation for addition
    adc #$08                            ; moving down to next
    cmp #$40
    bcc @calc_draw_address
    inc DRAW_Y_SCREEN                   ; move to next screen
    lda #$00

@calc_draw_address:
    sta NT_CURRENT_ROW              ; set next row
    lda #$00
    sta DRAW_BACKUP_BTM_SUPERTILE
    lda PPU_WRITE_ADDR              ; load PPU nametable address low byte
    sta $00                         ; set PPU nametable address low byte
    lda PPU_WRITE_ADDR+1            ; load PPU nametable address high byte
    sta $01                         ; set PPU nametable address high byte
                                    ; ($00) now contains the start PPU address for drawing
    jsr get_attr_addr               ; convert nametable address ($00) into the corresponding attribute table address ($12)
                                    ; now know both the nametable write address ($00) and attribute write address ($12)
    lda #$00
    sta $0e
    ldx #$80                        ; add 4 nametable rows to PPU_WRITE_ADDR
    ldy #$02                        ; default assume writing 2 rows of tile data to graphics buffer
    lda HAS_MARKED_SUPERTILE_BACKUP
    beq @should_backup
    lda SUPERTILE_DRAW_COUNT        ; load number of supertiles remaining to draw
    beq @set_backup_continue

@should_backup:
    lda PPU_WRITE_ADDR+1
    and #$03
    cmp #$03                    ; see if $23xx, $27xx, $2bxx, or $2fxx (last 6 rows of nametable)
    bcc @check_half_supertile   ; branch if not the last 6 rows of a nametable
    lda PPU_WRITE_ADDR          ; last 6 rows of nametable
    cmp #$80
    bcs @calc_add_next_ppu_addr ; branch if odd supertile

; start PPU write address is not the last 2 nametable rows
@check_half_supertile:
    ldy #$04                        ; writing 4 rows of tile data to graphics buffer
    lda PPU_WRITE_ADDR
    and #$7f                        ; strip bit 7
    cmp #$40
    bcc @calc_add_next_ppu_addr     ; branch top half of supertile
    lda #$08                        ; starting to write data at PPU address
                                    ; that is on the bottom half of supertile in the nametable
                                    ; !(HUH) didn't need comparisons in @should_backup
    sta $0e                         ; set level_x_supertile_data read offset to bottom half of supertile
    ldy #$02                        ; writing 2 rows of tile data to graphics buffer
    inc HAS_MARKED_SUPERTILE_BACKUP ; set to backup if drawn all supertiles

; bottom two rows of a supertile or have drawn all supertiles
@set_backup_continue:
    inc DRAW_BACKUP_BTM_SUPERTILE ; indicate that bottom two rows of this nametable
                                  ; should be backed up
    ldx #$40                      ; add 2 nametable rows to PPU_WRITE_ADDR

@calc_add_next_ppu_addr:
    sty $07                    ; set number of rows to write to graphics buffer (either #$02 of #$04)
    lda PPU_WRITE_ADDR+1       ; load PPU address high byte, in preparation for adding to the PPU address
    and #$fc                   ; strip to high byte for first row of nametable, i.e. #$20, #$24, #$28, or #$2c
                               ; this is to keep track for when there is an overflow past bottom of nametable
    sta $05                    ; store base nametable address high byte in $05
    txa                        ; transfer amount to add to PPU_WRITE_ADDR
                               ; (either 2 (#$40) or 4 (#$80) nametable rows)
    clc                        ; clear carry in preparation for addition
    adc PPU_WRITE_ADDR         ; add to nametable address
    sta PPU_WRITE_ADDR         ; set new nametable address low byte
    lda PPU_WRITE_ADDR+1       ; load nametable address high byte
    adc #$00                   ; add any carry obtained when adding to nametable low byte
    and #$03                   ; strip to offset from nametable base, when overflow past bottom will be #$00
    ora $05                    ; merge with base nametable
    sta PPU_WRITE_ADDR+1       ; set new PPU address high byte
                               ; the result here is adding either #$40 or #$80 to the nametable address
                               ; wrapping around when overflow past bottom of nametable
                               ; i.e. wrap when adding 1 to high byte
                               ; #$23 + #$01 -> #$20, #$27 + #$01 -> #$24, #$2b + #$01 -> #$28, #$2f + #$01 -> #$2c
    ldx GRAPHICS_BUFFER_OFFSET ; load current graphics buffer write offset
    lda #$06                   ; byte 0 = #$06 (block mode)
                               ; byte 1 and 2 will be PPU address
                               ; byte 3 is length, and bytes 4 to (byte 4 + length) are written to PPU
    sta CPU_GRAPHICS_BUFFER,x  ; set graphics format to block mode
    inx                        ; increment graphics buffer write offset

@write_row_tiles:
    jsr write_4_byte_draw_header ; write PPU address ($00) and specify writing 4 bytes to the graphics buffer (1 row)
    jsr calc_bg_collision_offset ; set $14 to bg collision code BG_COLLISION_DATA offset
    lda #$00
    sta $0f

; writes 4 nametable pattern table tile indexes
; simultaneously calculates the collision nibble
@write_loop:
    ldy $0e                      ; load level_x_supertile_data read offset
    lda ($08),y                  ; read graphic byte
    sta CPU_GRAPHICS_BUFFER,x    ; write pattern table tile to graphics buffer
    iny                          ; increment read offset
    inx                          ; increment graphics buffer write offset
    sty $0e                      ; update level_x_supertile_data read offset
    sta $0c                      ; set tile index in $0c for use later in calculating bg collision
    lda $07                      ; load row number (#$04 down to #$01)
    ora $06                      ; merge with group byte counter (#$04 down to #$01) (column)
    lsr                          ; see if either $06 or $07 have bit 0 set
    bcs @check_byte              ; branch if either row is #$01 or #$03, or group byte counter is #$01 or #$03
    lda $0c                      ; row or column is even, need to calculate bg collision code data
                                 ; load pattern table tile index
    jsr calc_bg_collision_nibble ; calculate collision code (low or high nibble) for tile a with index into super-tile $06 (#$02 or #$04)
    ora $0f                      ; merge with current bg collision byte
    sta $0f                      ; set collision byte

@check_byte:
    dec $06                 ; decrement group byte counter (column)
    bne @write_loop         ; branch to write next graphic byte if more to write
    lda $07                 ; finished with row, load number of rows to write
    lsr
    bcs @next_row
    lda $0f                 ; even row, need to write bg collision code
    ldy $14                 ; load bg collision offset
    sta BG_COLLISION_DATA,y ; write collision code byte

@next_row:
    jsr ppu_addr_next_row                ; update graphics buffer PPU address to next row
    bne @write_row_tiles                 ; branch to continue if more rows to write, i.e. $07 is non-zero
    jsr attr_tbl_block_size_one          ; done writing all 4 rows
                                         ; populate the graphics buffer with the PPU address for attribute table
                                         ; and set block mode block size to 1 (writing one entry to PPU)
                                         ; each attribute table byte is for 4x4 tiles (a supertile)
                                         ; which was just written
    ldy $0d                              ; load level palette data read offset (supertile index for level)
    lda (LEVEL_SUPERTILE_PALETTE_DATA),y ; load palette byte, each supertile corresponds to 1 attribute table byte
    sta CPU_GRAPHICS_BUFFER,x            ; set palette byte
    inx                                  ; increment graphics buffer write offset
    lda DRAW_BACKUP_BTM_SUPERTILE
    beq @mark_end
    lda SUPERTILE_DRAW_COUNT             ; backing up or duplicating last 2 rows just written
                                         ; load number of supertiles remaining to draw
    beq @draw_from_backup                ; branch if finished entire column of supertiles
                                         ; to add the previously backed up bottom half supertile
    lda CPU_GRAPHICS_BUFFER-1,x          ; load just-written supertile palette byte
    and #$f0                             ; strip attribute bits for top half of supertile
    sta SUPERTILE_PALETTE_MERGE          ; set high nibble
    ldy #$03                             ; backing up the last 2 nametable row (8 tiles) (half-supertile)

@last_row_loop:
    lda CPU_GRAPHICS_BUFFER-5,x  ; load the nametable tile from just-written last row
    sta GRAPHICS_BUFFER_TEMP+4,y ; backup nametable tile
    dex                          ; decrement backup tile count
    dey                          ; increment tile backup write offset
    bpl @last_row_loop
    ldy #$03

@penultimate_row_loop:
    lda CPU_GRAPHICS_BUFFER-8,x ; load the nametable tile from just-written penultimate row
    sta GRAPHICS_BUFFER_TEMP,y  ; backup nametable tile
    dex                         ; decrement backup tile count
    dey                         ; increment tile backup write offset
    bpl @penultimate_row_loop
    bmi x_nt_draw_exit          ; always exit

; SUPERTILE_DRAW_COUNT and DRAW_BACKUP_BTM_SUPERTILE both non-zero
; write the 2 backed-up rows and merge the attribute byte
; e.g. draw laser chandelier boss screen ceiling, top of cliff
@draw_from_backup:
    lda CPU_GRAPHICS_BUFFER-1,x ; load just-written supertile palette byte
    and #$0f                    ; strip attribute bits for bottom half of supertile
    ora SUPERTILE_PALETTE_MERGE ; merge with bottom half attribute bits of supertile saved previously
    sta CPU_GRAPHICS_BUFFER-1,x ; set new merged palette byte for split supertile
    lda #$02
    sta $07                     ; set number of rows to draw as #$02
    ldy #$00                    ; initialize GRAPHICS_BUFFER_TEMP read offset

@write_next_group:
    jsr write_4_byte_draw_header ; write PPU address ($00) and specify writing 4 bytes to the graphics buffer (1 row)

@byte_loop:
    lda GRAPHICS_BUFFER_TEMP,y
    sta CPU_GRAPHICS_BUFFER,x
    iny
    inx
    dec $06                    ; decrement graphic group byte counter
    bne @byte_loop
    jsr ppu_addr_next_row      ; update graphics buffer PPU address to next row
                               ; and decrement row counter $07
    bne @write_next_group      ; branch to continue if more rows to write

@mark_end:
    lda #$ff                   ; end of data byte
    sta CPU_GRAPHICS_BUFFER,x  ; write end of block mode data byte
    inx                        ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET ; set current graphics buffer write offset
    dec SUPERTILE_DRAW_COUNT   ; decrement number of remaining supertiles to draw in column
    bpl x_nt_draw_exit         ; exit if haven't drawn all supertiles
    lda NT_CURRENT_COLUMN      ; SUPERTILE_DRAW_COUNT is zero, load NT_CURRENT_COLUMN
    clc                        ; clear carry in preparation for addition
    adc #$04                   ; move to next column
    cmp #$20                   ; see if have written end of nametable
    bcc @set_next_draw_point   ; branch if not written the last column of the nametable
    inc DRAW_X_SCREEN          ; finished writing all #$20 columns of nametable
                               ; move to next current level screen number (how many screens into the level)
    lda #$00

@set_next_draw_point:
    sta NT_CURRENT_COLUMN   ; set nametable column write offset
    lda #$00
    sta X_DRAW_ROUTINE
    lda X_SCROLL_DRAW_POINT ; load current X_SCROLL draw checkpoint
    clc                     ; clear carry in preparation for addition
    adc #$20
    sta X_SCROLL_DRAW_POINT ; set X_SCROLL draw checkpoint
                            ; scroll right by #$20 pixels will trigger next column draw
    bcc x_nt_draw_exit
    inc SCREENS_DRAWN       ; increment number of screens drawn

x_nt_draw_exit:
    rts

; writes the PPU address and specifies to write 4 bytes to the graphics buffer
; input
;  * $00 - load PPU address low byte
;  * $01 - load PPU address high byte
;  * x - graphics buffer write offset
; output
;  * $06 - byte counter initialized to #$04
write_4_byte_draw_header:
    lda $01                   ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address high byte
    inx                       ; increment graphics buffer write offset
    lda $00                   ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address low byte
    inx                       ; increment graphics buffer write offset
    lda #$04                  ; writing 4 bytes to the graphics buffer (block mode)
    sta CPU_GRAPHICS_BUFFER,x
    sta $06                   ; initialize byte counter to #$04
    inx                       ; increment graphics buffer write offset
    rts

; move PPU address used in graphics buffer to point to next nametable row
; decrement row counter $07
; input
;  * $00 - load PPU address low byte
;  * $01 - load PPU address high byte
;  * $07 - row counter
; output
;  * zero flag - set when written all rows, i.e. $07 has decremented to #$00
ppu_addr_next_row:
    lda $00       ; load PPU address low byte
    clc           ; clear carry in preparation for addition
    adc #$20      ; move to next nametable row
    sta $00       ; update PPU address low byte
    bcc @continue ; branch if no overflow
    inc $01       ; PPU low address overflow, add 1 to PPU address high byte

@continue:
    dec $07 ; decrement number of rows to draw
    rts

; converts a nametable address into the corresponding attribute table address
; e.g. $2f8c -> $2ffb, or $2c0c -> $2fc3
; input
;  * $00 - PPU nametable address low byte
;  * $01 - PPU nametable address high byte
; output
;  * $12 - PPU attribute table address low byte
;  * $13 - PPU attribute table address high byte
get_attr_addr:
    lda $01
    and #$03
    sta $12
    lda $00
    asl
    rol $12
    asl
    asl
    asl
    rol $12
    asl
    rol $12
    asl
    rol $12
    lda $01
    ora #$03
    sta $13
    lda $12
    ora #$c0
    sta $12
    rts

; populates the graphics buffer with the PPU address for attribute table
; and sets block mode block size to 1 (writing one entry to PPU)
; input
;  * $12 - PPU attribute table address low byte
;  * $13 - PPU attribute table address high byte
attr_tbl_block_size_one:
    lda $13                   ; load attribute table address high byte
    sta CPU_GRAPHICS_BUFFER,x ; write attribute table address high byte
    inx                       ; increment graphics buffer write offset
    lda $12                   ; load attribute table address low byte
    sta CPU_GRAPHICS_BUFFER,x ; write attribute table address low byte
    inx                       ; increment graphics buffer write offset
    lda #$01                  ; block size = 1
    sta CPU_GRAPHICS_BUFFER,x ; specifying block size as one
    inx                       ; increment graphics buffer write offset
    rts

; input
;  * $00 - load PPU address low byte
;  * $01 - load PPU address high byte
; output
;  * $14 - bg collision offset
;  * $15 - specifies bg collision nibble, #$00 = low nibble (#$0f), #$80 = high nibble (#$f0)
calc_bg_collision_offset:
    lda #$00
    sta $15
    lda $01
    sta $14
    lda $00
    asl
    rol $14
    asl
    rol $14
    asl
    lsr $14
    ror
    lsr $14
    ror
    lsr $14
    ror
    lsr $14
    ror
    lsr $14
    ror
    ror $15
    sta $14  ; set bg collision byte
    rts

; populate either low or high nibble with collision code for tile a with index into super-tile $06 [#$00-#$03]
; input
;  * a - pattern table tile index
;  * $06 - pattern tile index within collision byte (#$02, or #$04)
; output
;  * a - either low or high nibble is populated with the collision code
calc_bg_collision_nibble:
    ldy #$00      ; start with collision code 0
    cmp #$ff      ; compare tile index to #$ff
    beq @continue ; branch if tile code is #$ff, to always use collision code 0

@find_collision_code:
    cmp COLLISION_CODE_TILE_INDICES,y ; compare tile index to the max for the collision code
    bcc @continue                     ; branch if tile index less than max, i.e. found the collision code
    iny                               ; didn't find collision code, increment to next collision code
    bcs @find_collision_code          ; always branch to test next collision code

@continue:
    lda $06
    sbc #$00                  ; subtract 1 (carry always clear)
    lsr
    lsr
    tya                       ; move collision code to a
    bcc @exit                 ; exit if bit 1 of ($06-1) is clear
    lda collision_codes_tbl,y ; load collision code in high nibble

@exit:
    rts

collision_codes_tbl:
    .byte $00 ; #$01 - player on floating platform
    .byte $10 ; #$02 - player on land / solid wall collision
    .byte $20 ; #$03 - collapsible ground on level 7 (eggshells)
    .byte $30 ; #$04 - player in water
    .byte $40 ; #$05
    .byte $50 ; #$06 - player on positive incline (start)
    .byte $60 ; #$07 - player on positive incline (second)
    .byte $70 ; #$08 - player on positive incline (most of incline)
    .byte $80 ; #$09 - negative incline (second)
    .byte $90 ; #$0a - negative incline (most of the time)
    .byte $a0 ; #$0b - negative incline (start)
    .byte $b0
    .byte $c0
    .byte $d0
    .byte $e0
    .byte $f0

; write off-screen nametable tile data to CPU_GRAPHICS_BUFFER in response to scroll
run_y_draw_routine:
    lda Y_DRAW_ROUTINE
    jsr run_routine_from_tbl_below

y_scroll_draw_routine_ptr_tbl:
    .addr y_scroll_draw_routine_exit ; noop
    .addr y_scroll_draw_routine_01   ; called once when scrolling vertically
    .addr y_scroll_draw_routine_02

; unused
bank_f_unused_02:
    rts

y_scroll_draw_routine_01:
    jsr y_scroll_calc_update_bg ; update background tiles and bg collision
    jmp y_scroll_update_attr    ; update attribute table

y_scroll_calc_update_bg:
    inc Y_DRAW_ROUTINE       ; advance to y_scroll_draw_routine_02
    lda X_SCROLL             ; load PPU horizontal scroll
    sta X_SCROLL_TILE_UPDATE
    lda PPUCTRL_SETTINGS
    sta PPUCTRL_TILE_UPDATE
    lda X_DRAW_ROUTINE
    beq @calc_ppu_addr
    lda #$01
    sta X_DRAW_ROUTINE

@calc_ppu_addr:
    lda NT_ROW_SCROLL
    jsr prep_y_scroll_write ; calculate PPU address ($00) and tile update write size
                            ; based on the NT_ROW_SCROLL (a) and X_SCROLL_TILE_UPDATE
    ldy LEVEL_Y_SCREEN
    lda Y_SCROLL_DIR        ; 0 = vertically scrolling up, 1 = vertically scrolling down
    beq @write_supertile    ; branch if scrolling up
    iny                     ; scrolling down, add 1 to LEVEL_Y_SCREEN

@write_supertile:
    sty $11               ; store LEVEL_Y_SCREEN in $11
    lda #$00
    ldy $11
    beq @set_level_screen ; skip multiplication if not needed

; a = LEVEL_Y_SCREEN * LEVEL_WIDTH
@loop:
    clc             ; clear carry in preparation for addition
    adc LEVEL_WIDTH ; add amount to screen number offset
    dey
    bne @loop

; a now contains the offset into the correct row into level_x_screen_layout_tbl
@set_level_screen:
    clc                    ; clear carry in preparation for addition
    adc X_SCREEN           ; add number of screen scrolled horizontally
    sta Y_LAYOUT_OFFSET    ; (DRAW_Y_SCREEN * LEVEL_WIDTH) + DRAW_X_SCREEN
                           ; this is the level_x_screen_layout_tbl offset
    ldy #$00
    beq y_scroll_update_bg ; always branch

y_scroll_draw_routine_02:
    lda #$00
    sta Y_DRAW_ROUTINE      ; set routine to y_scroll_draw_routine_exit
    lda NT_ROW_SCROLL
    clc                     ; clear carry in preparation for addition
    adc #$01
    jsr prep_y_scroll_write ; calculate PPU address ($00) and tile update write size
                            ; based on the NT_ROW_SCROLL (a) and X_SCROLL_TILE_UPDATE
    ldy #$04

; input
;  * y - level_x_supertile_data super-tile start offset (#$00 or #$04)
;  * $02 - number of nametable tiles remaining in nametable row
; behaves very similar to y_scroll_update_attr
y_scroll_update_bg:
    sty $10
    lda NT_ROW_SCROLL ; load how many nametable rows scrolled
    and #$03
    beq @continue     ; branch if at start of a supertile
    tya               ; writing tiles for 2nd, 3rd, or 4th super-tile row
    clc               ; clear carry in preparation for addition
    adc #$08
    sta $10

@continue:
    jsr calc_bg_collision_offset      ; set $14 to bg collision code BG_COLLISION_DATA offset
    lda Y_LAYOUT_OFFSET               ; load level_x_screen_layout_tbl index
    sta $11                           ; set to level_x_screen_layout_tbl offset
    ldy $02                           ; load the number of visible tiles in current nametable row being written
                                      ; writing tiles for one row of the visible portion of the current base nametable
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$06                          ; #$06 = block mode
    sta CPU_GRAPHICS_BUFFER,x
    inx                               ; increment graphics buffer write offset
    lda X_SCROLL_TILE_UPDATE          ; load PPU horizontal scroll
    jsr write_y_lvl_tiles             ; write y level bg tiles for PPU X scroll a and their background collision data at PPU address ($00)
    lda #$00
    sta $15
    lda $14                           ; load BG_COLLISION_DATA offset
    and #$f8
    eor #$80
    sta $14                           ; swap to collision for other nametable horizontally
    inc $11                           ; increment level_x_screen_layout_tbl index
    ldy $03
    beq y_scroll_draw_routine_exit
    lda $00                           ; load PPU address low bye
    and #$e0
    sta $00
    lda $01
    eor #$04
    sta $01
    dex
    jsr write_y_lvl_tiles_no_x_scroll
    lda #$00
    sta $15
    lda $14
    and #$f8
    eor #$80
    sta $14
    inc $11
    ldy $04
    beq y_scroll_draw_routine_exit
    lda $00                           ; load PPU address low byte
    and #$e0
    sta $00
    lda $01                           ; load PPU address high byte
    eor #$04
    sta $01
    dex
    jsr write_y_lvl_tiles_no_x_scroll

y_scroll_draw_routine_exit:
    rts

; calculates PPU address ($00) and tile update write size based on the
; NT_ROW_SCROLL (a) and X_SCROLL_TILE_UPDATE
; input
;  * a - NT_ROW_SCROLL
; output
;  * a
;  * ($00) - 2 byte PPU address
;  * $02 - number of nametable tiles remaining in nametable row
;  * $03 - number of nametable tiles in current nametable scrolled past horizontally
;  * $04 - number of tiles in next horizontal nametable to write
;          #$00 when not writing into next nametable
prep_y_scroll_write:
    sta $00                ; set the nametable row number
    lda SCREEN_SCROLL_TYPE ; 0 = horizontal, 1 = vertical/overhead
    lsr
    lda #$08
    bcc @determine_nt      ; branch if scrolling horizontally
    lda #$00

@determine_nt:
    sta $04                 ; #$00 = vertically scrolling, #$08 = horizontally scrolling
    lda PPUCTRL_TILE_UPDATE ; load PPUCTRL_SETTINGS used while updating tiles
    lsr                     ; push base nametable address (0 = $2000, 1 = $2400) to carry
    lda #$20                ; top left nametable
    bcc @calc_ppu_addr
    lda #$2c                ; bottom right nametable

@calc_ppu_addr:
    sta $01                  ; set PPU address high byte
    lda #$00
    asl $00
    rol
    asl $00
    rol
    asl $00
    rol
    asl $00
    rol                      ; shifting nametable row number 5 bits left, overflowing into $01
    asl $00                  ; same as multiplying nametable row by #$20
    rol                      ; this is because each nametable row is #$20 tiles
    ora $01                  ; merge base nametable address (#$20 or #$2c) with overflow
    sta $01                  ; $00 = PPU address low byte, $01 = PPU address high byte
                             ; now have PPU address of the start of the nametable row
                             ; now calculate address of the offset within the row
    lda X_SCROLL_TILE_UPDATE ; load PPU X scroll
    lsr
    lsr                      ; dividing X pixels scrolled by 8
    lsr                      ; since each tile is 8 pixels wide
    sta $03                  ; set which tile within the row the scroll is at
    lda #$20                 ; #$20 tiles per nametable row
    sec                      ; set carry flag in preparation for subtraction
    sbc $03                  ; subtract tile index within row
    sta $02                  ; set number of tiles in the current visible nametable row
    lda $00                  ; load PPU address low byte
    clc                      ; clear carry in preparation for addition
    adc $03                  ; add number of tiles scrolled horizontally
    sta $00                  ; set adjusted PPU address low byte
    ldy #$00
    lda $03                  ; load number of tiles scrolled horizontally
    clc                      ; clear carry in preparation for addition
    adc $04                  ; add offset (#$00 = vertically scrolling, #$08 = horizontally scrolling)
    cmp #$20                 ; see if wrapped to next nametable row
    bcc @exit                ; branch if didn't wrap to next nametable row
    sbc #$20                 ; wrapped to next nametable row
    tay                      ; set number of remaining tiles in nametable row
    lda #$20                 ; writing #$20 tiles

@exit:
    sta $03 ; set number of graphics tiles to write
    sty $04 ; #$00 - not writing past end of current nametable
            ; set number of tiles in next nametable to write
    rts

; write y level tiles and their background collision data at PPU address ($00)
write_y_lvl_tiles_no_x_scroll:
    lda #$00

; write y level tiles for horizontal scroll a and their background collision data at PPU address ($00)
; input
;  * a - X scroll, used along with NT_ROW_SCROLL, to determine LEVEL_SUPERTILE_PALETTE_DATA read offset
;  * y - size of block of graphics bytes to write
;  * ($00) - 2-byte PPU address to the nametable write location
;  * $10 - offset into super-tile to start reading, i.e. level_x_supertile_data super-tile start offset
;    will be either #$00 or #$04
;  * $11 - level_x_screen_layout_tbl offset specifying screen supertiles
;  * $15 - specifies bg collision nibble, #$00 = low nibble (#$0f), #$80 = high nibble (#$f0)
write_y_lvl_tiles:
    sta $17                             ; store PPU horizontal scroll
    lda $01
    sta CPU_GRAPHICS_BUFFER,x           ; write PPU address high byte
    inx                                 ; increment graphics buffer write offset
    lda $00
    sta CPU_GRAPHICS_BUFFER,x           ; write PPU address low byte
    inx                                 ; increment graphics buffer write offset
    sty $05                             ; transfer graphics block size to $05
    tya
    sta CPU_GRAPHICS_BUFFER,x           ; write graphics block size to buffer
    inx                                 ; increment graphics buffer write offset
    ldy $11
    lda (LEVEL_SCREEN_LAYOUT),y         ; load offset to supertiles for screen y (level_x_screen_layout_tbl offset)
    asl                                 ; double since byte is an offset into level_x_supertiles_screen_ptr_table
                                        ; and each entry in that is a #$02 byte memory address
    tay                                 ; transfer to offset register
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab low-byte to specific level_x_supertiles_screen_xx
    sta $0a
    iny                                 ; increment LEVEL_SCREEN_SUPERTILES_PTR read offset
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab high-byte to specific level_x_supertiles_screen_xx
    sta $0b                             ; ($0a) now stores address to specific level_x_supertiles_screen_xx
                                        ; this is the list of all supertiles for the screen
    lda $17                             ; load PPU horizontal scroll
    lsr
    lsr
    lsr                                 ; divide by 8 to determine tile offset
                                        ; since each tile has 8 pixels
    sta $0f                             ; store nametable row tile offset in $0f
    and #$03
    sta $07                             ; store offset into super-tile [#$00-#$03]
    lda #$04
    sec                                 ; set carry flag in preparation for subtraction
    sbc $07
    sta $06                             ; store number of tiles to the right edge of super-tile
    lda $0f
    lsr
    lsr                                 ; dividing $17 by #$20, the total number of horizontal pixels in a super-tile
    sta $0f                             ; set super-tile offset from left of nametable row
    lda NT_ROW_SCROLL                   ; load nametable row number for top of visible screen
    and #$fc                            ; strip to super-tile row
    asl                                 ; double since a is number of nametable rows and there are 8 super-tiles per row
                                        ; but each super-tile is 4 rows, so to get super-tile offset
                                        ; (nametable_row_number * 8) / 4, which simplifies to nametable_row_number * 2
    clc                                 ; clear carry in preparation for addition
    adc $0f                             ; add number of super-tile from left of nametable for screen
    sta $0f                             ; set level_x_supertiles_screen_xx read offset

@supertile:
    lda #$00
    sta $0c
    ldy $0f
    lda ($0a),y                    ; load supertile index (level_x_supertiles_screen_xx offset)
                                   ; !(OBS) compared to horizontal draw routine,
                                   ; vertical routine does not store supertile index in a variable
    asl
    rol $0c
    asl
    rol $0c
    asl
    rol $0c
    asl
    rol $0c                        ; multiply by #$10 since each supertile is #$10 bytes (#$10 tiles)
    clc                            ; clear carry in preparation for addition
    adc LEVEL_SUPERTILE_DATA_PTR
    sta $08
    lda $0c
    adc LEVEL_SUPERTILE_DATA_PTR+1
    sta $09                        ; set level_x_supertile_data read offset
                                   ; ($08) now points to the beginning of
                                   ; the supertile bytes for the supertile
    lda $10                        ; load level_x_supertile_data start offset for super-tile (#$00, or #$04)
    clc                            ; clear carry in preparation for addition
    adc $07                        ; add horizontal offset within super-tile [#$00-#$03]
    sta $16                        ; set level_x_supertile_data read offset

@write_tile_and_collision:
    ldy $16                   ; load level_x_supertile_data read offset
    lda ($08),y               ; read graphic byte
    sta CPU_GRAPHICS_BUFFER,x ; write pattern table tile to graphics buffer
    inx                       ; increment graphics buffer write offset
    iny                       ; increment read offset
    sty $16                   ; update level_x_supertile_data read offset
    sta $0d                   ; set $0d to pattern table tile byte
    lda $10
    and #$04
    bne @next_tile_byte
    lda $06                   ; load number of tiles to the right edge of super-tile
    lsr
    bcc @write_bg_collision   ; branch if $06 is even, i.e. need to move to bg collision next nibble
    lda $15                   ; move to next bg collision nibble
    eor #$80                  ; i.e. high -> low, or low -> hight
    sta $15                   ; set collision nibble
    bmi @next
    inc $14                   ; finished byte, increment BG_COLLISION_DATA offset

@next:
    jmp @next_tile_byte

; $06 is even (either #$02 or #$04)
@write_bg_collision:
    lda $0d                       ; load pattern table tile byte
    jsr calc_bg_collision_nibble  ; calculate collision code (low or high nibble) for tile a with index into super-tile $06 (#$02 or #$04)
    sta $0d                       ; set to collision code for tile (in either high or low nibble)
    lda $15                       ; load bg collision nibble, #$00 = low nibble (#$0f), #$80 = high nibble (#$f0)
    rol
    rol
    and #$01                      ; bit 7 of $15
    tay
    lda bg_collision_helper_tbl,y ; load correct masking nibble
    ldy $14                       ; load collision data write offset
    and BG_COLLISION_DATA,y       ; strip old collision nibble
    ora $0d                       ; merge with new bg collision nibble
    sta BG_COLLISION_DATA,y       ; update bg collision

@next_tile_byte:
    dec $05                       ; decrement number of tiles to write
    beq @write_end_exit
    dec $06                       ; decrement index into super-tile
    bne @write_tile_and_collision ; branch if not on new super-tile
    inc $0f                       ; new supertile, increment number of super-tiles written
    lda #$00
    sta $07
    lda #$04
    sta $06                       ; reset super-tile index [#$01-#$04]
    bne @supertile                ; always branch

@write_end_exit:
    dec $14                    ; decrement level_x_screen_layout_tbl difference offset for next screen
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x  ; write graphics group end byte
    inx                        ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET ; set graphics buffer write offset
    rts

; updates a row of attribute bytes in up to 3 steps
;  * 1 - update attribute bytes for visible portion of base nametable
;  * 2 - if visible portion spans 2 nametables, update attribute bytes for the
;  visible portion of non-base nametable.  If space to right on same non-base
;  nametable, update 2 attribute tiles to the right (non-visible)
;  * 3 - if the 2 attribute tiles to the right of the visible portion of the
;  non-base nametable overflow back into the base-nametable, then write those
; behaves very similar to y_scroll_calc_update_bg
y_scroll_update_attr:
    lda NT_ROW_SCROLL
    lsr
    eor Y_SCROLL_DIR  ; 0 = vertically scrolling up, 1 = vertically scrolling down
    lsr
    ldy #$f0
    ldx #$0f          ; x and y registers store masks for attribute updates
    lda #$00
    bcc @continue
    lda Y_SCROLL_DIR  ; 0 = vertically scrolling up, 1 = vertically scrolling down
    lsr
    lda LEVEL_WIDTH
    bcc @continue
    eor #$ff
    adc #$00
    ldy #$0f
    ldx #$f0          ; x and y registers store masks for attribute updates

@continue:
    sta $14                           ; store level_x_supertiles_screen_xx offset, specifying which supertile to load
    sty $0c                           ; set mask specifying nibble to keep when updating palette for nametable
    stx $0d                           ; set mask specifying nibble to keep when updating palette for nametable
    lda NT_ROW_SCROLL
    jsr prep_y_scroll_write           ; calculate PPU address ($00) and tile update write size
                                      ; based on the NT_ROW_SCROLL (a) and X_SCROLL_TILE_UPDATE
    jsr get_attr_addr                 ; convert nametable address ($00) into the corresponding attribute table address ($12)
    lda Y_LAYOUT_OFFSET               ; load level_x_screen_layout_tbl index
    sta $11                           ; set to level_x_screen_layout_tbl offset
    ldy $02                           ; load the number of nametable tiles in nametable row being written
    ldx GRAPHICS_BUFFER_OFFSET
    dex                               ; overwriting #$ff end of block-mode graphics marker
                                      ; to insert another write block for the attribute data
                                      ; 2 byte PPU address ($12), attribute data byte(s), and then #$ff
    lda X_SCROLL_TILE_UPDATE          ; load starting point necessary to calculate which palette data to use
    jsr y_scroll_draw_write_attribute ; write attribute byte(s) into graphics buffer at PPU address ($12)
                                      ; this specifies the attribute bytes for the base nametable visible tiles
    ldy $03                           ; load the number of nametable tiles in current nametable scrolled past horizontally
    beq @exit                         ; exit if at left of nametable
    inc $11                           ; not at left edge, need to update at least two nametables worth of attribute bytes
                                      ; increment level_x_screen_layout_tbl offset
    lda $12                           ; load PPU attribute write address low byte
    and #$f8
    sta $12                           ; ensure PPU address is at beginning of attribute table row
    lda $13                           ; load PPU attribute write address high byte
    eor #$04                          ; swap nametable horizontally
    sta $13                           ; i.e. if address is for a left nametable ($23.. or $2b..), make right ($27.. or $2f)
                                      ; if address is for a right nametable ($27.. or $2f..), make left ($23.. or $2b..)
    dex                               ; decrement graphics buffer to overwrite #$ff end of block-mode graphics marker
                                      ; prepping for writing attribute bytes
    jsr @write_attribute              ; write attribute byte(s) into graphics buffer at PPU address ($12)
                                      ; which points to the starting at left edge of non-visible nametable
    ldy $04                           ; load any additional writes on non-visible portion of base nametable
                                      ; this area is about to be scrolled to
    beq @exit                         ; exit if no need to write additional attribute byte(s)
    inc $11                           ; already written visible portion of current nametable,
                                      ; and written non-base nametable visible portion
                                      ; now need to write non-visible portion of base nametable
                                      ; increment level_x_screen_layout_tbl offset
    lda $12
    and #$f8
    sta $12                           ; ensure PPU address is at beginning of attribute table row
    lda $13                           ; swap nametable horizontally
    eor #$04
    sta $13                           ; swap back to base nametable
    dex                               ; decrement graphics buffer to overwrite #$ff end of block-mode graphics marker
    jsr @write_attribute              ; write attribute byte(s) into graphics buffer at PPU address ($12)
                                      ; which points to the starting at left edge of current base nametable

@exit:
    rts

@write_attribute:
    lda #$00 ; writing attribute bytes starting at left edge of nametable

; write attribute byte(s) into graphics buffer at PPU address ($12)
; input
;  * a - X scroll, used along with NT_ROW_SCROLL, to determine LEVEL_SUPERTILE_PALETTE_DATA read offset
;  * x - CPU_GRAPHICS_BUFFER write offset
;  * y - number of nametable tiles in nametable row being written
;        used to calculate graphics buffer write block size for attribute table
;        [(y - 1) / 4] + 1
;  * ($12) - 2-byte PPU address to the attribute table write location
;  * $0d - which nibble to focus on high/low (#$0f or #$f0)
y_scroll_draw_write_attribute:
    sta $17
    dey
    tya
    lsr
    lsr
    tay
    iny                                 ; set graphics buffer block size to [(y - 1) / 4] + 1
    lda $13                             ; load PPU address high byte
    sta CPU_GRAPHICS_BUFFER,x           ; set PPU address high byte
    inx                                 ; increment graphics buffer write offset
    lda $12                             ; load PPU address low byte
    sta CPU_GRAPHICS_BUFFER,x           ; set PPU address low byte
    inx                                 ; increment graphics buffer write offset
    sty $05                             ; set to block size
    tya                                 ; transfer block size to a
    sta CPU_GRAPHICS_BUFFER,x           ; write block size (number of bytes to write to buffer)
    inx                                 ; increment graphics buffer write offset
    ldy $11                             ; load level_x_screen_layout_tbl offset
    lda (LEVEL_SCREEN_LAYOUT),y         ; load pointer to supertiles for specified screen y
    asl                                 ; double since byte is an offset into level_x_supertiles_screen_ptr_table
                                        ; and each entry in that is a #$02 byte memory address
    tay                                 ; transfer to offset register
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab low-byte to specific level_x_supertiles_screen_xx
    sta $0a
    iny                                 ; increment LEVEL_SCREEN_SUPERTILES_PTR read offset
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab high-byte to specific level_x_supertiles_screen_xx
    sta $0b                             ; ($0a) now stores address to specific level_x_supertiles_screen_xx
                                        ; this is the list of all supertiles for the screen
    lda $11                             ; load level_x_screen_layout_tbl offset
    clc                                 ; clear carry in preparation for addition
    adc $14                             ; add level_x_screen_layout_tbl offset for next screen
    tay                                 ; transfer to offset register
    lda (LEVEL_SCREEN_LAYOUT),y         ; load pointer to supertiles for specified screen y
    asl                                 ; double since byte is an offset into level_x_supertiles_screen_ptr_table
                                        ; and each entry in that is a #$02 byte memory address
    tay                                 ; transfer to offset register
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab low-byte to specific level_x_supertiles_screen_xx
    sta $15
    iny                                 ; increment LEVEL_SCREEN_SUPERTILES_PTR read offset
    lda (LEVEL_SCREEN_SUPERTILES_PTR),y ; grab high-byte to specific level_x_supertiles_screen_xx
    sta $16                             ; ($15) now stores address to specific level_x_supertiles_screen_xx
                                        ; this is the list of all supertiles for the screen
    lda $17                             ; calculating level_x_supertiles_screen_xx offset
    lsr
    lsr
    lsr
    lsr
    lsr                                 ; divide by #$20
    sta $0f
    lda NT_ROW_SCROLL
    and #$fc
    asl
    clc                                 ; clear carry in preparation for addition
    adc $0f                             ; add NT_ROW_SCROLL
    sta $0f                             ; set new screen supertile index
    lda SUPERTILE_ATTRIBUTE_OVERRIDE    ; see if level had a door that was destroyed
                                        ; only level 2 has additional palette data to load
                                        ; after a door is destroyed
    bne @palette_override

@write_palette_loop:
    ldy $0f                              ; load screen supertile index
    lda ($0a),y                          ; load supertile index byte (level_x_supertiles_screen_xx offset)
    tay                                  ; transfer supertile index to offset register
    lda (LEVEL_SUPERTILE_PALETTE_DATA),y ; load palette for supertile index (level_x_palette_data offset)
    and $0c                              ; strip to high or low nibble
    sta $0e                              ; store temporarily in $0e
    ldy $0f                              ; load screen supertile index
    lda ($15),y                          ; load supertile index byte (level_x_supertiles_screen_xx offset)
    tay                                  ; transfer supertile index to offset register
    lda (LEVEL_SUPERTILE_PALETTE_DATA),y ; load palette for supertile index (level_x_palette_data offset)
    and $0d                              ; strip to high or low nibble
    ora $0e                              ; merge with other nibble
    sta CPU_GRAPHICS_BUFFER,x            ; write supertile palette byte to graphics buffer
    inx                                  ; increment graphics buffer write offset
    inc $0f                              ; increment level screen supertiles index read offset
    dec $05                              ; decrement number of graphics bytes to write (block size)
    bne @write_palette_loop              ; branch if more palette bytes to write

@mark_end:
    lda #$ff                   ; end of data byte
    sta CPU_GRAPHICS_BUFFER,x  ; write end of block mode data byte
    inx                        ; increment graphics buffer write offset
    stx GRAPHICS_BUFFER_OFFSET ; set current graphics buffer write offset
    rts

; runs after a door (enemy type #$0d) is destroyed and palette data is loaded
; only ever run for level 2 after boss defeat scroll
; almost identical to @write_palette_loop, but with a call to possibly override
; for both attribute nibbles
@palette_override:
    ldy $0f                              ; load screen supertile index
    lda ($0a),y                          ; load supertile index byte (level_x_supertiles_screen_xx offset)
    tay                                  ; transfer supertile index to offset register
    lda (LEVEL_SUPERTILE_PALETTE_DATA),y ; load palette for supertile index (level_x_palette_data offset)
    jsr overwrite_attribute_byte         ; override if necessary
    and $0c                              ; strip to high or low nibble
    sta $0e                              ; store temporarily in $0e
    ldy $0f                              ; load screen supertile index
    lda ($15),y                          ; load supertile index byte (level_x_supertiles_screen_xx offset)
    tay                                  ; transfer supertile index to offset register
    lda (LEVEL_SUPERTILE_PALETTE_DATA),y ; load palette for supertile index (level_x_palette_data offset)
    jsr overwrite_attribute_byte         ; override if necessary
    and $0d                              ; strip to high or low nibble
    ora $0e                              ; merge with other attribute nibble
    sta CPU_GRAPHICS_BUFFER,x            ; write attribute byte (palette)
    inx                                  ; increment graphics buffer write offset
    inc $0f                              ; increment level screen supertiles index read offset
    dec $05                              ; decrement number of graphics bytes to write (block size)
    bne @palette_override                ; branch if more palette bytes to write
    beq @mark_end                        ; always branch

; if a value from the ATTRIBUTE_OVERRIDES byte array is populated with non-zero
; attribute bytes, then the supertile indexes matching the index in
; attribute_overrides_tbl will be overwritten
; e.g., if ATTRIBUTE_OVERRIDES+3 is $aa, then supertile index #$af's attribute
; byte will be replaced with $aa since attribute_overrides_tbl+3 is #$af
; input
;  * a - palette for supertile index (level_x_palette_data offset)
;  * y - supertile index byte (level_x_supertiles_screen_xx offset)
; output
;  * a - palette byte data nibble
overwrite_attribute_byte:
    sty $00  ; store supertile index (level_x_supertiles_screen_xx offset)
    sta $01  ; store original supertile attribute byte
    tya      ; transfer supertile index to a
    ldy #$09 ; start at end of table

@loop:
    cmp attribute_overrides_tbl,y ; compare supertile index to replace list
    beq @continue                 ; branch if found palette byte that matches a
                                  ; which means that it can be be overwritten
    dey                           ; look at next palette offset
    bpl @loop
    bmi @exit_not_found

@continue:
    lda ATTRIBUTE_OVERRIDES,y ; see if palette byte has an override
    bne @exit                 ; exit if found palette override byte

@exit_not_found:
    lda $01 ; unable to find restore original palette byte data

@exit:
    rts

; a table of supertile indexes that can be overwritten
attribute_overrides_tbl:
    .byte $89,$8d,$ac,$af,$c1,$c2,$ec,$ed,$ee,$ef

bg_collision_helper_tbl:
    .byte $0f,$f0

graphics_buffer_full:
    rts

; updates nametable supertile (4x4 tile) at (a, y) and possibly its palette
; input
;  * a - X position
;  * y - Y position
;  * $08 - supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space
load_banks_update_supertile_if_room:
    ldx GRAPHICS_BUFFER_OFFSET                ; load graphics buffer offset
    cpx #$30                                  ; see if graphics buffer is full for frame
    bcs graphics_buffer_full                  ; exit if graphics buffer is full
    ldx #$f8
    jmp load_banks_update_nametable_supertile ; update nametable supertile (4x4 tile) at (a, y)

; updates nametable supertile (4x4 tile) at (a, y) and the supertile's palette
; input
;  * a - X position
;  * y - Y position
;  * $08 - is the supertile to draw (level_x_supertile_data offset)
; output
;  * carry flag - set when unable to update nametable supertile due to not enough space
load_banks_update_supertile_and_palette_if_room:
    ldx GRAPHICS_BUFFER_OFFSET                  ; load graphics buffer offset
    cpx #$30                                    ; see if graphics buffer is full for frame
    bcs graphics_buffer_full                    ; exit if graphics buffer is full
    jmp load_banks_update_supertile_and_palette ; update nametable supertile (4x4 tile) and its palette at (a, y)

; updates nametable supertile (4x4 tile) at (a, y) and possibly its palette
; input
;  * a - X position
;  * y - Y position
;  * $06 - when #$e0, specifies to update the palette for the supertile based on palette offset $08
;  * $08 - supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
update_nametable_supertile:
    ldx GRAPHICS_BUFFER_OFFSET ; load current graphics buffer offset
    clc                        ; clear carry in preparation for addition
    adc X_SCROLL               ; add horizontal scroll to X position
    sta $00                    ; set PPU address write low byte
    lda PPUCTRL_SETTINGS
    bcc @continue              ; branch if no overflow when calculating sum of horizontal scroll and X position
    eor #$01                   ; overflow, flip bit 0 (base nametable low nibble)

@continue:
    and #$01              ; strip PPUCTRL_SETTINGS to base nametable address low nibble
    ora #$08              ; set bit 3
    sta $01               ; set PPU address write high byte
    lda $00               ; load X position plus horizontal screen offset
    and $06               ; keep bits specified in $06 (#$e0 or #$f8)
                          ; strips or keeps the low nibble of the PPU address low byte
    lsr
    lsr
    lsr
    sta $00               ; set PPU address write low byte
    lsr
    lsr
    sta $02
    tya                   ; load Y position
    clc                   ; clear carry in preparation for addition
    adc Y_SCROLL          ; add vertical screen offset to Y position
    bcs @handle_overflow  ; branch if overflow
    cmp #$f0              ; see if at bottom of nametable
    bcc @calc_ppu_address ; branch if not at the bottom the nametable

; Y + Y_SCROLL overflow into next nametable, add base offset of #$10
@handle_overflow:
    clc      ; clear carry in preparation for addition
    adc #$10

@calc_ppu_address:
    and $06
    sta $03
    asl
    rol $01                        ; rotate PPU address write high byte
    asl
    rol $01                        ; rotate PPU address write high byte
    ora $00                        ; merge with PPU address write low byte
    sta $00                        ; set PPU address write low byte
    lda $03
    lsr
    lsr
    ora $02
    ora #$c0
    sta $02
    lda $01                        ; load PPU address write high byte
    ora #$03
    sta $03
    lda #$00
    sta $09                        ; initialize value used to be offset from LEVEL_SUPERTILE_DATA_PTR
    lda $08                        ; load supertile data offset (LEVEL_SUPERTILE_DATA_PTR offset)
    asl
    rol $09
    asl
    rol $09
    asl
    rol $09
    asl
    rol $09
    adc LEVEL_SUPERTILE_DATA_PTR   ; calculate supertile data address low byte
    sta $0a                        ; store supertile data address low byte
    lda $09
    adc LEVEL_SUPERTILE_DATA_PTR+1 ; add supertile data address high byte
    sta $0b                        ; store supertile data address high byte
    lda #$06                       ; byte 0 = #$06 (block mode)
                                   ; byte 1 and 2 will be PPU address
                                   ; byte 3 is length, and bytes 4 to (byte 4 + length) are written to PPU
    sta CPU_GRAPHICS_BUFFER,x      ; set graphics format to block mode
    inx
    ldy #$04                       ; write 4 rows of nametable data, each row updating 4 tiles (one supertile in total)
    lda $00                        ; load PPU address write low byte
    bpl @write_nametable_rows      ; branch PPU address low byte is low enough to not cause concern with going outside nametable
    lda $01                        ; low address is >= #$80, check if PPU address high byte is close to bottom of $2c00 nametable
                                   ; load PPU address write high byte
    and #$03
    cmp #$03                       ; see if next row's nametable address is invalid
    bne @write_nametable_rows      ; branch if address is still below $3000, indicating a valid address
    ldy #$02                       ; updating entire supertile would write past end of nametable, just update 2 rows

@write_nametable_rows:
    sty $04  ; set number of rows to write
    ldy #$00

@write_row:
    lda $01                   ; load PPU address write high byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address write high byte
    inx                       ; increment graphics buffer write offset
    lda $00                   ; load PPU address write low byte
    sta CPU_GRAPHICS_BUFFER,x ; set PPU address write low byte
    inx                       ; increment graphics buffer write offset
    lda #$04                  ; set length of data to write to 4 bytes
    sta $05                   ; set length counter
    sta CPU_GRAPHICS_BUFFER,x ; write number of bytes in graphics group
    inx                       ; increment graphics buffer write offset

@write_loop:
    lda ($0a),y               ; load graphics buffer byte
    sta CPU_GRAPHICS_BUFFER,x ; write graphics buffer byte
    iny                       ; increment supertile read offset
    inx                       ; increment graphics buffer write offset
    dec $05                   ; decrement graphics block length
    bne @write_loop           ; branch to write next block if more bytes to write
    lda $00                   ; written all bytes in graphic group, load original PPU address write low byte
    clc                       ; clear carry in preparation for addition
    adc #$20                  ; move down to next row in nametable
    sta $00                   ; update PPU address write low byte to be next row down
    bcc @check_next_row
    inc $01                   ; overflow on PPU address low byte, increment high byte

@check_next_row:
    dec $04                              ; decrement remaining number of nametable rows to write
    bne @write_row                       ; branch to write next row (4 tiles) of the supertile if more to write
    lda $06                              ; written all rows, see if should update palette for supertile
    cmp #$e0                             ; when $06 is #$e0, specifies to update palette for supertile
    bne @write_buffer_end_exit           ; branch if not updating palette to write #$ff (no more data) to buffer and exit
                                         ; $06 is #$e0
                                         ; this means to update the palette for the supertile to palette offset $08
    lda $03                              ; load PPU write address high byte
    sta CPU_GRAPHICS_BUFFER,x            ; write PPU write address high byte
    inx                                  ; increment graphics buffer write offset
    lda $02                              ; load PPU write address low byte
    sta CPU_GRAPHICS_BUFFER,x            ; write PPU write address low byte
    inx                                  ; increment graphics buffer write offset
    lda #$01                             ; setting length of data to write to 1 byte (palette byte)
    sta CPU_GRAPHICS_BUFFER,x            ; write length of graphic data
    inx                                  ; increment graphics buffer write offset
    ldy $08                              ; load palette data offset
    lda (LEVEL_SUPERTILE_PALETTE_DATA),y ; load palette for supertile
    sta CPU_GRAPHICS_BUFFER,x            ; set palette for supertile
    inx                                  ; increment graphics buffer write offset

@write_buffer_end_exit:
    lda #$ff
    sta CPU_GRAPHICS_BUFFER,x  ; write end of graphics byte
    inx                        ; increment graphics buffer offset
    stx GRAPHICS_BUFFER_OFFSET ; set new graphics buffer offset
    clc                        ; clear carry
    rts

clear_y_scroll:
    lda #$00
    sta Y_SCROLL           ; set PPU vertical scroll to top of nametables (no scroll)
    sta SCREEN_SCROLL_TYPE ; set horizontal scrolling

reset_scroll_draw_point:
    lda #$00
    sta X_SCROLL              ; set PPU horizontal scroll to no scroll
    sta NT_MIRRORING          ; set vertical mirroring
    sta PAUSE_PALETTE_UPDATES ; un-pause palette updates

reset_draw_point:
    lda #$00
    sta NT_CURRENT_COLUMN
    lda #$e0
    sta X_SCROLL_DRAW_POINT
    dec SCREENS_DRAWN
    rts

load_level_header:
    lda CURRENT_LEVEL                      ; load current level
    asl                                    ; double since each entry is a #$02 byte memory address
    tay                                    ; transfer to offset register
    lda supertiles_screen_ptr_tbl,y
    sta LEVEL_SCREEN_SUPERTILES_PTR
    lda supertiles_screen_ptr_tbl+1,y
    sta LEVEL_SCREEN_SUPERTILES_PTR+1
    lda supertile_data_ptr_tbl,y
    sta LEVEL_SUPERTILE_DATA_PTR
    lda supertile_data_ptr_tbl+1,y
    sta LEVEL_SUPERTILE_DATA_PTR+1
    lda supertile_palette_data_ptr_tbl,y
    sta LEVEL_SUPERTILE_PALETTE_DATA
    lda supertile_palette_data_ptr_tbl+1,y
    sta LEVEL_SUPERTILE_PALETTE_DATA+1
    lda screen_layout_ptr_tbl+1,y
    sta LEVEL_SCREEN_LAYOUT+1
    sta $09
    lda screen_layout_ptr_tbl,y
    sta $08
                                           ; first two bytes are LEVEL_WIDTH and LEVEL_HEIGHT
    clc                                    ; clear carry in preparation for addition
    adc #$02                               ; add 2 bytes to level_x_screen_layout_tbl pointer to skip address
    sta LEVEL_SCREEN_LAYOUT
    bcc @continue                          ; branch if no carry
    inc LEVEL_SCREEN_LAYOUT+1              ; overflow, add one to address high byte

@continue:
    ldy #$00
    lda ($08),y
    sta LEVEL_WIDTH   ; set the number of horizontal screens the level has
    iny
    lda ($08),y
    sta LEVEL_HEIGHT  ; set the number of vertical screens the level has
                      ; together these define a rectangle of screens for the level
                      ; not all screens are accessible
    lda CURRENT_LEVEL ; load current level
    asl
    asl
    asl
    asl
    tay
    ldx #$00

@set_lvl_collision_code_indices:
    lda lvl_collision_code_index_tbl,y
    sta COLLISION_CODE_TILE_INDICES,x
    iny
    inx
    cpx #$10
    bcc @set_lvl_collision_code_indices
    rts

; each #$10 bytes is a level's worth of collision codes
; see COLLISION_CODE_TILE_INDICES
lvl_collision_code_index_tbl:
    .byte $a0,$a0,$f8,$f8,$f8,$f8,$fa,$fc,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $1a,$30,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $27,$27,$42,$42,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $27,$2c,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $2c,$2e,$36,$36,$36,$36,$37,$38,$39,$3b,$3d,$3f,$ff,$ff,$ff,$ff
    .byte $21,$58,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $17,$17,$45,$4d,$4d,$4d,$4f,$51,$52,$54,$56,$57,$ff,$ff,$ff,$ff
    .byte $40,$50,$60,$60,$60,$60,$62,$64,$65,$66,$68,$6a,$ff,$ff,$ff,$ff

; loads appropriate tiles, pallettes, auto-scroll for position in level
level_run_tile_routines:
    lda CURRENT_LEVEL            ; load current level
    asl                          ; double since each entry is a #$02 byte memory address
    tay                          ; transfer to offset register
    lda tile_routine_ptr_tbl,y
    sta $00
    lda tile_routine_ptr_tbl+1,y
    sta $01
    jmp ($00)

tile_routine_ptr_tbl:
    .addr level_1_run_tile_routines
    .addr level_2_run_tile_routines
    .addr level_3_run_tile_routines
    .addr level_4_run_tile_routines
    .addr level_5_run_tile_routines
    .addr level_6_run_tile_routines
    .addr level_7_run_tile_routines
    .addr level_8_run_tile_routines

screen_layout_ptr_tbl:
    .addr level_1_screen_layout_tbl ; bank 4
    .addr level_2_screen_layout_tbl ; bank 1
    .addr level_3_screen_layout_tbl ; bank 7
    .addr level_4_screen_layout_tbl ; bank 6
    .addr level_5_screen_layout_tbl ; bank 8
    .addr level_6_screen_layout_tbl ; bank 9
    .addr level_7_screen_layout_tbl ; bank a
    .addr level_8_screen_layout_tbl ; bank b

supertiles_screen_ptr_tbl:
    .addr level_1_supertiles_screen_ptr_table ; bank 4
    .addr level_2_supertiles_screen_ptr_table ; bank 1
    .addr level_3_supertiles_screen_ptr_table ; bank 7
    .addr level_4_supertiles_screen_ptr_table ; bank 6
    .addr level_5_supertiles_screen_ptr_table ; bank 8 and 9
    .addr level_6_supertiles_screen_ptr_table ; bank 9
    .addr level_7_supertiles_screen_ptr_table ; bank a
    .addr level_8_supertiles_screen_ptr_table ; bank b

supertile_data_ptr_tbl:
    .addr level_1_supertile_data ; bank 5
    .addr level_2_supertile_data ; bank 1
    .addr level_3_supertile_data ; bank 7
    .addr level_4_supertile_data ; bank 6
    .addr level_5_supertile_data ; bank 9
    .addr level_6_supertile_data ; bank 9
    .addr level_7_supertile_data ; bank a
    .addr level_8_supertile_data ; bank b

supertile_palette_data_ptr_tbl:
    .addr level_1_palette_data ; bank 5
    .addr level_2_palette_data ; bank 1
    .addr level_3_palette_data ; bank 7
    .addr level_4_palette_data ; bank 7
    .addr level_5_palette_data ; bank 9
    .addr level_6_palette_data ; bank 9
    .addr level_7_palette_data ; bank b
    .addr level_8_palette_data ; bank b

; bank 4 addresses
level_enemy_screen_ptr_ptr_tbl:
    .addr level_1_enemy_screen_ptr_tbl ; level 1
    .addr level_2_enemy_screen_ptr_tbl ; level 2
    .addr level_3_enemy_screen_ptr_tbl ; level 3
    .addr level_4_enemy_screen_ptr_tbl ; level 4
    .addr level_5_enemy_screen_ptr_tbl ; level 5
    .addr level_6_enemy_screen_ptr_tbl ; level 6
    .addr level_7_enemy_screen_ptr_tbl ; level 7
    .addr level_8_enemy_screen_ptr_tbl ; level 8

; fire near or at player $0a (randomly miss)
; input
;  * $0a - player index to target, 0 = player 1, 1 = player 2
;  * y - enemy bullet speed code (see bullet_velocity_adjust_ptr_tbl)
; output
;  * zero flag - set when bullet created, clear when unable to create
fire_near_player:
    sty $06                   ; set bullet speed code
    lda #$00
    sta $00                   ; set bullet angle value (see enemy_fract_vel_dir_lookup_tbl)
    lda #$01
    sta $0f                   ; set to use quadrant_aim_dir_01
    jsr find_alive_player_pos ; find player's position ($0b,$0a) of player in the normal state (PLAYER_STATE = 2)
                              ; starting with the closest player specified in $0a
                              ; if no available player, set bottom center of screen
    lda RANDOM_NUM            ; load random number
    and #$03
    tay
    lda target_y_adj_tbl,y    ; adding a random adjustment from player Y position
    clc                       ; clear carry in preparation for addition
    adc $0a                   ; add player's Y position
    pha                       ; push Y target position to stack
    ror                       ; push carry to bit 7
    eor target_y_adj_tbl,y
    asl
    pla                       ; restore Y target position
    bcs @continue             ; branch if negative Y adjustment and no overflow adjusting Y position or
                              ; non-negative y adjustment and overflow adjusting Y position
                              ; to use player Y position instead of adjusted Y position
    sta $0a                   ; use adjusted target Y position

@continue:
    lda RANDOM_NUM         ; load random number
    lsr
    lsr
    and #$03
    tay
    lda target_x_adj_tbl,y ; adding a random adjustment from player X position
    clc                    ; clear carry in preparation for addition
    adc $0b                ; add player's X position
    pha                    ; restore X target position
    ror
    eor target_x_adj_tbl,y
    asl
    pla
    bcs @get_aim_fire      ; branch if negative X adjustment and no overflow adjusting X position or
                           ; non-negative X adjustment and overflow adjusting X position
                           ; to use player X position instead of adjusted X position
    sta $0b                ; set target X position

@get_aim_fire:
    jsr get_quadrant_aim_dir         ; get aim direction code for target ($0b, $0a) from location ($09, $08) using table code $0f
    jmp create_bullet_if_appropriate ; create enemy bullet (type a) at ($09, $08) in quadrant $07 and speed $06

; fires regular (bullet type = #$00) bullet at player index specified in $0a (0 = p1, 1 = p2) from ($09, $08)
; using speed code #$02 (1x speed)
; input
;  * $09 - enemy X position
;  * $08 - enemy Y position
;  * $0a - the closest player to the enemy (0 = p1, 1 = p2)
; output
;  * $11 - created bullet enemy slot
;  * zero flag - set when bullet created, clear when unable to create
fire_bullet_at_player_1x_speed:
    ldy #$02

; fires regular (bullet type = #$00) bullet at player index specified in $0a (0 = p1, 1 = p2) from ($09, $08)
; input
;  * $0a - player index to target (0 = p1, 1 = p2)
;    if player with index $0a isn't in normal state (#$02), then attempts to
;    target other player. If neither player is in a normal state, targets bottom
;    center of screen (#$80, #$ff)
;  * y - bullet speed code
;  * $09 - enemy X position
;  * $08 - enemy Y position
;  * $0a - the closest player to the enemy (0 = p1, 1 = p2)
; output
;  * $11 - created bullet enemy slot
;  * zero flag - set when bullet created, clear when unable to create
fire_bullet_at_player:
    lda #$00
    sty $06                             ; store enemy bullet speed code
    sta $00                             ; set bullet type
    lda #$01
    sta $0f
    jsr get_quadrant_aim_dir_for_player ; set a to the aim direction within a quadrant
                                        ; based on source position ($09, $08) targeting player index $0a

; input
;  * a - (...x xxxx) specifies bullet angle value (see enemy_fract_vel_dir_lookup_tbl)
;  * $00 - (...x xxxx) specifies bullet angle value (see enemy_fract_vel_dir_lookup_tbl)
;  * $06 - enemy bullet speed code (see bullet_velocity_adjust_ptr_tbl)
;  * $08 - Y position
;  * $09 - X position
;  * $07 - specifies quadrant to aim in (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
;    * bit 0 - 0 = bottom half of plane (quadrants III and IV), 1 = top half of plane (quadrants I and II)
;    * bit 1 - 0 = right half of the plan (quadrants I and IV), 1 = left half of plane (quadrants II and III)
; output
;  * zero flag - set when bullet created, clear when unable to create
create_bullet_if_appropriate:
    ora $00                 ; merge enemy bullet quadrant aim dir with bullet type code
    sta $0a                 ; store bullet type and bullet velocity in $0a
    jmp create_enemy_bullet ; create enemy bullet (type $0a) at ($09, $08) in quadrant $07 and speed $06

target_y_adj_tbl:
    .byte $00,$10,$f0,$f8

target_x_adj_tbl:
    .byte $f0,$10,$20,$f8

; !(UNUSED) dead code that creates a bullet
; input
;  * a - (...x xxxx) specifies bullet angle value (see enemy_fract_vel_dir_lookup_tbl)
;  * y - enemy bullet speed code (see bullet_velocity_adjust_ptr_tbl)
bank_f_unused_03:
    asl
    sty $06            ; set enemy bullet speed code
    sta $0a            ; set bullet angle value
    and #$1f
    ldy #$00
    cmp #$07           ; compare bullet angle to #$07
    bcc @create_bullet
    cmp #$12
    bcs @create_bullet
    ldy #$02           ; bullet angle between #$07 and #$12

@create_bullet:
    cmp #$0d
    bcc @set_quad_create_bullet
    iny

@set_quad_create_bullet:
    sty $07 ; set quadrant to aim in (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)

; create enemy bullet, either overhead bullet (enemy type #$0e) or regular (enemy type #$02) depending on OVERHEAD_FLAG
; input
;  * $08 - Y position
;  * $09 - X position
;  * $0a - (...x xxxx) specifies bullet angle value (see enemy_fract_vel_dir_lookup_tbl)
;  * $06 - enemy bullet speed code (see bullet_velocity_adjust_ptr_tbl)
;  * $07 - specifies quadrant to aim in (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
;    * bit 0 - 0 = bottom half of plane (quadrants III and IV), 1 = top half of plane (quadrants I and II)
;    * bit 1 - 0 = right half of the plan (quadrants I and IV), 1 = left half of plane (quadrants II and III)
; output
;  * zero flag - set when bullet created, clear when unable to create
create_enemy_bullet:
    stx ENEMY_CURRENT_SLOT
    ldy #$02               ; enemy type #$02 - bullet
    lda OVERHEAD_FLAG      ; (0 = side view, 1 = overhead view)
    beq @create_enemy
    ldy #$0e               ; enemy type #$0e - overhead level bullet

@create_enemy:
    jsr create_enemy_y ; create enemy of type y in first available slot
    bne @exit          ; branch if unable to create the enemy
    lda $06            ; load bullet speed code
    cmp #$07
    bcc @continue
    lda #$07

@continue:
    sta $06                     ; store speed code in $06
    lda $08                     ; load created bullet enemy Y position
    sta ENEMY_Y_POS,x           ; set created bullet enemy Y position
    lda $09                     ; load created bullet enemy Y position
    sta ENEMY_X_POS,x           ; set created bullet enemy X position
    lda $0a
    and #$1f                    ; keep bits ...x xxxx (quadrant aim dir)
    jsr calc_velocities_for_dir ; determine velocity based on quadrant aim dir (a), quadrant ($07), and speed adjust code ($06)
    jsr set_enemy_velocity      ; sets bullet velocity based on $05, $04, $0b, and $0a
    stx $11
    ldx $10
    lda #$00
    rts

@exit:
    ldx $10
    lda #$01
    rts

; set the enemy velocities, usually bullets or things like red bubble
; input
;  * $05 - enemy Y fast velocity
;  * $04 - enemy Y fractional velocity
;  * $0b - enemy X fast velocity
;  * $0a - enemy X fractional velocity
set_enemy_velocity:
    lda $05
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    lda $04
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    lda $0b
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    lda $0a
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    rts

; determine the bullet velocities based on quadrant aim dir (a) and quadrant ($07)
; input
;  * a - quadrant aim dir (see enemy_fract_vel_dir_lookup_tbl)
;  * $07 - specifies quadrant to aim in (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
;    * bit 0 - 0 = bottom half of plane (quadrants III and IV), 1 = top half of plane (quadrants I and II)
;    * bit 1 - 0 = right half of the plan (quadrants I and IV), 1 = left half of plane (quadrants II and III)
;  * $06 - speed adjust code [#$00-#$07], e.g. speed adjust code #$01 is .75 speed (see bullet_velocity_adjust_ptr_tbl)
; output
;  * $04 - Y fractional velocity value
;  * $05 - Y velocity fast value
;  * $0a - X fractional velocity value
;  * $0b - X velocity fast value
calc_velocities_for_dir:
    tay                                  ; store quadrant aim direction in y
    lda enemy_fract_vel_dir_lookup_tbl,y
    tay                                  ; transfer result to offset register
    lda enemy_fract_vel_tbl+1,y          ; load the fractional X velocity
    sta $04                              ; store X fractional velocity byte
    lda #$00                             ; set X velocity fast value to #$00
    sta $05                              ; store X velocity fast value
    jsr adjust_velocity                  ; adjust X velocity based on speed adjust code ($06)
    lda $04                              ; load adjusted X fractional velocity byte
    sta $0a                              ; set adjusted X fractional velocity byte
    lda $05                              ; load adjusted X velocity fast value
    sta $0b                              ; set adjusted X velocity fast value
    lda enemy_fract_vel_tbl,y            ; load Y fractional velocity
    sta $04                              ; set Y fractional velocity
    lda #$00                             ; load Y fast velocity
    sta $05                              ; set  Y fast velocity
    jsr adjust_velocity                  ; adjust X velocity based on speed adjust code ($06)
    lda $07                              ; load direction
    lsr                                  ; puts bit 0 to carry (up down bit)
    bcc @set_x_vel                       ; branch if moving down
    lda #$00                             ; moving up, flip Y velocity so enemy travels up instead of down
    sec                                  ; set carry flag in preparation for subtraction
    sbc $04                              ; #$00 - $04
    sta $04                              ; update Y fractional velocity to be negative
    lda #$00                             ; a = #$00
    sbc $05                              ; #$00 - $05
    sta $05                              ; update Y velocity fast to be negative

@set_x_vel:
    lda $07   ; load direction
    lsr
    lsr
    bcc @exit ; exit if firing right
    lda #$00  ; firing left, a = #$00
    sec       ; set carry flag in preparation for subtraction
    sbc $0a   ; negate X fractional velocity
    sta $0a   ; set X fractional velocity value
    lda #$00  ; a = #$00
    sbc $0b   ; negate X fast velocity
    sta $0b   ; set X velocity fast value

@exit:
    rts

; table of enemy (usually bullet) fractional velocity indexes based on quadrant aim direction (#$18 bytes)
; offsets into enemy_fract_vel_tbl
; [#$00-#$17] #$00 is pointing right as value increments direction goes clockwise
; #$00 right, #$03 is down, #$06 left, #$09 is straight up
enemy_fract_vel_dir_lookup_tbl:
    .byte $00,$02,$04,$06,$08,$0a ; quadrant IV
    .byte $0c,$0a,$08,$06,$04,$02 ; quadrant III
    .byte $00,$02,$04,$06,$08,$0a ; quadrant II
    .byte $0c,$0a,$08,$06,$04,$02 ; quadrant I

; table for X and Y fractional velocities, based on index specified from enemy_fract_vel_dir_lookup_tbl (#$d bytes)
; byte 0 - Y fractional velocity
; byte 1 - X fractional velocity
enemy_fract_vel_tbl:
    .byte $00,$ff
    .byte $42,$f7
    .byte $80,$dd
    .byte $b5,$b5
    .byte $dd,$80
    .byte $f7,$42
    .byte $ff,$00 ; shooting horizontally

; adjusts enemy (usually bullet) X or Y velocity based on speed code (#$00-#$07)
; e.g. bullet speed code #$01 is .75 speed
; assumes fast velocity will always be #$00, otherwise, math won't work in all cases
; input
;  * $04 - fractional velocity value (either x dir or y dir)
;  * $05 - velocity fast value (either x dir or y dir)
;  * $06 - speed adjust code [#$00-#$07], e.g. speed adjust code #$01 is .75 speed
; output
;  * $04 - fractional velocity value (either x dir or y dir)
;  * $05 - velocity fast value (either x dir or y dir)
adjust_velocity:
    lda $06                        ; speed (0-7)
    jsr run_routine_from_tbl_below

; pointer table for bullet speeds
bullet_velocity_adjust_ptr_tbl:
    .addr bullet_velocity_adjust_00 ; .5x speed
    .addr bullet_velocity_adjust_01 ; .75x speed
    .addr bullet_velocity_adjust_02 ; normal speed
    .addr bullet_velocity_adjust_03 ; 1.25x speed
    .addr bullet_velocity_adjust_04 ; 1.5x speed
    .addr bullet_velocity_adjust_05 ; 1.62x speed
    .addr bullet_velocity_adjust_06 ; 1.75x speed
    .addr bullet_velocity_adjust_07 ; 1.87x speed
    .addr bullet_velocity_adjust_08 ; 2x speed

; bullet speed 0 (.5x speed)
; halves fast and slow velocity, e.g. #$03 #80 (3.5) becomes #$01 #$c0 (1.75)
bullet_velocity_adjust_00:
    lsr $05 ; half fast velocity
    ror $04 ; half fractional value, including carry from fast velocity
    rts

; bullet speed 1 (.75x speed)
; first half value, then half that again to add to the originally halved value
bullet_velocity_adjust_01:
    lsr $05                       ; half fast velocity
    ror $04                       ; half fractional value, including carry from fast velocity
    jmp bullet_velocity_adjust_04

; bullet speed 3 (1.25x speed)
; halves fast and fractional velocity, halves fractional again and adds it to original velocity
bullet_velocity_adjust_03:
    lda $05                          ; load fast velocity
    lsr                              ; half fast velocity
    lda $04                          ; load fractional velocity
    ror                              ; half fractional velocity, including carry from fast velocity
    lsr                              ; half fractional velocity again
    bpl bullet_velocity_adjust_add_a ; add .25 *  to original velocity

; bullet speed 4 (1.5x speed)
bullet_velocity_adjust_04:
    lda $05 ; load fast velocity
    lsr     ; half fast velocity
    lda $04 ; load original fractional velocity
    ror     ; half fractional velocity, including carry from fast velocity

bullet_velocity_adjust_add_a:
    clc      ; clear carry in preparation for addition
    adc $04  ; add to original value of $04
    sta $04  ; store value back in $04
    lda $05  ; re-load $05
    adc #$00 ; add any carry
    sta $05  ; update $05

; bullet speed 2 (normal speed)
bullet_velocity_adjust_02:
    rts

; bullet speed 5 (1.62x speed)
bullet_velocity_adjust_05:
    lda $05
    lsr
    lda $04
    ror
    sta $00
    lsr
    bpl bullet_dir_half_a_add_to_vel

; bullet speed 6 (1.75x speed) (for any value less than 1.14)
; doesn't work correctly when carry from fractional velocity, e.g. 1.5 becomes 1.62 and not 2.62
; however, 1.1 (#$01 #1a) correctly goes to (#$01 #$ed) (1.92)
bullet_velocity_adjust_06:
    lda $05
    lsr
    lda $04
    ror
    sta $00

bullet_dir_half_a_add_to_vel:
    lsr
    clc                              ; clear carry in preparation for addition
    adc $00
    jmp bullet_velocity_adjust_add_a

; bullet speed 7 (1.87x speed)
; doesn't work correctly when carry from fractional velocity, e.g. 1.5 becomes 1.81 and not 2.81
; however, 1.05 (#$01 #0d) correctly goes to (#$01 #$f7) (1.96)
bullet_velocity_adjust_07:
    lda $05
    lsr
    lda $04
    ror
    sta $00
    lsr
    sta $01
    clc                              ; clear carry in preparation for addition
    adc $00
    lsr $01
    clc                              ; clear carry in preparation for addition
    adc $01
    jmp bullet_velocity_adjust_add_a

; bullet speed 8 (2x speed)
bullet_velocity_adjust_08:
    asl $04
    rol $05
    rts

; !(UNUSED)
get_overhead_aim_dir_01:
    jsr rotate_aim_dir_01                     ; determine next aim direction [#$00-#$0b] ($0c), adjusts ENEMY_VAR_1 to get closer to that value using quadrant_aim_dir_01
    jmp convert_enemy_aim_dir_to_overhead_dir ; convert enemy aim direction overhead aim direction and store in ENEMY_VAR_2,x

; determine next aim direction [#$00-#$17] ($0c) to target player with index $0a
; adjusts ENEMY_VAR_1,x to get closer to that value using quadrant_aim_dir_01
; then updates the overhead aim direction ENEMY_VAR_2,x
; input
;  * x - enemy slot index
overhead_quad_aim_dir_01:
    jsr rotate_aim_dir_01_zp ; determine next aim direction [#$00-#$17] ($0c) to target player with index $0a
                             ; adjusts ENEMY_VAR_1,x to get closer to that value using quadrant_aim_dir_01

; converts an enemy aim direction ENEMY_VAR_1,x to an overhead aim direction
; and store result in ENEMY_VAR_2,x
; input
;  * x - enemy slot index
convert_enemy_aim_dir_to_overhead_dir:
    ldy ENEMY_VAR_1,x

; converts an enemy aim direction to an overhead aim direction
; changes range from #$00-#$16 to #$00-#$07
; input
;  * x - enemy slot index
;  * y - aim direction
; output
;  * ENEMY_VAR_2,x - overhead target aim direction
convert_dir_to_overhead_dir:
    lda dir_to_overhead_dir_tbl,y ; convert to overhead aim direction
    sta ENEMY_VAR_2,x             ; set overhead aim direction
    rts

; determines next aim direction [#$00-#$17] ($0c) to target player with index $0a
; adjusts ENEMY_VAR_1,x to get closer to that value using quadrant_aim_dir_01
; input
;  * x - enemy slot index
;  * $0a - player index to target (0 = p1, 1 = p2).  If targeted player isn't in
;    normal state (#$02), targets
;  * $0b - target X position to use when $0a is negative, i.e. not targeting player
;  * $0c - target Y position to use when $0a is negative, i.e. not targeting player
;  * ENEMY_X_POS,x - source X position
;  * ENEMY_Y_POS,x - source Y position
; output
;  * $08 - enemy Y position
;  * $09 - enemy X position
rotate_aim_dir_01_zp:
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)

; either increments or decrements ENEMY_VAR_1 by 1 to aim towards the player using quadrant_aim_dir_01
; used by spinning bubbles (enemy type = #$1d), tank (enemy type = #$12), and white blob (enemy type = #$13)
; input
;  * $0a - player index (0 = p1, 1 = p2)
; output
;  * carry flag - set when enemy already aiming at player, clear when rotation happened
;  * minus flag
;  * zero flag - clear when clockwise direction, set when counterclockwise
rotate_aim_dir_01:
    jsr get_rotate_01    ; set enemy aim direction ($0c) and rotation direction (a) targeting player $0a using quadrant_aim_dir_01
                         ; if no player to target, use $(0b, $0c)
    jmp rotate_enemy_var ; rotate the enemy's aim (ENEMY_VAR_1,x) by one in a clockwise or counterclockwise direction if needed

; either increments or decrements ENEMY_VAR_1,x by 1 to aim towards the player using quadrant_aim_dir_00
; used by rotating gun (enemy type = #$04) and alien fetus (enemy type = #$11)
; output
;  * ENEMY_VAR_1,x - enemy aim direction [#$00-#$0b] #$00 when facing right incrementing clockwise
;  * carry flag - set when enemy already aiming at player, clear when rotation happened
rotate_aim_dir_00_zp:
    jsr copy_enemy_vars_to_zp ; copy enemy (ENEMY_X_POS, ENEMY_Y_POS) position to ($09, $08)
    jsr get_rotate_00         ; get enemy aim direction and rotation direction using quadrant_aim_dir_00

; rotate the enemy's aim by one in a clockwise or counterclockwise direction
; input
;  * ENEMY_VAR_1,x - enemy's aim direction [#$00-#$17]
;  * minus flag - set when enemy is already aiming at player and no rotation is required
;  * carry flag - set when enemy already aiming at player, clear when rotation happened
;  * zero flag - clear when clockwise direction, set when counterclockwise
rotate_enemy_var:
    bmi @set_carry_exit            ; exit if enemy is already aiming at the player
    bne @rotate_1_counterclockwise ; if a = #$01, then a counterclockwise rotation
    inc ENEMY_VAR_1,x              ; move enemy aim direction clockwise
    lda ENEMY_VAR_1,x              ; load enemy aim direction
    cmp $04                        ; compare maximum supported enemy aim dir, e.g. rotating gun is #$0b
    bcc @continue                  ; continue if not past last position
    lda #$00                       ; wrapped around, set to first aim direction #$00 (horizontal left)
    beq @set_var_continue

; rotate counter-clockwise
@rotate_1_counterclockwise:
    dec ENEMY_VAR_1,x ; update enemy aim direction
                      ; moves direction counter clockwise
    bpl @continue     ; branch if counter-clockwise doesn't cause underflow
    lda $04           ; load maximum number of the supported enemy aim dir, e.g. rotating gun is #$0c (left and slightly down)
    sec               ; set carry flag in preparation for subtraction
    sbc #$01          ; subtract one

@set_var_continue:
    sta ENEMY_VAR_1,x ; update enemy aim direction

@continue:
    clc ; not yet at desired direction, clear carry and exit
    rts

; rotating gun is at desired position, set carry and exit
@set_carry_exit:
    sec ; set carry flag, already aiming at player
    rts

; convert enemy aim direction to overhead aim direction
dir_to_overhead_dir_tbl:
    .byte $00 ; #$00 -> #$00 (right)
    .byte $00 ; #$01 -> #$00 (right)
    .byte $01 ; #$02 -> #$01 (down right)
    .byte $01 ; #$03 -> #$01 (down right)
    .byte $01 ; #$04 -> #$01 (down right)
    .byte $02 ; #$05 -> #$02 (down)
    .byte $02 ; #$06 -> #$02 (down)
    .byte $02 ; #$07 -> #$02 (down)
    .byte $03 ; #$08 -> #$03 (down left)
    .byte $03 ; #$09 -> #$03 (down left)
    .byte $03 ; #$0a -> #$03 (down left)
    .byte $04 ; #$0b -> #$04 (left)
    .byte $04 ; #$0c -> #$04 (left)
    .byte $04 ; #$0d -> #$04 (left)
    .byte $05 ; #$0e -> #$05 (left up)
    .byte $05 ; #$0f -> #$05 (left up)
    .byte $05 ; #$10 -> #$05 (left up)
    .byte $06 ; #$11 -> #$06 (up)
    .byte $06 ; #$12 -> #$06 (up)
    .byte $06 ; #$13 -> #$06 (up)
    .byte $07 ; #$14 -> #$07 (up right)
    .byte $07 ; #$15 -> #$07 (up right)
    .byte $07 ; #$16 -> #$07 (up right)
    .byte $00 ; #$17 -> #$00

; determines which direction to rotate based on quadrant_aim_dir_00
; targeting player index ($0a)
; input
;  * $0a - player index to target, 0 = player 1, 1 = player 2
;  * $0b - target X position to use when $0a is negative, i.e. not targeting player
;  * $0c - target Y position to use when $0a is negative, i.e. not targeting player
;  * $08 - source Y position
;  * $09 - source X position
; output
;  * negative flag - set when enemy is already aiming at player and no rotation is needed
;  * a - rotation direction, #$00 clockwise, #$01 counterclockwise, #$80 no rotation needed
;  * $0c - new enemy aim direction
;  * $17 - quadrant aim direction
get_rotate_00:
    lda #$00                     ; a = #$00 (use quadrant_aim_dir_00)
    beq get_rotate_dir_for_index ; always branch, get enemy aim direction and rotation direction using quadrant_aim_dir_00

; determines which direction to rotate based on quadrant_aim_dir_01
; targeting player index ($0a)
; input
;  * $0a - player index to target, 0 = player 1, 1 = player 2
;  * $0b - target X position to use when $0a is negative, i.e. not targeting player
;  * $0c - target Y position to use when $0a is negative, i.e. not targeting player
;  * $08 - source Y position
;  * $09 - source X position
; output
;  * negative flag - set when enemy is already aiming at player and no rotation is needed
;  * a - rotation direction, #$00 clockwise, #$01 counterclockwise, #$80 no rotation needed
;  * $0c - new enemy aim direction
;  * $17 - quadrant aim direction
get_rotate_01:
    lda #$01 ; a = #$01 (use quadrant_aim_dir_01)

; determines which direction to rotate based on quadrant_aim_dir_lookup_ptr_tbl index offset (a)
; targeting player index ($0a)
; input
;  * a - quadrant_aim_dir_lookup_ptr_tbl offset table
;  * $0a - player index to target, 0 = player 1, 1 = player 2
;  * $0b - target X position to use when $0a is negative, i.e. not targeting player
;  * $0c - target Y position to use when $0a is negative, i.e. not targeting player
;  * $08 - source Y position
;  * $09 - source X position
; output
;  * negative flag - set when enemy is already aiming at player and no rotation is needed
;  * a - rotation direction, #$00 clockwise, #$01 counterclockwise, #$80 no rotation needed
;  * $0c - new enemy aim direction
;  * $17 - quadrant aim direction
get_rotate_dir_for_index:
    sta $0f                   ; set quadrant_aim_dir_lookup_ptr_tbl index offset
    lda $0a                   ; load player index
    bpl @get_quadrant_aim_dir ; branch if closest player has been determined
    lda $0c                   ; no player to target, e.g. overhead soldier walked beneath last player
                              ; note when both player states are not normal, still targets player 1
    sta $0a                   ; use $0c as target Y position
    jsr get_quadrant_aim_dir  ; get aim direction code for target ($0b, $0a) from location ($09, $08) using table code $0f
                              ; depending on quadrant_aim_dir_xx, dir code will be [#$00-#$03], [#$00-#$06], or [#$00-#$0f]
    jmp get_rotate_dir        ; determine which direction to rotate
                              ; based on a (quadrant aim dir) and quadrant ($07)

@get_quadrant_aim_dir:
    jsr get_quadrant_aim_dir_for_player ; set a to the aim direction within a quadrant
                                        ; based on source position ($09, $08) targeting player index $0a

; determines which direction to rotate
; based on a (quadrant aim dir) and quadrant ($07)
; input
;  * a - quadrant aim direction (quadrant_aim_dir_xx value)
;  * x - enemy slot index
;  * ENEMY_VAR_1,x - current enemy aim direction
;  * $07 - specifies quadrant to aim in (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
;    * bit 0 - 0 = bottom half of plane (quadrants III and IV), 1 = top half of plane (quadrants I and II)
;    * bit 1 - 0 = right half of the plan (quadrants I and IV), 1 = left half of plane (quadrants II and III)
;  * $0f - quadrant_aim_dir_lookup_ptr_tbl offset
; output
;  * negative flag - set when enemy is already aiming at player and no rotation is needed
;  * a - rotation direction, #$00 clockwise, #$01 counterclockwise, #$80 no rotation needed
;  * $0c - new enemy aim direction
;  * $17 - quadrant aim direction
get_rotate_dir:
    sta $17       ; store quadrant aim direction code in $17
    sta $0c       ; store quadrant aim direction code in $0c
    lda $0f       ; load quadrant_aim_dir_lookup_ptr_tbl offset (which quadrant_aim_dir_xx to use)
    lsr           ; move bit 0 to the carry
    lda #$06      ; using either quadrant_aim_dir_00, or quadrant_aim_dir_02
                  ; midway direction, i.e. 9 o'clock
    ldy #$0c      ; maximum aim direction (same as #$00 aim dir), i.e. 3 o'clock
    bcc @continue ; branch if aim type quadrant_aim_dir_00 or quadrant_aim_dir_02
    lda #$0c      ; using quadrant_aim_dir_01
                  ; midway direction, i.e. 9 o'clock
    ldy #$18      ; maximum aim direction (same as #$00 aim dir), i.e. 3 o'clock

@continue:
    sta $05               ; store either #$06 or #$0c into $06, which is the midway aim direction, i.e. 9 o'clock
    sty $04               ; store either #$0c or #$18 into $06, which is the maximum aim direction, i.e. 3 o'clock
    lda $07               ; load player position relative to enemy (left/right and above/below)
    and #$02              ; keep bit 1 (set when player to the left)
    beq @check_player_pos ; branch if player to the right of the enemy (#$00 or #$01)
    lda $05               ; player to left of enemy enemy, load mid way direction (either #$06 or #$0c)
    sec                   ; set carry flag in preparation for subtraction
    sbc $0c               ; subtract quadrant aim direction code from from midway direction point ($05 - $0c)
    sta $0c               ; store result back into $0c

@check_player_pos:
    lda $07
    lsr                     ; shift right the position relative to enemy (left/right and above/below)
    bcc @calc_reflected_dir ; branch if player below enemy
    lda $04                 ; player is above enemy, need to reflect aim dir ($0c) across x-axis
                            ; load max direction value (#$0c or #$18)
    sec                     ; set carry flag in preparation for subtraction
    sbc $0c                 ; subtract either the quadrant aim direction code directly from max aim direction (when player to right)
                            ; or subtract the offset amount from half-way (when player to left) from max aim direction (reflect across x-axis)
                            ; ($06 - $0c)
    cmp $04                 ; compare to max direction value
    bcc @continue2          ; branch if aim direction wasn't #$00
    lda #$00                ; direction result was #$00, set value to #$00 (right 3 o'clock)

@continue2:
    sta $0c ; set new aim direction

; calculates the aim direction reflected along the vertical axis
; e.g. if original aim direction #$00 is right and increment clockwise
; reflected aim direction #$00 is left increment clockwise
@calc_reflected_dir:
    lda #$00
    sta $0e
    lda ENEMY_VAR_1,x     ; load enemy's current aim direction
    clc                   ; clear carry in preparation for addition
    adc $05               ; add mid way direction (either #$06 or #$0c)
    cmp $04               ; compare result to max direction value (#$0c or #$18)
    bcc @check_dir_result
    inc $0e               ; overflow
    sbc $04

@check_dir_result:
    sta $0d                    ; store reflection aim direction
    lda $0c                    ; load new enemy aim direction
    cmp ENEMY_VAR_1,x          ; compare to enemy current aim direction
    beq @exit_no_change        ; exit if no direction change
    ldy $0e
    bne @check_exit            ; branch no overflow calculating reflected aim direction
                               ; i.e. reflected aim direction is on the right
    bcc @exit_counterclockwise
    cmp $0d                    ; compare to reflected aim direction
    bcs @exit_counterclockwise

@exit_clockwise:
    lda #$00
    beq @exit

@check_exit:
    bcs @exit_clockwise
    cmp $0d             ; compare to reflected aim direction
    bcc @exit_clockwise

@exit_counterclockwise:
    lda #$01

@exit:
    rts

@exit_no_change:
    lda #$80
    bne @exit ; always exit negative flag set

; !(UNUSED)
bank_f_unused_04:
    lda #$02
    sta $0f                             ; using quadrant_aim_dir_02
    jsr get_quadrant_aim_dir_for_player ; set a to the aim direction within a quadrant
                                        ; based on source position ($09, $08) targeting player index $0a
    sta $0c                             ; set to quadrant aim direction (quadrant_aim_dir_xx value)
    ldy ENEMY_VAR_3,x
    lda $07                             ; load aiming quadrant (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
    lsr
    lsr
    bcc @continue                       ; branch if right of the screen
    lda #$20                            ; left half of the screen
    sec                                 ; set carry flag in preparation for subtraction
    sbc $0c
    sta $0c                             ; subtract #$20 from quadrant aim direction

@continue:
    lsr $07
    bcc @x_vel ; branch if bottom half of screen
    lda #$40   ; top half of screen
    sec        ; set carry flag in preparation for subtraction
    sbc $0c
    and #$3f
    sta $0c

@x_vel:
    lda #$00
    sta $0e
    lda ENEMY_X_VELOCITY_FAST,y
    clc                         ; clear carry in preparation for addition
    adc #$20
    cmp #$40
    bcc @check_vel              ; branch if moving right
    inc $0e                     ; moving left (or moving right really really fast), increment $0e
    sbc #$40

@check_vel:
    sta $0d                     ; set modified X fast velocity
    lda $0c                     ; load quadrant aim direction
    cmp ENEMY_X_VELOCITY_FAST,y
    beq @exit_negative
    ldy $0e
    bne @check_carry
    bcc @exit_1
    cmp $0d                     ; $0e is zero
    bcs @exit_1

@exit_0:
    lda #$00
    beq @exit

@check_carry:
    bcs @exit_0
    cmp $0d
    bcc @exit_0

@exit_1:
    lda #$01

@exit:
    rts

@exit_negative:
    lda #$80
    bne @exit ; always exit

; determines the aim direction within a quadrant based on source position
; ($09, $08) targeting player index $0a
; input
;  * $0f - quadrant_aim_dir_lookup_ptr_tbl offset [#$00-#$02]
;  * $0a - player index to target (0 = p1, 1 = p2)
;    if player with index $0a isn't in normal state (#$02), then attempts to
;    target other player. If neither player is in a normal state, targets bottom
;    center of screen (#$80, #$ff)
;  * $08 - source enemy Y position
;  * $09 - source enemy X position
; output
;  * a - quadrant aim direction (quadrant_aim_dir_xx value)
;  * $07 - quadrant to aim in
;    (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
;    * bit 0
;      * 0 = bottom half of plane (quadrants III and IV)
;      * 1 = top half of plane (quadrants I and II)
;    * bit 1
;      * 0 = right half of the plan (quadrants I and IV)
;      * 1 = left half of plane (quadrants II and III)
;  * $0a - target Y position
;  * $0b - target X position
get_quadrant_aim_dir_for_player:
    jsr find_alive_player_pos ; find player's position of player in the normal state (PLAYER_STATE = 2)
                              ; starting with the closest player specified in $0a
                              ; if no available player, set bottom center of screen

; determines the aim direction within a quadrant
; based on source position ($09, $08) targeting player location ($0b, $0a)
; input
;  * $08 - source enemy Y position
;  * $09 - source enemy X position
;  * $0a - target Y position
;  * $0b - target X position
;  * $0f - which of the #$03 tables from quadrant_aim_dir_lookup_ptr_tbl to use
; output
;  * a - quadrant aim direction (quadrant_aim_dir_xx value)
;  * $07 - quadrant to aim in
;    (0 = quadrant IV, 1 = quadrant I, 2 = quadrant III, 3 = quadrant II)
;    * bit 0
;      * 0 = bottom half of plane (quadrants III and IV)
;      * 1 = top half of plane (quadrants I and II)
;    * bit 1
;      * 0 = right half of the plan (quadrants I and IV)
;      * 1 = left half of plane (quadrants II and III)
get_quadrant_aim_dir:
    ldy #$00              ; default assume player is to the right and equal to or below enemy
    lda $0a               ; load target Y position
    sec                   ; set carry flag in preparation for subtraction
    sbc $08               ; subtract enemy Y position from player Y position
    bcs @shift_get_x_diff ; branch if no overflow occurred (enemy above player or same vertical position)
    eor #$ff              ; enemy below player, handle overflow, flip all bits and add one
    adc #$01
    iny                   ; y used to keep track of where player is in relation to enemy, i.e. $07
                          ; mark that enemy was below player

@shift_get_x_diff:
    lsr           ; shift the difference between player and enemy y difference 5 bits
    lsr           ; (every #$20 pixels difference is a new horizontal direction)
    lsr
    lsr
    lsr
    sta $0a       ; store result in $0a (row offset for quadrant_aim_dir_xx)
    lda $0b       ; load player X position
    sec           ; set carry flag in preparation for subtraction
    sbc $09       ; subtract enemy X position from player X position
    bcs @continue ; branch if no overflow (player to right of enemy)
    eor #$ff      ; enemy to left of player, handle overflow, flip all bits and add one
    adc #$01
    iny           ; player to left of enemy, increment y by two to set correct relative position
    iny           ; if y was 0, now is 2, if y was 1, now is 3

@continue:
    lsr                                     ; shift the difference between player and enemy x difference 6 bits
    lsr                                     ; (every #$40 pixels difference is a new horizontal direction)
    lsr
    lsr
    lsr
    sty $07                                 ; store position of player relative to enemy in $07 (above/below, left/right)
    lsr                                     ; push bit 5 to the carry flag for use after plp instruction below
    sta $0b                                 ; overwrite player X position with shifted bits 6 and 7
                                            ; (values [#$00-#$03]) of horizontal distance
    php                                     ; backup CPU status flags on stack
    lda $0f                                 ; load which of the #$03 tables from quadrant_aim_dir_lookup_ptr_tbl to use
    asl                                     ; double since each entry is #$2 bytes
    tay                                     ; transfer to offset register
    lda quadrant_aim_dir_lookup_ptr_tbl,y   ; get low byte of quadrant_aim_dir_xx address
    sta $0c                                 ; store low byte of pointer address in $0c
    lda quadrant_aim_dir_lookup_ptr_tbl+1,y ; get high byte of quadrant_aim_dir_xx address
    sta $0d                                 ; store high byte of pointer address in $0d
    lda $0a                                 ; load y difference to determine row offset
    asl
    asl                                     ; quadruple since each entry is #$04 bytes to get correct row
    adc $0b                                 ; add the x distance between player and enemy as offset into the entry to load
                                            ; this gets the column of the aim direction
    tay                                     ; transfer to offset register
    lda ($0c),y                             ; load specific byte from quadrant_aim_dir_xx
    plp                                     ; restore CPU status flags from stack
    bcs @set_and_exit                       ; branch if bit 5 of difference between player and enemy was set
    lsr                                     ; this segments screen into bands for which nibble to use
    lsr
    lsr
    lsr

@set_and_exit:
    and #$0f ; keep low nibble
    rts

; pointer table for set of quadrant aim directions
quadrant_aim_dir_lookup_ptr_tbl:
    .addr quadrant_aim_dir_00
    .addr quadrant_aim_dir_01
    .addr quadrant_aim_dir_02

quadrant_aim_dir_00:
    .byte $00,$00,$00,$00 ; player at same height
    .byte $32,$11,$00,$00
    .byte $32,$11,$11,$11
    .byte $32,$22,$11,$11
    .byte $33,$22,$11,$11
    .byte $33,$22,$22,$11
    .byte $33,$22,$22,$11
    .byte $33,$22,$22,$22

quadrant_aim_dir_01:
    .byte $00,$00,$00,$00 ; player at same height
    .byte $63,$21,$11,$11
    .byte $64,$32,$21,$11
    .byte $65,$43,$22,$22
    .byte $65,$44,$33,$22
    .byte $65,$54,$33,$32
    .byte $65,$54,$43,$33
    .byte $65,$54,$44,$33

quadrant_aim_dir_02:
    .byte $80,$00,$00,$00
    .byte $f8,$53,$32,$21
    .byte $fb,$86,$54,$33
    .byte $fd,$a8,$75,$54
    .byte $fe,$b9,$87,$65
    .byte $fe,$cb,$98,$76
    .byte $fe,$db,$a9,$87
    .byte $ff,$dc,$ba,$98

; finds a player's position of a player in the normal (alive) state
; (PLAYER_STATE = 2) starting with player index specified in bit 0 of $0a
; if no player exists in a normal state (#$02), then targets bottom center of
; screen (#$80, #$ff)
; input
;  * $0a - preferred player index to target (0 = p1, 1 = p2)
; output
;  * $0a - player's Y position, #$ff if no player with PLAYER_STATE = 2
;  * $0b - player's X position, #$80 if no player with PLAYER_STATE = 2
find_alive_player_pos:
    lda $0a            ; load the player player index
    and #$01           ; should only be #$00 or #$01 (p1 or p2)
    tay                ; transfer player index to y register
    lda PLAYER_STATE,y ; load the closest player's PLAYER_STATE
    cmp #$02           ; see if normal state
    beq @continue      ; branch if normal state
    tya                ; not normal state, either falling, dead or can't move
    eor #$01           ; flip to other player
    tay
    lda PLAYER_STATE,y ; load other player's PLAYER_STATE
    cmp #$02           ; see if normal state
    beq @continue      ; branch if normal state
    lda #$ff           ; other player also not in normal state
    sta $0a            ; set player Y position to #$ff (bottom of screen)
    lda #$80           ; a = #$80
    bne @exit          ; always branch

; found player in normal state, use that player
@continue:
    lda PLAYER_SPRITE_Y_POS,y
    sta $0a
    lda PLAYER_SPRITE_X_POS,y

@exit:
    sta $0b
    rts

; !(HUH)
; plays player death sound and updates player state
; input
;  * x - player index (0 = p1, 1 = p2)
kill_player_x:
    nop

; plays player death sound and updates player state
; input
;  * x - player index (0 = p1, 1 = p2)
kill_player:
    lda BOSS_DEFEATED_FLAGS
    bne @exit                          ; don't play player death sound boss defeated
    lda #$25                           ; player death sound
    jsr play_sound                     ; play sound_25 (P OUT) - player death
    lda #$00
    sta NEW_LIFE_INVINCIBILITY_TIMER,x
    lda #$01
    sta PLAYER_STATE_TIMER,x           ; mark player death to begin lying on ground timer
    lda #$01
    sta PLAYER_SKIP_COLLISION,x        ; disable player sprite collision testing
    lda #$03
    sta PLAYER_STATE,x                 ; set sp

@exit:
    rts

; !(UNUSED)
clear_all_player_bullets:
    ldx #$0f

@bullet_loop:
    jsr clear_bullet_values
    dex
    bpl @bullet_loop
    rts

; initialize bullet memory values to #$00
clear_bullet_values:
    lda #$00
    sta PLAYER_BULLET_SPRITE_CODE,x
    sta PLAYER_BULLET_Y_POS,x
    sta PLAYER_BULLET_X_POS,x
    sta PLAYER_BULLET_SPRITE_ATTR,x
    sta PLAYER_BULLET_STATE,x
    sta PLAYER_BULLET_WEAPON_TYPE,x
    sta PLAYER_BULLET_Y_VEL_ACCUM,x
    sta PLAYER_BULLET_X_VEL_ACCUM,x
    sta PLAYER_BULLET_Y_VEL_FRACT,x
    sta PLAYER_BULLET_Y_VEL_FAST,x
    sta PLAYER_BULLET_X_VEL_FRACT,x
    sta PLAYER_BULLET_X_VEL_FAST,x
    sta PLAYER_BULLET_TIMER,x
    sta PLAYER_BULLET_COLLISION_CODE,x
    rts

; creates enemy of type y in first available slot
; input
;  * y - enemy type
; output
;  * zero flag - set when enemy created, clear if no available slot
;  * x - enemy slot of created enemy, if created
create_enemy_y:
    jsr find_next_enemy_slot ; find next available enemy slot, put result in x register
    bne @exit                ; branch if zero flag clear, meaning no available enemy slot was found
    jmp initialize_enemy     ; create enemy of type y at slot x

@exit:
    rts

; creates an enemy of type y in first available slot (between slot 3 and slot 0)
; input
;  * y - enemy type
; output
;  * x - free enemy slot that is currently not being used, if found, otherwise #$ff
;  * zero flag - set when enemy slot found, clear when no slot available
create_enemy_slot_0_to_3:
    ldx #$03

; creates an enemy of type y in first available slot (between slot x and slot 0)
; input
;  * x - highest slot to use for the enemy
;  * y - enemy type
; output
;  * x - free enemy slot that is currently not being used, if found, otherwise #$ff
;  * zero flag - set when enemy slot found, clear when no slot available
create_and_init_enemy:
    jsr find_next_enemy_slot_x_to_0 ; find next available enemy slot (0-x), put result in x register
    bne @exit
    jmp initialize_enemy            ; create enemy of type y at slot x

@exit:
    rts

; finds next available enemy slot (all slots #$00-#$0d)
; output
;  * x - free enemy slot that is currently not being used, if found, otherwise #$ff
;  * zero flag - set when enemy slot found, clear when no slot available
find_next_enemy_slot:
    ldx #$0d

; finds next available enemy slot from slot x down to #$00
; input
;  * x - starting enemy slot (inclusive)
; output
;  * x - free enemy slot that is currently not being used, if found, otherwise #$ff
;  * zero flag - set when enemy slot found, clear when no slot available
find_next_enemy_slot_x_to_0:
    lda ENEMY_ROUTINE,x
    beq @exit
    dex                             ; decrement enemy slot
    bpl find_next_enemy_slot_x_to_0 ; branch if possible to see if a lower slot index is available

@exit:
    rts

; tires to replace an enemy bullet with enemy type y
; input
;  * y - enemy type
; output
;  * x - if replaced, enemy slot that previously contained a bullet but now contains the new enemy
;  * zero flag - clear when bullet was replaced and new enemy created, set when no enemy bullet existed
replace_bullet_with_enemy:
    jsr find_first_enemy_bullet ; find first slot that contains a bullet, if exists
    bne @no_enemy_bullets       ; branch if zero flag set, indicating no enemy bullet exists
    jmp initialize_enemy        ; create enemy of type y at slot x

@no_enemy_bullets:
    rts

; finds the first bullet if any exist
; output
;  * x - slot that contains a bullet
;  * zero flag - clear when bullet found, set when not found
find_first_enemy_bullet:
    ldx #$0d

@find_bullet_loop:
    lda ENEMY_TYPE,x
    cmp #$02              ; see if bullet
    beq @exit
    dex
    bpl @find_bullet_loop

@exit:
    rts

; clear all enemy data (enemy routine, hp, attributes, etc)
clear_all_enemy_data:
    ldx #$0d

@loop:
    jsr clear_enemy ; clear enemy data (enemy routine, hp, attributes, etc)
    dex
    bpl @loop
    rts

; !(UNUSED)
clear_enemy_pt_2:
    lda #$00
    beq set_enemy_pt_2 ; always branch

; initializes enemy attributes, routine, type, and hp. Then sets all other enemy properties to all zero
;  * x - enemy slot index
;  * y - enemy type
initialize_enemy:
    jsr set_enemy_collision_and_type ; set the collision box type, and enemy type based on y
    lda #$01
    sta ENEMY_ROUTINE,x
    sta ENEMY_HP,x
    lda #$00
    beq set_enemy_pt_1               ; always branch

; clears enemy data (enemy routine, hp, attributes, etc)
; input
;  * x - enemy slot index
clear_enemy:
    jsr clear_enemy_type ; marks enemy type as #$7f, indicating the enemy slot is free
    lda #$00
    sta ENEMY_ROUTINE,x
    sta ENEMY_HP,x

; input
;  * a - value to set all the variables to (always #$00 in game)
set_enemy_pt_1:
    sta ENEMY_ATTRIBUTES,x
    sta ENEMY_SPRITE,x
    sta ENEMY_SPRITE_ATTR,x
    sta ENEMY_VAR_6,x
    sta ENEMY_VAR_7,x
    sta ENEMY_Y_POS,x
    sta ENEMY_Y_VEL_ACCUM,x
    sta ENEMY_X_VEL_ACCUM,x
    sta ENEMY_Y_VELOCITY_FRACT,x ; store enemy's fractional Y velocity
    sta ENEMY_X_VELOCITY_FRACT,x ; store enemy's fractional X velocity
    sta ENEMY_Y_VELOCITY_FAST,x  ; store enemy's fast Y velocity
    sta ENEMY_X_VELOCITY_FAST,x  ; store enemy's fast X velocity
    sta ENEMY_DELAY,x
    sta ENEMY_FIRING_DELAY,x
    sta ENEMY_FRAME,x
    sta ENEMY_ANIMATION_DELAY,x
    sta ENEMY_DESTROY_ATTRS,x

; input
;  * a - value to set all the variables to (always #$00 in game)
set_enemy_pt_2:
    sta ENEMY_VAR_1,x
    sta ENEMY_VAR_2,x
    sta ENEMY_VAR_3,x
    sta ENEMY_VAR_4,x
    sta ENEMY_VAR_5,x
    rts

clear_enemy_type:
    ldy #$7f ; clear enemy type

; sets the collision box type, and enemy type based on y
; !(HUH) - overwrites previously written enemy type in create_lvl_defined_enemy
; input
;  * x - enemy slot index
;  * y - enemy type
set_enemy_collision_and_type:
    lda enemy_collision_code_tbl,y ; load the collision index (see collision_box_tbl)
    sta ENEMY_COLLISION_INDEX,x    ; set enemy's collision index (specifies collision box dimensions)
    tya
    sta ENEMY_TYPE,x               ; set enemy type variable
    rts

; writes palette colors from PALETTE_CPU_BUFFER to CPU_GRAPHICS_BUFFER
; if PAUSE_PALETTE_UPDATES is zero, and enough space in graphics buffer.
safe_write_palette_to_graphics_buffer:
    lda PAUSE_PALETTE_UPDATES
    bne @exit                            ; exit if palette updates are paused
    lda GRAPHICS_BUFFER_OFFSET
    cmp #$20                             ; see if enough space to write palette to graphics buffer
    bcc write_palette_to_graphics_buffer ; write bg and sprite palette colors from palette buffer to graphics buffer

@exit:
    rts

; !(UNUSED)
write_sprite_palette_to_graphics_buffer:
    ldy #$05                    ; graphics_buffer_palette_addr_tbl offset, only writing sprite palette colors
    bne write_10_palette_colors ; always branch to write #$10 palette colors to graphics buffer

; writes background palette colors to graphics buffer
; eventually will be read and written to correct place ($3f00-$3f0f) in PPU RAM
write_bg_palette_to_graphics_buffer:
    ldy #$02 ; graphics_buffer_palette_addr_tbl offset, only writing bg palette colors

write_10_palette_colors:
    lda #$10                     ; writing #$10 palette colors to graphics buffer
    bne write_to_graphics_buffer ; always branch to write palette colors to graphics buffer

; writes palette colors from PALETTE_CPU_BUFFER to CPU_GRAPHICS_BUFFER
; eventually will be read and written to correct place in PPU RAM
write_palette_to_graphics_buffer:
    ldy #$02 ; graphics_buffer_palette_addr_tbl offset, writing entire PPU palette
    lda #$20 ; writing #$20 palette colors

write_to_graphics_buffer:
    sta $00                    ; set number of palette colors to write to graphics buffer
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$03                   ; writing the vram increment and 2 byte PPU address
    sta $01                    ; set loop counter

; write VRAM increment and PPU address for palette
@palette_addr_loop:
    lda graphics_buffer_palette_addr_tbl,y
    sta CPU_GRAPHICS_BUFFER,x              ; writing vram increment and PPU address
    inx                                    ; increment graphics buffer write offset
    dey                                    ; decrement graphics_buffer_palette_addr_tbl read offset
    dec $01                                ; decrement loop counter
    bne @palette_addr_loop
    tay                                    ; transfer low byte of PPU address to offset register
                                           ; this will be either #$00 or #$10
                                           ; which corresponds to what palette colors to draw

; write the palette colors to the graphics buffer
@palette_loop:
    lda PALETTE_CPU_BUFFER,y  ; load palette color
    sta CPU_GRAPHICS_BUFFER,x
    inx                       ; increment graphics buffer write offset
    iny                       ; increment palette buffer read offset
    dec $00                   ; decrement loop counter (number of palette colors to write)
    bne @palette_loop         ; loop if more colors to put in graphics buffer
    ldy #$08                  ; writing 8 bytes to cpu graphics buffer

; set to change PPUADDR two times, first to $3f00, then to $0000
@palette_ppuaddr_reset_loop:
    lda palette_end_ppu_addr_tbl,y  ; load graphics buffer byte
    sta CPU_GRAPHICS_BUFFER,x       ; write to graphics buffer
    inx                             ; increment graphics buffer write offset
    dey                             ; decrement read offset
    bpl @palette_ppuaddr_reset_loop ; loop if more data to write
    stx GRAPHICS_BUFFER_OFFSET      ; update graphics buffer write offset
    rts

graphics_buffer_palette_addr_tbl:
    .byte $00,$3f,$01 ; PPUCTRL = #$28, PPU address = $3f00 (background palette)
    .byte $10,$3f,$01 ; PPUCTRL = #$28, PPU address = $3f10 (sprite palette)

; resets PPU address after writing palette colors to graphics buffer
; set PPUADDR to $3f00, then set to $0000
; !(HUH) not sure why setting two different PPU addresses
palette_end_ppu_addr_tbl:
    .byte $ff,$00,$00,$01,$ff,$00,$3f,$01,$ff

; clears player bullet's state, sprite, sprite attr and collision code
; input
;  * x - player bullet offset
clear_bullet_sprite:
    lda #$00
    sta PLAYER_BULLET_STATE,x
    sta PLAYER_BULLET_SPRITE_CODE,x
    sta PLAYER_BULLET_SPRITE_ATTR,x
    sta PLAYER_BULLET_COLLISION_CODE,x
    rts

; sets the vertical auto-scroll direction, stop screen, and scroll value
; will adjust auto-scroll direction based on whether player has passed the desired scroll stop point or not
; e.g. scroll up to screen y scroll value a
; used only on level 1 for the top of hills and only ever has initial desired scroll direction of up
; input
;  * a - desired auto-scroll stop position
;    bit 0 specifies desired auto-scroll vertical direction (0 = up, 1 = down)
;  * y - the stop screen number
set_y_autoscroll_stop:
    pha                      ; save the scroll stop position to the stack
    lsr                      ; push bit 0 to the carry, this specifies the 'desired' scroll direction (0 = up, 1 = down)
    pla                      ; restore the scroll stop position from the stack
    bcs @default_scroll_down ; branch if scrolling down to stop position
                             ; scrolling up desired, if past auto-scroll stop, then direction will be flipped (to down)
    cpy Y_SCREEN             ; compare auto-scroll stop screen to the desired vertical screen to stop on
    bcc @set_autoscroll      ; not yet at the stop scroll screen, branch to set the auto-scroll screen and position (scrolling up)
    bne @flip_scroll_dir     ; branch if past the scroll stop screen
                             ; flips auto-scroll direction to down and sets the auto-scroll screen and position
    cmp Y_SCROLL             ; compare scroll stop position to PPU vertical scroll
    bcc @set_autoscroll      ; not yet at the stop scroll position, branch to set the auto-scroll screen and position (scrolling up)
    bne @flip_scroll_dir     ; branch if past the scroll stop position
                             ; flips auto-scroll direction to down and sets the auto-scroll screen and position
    beq @stop_autoscroll     ; always branch since at stop scroll position, don't set an auto-scroll

; auto-scroll down
; if player isn't at Y_SCREEN, then scroll direction is flipped to up
@default_scroll_down:
    cpy Y_SCREEN         ; compare auto-scroll stop screen to the number of vertical nametable screens already scrolled
    bcc @flip_scroll_dir ; branch if not yet at auto-scroll stop screen
                         ; flips auto-scroll direction to up and sets the auto-scroll screen and position
    bne @set_autoscroll  ; branch if scrolled passed the stop screen to set the auto-scroll screen and position (scrolling down)
    cmp Y_SCROLL         ; on desired screen, compare scroll stop position to PPU vertical scroll
    bcc @flip_scroll_dir ; branch if not yet at scroll position
                         ; flips auto-scroll direction to up and sets the auto-scroll screen and position
    bne @set_autoscroll  ; branch if past scroll position to set the auto-scroll screen and position (scrolling down)

; at scroll position on correct screen, stop auto-scroll
@stop_autoscroll:
    lda #$00
    beq @set_autoscroll ; always branch to set Y_AUTOSCROLL_STOP_POS to #$00 and Y_AUTOSCROLL_STOP_SCREEN to y

@flip_scroll_dir:
    eor #$01 ; fip bit 0, which specifies scroll direction

@set_autoscroll:
    sta Y_AUTOSCROLL_STOP_POS
    sty Y_AUTOSCROLL_STOP_SCREEN
    rts

; sets 2nd and 3rd scanline locations based on diff from 1st irq
; input
;  * SCANLINE_IRQ_2_DIFF
;  * SCANLINE_IRQ_3_DIFF
set_irq_scanlines:
    lda #$ff
    sta SCANLINE_IRQ_2      ; initialize 2nd scanline to #$ff (no irq)
    sta SCANLINE_IRQ_3      ; initialize 3rd scanline to #$ff (no irq)
    lda SCANLINE_IRQ_1      ; load scanline where the 1st interrupt will occur (#$ff for no irq)
    clc                     ; clear carry in preparation for addition
    adc SCANLINE_IRQ_2_DIFF ; add the number of scanlines after the 1st irq before the next irq
    bcs @exit               ; exit if either no irqs or only a single irq
    sta SCANLINE_IRQ_2      ; set the actual scanline where the 2nd irq occurs
    adc SCANLINE_IRQ_3_DIFF ; add the number of scanlines after the 2nd irq before the next irq
    bcs @exit               ; exit if no 3rd irq
    sta SCANLINE_IRQ_3      ; set the actual scanline where the 3rd irq occurs

@exit:
    rts

; determine Y position from bg collision
; when landing on not an incline ($09 < #$06)
;  * returns Y position minus offset from 2-row nametable stripe minus $07 plus #$09
;    $06 - ($01 & #$0f) - $07 + #$09
; when landing on an incline ($09 >= #$06)
; input
;  * $00 - X position
;  * $01 - absolute y location (Y position plus PPU vertical scroll)
;  * $06 - Y position
;  * $07 - amount add/subtract to resulting Y position (offset from vertical center of sprite)
;  * $09 - background collision code
; output
;  * a - Y position for player
get_landing_y_pos:
    lda $01           ; load absolute y location
    and #$0f          ; strip to only offset within 2 nametable rows
    sta $02           ; set offset from #$10 pixel (2 nametable rows)
    lda #$09
    ldy $09           ; load background collision code
    cpy #$06          ; see if on incline
    bcc @not_incline  ; branch if not on incline
    jsr @incline_loop
    clc

@not_incline:
    adc $06 ; add Y position to #$09
    sec     ; set carry flag in preparation for subtraction
    sbc $02 ; subtract offset from top of 2 nametable row stripe
    sec     ; set carry flag in preparation for subtraction
    sbc $07 ; subtract any hard-coded offset
    rts

; loop through finding appropriate Y position based on incline
@incline_loop:
    lda $00                              ; load X position
    and #$0f                             ; strip to offset from nametable tile 2-column width
    sta $04
    cmp landing_y_incline_cutoff_tbl-6,y ; y is at least 6 since player is landing on an incline player surface
                                         ; compare offset from 2-column width to cutoff for incline
                                         ; e.g. 3 pixels away from 2-nametable strip and on surface #$08 (most of positive incline)
                                         ; #$03 < #$04, carry not set
    ror                                  ; push carry to bit 7
    eor landing_y_incline_dir_tbl-6,y
    bmi @exit_loop                       ; exit if greater than cutoff and on main portion of incline or
                                         ; less than cutoff and on start or second part of incline (both positive and negative)
    lda $06                              ; load Y position
    clc                                  ; clear carry in preparation for addition
    adc landing_y_incline_adj_tbl-6,y    ; add or subtract from Y position based on positive/negative incline
    sta $06                              ; set new Y position
    lda landing_y_incline_off_tbl-6,y    ; load next incline
    tay                                  ; set new offset
    bne @incline_loop

; finished Y calc from loop, now handle adjustment based on positive/negative incline
@exit_loop:
    lda $04                                  ; load offset from nametable tile 2-column width
    sec                                      ; set carry flag in preparation for subtraction
    sbc landing_y_incline_main_x_adj_tbl-6,y ; subtract #$04 or #$0c when on main portion of incline
    lsr                                      ; half the result
    sta $03                                  ; store X offset
    lda landing_y_incline_type_tbl-6,y
    asl                                      ; push bit 7 to carry
    lda $03                                  ; load X offset
    bcc @exit                                ; branch if positive incline
    eor #$ff                                 ; negative incline, convert to subtraction
    adc #$00                                 ; flip all bits and add 1
    clc

@exit:
    adc landing_y_incline_final_adj_tbl-6,y
    rts

; offset from 2-nametable tile column based on surface
landing_y_incline_cutoff_tbl:
    .byte $7f ; #$06 - player on positive incline (start)
    .byte $04 ; #$07 - player on positive incline (second)
    .byte $04 ; #$08 - player on positive incline (most of incline)
    .byte $7f ; #$09 - player on negative incline (second)
    .byte $0c ; #$0a - player on negative incline (most of time)
    .byte $0c ; #$0b - player on negative incline (start)

landing_y_incline_dir_tbl:
    .byte $80 ; #$06 - player on positive incline (start)
    .byte $80 ; #$07 - player on positive incline (second)
    .byte $00 ; #$08 - player on positive incline (most of incline)
    .byte $80 ; #$09 - player on negative incline (second)
    .byte $00 ; #$0a - player on negative incline (most of time)
    .byte $80 ; #$0b - player on negative incline (start)

; added or subtracted from Y position
landing_y_incline_adj_tbl:
    .byte $00 ; #$06 - player on positive incline (start)
    .byte $f0 ; #$07 - player on positive incline (second)
    .byte $10 ; #$08 - player on positive incline (most of incline)
    .byte $00 ; #$09 - player on negative incline (second)
    .byte $f0 ; #$0a - player on negative incline (most of time)
    .byte $10 ; #$0b - player on negative incline (start)

; next y offset to use when reading
landing_y_incline_off_tbl:
    .byte $00 ; #$06 -> exit
    .byte $08 ; #$07 -> #$08
    .byte $07 ; #$08 -> #$07
    .byte $00 ; #$09 -> exit
    .byte $0b ; #$0a -> #$0b
    .byte $0a ; #$0b -> #$0a

landing_y_incline_main_x_adj_tbl:
    .byte $00 ; #$06 - player on positive incline (start)
    .byte $00 ; #$07 - player on positive incline (second)
    .byte $04 ; #$08 - player on positive incline (most of incline)
    .byte $00 ; #$09 - player on negative incline (second)
    .byte $0c ; #$0a - player on negative incline (most of time)
    .byte $00 ; #$0b - player on negative incline (start)

landing_y_incline_final_adj_tbl:
    .byte $09 ; #$06 - player on positive incline (start)
    .byte $01 ; #$07 - player on positive incline (second)
    .byte $0f ; #$08 - player on positive incline (most of incline)
    .byte $02 ; #$09 - player on negative incline (second)
    .byte $00 ; #$0a - player on negative incline (most of time)
    .byte $0a ; #$0b - player on negative incline (start)

landing_y_incline_type_tbl:
    .byte $80 ; #$06 - player on positive incline (start)
    .byte $80 ; #$07 - player on positive incline (second)
    .byte $80 ; #$08 - player on positive incline (most of incline)
    .byte $00 ; #$09 - player on negative incline (second)
    .byte $00 ; #$0a - player on negative incline (most of time)
    .byte $00 ; #$0b - player on negative incline (start)

; offset by bg collision type
; bullets and overhead bullets (except levels 5 and 8), player routine
; #$00 when collision shouldn't count, #$01 when collision should count
bg_collision_test_tbl:
    .byte $00
    .byte $00 ; #$01 - floating platform
    .byte $01 ; #$02 - land/door
    .byte $01 ; #$03 - collapsible floor on level 7 (eggshells)
    .byte $01 ; water
    .byte $01
    .byte $01 ; incline (start)
    .byte $01 ; incline (second)
    .byte $01 ; incline (most of incline)
    .byte $01 ; floating incline (second)
    .byte $01 ; floating incline (most of the time)
    .byte $01 ; floating incline (start)

; used by weapon item and used by enemy bullets (on levels 5 and 8 only)
; offset by bg collision type
; * for weapon item, when non-zero the weapon item will land on the bg (stop
;   moving)
; * for enemy bullets, when non-zero the bullet will be removed
; identical to bg_collision_test_tbl, but excludes incline bg collisions (see
; INCLINE_BG_COLLISION_FLAG))
bg_collision_test_no_incline_tbl:
    .byte $00
    .byte $00 ; floating platform
    .byte $01 ; land
    .byte $01
    .byte $01 ; water
    .byte $01
    .byte $00 ; incline (start)
    .byte $00 ; incline (second)
    .byte $00 ; incline (most of incline)
    .byte $00 ; floating incline (second)
    .byte $00 ; floating incline (most of the time)
    .byte $00 ; floating incline (start)

; each byte is the collision code for the enemy, the offset is the enemy type
; example enemy_collision_code_tbl,3 is the initial attributes for the soldier, which is #$08
; specifies row for specific PLAYER_ACTION_STATE in collision_box_tbl
; for example, #$00 is a boxy enemy, #$04 is a bullet, #$08 is soldier
enemy_collision_code_tbl:
    .byte $00,$00,$04,$08,$00,$08,$04,$10,$0c,$08,$00,$0c,$0c,$0c,$04,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $0c,$00,$00,$00,$00,$00,$00,$00,$0c,$04,$00,$04,$00,$00,$0c,$0c
    .byte $04,$00,$0c,$04,$08,$04,$18,$04,$0c,$00,$04,$1c,$04,$0c,$00,$00
    .byte $08,$00,$00,$00,$00,$0c,$00,$0c,$04,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$0c,$00,$0c,$04,$00,$04,$00,$0c,$00,$0c,$08,$0c,$00,$0c
    .byte $00,$00,$00,$08,$14,$0c,$0c,$0c,$00,$18,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; loads palette y from alternate_palettes_tbl if the boss has not been defeated
; input
;  * y - alternate_palettes_tbl offset to load
set_level_palette:
    lda BOSS_DEFEATED_FLAGS
    bne set_palette_exit    ; exit if boss already defeated

; loads palette y from alternate_palettes_tbl
; input
;  * y - alternate_palettes_tbl offset to load
set_palette:
    ldx alternate_palettes_tbl,y
    lda #$03
    sta $00

@palette_loop:
    lda alternate_palettes_tbl+1,y
    sta PALETTE_CPU_BUFFER,x
    iny
    inx
    dec $00
    bne @palette_loop

set_palette_exit:
    rts

; mostly background palettes, but contains 2 sprite palettes as well
; byte 0 = palette buffer offset
; byte 1, 2, and 3 = the palette colors (no unused color)
alternate_palettes_tbl:
    ; #$00 - background palette 2
    .byte $09
    .byte COLOR_WHITE_20,COLOR_MED_OLIVE_18,COLOR_DARK_OLIVE_08

    ; #$04 - background palette 3
    .byte $0d
    .byte COLOR_DARK_GREEN_0a,COLOR_MED_OLIVE_18,COLOR_DARK_OLIVE_08

    ; #$08 - background palette 2
    .byte $09
    .byte COLOR_DARK_VIOLET_02,COLOR_MED_VIOLET_12,COLOR_LT_VIOLET_22

    ; #$0c - background palette 3
    .byte $0d
    .byte COLOR_DARK_VIOLET_02,COLOR_MED_VIOLET_12,COLOR_DARK_GREEN_0a

    ; #$10 - background palette 2
    .byte $09
    .byte COLOR_MED_OLIVE_18,COLOR_MED_GREEN_1a,COLOR_DARK_GREEN_0a

    ; #$14 - background palette 3
    .byte $0d
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$18 - background palette 0
    .byte $01
    .byte COLOR_MED_OLIVE_18,COLOR_MED_GREEN_1a,COLOR_DARK_GREEN_0a

    ; #$1c - background palette 1
    .byte $05
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$20 - background palette 1
    .byte $05
    .byte COLOR_LT_OLIVE_28,COLOR_MED_OLIVE_18,COLOR_DARK_OLIVE_08

    ; #$24 - background palette 2
    .byte $09
    .byte COLOR_DARK_OLIVE_08,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$28 - background palette 3
    .byte $0d
    .byte COLOR_DARK_GRAY_00,COLOR_MED_OLIVE_18,COLOR_DARK_GREEN_0a

    ; #$2c - background palette 2
    .byte $09
    .byte COLOR_LT_TEAL_2c,COLOR_MED_TEAL_1c,COLOR_DARK_TEAL_0c

    ; #$30 - background palette 1
    .byte $05
    .byte COLOR_LT_GREEN_2a,COLOR_MED_BLUE_GREEN_1b,COLOR_DARK_BLUE_GREEN_0b

    ; #$34 - background palette 0
    .byte $01
    .byte COLOR_LT_MAGENTA_24,COLOR_MED_MAGENTA_14,COLOR_DARK_MAGENTA_04

    ; #$38 - background palette 2
    .byte $09
    .byte COLOR_LT_PURPLE_23,COLOR_MED_PURPLE_13,COLOR_DARK_PURPLE_03

    ; #$3c - background palette 2
    .byte $09
    .byte COLOR_LT_TEAL_2c,COLOR_MED_PURPLE_13,COLOR_DARK_TEAL_0c

    ; #$40 - background palette 2
    .byte $09
    .byte COLOR_LT_TEAL_2c,COLOR_MED_TEAL_1c,COLOR_DARK_TEAL_0c

    ; #$44 - background palette 0
    .byte $01
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_BLACK_0f

    ; #$48 - background palette 2
    .byte $09
    .byte COLOR_PALE_TEAL_3c,COLOR_MED_TEAL_1c,COLOR_DARK_TEAL_0c

    ; #$4c - background palette 0
    .byte $01
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$50 - background palette 1
    .byte $05
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$54 - background palette 3
    .byte $0d
    .byte COLOR_DARK_BLUE_GREEN_0b,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$58 - background palette 1
    .byte $05
    .byte COLOR_BLACK_0f,COLOR_MED_PURPLE_13,COLOR_DARK_PURPLE_03

    ; #$5c - background palette 1
    .byte $05
    .byte COLOR_BLACK_0f,COLOR_MED_GREEN_1a,COLOR_DARK_GREEN_0a

    ; #$60 - background palette 1
    .byte $05
    .byte COLOR_BLACK_0f,COLOR_MED_BLUE_GREEN_1b,COLOR_DARK_BLUE_GREEN_0b

    ; #$64 - background palette 1
    .byte $05
    .byte COLOR_DARK_TEAL_0c,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$68 - background palette 0
    .byte $01
    .byte COLOR_DARK_OLIVE_08,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$6c - background palette 1
    .byte $05
    .byte COLOR_DARK_OLIVE_08,COLOR_LT_GRAY_10,COLOR_MED_OLIVE_18

    ; #$70 - background palette 3
    .byte $0d
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$74 - background palette 2
    .byte $09
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$78 - background palette 0
    .byte $01
    .byte COLOR_LT_ORANGE_27,COLOR_MED_ORANGE_17,COLOR_DARK_ORANGE_07

    ; #$7c - background palette 3
    .byte $0d
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00

    ; #$80 - background palette 2
    .byte $09
    .byte COLOR_LT_ORANGE_27,COLOR_MED_RED_16,COLOR_DARK_MAGENTA_04

    ; #$84 - background palette 3
    .byte $0d
    .byte COLOR_MED_RED_16,COLOR_DARK_RED_06,COLOR_DARK_GRAY_00

    ; #$88 - background palette 1
    .byte $05
    .byte COLOR_DARK_OLIVE_08,COLOR_DARK_GRAY_00,COLOR_MED_OLIVE_18

    ; #$8c - background palette 0
    .byte $01
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_MED_OLIVE_18

    ; #$90 - background palette 3
    .byte $0d
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_OLIVE_08

    ; #$94 - background palette 1
    .byte $05
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_VIOLET_02

    ; #$98 - background palette 2
    .byte $09
    .byte COLOR_LT_GRAY_10,COLOR_DARK_GRAY_00,COLOR_DARK_RED_06

    ; #$9c -  background palette 2
    .byte $09
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_RED_06

    ; #$a0 -  background palette 3
    .byte $0d
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_MED_RED_16

    ; #$a4 -  background palette 1
    .byte $05
    .byte COLOR_LT_MAGENTA_24,COLOR_MED_MAGENTA_14,COLOR_DARK_MAGENTA_04

    ; #$a8 -  background palette 2
    .byte $09
    .byte COLOR_WHITE_20,COLOR_LT_GRAY_10,COLOR_DARK_RED_06

    ; #$ac - sprite palette 3
    .byte $1d
    .byte COLOR_LT_MAGENTA_24,COLOR_MED_MAGENTA_14,COLOR_DARK_MAGENTA_04

    ; #$b0 - sprite palette 3
    .byte $1d
    .byte COLOR_WHITE_20,COLOR_DARK_GRAY_00,COLOR_DARK_TEAL_0c

; load the collision code indices for the given offset y
; input
;  * y - collision code starting offset
load_alt_collision_code_indices:
    ldx #$00

@set_alt_collision_code_indices:
    lda alt_collision_code_index_tbl,y  ; load collision code index
    sta COLLISION_CODE_TILE_INDICES,x   ; set collision code index
    iny                                 ; increment read offset
    inx                                 ; increment write offset
    cpx #$10                            ; see if loaded all collision code indices
    bcc @set_alt_collision_code_indices ; loop if more to load
    rts

; each #$10 bytes is a level's worth of collision codes
; see COLLISION_CODE_TILE_INDICES
; compare lvl_collision_code_index_tbl
alt_collision_code_index_tbl:
    .byte $2d,$2d,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; #$00 - level 5 top of cliff
    .byte $fd,$fd,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; #$10 - level 7 boss (temple of terror)
    .byte $10,$33,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; #$20 - level 2 (mid-level)
    .byte $0f,$23,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; #$30 - level 6 before door
    .byte $40,$40,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; #$40 - level 8 stomping ceiling
    .byte $01,$01,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; #$50 - final boss

; end of level fade to black after player walks out of frame
; darkens every #$08 frames
; output
;  * carry flag - set when screen fully dark, clear for more to darken next
fade_to_black:
    dec END_LEVEL_FADE_DELAY ; decrement delay subsequent fade to darker black
    bne @exit_clear          ; exit with more to darken if delay hasn't elapsed
    lda #$08
    sta END_LEVEL_FADE_DELAY ; set delay for subsequent darkening of screen
    lda #$00
    sta $00                  ; initialize number of palettes colors darkened
    ldy #$1f                 ; initialize color palette index (#$1f down to #$00)
                             ; PALETTE_CPU_BUFFER is #$20 bytes

@palette_color_loop:
    lda PALETTE_CPU_BUFFER,y
    cmp #$0f
    beq @next_color          ; branch if palette color is already black
    sec                      ; set carry flag in preparation for subtraction
    sbc #$10                 ; make palette color dimmer
    bcs @continue            ; branch if no overflow to continue
    lda #$0f                 ; overflow set pure black

@continue:
    sta PALETTE_CPU_BUFFER,y ; set new dimmer color
    inc $00                  ; increment number of palette colors darkened

@next_color:
    dey                     ; move to next palette color index
    bpl @palette_color_loop ; branch if more palette colors to write
    sec
    lda $00
    beq @exit               ; exit if no colors were darkened this loop
                            ; this exits with the carry set indicating nothing more to darken

@exit_clear:
    clc ; set carry clear to indicate more colors to darken next animation

@exit:
    rts

; CPU address $faca
reset_vector:
    cld
    sei
    ldx #$ff
    txs         ; initialize stack pointer to $01ff
    lda #$00
    sta PPUCTRL
    sta PPUMASK
    ldx #$02    ; wait for 2 vertical blanking periods
                ; to make sure the PPU has stabilized after reset

@wait_for_vblank:
    bit PPUSTATUS        ; read PPUSTATUS to check the vblank flag (bit 7)
    bpl @wait_for_vblank ; loop until vblank (time when writing to PPU is 'safe')

; vertical blanking
@ensure_vblank:
    bit PPUSTATUS
    bmi @ensure_vblank
    dex                  ; decrement vertical blank initialization count
    bne @wait_for_vblank ; branch if need to wait another vblank period
                         ; for ppu to stabilize
    jsr silence_apu      ; initialize apu
    ldx #$00
    txa
    sta $00
    sta $01
    ldy #$fc

; initialize memory addresses $00fb to $0000 (inclusive) to #$00
@clear_memory:
    dey
    sta ($00),y
    bne @clear_memory
    inc $01
    ldx #$07

; initialize memory addresses $0100 to $01ff (inclusive) to #$00
; then $0200 to $02ff repeatedly until $0600 to $06ff
@clear_memory2:
    sta ($00),y
    iny
    bne @clear_memory2
    inc $01
    cpx $01            ; see if reached the last block of memory to clear
    bne @clear_memory2
    ldx #$e0

; initialize memory $0700 to $07df (inclusively) to #$00
@clear_memory3:
    dex
    sta $0700,x
    bne @clear_memory3
    ldx #$e3
    ldy HI_SCORE_RESET_FLAG
    cpy #$53
    bne @set_hi_score
    ldy HI_SCORE_RESET_FLAG+1
    cpy #$b1
    beq @clear_memory4

; sets high score if HI_SCORE_RESET_FLAG <> #$53 or HI_SCORE_RESET_FLAG+1 #$b1
@set_hi_score:
    ldy #$00
    sty HI_SCORE
    ldy #$20
    sty HI_SCORE+1 ; set hi score to 0020000
    ldx #$e2

; initialize memory $07e2 to $07ff (inclusively) to #$00
@clear_memory4:
    sta $0700,x
    inx
    bne @clear_memory4
    lda #$53
    sta HI_SCORE_RESET_FLAG
    lda #$b1
    sta HI_SCORE_RESET_FLAG+1          ; mark hi score set
    jsr load_sound_banks_init_channels ; load the sound banks (bank c and d), and init pulse and noise channels
    jsr set_ppuctrl_ppumask
    lda #$00
    sta $e000                          ; disable/acknowledge the IRQ
    lda PPUSTATUS
    lda #$10
    tax

; write to PPUADDR #$10 times
@write_ppuaddr:
    sta PPUADDR
    sta PPUADDR
    eor #$10
    dex
    bne @write_ppuaddr
    lda #$00
    sta $a000          ; vertical mirroring (horizontal arrangement)
                       ; A B
                       ; A B
    cli                ; enable IRQ processing

; run between NMI interrupts after nmi_start code finishes
; loop forever updating RANDOM_NUM before NMI
forever_loop:
    inc RANDOM_NUM
    lda RANDOM_NUM    ; load random number
    adc FRAME_COUNTER
    sta RANDOM_NUM    ; randomize random number
    jmp forever_loop

; NMI entry point, beginning of vertical blanking interval (vblank). This happens once per video frame and is triggered by PPU.
; The PPU is available for graphics updates
; The NES will automatically clear the screen so you do not have to worry about trying to clear it with code.
; The end of all the game code will end at RTI (return from interrupt).
nmi_start:
    pha
    txa
    pha
    tya
    pha
    lda PPUSTATUS
    cli                                  ; enable IRQ processing
    ldy NMI_CHECK                        ; load whether nmi was interrupted
    bne handle_sounds_set_ppu_scroll_rti ; branch if nmi interrupted previous frame's game loop
    inc NMI_CHECK                        ; nmi was not interrupted, set to #$01
    lda PPUCTRL_SETTINGS
    and #$7f                             ; strip bit 7 (don't generate an NMI at the start of the vertical blanking interval)
    sta PPUCTRL                          ; prevent trigger of NMI at start of vblank
    sta PPUCTRL_SETTINGS                 ; prevent trigger of NMI at start of vblank
    sty PPUADDR
    sty PPUADDR
    lda PPUMASK_SETTINGS
    and #$e7
    sta PPUMASK
    sty OAMADDR                          ; set OAM address to #00 (DMA is used instead)
    lda #$02                             ; setting OAMDMA to #$02 tells PPU to load sprite data from $0200-$02ff
    sta OAMDMA                           ; write #$100 (256 decimal) bytes ($0200 to $02ff) of data to PPU OAM (the entire screen)
    jsr write_cpu_graphics_buffer_to_ppu ; read the cpu graphics buffer and decodes the data to write to the PPU
                                         ; this is done early after nmi so that vblank hasn't finished
    lda PPUMASK_SETTINGS
    ldx PPU_READY
    beq @continue
    dec PPU_READY
    and #$e7

@continue:
    sta PPUMASK
    jsr init_scanline_irq ; initializes scanline IRQ variables before the first scanline interrupt
                          ; when IRQ_TYPE non-zero, enables scanline interrupts and sets first interrupt
    jsr set_chr_banks
    lda NOOP_IRQ_FLAG     ; load whether or not to set IRQ to do nothing
    beq @exe_game_routine
    jsr set_noop_irq

@exe_game_routine:
    lda #$3c
    jsr load_prg_bank_set                      ; set $8000-$9fff to bank c and $a000-$bfff to bank d
    jsr handle_sound_slots
    jsr load_controller_state                  ; read controller for p1 and p2, stores results into memory
    jsr exe_game_routine                       ; go into game routine loop
    jsr load_bank_6_load_sprites_to_oam_buffer ; write the hud, player, and enemy sprites to OAM CPU buffer
    ldx GRAPHICS_BUFFER_OFFSET
    lda #$00
    sta CPU_GRAPHICS_BUFFER,x
    sta NMI_CHECK                              ; finished executing game logic for frame, mark nmi complete

remove_registers_from_stack_and_rti:
    pla ; pop pushed y value from stack
    tay ; store in y
    pla ; pop pushed x value from stack
    tax ; store in x
    pla ; restore a from stack
    rti ; return to forever_loop until nmi is triggered again
        ; rti pops the processor flags and then the program counter
        ; then starts executing at that location

; nmi interrupted previous frame's game loop, just continue with music
handle_sounds_set_ppu_scroll_rti:
    jsr set_chr_banks    ; set pattern table tiles and nametable mirroring
    lda PPUMASK_SETTINGS
    ldx PPU_READY
    beq @continue
    and #$e7

@continue:
    sta PPUMASK
    jsr init_scanline_irq            ; initializes scanline IRQ variables before the first scanline interrupt
                                     ; when IRQ_TYPE non-zero, enables scanline interrupts and sets first interrupt
    lda NMI_CHECK
    bmi @set_top_chr_banks           ; branch if frame was interrupted when setting sound variables
                                     ; to skip playing any sound
    jsr frame_lag_handle_sound_slots ; frame was interrupted after setting sound variables

@set_top_chr_banks:
    jmp remove_registers_from_stack_and_rti

; set left pattern table to all black tiles
clear_left_pattern_tbl:
    lda #$42 ; all black tiles
    tax      ; left bottom half character bank

; sets the left ($0000-$0fff) pattern table tile banks
; input
;  * a - 2 KiB left top half character bank
;  * x - 2 KiB left bottom half character bank
set_left_pattern_tbl_banks:
    ldy #$00
    sty $8000       ; R0: Select 2 KiB CHR bank at PPU $0000-$07FF (left top half chr bank)
    sta $8001       ; set left top half character bank to bank a
    iny
    sty $8000       ; R1: Select 2 KiB CHR bank at PPU $0800-$0FFF (left bottom half chr bank)
    stx $8001       ; set left bottom half character bank to bank x
    lda BANK_SELECT ; load last backed-up bank select command
    sta $8000       ; set bank select command
    rts

; initializes all the IRQ variables before the first scanline interrupt
; when IRQ_TYPE non-zero, enables scanline interrupts and sets first interrupt
init_scanline_irq:
    lda PPUSTATUS
    ldy #$00
    sty PPUSCROLL
    sty PPUSCROLL
    lda PPUSTATUS
    ldx #$ff
    stx $c000     ; set number of scanlines until trigger scanline IRQ
    stx $c001     ; reload countdown timer
    lda #$10
    sty PPUADDR
    sty PPUADDR
    sta PPUADDR
    sta PPUADDR
    sty PPUADDR
    sty PPUADDR
    sta PPUADDR
    sta PPUADDR
    sty PPUADDR
    sty PPUADDR
    ldx IRQ_TYPE  ; load which IRQ function handler to call, indexes into irq_handler_ptr_tbl
    beq @continue ; branch if irq_handler_00_ptr_tbl (noop handler) to disable IRQ
    ldx #$01      ; non noop IRQ handler, enable IRQ

@continue:
    lda SCANLINE_IRQ_1                   ; load scanline where fist interrupt will occur
    sta $c000                            ; set number of scanlines until trigger scanline IRQ
    sta $c001                            ; reload countdown timer
    sta $e000,x                          ; disable/enable the IRQ
    jsr set_ppuaddr_ppuscroll_enable_nmi ; sets PPUADDR to $2000 (top left nametable)
                                         ; sets PPUSCROLL, and enables NMI for vblank
    lda IRQ_TYPE                         ; load which IRQ function handler to call, indexes into irq_handler_ptr_tbl
    asl                                  ; double since each entry is a #$02 byte memory address
    tay                                  ; transfer to offset register
    lda irq_handler_ptr_tbl,y
    sta IRQ_PTR_TBL                      ; set IRQ type-specific handler pointer table address low byte
    lda irq_handler_ptr_tbl+1,y
    sta IRQ_PTR_TBL+1                    ; set IRQ type-specific handler pointer table address high byte
    lda #$00
    sta IRQ_ROUTINE                      ; set to use initial routine, i.e. irq_handler_xx_00
    lda IRQ_X_SCROLL
    sta SPLIT_X_SCROLL                   ; set horizontal scroll to use after first interrupt
    lda IRQ_PPUCTRL_SETTINGS
    sta SPLIT_PPUCTRL                    ; set PPU control available to use after first interrupt (e.g. used in irq_handler_04_00)
    lda SCANLINE_IRQ_2_DIFF              ; load the number of scanlines after SCANLINE_IRQ_1 to run a 2nd scanline irq
    sta SPLIT_SCANLINE_IRQ_2             ; set the number of scanlines after first IRQ
    lda SCANLINE_IRQ_3_DIFF              ; load the number of scanlines after SCANLINE_IRQ_2 to run a 3rd scanline irq
    sta SPLIT_SCANLINE_IRQ_3
    lda IRQ_PPUADDR
    sta IRQ_HANDLER_PPUADDR
    lda IRQ_PPUADDR+1
    sta IRQ_HANDLER_PPUADDR+1
    rts

; initialize the post-IRQ values for PPU control, horizontal and vertical scroll
; to match the pre-IRQ values
init_irq_scroll:
    lda PPUCTRL_SETTINGS
    sta IRQ_PPUCTRL_SETTINGS
    lda Y_SCROLL             ; load PPU vertical scroll
    sta IRQ_Y_SCROLL
    lda X_SCROLL             ; load PPU horizontal scroll
    sta IRQ_X_SCROLL
    rts

; during next nmi, sets the irq variables to only run remove_registers_from_stack_and_rti
set_nmi_noop_irq:
    inc NOOP_IRQ_FLAG
    rts

; sets the irq variables to only run remove_registers_from_stack_and_rti
set_noop_irq:
    lda #$ff
    sta SCANLINE_IRQ_1      ; set scanline irq to be bottom of screen
    sta SCANLINE_IRQ_2_DIFF ; set next scanline irq to be bottom of screen
    sta SCANLINE_IRQ_3_DIFF ; set next next scanline irq to be bottom of screen
    lda #$00
    sta IRQ_TYPE            ; set irq routine type to irq_handler_00_ptr_tbl (do just rti)
    sta IRQ_ROUTINE         ; set to call remove_registers_from_stack_and_rti
    sta NOOP_IRQ_FLAG       ; do not set noop IRQ
    rts

; sets PPUADDR to $2000 (top left nametable), sets PPUSCROLL, and enables NMI for vblank
set_ppuaddr_ppuscroll_enable_nmi:
    lda PPUSTATUS
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR   ; set PPUADDR to $2000 (top left nametable)
    lda PPUSTATUS
    lda X_SCROLL  ; load PPU horizontal scroll
    sta PPUSCROLL
    lda Y_SCROLL  ; load PPU vertical scroll
    sta PPUSCROLL

enable_nmi_for_vblank:
    lda PPUCTRL_SETTINGS ; load current PPUCTRL settings
    ora #$80             ; set bit 7 (generate an NMI at the start of the vertical blanking interval)
    sta PPUCTRL_SETTINGS ; enable NMI at start of vertical blanking interval
    sta PPUCTRL          ; enable NMI at start of vertical blanking interval
    rts

disable_nmi_for_vblank:
    lda PPUCTRL_SETTINGS ; load current PPUCTRL settings
    and #$7f             ; strip bit 7 (don't generate an NMI at the start of the vertical blanking interval)
    sta PPUCTRL          ; prevent trigger of NMI at start of vblank
    sta PPUCTRL_SETTINGS ; prevent trigger of NMI at start of vblank
    rts

silence_apu:
    lda #$0f
    sta APU_STATUS      ; silence DMC, disable noise, triangle, and pulse channels
    lda #$c0
    sta APU_FRAME_COUNT ; mode = 1 (5 step) and disable frame interrupts
                        ; using MMC3 interrupts instead
    rts

set_ppuctrl_ppumask:
    lda #$a8
    sta PPUCTRL_SETTINGS ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
    sta PPUCTRL          ; enable nmi for vertical blank, 8x16 sprites, base nametable $2000
    lda #$1e
    sta PPUMASK_SETTINGS ; show bg and sprites in leftmost 8 pixels of screen, and enable bg rendering
    lda #$05
    sta PPU_READY        ; reset PPU_READY to ensure #$05 NMI occurs since configure_PPU
    rts

; disables nmi at vertical blanking start, sets PPUMASK and sets PPUADDR to $0000
disable_nmi_set_ppumask:
    jsr disable_nmi_for_vblank
    lda #$00
    sta PPUADDR
    sta PPUADDR
    lda PPUMASK_SETTINGS
    and #$e7                   ; %1110 0111
    sta PPUMASK
    rts

; reads the controllers for p1 and p2
; ultimately stores results into CONTROLLER_STATE,x and CONTROLLER_STATE_DIFF,x
; due to DMC channel DPCM (Delta Pulse Coded Modulation) bug in the APU, input is read twice to confirm
; if the inputs match then it is assumed to be a valid read, otherwise, clears read input
load_controller_state:
    ldx #$00
    jsr read_controller_state   ; read p1 and p2 controller port input
    ldx #$02
    jsr read_controller_state   ; re-read input to confirm it was not affected by DMC channel DPCM bug
    lda $00                     ; start with player 1
    cmp $02                     ; compare first read with second read of p1 port input
    bne invalid_controller_read ; branch if they are not equal (DMC channel DPCM bug)
    lda $01                     ; port 1 controller input read successful, continue to second port
    cmp $03                     ; compare first read with second read of p2 port input
    bne invalid_controller_read ; branch if they are not equal (DMC channel DPCM bug)
    ldx #$00                    ; successful port 1 and port 2 controller input reads
    jsr @set_controller_state   ; set port 1 controller state
    inx                         ; move to port 2 and set controller state

@set_controller_state:
    lda $00,x                     ; load controller input
    tay                           ; transfer to offset register
    eor CTRL_KNOWN_GOOD,x         ; find the differences between previous known-good input and new input
    and $00,x                     ; set a to only have differences in input between last known-good and new input
    sta CONTROLLER_STATE_DIFF,x   ; store input differences value into memory
    sta CONTROLLER_STATE_DIFF_B,x ; store input differences value into memory
    sty CONTROLLER_STATE,x        ; store new known-good input into CONTROLLER_STATE
    sty CTRL_KNOWN_GOOD,x         ; store new known-good input into CTRL_KNOWN_GOOD (used only for controller input code)
    rts

invalid_controller_read:
    lda #$00                      ; clearing port 1 and port 2
    sta CONTROLLER_STATE_DIFF     ; clear port 1
    sta CONTROLLER_STATE_DIFF_B   ; clear port 1
    sta CONTROLLER_STATE_DIFF+1   ; clear port 2
    sta CONTROLLER_STATE_DIFF_B+1 ; clear port 2
    rts

; reads the p1 and p2 controllers
; stores the inputs in a bit field in $04 and $05 respectively
; from msb to lsb: A, B, select, start, up, down, left, right
; sets and immediately clears strobe bit to read from controllers
; input
;  * x - #$00 or #$02 offset in memory to store results, used to read controller input multiple times a frame
;        to confirm input that was read was not affected by DMC channel DPCM bug
read_controller_state:
    ldy #$01
    sty CONTROLLER_1 ; set the strobe bit so controller input for both controllers can be read
    dey
    sty CONTROLLER_1 ; clear strobe bit before starting controller read
    ldy #$08         ; loop counter to go through #$08 inputs
                     ; A, B, select, start, up, down, left, right

; read controller input for individual button press
; pushing each entry into the resulting byte for each controller input
; this looks at both bit 0 and bit 1 from the NES to determine input
; this means the game supports both the standard controller as well as a Famicom expansion port controller
read_controller_button:
    lda CONTROLLER_1           ; read controller input to determine if button is pressed
                               ; Super C is concerned with the 2 least significant bits (NES and Famicom inputs)
    sta $04                    ; store input value in $04
    lsr                        ; move lsb specifying if button is pressed for a standard controller into carry flag
    ora $04                    ; or the original value with the shifted value
                               ; this is essentially also checking if bit 1 (Famicom expansion port controller) is set
    lsr                        ; move bit representing whether the button is pressed to the carry flag
    rol $00,x                  ; shift carry flag (button input flag) onto player 1 controller input bit-field
                               ; $00,x by pushing the button state bit to the next bit
    lda CONTROLLER_2           ; do the same thing for player 2 controller
    sta $05                    ; store input value in $05
    lsr                        ; move lsb specifying if button is pressed for a standard controller into carry flag
    ora $05                    ; or the original value with the shifted value
                               ; this is essentially also checking if bit 1 (Famicom expansion port controller) is set
    lsr                        ; move bit representing whether the button is pressed to the carry flag
    rol $01,x                  ; shift carry flag (button input flag) onto player 1 controller input bit-field
                               ; $01,x by pushing the button state bit to the next bit
    dey                        ; decrement button loop counter
    bne read_controller_button ; loop to see if next button is pressed
    rts                        ; finished reading controller inputs. $04 and $05 contain button state

; load bank 3 and appropriate bank for level
load_prg_bank_3_and_level_bank:
    ldy CURRENT_LEVEL    ; load current level
    lda level_bank_tbl,y

; set PRG ROM at $8000-$9fff based on a register and $a000-$bfff to bank 3
load_prg_bank_a_and_3:
    ldy #$33
    bne load_prg_bank_a_and_y ; set PRG ROM at $8000-$9fff based on a register and $a000-$bfff to bank 3

load_sound_banks:
    ldy $8000
    sty LOW_PRG_BANK
    ldy $bfff
    sty HIGH_PRG_BANK
    lda #$3c

; swaps out PRG ROM at $8000-$9fff and $a000-$bfff based on a register
; value of a register is used to specify bank to load into $8000 and (a + 1) is used to
; specify bank to load into $a000
; Example: when a is #$3c bank c will be loaded into $8000 and bank d will be loaded into $a000
; input
;  * a - bank number to load into PRG memory $8000-$9fff
;      - one is added to a register to determine bank register to load into PRG memory $a000-$bfff
; !(HUH) high nibble is not set, only low bits are used when switching banks
; this is probably an artifact of how development was done with more banks and then the address lines
; for the higher bits weren't used
load_prg_bank_set:
    tay
    iny

; swaps out PRG ROM at $8000-$9fff and $a000-$bfff based on a and y registers
; input
;  * a - bank number to load into PRG memory $8000-$9fff
;  * y - bank number to load into PRG memory $a000-$bfff
load_prg_bank_a_and_y:
    stx PRG_BANK_CHG_X_BACKUP ; backup x
    ldx #$06                  ; tell MMC3 mapper to change PRG ROM bank at $8000-$9fff
    stx BANK_SELECT           ; backup last bank select command
    stx $8000                 ; R6: Select 8 KiB PRG ROM bank at $8000-$9fff
    sta $8001                 ; select PRG bank to be in address $8000-$9fff
    inx                       ; tell MMC3 mapper to change PRG ROM bank at $a000-$bfff
    stx BANK_SELECT           ; backup last bank select command
    stx $8000                 ; R7: Select 8 KiB PRG ROM bank at $a000-$bfff
    sty $8001                 ; select PRG bank to be in address $a000-$bfff
    ldx PRG_BANK_CHG_X_BACKUP ; restore x
    rts

; swaps out PRG ROM at $8000-$9fff and $a000-$bfff based on LOW_PRG_BANK and HIGH_PRG_BANK respectively
load_prg_banks:
    lda LOW_PRG_BANK          ; load bank number of PRG ROM bank at $8000-$9fff
    ldy HIGH_PRG_BANK         ; load bank number of PRG ROM bank at $a000-$bfff
    jmp load_prg_bank_a_and_y

load_prg_bank_a_and_a_plus_1:
    tay
    iny

; swaps out PRG ROM at $8000-$9fff and $a000-$bfff based on a and y registers
; input
;  * a - bank number to load into PRG memory $8000-$9fff
;  * y - bank number to load into PRG memory $a000-$bfff
load_prg_bank_a_and_y_2:
    ldx #$06        ; tell MMC3 mapper to change PRG ROM bank at $8000-$9fff
    stx $8000       ; R6: Select 8 KiB PRG ROM bank at $8000-$9fff
    sta $8001       ; change PRG ROM at $8000-$9fff to specified bank
    inx             ; tell MMC3 mapper to change PRG ROM bank at $a000-$bfff
    stx $8000       ; R7: Select 8 KiB PRG ROM bank at $a000-$bfff
    sty $8001       ; change PRG ROM at $a000-$bfff to specified bank
    ldx BANK_SELECT ; load last backed-up bank select
    stx $8000       ; set the last backed-up bank select command
    rts

; set high PRG bank ($a000-$bfff) containing sound data for sounds >= sound_29
; almost all sounds use bank d, but sound 32 and 36 use bank 1
; input
;  * a - sound code to play
load_high_prg_sound_bank:
    ldy #$31                 ; initialize y to bank 1 for $a000-$bfff
    cmp #$32                 ; see if sound code is #$32
    beq load_high_prg_bank_y ; branch if sound_32 to set the high program rom bank ($a000-$bfff) to 1
    cmp #$36                 ; see if sound code is #$36
    beq load_high_prg_bank_y ; branch if sound_36 to set the high program rom bank ($a000-$bfff) to 1
    ldy #$3d                 ; otherwise set high program rom bank ($a000-$bfff) to bank d

; set the high program rom bank ($a000-$bfff) to bank value specified in y
; input
;  * y - bank number whose bank should be loaded into $a000
load_high_prg_bank_y:
    lda #$07
    sta $8000 ; R7: Select 8 KiB PRG ROM bank at $a000-$bfff
    sty $8001 ; set PRG ROM bank at $a000 to either bank 2, 6, or d
    rts

; PRG bank number for each level
; high nibble isn't used
level_bank_tbl:
    .byte $32,$3a,$32,$3a,$38,$38,$34,$34

frame_lag_handle_sound_slots:
    lda $8000
    pha                              ; backup
    lda $bfff                        ; load bank byte
    pha                              ; backup
    lda #$3c                         ; load prg rom bank c ($8000-$9fff)
    jsr load_prg_bank_a_and_a_plus_1 ; load bank c and d
    jsr handle_sound_slots
    pla
    tay
    pla
    jmp load_prg_bank_a_and_y_2

; input
;  * a - the sound code to play
play_sound:
    pha                             ; push sound code to stack
    lda NMI_CHECK                   ; load NMI_CHECK flag, should always be #$01 here
    ora #$80                        ; ensure most significant bit is set (1)
    sta NMI_CHECK                   ; while bank 1 is loaded and inside init_sound_code_vars
    jsr load_sound_banks            ; set $8000-$9fff to bank c and $a000-$bfff to bank d (sound banks)
                                    ; backs up current banks in LOW_PRG_BANK and HIGH_PRG_BANK
    pla                             ; pop sound code back from stack
    jsr load_sound_banks_init_sound
    jsr load_prg_banks              ; restore previous program rom banks
                                    ; by setting $8000-$9fff to LOW_PRG_BANK and $a000-$bfff to HIGH_PRG_BANK
    lda NMI_CHECK                   ; load NMI_CHECK flag, should always be #$81 here
    and #$7f                        ; finished with init_sound_code_vars, clear bit 7
    sta NMI_CHECK                   ; reset NMI_CHECK flag back to #$01
    rts

; initializes various variables for the specified sound code. Variables are
; later read by handle_sound_code, which actually plays the sound.
; input
;  * a - sound code
load_sound_banks_init_sound:
    sta INIT_SOUND_CODE_00       ; store the sound code to play
    lda $bfff                    ; load current $a000-$bfff bank's bank byte
    pha                          ; back up current high prg bank number
    lda INIT_SOUND_CODE_00
    jsr load_high_prg_sound_bank ; set high PRG bank ($a000-$bfff) for sounds >= sound_29
                                 ; almost all sounds use bank d, but sound 32 and 36 use bank 1
    lda INIT_SOUND_CODE_00
    jsr init_sound_code_vars
    pla                          ; pop high program rom bank value off the stack
    tay                          ; move bank number to y
    jmp load_high_prg_bank_y     ; restore the high program rom bank ($a000-$bfff) from stack

; loads the sound banks (bank c and d), and inits pulse and noise channels
load_sound_banks_init_channels:
    jsr load_sound_banks              ; set $8000-$9fff to bank c and $a000-$bfff to bank d (sound banks)
                                      ; backs up current banks in LOW_PRG_BANK and HIGH_PRG_BANK
    jsr init_pulse_and_noise_channels
    jmp load_prg_banks                ; restore previous program rom banks
                                      ; by setting $8000-$9fff to LOW_PRG_BANK and $a000-$bfff to HIGH_PRG_BANK

; writes the hud, player, and enemy sprites to OAM CPU buffer
load_bank_6_load_sprites_to_oam_buffer:
    lda #$36                       ; load bank 6
    jsr load_prg_bank_set          ; set $8000-$9fff to bank 6 and $a000-$bfff to bank 7
    jmp load_sprites_to_oam_buffer ; write the hud, player, and enemy sprites to OAM CPU buffer

load_banks_exe_enemy_routines:
    jsr load_prg_bank_3_and_level_bank ; load bank 3 and appropriate bank for level
    jmp exe_enemy_routines

load_bank_4_create_screen_enemies:
    lda #$34
    jsr load_prg_bank_set     ; set $8000-$9fff to bank 4 and $a000-$bfff to bank 5
    jmp create_screen_enemies

; run timer logic for ENEMY_GEN_DELAY and if delay elapsed, generate random enemy
load_banks_enemy_gen_routine:
    lda #$30
    jsr load_prg_bank_a_and_3 ; set PRG ROM at $8000-$9fff to bank 0 and $a000-$bfff to bank 3
    jmp enemy_gen_routine     ; run timer logic for ENEMY_GEN_DELAY and if delay elapsed, generate random enemy

; if level 1 (fort firestorm), create the intro helicopter enemy
; otherwise, do nothing
load_banks_lvl_1_create_intro_heli:
    lda #$34
    jsr load_prg_bank_set       ; set $8000-$9fff to bank 4 and $a000-$bfff to bank 5
    jmp lvl_1_create_intro_heli ; if level 1 (fort firestorm), create the intro helicopter enemy
                                ; otherwise, do nothing

; loads appropriate tiles, pallettes, auto-scroll for position in level
load_banks_level_run_tile_routines:
    lda #$36
    jsr load_prg_bank_set       ; set $8000-$9fff to bank 6 and $a000-$bfff to bank 7
    jmp level_run_tile_routines ; load appropriate tiles, pallettes, auto-scroll for position in level

load_banks_run_player_state_routines:
    lda #$30
    jsr load_prg_bank_a_and_3     ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 3
    jmp run_player_state_routines

; clear destroyed bullets, copy bullet data to general sprite buffers
load_banks_handle_player_bullets:
    lda #$30
    jsr load_prg_bank_a_and_3
    jmp handle_player_bullets ; clear destroyed bullets, copy bullet data to general sprite buffers

; copies values from bullet-specific buffers to the general sprite buffers
; copies half of all bullets, alternating every frame which half
load_banks_cp_bullet_to_sprite_buffers:
    lda #$30
    jsr load_prg_bank_set           ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp cp_bullet_to_sprite_buffers

; create child flames for F weapon or destroy if not F weapon
load_banks_create_f_child_flames_or_destroy:
    lda #$30
    jsr load_prg_bank_set                ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jsr create_f_child_flames_or_destroy ; create child flames for F weapon or destroy
    jmp load_prg_bank_3_and_level_bank   ; load bank 3 and appropriate bank for level

; load the palette colors for the current level or scene (intro scenes, and ending credits)
load_banks_set_palette_for_current_level:
    lda #$36
    jsr load_prg_bank_set              ; set $8000-$9fff to bank 6 and $a000-$bfff to bank 7
    jmp set_palettes_for_current_level

; loads the palette colors for the level or scene (intro scenes, and ending credits)
; input
;  * a - level of palettes to load. additionally #$08, and #$09 for intro, and #$0a for ending credits
load_banks_set_palette:
    pha
    lda #$36
    jsr load_prg_bank_set ; set $8000-$9fff to bank 6 and $a000-$bfff to bank 7
    pla
    jmp set_palettes

; write pattern tile (text) or palette information (color) to CPU offset CPU_GRAPHICS_BUFFER
; this is used when GRAPHICS_BUFFER_MODE is #$00, which defines the CPU_GRAPHICS_BUFFER format for text and palette data
; input
;  * a - first six bits are index into the short_text_pointer_table
;  when bit 7 set, write all blank characters instead of actual characters. Used for flashing effect
load_bank_0_write_text_to_mem:
    pha                   ; backup the specific text string to load
    lda #$30              ; load bank 0 into $8000-$9fff
    jsr load_prg_bank_set ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    pla                   ; restore the specific text string to load
    jmp write_text_to_mem ; write text string a to the CPU_GRAPHICS_BUFFER for rendering

; input
;  * x - graphic_data_ptr_tbl offset
load_banks_write_graphic_data_to_ppu:
    lda #$30
    jsr load_prg_bank_set         ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp write_graphic_data_to_ppu

load_banks_x_nt_draw_routine_02:
    jsr load_level_graphic_banks
    jmp x_nt_draw_routine_02

load_banks_write_supertile_to_buffer:
    jsr load_level_graphic_banks
    jmp write_supertile_to_buffer

; write off-screen nametable tile data to CPU_GRAPHICS_BUFFER in response to scroll
load_banks_run_draw_routines:
    jsr load_level_graphic_banks
    jmp run_draw_routines        ; run horizontal and vertical draw routines

load_banks_load_level_header:
    jsr load_level_graphic_banks
    jmp load_level_header

; updates a nametable supertile (4x4 tile) and its palette
; input
;  * a - X position
;  * y - Y position
;  * $08 - supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
; output
;  * x - graphics buffer write offset
load_banks_update_supertile_and_palette:
    ldx #$e0

; updates nametable supertile (4x4 tile) at (a, y) and possibly its palette
; input
;  * a - X position
;  * x - when #$e0, specifies to update the palette for the supertile based on palette offset $08
;  * $08 - supertile data and palette data offset (LEVEL_SUPERTILE_DATA_PTR offset)
;  * y - Y position
load_banks_update_nametable_supertile:
    stx $06
    pha
    tya
    pha
    jsr load_level_graphic_banks       ; load appropriate banks for graphic data for each level
    pla
    tay
    pla
    jsr update_nametable_supertile     ; update nametable supertile (4x4 tile) at (a, y) and possibly its palette
    jmp load_prg_bank_3_and_level_bank ; load bank 3 and appropriate bank for level

; load appropriate banks (0 and 1) and run player input simulation for demo
load_banks_run_demo_input:
    lda #$30
    jsr load_prg_bank_set ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp run_demo_input    ; run player input simulation for demo

load_banks_set_level_scroll_screen:
    lda #$30
    jsr load_prg_bank_set       ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp set_level_scroll_screen

load_banks_init_level_vars:
    lda #$30
    jsr load_prg_bank_set ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp init_level_vars

load_banks_run_end_level_anim_routine:
    lda #$30
    jsr load_prg_bank_set          ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp run_end_level_anim_routine

load_bank_4_run_sound_menu_routine:
    lda #$34
    jsr load_prg_bank_set      ; set $8000-$9fff to bank 4 and $a000-$bfff to bank 5
    jmp run_sound_menu_routine

; check for 10 extra lives cheat
load_banks_extra_lives_cheat_input_check:
    lda #$30
    jsr load_prg_bank_set             ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp extra_lives_cheat_input_check ; check for 10 extra lives cheat

; shows flashing level number and reads d-pad for level select
load_banks_level_select:
    lda #$30
    jsr load_prg_bank_set ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp level_select      ; show flashing level number and reads d-pad for level select

; animate reveal of logo and showing of the player selection and license text
; output
;  * carry flag - set when all animation routines have completed (animation finished)
load_banks_run_intro_animation_routine:
    lda #$30
    jsr load_prg_bank_set           ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp run_intro_animation_routine

load_banks_run_scroll_credits_routine:
    lda #$30
    jsr load_prg_bank_set          ; set $8000-$9fff to bank 0 and $a000-$bfff to bank 1
    jmp run_scroll_credits_routine

; loads appropriate banks for graphic data for each level
; level 1 - bank 4 and 5
; level 2 - bank 0 and 1
; level 3 - bank 6 and 7
; level 4 - bank 6 and 7
; level 5 - bank 8 and 9
; level 6 - bank 8 and 9
; level 7 - bank a and b
; level 8 - bank a and b
load_level_graphic_banks:
    ldy CURRENT_LEVEL            ; load current level
    lda level_graphic_bank_tbl,y
    jmp load_prg_bank_set        ; set $8000-$9fff and $a000-$bfff to banks appropriate for level

; specifies which banks to load for graphic data for each level
; high nibble isn't used
level_graphic_bank_tbl:
    .byte $34,$30,$36,$36,$38,$38,$3a,$3a

; sets the character banks (pattern table tiles) and nametable mirroring
; input
;  * NT_MIRRORING - nametable mirroring (0: vertical; 1: horizontal)
;  * LEFT_TOP_HALF_CHR_BANK - bank number for top half of the left pattern table
;  * LEFT_BOTTOM_CHR_HALF_BANK - bank number for bottom half of the left pattern table
;  * RIGHT_FIRST_QTR_CHR_BANK - bank number for first quarter of the right pattern table
;  * RIGHT_SECOND_QTR_CHR_BANK - bank number for second quarter of the right pattern table
;  * RIGHT_THIRD_QTR_CHR_BANK - bank number for third quarter of the right pattern table
;  * RIGHT_FOURTH_QTR_CHR_BANK - bank number for fourth quarter of the right pattern table
set_chr_banks:
    lda NT_MIRRORING              ; load nametable mirror setting
    sta $a000                     ; set nametable mirroring (0: vertical; 1: horizontal)
    ldy #$00
    sty $8000                     ; R0: Select 2 KiB CHR bank at PPU $0000-$07FF
    lda LEFT_TOP_HALF_CHR_BANK    ; load bank number of PPU $0000-$07ff (top half of left pattern table)
    sta $8001
    iny
    sty $8000                     ; R1: Select 2 KiB CHR bank at PPU $0800-$0FFF
    lda LEFT_BOTTOM_CHR_HALF_BANK ; load bank number of PPU $0800-$0fff (bottom half of left pattern table)
    sta $8001
    iny
    sty $8000                     ; R2: Select 1 KiB CHR bank at PPU $1000-$13FF
    lda RIGHT_FIRST_QTR_CHR_BANK  ; load bank number of PPU $1000-$13ff (first quarter of right pattern table)
    sta $8001
    iny
    sty $8000                     ; R3: Select 1 KiB CHR bank at PPU $1400-$17FF
    lda RIGHT_SECOND_QTR_CHR_BANK ; load bank number of PPU $1400-$17ff (second quarter of right pattern table)
    sta $8001
    iny
    sty $8000                     ; R4: Select 1 KiB CHR bank at PPU $1800-$1BFF
    lda RIGHT_THIRD_QTR_CHR_BANK  ; load bank number of PPU $1800-$1bff (third quarter of right pattern table)
    sta $8001
    iny
    sty $8000                     ; R5: Select 1 KiB CHR bank at PPU $1C00-$1FFF
    lda RIGHT_FOURTH_QTR_CHR_BANK ; load bank number of PPU $1c00-$1fff (last quarter of right pattern table)
    sta $8001
    rts

; unused #$9e bytes out of #$2,000 bytes total (98.07% full)
; unused 158 bytes out of 8,192 bytes total (98.07% full)
; filled with 158 #$ff bytes by superc.cfg configuration
bank_f_unused_space:

.segment "BANKF_ID"

; presumed assembly date - November 29, 1989
; MAST is probably short for "master" as in it's the master version/copy
    .byte "MAST891129"

.segment "VECTORS"

; locations of all 'vectors'. These are the 3 handles for NES interrupts
; stored in the .nes ROM as the last $06 bytes (CPU addresses $fffa-$ffff)
; these are stored at known locations so the NES can point the instruction
; pointer at known locations for triggering interrupts.
  .addr nmi_start, reset_vector, irq