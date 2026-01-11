; NES Super C Disassembly - v1.01
; https://github.com/vermiceli/nes-super-c/
; Bank C contains the sound engine and some of the encoded sound data.

; 8 KiB PRG ROM
.segment "BANKC"

.include "constants.asm"

; import from bank 1
.import sound_32_slot_00
.import sound_32_slot_01
.import sound_32_slot_02
.import sound_32_slot_03
.import sound_36_slot_00
.import sound_36_slot_01
.import sound_36_slot_02
.import sound_36_slot_03

; import from bank d
.import sound_29_slot_01_3
.import sound_29_slot_02
.import sound_29_slot_03
.import sound_2a_slot_00
.import sound_2a_slot_01
.import sound_2a_slot_02
.import sound_2a_slot_03
.import sound_2b_slot_00
.import sound_2b_slot_01
.import sound_2b_slot_02
.import sound_2b_slot_03
.import sound_2c_slot_00
.import sound_2c_slot_01
.import sound_2c_slot_02
.import sound_2c_slot_03
.import sound_2d_slot_00
.import sound_2d_slot_01
.import sound_2d_slot_02
.import sound_2d_slot_03
.import sound_2f_slot_00
.import sound_2f_slot_01
.import sound_2f_slot_02
.import sound_2f_slot_03
.import sound_30_slot_00
.import sound_30_slot_01
.import sound_30_slot_02
.import sound_30_slot_03
.import sound_31_slot_00
.import sound_31_slot_01
.import sound_31_slot_02
.import sound_31_slot_03
.import sound_33_slot_00
.import sound_33_slot_01
.import sound_33_slot_02
.import sound_33_slot_03
.import sound_34_slot_00
.import sound_34_slot_01
.import sound_34_slot_02
.import sound_34_slot_03
.import sound_35_slot_00
.import sound_35_slot_01
.import sound_35_slot_02
.import sound_35_slot_03

; import from bank e
.import dpcm_sample_00
.import dpcm_sample_01
.import dpcm_sample_02
.import dpcm_sample_03
.import dpcm_sample_04
.import dpcm_sample_05
.import dpcm_sample_06
.import dpcm_sample_07
.import dpcm_sample_08
.import dpcm_sample_09
.import dpcm_sample_0a
.import dpcm_sample_0b
.import dpcm_sample_0c

; import from bank f
.import load_high_prg_sound_bank
.import load_sound_banks_init_sound

; export for bank d
.export sound_29_slot_01

; export for bank f
.export init_sound_code_vars
.export init_pulse_and_noise_channels
.export handle_sound_slots

.byte $3c ; bank byte

; set pulse channel duty cycle, volume, and sweep data, mute noise channel
init_pulse_and_noise_channels:
    lda #$00
    stx SOUND_VAR_1 ; backup x
    tax

; write #$00 sound code to all sound slots
@loop:
    sta SOUND_CODE,x
    .ifdef Probotector
        sta SOUND_PROBO_2,x
    .endif
    inx
    cpx #$06
    bcc @loop
    sta SOUND_FRAME_SKIP_COUNT
    ldx SOUND_VAR_1            ; restore x

reset_channels:
    jsr init_triangle_channel
    lda #$30
    sta APU_PULSE_CONFIG
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif
    sta APU_PULSE2_CONFIG
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif
    sta APU_NOISE_CONFIG
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif
    lda #$7f
    sta APU_PULSE_SWEEP
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif
    sta APU_PULSE2_SWEEP
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif
    rts

; initializes various variables for the sound code slots.  Variables are later
; read by handle_sound_code, which actually plays the sound.
; This method also initializes the pulse and noise channels for sound effects
; input
;  * a - the sound code to play
init_sound_code_vars:
    sta INIT_SOUND_CODE  ; store the sound code to play
    cmp #$37
    bcc @check_bg        ; branch if not a dmc sample
    jmp play_dpcm_sample ; sound code larger than or equal to #$37, sound code is a DPCM sample
                         ; jump to configure DMC (delta modulation channel) and play sample

@check_bg:
    cmp #$28
    bcc @check_mortar_round           ; branch if not background music (bg music)
    jsr init_pulse_and_noise_channels ; sound effect, initialize pulse and noise channels

@check_mortar_round:
    cmp #$17               ; see if sound_17 (mortar round) (actually silent)
    bne @get_sound_address
    cmp SOUND_CODE+4       ; sound_17, see if sound slot for pulse 1 is also set to sound_17
    bne @get_sound_address ; branch if they are different sound codes
    rts

@get_sound_address:
    stx SOUND_VAR_2         ; backup x register
    sty SOUND_VAR_3         ; backup y register
    lda INIT_SOUND_CODE     ; load the sound code to play
    asl                     ; double since each entry is a #$02 byte memory address
    tay                     ; transfer to offset register
    lda sound_code_tbl-2,y
    sta SOUND_CODE_ADDR
    lda sound_code_tbl-1,y
    sta SOUND_CODE_ADDR+1
    ldy #$00                ; initialize sound code read offset
    lda (SOUND_CODE_ADDR),y ; read first byte of sound code
    sta NUM_SOUND_PARTS     ; store number of sound parts
                            ; (actual number of parts is 1 greater than this value)

@sound_part_vars_loop:
    iny                     ; increment sound code read offset
    lda (SOUND_CODE_ADDR),y ; read next byte of sound code (sound slot index)
    tax                     ; transfer sound slot index to x
    lda INIT_SOUND_CODE     ; load sound code
    cmp SOUND_CODE,x        ; see if another sound code is playing for the current slot
    bcs @continue           ; branch if no sound playing in slot or sound code to play takes priority
                            ; over sound code currently using the sound slot
                            ; i.e. the sound code to play is larger than the one playing
    iny                     ; move to next sound part as existing sound gets priority for the slot
    iny                     ; skip sound part address (2 bytes)
    jmp @next_sound_part    ; and move to next sound part

@continue:
    iny                            ; increment sound code read offset
    lda (SOUND_CODE_ADDR),y        ; load sound code byte (sound part address low byte)
    sta SOUND_CMD_ADDRS_LOW,x      ; set low byte of sound part address (sound_xx_slot_xx)
    .ifdef Superc
        sta SOUND_PART_ADDR        ; set low byte of sound part address (sound_xx_slot_xx)
    .endif
    iny                            ; increment sound read offset
    lda (SOUND_CODE_ADDR),y        ; load sound code byte (sound part address high byte)
    sta SOUND_CMD_ADDRS_HIGH,x     ; set high byte of sound part address (sound_xx_slot_xx)
    .ifdef Superc
        sta SOUND_PART_ADDR+1      ; set high byte of sound part address (sound_xx_slot_xx)
    .endif
    lda #$00
    sta SOUND_CMD_REPEATS,x
    lda #$01
    sta SOUND_CMD_LENGTH,x         ; set to #$01 so that the sound code part will be read in handle_sound_code
    cpx #$03                       ; see if sound slot 3 (noise and dmc channel)
    beq @read_byte_0               ; branch if slot 3 (noise and dmc channel)
    lda #$f8                       ; not slot 3
    sta SOUND_LEN_TIMERS_HIGH,x    ; set all length bits (L) to 1 (#$1e)
    cpx #$05                       ; see if sound slot 5 (noise channel)
    beq @read_byte_0               ; branch if noise channel
    lda #$00                       ; pulse or triangle channel
    sta VIBRATO_CTRL,x
    sta SOUND_VOLUME_ADJ2,x
    sta SOUND_TIMER_ADJ,x
    cpx #$04                       ; see if sound slot 4 (pulse 1)
    beq @read_byte_0               ; branch if sound slot 4 (pulse 1)
    sta SOUND_NOTE_PERIOD_OFFSET,x
    cpx #$02                       ; see if sound slot #$02 (triangle channel)
    beq @read_byte_0               ; branch if sound slot #$02 (triangle channel)
    sta SOUND_CFG_HIGH_B,x         ; sound slot #$00 or #$01 (pulse 1, or pulse 2 channel)
    lda #$01
    sta SOUND_PITCH_ADJ_TIMER,x
    lda #$80
    sta SOUND_PITCH_FLAGS,x

@read_byte_0:
    .ifdef Probotector
            lda INIT_SOUND_CODE      ; load current sound code
            cmp #$28
            bcc @sound_effect        ; branch if sound effect
            lda #$00                 ; sound is background music (bgm)
            beq @set_sound_part_next ; always branch since background music (bgm)

        @sound_effect:
            lda #$01

        @set_sound_part_next:
            sta SOUND_FLAGS,x
    .else
            sty SOUND_VAR_1         ; store sound code read offset
            ldy #$00
            lda (SOUND_PART_ADDR),y ; read first byte of sound part, e.g. sound_xx_slot_xx
                                    ; specifies sound type, i.e. sound effect or background music (bgm)
            iny                     ; initialize y to #$01
            cmp #$10                ; compare byte 0 of sound part to #$10
            bcc @set_sound_flags    ; branch if sound effect command (< #$10)
            cpx #$04                ; see if sound slot 4 (pulse 1)
            beq @set_sound_flags    ; branch if slot 4 (pulse 1) (sound effect command)
            dey                     ; not slot 4 and y >= #$10, background music (bgm) command

        @set_sound_flags:
                              ; #$01 - byte 0 <= #$10 or slot 4 (read_sound_effect_cmd)
            tya               ; #$00 - otherwise (read_bgm_cmd)
            sta SOUND_FLAGS,x ; set sound command type (sound effect or background music)
            ldy SOUND_VAR_1   ; re-load sound code read offset
    .endif
    lda INIT_SOUND_CODE       ; load current sound code
    sta SOUND_CODE,x          ; set sound code

@next_sound_part:
    dec NUM_SOUND_PARTS       ; decrement sound part parsing index
    bmi @exit                 ; exit if parsed all sound parts
    jmp @sound_part_vars_loop ; loop to parse next sound code part

@exit:
    ldx SOUND_VAR_2 ; restore x register
    ldy SOUND_VAR_3 ; restore y register
    rts

; input
;  * a - pause state
check_pause_reset_channels:
    sta PAUSE_STATE_01 ; set current pause value (0 = unpaused, 1 = paused)
    cmp #$00           ; see if unpausing
    beq @unpausing     ; if unpausing, continue to play level music
    jmp reset_channels ; pausing, reset triangle, noise, and pulse channels to stop level music

@unpausing:
    lda SOUND_CODE+4                ; load sound code for sound slot 4
    bne @exit                       ; exit if sound slot 4 is already playing
    lda SOUND_CODE                  ; load sound code for slot 0
    beq @exit                       ; exit if sound slot 0 is empty
    ldx #$00                        ; sound slot 0
    ldy #$00                        ; pulse channel 1
    jsr set_pulse_channel_registers
                                    ; could have been optimized to a jmp call with no rts

@exit:
    rts

; silences pulse wave channel
; after pause or level end, or anytime need to silence pulse wave channel
; input
;  * x - sound register offset
;  * y - APU channel register offset
mute_pulse_channel:
    lda #$30                       ; a = #$30
    sta APU_PULSE_CONFIG,x         ; set volume to 0 and duty cycle to 25%
    .ifdef Superc
        jsr wait                   ; execute #$0a nop instructions
    .endif
    bne set_pulse_timer_and_length ; always branch to set pulse timer and length

; sets volume, timer (pitch), and length
; mutes/unmutes pulse wave channel based on pause state
; input
;  * x - sound slot
;  * y - sound register write offset
set_pulse_channel_registers:
    lda SOUND_FLAGS,x             ; load the current sound slot's sound flags
    and #$41                      ; keep bits .x.. ...x
    ora PAUSE_STATE_01            ; merge with current game pause state (0 = unpaused, 1 = paused)
    bne mute_pulse_channel        ; branch if paused
    lda SOUND_PULSE_VOLUME,x
    jsr calc_and_set_pulse_config
    ldx #$00

set_pulse_timer_and_length:
    lda SOUND_PULSE_PERIOD_LOW,x   ; load in memory pulse period value
    sta APU_PULSE_PERIOD_LOW,y     ; set APU pulse period value
    .ifdef Superc
        jsr wait                   ; execute #$0a nop instructions
    .endif
    lda SOUND_LEN_TIMERS_HIGH,x
    ora #$08
    sta APU_PULSE_LEN_TIMER_HIGH,y
    .ifdef Probotector
        rts
    .else
        jmp wait                   ; execute #$0a nop instructions
    .endif

handle_sound_slots:
    .ifdef Probotector
        jsr probotector_mark_bgm
    .endif
    lda SOUND_FRAME_SKIP_COUNT   ; see if should skip sound for current frame
    beq @continue                ; branch to continue if this sound doesn't skip any frames
    inc SOUND_FRAME_SKIP         ; not running sound routine this frame, used only for sound_32
    lda SOUND_FRAME_SKIP
    cmp SOUND_FRAME_SKIP_COUNT
    bne @continue                ; branch to play sounds this frame
    lda #$00                     ; counted up to SOUND_FRAME_SKIP_COUNT, skip playing sound this frame
    sta SOUND_FRAME_SKIP         ; and reset skip counter
    rts

@continue:
    lda PAUSE_STATE                ; (0 = not paused, 1 = paused)
    cmp PAUSE_STATE_01             ; compare to last frame pause state
    beq @sound_slots
    jsr check_pause_reset_channels

@sound_slots:
    ldx #$00
    ldy #$00

; input
;  * x - sound slot index
;  * y - channel register offset, e.g. #$00 (pulse 1 channel), #$04 (pulse 2
;    channel), #$08 (triangle channel), #$0c (noise/dmc channel)
@sound_slot_loop:
    stx SOUND_CURRENT_SLOT    ; set the current sound slot to the current loop index
    sty SOUND_CHNL_REG_OFFSET ; set sound channel config register offset (#$00, #$04, #$08, or #$0c)
    lda SOUND_CODE,x          ; load sound code for sound slot
    beq @prep_next_loop       ; prep to move to next sound slot, or exit if looped through all slots
    jsr handle_sound_code     ; read and interpret sound code in slot x

@prep_next_loop:
    inx                         ; increment read offset
    cpx #$06                    ; see if looped through all sound slots
    beq sound_music_entry_exit  ; exit if looped through all sound slots
    lda sound_channel_reg_tbl,x ; load sound channel config base register (#$00, #$04, #$08, or #$0c) for slot
    tay
    jmp @sound_slot_loop

sound_music_entry_exit:
    rts

; table to determine the APU register base offset from the sound slot
sound_channel_reg_tbl:
    .byte $00 ; sound slot #$00 (pulse 1 channel) --> $4000
    .byte $04 ; sound slot #$01 (pulse 2 channel) --> $4004
    .byte $08 ; sound slot #$02 (triangle channel) --> $4008
    .byte $0c ; sound slot #$03 (noise and dmc channel) --> $400c
    .byte $00 ; sound slot #$04 (pulse 1 channel) --> $4000
    .byte $0c ; sound slot #$05 (noise channel) --> $400c

    .ifdef Probotector
        probotector_mark_bgm:
            lda PAUSE_STATE
            bne @exit
            inc SOUND_VOLUME_ADJ2+2
            lda SOUND_VOLUME_ADJ2+2
            cmp #$06
            bne @exit
            lda #$00
            sta SOUND_VOLUME_ADJ2+2
            tax

        @mark_bgm_slots:
            cpx #$04
            beq @exit
            lda SOUND_CODE,x
            beq @next_slot
            lda SOUND_FLAGS,x
            lsr
            bcs @next_slot          ; branch if sound effect to skip
            dec SOUND_CMD_LENGTH,x  ; background music (bgm)
            beq @mark_bgm_playing
            jsr @existing_sound_cmd
            inx
            jmp @mark_bgm_slots

        @mark_bgm_playing:
            inc SOUND_CMD_LENGTH,x ; set length back
            lda #$01
            sta $01ce,x            ; mark background music playing

        @next_slot:
            inx
            jmp @mark_bgm_slots

        @existing_sound_cmd:
            cpx #$02
            bcs @exit
            lda SOUND_PITCH_FLAGS,x
            bmi @exit
            lda SOUND_LENGTH_MULTIPLIER,x
            sta SOUND_VAR_1
            lda SOUND_PITCH_FLAGS,x
            and #$10
            beq @continue
            lda SOUND_VAR_1
            asl a
            sta SOUND_VAR_1

        @continue:
            lda SOUND_CMD_LENGTH,x

        @loop:
            sec
            sbc SOUND_VAR_1
            beq @handle_end_bgm_cmd
            bcs @loop

        @exit:
            rts

        @handle_end_bgm_cmd:
            lda SOUND_PERIOD,x
            sta CUR_SOUND_PERIOD_LOW
            lda SOUND_LEN,x
            sta CUR_SOUND_LEN_TIMER_HIGH
            jmp overwrite_pitches
    .endif

; read and interpret loaded sound code in slot x
; output
;  * x - sound slot with the sound code to handle
handle_sound_code:
    jsr load_high_prg_sound_bank    ; set high PRG bank ($a000-$bfff) for sounds >= sound_29
    lda PAUSE_STATE_01              ; load current game pause state (0 = unpaused, 1 = paused)
    beq @check_sound_command        ; branch if game not paused
    lda SOUND_CODE,x                ; game paused, load sound code for current slot
    cmp #$27                        ; see if sound code is the pause jingle (sound_27)
    .ifdef Probotector
        bne sound_music_entry_exit2 ; exit if not the game pausing jingle sound
    .else
        bne sound_music_entry_exit  ; exit if not the game pausing jingle sound
    .endif

@check_sound_command:
    dec SOUND_CMD_LENGTH,x     ; decrement the remaining number of video frames sound should play for
                               ; before continuing to the next sound command
    bne existing_sound_cmd     ; branch if sound command hasn't finished executing
    ldy #$00                   ; previous sound slot command finished, move to next sound slot command
    lda SOUND_CMD_ADDRS_LOW,x  ; load low byte of sound part address (sound_xx_slot_xx) for the sound slot
    sta CUR_SOUND_ADDR         ; set low byte of current slot's sound part address (sound_xx_slot_xx)
    lda SOUND_CMD_ADDRS_HIGH,x ; load high byte of sound part address (sound_xx_slot_xx) for the sound slot
    sta CUR_SOUND_ADDR+1       ; set high byte of current slot's sound part address (sound_xx_slot_xx)
    lda SOUND_FLAGS,x
    jmp read_sound_cmd

.ifdef Probotector
    sound_music_entry_exit2:
        rts
.endif

existing_sound_cmd:
    cpx #$02                        ; compare sound slot to #$02 (triangle channel)
    beq triangle_adj
    .ifdef Probotector
        bcs sound_music_entry_exit2 ; exit if sound slot #$03 (noise and dmc), #$04 (pulse 1), or #$05 (noise)
    .else
        bcs sound_music_entry_exit  ; exit if sound slot #$03 (noise and dmc), #$04 (pulse 1), or #$05 (noise)
    .endif
    lda SOUND_FLAGS,x               ; sound slot #$00 (pulse 1), or #$01 (pulse 2)
    and #$41
    .ifdef Probotector
        bne sound_music_entry_exit2
    .else
        bne sound_music_entry_exit
    .endif
    lda SOUND_PITCH_FLAGS,x
    bmi check_pitch_adjust
    lda SOUND_LENGTH_MULTIPLIER,x
    sta SOUND_VAR_1
    lda SOUND_PITCH_FLAGS,x
    and #$10
    beq @continue
    lda SOUND_VAR_1                 ; load sound length multiplier
    asl
    sta SOUND_VAR_1                 ; double sound length multiplier

@continue:
    lda SOUND_CMD_LENGTH,x

@loop:
    sec                    ; set carry flag in preparation for subtraction
    sbc SOUND_VAR_1        ; subtract SOUND_LENGTH_MULTIPLIER,x or SOUND_LENGTH_MULTIPLIER,x * 2
    beq handle_end_bgm_cmd
    bcs @loop

; check and adjust pitch using SOUND_PITCH_ADJ_TIMER,x if appropriate
; then continue with reading current bgm_cmd
check_pitch_run_bgm_cmd:
    lda SOUND_FLAGS,x
    and #$06
    cmp #$06
    beq run_bgm_cmd   ; branch if no decrescendo (using sound_control_envelope)

check_pitch_adjust:
    lda VIBRATO_CTRL,x
    and #$10
    beq run_bgm_cmd
    dec SOUND_PITCH_ADJ_TIMER,x
    bne run_bgm_cmd
    inc SOUND_PITCH_ADJ_TIMER,x      ; SOUND_PITCH_ADJ_TIMER,x elapsed, reset
    jsr calc_current_pitch
    jsr write_slot_len_period_to_apu

; set volume and run current bgm_cmd based on SOUND_FLAGS,x bits 1 and 2
run_bgm_cmd:
    dec SOUND_VOLUME_CHANGE_DELAY,x
    .ifdef Probotector
        bne sound_music_entry_exit2
    .else
        bne sound_music_entry_exit
    .endif
    inc SOUND_VOLUME_CHANGE_DELAY,x ; restore volume
    jsr pulse_set_volume            ; set volume in APU based on many variables
    lda SOUND_FLAGS,x
    and #$06
    tay                             ; transfer to offset register
    lda bgm_cmd_ptr_tbl,y
    sta SOUND_VAR_1
    lda bgm_cmd_ptr_tbl+1,y
    sta SOUND_VAR_1+1
    jmp (SOUND_VAR_1)

handle_end_bgm_cmd:
    lda SOUND_PERIOD,x
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    lda SOUND_LEN,x
    sta CUR_SOUND_LEN_TIMER_HIGH
    jsr overwrite_pitches
    jmp check_pitch_run_bgm_cmd  ; branch to continue
                                 ; to check and adjust pitch and read current bgm_cmd

; used in BGM7
; slot 2 (triangle) sound command that isn't finished from previous frame
; input
;  * x - sound slot (always #$02)
triangle_adj:
    inc SOUND_EFFECT_VOLUME_CONTINUED
    lda SOUND_EFFECT_VOLUME_CONTINUED
    and #$40
    bne @exit
    lda SOUND_EFFECT_VOLUME_CONTINUED
    and #$0f
    bne @exit
    lda SOUND_EFFECT_VOLUME,x
    beq @exit
    cmp #$80
    bcs @exit
    lda SOUND_TIMER_ADJ+3             ; load current linear counter reload value
                                      ; (fine grain duration before silencing channel)
    sec                               ; set carry flag in preparation for subtraction
    sbc #$40
    bcc @exit                         ; exit if SOUND_TIMER_ADJ+3 is less than #$40
    sta SOUND_TIMER_ADJ+3             ; set new linear counter reload value
    sta APU_TRIANGLE_CONFIG           ; set new linear counter reload value (fine grain duration)
    .ifdef Superc
        jsr wait                      ; execute #$0a nop instructions
    .endif

@exit:
    rts

; input
;  * x
sound_control_envelope:
    lda SOUND_CTRL_ENVELOPE_OFFSET,x ; load sound_envelope_ptr_tbl offset
    asl                              ; double since each entry is a #$02 byte memory address
    tay                              ; transfer to offset register
    lda sound_envelope_ptr_tbl,y
    sta SOUND_VAR_1
    lda sound_envelope_ptr_tbl+1,y
    sta SOUND_VAR_1+1

; read sound_envelope_xx byte and, if not control byte, calculate and set volume
; based on byte
sound_envelope_read:
    lda SOUND_ENVELOPE_READ_OFFSET,x   ; load sound_envelope_xx read offset
    tay
    lda (SOUND_VAR_1),y                ; read sound_envelope_xx byte 0
    cmp #$fb
    bcs handle_sound_envelope_cmd_byte ; branch if control byte has been read
    cmp #$10
    bcc @read_volume
    lsr
    lsr
    lsr
    lsr
    sta SOUND_VOLUME_CHANGE_DELAY,x

; byte 1 less than #$10 or using high nibble shifted
@read_volume:
    inc SOUND_ENVELOPE_READ_OFFSET,x   ; increment read offset
    lda (SOUND_VAR_1),y                ; re-read byte
    and #$0f                           ; strip to just volume bits
    beq calc_save_and_set_pulse_config ; branch if 0 to set APU config with read volume (modified by SOUND_VOLUME_ADJ2,x)
    sec                                ; volume non-zero, set carry flag in preparation for subtraction
    sbc SOUND_VOLUME_ADJ,x             ; subtract by sound channel adjustment amount
    bcc @low_volume
    beq @low_volume                    ; branch if 0 or underflow to use lowest volume
    bne calc_save_and_set_pulse_config ; calculated volume is non-zero, branch

@low_volume:
    lda #$01

; calculate volume by subtracting SOUND_VOLUME_ADJ2,X
; the minimum volume is 1
; merge calculated volume with SOUND_CFG_HIGH_A,x or SOUND_CFG_HIGH_B,x
; then set APU config register
; input
;  * a - volume before adjusted by SOUND_VOLUME_ADJ2,x
calc_save_and_set_pulse_config:
    sta SOUND_PULSE_VOLUME,x

calc_and_set_pulse_config:
    ora SOUND_PULSE_VOLUME,x ; merge to create volume (config low nibble)
                             ; !(HUH) SOUND_PULSE_VOLUME,x is always in a
                             ; no need to ora with itself
    beq @calc_cfg_set
    sec                      ; set carry flag in preparation for subtraction
    sbc SOUND_VOLUME_ADJ2,x  ; subtract from volume
    bcc @vol_1_calc_cfg_set
    beq @vol_1_calc_cfg_set

; select duty cycle for pulse channel from either SOUND_CFG_HIGH_A or SOUND_CFG_HIGH_B
@calc_cfg_set:
    sta SOUND_VAR_1        ; set volume
    lda VIBRATO_CTRL,x
    and #$20               ; see if using SOUND_CFG_HIGH_B or SOUND_CFG_HIGH_A
    bne @merge_cfg_set     ; branch if bit 5 is set to use SOUND_CFG_HIGH_B
    lda SOUND_VAR_1        ; bit 5 clear, using SOUND_CFG_HIGH_A
                           ; load volume
    ora SOUND_CFG_HIGH_A,x ; merge with high nibble

@write_pulse_config_to_apu:
    jsr ldx_channel_register ; set x to apu channel register [#$00, #$04, #$08, #$c] for sound slot x
    bcs @restore_x_exit      ; branch if there is already a sound playing slot 4, so can't play slot 0
    sta APU_PULSE_CONFIG,x
    .ifdef Superc
        jsr wait             ; execute #$0a nop instructions
    .endif

@restore_x_exit:
    ldx SOUND_CURRENT_SLOT ; load current sound slot [#$00-#$05]
    rts

@vol_1_calc_cfg_set:
    lda #$01          ; set volume 1
    bne @calc_cfg_set ; always branch

@merge_cfg_set:
    lda SOUND_CFG_HIGH_B,x         ; load register config high nibble
    and #$f0
    ora SOUND_VAR_1                ; merge with volume
    jmp @write_pulse_config_to_apu

handle_sound_envelope_cmd_byte:
    cmp #$fe
    beq @sound_envelope_fe
    bcs sound_envelope_ff
    inc SOUND_ENVELOPE_READ_OFFSET,x      ; increment read offset
    lda SOUND_ENVELOPE_READ_OFFSET,x
    sta SOUND_ENVELOPE_BASE_READ_OFFSET,x ; save read offset
    jmp sound_envelope_read

; sound_envelope_xx byte is #$fe
@sound_envelope_fe:
    iny                                   ; increment sound envelope read offset
    inc SOUND_ENVELOPE_READ_LEN,x
    lda (SOUND_VAR_1),y                   ; re-read sound_envelope_xx byte
    cmp SOUND_ENVELOPE_READ_LEN,x
    bne @reset_read_continue
    lda #$00                              ; !(UNUSED) reset
    sta SOUND_ENVELOPE_READ_LEN,x
    sta SOUND_ENVELOPE_BASE_READ_OFFSET,x
    inc SOUND_ENVELOPE_READ_OFFSET,x
    inc SOUND_ENVELOPE_READ_OFFSET,x      ; increment read offset by 2
    jmp sound_envelope_read

@reset_read_continue:
    lda SOUND_ENVELOPE_BASE_READ_OFFSET,x
    sta SOUND_ENVELOPE_READ_OFFSET,x      ; set sound_envelope_xx read offset
    jmp sound_envelope_read

bgm_cmd_01:
    dec SOUND_ENVELOPE_READ_OFFSET,x
    bmi sound_envelope_ff
    dec SOUND_PULSE_VOLUME,x
    bmi @continue
    lda SOUND_PULSE_VOLUME,x
    jmp calc_save_and_set_pulse_config ; !(OBS) could have called calc_and_set_pulse_config
                                       ; and save one sta instruction

@continue:
    inc SOUND_PULSE_VOLUME,x
    rts

bgm_cmd_02:
    lda SOUND_FLAGS,x
    and #$02
    bne bgm_cmd_03

pulse_set_volume:
    lda SOUND_FLAGS,x
    and #$06
    cmp #$06
    beq pulse_set_volume_exit          ; exit if both bits 1 and 2 are clear
    lda SOUND_CMD_LENGTH,x
    cmp DECRESCENDO_END_PAUSE,x
    bcs pulse_set_volume_exit
    lda SOUND_FLAGS,x                  ; resume decrescendo, load the current sound slot's sound flags
    ora #$06                           ; set bits 1 and 2
    sta SOUND_FLAGS,x                  ; save sound flags now specify that decrescendo should resume
    lda SOUND_CFG_HIGH_B,x
    and #$0c                           ; strip to length counter halt, and constant volume bits
    bne save_vol_calc_set_pulse_config ; branch if volume specified by game
    jmp set_pulse_flags_exit           ; !(HUH) unnecessary, next line is set_pulse_flags_exit

set_pulse_flags_exit:
    lda SOUND_CFG_HIGH_B,x
    and #$02
    beq @exit
    lda VIBRATO_CTRL,x
    ora #$20
    sta VIBRATO_CTRL,x     ; set using SOUND_CFG_HIGH_B,x

@exit:
    rts

; sound byte is #$ff
sound_envelope_ff:
    lda SOUND_FLAGS,x
    ora #$04                         ; set bit 2
    and #$fd                         ; strip bit 1
    sta SOUND_FLAGS,x
    lda SOUND_ENVELOPE_READ_OFFSET,x
    ora #$80                         ; see @dec_vol_adj_timer
    sta SOUND_ENVELOPE_READ_OFFSET,x

pulse_set_volume_exit:
    rts

save_vol_calc_set_pulse_config:
    lsr
    lsr
    sta SOUND_VAR_1                    ; save volume
    jsr set_pulse_flags_exit
    lda SOUND_VAR_1                    ; load volume
    jmp calc_save_and_set_pulse_config

bgm_cmd_03:
    lda SOUND_PITCH_FLAGS,x
    bmi @handle_vol_envelope
    sty SOUND_VAR_1          ; set current bgm_cmd
                             ; can be either bgm_cmd_03
                             ; or bgm_cmd_02 (when SOUND_FLAGS,x bit 1 is set)
    lda SOUND_PITCH_FLAGS,x
    and #$0f
    sta SOUND_VAR_2
    lda PITCH_OVERWRITE,x
    sec                      ; set carry flag in preparation for subtraction
    sbc #$01
    and #$03
    sec                      ; set carry flag in preparation for subtraction
    sbc SOUND_VAR_2
    and #$03
    cpx #$01
    bne @continue
    ora #$04                 ; pulse 2 channel use upper 4 overwrites

@continue:
    tay
    lda SOUND_PERIOD_LOW,y
    sta CUR_SOUND_PERIOD_LOW         ; set pulse channel period/timer, or triangle channel linear counter reload value
    lda SOUND_LEN_TIMER_HIGH,y
    sta CUR_SOUND_LEN_TIMER_HIGH
    jsr calc_write_len_period_to_apu ; set length and period APU registers

; bgm_cmd_03 negative SOUND_PITCH_FLAGS,x
@handle_vol_envelope:
    lda SOUND_CFG_HIGH_B,x
    and #$0c
    bne pulse_set_volume_exit
    lda SOUND_CTRL_ENVELOPE_OFFSET,x
    bmi @dec_vol_adj_timer
    lda SOUND_ENVELOPE_READ_OFFSET,x
    bmi @dec_vol_adj_timer
    dec SOUND_VOLUME_ADJ_TIMER,x
    beq @lower_volume
    jmp sound_control_envelope       ; read from and handle sound_envelope_xx

@lower_volume:
    inc SOUND_VOLUME_ADJ,x ; lower volume by increasing
    lda SOUND_VOLUME_ADJ,x
    cmp #$10
    bcc @set_timer
    lda #$0f
    sta SOUND_VOLUME_ADJ,x ; set maximum volume adust to #$0f

@set_timer:
    lda SOUND_DECRESCENDO_STEP_TIME,x
    sta SOUND_VOLUME_ADJ_TIMER,x
    jmp sound_control_envelope

@dec_vol_adj_timer:
    dec SOUND_VOLUME_ADJ_TIMER,x
    bne pulse_set_volume_exit
    lda SOUND_DECRESCENDO_STEP_TIME,x
    sta SOUND_VOLUME_ADJ_TIMER,x
    dec SOUND_PULSE_VOLUME,x
    bmi @inc_pulse_volume             ; branch if SOUND_PULSE_VOLUME,x is negative
    lda #$01                          ; SOUND_PULSE_VOLUME,x is positive (or zero)
    cmp SOUND_PULSE_VOLUME,x
    beq @set_pulse_config             ; branch if SOUND_PULSE_VOLUME,x is #$01
    bcs @inc_pulse_volume             ; SOUND_PULSE_VOLUME,x is positive and less than #$01, i.e. #$00

; SOUND_PULSE_VOLUME,x is greater than or equal to #$01
@set_pulse_config:
    lda SOUND_PULSE_VOLUME,x
    jmp calc_and_set_pulse_config

; SOUND_PULSE_VOLUME,x is zero or negative
@inc_pulse_volume:
    inc SOUND_PULSE_VOLUME,x
    rts

calc_current_pitch:
    lda SOUND_PITCH_CTRL,x            ; load sound_pitch_ctrl_ptr_tbl offset
    asl                               ; double since each entry is a #$02 byte memory address
    tay                               ; transfer to offset register
    lda sound_pitch_ctrl_ptr_tbl,y
    sta SOUND_VAR_1
    lda sound_pitch_ctrl_ptr_tbl+1,y
    sta SOUND_VAR_1+1
    lda SOUND_PITCH_OFFSET,x          ; load sound_pitch_ctrl_xx read offset
    tay                               ; transfer to offset register
    lda (SOUND_VAR_1),y
    cmp #$fb
    bcs sound_pitch_ctrl_control_byte
    cmp #$10
    bcc @calc_pitch
    lsr
    lsr
    lsr
    lsr
    sta SOUND_PITCH_ADJ_TIMER,x

@calc_pitch:
    inc SOUND_PITCH_OFFSET,x  ; increment sound_pitch_ctrl_xx read offset
    lda (SOUND_VAR_1),y
    and #$0f
    sta SOUND_VAR_1
    cmp #$08
    bcs @set_period_linear
    jsr calc_period_adjust    ; SOUND_VAR_1 less than #$08
                              ; calculate a = SOUND_PERIOD_MULT,x * SOUND_VAR_1
    jsr adjust_current_period ; set CUR_SOUND_PERIOD_LOW = a + SOUND_PERIOD,x
                              ; if overflow increment CUR_SOUND_LEN_TIMER_HIGH
    rts

; SOUND_VAR_1 between #$08 and #$0f
@set_period_linear:
    lda #$10
    sec                           ; set carry flag in preparation for subtraction
    sbc SOUND_VAR_1
    sta SOUND_VAR_1               ; SOUND_VAR_1 = SOUND_VAR_1 - #$10
    jsr calc_period_adjust        ; calculate SOUND_VAR_1 = SOUND_PERIOD_MULT,x * SOUND_VAR_1
    jsr sound_set_adj_period_len2 ; set CUR_SOUND_PERIOD_LOW = SOUND_PERIOD,x - SOUND_VAR_1
                                  ; if underflow decrement CUR_SOUND_LEN_TIMER_HIGH
    rts

; SOUND_VAR_1 = SOUND_VAR_1 * SOUND_PERIOD_MULT,x
calc_period_adjust:
    lda SOUND_PERIOD_MULT,x ; load multiplier
    tax
    lda SOUND_VAR_1

; SOUND_VAR_1 * SOUND_PERIOD_MULT,x
@loop:
    dex
    beq @exit
    clc             ; clear carry in preparation for addition
    adc SOUND_VAR_1
    jmp @loop

@exit:
    ldx SOUND_CURRENT_SLOT ; restore x to current sound slot [#$00-#$05]
    sta SOUND_VAR_1
    rts

; set CUR_SOUND_PERIOD_LOW = a + SOUND_PERIOD,x
; if overflow, increment CUR_SOUND_LEN_TIMER_HIGH
; input
;  * a - amount to add to the sound period
adjust_current_period:
    clc                          ; clear carry in preparation for addition
    adc SOUND_PERIOD,x
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    bcc @continue
    lda SOUND_LEN,x              ; overflow occurred
    sta CUR_SOUND_LEN_TIMER_HIGH
    inc CUR_SOUND_LEN_TIMER_HIGH
    rts

@continue:
    lda SOUND_LEN,x
    sta CUR_SOUND_LEN_TIMER_HIGH
    rts

; set CUR_SOUND_PERIOD_LOW = SOUND_PERIOD,x - SOUND_VAR_1
; if underflow decrement CUR_SOUND_LEN_TIMER_HIGH
sound_set_adj_period_len2:
    lda SOUND_PERIOD,x
    sec                          ; set carry flag in preparation for subtraction
    sbc SOUND_VAR_1
    bcs @continue
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    lda SOUND_LEN,x
    sta CUR_SOUND_LEN_TIMER_HIGH
    dec CUR_SOUND_LEN_TIMER_HIGH
    rts

; !(OBS) could have been simplified to not duplicate the the next 3 lines
@continue:
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    lda SOUND_LEN,x
    sta CUR_SOUND_LEN_TIMER_HIGH
    rts

sound_pitch_ctrl_control_byte:
    cmp #$fe
    beq sound_pitch_ctrl_fe
    bcs set_pitch_adj_timer_to_ff
    inc SOUND_PITCH_OFFSET,x             ; increment sound_pitch_ctrl_xx read offset
    lda SOUND_PITCH_OFFSET,x
    sta SOUND_PITCH_CTRL_PARENT_OFFSET,x ; backup sound_pitch_ctrl_xx read offset
    jmp calc_current_pitch

sound_pitch_ctrl_fe:
    iny
    inc SOUND_PITCH_READ_LEN,x
    lda (SOUND_VAR_1),y
    cmp SOUND_PITCH_READ_LEN,x
    bne @handle_pitch_ctrl
    lda #$00
    sta SOUND_PITCH_READ_LEN,x
    sta SOUND_PITCH_CTRL_PARENT_OFFSET,x ; reset sound_pitch_ctrl_xx read offset backup
    inc SOUND_PITCH_OFFSET,x
    inc SOUND_PITCH_OFFSET,x             ; add 2 to sound_pitch_ctrl_xx read offset
    jmp calc_current_pitch

@handle_pitch_ctrl:
    lda SOUND_PITCH_CTRL_PARENT_OFFSET,x ; load backed up sound_pitch_ctrl_xx read offset
    sta SOUND_PITCH_OFFSET,x             ; restore sound_pitch_ctrl_xx read offset
    jmp calc_current_pitch

set_pitch_adj_timer_to_ff:
    lda #$ff
    sta SOUND_PITCH_ADJ_TIMER,x
    rts

; read sound command
; input
;  * a - SOUND_FLAGS,x - bit 0 specifies type of command
;  * x - sound register offset
;  * y - sound_xx_slot_xx read offset
;  * (CUR_SOUND_ADDR) - pointer to sound byte
read_sound_cmd:
    lsr                       ; shift bit 0 of SOUND_FLAGS,x to carry
                              ; #$00 - byte 0 >= #$10 or slot 4, #$01 = otherwise
    bcc read_bgm_cmd          ; branch if background music command
    jmp read_sound_effect_cmd ; read sound bytes for sound effect command

; sound code starts with byte that is greater than or equal to #$30
; reads the sound byte and handles it
; input
;  * (CUR_SOUND_ADDR) - pointer to sound byte
;  * y - sound byte read offset
; output
;  * x - SOUND_CURRENT_SLOT [0-3]
read_bgm_cmd:
    lda SOUND_CURRENT_SLOT   ; load sound slot index
    cmp #$03                 ; compare to sound slot #$03 (noise/dmc channel)
    beq parse_percussion_cmd ; branch if sound slot #$03 (noise/dmc channel)
    lda (CUR_SOUND_ADDR),y   ; not noise channel, load sound byte
    and #$f0                 ; keep high nibble
    cmp #$c0                 ; compare to #$c0
    bcs @regular_sound_cmd   ; branch if high nibble is greater than #$c0 to process the sound command
    jmp simple_sound_cmd     ; simple sound command, just a note and length multiplier

@regular_sound_cmd:
    and #$30                  ; keep bits ..xx .... of high nibble
    lsr
    lsr
    lsr
    tax
    lda sound_cmd_ptr_tbl,x   ; load low byte of address pointer
    sta SOUND_VAR_1           ; set low byte of address pointer
    lda sound_cmd_ptr_tbl+1,x ; load high byte of address pointer
    sta SOUND_VAR_1+1         ; set high byte of address pointer
    ldx SOUND_CURRENT_SLOT    ; load current sound slot [#$00-#$05]
    lda (CUR_SOUND_ADDR),y    ; load sound code byte
    and #$0f                  ; keep low nibble
    jmp (SOUND_VAR_1)         ; jump to sound_cmd_routine_xx

; only for sound slot #$03 (noise and dmc channel)
parse_percussion_cmd:
    lda (CUR_SOUND_ADDR),y           ; load sound code byte
    and #$f0                         ; keep high nibble
    cmp #$f0                         ; see if high nibble is #$f
    bne @continue                    ; branch if high nibble isn't #$f
    lda (CUR_SOUND_ADDR),y           ; high nibble is #$f, load re-load full sound byte
    and #$0f                         ; keep low nibble
    cmp #$0b
    bcs @sound_cmd_routine_03        ; branch if control command to move to next (child or parent) sound command
    jmp calc_cmd_len_play_percussion ; high nibble isn't #$0b, #$0c, #$0d, #$0e, nor #$0f
                                     ; play percussive sound (dpcm sample)

@sound_cmd_routine_03:
    jmp sound_cmd_routine_03 ; high nibble >= #$0b, go to sound_cmd_routine_03 with low nibble
                             ; either #$0b, #$0c, #$0d, #$0e, or #0$f
                             ; moves to next (child or parent) sound command
                             ; or finished with entire sound command and re-initialize channel

; high nibble isn't #$f
@continue:
    cmp #$e0
    beq control_nibble_e
    cmp #$d0
    beq @control_nibble_d            ; branch if sound command high nibble is #$d to determine SOUND_LENGTH_MULTIPLIER
                                     ; and then loop to actually play percussion sound
    jmp calc_cmd_len_play_percussion

; high nibble is #$d (delay command)
; load low nibble and set it as SOUND_LENGTH_MULTIPLIER before looping to actually
; play the percussion sound sample
@control_nibble_d:
    lda (CUR_SOUND_ADDR),y        ; read slot #$03 sound command byte
    and #$0f                      ; keep low nibble
    sta SOUND_LENGTH_MULTIPLIER,x ; set sound length multiplier
    lda SOUND_FLAGS,x             ; load the current sound slot's sound flags
    and #$7f                      ; strip bit 7 (has sweep flag)
    sta SOUND_FLAGS,x             ; set new sound flags for sound slot (disable sweep)
    iny                           ; increment sound_xx read offset
    jmp parse_percussion_cmd      ; recursively loop to read next byte of percussion command

play_percussive_sound:
    lda (CUR_SOUND_ADDR),y ; load sound_xx byte
    lsr
    lsr
    lsr
    lsr                    ; move high nibble to low nibble
    cmp #$0f               ; see if high nibble was #$0f
    bne @continue
    lda #$0f               ; high nibble #$0f, set apu status and exit
    sta APU_STATUS         ; disable DMC (D). enabled noise (N), triangle (T), and pulse channels (2/1)
    rts

@continue:
    cmp #$0c       ; see if high nibble was #$0c
    bne @continue2
    rts            ; exit if high nibble was #$0c

@continue2:
    tax                                 ; transfer percussion_tbl offset to x
    lda SOUND_FLAGS+3                   ; load sound slot 3's flags
    bmi calc_dpcm_play                  ; branch if sweep flag enabled
    lda percussion_tbl,x                ; load sound code based on high nibble from sound_xx
    ldx SOUND_CURRENT_SLOT              ; restore sound slot offset, play percussion sound
    cmp #$37                            ; see if dmc sample sound
    bcs play_dpcm_sample                ; branch if dmc sample percussive sound, i.e. #$37, #$38, #$39, #$3a, or #$3b
    .ifdef Probotector
        jmp init_sound_code_vars        ; not a dmc sample, play sound (in practice this is always a noise channel sound)
    .else
        jmp load_sound_banks_init_sound ; not a dmc sample, play sound (in practice this is always a noise channel sound)
    .endif

; sound slot #$03 (percussion command) high nibble is #$e
control_nibble_e:
    lda SOUND_FLAGS,x      ; load the current sound slot's sound flags
    ora #$80               ; enable sweep flag
    sta SOUND_FLAGS,x      ; set new sound flags for sound slot (enable sweep)
    lda (CUR_SOUND_ADDR),y ; re-load full load sound_xx byte
    and #$0f               ; load low nibble
    cmp #$04               ; see if #$04
    bne @continue          ; branch if not #$04
    lda #$03
    sta SOUND_DPCM_SAMPLE

@adv_percussion_cmd:
    iny
    jmp parse_percussion_cmd

@continue:
    cmp #$03
    bne @set_base_offset    ; value is not #$03 nor #$04
    lda #$0f
    sta SOUND_DPCM_SAMPLE
    bne @adv_percussion_cmd

@set_base_offset:
    lda #$1b                ; sample #$1b (dpcm_sample_0c)
    sta SOUND_DPCM_SAMPLE   ; start with offset at dpcm_sample_0c
    bne @adv_percussion_cmd ; always branch

; play DPCM sample SOUND_DPCM_SAMPLE + x
; input
;  * x - base dpcm_samples offset
calc_dpcm_play:
    txa
    clc                    ; clear carry in preparation for addition
    adc SOUND_DPCM_SAMPLE
    bne play_dpcm_sample_a

play_dpcm_sample:
    sec                 ; set carry flag in preparation for subtraction
    sbc #$35            ; sound codes larger than #$35 are commands to initialize the DMC
                        ; subtract #$35 to get actual initialization values (offset to dpcm_sample_data_tbl)
    sta INIT_SOUND_CODE ; set dpcm_sample_data_tbl offset
    lda INIT_SOUND_CODE ; !(HUH) not needed, already in register
                        ; INIT_SOUND_CODE is also overwritten later init_sound_code_vars

play_dpcm_sample_a:
    asl
    asl                     ; quadruple since each entry in dpcm_samples is 4 bytes
    tax                     ; transfer to offset register
    lda dpcm_samples+1,x    ; !(UNUSED) immediately overwritten
    lda #$0f
    sta APU_STATUS          ; reset DMC (silence it)
    lda dpcm_samples,x      ; load DMC configuration (max sampling rate, no looping)
    sta APU_DMC             ; set DMC configuration (max sampling rate, no looping)
    lda dpcm_samples+1,x    ; load direct load write value (always #$00)
    sta APU_DMC_COUNTER     ; set direct load write value (initial value)
    lda dpcm_samples+2,x    ; load address of DPCM sample data (this value * #$40) + $c000 is final address
    sta APU_DMC_SAMPLE_ADDR ; set address of DPCM sample data
    lda dpcm_samples+3,x    ; load length of sample
    sta APU_DMC_SAMPLE_LEN  ; set length of sample
    lda #$1f                ; a = #$1f
    sta APU_STATUS          ; enable DMC, noise, triangle, and the 2 pulse channels
                            ; i.e. start DMC playback
    ldx #$03
    rts

; noise and dmc sound codes related to percussive music
; usually drum hit sounds
;  * sound_01 - percussive tick (noise)
;  * sound_02 - percussive tick (noise)
;  * sound_03 - percussive tick (noise)
;  * sound_04 - percussive tick (noise)
;  * sound_05 - percussive tick (noise)
;  * sound_37 - dmc sample
;  * sound_38 - dmc sample
;  * sound_39 - dmc sample
;  * sound_3a - dmc sample
;  * sound_3b - dmc sample
percussion_tbl:
    .byte $01,$02,$03,$04,$05,$3a,$3a,$37,$38,$39,$3a,$3b

; one of two modes (along with read_bgm_cmd) of parsing sound commands.
; This mode supports pulse, noise, and triangle channels, but is used for pulse
; and noise channels only in the game.
; input
;  * x - sound register offset
;  * y - sound_xx_slot_xx read offset
;  * (CUR_SOUND_ADDR) - pointer to sound byte
read_sound_effect_cmd:
    lda (CUR_SOUND_ADDR),y          ; read byte 0 of sound part command
    cmp #$e6                        ; see if need to determine if should run sound_cmd_routine_03
    bcs @check_sound_cmd_routine_03 ; branch if greater than or equal to #$e6
                                    ; to see if >= #$fb to run sound_cmd_routine_03
    jmp @continue

; run sound_cmd_routine_03 when byte >= #$fb
; overly complicated because @check_noise and @restore_cmd_byte just end up
; always going to @continue with the same a register value
@check_sound_cmd_routine_03:
    cmp #$f0
    bcc @check_noise         ; branch to continue if shouldn't run sound_cmd_routine_03
    and #$0f                 ; sound byte >= #$f0, strip high nibble
    cmp #$0b
    bcc @restore_cmd_byte    ; branch if low nibble less than #$0b to restore full byte and continue
    jmp sound_cmd_routine_03 ; sound command byte >= #$fb

; previous section stripped high nibble, but command was < #$fb,
; restore full command byte to continue
@restore_cmd_byte:
    lda (CUR_SOUND_ADDR),y
    bne @continue

; unneeded check for noise branch
@check_noise:
    cpx #$05      ; see if sound slot is for noise channel
    beq @continue ; !(HUH) useless branch, just continues execution

; sound effect command byte is 0 < #$fb
@continue:
    and #$f0                       ; strip to just high nibble
    cmp #$00
    bne sound_effect_set_vol_pitch ; branch if high nibble non-zero
    lda (CUR_SOUND_ADDR),y         ; re-load byte 0 of sound effect command
    and #$0f                       ; strip to SOUND_LENGTH_MULTIPLIER,x portion
    bne @set_length_config         ; branch if SOUND_LENGTH_MULTIPLIER,x portion is non-zero (and high nibble zero)
    jmp sound_effect_set_vol_pitch ; byte 0 is all zero

; high nibble zero, low nibble non-zero
@set_length_config:
    sta SOUND_LENGTH_MULTIPLIER,x ; set multiplier (low nibble of byte 0)
    lda SOUND_FLAGS,x             ; re-load byte 0 of sound effect command
    and #$f9                      ; strip bits 1 and 2
    sta SOUND_FLAGS,x             ; update sound flags
    iny                           ; advance sound code part read offset
    lda (CUR_SOUND_ADDR),y        ; load byte 1 of sound effect command
    cpx #$02                      ; see if sound slot is for triangle channel
    beq @set_triangle_config      ; branch if triangle channel to set config and exit
                                  ; !(OBS) shouldn't ever happen since no triangle sound effect commands
                                  ; only pulse and noise channels
    cpx #$05                      ; see if sound slot is for noise channel
    bne @set_pulse_cfg_sweep      ; branch if slot is for a pulse channel
    lda #$30                      ; noise channel, halt length counter and set constant volume
    sta SOUND_CFG_HIGH_A,x        ; set the length counter halt (L) and constant volume flags (C)
                                  ; see APU_NOISE_CONFIG ($400c)
    jmp @check_length_vol_adv_cmd

@set_pulse_cfg_sweep:
    sta SOUND_CFG_HIGH_A,x   ; set apu channel config register memory variable ($4000,x)
    iny                      ; advance sound code part read offset
    lda (CUR_SOUND_ADDR),y   ; read sound code command byte
    cmp #$88
    beq @pulse_disable_sweep ; branch if byte 3 is #$88 to disable sweep before setting sweep
    lda SOUND_FLAGS,x
    ora #$80
    sta SOUND_FLAGS,x        ; set bit 7
    lda (CUR_SOUND_ADDR),y   ; load sweep value

; input
;  * a - APU_REGISTER_BASE+1,x
;  * x - sound slot
@check_slot_pulse_set_sweep:
    cpx #$04                   ; see if sound slot 4 (pulse 1)
    bne @pulse_set_sweep
    .ifdef Superc
        sta SOUND_VAR_UNUSED+3 ; !(UNUSED) only ever written to
    .endif

@pulse_set_sweep:
    jsr ldx_channel_register       ; set x to apu channel register [#$00, #$04, #$08, #$c] for sound slot x
    bcs @restore_slot_adv_cmd_part ; branch if sound with higher priority than slot 0 playing in slot 4
    sta APU_REGISTER_BASE+1,x
    .ifdef Superc
        jsr wait                   ; execute #$0a nop instructions
    .endif

@restore_slot_adv_cmd_part:
    ldx SOUND_CURRENT_SLOT ; load current sound slot [#$00-#$05]
    iny                    ; advance sound code part read offset

@read_sound_effect_cmd:
    jmp read_sound_effect_cmd

; sound effect command byte 2 is #$88
@pulse_disable_sweep:
    lda SOUND_FLAGS,x
    and #$7f                        ; strip bit 7
    sta SOUND_FLAGS,x
    lda #$7f                        ; set sweep value
    jmp @check_slot_pulse_set_sweep

; input
;  * a - triangle channel linear counter reload value (fine grain duration control)
;  * y - sound_xx read offset
@set_triangle_config:
    sta APU_TRIANGLE_CONFIG   ; set new linear counter reload value (fine grain duration)
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif
    iny                       ; advance sound command part read offset
    jmp read_sound_effect_cmd

; move to next sound effect command
; supports noise and pulse, but only used for noise slot sound effect commands
; when length counter halt or constant volume, then
; input
;  * a - the length counter halt (L) and constant volume flags (C)
;    always #$30 (infinite length, constant volume)
;  * y - sound_xx read offset
@check_length_vol_adv_cmd:
    cmp #$00                   ; see if channel has length and using APU envelope
    bne @read_sound_effect_cmd ; branch if constant volume and/or constant volume
    iny                        ; advance sound command part read offset
    jmp read_sound_effect_cmd

; sound effect command byte 0 high nibble is non-zero
; or entirety of byte 0 #$00
; SOUND_LENGTH_MULTIPLIER,x has been set, now apply it
;  * x - sound slot
;  * y - sound_xx read offset
sound_effect_set_vol_pitch:
    cpx #$05               ; see if sound slot is for noise channel
    bne @continue          ; branch if not slot for noise channel
    lda (CUR_SOUND_ADDR),y ; noise channel, re-load byte 0 of sound effect command
    cmp #$10
    bne @continue
    iny

@continue:
    lda SOUND_LENGTH_MULTIPLIER,x
    sta SOUND_CMD_LENGTH,x
    lda (CUR_SOUND_ADDR),y
    cpx #$02                        ; compare to sound slot #$02 (triangle channel)
    beq sound_effect_read_pitch_adv ; !(OBS) shouldn't ever happen since no triangle sound effect commands
                                    ; only pulse and noise channels
    lsr
    lsr
    lsr
    lsr                             ; checking if should use constant volume (noise channel or pulse with C set)
    sta SOUND_EFFECT_VOLUME,x       ; set constant volume
    cpx #$05                        ; see if sound slot is for noise channel
    beq @set_volume                 ; branch if noise channel to set volume
    lda SOUND_CFG_HIGH_A,x          ; pulse channel, load current length halt bit and volume constant bit
    and #$10                        ; check the constant volume bit (C)
    beq volume_envelope             ; branch if using volume envelope (not using constant volume)
    lda SOUND_EFFECT_VOLUME,x       ; load constant volume

@set_volume:
    ora SOUND_CFG_HIGH_A,x

; set length halt bit, constant volume bit, and (if specified) the constant volume
set_config:
    jsr ldx_channel_register        ; set x to apu channel register [#$00, #$04, #$08, #$c] for sound slot x
    bcs sound_effect_read_pitch_adv ; branch if there is already a sound playing slot 4, so can't play slot 0
    sta APU_PULSE_CONFIG,x          ; set length halt, constant volume bit, and (if specified) constant volume
                                    ; duty is set elsewhere
    .ifdef Superc
        jsr wait                    ; execute #$0a nop instructions
    .endif

sound_effect_read_pitch_adv:
    ldx SOUND_CURRENT_SLOT       ; restore current sound slot [#$00-#$05]
    lda (CUR_SOUND_ADDR),y       ; reload full sound byte
    and #$0f                     ; strip to low nibble
    cpx #$05                     ; see if sound slot is for noise channel
    beq @set_pitch_adv           ; branch if noise channel
    sta CUR_SOUND_LEN_TIMER_HIGH ; non-noise channel
    lda (CUR_SOUND_ADDR),y       ; reload full sound byte
    beq @set_pitch_adv
    iny
    lda (CUR_SOUND_ADDR),y

@set_pitch_adv:
    sta CUR_SOUND_PERIOD_LOW               ; set pulse channel period/timer, or triangle channel linear counter reload value
    .ifdef Probotector
        cpx #$05
        beq @write_slot_len_period_adv_cmd
        jsr probotector_calc_period_timer
    .endif

@write_slot_len_period_adv_cmd:
    jsr write_slot_len_period_to_apu
    jmp adv_sound_cmd_addr

volume_envelope:
    lda SOUND_CFG_HIGH_A,x
    jmp set_config

.ifdef Probotector
    probotector_calc_period_timer:
        lda #$00
        sta SOUND_VAR_1
        sta SOUND_VAR_3
        lda CUR_SOUND_PERIOD_LOW
        sta SOUND_VAR_1+1
        sta SOUND_CODE_ADDR
        lda CUR_SOUND_LEN_TIMER_HIGH
        sta SOUND_VAR_2
        sta SOUND_CODE_ADDR+1
        ldx #$04
        jsr @calc_period_timer
        ldx #$04
        jsr @calc_period_timer
        ldx SOUND_CURRENT_SLOT
        lda SOUND_VAR_3
        clc
        adc #$80
        lda SOUND_CODE_ADDR
        adc #$00
        sta CUR_SOUND_PERIOD_LOW
        lda SOUND_CODE_ADDR+1
        adc #$00
        sta CUR_SOUND_LEN_TIMER_HIGH
        rts

    @calc_period_timer:
        lsr SOUND_VAR_2
        ror SOUND_VAR_1+1
        ror SOUND_VAR_1
        dex
        bne @calc_period_timer
        lda SOUND_VAR_3
        sec
        sbc SOUND_VAR_1
        sta SOUND_VAR_3
        lda SOUND_CODE_ADDR
        sbc #$00
        sec
        sbc SOUND_VAR_1+1
        sta SOUND_CODE_ADDR
        lda SOUND_CODE_ADDR+1
        sbc SOUND_VAR_2
        sta SOUND_CODE_ADDR+1
        rts
.endif

triangle_set_ctrl_pitch:
    lda (CUR_SOUND_ADDR),y
    and #$0f
    tax
    inx
    lda #$00
    sta SOUND_EFFECT_VOLUME_CONTINUED
    lda SOUND_TRIANGLE_CONFIG
    sta SOUND_VAR_1
    beq @sustain
    cmp #$80
    bcs @disable_control_flag
    inc SOUND_VAR_1
    lda SOUND_VAR_1

@loop:
    dex
    beq @break_loop
    clc             ; clear carry in preparation for addition
    adc SOUND_VAR_1
    bcs @sustain
    jmp @loop

@break_loop:
    cmp #$80
    bcc @set_triangle_cfg_pitch

@sustain:
    lda #$90 ; set linear counter to auto-reload, disable length counter
             ; causes a sustained pitch

@set_triangle_cfg_pitch:
    sta APU_TRIANGLE_CONFIG            ; set new linear counter reload value (fine grain duration)
    .ifdef Superc
        jsr wait                       ; execute #$0a nop instructions
    .endif
    sta SOUND_TIMER_ADJ+3              ; set linear counter reload value
                                       ; (fine grain duration before silencing channel)
    ldx SOUND_CURRENT_SLOT             ; load current sound slot [#$00-#$05]
    lda VIBRATO_CTRL,x
    bpl @handle_write_len_pitch_to_apu
    rts

@handle_write_len_pitch_to_apu:
    jmp handle_write_len_pitch_to_apu

; enable linear counter, and length counter
; cause pitch to increase
@disable_control_flag:
    sec                         ; set carry flag in preparation for subtraction
    sbc #$80
    jmp @set_triangle_cfg_pitch

reset_envelope_cfg_pitch:
    lda #$01
    sta SOUND_VOLUME_CHANGE_DELAY,x
    lda SOUND_EFFECT_VOLUME,x
    jsr calc_save_and_set_pulse_config
    lda SOUND_CTRL_ENVELOPE_OFFSET,x
    and #$0f
    sta SOUND_ENVELOPE_READ_OFFSET,x
    lda SOUND_FLAGS,x
    and #$bb
    ora #$02
    sta SOUND_FLAGS,x
    lda VIBRATO_CTRL,x
    and #$df
    sta VIBRATO_CTRL,x                 ; set using SOUND_CFG_HIGH_A,x
    jmp handle_write_len_pitch_to_apu

calc_pulse_cfg_src:
    lda VIBRATO_CTRL,x
    and #$df
    sta VIBRATO_CTRL,x     ; set using SOUND_CFG_HIGH_A,x
    lda SOUND_CFG_HIGH_B,x
    lsr
    bcc @exit
    lda VIBRATO_CTRL,x
    ora #$20
    sta VIBRATO_CTRL,x     ; set using SOUND_CFG_HIGH_B,x

@exit:
    rts

; use low octave values
; SOUND_NOTE_PERIOD_OFFSET,y is negative
; input
;  * a - SOUND_NOTE_PERIOD_OFFSET,y
;  * x - note_period_tbl offset
set_in_memory_low_octave_period:
    and #$7f                             ; strip bit 7
    sta SOUND_VAR_2
    txa
    sec                                  ; set carry flag in preparation for subtraction
    sbc SOUND_VAR_2
    bcs calc_write_len_period_adv_a      ; branch if using normal pitches (no underflow)
    sec                                  ; underflow use lower pitches
                                         ; set carry flag in preparation for subtraction
    sbc #$e8
    tax
    lda note_period_tbl_2,x
    sta CUR_SOUND_PERIOD_LOW             ; set pulse channel period/timer, or triangle channel linear counter reload value
    lda note_period_tbl_2+1,x
    sta CUR_SOUND_LEN_TIMER_HIGH
    jmp calc_octave_write_len_period_adv

simple_sound_cmd:
    jsr calc_cmd_len_play_percussion
    cpx #$02                         ; compare to sound slot #$02 (triangle channel)
    bne @continue
    jmp triangle_set_ctrl_pitch

@continue:
    jsr calc_pulse_cfg_src
    lda SOUND_CTRL_ENVELOPE_OFFSET,x
    bmi reset_envelope_cfg_pitch
    lda #$00
    sta SOUND_VOLUME_ADJ,x
    sta SOUND_ENVELOPE_READ_OFFSET,x
    sta SOUND_ENVELOPE_BASE_READ_OFFSET,x
    sta SOUND_ENVELOPE_READ_LEN,x
    sta SOUND_VOLUME_CHANGE_DELAY,x
    inc SOUND_VOLUME_CHANGE_DELAY,x
    sty SOUND_VAR_2                       ; backup y
    jsr sound_control_envelope
    ldy SOUND_VAR_2                       ; restore y
    lda SOUND_FLAGS,x
    and #$b9
    sta SOUND_FLAGS,x
    lda VIBRATO_CTRL,x
    and #$df
    sta VIBRATO_CTRL,x                    ; set using SOUND_CFG_HIGH_A,x

handle_write_len_pitch_to_apu:
    lda (CUR_SOUND_ADDR),y
    and #$f0                            ; high nibble is note_period_tbl offset, e.g. 0010 0000 -> offset 4
    lsr
    lsr
    lsr                                 ; shift right 3 times since each entry is a 2 byte address
    tax                                 ; transfer note_period_tbl offset to x
    tya
    pha                                 ; backup y
    ldy SOUND_CURRENT_SLOT
    lda SOUND_NOTE_PERIOD_OFFSET,y
    beq calc_write_len_period_adv       ; branch if no adjustment from note_period_tbl offset
    bmi set_in_memory_low_octave_period
    txa                                 ; restore note_period_tbl offset
    clc                                 ; clear carry in preparation for addition
    adc SOUND_NOTE_PERIOD_OFFSET,y      ; add to note_period_tbl read offset

calc_write_len_period_adv_a:
    tax

calc_write_len_period_adv:
    lda note_period_tbl,x
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    lda note_period_tbl+1,x
    sta CUR_SOUND_LEN_TIMER_HIGH

calc_octave_write_len_period_adv:
    pla
    tay                      ; restore y
    ldx SOUND_CURRENT_SLOT   ; load current sound slot [#$00-#$05]
    lda SOUND_OCTAVE_SHIFT,x
    tax

; half timer, going up one octave per loop
@loop:
    cpx #$05                     ; see if sound slot is for noise channel
    beq @continue
    lsr CUR_SOUND_LEN_TIMER_HIGH
    ror CUR_SOUND_PERIOD_LOW
    inx
    bne @loop                    ; always branch

@continue:
    ldx SOUND_CURRENT_SLOT               ; load current sound slot [#$00-#$05]
    lda CUR_SOUND_PERIOD_LOW             ; load pulse channel period/timer, or triangle channel linear counter reload value
    sta SOUND_PERIOD,x
    lda CUR_SOUND_LEN_TIMER_HIGH
    sta SOUND_LEN,x
    cpx #$02                             ; compare to sound slot #$02 (triangle channel)
    beq @write_len_period_adv
    lda SOUND_PITCH_FLAGS,x
    bmi @check_ctrl_write_len_period_adv
    jsr overwrite_pitches

@check_ctrl_write_len_period_adv:
    lda VIBRATO_CTRL,x
    and #$10
    beq @write_len_period_adv
    lda SOUND_PITCH_ADJ_PERIOD,x
    bne @write_len_period_adv
    tya
    pha
    jsr calc_current_pitch
    pla
    tay

@write_len_period_adv:
    jsr write_slot_len_period_to_apu
    jmp adv_sound_cmd_addr

write_slot_len_period_to_apu:
    cpx #$05                         ; see if sound slot is for noise channel
    beq write_len_period_to_apu      ; branch if slot #$05 to set the length, and timer/period
    jsr adjust_pitch                 ; not noise channel, adjust pitch by SOUND_TIMER_ADJ,x
    cpx #$02                         ; compare to sound slot #$02 (triangle channel)
    beq write_len_period_to_apu      ; branch if slot #$02 to set the length, and timer/period
    lda VIBRATO_CTRL,x               ; pulse channel
    and #$10
    beq calc_write_len_period_to_apu
    lda SOUND_PITCH_ADJ_TIMER,x
    cmp #$ff
    bne calc_write_len_period_to_apu
    rts

; input
;  * x - sound slot
calc_write_len_period_to_apu:
    lda CUR_SOUND_LEN_TIMER_HIGH    ; load sound pulse length
    cmp SOUND_LEN_TIMERS_HIGH,x     ; see if a new period was set
    bne write_len_period_to_apu_a
    lda SOUND_FLAGS,x               ; period hasn't changed
    and #$81
    cmp #$81
    beq write_len_period_to_apu     ; branch if #$81 to skip constant volume check
    lda SOUND_CFG_HIGH_A,x
    and #$10                        ; see if constant volume
    bne set_low_period_write_to_apu ; branch to save low period and then write that to APU

; set the length, and timer/period
write_len_period_to_apu:
    lda CUR_SOUND_LEN_TIMER_HIGH ; for noise channel, this value is written to APU
                                 ; but ignored

; sets the length counter, and, timer/period for pulse, noise (slot #$05), and triangle channels
; input
;  * a - length counter load and timer high bites (always CUR_SOUND_LEN_TIMER_HIGH)
;  * x - sound slot
write_len_period_to_apu_a:
    sta SOUND_LEN_TIMERS_HIGH,x     ; backup the current slot's set the length counter load and timer high bits
                                    ; when sound slot is #$04 or #$05, actually writing to SOUND_LEN_TIMERS_HIGH_CONTINUED
                                    ; does not set any high timer portion
    ora #$08                        ; set low bit of L (length counter) to ensure any duration is specified
    jsr ldx_channel_register        ; set x to apu channel register [#$00, #$04, #$08, #$c] for sound slot x
    bcs set_low_period_write_to_apu ; branch if there is already a sound playing slot 4, so can't play slot 0
    sta APU_REGISTER_BASE+3,x       ; set length counter and timer high value [#$03, #$07, #$0b, #$0f]
    .ifdef Superc
        jsr wait                    ; execute #$0a nop instructions
    .endif

set_low_period_write_to_apu:
    lda CUR_SOUND_PERIOD_LOW     ; load pulse channel period/timer, or triangle channel linear counter reload value
    ldx SOUND_CURRENT_SLOT       ; load current sound slot [#$00-#$05]
    cpx #$02                     ; compare to sound slot #$02 (triangle channel)
    bcs @continue                ; branch if noise or pulse channel to set duty
    sta SOUND_PULSE_PERIOD_LOW,x ; pulse channel 1 or 2, update period/timer low byte

; sets the pulse, noise, or triangle timer/period
@continue:
    jsr ldx_channel_register  ; set x to apu channel register [#$00, #$04, #$08, #$c] for sound slot x
    bcs @exit                 ; branch if there is already a sound playing slot 4, so can't play slot 0
    sta APU_REGISTER_BASE+2,x ; set the pulse, noise, or triangle timer/period
    .ifdef Superc
        jsr wait              ; execute #$0a nop instructions
    .endif

@exit:
    ldx SOUND_CURRENT_SLOT ; load current sound slot [#$00-#$05]
    rts

; when SOUND_PITCH_FLAGS,x bit 6 set, sets one of 3 sets of 3 slots based on PITCH_OVERWRITE,x
;  * 0, 1, and 2
;  * 1, 2, and 3
;  * 2, 3, and 4
;  * 3, 4, and 5
; otherwise, SOUND_PITCH_FLAGS,x is incremented
overwrite_pitches:
    sty SOUND_VAR_1       ; backup y
    lda PITCH_OVERWRITE,x
    cpx #$01              ; see if pulse 2
    bne @continue         ; branch if slot 0 (pulse 1)
    ora #$04              ; pulse slot 2
                          ; set bit 2 to use slot 4 (higher priority pulse 1 slot)

@continue:
    tay
    lda CUR_SOUND_LEN_TIMER_HIGH
    sta SOUND_LEN_TIMER_HIGH,y
    lda CUR_SOUND_PERIOD_LOW     ; load pulse channel period/timer, or triangle channel linear counter reload value
    sta SOUND_PERIOD_LOW,y
    lda SOUND_PITCH_FLAGS,x
    and #$40
    beq @inc_exit                ; brach if bit 6 clear
    lda SOUND_PITCH_FLAGS,x
    and #$bf
    sta SOUND_PITCH_FLAGS,x      ; strip bit 6
    lda #$03
    sta SOUND_VAR_2

@loop:
    iny
    lda CUR_SOUND_LEN_TIMER_HIGH
    sta SOUND_LEN_TIMER_HIGH,y
    lda CUR_SOUND_PERIOD_LOW     ; load pulse channel period/timer, or triangle channel linear counter reload value
    sta SOUND_PERIOD_LOW,y
    dec SOUND_VAR_2
    bne @loop

@inc_exit:
    ldy SOUND_VAR_1       ; restore y
    inc PITCH_OVERWRITE,x
    lda PITCH_OVERWRITE,x
    and #$03
    sta PITCH_OVERWRITE,x
    rts

; calculates pulse or triangle pitch from SOUND_TIMER_ADJ,x
adjust_pitch:
    lda SOUND_TIMER_ADJ,x
    sta SOUND_VAR_1
    beq @exit
    bpl @increase_pitch
    and #$7f                     ; make positive
    sta SOUND_VAR_1              ; set timer adjust amount
    lda CUR_SOUND_PERIOD_LOW     ; load pulse channel period/timer, or triangle channel linear counter reload value
    clc                          ; clear carry in preparation for addition
    adc SOUND_VAR_1              ; increase the period (lower pitch)
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    bcc @exit
    inc CUR_SOUND_LEN_TIMER_HIGH ; overflow, increase timer high value by 1

@exit:
    rts

@increase_pitch:
    lda CUR_SOUND_PERIOD_LOW     ; load pulse channel period/timer, or triangle channel linear counter reload value
    sec                          ; set carry flag in preparation for subtraction
    sbc SOUND_VAR_1
    sta CUR_SOUND_PERIOD_LOW     ; set pulse channel period/timer, or triangle channel linear counter reload value
    bcs @exit2
    dec CUR_SOUND_LEN_TIMER_HIGH ; underflow

@exit2:
    rts

; called from calc_cmd_len_play_percussion when sound slot is #$03 (noise/dmc channel)
; advance sound_xx read offset, and plays dpcm sample based on low nibble
adv_sound_play_percussive:
    jsr adv_sound_cmd_addr    ; advance the sound_xx read offset (SOUND_CMD_LOW_ADDR) to current read location + 1
    dey                       ; decrement sound_xx read offset (it was incremented by adv_sound_cmd_addr)
    jmp play_percussive_sound ; play appropriate intro sound code based on next sound_xx high nibble

; load sound_xx byte and calculate new SOUND_CMD_LENGTH and DECRESCENDO_END_PAUSE
; for simple_sound_cmd, high nibble is less than #$c
; for read_bgm_cmd sound slot #$03, also play a percussion sound sample (adv_sound_play_percussive)
; input
;  * x - sound slot index [0-3]
calc_cmd_len_play_percussion:
    lda (CUR_SOUND_ADDR),y ; load sound byte
    and #$0f               ; keep low nibble

; input
;  * a - amount (+1) multiplied to SOUND_LENGTH_MULTIPLIER,x to get total delay
;        ex: a = #$03, SOUND_LENGTH_MULTIPLIER,x = #$09 => #$24 == #$04 * #$09
calc_cmd_delay:
    sta SOUND_VAR_1               ; set amount of times to add SOUND_LENGTH_MULTIPLIER to itself
    beq @skip_loop                ; don't loop if multiplier is #$00
    lda SOUND_LENGTH_MULTIPLIER,x

@calc_delay_loop:
    clc                           ; clear carry in preparation for addition
    adc SOUND_LENGTH_MULTIPLIER,x ; add SOUND_LENGTH_MULTIPLIER to itself
    dec SOUND_VAR_1               ; decrement loop counter
    bne @calc_delay_loop          ; loop if not finished adding to itself
    beq @loop_complete            ; break loop if added SOUND_LENGTH_MULTIPLIER SOUND_VAR_1 times

@skip_loop:
    lda SOUND_LENGTH_MULTIPLIER,x ; load new loop counter, i.e. SOUND_LENGTH_MULTIPLIER,x * SOUND_VAR_1

@loop_complete:
    sta SOUND_CMD_LENGTH,x        ; set new SOUND_LENGTH_MULTIPLIER value in SOUND_CMD_LENGTH
    .ifdef Probotector
        lda SOUND_PROBO_2,x
        beq @adj_pitch
        lda #$00
        sta SOUND_PROBO_2,x
        dec SOUND_CMD_LENGTH,x
        bne @adj_pitch
        cpx #$03
        bne @restore_set_cmd_addr
        jsr play_percussive_sound
        jmp @set_cmd_addr

    @restore_set_cmd_addr:
        pla
        pla

    @set_cmd_addr:
        jsr adv_sound_cmd_addr
        ldy #$00
        lda SOUND_CMD_ADDRS_LOW,x
        sta CUR_SOUND_ADDR
        lda SOUND_CMD_ADDRS_HIGH,x
        sta CUR_SOUND_ADDR+1
        jmp read_bgm_cmd
    .endif

@adj_pitch:
    cpx #$02                             ; compare to sound slot #$02 (triangle channel)
    bcs @continue                        ; branch if slot #$02 (triangle), #$03 (noise), #$04 (pulse 1), or #$05 (noise)
    lda VIBRATO_CTRL,x                   ; load whether or not to use vibrato (0 = yes, #$80 = no)
    and #$10
    beq @continue
    lda SOUND_PITCH_ADJ_PERIOD,x
    sta SOUND_PITCH_ADJ_TIMER,x
    inc SOUND_PITCH_ADJ_TIMER,x
    lda #$00
    sta SOUND_PITCH_OFFSET,x             ; initialize sound_pitch_ctrl_xx read offset
    sta SOUND_PITCH_READ_LEN,x
    sta SOUND_PITCH_CTRL_PARENT_OFFSET,x ; reset sound_pitch_ctrl_xx read offset backup

@continue:
    cpx #$02                        ; compare to sound slot #$02 (triangle channel)
    beq @exit                       ; exit if sound slot #$02 (triangle channel)
    cpx #$03                        ; compare to sound slot #$03 (noise/dmc channel)
    beq adv_sound_play_percussive   ; play dmc sample if sound slot #$03 (noise/dmc channel)
    lda DECRESCENDO_PERCENT,x       ; load sound byte low nibble
    jsr @calc_decrescendo_pause_end ; (DECRESCENDO_PERCENT,x * SOUND_CMD_LENGTH,x) / 16
    sta DECRESCENDO_END_PAUSE,x     ; set result in DECRESCENDO_END_PAUSE,x
    lda #$01
    sta SOUND_VOLUME_ADJ_TIMER,x

@exit:
    rts

; output
;  * SOUND_VAR_3 - new DECRESCENDO_END_PAUSE,x value
@calc_decrescendo_pause_end:
    and #$0f        ; should already only be the low nibble
    sta SOUND_VAR_1
    lda #$00
    sta SOUND_VAR_2
    sta SOUND_VAR_3

; calculate a = SOUND_VAR_1 * SOUND_CMD_LENGTH,x
@loop:
    dec SOUND_VAR_1
    bmi @shift
    clc
    lda SOUND_CMD_LENGTH,x
    adc SOUND_VAR_2
    sta SOUND_VAR_2
    bcc @loop
    inc SOUND_VAR_3        ; store overflow in SOUND_VAR_3
    bne @loop              ; always branch

; move high nibble of a into low nibble of SOUND_VAR_3
; shift any overflow during multiplication as well
@shift:
    asl
    rol SOUND_VAR_3
    asl
    rol SOUND_VAR_3
    asl
    rol SOUND_VAR_3
    asl
    rol SOUND_VAR_3
    lda SOUND_VAR_3 ; shift high nibble to low nibble of SOUND_VAR_3
    rts

; get the APU configuration register offset (SOUND_CHNL_REG_OFFSET) from sound
; slot, e.g. #$01 --> #$04.  If there are 2 pulse channel 1 sounds playing, this
; will set the carry to indicate no way to play sound
; input
;  * x - sound slot
; output
;  * x - sound channel configuration register offset
;  * carry flag
;    * clear to update the apu register
;    * set to not update apu register.  When set, there is already a sound
;      playing on that channel that has priority.  This check is only performed
;      for slot #$00 (pulse 1 channel)
ldx_channel_register:
    pha
    cpx #$00    ; see if current sound slot is pulse channel 1
    beq @slot_0 ; branch if slot is the high priority pulse channel 1

@exit_clear:
    clc

@exit:
    lda sound_channel_reg_tbl,x ; load register offset for slot
    tax                         ; transfer register offset to a
    pla                         ; restore a before exiting
    rts

@slot_0:
    lda SOUND_CODE+4 ; load second pulse 1 slot
    beq @exit_clear  ; branch if no sound in slot 4 (higher priority slot),
                     ; indicating that slot 0 data can be used to write to APU
    sec              ; sound slot 4 is in use, set carry to not play sound slot 0
    bne @exit        ; always branch

; set sound channel configuration (mute), advance sound command address
; input
;  * a - amount to multiply SOUND_CMD_LENGTH by
sound_cmd_routine_00:
    jsr calc_cmd_delay           ; multiply SOUND_CMD_LENGTH by a
    lda #$00                     ; sound config low nibble = #$00 (mute sound channel)
    cpx #$02                     ; see if sound slot #$02 (triangle channel)
    bne @continue
    jsr init_triangle_channel    ; triangle sound channel, just initialize
    jmp sound_envelope_read_addr

; not triangle channel
@continue:
    lda #$30
    jsr ldx_channel_register     ; set x to apu channel register [#$00, #$04, #$08, #$c] for sound slot x
    bcs sound_envelope_read_addr ; branch if there is already a sound playing slot 4, so can't play slot 0
    sta APU_PULSE_CONFIG,x       ; set pulse 1, pulse 2, or triangle configuration
    .ifdef Superc
        jsr wait                 ; execute #$0a nop instructions
    .endif

sound_envelope_read_addr:
    ldx SOUND_CURRENT_SLOT ; load current sound slot
    lda SOUND_FLAGS,x      ; load the current sound slot's sound flags
    ora #$40               ; set bit 6 (mute flag)
    sta SOUND_FLAGS,x
    jmp adv_sound_cmd_addr ; set the sound_xx command read offset to current read location + 1

; set in memory configuration for channel, set multiplier, and sometimes read_bgm_cmd
; input
;  * a - low nibble of sound byte value
sound_cmd_routine_01:
    sta SOUND_LENGTH_MULTIPLIER,x ; store value used to calculate SOUND_CMD_LENGTH
    iny                           ; increment sound code read offset
    lda (CUR_SOUND_ADDR),y        ; load sound code byte
    cpx #$02                      ; compare current sound slot to sound slot #$02 (triangle channel)
    beq set_sound_triangle_config ; branch if triangle channel to set triangle config in memory and read_bgm_cmd
    and #$f0                      ; not triangle sound slot, get low nibble
    sta SOUND_CFG_HIGH_A,x
    lda (CUR_SOUND_ADDR),y
    and #$0f
    sta SOUND_EFFECT_VOLUME,x
    iny
    lda (CUR_SOUND_ADDR),y
    and #$40
    bne @continue
    lda (CUR_SOUND_ADDR),y
    jmp @set_envelope_offset

@continue:
    lda VIBRATO_CTRL,x
    ora #$80
    sta VIBRATO_CTRL,x
    lda (CUR_SOUND_ADDR),y
    and #$bf

@set_envelope_offset:
    sta SOUND_CTRL_ENVELOPE_OFFSET,x
    bpl @load_volume
    lda #$00
    beq @store_volume

@load_volume:
    lda SOUND_EFFECT_VOLUME,x

@store_volume:
    sta SOUND_VOLUME_ADJ2,x
    iny

read_decrescendo:
    lda (CUR_SOUND_ADDR),y
    and #$0f
    sta DECRESCENDO_PERCENT,x
    lda (CUR_SOUND_ADDR),y
    lsr
    lsr
    lsr
    lsr
    sta SOUND_DECRESCENDO_STEP_TIME,x
    lda SOUND_DECRESCENDO_STEP_TIME,x
    bne @set_vol_timer_adv
    lda #$01
    sta SOUND_DECRESCENDO_STEP_TIME,x

@set_vol_timer_adv:
    sta SOUND_VOLUME_ADJ_TIMER,x
    iny
    jmp read_bgm_cmd

set_sound_triangle_config:
    sta SOUND_TRIANGLE_CONFIG
    iny
    jmp read_bgm_cmd

sound_cmd_routine_02:
    sta SOUND_VAR_1
    lda VIBRATO_CTRL,x
    and #$7f
    sta VIBRATO_CTRL,x       ; set using SOUND_CFG_HIGH_A,x
    lda SOUND_VAR_1
    cmp #$06
    bcs @continue
    sta SOUND_OCTAVE_SHIFT,x
    iny
    jmp read_bgm_cmd

@continue:
    and #$0f
    sec                                             ; set carry flag in preparation for subtraction
    sbc #$06
    asl
    tax
    lda sound_cmd_routine_02_subroutine_ptr_tbl,x
    sta SOUND_VAR_1
    lda sound_cmd_routine_02_subroutine_ptr_tbl+1,x
    sta SOUND_VAR_1+1
    ldx SOUND_CURRENT_SLOT                          ; load current sound slot [#$00-#$05]
    jmp (SOUND_VAR_1)

; !(UNUSED)
sound_cmd_routine_02_unused:
    iny
    lda (CUR_SOUND_ADDR),y
    and #$0f
    jmp sound_cmd_routine_02

sound_cmd_routine_02_subroutine_ptr_tbl:
    .addr sound_cmd_routine_02_subroutine_00
    .addr sound_cmd_routine_02_subroutine_01
    .addr sound_cmd_routine_02_subroutine_02
    .addr sound_cmd_routine_02_subroutine_03
    .addr sound_cmd_routine_02_subroutine_04
    .addr sound_cmd_routine_02_subroutine_05
    .addr sound_cmd_routine_02_subroutine_06
    .addr sound_cmd_routine_02_subroutine_07 ; set timer adjust value SOUND_TIMER_ADJ,x (adjusts pitch)
    .addr sound_cmd_routine_02_subroutine_08 ; read and set SOUND_FRAME_SKIP_COUNT
    .addr sound_cmd_routine_02_subroutine_07 ; set timer adjust value SOUND_TIMER_ADJ,x (adjusts pitch)

sound_cmd_routine_02_subroutine_00:
    iny
    lda (CUR_SOUND_ADDR),y
    sta SOUND_LENGTH_MULTIPLIER,x
    lda VIBRATO_CTRL,x
    bpl @continue
    lda SOUND_LENGTH_MULTIPLIER,x
    and #$0f
    sta SOUND_LENGTH_MULTIPLIER,x

@continue:
    iny
    jmp read_bgm_cmd

sound_cmd_routine_02_subroutine_01:
    iny
    lda (CUR_SOUND_ADDR),y
    sta SOUND_CTRL_ENVELOPE_OFFSET,x
    lda VIBRATO_CTRL,x
    and #$7f
    sta VIBRATO_CTRL,x
    iny
    jmp read_bgm_cmd

sound_cmd_routine_02_subroutine_02:
    iny
    jmp read_decrescendo

; determine which value is used for setting config register
; either SOUND_CFG_HIGH_A or SOUND_CFG_HIGH_B
; then read background music command
sound_cmd_routine_02_subroutine_03:
    iny
    lda (CUR_SOUND_ADDR),y
    beq set_use_sound_cfg_high_a
    and #$0f
    beq set_sound_cfg_high_a
    lda (CUR_SOUND_ADDR),y       ; using SOUND_CFG_HIGH_B,x
    sta SOUND_CFG_HIGH_B,x
    lda VIBRATO_CTRL,x
    ora #$20
    sta VIBRATO_CTRL,x           ; set using SOUND_CFG_HIGH_B,x

sound_cmd_routine_02_next:
    iny
    jmp read_bgm_cmd

set_use_sound_cfg_high_a:
    lda VIBRATO_CTRL,x
    and #$df
    sta VIBRATO_CTRL,x            ; set using SOUND_CFG_HIGH_A,x
    lda #$00
    sta SOUND_CFG_HIGH_B,x
    jmp sound_cmd_routine_02_next

set_sound_cfg_high_a:
    lda (CUR_SOUND_ADDR),y
    sta SOUND_CFG_HIGH_A,x
    jmp sound_cmd_routine_02_next

sound_cmd_routine_02_subroutine_04:
    iny
    lda (CUR_SOUND_ADDR),y
    bmi @negate
    asl

@set_period_offset_adv:
    sta SOUND_NOTE_PERIOD_OFFSET,x
    iny
    jmp read_bgm_cmd

@negate:
    asl
    ora #$80
    jmp @set_period_offset_adv

sound_cmd_routine_02_subroutine_05:
    iny
    lda (CUR_SOUND_ADDR),y
    beq @disable_pitch_adj
    sec                           ; set carry flag in preparation for subtraction
    sbc #$50
    sta SOUND_PITCH_CTRL,x        ; set sound_pitch_ctrl_ptr_tbl offset
    lda VIBRATO_CTRL,x
    ora #$10
    sta VIBRATO_CTRL,x
    iny
    lda (CUR_SOUND_ADDR),y
    and #$f0
    lsr
    lsr
    lsr
    lsr
    cmp #$00
    beq @set_pitch_timer
    sta SOUND_VAR_1
    dec SOUND_VAR_1
    beq @set_multiplier
    lda SOUND_LENGTH_MULTIPLIER,x
    clc

@loop:
    adc SOUND_LENGTH_MULTIPLIER,x
    dec SOUND_VAR_1
    bne @loop
    beq @set_pitch_timer

@set_multiplier:
    lda SOUND_LENGTH_MULTIPLIER,x

@set_pitch_timer:
    sta SOUND_PITCH_ADJ_PERIOD,x
    lda (CUR_SOUND_ADDR),y
    and #$0f
    sta SOUND_PERIOD_MULT,x
    jmp sound_cmd_routine_02_next

@disable_pitch_adj:
    lda VIBRATO_CTRL,x
    and #$ef
    sta VIBRATO_CTRL,x            ; strip bit 4
    jmp sound_cmd_routine_02_next

sound_cmd_routine_02_subroutine_06:
    iny
    lda VIBRATO_CTRL,x
    bpl @read_byte
    lda (CUR_SOUND_ADDR),y
    and #$0f
    beq @set_pitch_flags_adv
    bne @handle_byte         ; always branch

@read_byte:
    lda (CUR_SOUND_ADDR),y
    beq @set_pitch_flags_adv

@handle_byte:
    cmp #$04
    bcc @continue
    sec           ; set carry flag in preparation for subtraction
    sbc #$03
    ora #$10

; sound by less than #$04
@continue:
    ora #$40
    sta SOUND_PITCH_FLAGS,x
    lda #$00
    sta PITCH_OVERWRITE,x

@adv_bgm_cmd:
    iny
    jmp read_bgm_cmd

@set_pitch_flags_adv:
    lda #$80
    sta SOUND_PITCH_FLAGS,x
    bne @adv_bgm_cmd        ; always branch

; read and set SOUND_TIMER_ADJ,x
sound_cmd_routine_02_subroutine_07:
    iny
    lda (CUR_SOUND_ADDR),y ; load timer adjust value (adjusts pitch)
    sta SOUND_TIMER_ADJ,x
    lda SOUND_FLAGS,x
    iny
    jmp read_sound_cmd

; read and set SOUND_FRAME_SKIP_COUNT
; used only by sound_32
sound_cmd_routine_02_subroutine_08:
    iny
    lda (CUR_SOUND_ADDR),y
    sta SOUND_FRAME_SKIP_COUNT
    lda #$00
    sta SOUND_FRAME_SKIP       ; initialize counter
    iny
    jmp read_bgm_cmd

sound_go_to_parent_cmd:
    lda PARENT_SOUND_ADDRS_LOW,x
    sta SOUND_CMD_ADDRS_LOW,x     ; set low byte of sound part address (sound_xx_slot_xx)
    lda PARENT_SOUND_ADDRS_HIGH,x
    sta SOUND_CMD_ADDRS_HIGH,x    ; set high byte of sound part address (sound_xx_slot_xx)
    jmp sound_start_part

change_sound_part_addr:
    iny
    lda (CUR_SOUND_ADDR),y     ; load repeat count
    cmp #$ff
    bne inc_repeat_adv         ; branch if more sub-commands to process
    iny                        ; read entire sound command, move to next address
    lda (CUR_SOUND_ADDR),y
    sta SOUND_CMD_ADDRS_LOW,x  ; set low byte of sound part address (sound_xx_slot_xx)
    sta SOUND_VAR_1            ; temporarily store low byte of address
    iny
    lda (CUR_SOUND_ADDR),y
    sta SOUND_CMD_ADDRS_HIGH,x ; set high byte of sound part address (sound_xx_slot_xx)
    sta CUR_SOUND_ADDR+1
    lda SOUND_VAR_1
    sta CUR_SOUND_ADDR
    ldy #$00

; practically same as read_sound_cmd
read_sound_cmd2:
    lda SOUND_FLAGS,x
    lsr
    bcs @read_sound_effect_cmd
    jmp read_bgm_cmd

@read_sound_effect_cmd:
    jmp read_sound_effect_cmd

; input
;  * a - the number of times to repeat the command
inc_repeat_adv:
    inc SOUND_CMD_REPEATS,x
    cmp SOUND_CMD_REPEATS,x    ; compare to the number of times to repeat
    beq sound_cmd_complete     ; branch if finished repeating the command
    bcs sound_go_to_parent_cmd ; always branch to repeat the just processed command

sound_start_part:
    ldy #$00
    lda SOUND_CMD_ADDRS_LOW,x  ; load low byte of sound part address (sound_xx_slot_xx)
    sta CUR_SOUND_ADDR
    lda SOUND_CMD_ADDRS_HIGH,x ; load high byte of sound part address (sound_xx_slot_xx)
    sta CUR_SOUND_ADDR+1
    bne read_sound_cmd2        ; always branch

sound_cmd_complete:
    lda #$00
    sta SOUND_CMD_REPEATS,x ; re-initialize repeat counter
    lda SOUND_FLAGS,x
    and #$f7
    sta SOUND_FLAGS,x
    iny
    tya
    clc                     ; clear carry in preparation for addition
    adc CUR_SOUND_ADDR
    sta CUR_SOUND_ADDR
    lda #$00
    tay
    adc CUR_SOUND_ADDR+1
    sta CUR_SOUND_ADDR+1    ; set new starting point for next sound command
    lda SOUND_FLAGS,x
    lsr                     ; push sound effect/background music bit to carry
    bcs @sound_effect       ; branch if sound efft
    jmp read_bgm_cmd        ; always just to process background music

@sound_effect:
    lda SOUND_FLAGS,x
    and #$fb
    sta SOUND_FLAGS,x
    jmp read_sound_effect_cmd

sound_cmd_routine_03:
    cmp #$0e
    beq change_sound_part_addr
    bcc @check_cmd_bit
    jmp run_end_sound_cmd_routine ; sound byte is #$ff

@check_cmd_bit:
    cmp #$0d
    beq @set_or_chg_addr
    cmp #$0c
    beq @set_flags_read_cmd
    cmp #$0b
    beq @set_parent_sound
    lda (CUR_SOUND_ADDR),y
    and #$0f
    sta SOUND_VOLUME_ADJ2,x
    iny
    jmp read_bgm_cmd

@set_parent_sound:
    iny
    tya
    clc                           ; clear carry in preparation for addition
    adc CUR_SOUND_ADDR
    sta PARENT_SOUND_ADDRS_LOW,x
    lda #$00
    adc CUR_SOUND_ADDR+1          ; add any overflow to high byte
    sta PARENT_SOUND_ADDRS_HIGH,x
    lda SOUND_FLAGS,x
    ora #$08
    sta SOUND_FLAGS,x
    jmp read_sound_cmd

@set_flags_read_cmd:
    iny
    lda SOUND_FLAGS,x
    and #$df
    sta SOUND_FLAGS,x  ; strip bit 5
    jmp read_sound_cmd

@set_or_chg_addr:
    lda SOUND_FLAGS,x
    and #$20
    beq @set_addr
    jmp @go_to_parent_addr

@set_addr:
    jsr move_sound_part_addr
    iny
    tya
    clc                             ; clear carry in preparation for addition
    adc CUR_SOUND_ADDR
    sta PARENT_SOUND_ADDRS_2_LOW,x
    lda #$00
    tay
    adc CUR_SOUND_ADDR+1
    sta PARENT_SOUND_ADDRS_2_HIGH,x
    lda SOUND_FLAGS,x
    ora #$20
    sta SOUND_FLAGS,x
    jmp sound_start_part            ; start processing sound at slot sound command address

@go_to_parent_addr:
    lda PARENT_SOUND_ADDRS_2_LOW,x
    sta CUR_SOUND_ADDR
    lda PARENT_SOUND_ADDRS_2_HIGH,x
    sta CUR_SOUND_ADDR+1
    lda SOUND_FLAGS,x
    and #$df
    sta SOUND_FLAGS,x               ; clear bit 5
    ldy #$00
    jmp read_sound_cmd

run_end_sound_cmd_routine:
    lda SOUND_CODE,x
    sta SOUND_VAR_2
    lda #$00
    sta SOUND_CODE,x
    txa
    asl
    tax
    lda end_sound_cmd_routine_ptr_tbl,x
    sta SOUND_VAR_1
    lda end_sound_cmd_routine_ptr_tbl+1,x
    sta SOUND_VAR_1+1
    ldx SOUND_CURRENT_SLOT                ; load current sound slot [#$00-#$05]
    jmp (SOUND_VAR_1)

; mutes the current sound channel, and for sound slot 1, see if playing boss heart destroyed sound
; if so, play end of level song
; input
;  * x - current sound slot
mute_channel:
    lda #$30                 ; a = #$30 (mute pulse channel register)
    jsr ldx_channel_register ; set x to apu channel register [0, 1, 4, 5, 8, #$c]
    bcs @continue            ; branch if there is already a sound playing slot 4, so can't play slot 0
    sta APU_PULSE_CONFIG,x   ; update pulse channel config (mute pulse channel 1 or 2 register)
    .ifdef Superc
        jsr wait             ; execute #$0a nop instructions
    .endif

@continue:
    ldx SOUND_CURRENT_SLOT ; load current sound slot [#$00-#$05]
    rts

init_triangle_channel:
    lda #$0b                ; %0000 1011
    sta APU_STATUS          ; disable triangle channel (while also enabling noise, and pulse channels)
    lda #$00                ; a = #$00
    sta APU_TRIANGLE_CONFIG ; set new linear counter reload value (fine grain duration)
    lda #$0f                ; a = #$0f
    sta APU_STATUS          ; re-enable triangle channel (enable noise, triangle, and pulse channels)
    rts

init_pulse_channel:
    ldx SOUND_CHNL_REG_OFFSET       ; load pulse waive channel register offset, i.e. #$04 for second pulse channel #$00 for first
    lda #$30                        ; a = #$30
    sta APU_PULSE_CONFIG,x          ; mute the pulse channel 0 register
    .ifdef Superc
        jsr wait                    ; execute #$0a nop instructions
    .endif
    lda #$7f                        ; bit 7 set to 0 all other bits 1
    sta APU_PULSE_SWEEP,x           ; disable pulse 1 channel sweep
    .ifdef Superc
        jsr wait                    ; execute #$0a nop instructions
    .endif
    lda SOUND_FLAGS+4
    and #$f9
    sta SOUND_FLAGS+4
    stx SOUND_TIMER_ADJ+4
    lda SOUND_CODE,x
    beq @exit
    ldy SOUND_CHNL_REG_OFFSET
    jsr set_pulse_channel_registers

@exit:
    ldx SOUND_CURRENT_SLOT ; load current sound slot [#$00-#$05]
    rts

; mutes the noise channel
mute_noise_channel:
    lda #$30             ; a = #$30
    sta APU_NOISE_CONFIG ; initialize noise config with no volume
    .ifdef Superc
        jsr wait         ; execute #$0a nop instructions
    .endif
    rts

adv_sound_cmd_addr:
    iny
    tya
    clc                        ; clear carry in preparation for addition
    adc CUR_SOUND_ADDR
    sta SOUND_CMD_ADDRS_LOW,x  ; set low byte of sound part address (sound_xx_slot_xx)
    lda #$00
    adc CUR_SOUND_ADDR+1       ; add any overflow to address high byte
    sta SOUND_CMD_ADDRS_HIGH,x ; set high byte of sound part address (sound_xx_slot_xx)
    rts

move_sound_part_addr:
    iny
    lda (CUR_SOUND_ADDR),y
    sta SOUND_CMD_ADDRS_LOW,x  ; set low byte of sound part address (sound_xx_slot_xx)
    iny
    lda (CUR_SOUND_ADDR),y
    sta SOUND_CMD_ADDRS_HIGH,x ; set high byte of sound part address (sound_xx_slot_xx)
    rts

bgm_cmd_ptr_tbl:
    .addr sound_control_envelope
    .addr bgm_cmd_01
    .addr bgm_cmd_02
    .addr bgm_cmd_03

end_sound_cmd_routine_ptr_tbl:
    .addr mute_channel
    .addr mute_channel
    .addr init_triangle_channel
    .addr mute_noise_channel
    .addr init_pulse_channel
    .addr mute_channel

sound_cmd_ptr_tbl:
    .addr sound_cmd_routine_00
    .addr sound_cmd_routine_01
    .addr sound_cmd_routine_02
    .addr sound_cmd_routine_03

; tables for note period to use when writing notes to the APU (#$30 bytes)
; the frequency of the pulse channels is a division of the CPU Clock (1.789773MHz NTSC, 1.662607MHz PAL)
; the output frequency (f) of the generator can be determined by the 11-bit period value (f_pulse) written to $4002-$4003/$4006-$4007
; note that triangle channel is one octave lower
; frequency = cpu_speed / (#$0f * (f_pulse + 1))
; ex: 1789773 / (#$10 * (#$06ae + 1)) => 65.38 Hz
.ifdef Probotector
    ; lower pitches
    note_period_tbl_2:
        .byte $69,$0c ; $0c69 - 3,177 - 32.70 (c 1)
        .byte $b6,$0b ; $0bb6 - 2,998 - 34.65 (c#/d flat 1)
        .byte $0f,$0b ; $0b0f - 2,831 - 36.69 (d1)
        .byte $6f,$0a ; $0a6f - 2,671 - 38.89 (d#/e flat 1)
        .byte $d9,$09 ; $09d9 - 2,521 - 41.20 (e 1)
        .byte $4b,$09 ; $094b - 2,379 - 43.66 (f 1)
        .byte $c6,$08 ; $08c6 - 2,246 - 46.25 (f#/g flat 1)
        .byte $47,$08 ; $0847 - 2,119 - 49.02 (g1)
        .byte $d0,$07 ; $07d0 - 2,000 - 51.93 (g#/a flat 1)
        .byte $61,$07 ; $0761 - 1,889 - 54.98 (a1)
        .byte $f7,$06 ; $06f7 - 1,783 - 58.25 (a#/b flat 1)
        .byte $93,$06 ; $0693 - 1,683 - 61.71 (b1)

    note_period_tbl:
        .byte $34,$06 ; $0634 - 1,588 - 65.40 Hz (c 2/deep c)
        .byte $db,$05 ; $05db - 1,499 - 69.28 Hz (c#/d flat 2)
        .byte $87,$05 ; $0587 - 1,415 - 73.38 Hz (d 2)
        .byte $37,$05 ; $0537 - 1,335 - 77.78 Hz (d#/e flat 2)
        .byte $ec,$04 ; $04ec - 1,260 - 82.41 Hz (e 2)
        .byte $a5,$04 ; $04a5 - 1,189 - 87.32 Hz (f 2)
        .byte $63,$04 ; $0463 - 1,123 - 92.45 Hz (f#/g flat 2)
        .byte $23,$04 ; $0423 - 1,059 - 98.03 Hz (g 2)
        .byte $e8,$03 ; $03e8 - 1,000 - 103.81 Hz (g#/a flat 2)
        .byte $b0,$03 ; $03b0 - 944 - 109.96 Hz (a 2)
        .byte $7b,$03 ; $037b - 891 - 116.49 Hz (a#/b flat 2)
        .byte $49,$03 ; $0349 - 841 - 123.41 Hz (b 2)
        .byte $1a,$03 ; $031a - 794 - 130.71 Hz (c 3)
        .byte $ed,$02 ; $02ed - 749 - 138.55 Hz (c#/d flat 3)
        .byte $c3,$02 ; $02c3 - 707 - 146.77 Hz (d 3)
        .byte $9b,$02 ; $029b - 667 - 155.56 Hz (d#/e flat 3)
        .byte $76,$02 ; $0276 - 630 - 164.68 Hz (e 3)
        .byte $53,$02 ; $0253 - 595 - 174.35 Hz (f 3)
        .byte $32,$02 ; $0232 - 562 - 184.57 Hz (f#/g flat 3)
        .byte $12,$02 ; $0212 - 530 - 195.69 Hz (g 3)
        .byte $f4,$01 ; $01f4 - 500 - 207.41 Hz (g#/a flat 3)
        .byte $d7,$01 ; $01d7 - 471 - 220.15 Hz (a 3)
        .byte $bd,$01 ; $01bd - 445 - 232.99 Hz (b 3)
        .byte $a4,$01 ; $01a4 - 420 - 246.82 Hz (c 4/middle c)
.else
    ; lower pitches
    note_period_tbl_2:
        .byte $5c,$0d ; $0d5c - 3,420 - 32.70 Hz (c 1)
        .byte $9c,$0c ; $0c9c - 3,228 - 34.64 Hz (c#/d flat 1)
        .byte $e8,$0b ; $0be8 - 3,048 - 36.69 Hz (d1)
        .byte $3c,$0b ; $0b3c - 2,876 - 38.88 Hz (d#/e flat 1)
        .byte $9c,$0a ; $0a9c - 2,716 - 41.17 Hz (e 1)
        .byte $02,$0a ; $0a02 - 2,562 - 43.64 Hz (f 1)
        .byte $72,$09 ; $0972 - 2,418 - 46.24 Hz (f#/g flat 1)
        .byte $ec,$08 ; $08ec - 2,284 - 48.95 Hz (g1)
        .byte $6c,$08 ; $086c - 2,156 - 51.86 Hz (g#/a flat 1)
        .byte $f2,$07 ; $07f2 - 2,034 - 54.97 Hz (a1)
        .byte $80,$07 ; $0780 - 1,920 - 58.23 Hz (a#/b flat 1)
        .byte $14,$07 ; $0714 - 1,812 - 61.70 Hz (b1)

    note_period_tbl:
        .byte $ae,$06 ; $06ae - 1,710 - 65.38 Hz (c 2/deep c)
        .byte $4e,$06 ; $064e - 1,614 - 69.26 Hz (c#/d flat 2)
        .byte $f4,$05 ; $05f4 - 1,524 - 73.35 Hz (d 2)
        .byte $9e,$05 ; $059e - 1,438 - 77.74 Hz (d#/e flat 2)
        .byte $4e,$05 ; $054e - 1,358 - 82.31 Hz (e 2)
        .byte $01,$05 ; $0501 - 1,281 - 87.25 Hz (f 2)
        .byte $b9,$04 ; $04b9 - 1,209 - 92.45 Hz (f#/g flat 2)
        .byte $76,$04 ; $0476 - 1,142 - 97.87 Hz (g 2)
        .byte $36,$04 ; $0436 - 1,078 - 103.67 Hz (g#/a flat 2)
        .byte $f9,$03 ; $03f9 - 1,017 - 109.88 Hz (a 2)
        .byte $c0,$03 ; $03c0 - 960 - 116.40 Hz (a#/b flat 2)
        .byte $8a,$03 ; $038a - 906 - 123.33 Hz (b 2)
        .byte $57,$03 ; $0357 - 855 - 130.68 Hz (c 3)
        .byte $27,$03 ; $0327 - 807 - 138.44 Hz (c#/d flat 3)
        .byte $fa,$02 ; $02fa - 762 - 146.61 Hz (d 3)
        .byte $cf,$02 ; $02cf - 719 - 155.36 Hz (d#/e flat 3)
        .byte $a7,$02 ; $02a7 - 679 - 164.50 Hz (e 3)
        .byte $81,$02 ; $0281 - 641 - 174.24 Hz (f 3)
        .byte $5d,$02 ; $025d - 605 - 184.59 Hz (f#/g flat 3)
        .byte $3b,$02 ; $023b - 571 - 195.56 Hz (g 3)
        .byte $1b,$02 ; $021b - 539 - 207.15 Hz (g#/a flat 3)
        .byte $fd,$01 ; $01fd - 509 - 219.33 Hz (a 3)
        .byte $e0,$01 ; $01e0 - 480 - 232.56 Hz (b 3)
        .byte $c5,$01 ; $01c5 - 453 - 246.39 Hz (c 4/middle c)
.endif

.ifdef Superc
    ; 10 nop instructions
    wait:
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        rts
.endif

sound_code_tbl:
    .addr sound_01
    .addr sound_02
    .addr sound_03
    .addr sound_04
    .addr sound_05
    .addr sound_06 ; SUTA - player landing on ground
    .addr sound_07 ; footstep
    .addr sound_08 ; earthquake shake (jungle, jagger froid, final boss)
    .addr sound_09 ; helicopter rotor noise
    .addr sound_0a ; THUNDER - level one intro thunder
    .addr sound_0b ; SHOT - default weapon
    .addr sound_0c ; M weapon
    .addr sound_0d ; LASER - L weapon
    .addr sound_0e ; SPREAD - S weapon
    .addr sound_0f ; FIRE - F weapon
    .addr sound_10 ; B SHOT
    .addr sound_11 ; T DAMEGE
    .addr sound_12 ; HARETSU
    .addr sound_13 ; T OUT
    .addr sound_14 ; Z OUT
    .addr sound_15 ; A OUT
    .addr sound_16 ; ROLL
    .addr sound_17 ; mortar round (silent)
    .addr sound_18 ; APPEAR
    .addr sound_19 ; POWER - pick up weapon item
    .addr sound_1a ; BOSS BK
    .addr sound_1b ; BAKUHA1
    .addr sound_1c ; BAKUHA2
    .addr sound_1d ; level 1 storm thunder
    .addr sound_1e ; silent
    .addr sound_1f ; ARUKU
    .addr sound_20 ; JIWARE
    .addr sound_21 ; SILEN - siren
    .addr sound_22 ; stomping ceiling slam
    .addr sound_23 ; P 1UP
    .addr sound_24
    .addr sound_25 ; P OUT - player death
    .addr sound_26 ; B OUT
    .addr sound_27 ; pause jingle
    .addr sound_28 ; BGM1 - Thunder Landing (Area 1)
    .addr sound_29 ; BGM4 - No Escape (Area 4, 7)
    .addr sound_2a ; BGM3 - Jungle Juncture (Area 3)
    .addr sound_2b ; BGM2 - In A Tight Squeeze (Area 2)
    .addr sound_2c ; BGM5 - M-3 (Area 5)
    .addr sound_2d ; BGM6 - Hotter Than Hell (Area 6)
    .addr sound_2e ; Xenophobic Organs (Area 7)
    .addr sound_2f ; BGM7 - Deathbed (Area 8) (misnamed)
    .addr sound_30 ; GREAT - Great Heli - Ruined Base (Boss 1)
    .addr sound_31 ; BOSS BGM - Ruined Base (Boss 2)
    .addr sound_32 ; BOSS2BGM - Creature from Outer Space (Boss 3)
    .addr sound_33 ; P CLEAR - end of level tune
    .addr sound_34 ; A CLEAR - final boss level clear
    .addr sound_35 ; OVER - game over
    .addr sound_36 ; ENDING - Free World (Ending)
    .addr sound_37 ; drum tap sound
    .addr sound_38 ; drum tap sound
    .addr sound_39 ; drum tap sound
    .addr sound_3a ; drum tap sound
    .addr sound_3b ; drum tap sound

sound_01:
    .byte $00,$05
    .addr sound_01_slot_05

sound_02:
    .byte $00,$05
    .addr sound_01_slot_05

sound_03:
    .byte $00,$05
    .addr sound_03_slot_05

sound_04:
    .byte $00,$05
    .addr sound_04_slot_05

sound_05:
    .byte $00,$05
    .addr sound_04_slot_05

; SUTA - player landing on ground
sound_06:
    .byte $01,$04
    .addr sound_06_slot_04
    .byte $05
    .addr sound_06_slot_05

; footstep
sound_07:
    .byte $01,$04
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_07_slot_04
    .endif
    .byte $05
    .addr sound_06_slot_05

; earthquake shake (jungle, jagger froid, final boss)
sound_08:
    .byte $01,$04
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_08_slot_04
    .endif
    .byte $05
    .addr sound_08_slot_05

; helicopter rotor noise
sound_09:
    .byte $01,$04
    .addr sound_09_slot_04
    .byte $05
    .addr sound_09_slot_05

; THUNDER - level one intro thunder
sound_0a:
    .byte $01,$04
    .addr sound_0a_slot_04
    .byte $05
    .addr sound_0a_slot_05

; SHOT - default weapon
sound_0b:
    .byte $01,$04
    .addr sound_0b_slot_04
    .byte $05
    .addr sound_0b_slot_05

; M weapon
sound_0c:
    .byte $01,$04
    .addr sound_0c_slot_04
    .byte $05
    .addr sound_0c_slot_05

; LASER - L weapon
sound_0d:
    .byte $01,$04
    .addr sound_0d_slot_04
    .byte $05
    .addr sound_0d_slot_05

; SPREAD - S weapon
sound_0e:
    .byte $01,$04
    .addr sound_0e_slot_04
    .byte $05
    .addr sound_0e_slot_05

; FIRE - F weapon
sound_0f:
    .byte $01,$04
    .addr sound_0f_slot_04
    .byte $05
    .addr sound_0f_slot_05

; B SHOT
sound_10:
    .byte $01,$04
    .addr sound_10_slot_04
    .byte $05
    .addr sound_10_slot_05

; T DAMEGE
sound_11:
    .byte $01,$04
    .addr sound_11_slot_04
    .byte $05
    .addr sound_11_slot_05

; HARETSU
sound_12:
    .byte $00,$05
    .addr sound_12_slot_05

; T OUT
sound_13:
    .byte $01,$04
    .addr sound_13_slot_04
    .byte $05
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_13_slot_05
    .endif

; Z OUT
sound_14:
    .byte $01,$04
    .addr sound_14_slot_04
    .byte $05
    .addr sound_14_slot_05

; A OUT
sound_15:
    .byte $01,$04
    .addr sound_15_slot_04
    .byte $05
    .addr sound_15_slot_05

; ROLL
sound_16:
    .byte $01,$04
    .addr sound_16_slot_04
    .byte $05
    .addr sound_16_slot_05

; mortar round (silent)
sound_17:
    .byte $00,$04
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_17_slot_04
    .endif

; APPEAR
sound_18:
    .byte $01,$04
    .addr sound_18_slot_04
    .byte $05
    .addr sound_18_slot_05

; POWER - pick up weapon item
sound_19:
    .byte $00,$04
    .addr sound_19_slot_04

; BOSS BK
sound_1a:
    .byte $01,$04
    .addr sound_1a_slot_04
    .byte $05
    .addr sound_1a_slot_05

; BAKUHA1
sound_1b:
    .byte $01,$04
    .addr sound_1b_slot_04
    .byte $05
    .addr sound_1b_slot_05

; BAKUHA2
sound_1c:
    .byte $01,$04
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_1c_slot_04
    .endif
    .byte $05
    .addr sound_1c_slot_05

; level 1 storm thunder
sound_1d:
    .byte $00,$05
    .addr sound_1d_slot_05

; silent
sound_1e:
    .byte $01,$04
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_1e_slot_04
    .endif
    .byte $05
    .ifdef Probotector
        .addr sound_slot_empty
    .else
        .addr sound_1e_slot_04
    .endif

; ARUKU
sound_1f:
    .byte $01,$04
    .addr sound_1f_slot_04
    .byte $05
    .addr sound_1f_slot_05

; JIWARE
sound_20:
    .byte $00,$05
    .addr sound_20_slot_05

; SILEN - siren
sound_21:
    .byte $00,$04
    .addr sound_21_slot_04

; stomping ceiling slam
sound_22:
    .byte $01,$04
    .addr sound_22_slot_04
    .byte $05
    .addr sound_22_slot_05

; P 1UP
sound_23:
    .byte $00,$04
    .addr sound_23_slot_04

; similar to sound_13 (T OUT)
sound_24:
    .byte $00,$04
    .addr sound_13_slot_04

; P OUT - player death
sound_25:
    .byte $01,$04
    .addr sound_25_slot_04
    .byte $05
    .addr sound_25_slot_05

; B OUT
sound_26:
    .byte $02,$01
    .addr sound_26_slot_01
    .byte $04
    .addr sound_26_slot_04
    .byte $05
    .addr sound_26_slot_05

; pause jingle
sound_27:
    .byte $00,$04
    .addr sound_27_slot_04

; BGM1 - Thunder Landing (Area 1)
sound_28:
    .byte $03,$00
    .addr sound_28_slot_00
    .byte $01
    .addr sound_28_slot_01
    .byte $02
    .addr sound_28_slot_02
    .byte $03
    .addr sound_28_slot_03

; BGM4 - No Escape (Area 4, 7)
sound_29:
    .byte $03,$00
    .addr sound_29_slot_00
    .byte $01
    .addr sound_29_slot_01
    .byte $02
    .addr sound_29_slot_02
    .byte $03
    .addr sound_29_slot_03

; BGM3 - Jungle Juncture (Area 3)
sound_2a:
    .byte $03,$00
    .addr sound_2a_slot_00
    .byte $01
    .addr sound_2a_slot_01
    .byte $02
    .addr sound_2a_slot_02
    .byte $03
    .addr sound_2a_slot_03

; BGM2 - In A Tight Squeeze (Area 2)
sound_2b:
    .byte $03,$00
    .addr sound_2b_slot_00
    .byte $01
    .addr sound_2b_slot_01
    .byte $02
    .addr sound_2b_slot_02
    .byte $03
    .addr sound_2b_slot_03

; BGM5 - M-3 (Area 5)
sound_2c:
    .byte $03,$00
    .addr sound_2c_slot_00
    .byte $01
    .addr sound_2c_slot_01
    .byte $02
    .addr sound_2c_slot_02
    .byte $03
    .addr sound_2c_slot_03

; BGM6 - Hotter Than Hell (Area 6)
sound_2d:
    .byte $03,$00
    .addr sound_2d_slot_00
    .byte $01
    .addr sound_2d_slot_01
    .byte $02
    .addr sound_2d_slot_02
    .byte $03
    .addr sound_2d_slot_03

; Xenophobic Organs (Area 7)
sound_2e:
    .byte $03,$00
    .addr sound_29_slot_00
    .byte $01
    .addr sound_29_slot_01
    .byte $02
    .addr sound_29_slot_02
    .byte $03
    .addr sound_29_slot_03

; BGM7 - Deathbed (Area 8) (misnamed)
sound_2f:
    .byte $03,$00
    .addr sound_2f_slot_00
    .byte $01
    .addr sound_2f_slot_01
    .byte $02
    .addr sound_2f_slot_02
    .byte $03
    .addr sound_2f_slot_03

; GREAT - Great Heli - Ruined Base (Boss 1)
sound_30:
    .byte $03,$00
    .addr sound_30_slot_00
    .byte $01
    .addr sound_30_slot_01
    .byte $02
    .addr sound_30_slot_02
    .byte $03
    .addr sound_30_slot_03

; BOSS BGM - Ruined Base (Boss 2)
sound_31:
    .byte $03,$00
    .addr sound_31_slot_00
    .byte $01
    .addr sound_31_slot_01
    .byte $02
    .addr sound_31_slot_02
    .byte $03
    .addr sound_31_slot_03

; BOSS2BGM - Creature from Outer Space (Boss 3)
sound_32:
    .byte $03,$00
    .addr sound_32_slot_00
    .byte $01
    .addr sound_32_slot_01
    .byte $02
    .addr sound_32_slot_02
    .byte $03
    .addr sound_32_slot_03

; P CLEAR - end of level tune
sound_33:
    .byte $03,$00
    .addr sound_33_slot_00
    .byte $01
    .addr sound_33_slot_01
    .byte $02
    .addr sound_33_slot_02
    .byte $03
    .addr sound_33_slot_03

; A CLEAR - final boss level clear
sound_34:
    .byte $03,$00
    .addr sound_34_slot_00
    .byte $01
    .addr sound_34_slot_01
    .byte $02
    .addr sound_34_slot_02
    .byte $03
    .addr sound_34_slot_03

; OVER - game over
sound_35:
    .byte $03,$00
    .addr sound_35_slot_00
    .byte $01
    .addr sound_35_slot_01
    .byte $02
    .addr sound_35_slot_02
    .byte $03
    .addr sound_35_slot_03

; ENDING - Free World (Ending)
sound_36:
    .byte $03,$00
    .addr sound_36_slot_00
    .byte $01
    .addr sound_36_slot_01
    .byte $02
    .addr sound_36_slot_02
    .byte $03
    .addr sound_36_slot_03

; drum tap sound
sound_37:
    .byte $00,$06
    .addr sound_37_slot_06

; drum tap sound
sound_38:
    .byte $00,$06
    .addr sound_38_slot_06

; drum tap sound
sound_39:
    .byte $00,$06
    .addr sound_39_slot_06

; drum tap sound
sound_3a:
    .byte $00,$06
    .addr sound_3a_slot_06

; drum tap sound
sound_3b:
    .byte $00,$06
    .addr sound_3b_slot_06

sound_envelope_ptr_tbl:
    .addr sound_envelope_00
    .addr sound_envelope_01
    .addr sound_envelope_02
    .addr sound_envelope_03
    .addr sound_envelope_04
    .addr sound_envelope_05
    .addr sound_envelope_06
    .addr sound_envelope_07
    .addr sound_envelope_08
    .addr sound_envelope_09
    .addr sound_envelope_0a
    .addr sound_envelope_0b
    .addr sound_envelope_0c
    .addr sound_envelope_0d
    .addr sound_envelope_0e
    .addr sound_envelope_0f
    .addr sound_envelope_10
    .addr sound_envelope_11
    .addr sound_envelope_12
    .addr sound_envelope_13
    .addr sound_envelope_14
    .addr sound_envelope_15
    .addr sound_envelope_16
    .addr sound_envelope_17
    .addr sound_envelope_18
    .addr sound_envelope_19
    .addr sound_envelope_1a
    .addr sound_envelope_1b
    .addr sound_envelope_1c
    .addr sound_envelope_1d
    .addr sound_envelope_1e
    .addr sound_envelope_1f
    .addr sound_envelope_20
    .addr sound_envelope_21
    .addr sound_envelope_22
    .addr sound_envelope_23
    .addr sound_envelope_24
    .addr sound_envelope_25
    .addr sound_envelope_26
    .addr sound_envelope_27
    .addr sound_envelope_28

sound_pitch_ctrl_ptr_tbl:
    .addr sound_pitch_ctrl_00
    .addr sound_pitch_ctrl_01
    .addr sound_pitch_ctrl_02
    .addr sound_pitch_ctrl_03
    .addr sound_pitch_ctrl_04
    .addr sound_pitch_ctrl_05
    .addr sound_pitch_ctrl_06
    .addr sound_pitch_ctrl_07

sound_envelope_00:
    .byte $14,$25,$34,$30,$11,$ff

sound_envelope_01:
    .byte $16,$25,$34,$30,$11,$ff

sound_envelope_02:
    .byte $15,$16,$15,$14,$13,$ff

sound_envelope_03:
    .byte $14,$25,$34,$43,$52,$ff

sound_envelope_04:
    .byte $18,$17,$16,$15,$14,$13,$ff

sound_envelope_05:
    .byte $14,$25,$36,$35,$34,$ff

sound_envelope_06:
    .byte $17,$28,$37,$30,$12,$ff

sound_envelope_07:
    .byte $17,$16,$15,$14,$13,$ff

sound_envelope_08:
    .byte $14,$15,$16,$15,$14,$13,$ff

sound_envelope_09:
    .byte $16,$25,$34,$ff

sound_envelope_0a:
    .byte $16,$15,$34,$43,$52,$ff

sound_envelope_0b:
    .byte $14,$14,$34,$43,$52,$ff

sound_envelope_0c:
    .byte $17,$16,$25,$44,$63,$fb,$53,$52,$fe,$ff

sound_envelope_0d:
    .byte $16,$15,$34,$43,$52,$ff

sound_envelope_0e:
    .byte $1a,$24,$23,$12,$11,$ff

sound_envelope_0f:
    .byte $19,$25,$34,$43,$ff

sound_envelope_10:
    .byte $1a,$16,$15,$44,$30,$11,$ff

sound_envelope_11:
    .byte $1f,$17,$26,$35,$44,$53,$ff

sound_envelope_12:
    .byte $16,$15,$24,$33,$43,$ff

sound_envelope_13:
    .byte $17,$15,$14,$43,$ff

sound_envelope_14:
    .byte $1b,$16,$25,$34,$43,$ff

sound_envelope_15:
    .byte $1f,$18,$27,$36,$45,$54,$ff

sound_envelope_16:
    .byte $19,$16,$25,$34,$43,$ff

sound_envelope_17:
    .byte $1b,$17,$26,$35,$44,$53,$ff

sound_envelope_18:
    .byte $16,$16,$15,$14,$13,$ff

sound_envelope_19:
    .byte $1f,$19,$28,$37,$46,$55,$64,$73,$ff

sound_envelope_1a:
    .byte $18,$35,$24,$13,$ff

sound_envelope_1b:
    .byte $17,$16,$35,$34,$33,$12,$ff

sound_envelope_1c:
    .byte $15,$16,$15,$14,$13,$ff

sound_envelope_1d:
    .byte $1b,$17,$26,$35,$44,$53,$ff

sound_envelope_1e:
    .byte $1a,$18,$17,$16,$15,$ff

sound_envelope_1f:
    .byte $18,$16,$25,$34,$43,$ff

sound_envelope_20:
    .byte $1f,$17,$26,$35,$44,$33,$20,$12,$ff

sound_envelope_21:
    .byte $1b,$18,$26,$35,$44,$53,$62,$ff

sound_envelope_22:
    .byte $16,$15,$24,$33,$42,$ff

sound_envelope_23:
    .byte $14,$25,$34,$43,$ff

sound_envelope_24:
    .byte $15,$27,$16,$15,$14,$ff

sound_envelope_25:
    .byte $1d,$29,$28,$37,$46,$55,$ff

sound_envelope_26:
    .byte $1c,$18,$17,$16,$15,$14,$ff

sound_envelope_27:
    .byte $14,$16,$15,$14,$13,$ff

sound_envelope_28:
    .byte $14,$14,$35,$44,$53,$42,$ff

sound_pitch_ctrl_00:
    .byte $fb,$11,$12,$13,$12,$11,$10,$1f,$1e,$1d,$1e,$1f,$10,$fe,$ff

sound_pitch_ctrl_01:
    .byte $fb,$11,$12,$23,$12,$11,$20,$1f,$1e,$2d,$1e,$1f,$10,$fe,$ff

sound_pitch_ctrl_02:
    .byte $fb,$11,$12,$13,$12,$11,$10,$1f,$1e,$2d,$1e,$1f,$10,$fe,$ff

sound_pitch_ctrl_03:
    .byte $fb,$11,$12,$13,$12,$11,$20,$1f,$1e,$1d,$1e,$1f,$20,$fe,$ff

sound_pitch_ctrl_04:
    .byte $fb,$10,$17,$fe,$ff

sound_pitch_ctrl_05:
    .byte $ff

sound_pitch_ctrl_06:
    .byte $50,$fb,$11,$12,$13,$12,$11,$10,$1f,$1e,$1d,$1e,$1f,$10,$fe,$ff

sound_pitch_ctrl_07:
    .byte $fb,$11,$12,$14,$12,$11,$10,$1f,$1e,$2d,$1e,$1f,$10,$fe,$ff

; #$1b samples total
; byte 0 - sampling rate APU_DMC
; byte 1 - initial sample value (always #$00) APU_DMC_COUNTER
; byte 2 - address (computed offset from $c000) APU_DMC_SAMPLE_ADDR
; byte 3 - sample length in bytes APU_DMC_SAMPLE_LEN
dpcm_samples:
; sample #$00 - dpcm_sample_01 (#$201 bytes)
; 513 byte sample
; 4,104 samples at 33.1 kHz sample rate
; .124 second sample
    .byte $0f,$00
    .byte .LOBYTE(dpcm_sample_01>>6)
    .byte $20

; sample #$01 - dpcm_sample_00 (#$141 bytes)
; 321 byte sample
; 2,568 samples at 16.9 khz sample rate
; .152 second sample
    .byte $0c,$00
    .byte .LOBYTE(dpcm_sample_00>>6)
    .byte $14

; sample #$02 - dpcm_sample_00 (#$141 bytes)
; 321 byte sample
; 2,568 samples at 21.3 khz sample rate
; .121 second sample
sound_37_slot_06:
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_00>>6)
    .byte $14

; sample #$03 - dpcm_sample_00 (#$141 bytes)
; 321 byte sample
; 2,568 samples at 24.9 khz sample rate
; .103 second sample
sound_38_slot_06:
    .byte $0e,$00
    .byte .LOBYTE(dpcm_sample_00>>6)
    .byte $14

; sample #$04 - dpcm_sample_00 (#$141 bytes)
; 321 byte sample
; 2,568 samples at 33.1 khz sample rate
; .078 second sample
sound_39_slot_06:
    .byte $0f,$00
    .byte .LOBYTE(dpcm_sample_00>>6)
    .byte $14

; sample #$05 - dpcm_sample_01 (#$201 bytes)
; 513 byte sample
; 4,104 samples at 33.1 kHz sample rate
; .124 second sample
sound_3a_slot_06:
    .byte $0f,$00
    .byte .LOBYTE(dpcm_sample_01>>6)
    .byte $20

; sample #$06 - dpcm_sample_02 (#$011 bytes)
; 17 byte sample
; 136 samples at 11.2 kHz sample rate
; .012 second sample
sound_3b_slot_06:
    .byte $09,$00
    .byte .LOBYTE(dpcm_sample_02>>6)
    .byte $01                        ; !(HUH) feels like it should be $04
                                     ; extra bytes after sample that aren't read

; sample #$07 - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$08 - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$09 - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$0a - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$0b - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$0c - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$0d - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$0e - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$0f - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$10 - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$11 - dpcm_sample_03 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_03>>6)
    .byte $2c

; sample #$12 - dpcm_sample_04 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_04>>6)
    .byte $2c

; sample #$13 - dpcm_sample_05 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_05>>6)
    .byte $2c

; sample #$14 - dpcm_sample_06 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_06>>6)
    .byte $2c

; sample #$15 - dpcm_sample_07 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_07>>6)
    .byte $2c

; sample #$16 - dpcm_sample_07 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_07>>6)
    .byte $2c

; sample #$17 - dpcm_sample_08 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_08>>6)
    .byte $2c

; sample #$18 - dpcm_sample_09 (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_09>>6)
    .byte $2c

; sample #$19 - dpcm_sample_0a (#$2c1 bytes)
; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_0a>>6)
    .byte $2c

; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
; sample #$1a - dpcm_sample_0b (#$2c1 bytes)
    .byte $0d,$00
    .byte .LOBYTE(dpcm_sample_0b>>6)
    .byte $2c

; 705 byte sample at 21.3 kHz sample rate
; 5,640 samples
; .26 second sample
; sample #$1b - dpcm_sample_0c (#$2c1 bytes)
    .byte $0d,$00                    ; 21.3 kHz sample rate
    .byte .LOBYTE(dpcm_sample_0c>>6) ; sample start address
    .byte $2c                        ; 705 sample length

; also used by sound_02
sound_01_slot_05:
    .incbin "assets/audio_data/sound_01_slot_05.bin"

sound_03_slot_05:
    .incbin "assets/audio_data/sound_03_slot_05.bin"

; also used by sound_05
sound_04_slot_05:
    .incbin "assets/audio_data/sound_04_slot_05.bin"

; sound effect sound part
sound_06_slot_04:
    .incbin "assets/audio_data/sound_06_slot_04.bin"

; also used by sound_07
sound_06_slot_05:
    .incbin "assets/audio_data/sound_06_slot_05.bin"

; also end of sound_06_slot_05
; mute pulse and disable sweep
sound_07_slot_04:
    .byte $ff ; sound_cmd_routine_03

sound_09_slot_04:
    .incbin "assets/audio_data/sound_09_slot_04.bin"

sound_09_slot_05:
    .incbin "assets/audio_data/sound_09_slot_05.bin"

sound_0b_slot_04:
    .incbin "assets/audio_data/sound_0b_slot_04.bin"

sound_0b_slot_05:
    .incbin "assets/audio_data/sound_0b_slot_05.bin"

sound_0c_slot_04:
    .incbin "assets/audio_data/sound_0c_slot_04.bin"

sound_0c_slot_05:
    .incbin "assets/audio_data/sound_0c_slot_05.bin"

sound_0d_slot_04:
    .incbin "assets/audio_data/sound_0d_slot_04.bin"

sound_0d_slot_05:
    .incbin "assets/audio_data/sound_0d_slot_05.bin"

sound_0e_slot_04:
    .incbin "assets/audio_data/sound_0e_slot_04.bin"

sound_0e_slot_05:
    .incbin "assets/audio_data/sound_0e_slot_05.bin"

sound_0f_slot_04:
    .incbin "assets/audio_data/sound_0f_slot_04.bin"

sound_0f_slot_05:
    .incbin "assets/audio_data/sound_0f_slot_05.bin"

sound_10_slot_04:
    .incbin "assets/audio_data/sound_10_slot_04.bin"

sound_10_slot_05:
    .incbin "assets/audio_data/sound_10_slot_05.bin"

sound_11_slot_04:
    .incbin "assets/audio_data/sound_11_slot_04.bin"

sound_11_slot_05:
    .incbin "assets/audio_data/sound_11_slot_05.bin"

sound_12_slot_05:
    .incbin "assets/audio_data/sound_12_slot_05.bin"

sound_14_slot_04:
    .incbin "assets/audio_data/sound_14_slot_04.bin"

sound_14_slot_05:
    .incbin "assets/audio_data/sound_14_slot_05.bin"

sound_15_slot_04:
    .incbin "assets/audio_data/sound_15_slot_04.bin"

sound_15_slot_05:
    .incbin "assets/audio_data/sound_15_slot_05.bin"

; also used by sound_24
sound_13_slot_04:
    .incbin "assets/audio_data/sound_13_slot_04.bin"

sound_13_slot_05:
    .byte $ff ; sound_cmd_routine_03

sound_16_slot_04:
    .incbin "assets/audio_data/sound_16_slot_04.bin"

sound_16_slot_05:
    .incbin "assets/audio_data/sound_16_slot_05.bin"

sound_17_slot_04:
    .byte $ff ; sound_cmd_routine_03

sound_18_slot_04:
    .incbin "assets/audio_data/sound_18_slot_04.bin"

sound_18_slot_05:
    .incbin "assets/audio_data/sound_18_slot_05.bin"

sound_19_slot_04:
    .incbin "assets/audio_data/sound_19_slot_04.bin"

sound_1a_slot_04:
    .incbin "assets/audio_data/sound_1a_slot_04.bin"

sound_1a_slot_05:
    .incbin "assets/audio_data/sound_1a_slot_05.bin"

sound_1b_slot_04:
    .incbin "assets/audio_data/sound_1b_slot_04.bin"

sound_1b_slot_05:
    .incbin "assets/audio_data/sound_1b_slot_05.bin"

sound_1c_slot_04:
    .byte $ff ; sound_cmd_routine_03

sound_1c_slot_05:
    .incbin "assets/audio_data/sound_1c_slot_05.bin"

sound_0a_slot_04:
    .incbin "assets/audio_data/sound_0a_slot_04.bin"

sound_0a_slot_05:
    .incbin "assets/audio_data/sound_0a_slot_05.bin"

sound_1d_slot_05:
    .incbin "assets/audio_data/sound_1d_slot_05.bin"

; also used for sound slot 05 (noise) for sound_1e
sound_1e_slot_04:
    .byte $ff ; sound_cmd_routine_03

sound_1f_slot_04:
    .incbin "assets/audio_data/sound_1f_slot_04.bin"

sound_1f_slot_05:
    .incbin "assets/audio_data/sound_1f_slot_05.bin"

sound_08_slot_04:
    .byte $ff ; sound_cmd_routine_03

sound_08_slot_05:
    .incbin "assets/audio_data/sound_08_slot_05.bin"

sound_20_slot_05:
    .incbin "assets/audio_data/sound_20_slot_05.bin"

sound_21_slot_04:
    .incbin "assets/audio_data/sound_21_slot_04.bin"

sound_22_slot_04:
    .incbin "assets/audio_data/sound_22_slot_04.bin"

sound_22_slot_05:
    .incbin "assets/audio_data/sound_22_slot_05.bin"

sound_23_slot_04:
    .incbin "assets/audio_data/sound_23_slot_04.bin"

sound_25_slot_04:
    .incbin "assets/audio_data/sound_25_slot_04.bin"

sound_25_slot_05:
    .incbin "assets/audio_data/sound_25_slot_05.bin"

sound_26_slot_01:
    .incbin "assets/audio_data/sound_26_slot_01.bin"

; also used when playing sound_26_slot_01
sound_26_slot_04:
    .incbin "assets/audio_data/sound_26_slot_04.bin"

sound_26_slot_05:
    .incbin "assets/audio_data/sound_26_slot_05.bin"

sound_27_slot_04:
    .incbin "assets/audio_data/sound_27_slot_04.bin"

.ifdef Probotector
    sound_slot_empty:
        .byte $ff
.endif

sound_28_slot_00:
    .incbin "assets/audio_data/sound_28_slot_00_0.bin"

sound_28_slot_00_1:
    .byte $fd
    .addr sound_28_slot_00_6
    .incbin "assets/audio_data/sound_28_slot_00_2.bin"
    .addr sound_28_slot_00_6
    .incbin "assets/audio_data/sound_28_slot_00_3.bin"
    .addr sound_28_slot_00_7
    .incbin "assets/audio_data/sound_28_slot_00_4.bin"
    .addr sound_28_slot_00_7
    .incbin "assets/audio_data/sound_28_slot_00_5.bin"
    .addr sound_28_slot_00_1

sound_28_slot_00_6:
    .incbin "assets/audio_data/sound_28_slot_00_6.bin"

sound_28_slot_00_7:
    .incbin "assets/audio_data/sound_28_slot_00_7.bin"

sound_28_slot_01:
    .incbin "assets/audio_data/sound_28_slot_01_0.bin"

sound_28_slot_01_1:
    .incbin "assets/audio_data/sound_28_slot_01_1.bin"
    .addr sound_28_slot_01_6
    .byte $fd
    .addr sound_28_slot_01_6
    .incbin "assets/audio_data/sound_28_slot_01_3.bin"
    .addr sound_28_slot_01_7
    .incbin "assets/audio_data/sound_28_slot_01_4.bin"
    .addr sound_28_slot_01_7
    .incbin "assets/audio_data/sound_28_slot_01_5.bin"
    .addr sound_28_slot_01_1

sound_28_slot_01_6:
    .incbin "assets/audio_data/sound_28_slot_01_6.bin"

sound_28_slot_01_7:
    .incbin "assets/audio_data/sound_28_slot_01_7.bin"

sound_28_slot_02:
    .incbin "assets/audio_data/sound_28_slot_02_0.bin"

sound_28_slot_02_1:
    .incbin "assets/audio_data/sound_28_slot_02_1.bin"
    .addr sound_28_slot_02_8
    .incbin "assets/audio_data/sound_28_slot_02_2.bin"
    .addr sound_28_slot_02_8
    .incbin "assets/audio_data/sound_28_slot_02_3.bin"
    .addr sound_28_slot_02_9
    .incbin "assets/audio_data/sound_28_slot_02_4.bin"
    .addr sound_28_slot_02_9
    .incbin "assets/audio_data/sound_28_slot_02_5.bin"
    .addr sound_28_slot_02_a
    .incbin "assets/audio_data/sound_28_slot_02_6.bin"
    .addr sound_28_slot_02_a
    .incbin "assets/audio_data/sound_28_slot_02_7.bin"
    .addr sound_28_slot_02_1

sound_28_slot_02_8:
    .incbin "assets/audio_data/sound_28_slot_02_8.bin"

sound_28_slot_02_9:
    .incbin "assets/audio_data/sound_28_slot_02_9.bin"

sound_28_slot_02_a:
    .incbin "assets/audio_data/sound_28_slot_02_a.bin"

sound_28_slot_03:
    .incbin "assets/audio_data/sound_28_slot_03_0.bin"

sound_28_slot_03_1:
    .incbin "assets/audio_data/sound_28_slot_03_1.bin"
    .addr sound_28_slot_03_8
    .incbin "assets/audio_data/sound_28_slot_03_2.bin"
    .addr sound_28_slot_03_8
    .incbin "assets/audio_data/sound_28_slot_03_3.bin"
    .addr sound_28_slot_03_9
    .incbin "assets/audio_data/sound_28_slot_03_4.bin"
    .addr sound_28_slot_03_9
    .incbin "assets/audio_data/sound_28_slot_03_5.bin"
    .addr sound_28_slot_03_a
    .incbin "assets/audio_data/sound_28_slot_03_6.bin"
    .addr sound_28_slot_03_a
    .incbin "assets/audio_data/sound_28_slot_03_7.bin"
    .addr sound_28_slot_03_1

sound_28_slot_03_8:
    .incbin "assets/audio_data/sound_28_slot_03_8.bin"

sound_28_slot_03_9:
    .incbin "assets/audio_data/sound_28_slot_03_9.bin"

sound_28_slot_03_a:
    .incbin "assets/audio_data/sound_28_slot_03_a.bin"

sound_29_slot_00:
    .incbin "assets/audio_data/sound_29_slot_00_0.bin"
    .addr sound_29_slot_00_3
    .incbin "assets/audio_data/sound_29_slot_00_1.bin"
    .addr sound_29_slot_00_3
    .incbin "assets/audio_data/sound_29_slot_00_2.bin"
    .addr sound_29_slot_00

sound_29_slot_00_3:
    .incbin "assets/audio_data/sound_29_slot_00_3.bin"

sound_29_slot_01:
    .incbin "assets/audio_data/sound_29_slot_01_0.bin"
    .addr sound_29_slot_01_3
    .incbin "assets/audio_data/sound_29_slot_01_1.bin" ; continues in bank d

; end of bank
; unused #$0 bytes out of #$2,000 bytes total (100% full)
; unused 0 bytes out of 8,192 bytes total (100% full)
bank_c_unused_space: