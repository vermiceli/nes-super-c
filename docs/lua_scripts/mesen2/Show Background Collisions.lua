function get_bg_collision_code(x, y)
    local SCANLINE_IRQ_1 = emu.read(0x045, emu.memType.nesMemory)
    local X_SCROLL = emu.read(0x0fd, emu.memType.nesMemory)
    local Y_SCROLL = emu.read(0x0fc, emu.memType.nesMemory)
    local PPUCTRL_SETTINGS = emu.read(0x0ff, emu.memType.nesMemory)

    local IRQ_Y_SCROLL = emu.read(0x0f9, emu.memType.nesMemory)
    local IRQ_X_SCROLL = emu.read(0x0fa, emu.memType.nesMemory)
    local IRQ_PPUCTRL_SETTINGS = emu.read(0x0fb, emu.memType.nesMemory)

    local SCANLINE_IRQ_3 = emu.read(0x049, emu.memType.nesMemory)
    local NT_MIRRORING = emu.read(0x026, emu.memType.nesMemory)
    local pos_x = x
    local pos_y = y
    local x_component = X_SCROLL
    local base_nametable = PPUCTRL_SETTINGS
    local vertical_component = Y_SCROLL

    -- between the 1st and 3rd scanline IRQs
    if pos_y >= SCANLINE_IRQ_1 and pos_y < SCANLINE_IRQ_3 then
        x_component = IRQ_X_SCROLL
        base_nametable = IRQ_PPUCTRL_SETTINGS
        vertical_component = IRQ_Y_SCROLL
        if IRQ_Y_SCROLL == 0xff then
            return 0x00
        end
    end

    local vertical_sum = pos_y + vertical_component
    pos_nametable = 0
    if vertical_sum >= 0xf0 then
        vertical_sum = vertical_sum + 0x10
        pos_nametable = 0x02
        vertical_sum = vertical_sum - 0x100
    end

    local offset_high_nibble = vertical_sum & 0xf0
    local x_sum = x_component + pos_x
    if x_sum >= 0x100 then
        pos_nametable = pos_nametable + 1
        x_sum = x_sum - 0x100
    end

    local offset_low_nibble = x_sum >> 4
    local bg_offset = (offset_high_nibble | offset_low_nibble) >> 1

    -- only care about the 2 nametable bits
    -- 00 (tl) ~ 00 (tl) -> 00 (tl)
    -- 00 (tl) ~ 01 (tr) -> 01 (tr)
    -- 00 (tl) ~ 10 (bl) -> 10 (bl)
    -- 00 (tl) ~ 11 (br) -> 11 (br)
    -- 01 (tr) ~ 01 (tr) -> 00 (tl)
    -- 01 (tr) ~ 10 (bl) -> 11 (br)
    -- 01 (tr) ~ 11 (br) -> 10 (bl)
    -- 10 (bl) ~ 10 (bl) -> 00 (tl)
    -- 10 (bl) ~ 11 (br) -> 01 (tr)
    -- 11 (br) ~ 11 (br) -> 00 (tl)
    -- calculate actual nametable based on base nametable from PPUCTRL_SETTINGS
    local real_nametable = pos_nametable ~ base_nametable

    -- if vertical mirroring (AB|AB) and bit 0 is set (tr or br), set bit 7 (use second collision tbl)
    -- if horizontal mirroring (AA|BB) and bit 1 is set (bl or br), set bit 7 (use second collision tbl)
    local use_second_collision_tbl = real_nametable & 0x01
    if NT_MIRRORING ~= 0 then
        real_nametable = real_nametable >> 1
        use_second_collision_tbl = real_nametable & 0x01
    end

    -- see if should use second background collision table
    if use_second_collision_tbl == 0x01 then
        bg_offset = bg_offset | 0x80
    end

    --emu.drawString(0x80, 0x90, string.format("%x", bg_offset), 0xfff, 0x000)
    local bg_collision_code = emu.read(0x0400 + bg_offset, emu.memType.nesMemory)
    if (offset_low_nibble & 0x01) == 0 then
        bg_collision_code = bg_collision_code >> 4
    else
        bg_collision_code = bg_collision_code & 0x0f
    end

    return bg_collision_code
end

function collision_code_to_color(bg_collision_code)
    local floatingColor = 0x50ff00ff
    local floorColor = 0x508fbc8f
    local waterColor = 0x500096ff
    local eggFloorColor = 0x50004f08 -- level 7 collapsible floor (eggshells)
    local solidColor = 0x50a9a9a9
    local tileColor = floorColor
    if bg_collision_code == 0x01 then
        tileColor = floatingColor
    elseif bg_collision_code == 0x02 then
        tileColor = floorColor
    elseif bg_collision_code == 0x03 then
        tileColor = eggFloorColor
    elseif bg_collision_code == 0x04 then
        tileColor = waterColor
    elseif bg_collision_code > 4 then
        tileColor = floorColor
    end
      
    return tileColor
end

function Main()
    local X_SCROLL = emu.read(0x0fd, emu.memType.nesMemory)
    local Y_SCROLL = emu.read(0x0fc, emu.memType.nesMemory)

    for i = 0, 256, 16 do
        for j = 0, 224, 16 do
            local bg_collision_code = get_bg_collision_code(i - math.fmod(X_SCROLL, 16), j - math.fmod(Y_SCROLL, 16))
            if bg_collision_code ~= 0 then
                local tile_color = collision_code_to_color(bg_collision_code)
                emu.drawRectangle(i, j, 16, 16, tile_color, true)
                --emu.drawString(i, j, string.format("%x", bg_collision_code))
            end
        end
    end
end

emu.addEventCallback(Main, emu.eventType.endFrame)
