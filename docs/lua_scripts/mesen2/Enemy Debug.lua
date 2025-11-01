-- used for testing to show enemy data

function Main()
    for i = 0,0xd do
        local ENEMY_SPRITES = emu.read(0x0508 + i, emu.memType.nesMemory)
        local ENEMY_Y_POS = emu.read(0x0522 + i, emu.memType.nesMemory)
        local ENEMY_X_POS = emu.read(0x053c + i, emu.memType.nesMemory)
        local ENEMY_SPRITE_ATTR = emu.read(0x0556 + i, emu.memType.nesMemory)
        local ENEMY_ROUTINE = emu.read(0x0668 + i, emu.memType.nesMemory)
        local ENEMY_HP = emu.read(0x0676 + i, emu.memType.nesMemory)
        local ENEMY_Y_VELOCITY_FAST = emu.read(0x06ae + i, emu.memType.nesMemory)
        local ENEMY_Y_VELOCITY_FRACT = emu.read(0x06a0 + i, emu.memType.nesMemory)
        local ENEMY_X_VELOCITY_FRACT = emu.read(0x06bc + i, emu.memType.nesMemory)
        local ENEMY_X_VELOCITY_FAST = emu.read(0x06ca + i, emu.memType.nesMemory)
        local ENEMY_TYPE = emu.read(0x06d8 + i, emu.memType.nesMemory)
        local ENEMY_DELAY = emu.read(0x06e6 + i, emu.memType.nesMemory)
        local ENEMY_FIRING_DELAY = emu.read(0x06f4 + i, emu.memType.nesMemory)
        local ENEMY_ANIMATION_DELAY = emu.read(0x702 + i, emu.memType.nesMemory)
        local ENEMY_FRAME = emu.read(0x0710 + i, emu.memType.nesMemory)
        local ENEMY_ATTRIBUTES = emu.read(0x071e + i, emu.memType.nesMemory)
        local ENEMY_DESTROY_ATTRS = emu.read(0x072c + i, emu.memType.nesMemory)
        local ENEMY_COLLISION_INDEX = emu.read(0x073a + i, emu.memType.nesMemory)
        local ENEMY_VAR_1 = emu.read(0x0748 + i, emu.memType.nesMemory)
        local ENEMY_VAR_2 = emu.read(0x0756 + i, emu.memType.nesMemory)
        local ENEMY_VAR_3 = emu.read(0x0764 + i, emu.memType.nesMemory)
        local ENEMY_VAR_4 = emu.read(0x0772 + i, emu.memType.nesMemory)
        local ENEMY_VAR_5 = emu.read(0x0780 + i, emu.memType.nesMemory)
        local ENEMY_VAR_6 = emu.read(0x078e + i, emu.memType.nesMemory)
        local ENEMY_VAR_7 = emu.read(0x079c + i, emu.memType.nesMemory)

        if ENEMY_TYPE ~= 0x7f then
          -- change variable to interested variable for studying
          emu.drawString(ENEMY_X_POS, ENEMY_Y_POS, string.format("%x:%x-%x", ENEMY_TYPE, ENEMY_X_POS, ENEMY_Y_POS))
        end
    end
end

emu.addEventCallback(Main, emu.eventType.endFrame)