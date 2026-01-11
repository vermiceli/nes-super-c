-- used for testing to show player data

function Main()
    local PLAYER_COUNT = emu.read(0x20, emu.memType.nesMemory)
    for i = 0,PLAYER_COUNT do
        local PLAYER_X_POS = emu.read(0xcc + i, emu.memType.nesMemory)
        local PLAYER_Y_POS = emu.read(0xce + i, emu.memType.nesMemory)
        local PLAYER_CURRENT_WEAPON = emu.read(0xb8 + i, emu.memType.nesMemory)
        local PLAYER_AIM_DIR = emu.read(0xba + i, emu.memType.nesMemory)
        local PLAYER_RECOIL_TIMER = emu.read(0xbc + i, emu.memType.nesMemory)
        local PLAYER_M_WEAPON_FIRE_TIME = emu.read(0xbe + i, emu.memType.nesMemory)
        local PLAYER_STATE_TIMER = emu.read(0xc0 + i, emu.memType.nesMemory)
        local PLAYER_SKIP_COLLISION = emu.read(0xc2 + i, emu.memType.nesMemory)
        local NEW_LIFE_INVINCIBILITY_TIMER = emu.read(0xc4 + i, emu.memType.nesMemory)
        local PLAYER_ACTION_STATE = emu.read(0xc6 + i, emu.memType.nesMemory)
        local F_WEAPON_CHARGE = emu.read(0xc8 + i, emu.memType.nesMemory)
        local PLAYER_GAME_OVER_STATUS = emu.read(0xca + i, emu.memType.nesMemory)
        local PLAYER_OVERHEAD_DIR = emu.read(0xd0 + i, emu.memType.nesMemory)
        local PLAYER_AUTO_MOVE_CHECKPOINT = emu.read(0xd2 + i, emu.memType.nesMemory)
        local INVINCIBILITY_TIMER = emu.read(0xd4 + i, emu.memType.nesMemory)
        local BOSS_DEFEATED_PLAYER_ANIM_SOUND = emu.read(0xd6 + i, emu.memType.nesMemory)
        local PLAYER_SPRITE = emu.read(0x0518 + i, emu.memType.nesMemory)
        local PLAYER_SPRITE_X_POS = emu.read(0x054c + i, emu.memType.nesMemory)
        local PLAYER_SPRITE_Y_POS = emu.read(0x0532 + i, emu.memType.nesMemory)
        local PLAYER_SPRITE_ATTR = emu.read(0x0566 + i, emu.memType.nesMemory)

          -- change variable to interested variable for studying
        emu.drawString(PLAYER_X_POS, PLAYER_Y_POS, string.format("%x-%x", PLAYER_X_POS, PLAYER_Y_POS))
    end
end

emu.addEventCallback(Main, emu.eventType.endFrame)