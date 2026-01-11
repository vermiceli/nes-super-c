function Main()
    -- 0xd4/d5 in memory store the amount of time the B weapon (barrier) lasts.
    -- By continually overwriting the timer to the max value (FF), the players
    -- will always be invincible
    emu.write(0x00d4, 0xff, emu.memType.nesMemory) -- player 1
    emu.write(0x00d5, 0xff, emu.memType.nesMemory) -- player 2
end

emu.addEventCallback(Main, emu.eventType.endFrame)