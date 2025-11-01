-- LUA script to be run by Mesen or Mesen2 which shows the sprite collision
-- boxes.  The These are used for collision detection for player to enemy (and
-- enemy bullet) collisions as well as collisions between player bullets and
-- enemies.
--
-- Player to enemy collision boxes are green, player bullet to enemy collision
-- boxes are blue.  When a collision is detected, the box will be red.

-- converts any number greater than 127 to its negative representation
local function signed(val)
  if val > 0x7F then
      val = (256 - val) * -1
  end

  return val
end

-- draws player to enemy collision boxes in green (red for collision)
-- draws player bullet to enemy collision boxes in blue (red for collision)
function drawCollisionBoxes()
  for i = 0, 0xd do -- enemy loop
      local ENEMY_SPRITE = emu.read(0x0508 + i, emu.memType.cpu)
      local ENEMY_TYPE = emu.read(0x06d8 + i, emu.memType.cpu)
      local ENEMY_COLLISION_INDEX = emu.read(0x73a + i, emu.memType.cpu)
      local ENEMY_X_POS = emu.read(0x053c + i, emu.memType.cpu)
      local ENEMY_Y_POS = emu.read(0x0522 + i, emu.memType.cpu)
      if ENEMY_TYPE ~= 0x7f and ENEMY_SPRITE ~= 0 then
          for j = 0, 2 do -- player loop
              local PLAYER_ACTION_STATE = emu.read(0xc6 + j, emu.memType.cpu)
              local PLAYER_X_POS = emu.read(0x054c + j, emu.memType.cpu)
              local PLAYER_Y_POS = emu.read(0x0532 + j, emu.memType.cpu)

              if PLAYER_ACTION_STATE ~= 0 then
                  local COLLISION_TBL_OFFSET = (PLAYER_ACTION_STATE + 1 + ENEMY_COLLISION_INDEX) % 0x100
                  local X_OFFSET = signed(emu.read(0x6000 + COLLISION_TBL_OFFSET + 1, emu.memType.prgRom)) + 1
                  local COLLISION_WIDTH = emu.read(0x6000 + COLLISION_TBL_OFFSET + 3, emu.memType.prgRom)
                  local Y_OFFSET = signed(emu.read(0x6000 + COLLISION_TBL_OFFSET, emu.memType.prgRom)) + 1
                  local COLLISION_HEIGHT = emu.read(0x6000 + COLLISION_TBL_OFFSET + 2, emu.memType.prgRom)

                  local color = 0x00ff00 -- green
                  if
                      PLAYER_X_POS >= (ENEMY_X_POS + X_OFFSET) and
                      PLAYER_X_POS <= (ENEMY_X_POS + X_OFFSET + COLLISION_WIDTH) and
                      PLAYER_Y_POS >= (ENEMY_Y_POS + Y_OFFSET) and
                      PLAYER_Y_POS <= (ENEMY_Y_POS + Y_OFFSET + COLLISION_HEIGHT)
                   then
                      -- collision
                      color = 0xff0000
                  end

                  emu.drawRectangle(
                      ENEMY_X_POS + X_OFFSET, -- x
                      ENEMY_Y_POS + Y_OFFSET, -- y
                      COLLISION_WIDTH, -- width
                      COLLISION_HEIGHT, -- height
                      color, -- color
                      false -- fill
                  )
              end
          end

          local PLAYER_BULLET_COLLISION_CODE = 0x9f -- 0x638,j
          local BULLET_COLLISION_TBL_OFFSET = PLAYER_BULLET_COLLISION_CODE + ENEMY_COLLISION_INDEX + 1
          local BULLET_X_OFFSET = signed(emu.read(0x6000 + BULLET_COLLISION_TBL_OFFSET + 1, emu.memType.prgRom)) + 1
          local BULLET_COLLISION_WIDTH = emu.read(0x6000 + BULLET_COLLISION_TBL_OFFSET + 3, emu.memType.prgRom)
          local BULLET_Y_OFFSET = signed(emu.read(0x6000 + BULLET_COLLISION_TBL_OFFSET, emu.memType.prgRom)) + 1
          local BULLET_COLLISION_HEIGHT = emu.read(0x6000 + BULLET_COLLISION_TBL_OFFSET + 2, emu.memType.prgRom)
          local color = 0x0000ff

          for j = 0, 16 do
              local PLAYER_BULLET_X_POS = emu.read(0x0588 + j, emu.memType.cpu)
              local PLAYER_BULLET_Y_POS = emu.read(0x0578 + j, emu.memType.cpu)
              local PLAYER_BULLET_COLLISION_CODE = emu.read(0x0638 + j, emu.memType.cpu)
              if
                PLAYER_BULLET_X_POS >= (ENEMY_X_POS + BULLET_X_OFFSET) and
                PLAYER_BULLET_X_POS <= (ENEMY_X_POS + BULLET_X_OFFSET + BULLET_COLLISION_WIDTH) and
                PLAYER_BULLET_Y_POS >= (ENEMY_Y_POS + BULLET_Y_OFFSET) and
                PLAYER_BULLET_Y_POS <= (ENEMY_Y_POS + BULLET_Y_OFFSET + BULLET_COLLISION_HEIGHT)
               then
                  -- collision
                  color = 0xff0000 -- red
              end
          end

          emu.drawRectangle(
              ENEMY_X_POS + BULLET_X_OFFSET, -- x
              ENEMY_Y_POS + BULLET_Y_OFFSET, -- y
              BULLET_COLLISION_WIDTH, -- width
              BULLET_COLLISION_HEIGHT, -- height
              color, -- color
              false -- fill
          )
      end
  end
end

emu.addEventCallback(drawCollisionBoxes, emu.eventType.endFrame)