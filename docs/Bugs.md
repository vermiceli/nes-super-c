# Overview

This document outlines bugs and how why they occur.

# 1. Random Enemy Generation Timer

Every level has enemies randomly generated.  For example, level 1 has random
soldiers generated and level 6 has random alien ladybugs generated.  Each level
was supposed to have a specific timer to control how fast are generated for that
level.  Due to a bug in loading the wrong index value, the timer is always set
to #$80 (128 decimal) frames.  This means that the initial value used when
calculating enemy generation delay is always #$80 before being adjusted based on
enemy difficulty and game completion.

The game is reading a value way past the intended table of timer values. This
bug actually results in every level being easier, but level 1 being only very
slightly easier.  Looking at the intended timer values, it seems the developers
wanted level 1 to be a easier than all the other levels by generating random
enemies more slowly.

* Level 1 Timer: #$7b (123 decimal)
* All Other Levels: #$3 (83 decimal)

```assembly
    lda CURRENT_LEVEL               ; !(BUG) a is immediately overwritten
                                    ; this should probably have been ldy CURRENT_LEVEL
    lda level_enemy_gen_delay_tbl,y ; y is actually PRG bank number, e.g. #$33 and not current level
                                    ; this causes the delay to always be #$80
```

# 2. Flame Weapon Split

When an F weapon flame collides with an enemy, a bug in creating the 4 or 8
split flames prevents enemy routines from executing for any enemies that have
not yet been processed in the current frame. As a result, some enemies skip
their routine execution for one frame.

The `exe_enemy_routines` method iterates through all enemies. For each enemy, it
checks whether any player bullets have collided with it. If an F weapon flame
collision is detected, the enemy loop counter at $10 (`ENEMY_CURRENT_SLOT`) is
incorrectly overwritten with 0 in `create_flame_split`.

After the current enemy finishes processing, the loop terminates because the
counter is now 0, and no remaining enemies are processed for that frame.

## Elevator Glitch

This bug causes the elevator glitch in Level 4 (Inner Base). In this glitch, the
elevator disappears and the screen scrolls upward at 4 times the normal elevator
speed (1.0 instead of 0.25).  The elevator activates once the vertical scroll
reaches a specific position.  However, if the player destroys an enemy on the
exact frame that the scroll reaches this trigger point, subsequent enemy
routines for that frame are skipped due to the bug. Because the elevator is
implemented as an enemy, its routine is also skipped. As a result, the elevator
activation code does not run, and the scroll speed is not updated.

The vertical scroll speed remains at 1.0. After one full screen of scrolling,
the scroll value wraps around, the elevator initialization finally runs, the
scroll speed is set to 0.25, and the elevator appears.

Videos demonstrating bug
  * Angrylanks: https://www.youtube.com/watch?v=9WdDyi3Ney8
  * Fulgar: https://www.youtube.com/watch?v=i4gSJQ3Mq_I

# 3. Level 2 Overhead Tank Soldier

Normally, in Level 2 (Base Area 2) you cannot advance until you destroy all the
overhead tanks on the screen.  In ono screen, there are 2 tanks and both have to
be destroyed before the player(s) can advance.  However, in 2011, David H
(@heidmand or Heidrage on twitch) shared
[a video](https://www.youtube.com/watch?v=YO3Tfee1wz8) where he only destroyed
the right tank, but was able to advance without destroying the second tank.  The
overall logic of the overhead tank was outlined in a tasvideos.org
[forum post](https://tasvideos.org/Forum/Posts/261505) by AnS, but it didn't
fully identify the exact cause of the bug.

A player is prevented from advancing until a variable `NUM_TANKS` (`$9f`) is 0.
When an overhead tank is destroyed, `NUM_TANKS` decremented.  Then, a function
is called to draw the destroyed tank background tiles.  After the tiles are
drawn, the enemy routine index is incremented so that on the next frame, the
tank begins drawing the explosion sprites.  This is all done in
`overhead_tank_soldier_routine_06`.

In `overhead_tank_soldier_routine_06`, the function to draw the background tiles
will not draw them when there is other graphics data being drawn.  When this
happens, the enemy routine index is not incremented.  This is so that the game
can retry drawing the tank destroyed tiles on the next frame.  However, on the
next frame as part of reattempting the drawing of the destroyed bank background
tiles, the `NUM_TANKS` variable is decremented again.  At this point, the player
is no longer blocked from advancing.

To execute this glitch, there must be something else being drawn to the
background.  In this case, it is the other tank's turrets being redrawn for a
recoil effect when firing the 3 bullets simultaneously.  So, if the destruction
of one tank times correctly with the firing of the other tank's 3 bullets, then
you can cause this glitch.