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

# 2. Elevator Glitch

# 3. Level 2 Overhead Tank Soldier

Normally, in Level 2 (Base Area 2) you cannot advance until you destroy the
overhead tanks on the screen.  In on screen, there are 2 tanks and both have to
be destroyed before the player(s) can advance.  However, in 2011, David H
(@heidmand or Heidrage on twitch) shared
[a video](https://www.youtube.com/watch?v=YO3Tfee1wz8) where he only destroyed
the right tank, but is able to advance without destroying the second tank.  The
overall logic of the overhead tank was outlined in a tasvideos.org
[forum post](https://tasvideos.org/Forum/Posts/261505) by AnS, but it doesn't
fully identify the exact cause of the bug.

A player is prevented from advancing until a variable `NUM_TANKS` (`$9f`) is 0.
When an overhead tank is destroyed, `NUM_TANKS` decremented.  Then, a function]
is called to draw the destroyed tank background tiles.  After the tiles are
drawn, the enemy routine index is incremented so that on the next frame, the
tank begins drawing the explosion sprites.  This is all done in
`overhead_tank_soldier_routine_06`.

In `overhead_tank_soldier_routine_06`, the function to draw the background tiles
will not draw them when there is other data being drawn.  When this happens,
the enemy routine index is not incremented.  This is so that the game can retry
drawing the tank destroyed tiles on the next frame.  However, on the next frame
as part of reattempting the drawing of the destroyed bank background tiles, the
`NUM_TANKS` variable is decremented again.  At this point, the player is no
longer blocked from advancing.

To execute this glitch, there must be something else being drawn to the
background.  In this case, it is the other tank's turrets being redrawn for a
recoil effect when firing the 3 bullets simultaneously.  So, if the destruction
of one tank times correctly with the firing of the other tank's 3 bullets, then
you can cause this glitch.