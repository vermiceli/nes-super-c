# Overview

This document outlines the bugs discovered in the code during the disassembly
process.

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