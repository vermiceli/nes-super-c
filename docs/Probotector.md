# Overview
This file outlines some of the differences between _Super C_ (US) and
_Probotector II: Return of the Evil Forces_.  _Probotector_ is the PAL-specific
variation of the _Super C_ (US) game.  Note that all of the documentation that
refers to code or look up table memory addresses were documented using _Super C_
(US) and may not be identical to the addresses in _Probotector_.

# Differences

## Logic

1. The cheat to get 10 lives in _Super C_ gives 30 lives in _Probotector_.
2. End of level animation timing for player auto-move is slightly different for
  level 3 (Jungle), level 7 (Headquarters), and level 8 (The Final Stage).
3. There are always 2 scanline interrupts that don't exist in _Super C_.  They
  set and reset the character banks before and after the visible screen.

## Enemies

1.  The Level 7 (Headquarters) soldier uses different sprites, and animation
  sequence.

## Audio

The sound engine was updated between _Super C_ and _Probotector_. Optimizations
were made to remove CPU wait commands between APU writes.  Sounds effects were
encoded differently due to the updated sound engine.  Background music encodings
were not changed.

## Background

Technically Level 2 (Base Area 2) has a different layout in _Probotector_.
Instead of having a screen layout of 2x13, _Probotector_ has 4x13.  However,
the screens are unreachable.

## Palettes

In general many of the palettes have been adjusted throughout the game.

## Text

Y.SAKAKURA is only credited in the Probotector release on the ending credits and
not in _Super C_.

## Sprites

Many of the sprites were changed to replace humans with robots.

## Graphics

The level 1 (Fort Firestorm) intro helicopter is changed.
The level 1 (Fort Firestorm) boss Helicopter is changed.