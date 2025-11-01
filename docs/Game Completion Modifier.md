# Overview

_Super C_ is more difficult after beating the game when replaying the levels.
Unlike _Contra_ which gets more and more difficult the more times you beat the
game, _Super C_ only keeps track of whether or not the game has been beaten
once.  Subsequent completions of the game have no effect on increased
difficultly.  This documents the ways in which beating the game affects the
behavior of subsequent playthroughs.

The game keeps track of whether or not the game has been beaten in the variable
`GAME_COMPLETED`, located at CPU memory address `$51`.

# Enemy Generation

## Delay

After beating the game, random enemies generate #$10 frames faster before.

```
ENEMY_GEN_DELAY = #$80 - (#$10 * GAME_COMPLETED) - (#$04 * ENEMY_DIFFICULTY)
```

ENEMY_DIFFICULTY is the number of players with the spread (S) weapon.

## Edge Check

Before beating the game, _Super C_ will not generate enemies if a player is
close to edge of the screen where the enemy would be generated.  This way the
player has a chance to react.  _Super C_ will start generating enemies when the
player is close to the edge if the player stays on the same screen for a long
time.  This is to prevent a player from staying still to rack up a score.

After beating the game, _Super C_ will not perform this edge check and enemies
will generate even when the player is close to the edge.

# Level 1 Wall Cannon

In Level 1 (Fort Firestorm), the first wall cannon doesn't fire.  After
completing the game it does.