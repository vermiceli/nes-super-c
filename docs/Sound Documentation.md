# Overview
A song or sound can be composed of multiple channels ('instruments').  The NES
has 5 channels: 2 pulse (square wave) channels, 1 triangle channel, 1 noise
channel, and 1 delta modulation channel (dmc).  _Super C_ maintains 6 slots of
data in memory that are used in priority order to play sounds.  Higher slots are
played before the lower slots.  Each slot is linked to an NES sound channel.
The conversion from slot to NES channel happens in `ldx_channel_register`.

  * #$00 = pulse 1 channel (bgm)
  * #$01 = pulse 2 channel (bgm mostly, 1 sound effect (B OUT))
  * #$02 = triangle channel (bgm)
  * #$03 = noise and dmc channel (bgm)
  * #$04 = pulse 1 channel (for sound effects)
  * #$05 = noise channel (for sound effects)

For example, if a sound is loaded in slot #$00 and slot #$04 (both pulse 1
channel), then the sound in slot #$04 will be played since it is higher.  The
background music is filled in slots #$00 through #$03.  So the sound effects,
typically have higher priority since they are filled in slots #$01, #$04, and
#$05.

Additionally, if a sound code is already playing in a slot and another sound is
meant to be played the larger sound code is played.  So, if two sound effects
are meant to be played on slot #$04, only the sound effect with the larger sound
code is played.

When the game loads a sound to play, it first determines which slots are needed
by loading data from `sound_code_tbl`. This table specifies the number of slots
needed to play the sound and where the instructions to play the sound exists,
i.e. the 2 byte cpu address where the sound channel instructions are.

## Definitions

* Sound channel - an 'instrument' that the NES can use to make noise.  The NES
has two pulse wave channels, a triangle wave channel, a noise channel, and a DMC
(sample playback) channel.
* Sound code - a number representing a logical sound to play, either a sound
effect or background music.  Usually composed of multiple channels playing
simultaneously.
* Sound command - A single sound part has multiple sound commands, e.g. set
length, set volume, etc.
* Sound part - A single sound code has to many parts, e.g. `sound_06` as two
sound parts: `sound_06_slot_04` and `sound_06_slot_05`.  A sound part
corresponds to a single slot and a single channel.
* Sound slot - A set of memory addresses used to control playback for a channel.
_Super C_ has more slots than channels, because multiple sounds can be playing
simultaneously, and _Super C_ can prioritize channels based on which slots are
being played.

# Sound Codes

| Sound | Japanese Name | Description                        | Slot | Command Type     | Channel  |
|-------|---------------|------------------------------------|------|------------------|----------|
| #$01  |               | percussive tick                    | 5    | sound effect     | noise    |
| #$02  |               | percussive tick (same as #$01)     | 5    | sound effect     | noise    |
| #$03  |               | percussive tick                    | 5    | sound effect     | noise    |
| #$04  |               | percussive tick                    | 5    | sound effect     | noise    |
| #$05  |               | percussive tick (same as #$04)     | 5    | sound effect     | noise    |
| #$06  | SUTA          | player landing on ground           | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$07  |               | footstep                           | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$08  |               | earthquake shake                   | 4    | sound effect     | pulse 1  |
|       |               | (jungle, jagger froid, final boss) | 5    | sound effect     | noise    |
| #$09  |               | helicopter rotor noise             | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$0a  | THUNDER       | level one intro thunder            | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$0b  | SHOT          | default weapon                     | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$0c  |               | M weapon                           | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$0d  | LASER         | L weapon                           | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$0e  | SPREAD        | S weapon                           | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$0f  | FIRE          | F weapon                           | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$10  | B SHOT        |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$11  | T DAMEGE      | enemy damage                       | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$12  | HARETSU       |                                    | 5    | sound effect     | noise    |
| #$13  | T OUT         | soldier destroyed                  | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$14  | Z OUT         |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$15  | A OUT         |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$16  | ROLL          |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$17  |               | mortar round (silent)              | 4    | sound effect     | pulse 1  |
| #$18  | APPEAR        |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$19  | POWER         | pick up weapon item                | 4    | sound effect     | pulse 1  |
| #$1a  | BOSS BK       |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$1b  | BAKUHA1       |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$1c  | BAKUHA2       |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$1d  |               | level 1 storm thunder              | 5    | sound effect     | noise    |
| #$1e  |               | silent                             | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$1f  | ARUKU         |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$20  | JIWARE        |                                    | 5    | sound effect     | noise    |
| #$21  | SILEN         | siren                              | 4    | sound effect     | pulse 1  |
| #$22  |               | stomping ceiling slam              | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$23  | P 1UP         |                                    | 4    | sound effect     | pulse 1  |
| #$24  |               |                                    | 4    | sound effect     | pulse 1  |
| #$25  | P OUT         | player death                       | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$26  | B OUT         |                                    | 1    | sound effect     | pulse 2  |
|       |               |                                    | 4    | sound effect     | pulse 1  |
|       |               |                                    | 5    | sound effect     | noise    |
| #$27  |               | pause jingle                       | 4    | sound effect     | pulse 1  |
| #$28  | BGM1          | Thunder Landing (Area 1)           | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$29  | BGM4          | No Escape (Area 4)                 | 0    | background music | pulse 1  |
|       |               | Xenophobic Organs (Area 7)         | 1    | background music | pulse 2  |
|       |               | Same as #$2e                       | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$2a  | BGM3          | Jungle Juncture (Area 3)           | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$2b  | BGM2          | In A Tight Squeeze (Area 2)        | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$2c  | BGM5          | M-3 (Area 5)                       | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$2d  | BGM6          | Hotter Than Hell (Area 6)          | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$2e  |               | No Escape (Area 4)                 | 0    | background music | pulse 1  |
|       |               | Xenophobic Organs (Area 7)         | 1    | background music | pulse 2  |
|       |               | Same as #$29                       | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$2f  | BGM7          | Deathbed (Area 8)                  | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$30  | GREAT         | Great Heli - Ruined Base           | 0    | background music | pulse 1  |
|       |               | Level 1 Boss, Level 6 Boss         | 1    | background music | pulse 2  |
|       |               | Level 8 Boss                       | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$31  | BOSS BGM      | Ruined Base                        | 0    | background music | pulse 1  |
|       |               | Level 2 Boss, Level 3 Boss         | 1    | background music | pulse 2  |
|       |               | Level 4 Boss, Level 5 Boss         | 2    | background music | triangle |
|       |               | Level 7 Boss                       | 3    | background music | noise    |
| #$32  | BOSS2BGM      | Creature from Outer Space (Boss 3) | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$33  | P CLEAR       | end of level tune                  | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$34  | A CLEAR       | final boss level clear             | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$35  | OVER          | game over                          | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$36  | ENDING        | Free World (Ending)                | 0    | background music | pulse 1  |
|       |               |                                    | 1    | background music | pulse 2  |
|       |               |                                    | 2    | background music | triangle |
|       |               |                                    | 3    | background music | noise    |
| #$37  |               | drum tap sound                     | 6    |                  | dmc      |
| #$38  |               | drum tap sound                     | 6    |                  | dmc      |
| #$39  |               | drum tap sound                     | 6    |                  | dmc      |
| #$3a  |               | drum tap sound                     | 6    |                  | dmc      |
| #$3b  |               | drum tap sound                     | 6    |                  | dmc      |

# Notes

* BGM7 is incorrectly named and should be BGM8 since it is played on The Final
Stage, which is the 8th level.  There is no option to play the background music
for level 7 (Headquarters).
* Sound #$17 is played for the mortar rounds (enemy type #$29), but the sound
code is empty so the sound is silent.

As for names, I can guess at some of the abbreviations and name meanings.

  * BAKUHA - ばくはつ (爆発) - Japanese for explosion
  * BGM - background music
  * BK - BAKUHA, i.e. explosion
  * P CLEAR - player clear, or pattern clear
  * A CLEAR - all clear
  * P - player
  * ARUKU - 歩く - Japanese for to walk
  * JIWARE - じわれ (地割れ) - Japanese for cracks/fissures in the ground
  * SILEN - siren

# MEDOLEY

The hidden sound menu UI has an entry named MEDOLEY, which is a mistranslation
of medley.  This entry plays a medley of sounds in the following order, for
detailed code, see `sound_medley_00`.

* `sound_28` for #$d5c frames (57 seconds)
* `sound_30` for #$ce4 frames (55 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_2b` for #$834 frames (35 seconds)
* `sound_31` for #$5dc frames (25 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_2a` for #$b40 frames (48 seconds)
* `sound_32` for #$708 frames (30 seconds)
* `sound_31` for #$5dc frames (25 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_29` for #$bb8 frames (50 seconds)
* `sound_31` for #$5dc frames (25 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_2c` for #$834 frames (35 seconds)
* `sound_31` for #$5dc frames (25 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_2d` for #$8ac frames (37 seconds)
* `sound_32` for #$708 frames (30 seconds)
* `sound_30` for #$ce4 frames (55 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_2e` for #$bb8 frames (50 seconds)
* `sound_31` for #$5dc frames (25 seconds)
* `sound_33` for #$101 frames (4 seconds)
* `sound_2f` for #$e10 frames (60 seconds)
* `sound_30` for #$ce4 frames (55 seconds)
* `sound_34` for #$168 frames (6 seconds)
* `sound_36` for #$b62 frames (48 seconds)
* `sound_35` for #$f0 frames (4 seconds)

# sound_code Parsing

The first byte of a sound code specifies one less than the number of sound parts
in the sound, then in groups of 3 are the sound slot index and a 2 byte address
to the sound part.

## Sound Part Parsing

Sound effects and background music are parsed differently.  This section of the
document outlines how parsing works for both sound effects and background music.

### Sound Effect Commands

When byte 0 of a sound command is less than #$10, then the sound command is
considered a sound effect.  Additionally, all slot 4 sound (pulse 1) commands
are sound effect commands regardless of the value of byte 0. As released, sound
effect sound commands are only used for pulse and noise channels.  However, it
does appear that the game has some support the triangle channel in sound effects
commands.

# Sound Banks

Almost all sound data is in bank c and bank d.  However, for `sound_32` and
`sound_36`, the sound data is stored in bank 1.

| Sound                  | $8000-$9fff | $a000-$bfff |
|------------------------|-------------|-------------|
| Most Sound Data        | #$0c        | #$0d        |
| `sound_32`, `sound_36` | #$0c        | #$01        |