# Graphics Buffer

Many times, graphics data is loaded into memory and then, once per frame, that
memory is read and written to the PPU.  This is referred to as the CPU graphics
buffer.  There are 3 different encodings that are possible for each "set" of
graphics data in the CPU graphics buffer:

0.  Reset Mode (#$00)
1.  Flush Mode (#$01, #$02, #$04, #$05)
2.  Repeat Mode (#$03)
3.  Block Mode (#$06, #$07)

The mode is specified in the first byte of the buffer.  Multiple modes can be
specified in the graphics buffer at one time.

After each mode completes processing its data, the immediate next byte is the
mode byte, which if non-zero, will cause the next "set" of graphics data to
be read as one of the 3 modes above.

Note that not all data is written to the graphics buffer first.  Some data is
written directly to the PPU.  This is handled in `write_graphic_data_to_ppu` and
uses a different format.

## Example

|     | 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 0a | 0b | 0c | 0d | 0e | 0f |
|-----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 300 | 03 | 26 | b0 | 04 | 00 | 06 | 26 | d0 | 04 | 2a | 2a | 2b | 2b | 26 | f0 | 04 |
| 310 | c5 | c6 | c5 | c6 | ff | 01 | 27 | 10 | 2e | c9 | 30 | cf | ff | 00 |    |    |

This example contains x sections

1.  [Repeat Mode] Write #$00 4 times starting at PPU address $26b0
2.  [Block Mode]
    * Write #$2a, #$2a, #$2b, #$2b starting at PPU address $26d0
    * Write #$c5, #$c6, #$c5, #$c6 starting at PPU address $26f0
    * Memory address $314 being #$ff signifies end of block mode
3.  [Flush Mode] Write #$2e, #$c9, #$30, #$cf starting at PPU address $2710
4.  [Reset Mode] Reinitialize graphics buffer

## Reset Mode

When the control byte is #$00, then the entire CPU graphics buffer has been
written to the PPU and the buffer is re-initialized.

## Flush Mode

Write data until #$ff is reached.

## Repeat Mode

In repeat mode, the first 2 bytes are the PPU address to write to.  Then, byte 0
is the number of repetitions and byte 1 is the byte to write to the PPU.
Byte 2 is the mode byte.

## Block Mode

The first 2 bytes are the PPU address to write to.  Then, byte 0 indicates the
number of following bytes that will be written to the PPU.  Once finished, if
the next byte isn't #$ff, then read the next two bytes as a PPU address, and
then repeat the process by reading the next byte as block size.

# Sprites

## Player Sprites

byte 0 is number of sprites in meta-sprite
if next byte is #$80 then shared, and next two bytes specify where to move to
otherwise then next 4 bytes are relative y, byte 1 is tile code, byte 2 is attrs, and byte 3 is relative x

## Enemy Sprites

Small sprite has bit 7 set.  To get to tile index, shift left (pushing bit 7 out) and set bit 0 (use right pattern table).  See `write_small_sprite`

Normal sprite
first byte is size of sprite
then read from end to front

if byte is negative, after adding X offset, if not on right half of screen, then don't draw sprite byte (see @skip_sprite)

example
sprite byte x is #$fc (-4)
if after adding x location, carry is clear then result is still negative, i.e. off screen to left (don't draw)

each regular sprite is 4 bytes
 from right to left: relative x, sprite attribute, sprite tile, sprite y