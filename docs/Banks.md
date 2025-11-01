# Banks

The game has 16, 8 KiB banks of program ROM (PRG ROM) and 128 KiB of character
ROM (CHR ROM) for a total size of 256 KiB.  MMC3 mappers support partitioning
the character ROM into different sizes to swap out part of the pattern table
tiles at a time.  _Super C_ splits the left pattern table into 2 2 KiB halves
and splits the right pattern table into 4 1 KiB quarters.

## Level Banks

Each level uses different banks for their enemies and graphics.  As part of
running the level, when necessary, PRG banks are swapped in and out.  This table
shows which banks each level uses.

For reference, this data is in the game in the following locaitons:
  * `level_bank_tbl`
  * `level_graphic_bank_tbl`

| Level   | Name            | $8000-$9fff PRG Bank | $a000-$bfff PRG Bank | $8000-$9fff Graphic Data | $a000-$bfff Graphic Data |
|---------|-----------------|----------------------|----------------------|--------------------------|--------------------------|
| Level 1 | Fort Firestorm  | #$02                 | #$03                 | #$04                     | #$05                     |
| Level 2 | First Base      | #$0a                 | #$03                 | #$00                     | #$01                     |
| Level 3 | Jungle          | #$02                 | #$03                 | #$06                     | #$07                     |
| Level 4 | Inner Base      | #$0a                 | #$03                 | #$06                     | #$07                     |
| Level 5 | The Cliff       | #$08                 | #$03                 | #$08                     | #$09                     |
| Level 6 | Entry to HQ     | #$08                 | #$03                 | #$08                     | #$09                     |
| Level 7 | Headquarters    | #$04                 | #$03                 | #$0a                     | #$0b                     |
| Level 8 | The Final Stage | #$04                 | #$03                 | #$0a                     | #$0b                     |

## CHR ROM Banks

The MMC3 mapper supports splitting the character rom data (pattern table).
There are multiple configurations available, but _Super C_ splits the left
pattern table in half and the right pattern table into quarters.  There are 128
1 KiB character rom (CHR ROM) banks (#$80).  The left pattern table tiles are
used for background tiles and the right pattern table tiles are used for 8x16
sprites.

Below is the list of CHR ROM banks used and when they are changed.  For the list
of banks used at the beginning of each level, see
`level_pattern_tile_banks_tbl`.

| Level   | Transition Point    | L1   | L2   | R1   | R2   | R3   | R4   |
|---------|---------------------|------|------|------|------|------|------|
| Intro   |                     | #$40 | #$6a | #$44 | #$45 | #$46 | #$07 |
| 1       | Helicopter Drop     | #$00 | #$02 | #$44 | #$45 | #$46 | #$47 |
| 1       |                     | #$38 | #$02 | #$44 | #$45 | #$46 | #$47 |
| 1       | First Hill          | #$38 | #$3a | #$44 | #$45 | #$46 | #$47 |
| 1       | Boss Screen         | #$04 | #$06 | #$44 | #$45 | #$46 | #$47 |
| 1       | Boss Screen (irq 1) | #$38 | #$3a | #$44 | #$45 | #$46 | #$47 |
| 1       | End Level Walk      | #$38 | #$3a | #$44 | #$45 | #$46 | #$47 |
| 2       |                     | #$3c | #$3e | #$49 | #$4a | #$4b | #$1a |
| 2       | First Open Door     | #$5c | #$5e | #$49 | #$4a | #$4b | #$1a |
| 2       | Approach Boss Door  | #$5c | #$32 | #$49 | #$4a | #$4b | #$1a |
| 3       |                     | #$0c | #$0e | #$44 | #$45 | #$46 | #$48 |
| 3       | Sandbag Sniper      | #$0c | #$10 | #$44 | #$45 | #$46 | #$48 |
| 3       | Boss Spider         | #$12 | #$14 | #$44 | #$45 | #$46 | #$48 |
| 3       | Boss Spider Defeat  | #$12 | #$10 | #$44 | #$45 | #$46 | #$48 |
| 3       | Level Boss          | #$16 | #$18 | #$44 | #$45 | #$46 | #$1a |
| 4       |                     | #$08 | #$0a | #$44 | #$45 | #$46 | #$47 |
| 4       | Up Elevator Shaft   | #$08 | #$0a | #$44 | #$45 | #$46 | #$1b |
| 4       | Elevator (irq 1)    | #$42 | #$42 | #$44 | #$45 | #$46 | #$1b |
| 4       | Boss (irq 1)        | #$08 | #$14 | #$44 | #$45 | #$46 | #$1b |
| 4       | Boss (irq 3)        | #$08 | #$0a | #$44 | #$45 | #$46 | #$1b |
| 5       |                     | #$1c | #$1e | #$44 | #$45 | #$46 | #$60 |
| 5       | Top of Cliff        | #$20 | #$22 | #$44 | #$45 | #$46 | #$60 |
| 5       | Boss Screen         | #$5a | #$06 | #$44 | #$45 | #$46 | #$60 |
| 5       | Boss Screen (irq)   | #$20 | #$22 | #$44 | #$45 | #$46 | #$60 |
| 5       | Boss Defeated       | #$20 | #$22 | #$44 | #$45 | #$46 | #$60 |
| 6       |                     | #$4c | #$4e | #$49 | #$4a | #$61 | #$62 |
| 6       | Blue Ground         | #$4c | #$52 | #$49 | #$4a | #$61 | #$62 |
| 6       | Screen before Gate  | #$50 | #$52 | #$49 | #$4a | #$61 | #$62 |
| 6       | Mini-boss           | #$54 | #$56 | #$49 | #$4a | #$61 | #$62 |
| 6       | Mini-boss Defeated  | #$54 | #$58 | #$49 | #$4a | #$61 | #$11 |
| 7       |                     | #$24 | #$26 | #$44 | #$45 | #$63 | #$64 |
| 7       | Last Manooki        | #$28 | #$26 | #$44 | #$45 | #$63 | #$64 |
| 7       | Bubble Pit Bottom   | #$28 | #$2a | #$44 | #$45 | #$63 | #$65 |
| 7       | Boss Screen         | #$2c | #$2e | #$44 | #$45 | #$66 | #$65 |
| 7       | Boss Screen (irq)   | #$30 | #$2e | #$44 | #$45 | #$66 | #$65 |
| 8       |                     | #$34 | #$36 | #$44 | #$45 | #$67 | #$68 |
| 8       | Stomping Ceiling    | #$6c | #$6e | #$44 | #$45 | #$67 | #$68 |
| 8       | Final Boss (irq)    | #$70 | #$72 | #$44 | #$45 | #$67 | #$68 |
| Credits |                     | #$40 | #$18 | #$44 | #$45 | #$46 | #$69 |

To calculate where in the ROM file the pattern tiles are based on the bank
number in the table above, use the following formula.

```
rom_address = bank_number * 0x400 + 0x20010
```

For example, CHR ROM bank #$00 is $20010, bank #$38 is $2e010, etc.