Below is a detailed overview of the layout of the _Super C_ ROM file's contents.

# PRG ROM Map

In summary, the PRG ROM contains the following things

| Type                    | Percentage |  Bytes |
|-------------------------|------------|--------|
| Code                    |     42.67% | 55,930 |
| Graphic Data            |     26.96% | 35,337 |
| Compressed Audio        |     17.00% | 22,280 |
| Unused                  |      6.51% |  8,539 |
| Sprite Data             |      2.67% |  4,516 |
| Configuration Data      |      3.45% |  4,106 |
| Text Data               |       .29% |    380 |

_Super C_ has 16 8 KiB PRG ROM banks.  To convert to PRG ROM address from ROM
file address, subtract #$0f from the ROM file address.  This removes the the
iNES ROM Header.

* Header
  * ROM File Address: `$00000-$0000f` - iNES ROM Header Data
* PRG Bank #$00
  * ROM File Address: `$00010-$01410` - Player Routines
  * ROM File Address: `$01411-$0178b` - Enemy Generation
  * ROM File Address: `$0178c-$017e9` - Game Engine Graphics Writing
  * ROM File Address: `$017ea-$0189d` - Game Text
  * ROM File Address: `$0189e-$01914` - Game Engine Graphics Writing
  * ROM File Address: `$01915-$01c01` - Blank, Intro, and End Credits Graphics
  * ROM File Address: `$01c02-$01da3` - Initialize Level Logic
  * ROM File Address: `$01da4-$0200f` - Attract Mode Player Auto Movement
* PRG Bank #$01
  * ROM File Address: `$02010-$02177` - Attract Mode Player Auto Movement
  * ROM File Address: `$02178-$022ab` - End Level Auto Move
  * ROM File Address: `$022ac-$022d3` - Cheat Code Check
  * ROM File Address: `$022d4-$02319` - Unused Level Select
  * ROM File Address: `$0231a-$023c6` - Intro Screen Animations
  * ROM File Address: `$023c7-$0254a` - Scroll Credits Routines
  * ROM File Address: `$0254b-$02612` - End Credits Text
  * ROM File Address: `$02613-$0263e` - Scroll Credits Routines
  * ROM File Address: `$0263f-$02b54` - Encoded Sound Data
  * ROM File Address: `$02b55-$02f4e` - Level Configuration Data
  * ROM File Address: `$02f4f-$03f9a` - Level Graphics Data
  * ROM File Address: `$03f9b-$0400e` - Unused
  * ROM File Address: `$0400f-$0400f` - Bank Byte
* PRG Bank #$02
  * ROM File Address: `$04010-$05d5d` - Level 1 and Level 3 Enemy Routines
  * ROM File Address: `$05d5e-$0600f` - Unused
* PRG Bank #$03
  * ROM File Address: `$06010-$06232` - Foreground Collision Detection Logic
  * ROM File Address: `$06233-$07d35` - Enemy Logic
  * ROM File Address: `$07d36-$0800e` - Unused
  * ROM File Address: `$0800f-$0800f` - Bank Byte
* PRG Bank #$04
  * ROM File Address: `$08010-$08010` - Bank Byte
  * ROM File Address: `$08011-$092d8` - Enemy Logic
  * ROM File Address: `$092d9-$09ae8` - Level Enemy Screen Data
  * ROM File Address: `$09ae9-$09fc7` - Hidden Sound Menu
  * ROM File Address: `$09fc8-$0a00f` - Level 1 Graphics Data
* PRG Bank #$05
  * ROM File Address: `$0a010-$0b523` - Level 1 Graphics Data
  * ROM File Address: `$0b524-$0c00e` - Unused
  * ROM File Address: `$0c00f-$0c00f` - Bank Byte
* PRG Bank #$06
  * ROM File Address: `$0c010-$0c010` - Bank Byte
  * ROM File Address: `$0c011-$0c214` - Sprite Logic
  * ROM File Address: `$0c215-$0cf3e` - Encoded Enemy Sprite Data
  * ROM File Address: `$0cf3f-$0d3b8` - Encoded Player Sprite Data
  * ROM File Address: `$0d3b9-$0da86` - Level Routine Logic
  * ROM File Address: `$0da87-$0e00f` - Level 4 Graphics Data
* PRG Bank #$07
  * ROM File Address: `$0e010-$0eb79` - Level 4 Graphics Data
  * ROM File Address: `$0eb7a-$0f893` - Level 3 Graphics Data
  * ROM File Address: `$0f894-$0f8c2` - Palette Logic
  * ROM File Address: `$0f8c3-$0f9e0` - Palette Data
  * ROM File Address: `$0f9e1-$1000e` - Unused
  * ROM File Address: `$1000f-$1000f` - Bank Byte
* PRG Bank #$08
  * ROM File Address: `$10010-$10010` - Bank Byte
  * ROM File Address: `$10011-$11df0` - Level 5 and Level 6 Enemy Routines
  * ROM File Address: `$11df1-$1200f` - Level 5 Graphics Data
* PRG Bank #$09
  * ROM File Address: `$12010-$12d35` - Level 5 Graphics Data
  * ROM File Address: `$12d36-$13a39` - Level 6 Graphics Data
  * ROM File Address: `$13a3a-$1400e` - Unused
  * ROM File Address: `$1400f-$1400f` - Bank Byte
* PRG Bank #$0a
  * ROM File Address: `$14010-$14010` - Bank Byte
  * ROM File Address: `$14011-$1585a` - Level 2 and Level 4 Enemy Routines
  * ROM File Address: `$1585b-$1600f` - Level 7 Graphics Data
* PRG Bank #$0b
  * ROM File Address: `$16010-$16826` - Level 7 Graphics Data
  * ROM File Address: `$16827-$17da6` - Level 8 Graphics Data
  * ROM File Address: `$17da7-$1800e` - Unused
  * ROM File Address: `$1800f-$1800f` - Bank Byte
* PRG Bank #$0c
  * ROM File Address: `$18010-$18010` - Bank Byte
  * ROM File Address: `$18011-$18cef` - Sound Engine
  * ROM File Address: `$18cf0-$18d37` - Sound Pitch Data
  * ROM File Address: `$18d38-$18d42` - Sound Engine
  * ROM File Address: `$18d43-$1a00f` - Encoded Sound Data
* PRG Bank #$0d
  * ROM File Address: `$1a010-$1bfec` - Encoded Sound Data
  * ROM File Address: `$1bfed-$1c00e` - Unused
  * ROM File Address: `$1c00f-$1c00f` - Bank Byte
* PRG Bank #$0e
  * ROM File Address: `$1c010-$1df0f` - DPCM Sample Data
  * ROM File Address: `$1df10-$1e00f` - Unused
* PRG Bank #$0f
  * ROM File Address: `$1e010-$1e08e` - Write Buffers to PPU
  * ROM File Address: `$1e08f-$1e3c8` - IRQ Handling Logic
  * ROM File Address: `$1e3c9-$1f98b` - Game Engine Logic
  * ROM File Address: `$1f98c-$1fa3f` - Palette Table Data
  * ROM File Address: `$1fa40-$1fb76` - Game Engine Logic
  * ROM File Address: `$1fb77-$1fbfb` - NMI
  * ROM File Address: `$1fbfc-$1ff61` - Game Engine Logic
  * ROM File Address: `$1ff62-$1ffff` - Unused
  * ROM File Address: `$20000-$20009` - Assembly Date Data
  * ROM File Address: `$2000a-$2000f` - Vector Address Data

## Visual PRG ROM Map

```
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTCGGGGGGCCCDDDDD
DDDCCUCCCCTTAAAAAAAAAADDDDDDDDGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGU
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCUUUUU
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCUUUUUU
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDDCCCCCCCCCC
GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGUUUUUUUUUUUUUUUUUUUUU
UCCCCSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSCCCCCCCCCCCCCCGGGGGGGGGG
GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGUUUUUUUUUUUU
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCGGGG
GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGUUUUUUUUUUUU
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCGGGGGGGGGGGGGGG
GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGUUUUU
CCCCCCCCCCCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU
UCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCGCCCCCCCCCCU
```

Legend
* G = Graphic Data
* C = Code
* A = Encoded Audio
* U = Unused
* D = Configuration Data
* S = Sprite Data
* T = Text Data

# CHR ROM Map

There is 128 KiB of CHR ROM data.  The left pattern table tile banks (background
tiles) are referenced as 2 KiB banks.  Each 2 KiB bank is half of a pattern
table.  The right pattern table tile banks (sprites) are referenced as 1 KiB
banks.

To easily see which banks are used by which levels and when, see
[Banks.md](./Banks.md).

  * 2 KiB CHR ROM Bank #$00 - Level 1 Helicopter Background Tiles
    * ROM File Address: `$20010-2080f`
    * CHR ROM Address: `$00000-007ff`
  * 2 KiB CHR ROM Bank #$02 - Level 1 Background Tiles
    * ROM File Address: `$20810-2100f`
    * CHR ROM Address: `$00800-00fff`
  * 2 KiB CHR ROM Bank #$04 - Level 1 Boss Background Tiles
    * ROM File Address: `$21010-2180f`
    * CHR ROM Address: `$01000-017ff`
  * 2 KiB CHR ROM Bank #$06 - Level 1 Boss, Level 5 Boss Background Tiles
    * ROM File Address: `$21810-2200f`
    * CHR ROM Address: `$01800-01fff`
    * 1 KiB CHR ROM Bank #$07 is also referenced in intro for showing logo
  * 2 KiB CHR ROM Bank #$08 - Level 4 Background Tiles
    * ROM File Address: `$22010-2280f`
    * CHR ROM Address: `$02000-027ff`
  * 2 KiB CHR ROM Bank #$0a - Level 4 Background Tiles
    * ROM File Address: `$22810-2300f`
    * CHR ROM Address: `$02800-02fff`
  * 2 KiB CHR ROM Bank #$0c - Level 3 Background Tiles
    * ROM File Address: `$23010-2380f`
    * CHR ROM Address: `$03000-037ff`
  * 2 KiB CHR ROM Bank #$0e - Level 3 Background Tiles
    * ROM File Address: `$23810-2400f`
    * CHR ROM Address: `$03800-03fff`
  * 2 KiB CHR ROM Bank #$10 - Level 3 After Spider Background Tiles
    * ROM File Address: `$24010-2480f`
    * CHR ROM Address: `$04000-047ff`
    * 1 KiB CHR ROM Bank #$11 is also referenced in level 6 mini-boss
  * 2 KiB CHR ROM Bank #$12 - Level 3 Boss Spider Background Tiles
    * ROM File Address: `$24810-2500f`
    * CHR ROM Address: `$04800-04fff`
  * 2 KiB CHR ROM Bank #$14 - Level 3, Level 4 Background Tiles
    * ROM File Address: `$25010-2580f`
    * CHR ROM Address: `$05000-057ff`
  * 2 KiB CHR ROM Bank #$16 - Level 3 Boss Background Tiles
    * ROM File Address: `$25810-2600f`
    * CHR ROM Address: `$05800-05fff`
  * 2 KiB CHR ROM Bank #$18 - Level 3 Boss, and End Credits Background Tiles
    * ROM File Address: `$26010-2680f`
    * CHR ROM Address: `$06000-067ff`
  * 1 KiB CHR ROM Bank #$1a - Level 2, Level 3 Sprites
    * ROM File Address: `$26810-26c0f`
    * CHR ROM Address: `$06800-06bff`
  * 1 KiB CHR ROM Bank #$1b - Level 4 Elevator Background Tiles
    * ROM File Address: `$26c10-2700f`
    * CHR ROM Address: `$06c00-06fff`
  * 2 KiB CHR ROM Bank #$1c - Level 5 Background Tiles
    * ROM File Address: `$27010-2780f`
    * CHR ROM Address: `$07000-077ff`
  * 2 KiB CHR ROM Bank #$1e - Level 5 Background Tiles
    * ROM File Address: `$27810-2800f`
    * CHR ROM Address: `$07800-07fff`
  * 2 KiB CHR ROM Bank #$20 - Level 5 Background Tiles
    * ROM File Address: `$28010-2880f`
    * CHR ROM Address: `$08000-087ff`
  * 2 KiB CHR ROM Bank #$22 - Level 5 Background Tiles
    * ROM File Address: `$28810-2900f`
    * CHR ROM Address: `$08800-08fff`
  * 2 KiB CHR ROM Bank #$24 - Level 7 Background Tiles
    * ROM File Address: `$29010-2980f`
    * CHR ROM Address: `$09000-097ff`
  * 2 KiB CHR ROM Bank #$26 - Level 7 Background Tiles
    * ROM File Address: `$29810-2a00f`
    * CHR ROM Address: `$09800-09fff`
  * 2 KiB CHR ROM Bank #$28 - Level 7 Background Tiles
    * ROM File Address: `$2a010-2a80f`
    * CHR ROM Address: `$0a000-0a7ff`
  * 2 KiB CHR ROM Bank #$2a - Level 7 Background Tiles
    * ROM File Address: `$2a810-2b00f`
    * CHR ROM Address: `$0a800-0afff`
  * 2 KiB CHR ROM Bank #$2c - Level 7 Background Tiles
    * ROM File Address: `$2b010-2b80f`
    * CHR ROM Address: `$0b000-0b7ff`
  * 2 KiB CHR ROM Bank #$2e - Level 7 Background Tiles
    * ROM File Address: `$2b810-2c00f`
    * CHR ROM Address: `$0b800-0bfff`
  * 2 KiB CHR ROM Bank #$30 - Level 7 Background Tiles
    * ROM File Address: `$2c010-2c80f`
    * CHR ROM Address: `$0c000-0c7ff`
  * 2 KiB CHR ROM Bank #$32 - Level 2 Background Tiles
    * ROM File Address: `$2c810-2d00f`
    * CHR ROM Address: `$0c800-0cfff`
  * 2 KiB CHR ROM Bank #$34 - Level 8 Background Tiles
    * ROM File Address: `$2d010-2d80f`
    * CHR ROM Address: `$0d000-0d7ff`
  * 2 KiB CHR ROM Bank #$36 - Level 8 Background Tiles
    * ROM File Address: `$2d810-2e00f`
    * CHR ROM Address: `$0d800-0dfff`
  * 2 KiB CHR ROM Bank #$38 - Level 1 Background Tiles
    * ROM File Address: `$2e010-2e80f`
    * CHR ROM Address: `$0e000-0e7ff`
  * 2 KiB CHR ROM Bank #$3a - Level 1 Background Tiles
    * ROM File Address: `$2e810-2f00f`
    * CHR ROM Address: `$0e800-0efff`
  * 2 KiB CHR ROM Bank #$3c - Level 1 After Boss Background Tiles
    * ROM File Address: `$2f010-2f80f`
    * CHR ROM Address: `$0f000-0f7ff`
  * 2 KiB CHR ROM Bank #$3e - Level 1 After Boss Background Tiles
    * ROM File Address: `$2f810-3000f`
    * CHR ROM Address: `$0f800-0ffff`
  * 2 KiB CHR ROM Bank #$40 - Intro and End Credits Background Tiles
    * ROM File Address: `$30010-3080f`
    * CHR ROM Address: `$10000-107ff`
  * 2 KiB CHR ROM Bank #$42 - Level 4 Elevator (all black tiles for IRQ)
    * ROM File Address: `$30810-3100f`
    * CHR ROM Address: `$10800-10fff`
  * 1 KiB CHR ROM Bank #$43 - Unused all black tiles
    * ROM File Address: `$30c10-3100f`
    * CHR ROM Address: `$10c00-10fff`
  * 1 KiB CHR ROM Bank #$44 - Player Sprites
    * ROM File Address: `$31010-3140f`
    * CHR ROM Address: `$11000-113ff`
  * 1 KiB CHR ROM Bank #$45 - Player Sprites
    * ROM File Address: `$31410-3180f`
    * CHR ROM Address: `$11400-117ff`
  * 1 KiB CHR ROM Bank #$46 - HUD, Weapon Item, End Credits, and Player Sprites
    * ROM File Address: `$31810-31c0f`
    * CHR ROM Address: `$11800-11bff`
  * 1 KiB CHR ROM Bank #$47 - Level 1 and Level 4 Sprites
    * ROM File Address: `$31c10-3200f`
    * CHR ROM Address: `$11c00-11fff`
  * 1 KiB CHR ROM Bank #$48 - Level 3 Sprites
    * ROM File Address: `$32010-3240f`
    * CHR ROM Address: `$12000-123ff`
  * 1 KiB CHR ROM Bank #$49 - Overhead Level Player Sprites
    * ROM File Address: `$32410-3280f`
    * CHR ROM Address: `$12400-127ff`
  * 1 KiB CHR ROM Bank #$4a - Overhead Level Player Sprites
    * ROM File Address: `$32810-32c0f`
    * CHR ROM Address: `$12800-12bff`
  * 1 KiB CHR ROM Bank #$4b - Overhead HUD, Weapon Item, and Player Sprites
    * ROM File Address: `$32c10-3300f`
    * CHR ROM Address: `$12c00-12fff`
  * 2 KiB CHR ROM Bank #$4c - Level 6 Background Tiles
    * ROM File Address: `$33010-3380f`
    * CHR ROM Address: `$13000-137ff`
  * 2 KiB CHR ROM Bank #$4e - Level 6 Background Tiles
    * ROM File Address: `$33810-3400f`
    * CHR ROM Address: `$13800-13fff`
  * 2 KiB CHR ROM Bank #$50 - Level 6 Background Tiles
    * ROM File Address: `$34010-3480f`
    * CHR ROM Address: `$14000-147ff`
  * 2 KiB CHR ROM Bank #$52 - Level 6 Background Tiles
    * ROM File Address: `$34810-3500f`
    * CHR ROM Address: `$14800-14fff`
  * 2 KiB CHR ROM Bank #$54 - Level 6 Mini-Boss Background Tiles
    * ROM File Address: `$35010-3580f`
    * CHR ROM Address: `$15000-157ff`
  * 2 KiB CHR ROM Bank #$56 - Level 6 Mini-Boss Background Tiles
    * ROM File Address: `$35810-3600f`
    * CHR ROM Address: `$15800-15fff`
  * 2 KiB CHR ROM Bank #$58 - Level 6 After Mini-Boss Background Tiles
    * ROM File Address: `$36010-3680f`
    * CHR ROM Address: `$16000-167ff`
  * 2 KiB CHR ROM Bank #$5a - Level 5 Background Tiles
    * ROM File Address: `$36810-3700f`
    * CHR ROM Address: `$16800-16fff`
  * 2 KiB CHR ROM Bank #$5c - Level 2 Background Tiles
    * ROM File Address: `$37010-3780f`
    * CHR ROM Address: `$17000-177ff`
  * 2 KiB CHR ROM Bank #$5e - Level 2 Background Tiles
    * ROM File Address: `$37810-3800f`
    * CHR ROM Address: `$17800-17fff`
  * 1 KiB CHR ROM Bank #$60 - Level 5 Sprites
    * ROM File Address: `$38010-3840f`
    * CHR ROM Address: `$18000-183ff`
  * 1 KiB CHR ROM Bank #$61 - Level 6 Sprites
    * ROM File Address: `$38410-3880f`
    * CHR ROM Address: `$18400-187ff`
  * 1 KiB CHR ROM Bank #$62 - Level 6 Sprites
    * ROM File Address: `$38810-38c0f`
    * CHR ROM Address: `$18800-18bff`
  * 1 KiB CHR ROM Bank #$63 - Level 7 Sprites
    * ROM File Address: `$38c10-3900f`
    * CHR ROM Address: `$18c00-18fff`
  * 1 KiB CHR ROM Bank #$64 - Level 7 Sprites
    * ROM File Address: `$39010-3940f`
    * CHR ROM Address: `$19000-193ff`
  * 1 KiB CHR ROM Bank #$65 - Level 7 Sprites
    * ROM File Address: `$39410-3980f`
    * CHR ROM Address: `$19400-197ff`
  * 1 KiB CHR ROM Bank #$66 - Level 7 Sprites
    * ROM File Address: `$39810-39c0f`
    * CHR ROM Address: `$19800-19bff`
  * 1 KiB CHR ROM Bank #$67 - Level 8 Sprites
    * ROM File Address: `$39c10-3a00f`
    * CHR ROM Address: `$19c00-19fff`
  * 1 KiB CHR ROM Bank #$68 - Level 8 Sprites
    * ROM File Address: `$3a010-3a40f`
    * CHR ROM Address: `$1a000-1a3ff`
  * 1 KiB CHR ROM Bank #$69 - End Credits Sprites
    * ROM File Address: `$3a410-3a80f`
    * CHR ROM Address: `$1a400-1a7ff`
  * 2 KiB CHR ROM Bank #$6a - Intro Background Tiles
    * ROM File Address: `$3a810-3b00f`
    * CHR ROM Address: `$1a800-1afff`
  * 2 KiB CHR ROM Bank #$6c - Level 8 Background Tiles
    * ROM File Address: `$3b010-3b80f`
    * CHR ROM Address: `$1b000-1b7ff`
  * 2 KiB CHR ROM Bank #$6e - Level 8 Background Tiles
    * ROM File Address: `$3b810-3c00f`
    * CHR ROM Address: `$1b800-1bfff`
  * 2 KiB CHR ROM Bank #$70 - Level 8 Background Tiles
    * ROM File Address: `$3c010-3c80f`
    * CHR ROM Address: `$1c000-1c7ff`
  * 2 KiB CHR ROM Bank #$72 - Level 8 Background Tiles
    * ROM File Address: `$3c810-3d00f`
    * CHR ROM Address: `$1c800-1cfff`
  * 12 KiB of CHR ROM Banks - unused duplicates of #$70-#$74 (final boss tiles)
    * ROM File Address: `$3d010-4000f`
    * CHR ROM Address: `$1d000-1ffff`
    * #$74 - duplicate of #$70
    * #$75 - duplicate of #$71
    * #$76 - duplicate of #$72
    * #$77 - duplicate of #$73
    * #$78 - duplicate of #$70
    * #$79 - duplicate of #$71
    * #$7a - duplicate of #$72
    * #$7b - duplicate of #$73
    * #$7c - duplicate of #$70
    * #$7d - duplicate of #$71
    * #$7e - duplicate of #$72
    * #$7f - duplicate of #$73