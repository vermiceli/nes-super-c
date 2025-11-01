Below is a detailed overview of the layout of the _Super C_ ROM file's contents.

# PRG ROM Map

_Super C_ has 16 8 KiB PRG ROM banks.

To convert to PRG ROM address from ROM file address, subtract #$0f from the
ROM file address.  This removes the the iNES ROM Header.

* Header
  * ROM File Address: `$00000-$0000f` - iNES ROM Header Data
* PRG Bank #$00
  * ROM File Address: `$00010-$0200f`
* PRG Bank #$01
  * ROM File Address: `$02010-$0400f`
* PRG Bank #$02
  * ROM File Address: `$04010-$0600f`
* PRG Bank #$03
  * ROM File Address: `$06010-$0800f`
* PRG Bank #$04
  * ROM File Address: `$08010-$0a00f`
* PRG Bank #$05
  * ROM File Address: `$0a010-$0c00f`
* PRG Bank #$06
  * ROM File Address: `$0c010-$0e00f`
* PRG Bank #$07
  * ROM File Address: `$0e010-$1000f`
* PRG Bank #$08
  * ROM File Address: `$10010-$1200f`
* PRG Bank #$09
  * ROM File Address: `$12010-$1400f`
* PRG Bank #$0a
  * ROM File Address: `$14010-$1600f`
* PRG Bank #$0b
  * ROM File Address: `$16010-$1800f`
* PRG Bank #$0c
  * ROM File Address: `$18010-$aa00f`
* PRG Bank #$0d
  * ROM File Address: `$1a010-$1c00f`
* PRG Bank #$0e
  * ROM File Address: `$1c010-$1e00f`
* PRG Bank #$0f
  * ROM File Address: `$1e010-$2000f`

# CHR ROM Map

There is 128 KiB of CHR ROM data.  The left pattern table tile banks (background
tiles) are referenced as 2 KiB banks.  Each 2 KiB bank is half of a pattern
table.  The right pattern table tile banks (sprites) are referenced as 1 KiB
banks.

  * 2 KiB CHR ROM Bank #$00
    * ROM File Address: `$20010-2080f`
    * CHR ROM Address: `$00000-007ff`
  * 2 KiB CHR ROM Bank #$02
    * ROM File Address: `$20810-2100f`
    * CHR ROM Address: `$00800-00fff`
  * 2 KiB CHR ROM Bank #$04
    * ROM File Address: `$21010-2180f`
    * CHR ROM Address: `$01000-017ff`
  * 2 KiB CHR ROM Bank #$06
    * ROM File Address: `$21810-2200f`
    * CHR ROM Address: `$01800-01fff`
    * 1 KiB CHR ROM Bank #$07 is also referenced in intro for showing logo
  * 2 KiB CHR ROM Bank #$08
    * ROM File Address: `$22010-2280f`
    * CHR ROM Address: `$02000-027ff`
  * 2 KiB CHR ROM Bank #$0a
    * ROM File Address: `$22810-2300f`
    * CHR ROM Address: `$02800-02fff`
  * 2 KiB CHR ROM Bank #$0c
    * ROM File Address: `$23010-2380f`
    * CHR ROM Address: `$03000-037ff`
  * 2 KiB CHR ROM Bank #$0e
    * ROM File Address: `$23810-2400f`
    * CHR ROM Address: `$03800-03fff`
  * 2 KiB CHR ROM Bank #$10
    * ROM File Address: `$24010-2480f`
    * CHR ROM Address: `$04000-047ff`
    * 1 KiB CHR ROM Bank #$11 is also referenced in stage 6 mini-boss
  * 2 KiB CHR ROM Bank #$12
    * ROM File Address: `$24810-2500f`
    * CHR ROM Address: `$04800-04fff`
  * 2 KiB CHR ROM Bank #$14
    * ROM File Address: `$25010-2580f`
    * CHR ROM Address: `$05000-057ff`
  * 2 KiB CHR ROM Bank #$16
    * ROM File Address: `$25810-2600f`
    * CHR ROM Address: `$05800-05fff`
  * 2 KiB CHR ROM Bank #$18
    * ROM File Address: `$26010-2680f`
    * CHR ROM Address: `$06000-067ff`
  * 1 KiB CHR ROM Bank #$1a
    * ROM File Address: `$26810-26c0f`
    * CHR ROM Address: `$06800-06bff`
  * 1 KiB CHR ROM Bank #$1b
    * ROM File Address: `$26c10-2700f`
    * CHR ROM Address: `$06c00-06fff`
  * 2 KiB CHR ROM Bank #$1c
    * ROM File Address: `$27010-2780f`
    * CHR ROM Address: `$07000-077ff`
  * 2 KiB CHR ROM Bank #$1e
    * ROM File Address: `$27810-2800f`
    * CHR ROM Address: `$07800-07fff`
  * 2 KiB CHR ROM Bank #$20
    * ROM File Address: `$28010-2880f`
    * CHR ROM Address: `$08000-087ff`
  * 2 KiB CHR ROM Bank #$22
    * ROM File Address: `$28810-2900f`
    * CHR ROM Address: `$08800-08fff`
  * 2 KiB CHR ROM Bank #$24
    * ROM File Address: `$29010-2980f`
    * CHR ROM Address: `$09000-097ff`
  * 2 KiB CHR ROM Bank #$26
    * ROM File Address: `$29810-2a00f`
    * CHR ROM Address: `$09800-09fff`
  * 2 KiB CHR ROM Bank #$28
    * ROM File Address: `$2a010-2a80f`
    * CHR ROM Address: `$0a000-0a7ff`
  * 2 KiB CHR ROM Bank #$2a
    * ROM File Address: `$2a810-2b00f`
    * CHR ROM Address: `$0a800-0afff`
  * 2 KiB CHR ROM Bank #$2c
    * ROM File Address: `$2b010-2b80f`
    * CHR ROM Address: `$0b000-0b7ff`
  * 2 KiB CHR ROM Bank #$2e
    * ROM File Address: `$2b810-2c00f`
    * CHR ROM Address: `$0b800-0bfff`
  * 2 KiB CHR ROM Bank #$30
    * ROM File Address: `$2c010-2c80f`
    * CHR ROM Address: `$0c000-0c7ff`
  * 2 KiB CHR ROM Bank #$32
    * ROM File Address: `$2c810-2d00f`
    * CHR ROM Address: `$0c800-0cfff`
  * 2 KiB CHR ROM Bank #$34
    * ROM File Address: `$2d010-2d80f`
    * CHR ROM Address: `$0d000-0d7ff`
  * 2 KiB CHR ROM Bank #$36
    * ROM File Address: `$2d810-2e00f`
    * CHR ROM Address: `$0d800-0dfff`
  * 2 KiB CHR ROM Bank #$38
    * ROM File Address: `$2e010-2e80f`
    * CHR ROM Address: `$0e000-0e7ff`
  * 2 KiB CHR ROM Bank #$3a
    * ROM File Address: `$2e810-2f00f`
    * CHR ROM Address: `$0e800-0efff`
  * 2 KiB CHR ROM Bank #$3c
    * ROM File Address: `$2f010-2f80f`
    * CHR ROM Address: `$0f000-0f7ff`
  * 2 KiB CHR ROM Bank #$3e
    * ROM File Address: `$2f810-3000f`
    * CHR ROM Address: `$0f800-0ffff`
  * 2 KiB CHR ROM Bank #$40
    * ROM File Address: `$30010-3080f`
    * CHR ROM Address: `$10000-107ff`
  * 2 KiB CHR ROM Bank #$42 - All black tiles, used on elevator during irq
    * ROM File Address: `$30810-3100f`
    * CHR ROM Address: `$10800-10fff`
  * 1 KiB CHR ROM Bank #$43 - Unused all black tiles
    * ROM File Address: `$30c10-3100f`
    * CHR ROM Address: `$10c00-10fff`
  * 1 KiB CHR ROM Bank #$44
    * ROM File Address: `$31010-3140f`
    * CHR ROM Address: `$11000-113ff`
  * 1 KiB CHR ROM Bank #$45
    * ROM File Address: `$31410-3180f`
    * CHR ROM Address: `$11400-117ff`
  * 1 KiB CHR ROM Bank #$46
    * ROM File Address: `$31810-31c0f`
    * CHR ROM Address: `$11800-11bff`
  * 1 KiB CHR ROM Bank #$47
    * ROM File Address: `$31c10-3200f`
    * CHR ROM Address: `$11c00-11fff`
  * 1 KiB CHR ROM Bank #$48
    * ROM File Address: `$32010-3240f`
    * CHR ROM Address: `$12000-123ff`
  * 1 KiB CHR ROM Bank #$49
    * ROM File Address: `$32410-3280f`
    * CHR ROM Address: `$12400-127ff`
  * 1 KiB CHR ROM Bank #$4a
    * ROM File Address: `$32810-32c0f`
    * CHR ROM Address: `$12800-12bff`
  * 1 KiB CHR ROM Bank #$4b
    * ROM File Address: `$32c10-3300f`
    * CHR ROM Address: `$12c00-12fff`
  * 2 KiB CHR ROM Bank #$4c
    * ROM File Address: `$33010-3380f`
    * CHR ROM Address: `$13000-137ff`
  * 2 KiB CHR ROM Bank #$4e
    * ROM File Address: `$33810-3400f`
    * CHR ROM Address: `$13800-13fff`
  * 2 KiB CHR ROM Bank #$50
    * ROM File Address: `$34010-3480f`
    * CHR ROM Address: `$14000-147ff`
  * 2 KiB CHR ROM Bank #$52
    * ROM File Address: `$34810-3500f`
    * CHR ROM Address: `$14800-14fff`
  * 2 KiB CHR ROM Bank #$54
    * ROM File Address: `$35010-3580f`
    * CHR ROM Address: `$15000-157ff`
  * 2 KiB CHR ROM Bank #$56
    * ROM File Address: `$35810-3600f`
    * CHR ROM Address: `$15800-15fff`
  * 2 KiB CHR ROM Bank #$58
    * ROM File Address: `$36010-3680f`
    * CHR ROM Address: `$16000-167ff`
  * 2 KiB CHR ROM Bank #$5a
    * ROM File Address: `$36810-3700f`
    * CHR ROM Address: `$16800-16fff`
  * 2 KiB CHR ROM Bank #$5c
    * ROM File Address: `$37010-3780f`
    * CHR ROM Address: `$17000-177ff`
  * 2 KiB CHR ROM Bank #$5e
    * ROM File Address: `$37810-3800f`
    * CHR ROM Address: `$17800-17fff`
  * 1 KiB CHR ROM Bank #$60
    * ROM File Address: `$38010-3840f`
    * CHR ROM Address: `$18000-183ff`
  * 1 KiB CHR ROM Bank #$61
    * ROM File Address: `$38410-3880f`
    * CHR ROM Address: `$18400-187ff`
  * 1 KiB CHR ROM Bank #$62
    * ROM File Address: `$38810-38c0f`
    * CHR ROM Address: `$18800-18bff`
  * 1 KiB CHR ROM Bank #$63
    * ROM File Address: `$38c10-3900f`
    * CHR ROM Address: `$18c00-18fff`
  * 1 KiB CHR ROM Bank #$64
    * ROM File Address: `$39010-3940f`
    * CHR ROM Address: `$19000-193ff`
  * 1 KiB CHR ROM Bank #$65
    * ROM File Address: `$39410-3980f`
    * CHR ROM Address: `$19400-197ff`
  * 1 KiB CHR ROM Bank #$66
    * ROM File Address: `$39810-39c0f`
    * CHR ROM Address: `$19800-19bff`
  * 1 KiB CHR ROM Bank #$67
    * ROM File Address: `$39c10-3a00f`
    * CHR ROM Address: `$19c00-19fff`
  * 1 KiB CHR ROM Bank #$68
    * ROM File Address: `$3a010-3a40f`
    * CHR ROM Address: `$1a000-1a3ff`
  * 1 KiB CHR ROM Bank #$69
    * ROM File Address: `$3a410-3a80f`
    * CHR ROM Address: `$1a400-1a7ff`
  * 2 KiB CHR ROM Bank #$6a
    * ROM File Address: `$3a810-3b00f`
    * CHR ROM Address: `$1a800-1afff`
  * 2 KiB CHR ROM Bank #$6c
    * ROM File Address: `$3b010-3b80f`
    * CHR ROM Address: `$1b000-1b7ff`
  * 2 KiB CHR ROM Bank #$6e
    * ROM File Address: `$3b810-3c00f`
    * CHR ROM Address: `$1b800-1bfff`
  * 2 KiB CHR ROM Bank #$70
    * ROM File Address: `$3c010-3c80f`
    * CHR ROM Address: `$1c000-1c7ff`
  * 2 KiB CHR ROM Bank #$72
    * ROM File Address: `$3c810-3d00f`
    * CHR ROM Address: `$1c800-1cfff`
  * 12 KiB of CHR ROM Banks - unused duplicates of #$70-#$74 (final boss tiles)
    * ROM File Address: `$3d010-4000f`
    * CHR ROM Address: `$1d000-1ffff`
    * #$74 - dupcliate of #$70
    * #$75 - duplicate of #$71
    * #$76 - duplicate of #$72
    * #$77 - duplicate of #$73
    * #$78 - duplicate of #$70
    * #$79 - duplciate of #$71
    * #$7a - duplicate of #$72
    * #$7b - duplicate of #$73
    * #$7c - duplicate of #$70
    * #$7d - duplicate of #$71
    * #$7e - duplicate of #$72
    * #$7f - duplicate of #$73