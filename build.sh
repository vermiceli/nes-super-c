#!/bin/bash

# Assembles and links the source assemblies into a .nes ROM.

# Run this script from a bash terminal if on linux or mac.
# If you are on windows, use either build.ps1, or build.bat
GAME="$1"
GAME_HASH="a3e9bacb35dc038186d7f59112e8a2e081f9d0e343608003b44ee76bc25526552bf4c4eaa44531b75b5626b51dbce8c055f6b7de87fb426c474a8f1463686841"
ROM_NAME="superc.nes"
DBG_NAME="superc.dbg"
ASSETS_NAME="assets.txt"
ASSET_GAME_TYPE="src/assets/asset-game-type.txt"

if [[ "$GAME" == "Probotector" ]]
then
    ROM_NAME="probotector.nes"
    DBG_NAME="probotector.dbg"
    GAME_HASH="4302bf3fdaf704f11fcd60c83bf475d11c8ebc772a31b45318c1abbf5ea011992d9e8714268b2b7ce938d2426599794d0277868a98c6162bcc5a10ae851c8eb5"
    ASSETS_NAME="probotector-assets.txt"
    echo "Probotector not supported."
    exit 1
else
    GAME="Superc"
fi

# function to check between different available hash functions
# mac doesn't come with sha512sum by default, but includes shasum
romHasher() {
    if command -v sha512sum &> /dev/null
    then
        sha512sum $1
    else
        shasum -a 512 $1
    fi
}

setBytes(){
    if test -f $3
    then
        return
    fi

    echo "    Writing file $3."
    dd bs=1 skip=$1 count=$2 if=baserom.nes of=$3 status=none
}

if ! ld65 --version &> /dev/null
then
    echo "cc65 compiler suite could not be found. Please install cc65 and add it to your path."
    exit
fi

mkdir -p obj

if test -f ROM_NAME
then
    echo "Deleting ${ROM_NAME}."
    rm ROM_NAME
fi

if test -f "obj/*.o"
then
    echo "Deleting object files."
    rm obj/*.o
fi

if ! test -f "baserom.nes"
then
    echo "No baserom.nes file found.  If assets are missing, then the build will fail."
else
    ROM_HASH=$(romHasher baserom.nes | awk '{print $1}')
    if [[ "$ROM_HASH" != "$GAME_HASH" ]]
    then
        echo "baserom.nes file integrity does NOT match expected result."
    fi
fi

# used to know which assets were last build
LAST_BUILD_TYPE="Superc"
if test -f $ASSET_GAME_TYPE
then
    LAST_BUILD_TYPE=`cat $ASSET_GAME_TYPE`
fi

# If the assets are from a different game, then delete them
# For example, if the assets were extracted from Super C and currently building
# Probotector, then delete the assets and extract them from the Probotector baserom.nes
if [[ "$LAST_BUILD_TYPE" != "$GAME" ]]
then
    echo "Removing graphic asset files"
    rm src/assets/chr_rom/*.bin

    echo "Removing audio data asset files"
    rm src/assets/audio_data/*.bin
fi

# loop through assets defined in assets.txt (or probotector-assets.txt) and extract bytes from baserom.nes
echo "Extracting binary data from baserom.nes"
while read -r line || [ -n "$p" ]
do
    set $line
    file=$1
    start=$2
    length=$3
    length=$(echo $length | tr -d '\r')
    file=$(echo "$file" | tr '\\' '/')
    setBytes $start $length $file
done < $ASSETS_NAME

echo "$GAME" > $ASSET_GAME_TYPE

echo "Assembling PRG Rom Banks"
ca65 -D $GAME --debug-info -o obj/ram.o src/ram.asm
ca65 -D $GAME --debug-info -o obj/constants.o src/constants.asm
ca65 -D $GAME --debug-info -o obj/ines_header.o src/ines_header.asm
ca65 -D $GAME --debug-info -o obj/bank-0.o src/bank-0.asm
ca65 -D $GAME --debug-info -o obj/bank-1.o src/bank-1.asm
ca65 -D $GAME --debug-info -o obj/bank-2.o src/bank-2.asm
ca65 -D $GAME --debug-info -o obj/bank-3.o src/bank-3.asm
ca65 -D $GAME --debug-info -o obj/bank-4.o src/bank-4.asm
ca65 -D $GAME --debug-info -o obj/bank-5.o src/bank-5.asm
ca65 -D $GAME --debug-info -o obj/bank-6.o src/bank-6.asm
ca65 -D $GAME --debug-info -o obj/bank-7.o src/bank-7.asm
ca65 -D $GAME --debug-info -o obj/bank-8.o src/bank-8.asm
ca65 -D $GAME --debug-info -o obj/bank-9.o src/bank-9.asm
ca65 -D $GAME --debug-info -o obj/bank-a.o src/bank-a.asm
ca65 -D $GAME --debug-info -o obj/bank-b.o src/bank-b.asm
ca65 -D $GAME --debug-info -o obj/bank-c.o src/bank-c.asm
ca65 -D $GAME --debug-info -o obj/bank-d.o src/bank-d.asm
ca65 -D $GAME --debug-info -o obj/bank-e.o src/bank-e.asm
ca65 -D $GAME --debug-info -o obj/bank-f.o src/bank-f.asm
ca65 -D $GAME --debug-info -o obj/chr_rom.o src/chr_rom.asm

echo "Creating .nes ROM"
ld65 -C superc.cfg --dbgfile $DBG_NAME \
./obj/ram.o ./obj/constants.o \
./obj/ines_header.o \
./obj/bank-0.o ./obj/bank-1.o ./obj/bank-2.o ./obj/bank-3.o \
./obj/bank-4.o ./obj/bank-5.o ./obj/bank-6.o ./obj/bank-7.o \
./obj/bank-8.o ./obj/bank-9.o ./obj/bank-a.o ./obj/bank-b.o \
./obj/bank-c.o ./obj/bank-d.o ./obj/bank-e.o ./obj/bank-f.o \
./obj/chr_rom.o \
-o $ROM_NAME

if test -f $ROM_NAME
then
    # compare assembled ROM hash to expected hash
    ROM_HASH=$(romHasher $ROM_NAME | awk '{print $1}')
    if [[ "$ROM_HASH" == "$GAME_HASH" ]]
    then
        echo "File integrity matches."
    else
        echo "File integrity does NOT match."
    fi
fi