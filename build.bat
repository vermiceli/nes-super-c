@echo off

rem Assembles and links the source assemblies into a .nes ROM.
rem Run this script from the windows command prompt if you do not have access to
rem PowerShell.  This script does not do some hashing validation that PowerShell
rem can.

SET ROM_NAME="superc.nes"
SET DBG_NAME="superc.dbg"
SET ASSETS_NAME=assets.txt
SET ASSET_GAME_TYPE=src\assets\asset-game-type.txt
SET GAME="%1"

IF %GAME% == "Probotector" (
    SET ROM_NAME="probotector.nes"
    SET DBG_NAME="probotector.dbg"
    SET ASSETS_NAME=probotector-assets.txt

    echo "Probotector not supported."
    EXIT
) ELSE (
    SET "GAME=Superc"
)

IF EXIST %ROM_NAME% (
    echo Deleting %ROM_NAME%.
    del %ROM_NAME%
)

IF NOT EXIST "obj" (
    mkdir "obj"
)

IF EXIST "obj\*.o" (
   echo Deleting object files.
   del "obj\*.o"
)

IF NOT EXIST baserom.nes (
    echo No baserom.nes file found.  If assets are missing, then the build will fail.
)

rem used to know which assets were last build
SET LAST_BUILD_TYPE="Superc"
IF EXIST %ASSET_GAME_TYPE% (
    SET /p LAST_BUILD_TYPE=<%ASSET_GAME_TYPE%
)

rem If the assets are from a different game, then delete them
rem For example, if the assets were extracted from Super C and currently building
rem Probotector, then delete the assets and extract them from the Probotector baserom.nes
IF NOT %LAST_BUILD_TYPE% == %Game% (
    echo Removing graphic asset files
    del src\assets\chr_rom\*.bin

    echo Removing audio data asset files
    del src\assets\audio_data\*.bin
)

rem loop through assets defined in assets.txt (or probotector-assets.txt) and extract bytes from baserom.nes
echo Extracting binary data from baserom.nes
for /f "tokens=1,2,3 delims= " %%i in (%ASSETS_NAME%) do (
  cscript /nologo set_bytes.vbs %%j %%k %%i
)

rem Store game type that the assets are for
IF EXIST %ASSET_GAME_TYPE% (
    del %ASSET_GAME_TYPE%
)

<nul SET /P "=%GAME%" > %ASSET_GAME_TYPE%

rem show commands run in output
echo Assembling PRG Rom Banks

@echo on
ca65 -D %GAME% --debug-info -o obj\ram.o src\ram.asm
ca65 -D %GAME% --debug-info -o obj\constants.o src\constants.asm
ca65 -D %GAME% --debug-info -o obj\ines_header.o src\ines_header.asm
ca65 -D %GAME% --debug-info -o obj\bank-0.o src\bank-0.asm
ca65 -D %GAME% --debug-info -o obj\bank-1.o src\bank-1.asm
ca65 -D %GAME% --debug-info -o obj\bank-2.o src\bank-2.asm
ca65 -D %GAME% --debug-info -o obj\bank-3.o src\bank-3.asm
ca65 -D %GAME% --debug-info -o obj\bank-4.o src\bank-4.asm
ca65 -D %GAME% --debug-info -o obj\bank-5.o src\bank-5.asm
ca65 -D %GAME% --debug-info -o obj\bank-6.o src\bank-6.asm
ca65 -D %GAME% --debug-info -o obj\bank-7.o src\bank-7.asm
ca65 -D %GAME% --debug-info -o obj\bank-8.o src\bank-8.asm
ca65 -D %GAME% --debug-info -o obj\bank-9.o src\bank-9.asm
ca65 -D %GAME% --debug-info -o obj\bank-a.o src\bank-a.asm
ca65 -D %GAME% --debug-info -o obj\bank-b.o src\bank-b.asm
ca65 -D %GAME% --debug-info -o obj\bank-c.o src\bank-c.asm
ca65 -D %GAME% --debug-info -o obj\bank-d.o src\bank-d.asm
ca65 -D %GAME% --debug-info -o obj\bank-e.o src\bank-e.asm
ca65 -D %GAME% --debug-info -o obj\bank-f.o src\bank-f.asm
ca65 -D %GAME% --debug-info -o obj\chr_rom.o src\chr_rom.asm
@echo off

rem link assemblies together to single .nes ROM

echo "Creating .nes ROM"

@echo on
ld65 -C superc.cfg --dbgfile %DBG_NAME% ^
.\obj\ram.o .\obj\constants.o ^
.\obj\ines_header.o ^
.\obj\bank-0.o .\obj\bank-1.o .\obj\bank-2.o .\obj\bank-3.o ^
.\obj\bank-4.o .\obj\bank-5.o .\obj\bank-6.o .\obj\bank-7.o ^
.\obj\bank-8.o .\obj\bank-9.o .\obj\bank-a.o .\obj\bank-b.o ^
.\obj\bank-c.o .\obj\bank-d.o .\obj\bank-e.o .\obj\bank-f.o ^
.\obj\chr_rom.o ^
-o %ROM_NAME%
@echo off