# Assembles and links the source assemblies into a .nes ROM.
# Run this script from powershell if available, if not available use build.bat
# batch file from the windows command prompt.

# Super C = [Default] US NES version
# Probotector = European NES version (not supported)
param([String]$Game='Superc')
$global:SOURCE_ROM = $null
$GAME_HASH = "A3E9BACB35DC038186D7F59112E8A2E081F9D0E343608003B44EE76BC25526552BF4C4EAA44531B75B5626B51DBCE8C055F6B7DE87FB426C474A8F1463686841"
$ROM_NAME = "superc.nes"
$DBG_NAME = "superc.dbg"
$ASSETS_NAME = "assets.txt"
$ASSET_GAME_TYPE = "src\assets\asset-game-type.txt"

IF ($Game -ceq "Probotector") {
    # $ROM_NAME = "probotector.nes"
    # $DBG_NAME = "probotector.dbg"
    # $GAME_HASH = "4302BF3FDAF704F11FCD60C83BF475D11C8EBC772A31B45318C1ABBF5EA011992D9E8714268B2B7CE938D2426599794D0277868A98C6162BCC5A10AE851C8EB5"
    # $ASSETS_NAME = "probotector-assets.txt"
    Write-Warning "Probotector not supported."
    return
}

<#
.SYNOPSIS

Copies bytes from the Super C NES rom file into binary files for use when
assembling.
#>
function Set-Bytes {
    param ($Skip, $Take, $Output)

    IF (Test-Path -Path $Output) {
        return
    }

    # only read baserom.nes once for speed improvements
    IF ($global:SOURCE_ROM -eq $null) {
        Write-Output "    Reading input file baserom.nes."
        IF ($PSVersionTable.PSVersion.Major -ge 6) {
            $global:SOURCE_ROM = Get-Content .\baserom.nes -AsByteStream
        } ELSE {
            $global:SOURCE_ROM = Get-Content .\baserom.nes -Raw -Encoding Byte
        }
    }

    Write-Output "    Writing file $Output."

    IF ($PSVersionTable.PSVersion.Major -ge 6) {
        $global:SOURCE_ROM | Select-Object -Skip $Skip -First $Take | Set-Content $Output -AsByteStream
    } ELSE {
        $global:SOURCE_ROM | Select-Object -Skip $Skip -First $Take | Set-Content $Output -Encoding Byte
    }
}

IF (Test-Path -Path $ROM_NAME) {
    Write-Output "Deleting $ROM_NAME."
    Remove-Item -Path $ROM_NAME
}

IF (-not (Test-Path -Path "obj")) {
    New-Item -ItemType Directory -Path obj
}

IF (Test-Path -Path obj\*.o) {
    Write-Output "Deleting object files."
    Remove-Item -Path obj\*.o
}

IF (-not (Test-Path -Path "baserom.nes")) {
    Write-Output "No baserom.nes file found.  If assets are missing, then the build will fail."
} ELSE {
    $SHA512_HASH = (Get-FileHash baserom.nes -Algorithm SHA512).Hash
    IF ($SHA512_HASH -ne $GAME_HASH) {
        Write-Warning "baserom.nes file integrity does NOT match expected result."
    }
}

# used to know which assets were last build
$LAST_BUILD_TYPE = "Superc"
IF (Test-Path $ASSET_GAME_TYPE) {
    $LAST_BUILD_TYPE = Get-Content $ASSET_GAME_TYPE -Raw
}

# If the assets are from a different game, then delete them
# For example, if the assets were extracted from Super C and currently building
# Probotector, then delete the assets and extract them from the Probotector baserom.nes
IF ($LAST_BUILD_TYPE -ne $Game) {
    Write-Output "Removing graphic asset files"
    Remove-Item -Path src\assets\chr_rom\* -Include *.bin

    Write-Output "Removing audio asset files"
    Remove-Item -Path src\assets\audio_data\* -Include *.bin
}

# loop through assets defined in assets.txt (or probotector-assets.txt) and extract bytes from baserom.nes
Write-Output "Extracting binary data from baserom.nes"
ForEach ($line in Get-Content -Path $ASSETS_NAME) {
    $tokens = -split $line
    Set-Bytes -Skip $tokens[1] -Take $tokens[2] -Output $tokens[0]
}

# Store game type that the assets are for
IF (Test-Path $ASSET_GAME_TYPE) {
    Remove-Item -Path $ASSET_GAME_TYPE
}

$Game | Set-Content -Path $ASSET_GAME_TYPE -NoNewline

# prevent write race condition
Start-Sleep -Milliseconds 100

Write-Output "Assembling ROM Banks"

# show commands run in output
Set-PSDebug -Trace 1
ca65 -D $Game --debug-info -o obj\ram.o src\ram.asm
ca65 -D $Game --debug-info -o obj\constants.o src\constants.asm
ca65 -D $Game --debug-info -o obj\ines_header.o src\ines_header.asm
ca65 -D $Game --debug-info -o obj\bank-0.o src\bank-0.asm
ca65 -D $Game --debug-info -o obj\bank-1.o src\bank-1.asm
ca65 -D $Game --debug-info -o obj\bank-2.o src\bank-2.asm
ca65 -D $Game --debug-info -o obj\bank-3.o src\bank-3.asm
ca65 -D $Game --debug-info -o obj\bank-4.o src\bank-4.asm
ca65 -D $Game --debug-info -o obj\bank-5.o src\bank-5.asm
ca65 -D $Game --debug-info -o obj\bank-6.o src\bank-6.asm
ca65 -D $Game --debug-info -o obj\bank-7.o src\bank-7.asm
ca65 -D $Game --debug-info -o obj\bank-8.o src\bank-8.asm
ca65 -D $Game --debug-info -o obj\bank-9.o src\bank-9.asm
ca65 -D $Game --debug-info -o obj\bank-a.o src\bank-a.asm
ca65 -D $Game --debug-info -o obj\bank-b.o src\bank-b.asm
ca65 -D $Game --debug-info -o obj\bank-c.o src\bank-c.asm
ca65 -D $Game --debug-info -o obj\bank-d.o src\bank-d.asm
ca65 -D $Game --debug-info -o obj\bank-e.o src\bank-e.asm
ca65 -D $Game --debug-info -o obj\bank-f.o src\bank-f.asm
ca65 -D $Game --debug-info -o obj\chr_rom.o src\chr_rom.asm

Set-PSDebug -Trace 0

# link assemblies together to single .nes ROM
Write-Output "Creating .nes ROM"

Set-PSDebug -Trace 1
ld65 -C superc.cfg --dbgfile $DBG_NAME `
.\obj\ram.o .\obj\constants.o `
.\obj\ines_header.o `
.\obj\bank-0.o .\obj\bank-1.o .\obj\bank-2.o .\obj\bank-3.o `
.\obj\bank-4.o .\obj\bank-5.o .\obj\bank-6.o .\obj\bank-7.o `
.\obj\bank-8.o .\obj\bank-9.o .\obj\bank-a.o .\obj\bank-b.o `
.\obj\bank-c.o .\obj\bank-d.o .\obj\bank-e.o .\obj\bank-f.o `
.\obj\chr_rom.o `
-o $ROM_NAME

# compare assembled ROM hash to expected hash if file exists
Set-PSDebug -Trace 0
IF (Test-Path -Path $ROM_NAME) {
    $SHA512_HASH = (Get-FileHash $ROM_NAME -Algorithm SHA512).Hash

    IF ($SHA512_HASH -eq $GAME_HASH) {
        Write-Output "File integrity matches."
    } ELSE {
        Write-Warning "File integrity does NOT match."
    }
}