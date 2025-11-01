' function used by build.bat for copying bytes from the source Super C NES rom
' file to the various binary files for use when assembling.

Set fso=CreateObject("Scripting.FileSystemObject")

' check if output already exists
If fso.FileExists(WScript.Arguments(2)) Then
Else
    Wscript.Echo "    Writing file " + WScript.Arguments(2) + "."

    ' read bytes from source Super C NES rom
    Set inFile=fso.OpenTextFile("baserom.nes")
    inFile.Skip(WScript.Arguments(0))
    buf=inFile.Read(WScript.Arguments(1))
    inFile.Close

    ' write binary file

    Set outFile = fso.CreateTextFile(WScript.Arguments(2))
    outFile.Write buf
    outFile.Close
End If