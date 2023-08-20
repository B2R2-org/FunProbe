rem Arg1: base directory path
rem Arg2: binary name

rem echo %1/ida/%2.json
rem echo %1/bin/%2

"C:\Program Files\IDA Pro 7.7\ida64.exe" -c -A -S"C:\scripts\idascript.py %2" %1
