@echo off

set erl_call="%bindir%\erl_call.exe"

if "%1"=="start" goto start
if "%1"=="call" goto call

echo Unknown command: "%1"
goto :eof

:start
%erl% -boot %boot_script% -sname %node_name% -setcookie %cookie% -extra "%2"
goto :eof

:call
%erl_call% -a %2 -n %node_name% -c %cookie%
