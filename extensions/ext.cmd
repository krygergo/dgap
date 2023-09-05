@echo off

set erl_call="%bindir%\erl_call.exe"

if "%1"=="console" goto console
if "%1"=="start" goto start
if "%1"=="stop" goto stop
if "%1"=="call" goto call

echo Unknown command: "%1"
goto :eof

:console
%erl% -boot %boot_script% -sname %node_name% -setcookie %cookie% -extra "%2"
goto :eof

:start
%erl% -boot %boot_script% -sname %node_name% -setcookie %cookie% -detached -extra "%2"
goto :eof

:stop
%erl_call% -q -n %node_name% -c %cookie%
goto :eof

:call
%erl_call% -a %2 -n %node_name% -c %cookie%
