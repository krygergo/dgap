@echo off

if "%1"=="start" goto start
if "%1"=="observer" goto observer

echo Unknown command: "%1"
goto :eof

:start
%erl% -boot %boot_script% -sname %node_name% -setcookie %cookie% -extra "%2"
goto :eof

:observer
erl_call -a "observer start" -n %node_name% -c %cookie%
