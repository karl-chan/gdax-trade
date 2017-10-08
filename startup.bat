@echo off
set arg=%1

if "%arg%"=="elm" call :elm
if "%arg%"=="server" call :server
if "%arg%"=="" (
    call :elm
    call :server
)
exit /b

:elm
cd static && elm-app build && cd dist && start chrome index.html && cd ../..
goto:eof

:server
stack build --trace && stack exec gdax-trade-server
goto:eof