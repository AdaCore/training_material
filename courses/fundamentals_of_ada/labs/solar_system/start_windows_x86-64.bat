set LOCAL=%~dp0
set INSTALL=E:\install\GNAT
set PATH=%INSTALL%\gnat-community-2020-x86_64-windows\bin;%PATH%
set PATH=%LOCAL%\libs64;%PATH%
set ADA_PROJECT_PATH=%LOCAL%\game_support;%LOCAL%\gnat_sdl;
set LIBRARY_PATH=%LOCAL%\libs64;%LIBRARY_PATH%
set GNAT_STUDIO_HOME=%LOCAL%
set HOST=Windows
@echo off
echo -
echo -
echo ********************************
echo **                            **
echo ** 64 bits environment ready  **
echo **                            **
echo ********************************
echo - 
echo -  run GNAT Studio from this console
echo - 
echo -
start "" "gnatstudio"
cmd
