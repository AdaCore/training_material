set LOCAL=%~dp0
set INSTALL=E:\install\GNAT
set PATH=%INSTALL%\gnat-community-2020-x86-windows\bin;%PATH%
set PATH=%LOCAL%\libs;%PATH%
set ADA_PROJECT_PATH=%LOCAL%\game_support;%LOCAL%\gnat_sdl;
set LIBRARY_PATH=%LOCAL%\libs;%LIBRARY_PATH%
set GNAT_STUDIO_HOME=%LOCAL%
set HOST=Windows
@echo off
echo -
echo -
echo ********************************
echo **                            **
echo ** 32 bits environment ready  **
echo **                            **
echo ********************************
echo - 
echo -  run GNAT Studio from this console
echo - 
echo -
start "" "gnatstudio"
cmd
