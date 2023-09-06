rem vvvv Put the GNAT and libs install path below
set GNAT=C:\GNAT\2021\bin
set LIBS=%cd%\libs64
if not exist "%GNAT%\*" goto error
if not exist "%LIBS%\*" goto error

set PATH=%GNAT%;%PATH%
set PATH=%LIBS%;%PATH%
set ADA_PROJECT_PATH=%cd%\game_support;%cd%\gnat_sdl;
set LIBRARY_PATH=%LIBS%;%LIBRARY_PATH%
set GNAT_STUDIO_HOME=%cd%
set HOST=Windows
start "" "gnatstudio"
:error:
cmd
