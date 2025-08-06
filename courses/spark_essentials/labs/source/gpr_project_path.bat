@echo off
if exist C:\SPARKPRO\%1\lib\gnat\sparklib_common.gpr set GPR_PROJECT_PATH=C:\SPARKPRO\%1\lib\gnat;%GPR_PROJECT_PATH%
if not exist C:\SPARKPRO\%1\lib\gnat\sparklib_common.gpr echo C:\SPARKPRO\%1\lib\gnat does not exist
