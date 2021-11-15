# RUN controls the execution of most examples
#     RUN=true   will execute the built programs
#     RUN=false  will not execute the built programs
RUN=true

# TARGET holds the configuration names of the compiler
#        it is mostly used to adapt the examples to special configurations
#        such as Windows (with no cygwin)
TARGET=$(shell gcc -dumpmachine)

# GPRBUILD is the default gprbuild command of the host system
#   To compile for specific target and/or runtime, --target and --RTS switches
#   can be added here
GPRBUILD=gprbuild

# RM is the default delete command of the host system
RM= rm -f

# SHELL is the default command shell of the host system
SHELL=sh

# EXE is the usual executable suffix of the target system
EXE=

# S is the usual directory separator of the host system
S=/

# CDir is the usual denomination of the current directory
CDir=./

# PREFIX is the usual prefix for GNAT commands
PREFIX=
 
#
# Windows Specific section
#
ifneq ($(findstring mingw32,$(TARGET)),)
  # By default we assume that CYGWIN is not available
  CYGWIN=false
  EXE=.exe

  # if CYGWIN is not available let's adapt to CMD shell
ifneq ($(CYGWIN), true)
  CDir=
  RM=cmd.exe /c del
  SHELL=cmd.exe
  S='\'
endif
endif

ifeq ($(SILENT), true)
  QUIET=-q
  MAKESILENT=-s
endif
