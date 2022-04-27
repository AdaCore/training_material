************************
Building with GPRbuild
************************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------------
Generic Build Tool
--------------------

* Designed for construction of large multi-language systems

  * Allows subsystems and libraries

* Manages three step build process:

  * Compilation phase:

    * Each compilation unit examined in turn, checked for consistency, and, if necessary, compiled (or recompiled) as appropriate

  * Post-compilation phase (binding):

    * Compiled units from a given language are passed to language-specific post-compilation tool (if any)
    * Objects grouped into static or dynamic libraries as specified

  * Linking phase:

    * Units or libraries from all subsystems are passed to appropriate linker tool

==============
Command Line
==============

-----------------------
GPRbuild Command Line
-----------------------

* Made up of three elements

  * Main project file (required)
  * Switches (optional)

    * :command:`gprbuild` switches
    * Options for called tools

  * Main source files (optional)

    * If not specified, executable(s) specified in project file are built
    * If no main files specified, no executable is built

--------------------------------
Common Options Passed To Tools
--------------------------------

* :command:`-cargs options`

  * Options passed to all compilers
  * Example:

    ::

      -cargs -g


* :command:`-cargs:<language> options`

  * Options passed to compiler for specific language
  * Examples:

    ::

      -cargs:Ada -gnatf
      -cargs:C -E


* :command:`-bargs options`

  * Options passed to all binder drivers

* :command:`-bargs:<language> options`

  * Options passed to binder driver for specific language
  * Examples:

    ::

      -bargs:Ada binder_prefix=ppc-elf
      -bargs:C++ c_compiler_name=ccppc


* :command:`-largs options`

  * Options passed to linker for generating executable

------------------------------
Common Command Line Switches
------------------------------

.. container:: latex_environment tiny

  .. list-table::

    * - -P *<project file>*

      - Name of main project file (space between *P* and *<filename>* is optional)

    * - -aP *<directory>*

      - Add *<directory>* to list of directories to search for project files

    * - -u [<source file> [, <source file>...]]

      - If sources specified, only compile these sources.

    * -

      - Otherwise, compile all sources in main project file

    * - -U [<source file> [, <source file>...]]

      - If sources specified, only compile these sources.

    * -

      - Otherwise, compile all sources in project tree

    * - -Xnm=val

      - Specify external reference that may be read via built-in function ``external``.

    * - --version

      - Display information about GPRbuild: version, origin and legal status

    * - --help

      - Display GPRbuild usage

    * - --config=<config project file name>

      - Configuration project file name (default :filename:`default.cgpr`)

-----------------------
Common Build Switches
-----------------------

Switches to be specified on command line or in ``Builder`` package of main project file

.. container:: latex_environment tiny

  .. list-table::

    * - --create-map-file[=<map file>]

      - When linking, (if supported) by the platform, create a map file :filename:`<map file>`.

    * -

      - (If not specified, filename is :filename:`<executable name>.map`)

    * - -j<num>

      - Use <num> simultaneous compilation jobs

    * - -k

      - Keep going after compilation errors (default is to stop on first error)

    * - -p (or --create-missing-dirs)

      - Creating missing output directory (e.g. object directory)

=====
Lab
=====

.. include:: labs/020_building_with_gprbuild.lab.rst
