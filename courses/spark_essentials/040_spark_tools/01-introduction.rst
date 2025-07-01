==============
Introduction
==============

------------------------
Identifying SPARK Code
------------------------

* Pragma or aspect :ada:`SPARK_Mode` identifies SPARK code
* As a pragma in the global/local configuration pragmas file
* As a configuration pragma at the start of a unit

  - Note: it comes before :ada:`with` clauses

  .. code:: ada

     pragma SPARK_Mode (On); -- On is the default
     with Lib; use Lib;
     package Pack is ...

* As an aspect on the unit declaration

  .. code:: ada

     package Pack
       with SPARK_Mode
     is ...

* **Both** unit spec and unit body need a pragma/aspect

----------------------
Main Tools for SPARK
----------------------

* GNAT development tools: SPARK is a subset of Ada 2022

  - Compiler also checks **SPARK-specific** legality rules

* SPARK analysis tools

  - Flow analysis and proof
  - File dependencies are **different** from the compiler

    + Due to generation of data dependencies
    + Analysis of unit depends on bodies of :ada:`with`'ed units
    + ...unless all data dependencies are specified

  - Behavior similar to builder like :toolname:`GPRbuild`

    + Units can be analyzed in parallel on multicore machines
    + Minimal rework if code and dependencies did not change

* IDEs for Ada/SPARK development

