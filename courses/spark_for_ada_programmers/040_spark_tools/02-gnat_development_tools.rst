========================
GNAT Development Tools
========================

----------------------
Compiling SPARK Code
----------------------

* GNAT compiler for Ada/SPARK

  - Checks conformance of source with Ada and SPARK legality rules
  - Compiles source into executable

* Native and cross compilers
* Any runtime library: full, embedded, light-tasking, light

---------------------------------
Enabling Assertions at Run-Time
---------------------------------

* Assertions can be enabled globally with switch :command:`-gnata`
* Assertions can be enabled/disabled locally with pragma
  :ada:`Assertion_Policy`

  For example to enable preconditions and disable postconditions:

  .. code:: ada

     pragma Assertion_Policy (Pre => Check, Post => Ignore);

* Pragma can also be used in global/local configuration pragmas file
* Failing assertion raises exception :ada:`Assertion_Failure`

----------------------
Debugging SPARK Code
----------------------

* GDB debugger for Ada/SPARK

  - Code should be compiled with :command:`-g -O0`

|

* Assertions can be debugged **too**!

  - Code should be compiled with :command:`-gnata`

