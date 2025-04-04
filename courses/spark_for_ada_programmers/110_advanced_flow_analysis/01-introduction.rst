==============
Introduction
==============

------------------------------------
Data and Information Flow Analysis
------------------------------------

* Data flow analysis

  - Models the variables used by a subprogram
  - Enforces data initialization policy
  - Detects reads of uninitialized data

* Data dependencies can be specified

  - Introduced by aspect :ada:`Global`

* Information flow analysis

  - Models the flow of information from inputs to outputs
  - Can be very useful for security analysis

* Flow dependencies can be specified

  - Introduced by aspect :ada:`Depends`

