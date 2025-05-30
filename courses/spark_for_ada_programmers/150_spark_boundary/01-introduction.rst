==============
Introduction
==============

----------------------
Modeling the System
----------------------

* Special variables used to interact with the system

  - Usually marked as volatile for the compiler
  - This prevents compiler optimizations

|

* :toolname:`GNATprove` needs to model these interactions

  - Both in flow analysis and proof
  - Distinction between different kinds of interactions

|

* This modeling is used as assumptions by :toolname:`GNATprove`

  - These assumptions need to be reviewed

------------------------
Integrating SPARK Code
------------------------

* Not all the program is in SPARK usually

  - The Operating System (if present) is rarely in SPARK
  - Some services (logging, input/output) may not be in SPARK
  - Only a core part may be in SPARK

|

* User needs to specify the boundary of SPARK code

|

* :toolname:`GNATprove` needs to model interactions with non-SPARK code

|

* GNAT needs to compile SPARK and non-SPARK code together

