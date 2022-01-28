--------------------------
Interfacing with C Lab
--------------------------

* Requirements

   - Given a C function that calculates speed in MPH from some information, your application should

      + Ask user for distance and time
      + Populate the structure appropriately
      + Call C function to return speed
      + Print speed to console

* Hints

   - Structure contains the following fields

      + Distance (floating point)
      + Distance Type (enumeral)
      + Seconds (floating point)
   
-------------------------------------------------
Interfacing with C Lab - GNAT Studio
-------------------------------------------------

To compile/link the C file into the Ada executable:

   1. Make sure the C file is in the same directory as the Ada source files
   2. :menu:`Edit` :math:`\rightarrow` :menu:`Project Properties`
   3. :menu:`Sources` :math:`\rightarrow` :menu:`Languages` :math:`\rightarrow` Check the "C" box
   4. Build and execute as normal
   
-----------------------------------------
Interfacing with C Lab Solution - Ada
-----------------------------------------

.. container:: source_include labs/answers/230_interfacing_with_c.txt :start-after:--Ada :end-before:--Ada :code:Ada

---------------------------------------
Interfacing with C Lab Solution - C
---------------------------------------

.. container:: source_include labs/answers/230_interfacing_with_c.txt :start-after:--C :end-before:--C :code:C
