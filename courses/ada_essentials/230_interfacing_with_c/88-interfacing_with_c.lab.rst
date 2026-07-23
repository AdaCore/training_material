=====
Lab
=====

------------------------
Interfacing with C Lab
------------------------

* Requirements

   - Given a C function that calculates speed in MPH from some information, your application should

      + Provide some values for distance and time (consider hard-coding, or prompting for user input)
      + Populate the structure appropriately
      + Call C function to return speed
      + Print speed to console

* Hints

   - Structure contains the following components

      + Distance (floating point)
      + Distance Type (enumeral)
      + Seconds (floating point)
   
--------------------------------------
Interfacing with C Lab - GNAT Studio
--------------------------------------

To compile/link the C file into the Ada executable:

   * Make sure the C file is in the same directory as the Ada source files
   * Add the following to the :filename:`default.gpr` file

     .. code::

       for Languages use ("Ada", "C");

     *This tells the compiler that the project has both Ada and C sources*

   * Build and execute as normal

.. note::

  The :ada:`Languages` directive is already set in the prompt's :filename:`default.gpr` file
   
---------------------------------------
Interfacing with C Lab Solution - Ada
---------------------------------------

.. container:: source_include 230_interfacing_with_c/lab/interfacing_with_c/answer/main.adb :code:Ada :number-lines:1

-------------------------------------
Interfacing with C Lab Solution - C
-------------------------------------

.. container:: source_include 230_interfacing_with_c/lab/interfacing_with_c/answer/c_file.c :code:C
