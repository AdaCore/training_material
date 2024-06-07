---------------------
Advanced Topics Lab
---------------------

* We are going to demonstrate different ways of controlling coverage information on the supplied base project

  * Which happens to include subprojects

* Copy the :filename:`cover_030_advanced_topics` lab from the course materials location

  * This is basically a copy of the *Tutorial* example from the GNAT distribution

* Directories in the folder:

  * :filename:`io` - I/O support subproject
  * :filename:`utils` - Utilities subproject
  * :filename:`sdc` - base project

* Each directory contains a project file and :filename:`src` directory

*Note: Many of the following pages use animation to first give you a task and then show you how to do it.* :menu:`Page Down` *does not always go to the next page!*

-------------------------------
Quick Note on The Application
-------------------------------

* **SDC** stands for *Simple Desktop Calculator*

* The input is a list of operands an operators in programmatic order, not mathematic

   * The calculator does have a simple memory

* Example: To add **11** to **22** you would enter :command:`11 22 +`

   * To multiply that by **10**, the next line might be :command:`10 *`

* In additon to numbers, you have the commands **Clear**, **Print**, and **Quit**

  Example run :command:`sdc.exe`

  .. list-table::
     :header-rows: 1

     * - Display
       - Description

     * - Welcome to SDC. Go ahead type your commands ...

     * - 100 20 +
       - User Input

     * - 100 20 +
       - Command echo

     * - 3 +
       - User Input

     * - 3 +
       - Command echo

     * - print
       - User Input

     * - print
       - Command echo

     * - ->  123
       - Result of **Print**

     * - quit
       - User Input

     * - quit
       - Command echo

     * - Thank you for using SDC.

----------------
Initialization
----------------

.. container:: animate 1-

   * Make sure your project builds

.. container:: animate 2-

   ::

      cd /path/to/sdc.gpr
      gprbuild -P sdc.gpr

   * Prepare the coverage libraries

.. container:: animate 3-

   ::

      gnatcov setup --prefix=.\gnatcov-rts

      OR

      gnatcov setup --prefix=./gnatcov-rts

   Don't forget to set the environment variable ``GPR_PROJECT_PATH`` to point to the folder containing the :filename:`gnatcov_rts.gpr` file

-----------------------------------------
Workflow One - Coverage on Base Project
-----------------------------------------

* Typically we only care about coverage on the project we are working with

   * We know we won't get 100% coverage on things like utility packages, so we don't want to instrument them

* Instrument the ``sdc`` project for statement and decision coverage *without* instrumenting the ``utils`` or ``io`` projects

.. container:: animate 2-

   * Two ways to do this

      * Method 1 - focus only on base project

         ``gnatcov instrument -Psdc.gpr --no-subprojects --level=stmt+decision``

      * Method 2 - specify particular project

         ``gnatcov instrument -Psdc.gpr --projects=sdc --level=stmt+decision``

------------------------
Workflow One Continued
------------------------

.. container:: animate 1-

   * Build your application

.. container:: animate 2-

   ``gprbuild -f -p -Psdc.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr``

   * Run your application and add the coverage to the project

.. container:: animate 3-

   ``obj/sdc``

   * Add the coverage to the project

.. container:: animate 4-

   ``gnatcov coverage --level=stmt+decision --annotate=xcov *.srctrace -Psdc.gpr``
   
   * Inspect the coverage

.. container:: animate 5-

   It should be in the :filename:`obj/*.xcov` files

---------------------------------------
Workflow Two - Excluding Source Files
---------------------------------------

* We only want coverage on package bodies

   * Modify the base project file to ignore all :filename:`*.ads` files

.. container:: animate 2-

   * We need to add the following to :filename:`sdc.gpr`

   ::

      package Coverage is
         for Ignored_Source_Files use ("*.ads");
      end Coverage;

   * You could also use ``for Ignored_Source_Files_List use ("list.txt");`` where :filename:`list.txt` lists all the spec files

.. image:: blue_line.png

* Instrument the base project, run the executable, and analyze the coverage data

.. container:: animate 3-

   ::

      gnatcov instrument -Psdc.gpr --level=stmt
      gprbuild -f -p -Psdc.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr
      obj/sdc
      gnatcov coverage --level=stmt --annotate=xcov *.srctrace -Psdc.gpr

   * When looking for :filename:`*.xcov` files, note they exist only for :filename:`*.adb` files

   * Note the warning that no information was found for unit :ada:`Except`

      * Because it's only a package spec
   
----------------------------------------
Workflow Three - Excluding Source Code
----------------------------------------

* We do not want coverage on any exception processing in unit :ada:`Sdc`

   * Use :ada:`pragma Annotate` to turn off coverage in the exception blocks

.. container:: animate 2-

   .. code:: Ada
      :number-lines: 34

      pragma Annotate (Xcov, Exempt_On, "Exception Handler");
      exception
         when Stack.Underflow =>
            Error_Msg ("Not enough values in the Stack.");

         when Stack.Overflow =>
           null;
      pragma Annotate (Xcov, Exempt_Off);

   * Now run your code and generate a summary report

--------------------------------------
Workflow Three - Exemption Reporting
--------------------------------------

* When you look look at :filename:`sdc.adb.xcov` you'll notices the exempted lines are marked with ``*``

::

   29 .:      begin
   30 .: 
   31 +:         Process (Next);
   32 .:         --  Read the next Token from the input and process it.
   33 .: 
   34 *:      pragma Annotate (Xcov, Exempt_On, "Exception Handler");
   35 *:      exception
   36 *:         when Stack.Underflow =>
   37 *:            Error_Msg ("Not enough values in the Stack.");
   38 *: 
   39 *:         when Stack.Overflow =>
   40 *:           null;
   41 *:      pragma Annotate (Xcov, Exempt_Off);
   42 .:      end;

* While the summary report contains a description of the exemption region

::

   =========================
   == 3. EXEMPTED REGIONS ==
   =========================

   sdc.adb:34:7-41:7: 2 exempted violations, justification:
   "Exception Handler"

   Exempted violations:
   sdc.adb:37:13: statement not executed
   sdc.adb:40:13: statement not executed

   1 exempted region, 2 exempted violations.
