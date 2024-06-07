************************************
Advanced GNATcoverage Capabilities
************************************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-------------------------------
More Options and Capabilities
-------------------------------

* Various options exist to include/exclude instrumentation for

  * Subprojects
  * Specific units
  * Specific files
  * Parts of a file

* In addition, the coverage report mechanism allows

  * Multiple output formats
  * Control over what information is reported
  * Coverage on instances of generics or the generics themselves
  * Plus more

=======================================
Project-Based Instrumentation Control
=======================================

------------------------
Simple Instrumentation
------------------------

* As we saw before, it is easy to instrument a simple project

   .. code:: gpr

      project Default is
         for Source_Dirs use ("src");
         for Object_Dir use "obj";
         for Main use ("test_driver.adb");
      end Default;

   ::

      gnatcov instrument -Pdefault.gpr --level=stmt

* But what happens in a more complicated build environment with multiple projects?

   .. code:: gpr

      with "io/io.gpr";
      with "utils/utils.gpr";
      project Sdc is
         for Languages use ("ada");
         for Source_Dirs use ("src");
         for Object_Dir use "obj";
         for Main use ("sdc.adb");
      end Sdc;

---------------------------------
Multiple Projects - Simple Case
---------------------------------

* As we would expect, the simple case (we want to instrument the entire application) works the same way:

   ::

      gnatcov instrument -Psdc.gpr --level=stmt

* This will instrument all source files within the :filename:`src` folder, plus any source files from projects ``io`` and ``utils``

  .. image:: gnatdas/cover_multiple_projects.png

---------------------------------
Indicating Projects of Interest
---------------------------------

* But what if we don't want coverage on a particular subproject?

  * Might be externally built
  * Might be a re-used project that we don't need coverage for

* Specifying *projects of interest* is handled by the instrumentation command

* We build the command using the following options

   * ``--no-subprojects`` tells the instrumenter to only process the root project
   * ``--projects=utils`` tells the instrumenter to only process the ``utils`` project

* You can combine the options for better control

   * Root project only 

     .. container:: latex_environment tiny

       ``gnatcov instrument -Psdc.gpr --no-subprojects --level=stmt``

   * Subproject ``utils`` only (not ``sdc``)

     .. container:: latex_environment tiny

       ``gnatcov instrument -Psdc.gpr --no-subprojects --projects=utils --level=stmt``

   * Root project and subproject ``io``

     .. container:: latex_environment tiny

       ``gnatcov instrument -Psdc.gpr --projects=sdc --projects=io --level=stmt``

----------------------------------------------
Indicating Units of Interest within Projects
----------------------------------------------

* By default, all units within project(s) of interest are considered *units of interest*

* We can control units of interest from the project file's :ada:`Coverage` package

   .. code:: gpr

      package Coverage is
        for Units use ("instructions", "tokens");
      end Coverage;

   * Note that **units** refer to the Ada name, not the source file name

      * So :ada:`package Naming` would have no affect

* The four keywords to control units of interest

   .. list-table::

      * - ``units``

        - List of units to instrument

      * - ``units_list``

        - Filename containing list of units to instrument

      * - ``excluded_units``

        - List of units **not** to instrument

      * - ``excluded_units_list``

        - Filename containing list of units **not** to instrument

-----------------------
What About Separates?
-----------------------

* Sometimes you build your application using a :ada:`separate` body for a package or subprogram

   .. code:: Ada

      package body Input is 
         procedure Read_New_Line is separate;
      end Input;

   * Useful when your build process wants a different body based on various situations

      .. code:: gpr

         package Naming is
            case Build is
               when "DEBUG" =>
                  for Body ("input.read_new_line")
                     use "read_new_line_from_console.adb";
               when "PRODUCTION" =>
                  for Body ("input.read_new_line")
                     use "read_new_line_from_file.adb";
            end case;
         end Naming;

* As a :ada:`separate` is not a unit, how do we prevent instrumentation of this subprogram when ``DEBUG`` is set?

   .. code:: gpr

      package Coverage is
        for Ignored_Source_Files use ("input-*.adb");
        for Ingored_Source_Files_List use "files_to_skip.txt";
      end Coverage;

----------------------------------------
Units of Interest and Their Dependents
----------------------------------------

* In a large project, we might want coverage on a unit PLUS every unit it calls

   * Analyzing the entire project is overkill, but we don't want to find all the units that our unit needs

* When instrumenting files, :toolname:`GNATcoverage` creates an *obligation file* (file extension :filename:`.sid`)

   * This file contains information regarding all the dependents for the unit

* To get coverage on a unit and all its dependents, use the :command:`--sid` option for the unit(s) you want

   * Use the unit name for individual files, or :filename:`@<filename>` to specify a file containing a list

     .. container:: latex_environment tiny

       ``gnatcov coverage -Pdefault.gpr --level=stmt --sid=tokens.sid --sid=@sidfiles.lst``

======================================
Source-Based Instrumentation Control
======================================

---------------------
Coverage Exemptions
---------------------

* Sometimes there are blocks of code for which you do not want coverage reporting

   * Typically for defensive coding purposes

      .. code:: Ada
         :number-lines: 1

         function My_New return Access_T is
            Retval : Access_T;
         begin
            Retval := new Record_T;
            if Retval = null then
                raise Program_Error;
            end if;
            return Retval;
         end My_New;

* The likelihood of **line 5** being :ada:`True` should be small, so we don't want the :ada:`False` branch (and the :ada:`raise` statement) to reduce our coverage totals

---------------------------
Coverage Exemption Region
---------------------------

* We need to modify the :ada:`My_New` subprogram to indicate where we do not want coverage

   * Use :ada:`pragma Annotate` to indicate source that should not be tracked

      .. code:: Ada
         :number-lines: 1

         function My_New return Access_T is
            Retval : Access_T;
         begin
            Retval := new Record_T;
            pragma Annotate (Xcov, Exempt_On, "justification");
            if Retval = null then
                raise Program_Error;
            end if;
            pragma Annotate (Xcov, Exempt_Off);
            return Retval;
         end My_New;

   * Note that we are turning on/off the *exemption*, not the *instrumentation*

      * That's why we start the block with :ada:`Exempt_On` on **line 5** and end with :ada:`Exempt_Off` on **line 9**

------------------------------
Coverage Exemption Reporting
------------------------------

* Coverage reports can be generated in multiple ways

  ``gnatcov coverage --level=stmt+decision --annotate=``**<form>**`` *trace* -P default.gpr``

  where *<form>* is one of the following:

report
   Summary that lists all coverage violations

xcov
   For each source file, the **xcov** file contains a global summary of assessment results followed by annotated source lines

xcov+
   Same as **xcov** except it provides extra details below lines with improperly satisfied obligations

html
   Web-based reporting mechanism to show coverage data for the project
   
xml
   XML database containing all necessary coverage information

--------------------
'xcov' Output File
--------------------

:filename:`utils.adb.xcov`

.. container:: columns

   .. container:: column

      .. container:: latex_environment tiny

         Without exemptions
         ::

            60% of 5 lines covered
            80% statement coverage (4 out of 5)
            0% decision coverage (0 out of 1)

            Coverage level: stmt+decision
               1 .: package body Utils is
               2 .: 
               3 .:    function My_New return Access_T is
               4 +:       Retval : Access_T;
               5 .:    begin
               6 +:       Retval := new Record_T;
               7 !:       if Retval = null then
               8 -:          raise Program_Error;
               9 .:       end if;
              10 +:       return Retval;
              11 .:    end My_New;
              12 .: 
              13 .: end Utils;

   .. container:: column

      .. container:: latex_environment tiny

         With exemptions

         ::

            60% of 5 lines covered
            60% statement coverage (3 out of 5)
            0% decision coverage (0 out of 1)

            Coverage level: stmt+decision
               1 .: package body Utils is
               2 .: 
               3 .:    function My_New return Access_T is
               4 +:       Retval : Access_T;
               5 .:    begin
               6 +:       Retval := new Record_T;
               7 *:       pragma Annotate (Xcov,
               8 *:                        Exempt_On,
               9 *:                        "justification");
              10 *:       if Retval = null then
              11 *:          raise Program_Error;
              12 *:       end if;
              13 *:       pragma Annotate (Xcov,
              14 .:                        Exempt_Off);
              15 +:       return Retval;
              16 .:    end My_New;
              17 .: 
              18 .: end Utils;

         *Note different coverage indicator for exempted code*

----------------------
'report' Output File
----------------------

* Exemptions appear in the coverage summary report

  ``gnatcov coverage --level=stmt+decision --annotate=report *trace* -P default.gpr``

::

   ** COVERAGE REPORT **
   ===========================
   == 1. ASSESSMENT CONTEXT ==
   ===========================
   Date and time of execution: 2024-02-21 15:08:45 -05:00
   Tool version: XCOV 24.0 (20231011)
   Command line:
   C:\GNATPRO\24.0\bin\gnatcov.exe coverage --level=stmt+decision --annotate=report main.exe-65d6573e-9358-65d65741.srctrace -P default.gpr
   Coverage level: stmt+decision
   Trace files:
   main.exe-65d6573e-9358-65d65741.srctrace
     kind     : source
     program  : C:\temp\temp\obj\main.exe
     date     : 2024-02-21 15:04:17 -05:00
     tag      : 
   =========================================
   == 2. NON-EXEMPTED COVERAGE VIOLATIONS ==
   =========================================
   2.1. STMT COVERAGE
   ------------------
   No violation.
   2.2. DECISION COVERAGE
   ----------------------
   No violation.
   =========================
   == 3. EXEMPTED REGIONS ==
   =========================
   utils.adb:7:7-13:7: 2 exempted violations, justification:
   "justification"
   Exempted violations:
   utils.adb:10:10: decision outcome TRUE never exercised
   utils.adb:11:10: statement not executed
   1 exempted region, 2 exempted violations.
   =========================
   == 4. ANALYSIS SUMMARY ==
   =========================
   No non-exempted STMT violation.
   No non-exempted DECISION violation.
   1 exempted region, 2 exempted violations.
   ** END OF REPORT **


=====
Lab
=====

.. include:: labs/cover_030_advanced_topics.lab.rst
