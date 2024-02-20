*****************
Basic Workflow
*****************

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

===================
Workflow Overview
===================

-----------------
General Process
-----------------

1. Set up instrumentation runtime
2. Instrument sources for coverage
3. Build the executable from the instrumented sources
4. Run the executable

   * Possibly many times

5. Generate and analyze code coverage reports

==================
A Simple Example
==================

------------------
Unit of Interest
------------------

We want to get code coverage on this unit

.. code:: Ada

  package Ops is
     type Op_Kind is (Increment, Decrement, Double, Half);

     procedure Apply
       (Op :        Op_Kind;
        X  : in out Integer);
  end Ops;

  package body Ops is
     procedure Apply
       (Op :        Op_Kind;
        X  : in out Integer) is
     begin
        case Op is
           when Increment => X := X + 1;
           when Decrement => X := X - 1;
           when Double    => X := X * 2;
           when Half      => X := X / 2;
        end case;
     exception
        when others =>
           null;
     end Apply;
  end Ops;

--------------------------------
Supplying an Execution Context
--------------------------------

* Sometimes, we have an application for which we want coverage

  * But more often, we want coverage on a package or collection of packages

* In our example, we have a package, so we need to create a main program to run

.. code:: Ada

  with Ada.Text_IO; use Ada.Text_IO;
  with Ops;
  procedure Test_Driver is
     procedure Run_One
       (Kind  : Ops.Op_Kind;
        Value : Integer) is
        X : Integer := Value;
     begin
        Ops.Apply (Kind, X);
        Put_Line ("Before:" & Value'Image & " After:" & X'Image);
     end Run_One;
  begin
     Run_One (Ops.Increment, 4);
  end Test_Driver;

-------
Setup
-------

* First need to verify our project builds cleanly

  ::

    gprbuild -P default.gpr

* Then we need to install the instrumentation context, giving a directory (*prefix*) where the data will be stored

  ::

     gnatcov setup --prefix=.\gnatcov-rts

* We need to update the environment variable ``GPR_PROJECT_PATH`` to add this context:

  .. container:: latex_environment scriptsize

    ::

       set GPR_PROJECT_PATH=%GPR_PROJECT_PATH%;\path\to\gnatcov-rts\share\gpr

       OR

       export GPR_PROJECT_PATH=$GPR_PROJECT_PATH:/path/to/gnatcov-rts/share/gpr

------------
Instrument
------------

We now need to add the instrumentation to the source code that will collect data

  ::

    gnatcov instrument -Pdefault.gpr --level=stmt

:dfn:`level` is the type of coverage information you will gather

  .. list-table::

    * - ``stmt``
      - Statement coverage
    * - ``stmt+decision``
      - Statement and decision coverage
    * - ``stmt+mcdc``
      - Statement and Masking MCDC
    * - ``stmt+uc_mcdc``
      - Statement and Unique Cause MCDC

-------
Build
-------

* To build the instrumented executable, we just need some extra switches

  .. container:: latex_environment tiny

    ::

      gprbuild -f -p -Pdefault.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

  .. container:: latex_environment small

    where

    ``-f``
       Force recompilation

    ``-p``
       Create missing object (and library/executable) directories

    ``--src-subdirs``
       Instruct the builder to search for the instrumented versions of the sources

    ``--implicit-with``
       Provide visibility to the builder over the coverage runtime referenced by the instrumented sources
   
---------
Execute
---------

* We can now execute the test program as we would normally

   ::

     obj\test_driver.exe


   :command:`Before: 4 After: 5`

* This generates a source trace file in the working directory that looks like :filename:`test-driver.exe-<stamp>.srctrace`

  * *stamp* will be a unique identifier to prevent clashes from multiple executions

---------
Analyze
---------

* Analysis of coverage is done by processing the source trace file(s) generated

  .. container:: latex_environment tiny

    ::

      gnatcov coverage --level=stmt --annotate=xcov test_driver*.srctrace -Pdefault.gpr

  .. container:: latex_environment small

    where

    ``--level=stmt``
      indicates we are looking for statement coverage

    ``--annotate=xcov``
      indicates we want an annotated source report in text format

    ``-Pdefault.gpr``
      indicates we want the report for all units for the executable in the project

* This generates :filename:`*.xcov` files in the :filename:`obj` directory for each unit in the project

--------------------
Viewing the Report
--------------------

* The report file (for package body :ada:`ops`) looks like:

  .. container:: latex_environment footnotesize

    ::

      C:\temp\gnatcov\src\ops.adb:
      33% of 6 lines covered
      33% statement coverage (2 out of 6)

      Coverage level: stmt
         1 .: package body Ops is
         2 .:    procedure Apply
         3 .:      (Op :        Op_Kind;
         4 .:       X  : in out Integer) is
         5 .:    begin
         6 +:       case Op is
         7 +:          when Increment => X := X + 1;
         8 -:          when Decrement => X := X - 1;
         9 -:          when Double    => X := X * 2;
        10 -:          when Half      => X := X / 2;
        11 .:       end case;
        12 .:    exception
        13 .:       when others =>
        14 -:          null;
        15 .:    end Apply;
        16 .: end Ops;

* Coverage information appears after the line number, where

    **.** indicates uncoverable line

    **+** means covered line

    **-** means uncovered line

=====
Lab
=====

.. include:: labs/cover_020_basic_workflow.lab.rst

