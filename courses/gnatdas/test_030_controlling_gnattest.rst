**********************
Controlling GNATtest
**********************

.. PRELUDE:: BEGIN

.. PRELUDE:: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE:: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE:: REQUIRES

.. PRELUDE:: PROVIDES

.. PRELUDE:: END

==========
Overview
==========

---------------------------
Controlling Test Behavior
---------------------------

* Two ways to affect test behavior

   * Internally
   * Externally

* Internal control comes from modifying the original source or the test driver

* External control comes from modifying the switches used to create or run the harness

===========================
Source-based Test Control
===========================

-------------
Global Data
-------------

* Many subprograms require global data initialization

   * Memory allocation
   * State values

* Test pass/fail criteria can depend on global state values

* Could build these into each individual test

   * But what if the values are common across multiple tests?

--------------------------------
Common Pre-/Post-Test Behavior
--------------------------------

* Unit's test data package (e.g. :ada:`Simple.Test_Data`) contains two visible subprograms

   * :ada:`Set_Up` is called before every test case is run

      * Allows initialization of global state

   * :ada:`Tear_Down` is called after every test case is run

      * Allows adding checks for global state

* Found in :filename:`<unit>-test_data.adb`

   .. code:: Ada

      procedure Set_Up (Gnattest_T : in out Test) is
         pragma Unreferenced (Gnattest_T);
      begin
         --  Clear stack before running test
         Simple_Stack.Reset;
      end Set_Up;

      procedure Tear_Down (Gnattest_T : in out Test) is
         pragma Unreferenced (Gnattest_T);
      begin
         Ada.Text_IO.Put_Line ("Count:" & Simple.Stack.Count'Image);
      end Tear_Down;

----------------------------
Passing Data Between Tests
----------------------------

* Notice that :ada:`Set_Up` and :ada:`Tear_Down` (in addition to each **Test** procedure) pass parameter :ada:`Gnattest_T` of type :ada:`Test`

   * Defined in **<unit>.Test_Data**

      .. code:: Ada

         package Simple_Stack.Test_Data is

         --  begin read only
            type Test is new AUnit.Test_Fixtures.Test_Fixture
         --  end read only
            with null record;

   * Note that the completion of the record type is outside of the *read only* block allowing you to modify it as you see fit

* Parameter of type :ada:`Test` is passed to :ada:`Set_Up` and :ada:`Tear_Down` and every test

   * Allows passing of any user-defined data

---------------------------------
Changes to Original Source Code
---------------------------------

* What happens when testing finds a bug?

   * Your source code needs to be modified
   * But does the test infrastructure need to be updated?

* :toolname:`GNATtest` can be run multiple times on a project

   * Any existing test will not be modified as long as

      * Subprogram name is the same
      * Full Ada names and order of parameters are the same
      * Test's *begin/end read only* comments are intact

   * Any added subprogram will get a new driver

=============================
Test Harness File Structure
=============================

------------------------
Default File Structure
------------------------

* By default, two folders are created in project's object directory

   * :filename:`driver` contains the main driver for the test runner

      * Not for modification by the user

   * :filename:`gnattest` contains the modifiable test harness

      * But do not edit inside the *begin/end read only* comments!

* GNAT typically puts all files it generates in the project's object directory

   * So we tend to set up source code control to ignore the object directory

* But we **do** want to control the tests we've created

-----------------------------------
Controlling Test Harness Location
-----------------------------------

* :filename:`driver` folder is always auto-generated - do **not** want to save it

* :filename:`gnattest` folder contains our test cases - **do** want to save it

* Three (mutually exclusive) ways to control this

   * :command:`--tests-dir=dirname`

      * Put all tests in :filename:`dirname`

   * :command:`--tests-root=dirname`

      * :filename:`dirname` will mirror the source directory hierarchy
      * Tests for units in each source directory go in the corresponding directory within :filename:`dirname`

   * :command:`--subdirs=dirname`

      * :filename:`dirname` will be created *inside* each appropriate source directory
      * Tests for units in source directory go in :filename:`dirname` subdirectory

* Notes

   * If :filename:`dirname` is relative, it will be relative to the object directory
   * If your GPR file uses ``source_dir/**``, you should not use :command:`subdirs`

      * And if using the other options, do not put them in your source folders

==================================
External Test Control (Switches)
==================================

-----------------------
Default Test Behavior
-----------------------

* Default test behavior is to fail on an unimplemented test

   * Value of :ada:`Default_Assert_Value` is set to :ada:`True`

* Can be controlled at generation or execution

   :command:`--skeleton-default=xxxx` (where **xxxx** is *pass* or *fail*)

   * When used during build (:command:`gnattest --skeleton-default=pass`) :ada:`Default_Assert_Value` is initialized based on value

   * When used during execution (:command:`test_runner --skeleton-default=pass`) :ada:`Default_Assert_Value` is set based on value

-----------------
Common Switches
-----------------

:command:`-U <source file>`
   Only build tests for *source file* and any of its dependents

:command:`--no-subprojects`
   Only process base project

:command:`--files=<filename>`
   Process files listed in *filename* (switch may appear multiple times)

:command:`--ignore=<filename>`
   Ignore files listed in *filename*

:command:`--passed-tests=val`
   *val* can be either *show* or *hide* to either display (or not display) passed tests

:command:`--separate-drivers[=val]`
   Generate separate test driver for each unit or test. (*val* can be either *unit* or *test*, defaulting to *unit*)

=====
Lab
=====

.. include:: labs/test_030_controlling_gnattest.lab.rst
