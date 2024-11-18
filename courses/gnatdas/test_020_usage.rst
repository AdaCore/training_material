*******
Usage
*******

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==========
Overview
==========

-------------------------
Test Generation Methods
-------------------------

* Framework Generation Mode

   * Used to generate framework for writing individual unit tests

      * Test drivers, stubs, etc

   * Creates one executable to run all tests

*  Test Execution Mode

   * Used to generate a driver to call individual test executables

=========================
Simple Test Generation
=========================

-------------------------
Building a Test Harness
-------------------------

* Build test harness for a simple project

   :command:`gnattest --harness-dir=driver -P default.gpr`

   * Where ``--harness-dir=driver`` creates the test harness in a folder called :filename:`driver` inside the :filename:`obj` directory

* To run the driver, build and run the executable in the :filename:`obj/driver` folder

.. container:: latex_environment tiny

  ::

    cd obj/driver
    gprbuild -P test_driver
    test_runner

  Gives the result::

     simple.ads:3:4: error: corresponding test FAILED: Test not implemented. (simple-test_data-tests.adb:44)
     simple.ads:7:4: error: corresponding test FAILED: Test not implemented. (simple-test_data-tests.adb:65)
     2 tests run: 0 passed; 2 failed; 0 crashed.

* Note that the tests fail!

  * We have only built a harness - it's up to the tester to implement the test

---------------------
Test Data Structure
---------------------

* :toolname:`GNATtest` builds a child package for each unit (e.g. :ada:`Simple`) to test called :ada:`Simple.Test_Data` which contains

   * Type :ada:`Test` to contain test information

      * Extensible by tester if necessary

   * :ada:`Set_Up`/:ada:`Tear_Down` procedures to call before/after test execution

      * Useful for initialize and verify global data

* :toolname:`GNATtest` also builds child package :ada:`Simple.Test_Data.Tests` containing test driver for each visible subprogram

   * :ada:`Test_XXXX_YYYY` where **XXXX** is the subprogram name and **YYYY** is a unique identifier (prevents overloading/scoping issues)
   * Implementation seeded with failure case ("Test not implemented") - should be replaced with test implementation

* When editing generated files, make sure **not** to edit between *begin read only* and *end read only* comments

  * Anywhere else will remain when test harness is regenerated

------------------
Test Case Format
------------------

* Test example

   .. code:: Ada
     :number-lines: 33

      --  begin read only
         procedure Test_Inc (Gnattest_T : in out Test);
         procedure Test_Inc_4f8b9f (Gnattest_T : in out Test) renames Test_Inc;
      --  id:2.2/4f8b9f38b0ce8c74/Inc/1/0/
         procedure Test_Inc (Gnattest_T : in out Test) is
         --  simple.ads:3:4:Inc
      --  end read only

            pragma Unreferenced (Gnattest_T);

         begin

            AUnit.Assertions.Assert
              (Gnattest_Generated.Default_Assert_Value,
               "Test not implemented.");

      --  begin read only
         end Test_Inc;
      --  end read only

   * Line 33-39 - test declaration *(do not modify)*
   * Line 41 - suppress unused parameter warning (if necessary)
   * Line 45-47 - Test assertion (if first parameter is :ada:`False`, test fails - print second parameter)
   * Line 49-51 - end of test *(do not modify)*

* By default, :ada:`Default_Assert_Value` is :ada:`False`, so that unimplemented tests fail

  * It is possible to change the value to :ada:`True` so that unimplemented tests do not clutter report

---------------------
Test Implementation
---------------------

* For :ada:`Test_Inc`, we modify the test to verify that :ada:`Increment` succeeded

   .. code:: Ada
      :number-lines: 45

      AUnit.Assertions.Assert
        (Inc(1) = 2,
         "Incrementation failed");

* Then we rerun the test

.. container:: latex_environment tiny

  ::

    gprbuild -P test_driver
    test_runner

  Giving the result::

     simple.ads:3:4: info: corresponding test PASSED
     simple.ads:7:4: error: corresponding test FAILED: Test not implemented. (simple-test_data-tests.adb:66)
     2 tests run: 1 passed; 1 failed; 0 crashed.

=====
Lab
=====

.. include:: labs/test_020_usage.lab.rst

