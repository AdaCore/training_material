*****************************
Advanced Testing Techniques
*****************************

.. PRELUDE: BEGIN

.. PRELUDE: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE: REQUIRES

.. PRELUDE: PROVIDES

.. PRELUDE: END

==========
Overview
==========

--------------------------
Improving Test Execution
--------------------------

* By default, :toolname:`GNATtest` builds a monolithic test driver

   * One executable to run all tests
   * Suitable for small to medium projects

* But that has limitations

   * Every test runs in succession

      * No way to run multiple tests at once

   * Larger projects can create a massive test executable

* :toolname:`GNATtest` has a mechanism to build multiple executables

   * Tests grouped by unit or test

------------------------------
Control Over Dependent Units
------------------------------

* When testing a unit, sometimes it is easier to test in isolation

   * Control over dependent unit calls

      * Verify data passed in
      * Control data being returned

* :toolname:`GNATtest` allows :dfn:`stubs` to be created for dependent units

   * Add verification process to data passed in
   * Set output or return values

* Stubs are used for all dependents of units being tested

=========================
Individual Test Drivers
=========================

--------------
Example Code
--------------

* Main unit

   .. code:: Ada

      package Simple is
         function Inc (X : Integer) return Integer;
         function Dec (X : Integer) return Integer;
      end Simple;

* Which depends on 

   .. code:: Ada

      package Dependent is
         procedure Yes_Or_No (Aaa : in     Integer;
                              Bbb : in out Integer;
                              Ccc :    out Boolean);
      end Dependent;

----------------------------------
Building Multiple Test Harnesses
----------------------------------

* :toolname:`GNATtest` can build multiple test harnesses

   * :command:`gnattest --separate-drivers=[unit|test]`

   unit
      Builds an executable for every package (for our example, :ada:`Simple` and :ada:`Dependent`)

   test
      Builds an executable for every test (for our example, :ada:`Inc`, :ada:`Dec`, :ada:`Yes_Or_No`)

* Then build the individual test drivers

   * :command:`gprbuild -P obj/gnattest/harness/test_drivers.gpr`

---------------------------------
Running Multiple Test Harnesses
---------------------------------

:command:`gnattest <test_drivers.list>`

   * Where :filename:`test_drivers.list` is a file containing a list of executables
   * Default version of list is in :filename:`obj/gnattest/harness/test_drivers.list`

      * Can be edited in-place or copied

.. container:: latex_environment tiny

   ::

      dependent.ads:2:4: error: corresponding test FAILED: Test not implemented. (dependent-test_data-tests.adb:44)
      simple.ads:7:4: error: corresponding test FAILED: Test not implemented. (simple-test_data-tests.adb:65)
      simple.ads:3:4: error: corresponding test FAILED: Test not implemented. (simple-test_data-tests.adb:44)
      3 tests run: 0 passed; 3 failed; 0 crashed.

============
Test Stubs
============

-----------------
What Is a Stub?
-----------------

* Stub is a piece of code that replaces the actual body of a unit if

   * Unit has not been implemented yet
   * Unit is hardware-dependent and hardware is not available
   * Specific unit results are difficult to control

      * For when you need a specific value to test your code

* Useful when you need to test one module without worrying about dependencies

----------------
Creating Stubs
----------------

:command:`gnattest --stub -P default.gpr`

   * Creates stubs and drivers for all units
   * Every dependent of unit being tested is stubbed

      * Including generics

   * Stub harnesses are in :filename:`gnattest_stub`

      * Rather than :filename:`gnattest`
      * Both folders can exist!

* Stubs are common across units

   * Mutiple test drivers call the same stub
   * Stub control handled by test

-------------------
Controlling Stubs
-------------------

* Setter routines for setting output/return values

   * Manipulate a global object containing stub information
   * Reside in package :ada:`Dependent.Stub_Data`
   * Typically called from test driver

* Can edit stub implementation directly

   * Add assertions to verify data passed in is correct
   * In :filename:`stubs` subfolder in folder named for project
   * Can add your own processing

      * e.g. Raise an exception on a specific input or after some number of calls

=========
Example
=========

-------------------
Code to Be Tested
-------------------

.. code:: Ada

   with Sensor;
   package Simple is
      procedure Check (Which  :        Sensor.Sensor_T;
                       Value  : in out Integer;
                       Status :    out Boolean);
   end Simple;

   with Logger;
   package body Simple is
      procedure Check (Which  :        Sensor.Sensor_T;
                       Value  : in out Integer;
                       Status :    out Boolean) is
      begin
         Value   := Sensor.Read (Which);
         Status := True;
         case Which is
            when Sensor.Speed =>
               if Value < 0 or Value > 99 then
                  Status := False;
                  Logger.Log_Error ("Invalid Speed");
               end if;
            when others =>
               null;
         end case;
   end Simple;

-----------------
Dependent Units
-----------------

.. code:: Ada

   package Logger is
      procedure Log_Error (Message : String);
   end Logger;

   package Sensor is
      type Sensor_T is (Speed, Heading, Altitude);
      function Read (Which : Sensor_T) return Integer;
   end Sensor;

*Implementation of these units is unimportant*

----------------
Building Tests
----------------

* No matter how the dependent units are implemented, the tests should be the same

:filename:`simple-test_data-tests.adb`

.. code:: Ada

   --  begin read only
      procedure Test_Check (Gnattest_T : in out Test);
      procedure Test_Check_0265af (Gnattest_T : in out Test) renames Test_Check;
   --  id:2.2/0265af9a17cc096e/Check/1/0/
      procedure Test_Check (Gnattest_T : in out Test) is
      --  simple.ads:3:4:Check
   --  end read only

         pragma Unreferenced (Gnattest_T);

         Value :  Integer := 0;
         Status : Boolean;

      begin

         -- Test 1
         Check (Sensor.Speed, Value, Status);
         AUnit.Assertions.Assert
           (Value in 0..99 and Status,
            "Valid speed not detected");

         -- Test 2
         Check (Sensor.Speed, Value, Status);
         AUnit.Assertions.Assert
           (not (Value in 0..99) and not Status,
            "Invalid speed not detected");

   --  begin read only
      end Test_Check;
   --  end read only

--------------------------
Setting Stub Return Data
--------------------------

* To make sure :ada:`Check` passes each test, we should stub :ada:`Sensor`

   * To control the value returned by :ada:`Sensor.Read`::

      gnattest -P default.gpr --stub
      gprbuild -P obj/gnattest_stub/harness/test_drivers.gpr

* Method 1 - use the setter function with :ada:`Test_Check`

   .. code:: Ada

      -- Test 1
      Set_Stub_Read_cac9ed_9101fc (Read_Result => 12);
      Check (Sensor.Speed, Value, Status);
      AUnit.Assertions.Assert
        (Value in 0..99 and Status,
         "Valid speed not detected" & value'Image & " " & status'Image);

      -- Test 2
      Set_Stub_Read_cac9ed_9101fc (Read_Result => 234);
      Check (Sensor.Speed, Value, Status);
      AUnit.Assertions.Assert
        (not (Value in 0..99) and not Status,
         "Invalid speed not detected");

* Method 2 - edit the stub directly :filename:`obj/gnattest_stub/stubs/default/sensor.adb`

   .. code:: Ada

      --  begin read only
      function Read
        (Which : Sensor_T) return Integer is
      --  end read only
      begin
         Stub_Data_Read_cac9ed_9101fc.Stub_Counter := Stub_Data_Read_cac9ed_9101fc.Stub_Counter + 1;
         if Stub_Data_Read_cac9ed_9101fc.Stub_Counter > 1 then
            return -1;
         else
            return Stub_Data.Stub_Data_Read_cac9ed_9101fc.Read_Result;
         end if;
      --  begin read only
      end Read;
      --  end read only
   
=====
Lab
=====

.. include:: labs/test_040_advanced_testing.lab.rst
