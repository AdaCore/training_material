
*************
Interfacing
*************
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

-----------------------------
System and SPARK Boundaries
-----------------------------

* A program has to interact with outside entities

   - With values that represent the external environment

      + Such as temperature, pressure, keyboard and display

   - With other software systems, possibly in a different language

      + For example, libraries and device drivers

   - An important design step is to identify the system boundary, where it interacts with such entities, and define a precise interface

* It is not always possible to write a whole system in SPARK

   - It is then important to clearly identify what parts are in SPARK and those that are not and define a clear interface between them

* This is the **SPARK boundary**
* The **System Boundary** and the **SPARK boundary** are the same if the system is written entirely in SPARK

-------------------------------------------
Interaction With the External Environment
-------------------------------------------

.. image:: interaction_with_environment.png

==================
External Objects
==================

------------------------------------
Monitored and Controlled Variables
------------------------------------

* A monitored variable may be **volatile**

   - Each time the monitored variable is read it may have a different value
   - For example reading the temperature or speed

* Updating a controlled variable may not have a discernable effect on the program

   - The writing of a value to a controlled variable may appear to be ineffective to flow analysis

--------------------------------------------
Monitored and Controlled Variable Examples
--------------------------------------------

* A simple example demonstrating the issues raised by monitored and controlled variables

      .. code:: Ada

         procedure Monitored_And_Controlled_Issues with
            SPARK_Mode,
            Global => (Input  => Monitored_Var,
                       Output => Controlled_Var)
         is
         begin
            -- Assertion may fail because successive reads of a
            -- monitored variable may have different values
            pragma Assert (Monitored_Var = Monitored_Var);
            -- This value may be significant to the
            -- outside environment as well as next value written
            Controlled_Var := 0;
            Controlled_Var := Monitored_Var;
         end Monitored_And_Controlled_Issues;

* Analysis incorrectly gives:

      .. code:: console

         monitored_and_controlled_issues.adb:5:4:
             info: assertion proved
         monitored_and_controlled_issues.adb:6:19:
             warning: unused assignment

-------------------------------------------------
Representing Monitored and Controlled Variables
-------------------------------------------------

* SPARK represents the special status of monitored and controlled variables by external state

   - This allows the tools (:toolname:`GNATprove`) to treat correctly
   - External state representing a volatile monitored variable
   - External state representing a controlled variable

===============
Object States
===============

----------------
External State
----------------

.. container:: columns

 .. container:: column

    * Model volatile state: actuators, sensors, queues...

 .. container:: column

    .. image:: valves_and_sensors.jpeg

-----------------------------------
Package Contracts: External State
-----------------------------------

* The external state contract is specified using four Boolean properties:

   - `Async_Readers`

      + It is an output: external entities may read the state

   - `Async_Writers`

      + It is an input: external entities may write to the state

   - `Effective_Reads`

      + Each value read is significant

   - `Effective_Writes`

      + Every write is significant

* Only apply properties to external states that are volatile

   - They do not apply to non-volatile (ordinary) external states

---------------------------------
External State: `Async_Readers`
---------------------------------

* `Async_Readers => True`

   - A component of the system external to the program might read a value written to an external state

.. code:: Ada

   with System.Storage_Elements;
   package Output_Port
   is
     Actuator : Integer
       with Volatile, -- A variable representing an external
                      -- state is normally volatile
            Async_Readers, -- Async_Readers => external readers
            Address => System.Storage_Elements.To_Address
                       (16#ACECAFE#);
   end Output_Port;

---------------------------------
External State: `Async_Writers`
---------------------------------

* `Async_Writers => True`

   - A component of the system external to the program might update the value of an external state (i.e. each successive read might give a different value)

   .. code:: Ada

      with System.Storage_Elements;
      package Input_Port is
        Sensor : Integer with Volatile,
                 Async_Writers, -- indicates external writers
                 Address => System.Storage_Elements.To_Address
                            (16#ACECAFE#);
      end Input_Port;

   - If neither `Async_Writers` nor `Async_Readers` is specified for a volatile variable then the External State specification defaults to:

      .. code:: Ada

         Async_Readers => True, Async_Writers => True
         Effective_Reads => True, Effective_Writes => True

-----------------------------------
External State: `Effective_Reads`
-----------------------------------

* `Effective_Reads => True`

   - Every read of the external state is significant

      + For example, `Effective_Reads` would typically be True for a serial data port but False for a port providing the current speed value

.. code:: Ada

   -- A type can be declared as volatile
   type Volatile_Type is mod 2**8 with Volatile;
   Volatile_Input : Volatile_Type
      with Async_Writers,
      Effective_Reads, -- Effective_Reads can only be True
                       -- if Async_Writers is true
      Address => System.Storage_Elements.To_Address
                 (16#ABCCAFE#);

-----------------------------------
External State: `Effective_Reads`
-----------------------------------

* `Effective_Reads` - how does it affect analysis?

   - Consider the following simple example in which `Async_Writers` is true for `Volatile_Input`

      .. code:: Ada

         if B then
            Temp := Volatile_Input;
         end if;
         Temp := Volatile_Input;

   - What is the correct flow relation if `Effective_Reads` is true for `Volatile_Input`?
   - What is the correct flow relation if `Effective_Reads` is false for `Volatile_Input`?

-----------------------------------
External State: `Effective_Reads`
-----------------------------------

* `Effective_Reads` - how does it affect analysis?

   .. code:: Ada

      if B then
         Temp := Volatile_Input;
      end if;
      Temp := Volatile_Input;

* If `Effective_Reads` is True then `Temp` will depend on `B`

   - (for example, if reading in characters from a buffer, `B` will determine whether `Temp` ends up holding the first or the second character)

* If `Effective_Reads` is False then `Temp` will not depend on `B`

   - (for example if reading the current temperature from a sensor)
   - The first assignment to `Temp` will marked as ineffective

------------------------------------
External State: `Effective_Writes`
------------------------------------

* `Effective_Writes => True`

   - Every value written is significant

      + For example, when writing a stream of data to a serial output port `Effective_Writes` should be True
      + Conversely, when writing to a simple indicator lamp two successive writes of `Turn_On` have no discernable effect and therefore `Effective_Writes` should be False

.. code:: Ada

   type Volatile_Type is mod 2**8 with Volatile;
   Volatile_Output : Volatile_Type
      with Async_Readers,
      -- Effective_Writes can only be True
      -- if Async_Readers is True
      Effective_Writes,
      Address => System.Storage_Elements.To_Address
                 (16#ADACAFE#);

--------------------------------------
External State - Where To Specify It
--------------------------------------

* In the examples seen so far

   - The variables which represent the external state are visible and so are their external state contracts

* Good programming practice recommends that:

   - Variables which represent external state are not visible at library level
   - They should be represented by a state abstraction having an external state contract consistent with the external state contracts of all of its constituents

.. code:: Ada

   package P with Abstract_State =>
      (Input_Port with External => -- It's an input port
         (Async_Writers,
          -- Not the case that every is read significant
          Effective_Reads => False))
   is

----------------------------------------
Example: A Simple Sensor Specification
----------------------------------------

* Specification

   - The sensor port is not visible

.. code:: Ada

   package Water_High with
      SPARK_Mode,
      Abstract_State =>
      -- State abstraction Sensor declared as external input
         (Sensor with External => Async_Writers),
      Initializes => Sensor -- The sensor is pre-initialized
   is
      procedure Is_Active (Active : out Boolean) with
         -- Sensor is a Global Input of Is_Active
         Global => Sensor;
   end Water_High;

--------------------------------
A Simple Sensor Implementation
--------------------------------

* Body (1)

   - The state abstraction `Sensor` is refined on to the external state variable `High_Sensor_Port`

.. code:: Ada

   with System.Storage_Elements;
   package body Water_High with
      SPARK_Mode,
      -- The refinement of Sensor
      Refined_State => (Sensor => High_Sensor_Port)
   is
      type Sensor_Values is range 0 .. 200;
      Active_Value : constant Sensor_Values := 200;
      -- Declaration of High_Sensor_Port with external state
      -- contract of Volatile with property
      -- Async_Writers - it is an input
      High_Sensor_Port : Sensor_Values with
         Address => System.Storage_Elements.To_Address
                    (16#FFFF_FFF0#),
         Volatile,
         Async_Writers;

----------------------------------------
A Simple Sensor Implementation - Query
----------------------------------------

* Body (2)

   - The body (implementation) of `Is_Active`

.. code:: Ada

   procedure Is_Active (Active : out Boolean) with
      -- Refined_Global contract in terms of High_Sensor_Port
      Refined_Global => High_Sensor_Port
   is
      Raw_Value : Sensor_Values;
   Begin
      -- High_Sensor_Port is allowed as RHS of assignment
      Raw_Value := High_Sensor_Port;
      pragma Warnings (Off,
            "attribute Valid is assumed to return True");
      if Raw_Value'Valid then
         -- Value read from external state might not be valid
         Active := Raw_Value = Active_Value;
      else
         Active := True; -- "safe" value
      end if;
   end Is_Active;

------------------------------------
Example: A More Complex I/O Device
------------------------------------

* An output device with the following specification:

   - if value to write equals last value written

      + set success to true

   - else

      + store value to write in last value written
      + write value to write to output register
      + wait for acknowledgement or time out
      + if acknowledgement received then set success to true otherwise false

------------------------------------------
A More Complex I/O Device Specification
------------------------------------------

* The package `Device` declaration

.. code:: Ada

   package Device with
      SPARK_Mode,
      -- The state abstraction representing the external state
      -- is complex. From spec given earlier it has volatile
      -- read and write constituents as well as a non-volatile
      -- constituent
      Abstract_State => (State with External =>
         (Async_Readers, Effective_Writes, Async_Writers)),
      Initializes => State
   is
      procedure Write (X : Integer;
                       Success : out Boolean) with
         -- In the Global_Contract of Write, State has mode
         -- In_Out, as it is both an input and an output
         Global => (In_Out => State);
         -- A function is possible with Global input of State
         -- provided it only uses constituents without the
         -- Async_Writers property
      function Last_Written return Integer with
         Global => State;
   end Device;

------------------------------------------
A More Complex I/O Device Implementation
------------------------------------------

* The refinement of `Device.State`

.. code:: Ada

   with System.Storage_Elements;
   package body Device with
      SPARK_Mode,
      -- State is refined into both volatile
      -- and non-volatile constituents
      Refined_State => (State =>
                       (Old_X, Status_Port, Register))
   is
      Old_X : Integer := 0; -- Non-volatile constituent
      Status_Port : Integer with
         Address => System.Storage_Elements.To_Address
                    (16#FFFF_FFF0#),
         Volatile,
         Async_Writers; -- Constituent that is an input
      Register : Integer with
         Address => System.Storage_Elements.To_Address
                    (16#FFFF_FFF4#),
         Volatile,
         Async_Readers,
         Effective_Writes; -- Constituent that is an output

---------------------------------------------------
A More Complex I/O Device Implementation - Writer
---------------------------------------------------

* The implementation of `Write`

.. code:: Ada

   Read_Ack : constant := -1;
   subtype Wait_Time is Natural range 1 .. 20000;
   procedure Write (X : Integer; Success : out Boolean) with
      -- Refined_Global contract in terms of
      -- constituents of Device.State
      Refined_Global => (Output => Register,
                         Input => Status_Port,
                         In_Out => Old_X)
   is
      Status_Value : Integer;
   begin
      if X /= Old_X then
         Old_X := X;
         Register := X; -- Write to output port
         for I in Wait_Time loop
            -- Read of input port is allowed on
            -- RHS of assignment statement
            Status_Value := Status_Port;
            Success := Status_Value = Read_Ack;
            exit when Success;
         end loop;
      else
         Success := True;
      end if;
   end Write;

--------------------------------------------------
A More Complex I/O Device Implementation - Query
--------------------------------------------------

* The implementation of `Last_Written`

   .. code:: Ada

      function Last_Written return Integer is (Old_X) with
         Refined_Global => Old_X;

   - `Refined_Global` contract in terms of the non-volatile constituent `Old_X` only

* **Note:** A `Refined_Global` contract does not need to denote all of the constituents of the refinement

   - Only those constituents used by the subprogram (and subprograms it calls)

* The mode of the constituents in the `Refined_Global` contract has to be consistent with their use in the subprogram

   - In the above example `Old_X` is only read by the subprogram

----------------------------------
Initialization Of External State
----------------------------------

* It may be impossible to explicitly initialize external state

   - It may be a read only port
   - Writing a spurious value to an output port could be erroneous
   - These types of external state are effectively pre-initialized

* An external state abstraction should have an `Initializes` contract when it is read before being updated

   - This will require all of its constituents to be initialized
   - Constituents which are external state variables with the property `Async_Writers` do not need explicit initialization
   - Non-volatile constituents can be initialized
   - But it may not be possible to explicitly initialize external state variables with the property `Async_Readers`

--------------------------------------------
Initialization Of External State Variables
--------------------------------------------

* In the example of a simple sensor `Water_High`

   - The specification has its external state abstraction initialized as the state abstraction is read and never updated by the program

      .. code:: Ada

         package Water_High with
            SPARK_Mode,
            Abstract_State =>
               (Sensor with External => Async_Writers),
            Initializes => Sensor
         is

   - But the body contains no explicit initializations because:

      + `High_Sensor_Port` is a external state variable with the property `Async_Writers` and does not require an explicit initialization
      + It may be a read-only port

      .. code:: Ada

         High_Sensor_Port : Sensor_Values with
            Address => System.Storage_Elements.To_Address
                       (16#FFFF_FFF0#),
            Volatile,
            Async_Writers;

--------------------------------------------
Initialization Of External State Variables
--------------------------------------------

* In the example more complex I/O device

   - The specification has its external state abstraction initialized as the state abstraction is always read before being updated

      + It has no subprogram with global state as mode `Output`

      .. code:: Ada

         package Device with
            SPARK_Mode,
            Abstract_State => (State with External =>
            (Async_Readers, Effective_Writes, Async_Writers)),
            Initializes => State
         is

   - But the state abstraction `State` represents both volatile and nonvolatile constituents
   - The non-volatile constituent can be initialized

      .. code:: Ada

         Refined_State => (State => (Old_X, Status_Port, Register))
         is
         Old_X : Integer := 0;

--------------------------------------------
Initialization Of External State Variables
--------------------------------------------

* In the example more complex I/O device

   - The constituent `Status_Port` does not require explicit initialization because it has the property `Async_Writers`

      .. code:: Ada

         Status_Port : Integer with
            Address => System.Storage_Elements.To_Address
                       (16#FFFF_FFF0#),
            Volatile,
            Async_Writers;

   - The constituent `Register` is volatile and has the properties `Async_Readers` and `Effective_Writes`
   - It may be erroneous to write a spurious value to this external state

      .. code:: Ada

         Register : Integer with
            Address => System.Storage_Elements.To_Address
                       (16#FFFF_FFF4#),
            Volatile,
            Async_Readers,
            Effective_Writes;

========
Lab
========

.. include:: labs/160_interfacing.lab.rst

==========
Summary
==========

----------
Summary
----------

* External data requires documentation

   - How it behaves when read / written

* Data should be hidden from view

   - But state is important for clients
