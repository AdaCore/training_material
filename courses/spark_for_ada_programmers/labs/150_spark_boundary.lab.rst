=====
Lab
=====

--------------------
SPARK Boundary Lab
--------------------

- Find the :filename:`150_spark_boundary` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`lab.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P lab.gpr`

- Unfold the source code directory (.) in the project pane

.. note::

   The GPR file uses a configuration file to specify that SPARK mode defaults to
   "On" for all units in this project. (So you won't see :ada:`with SPARK_Mode;`
   in the source.)

-----------------------
System Boundary (1/2)
-----------------------

.. container:: animate 1-

   - Find and open the files :filename:`alarm.ads` and :filename:`alarm.adb` in
     :toolname:`GNAT Studio`

   - Run :menu:`SPARK` |rightarrow| :menu:`Prove File`

.. container:: animate 2-

   *Lots of errors, including:*

   :color-red:`alarm.ads:6:13: error: function "Get_Temperature" with volatile input global "Temperature" with effective reads is not allowed in SPARK`

   :color-red:`alarm.ads:8:13: error: function "Get_Status" with volatile input global "Status" with effective reads is not allowed in SPARK`

   *Without specifying volatility property,* :ada:`Effective_Reads` *is True (so*
   *a function read could cause a state change, which is a side effect)*

   - Specify correct volatility properties for :ada:`Temperature` and :ada:`Status`

     + :ada:`Temperature` can be written to at any time
     + :ada:`Status` can be read at any time, so consecutive writes are expected

.. container:: animate 3-

   .. code:: Ada

      Temperature : Integer with
        Address => System.Storage_Elements.To_Address (16#FFFF_FFF0#),
        Volatile,
        Async_Writers;

      Status : Alarm_Status := Off with
        Address => System.Storage_Elements.To_Address (16#FFFF_FFF4#),
        Volatile,
        Async_Readers,
        Effective_Writes;

   *Note: warnings about the address specification can be turned off*
   *by setting the aspect* :ada:`Warnings => Off` *for these objects*

-----------------------
System Boundary (2/2)
-----------------------

.. container:: animate 1-

   - Prove the file again and examine the errors

.. container:: animate 2-

   :color-red:`alarm.ads:6:13: error: nonvolatile function "Get_Temperature" with volatile input global "Temperature" is not allowed in SPARK [E0006]`

   *When* :ada:`Get_Temperature` *is called, the result is volatile,*
   *so successive calls can yield different results*

   - Tell the prover that the result of :ada:`Get_Temperature` is volatile

.. container:: animate 3-

   .. code:: Ada

      function Get_Temperature return Integer
        with Volatile_Function;

   - Run the prover again - should find one more problem!

.. container:: animate 4-

   :color-red:`alarm.adb:15:10: error: call to a volatile function in interfering context is not allowed in SPARK`

   *Reads of volatile functions should be stored.*

   - Update :ada:`Set_Status` to use the volatile function in a "non-interfering context"

.. container:: animate 5-

   .. code:: Ada

      procedure Set_Status is
         Current : Integer := Get_Temperature;
      begin
         if Current > 100 then
            Status := On;
         end if;
      end Set_Status;

---------------------------------------
Abstract States at the Boundary (1/2)
---------------------------------------

.. container:: animate 1-

   - Add an external state :ada:`State` with both :ada:`Temperature` and
     :ada:`Status` as constituents

.. container:: animate 2-

   *Hint: Global data needs to be part of the abstract state, and*
   *the state will need to be refined to show the actual objects*

.. container:: animate 3-

   *Package spec*

   .. code:: Ada

      package Alarm
          with Abstract_State => (Input_State, Output_State)
      is

   *Private section*

   .. code:: Ada

      Temperature : Integer with
        Part_Of => Input_State,
        ...

      Status : Alarm_Status := Off with
        Part_Of => Output_State,
        ...

   *Package body*

   .. code:: Ada

      package body Alarm
        with Refined_State => (Input_State => Temperature,
                               Output_State => Status)
      is

---------------------------------------
Abstract States at the Boundary (2/2)
---------------------------------------

.. container:: animate 1-

   - Examine the file again

.. container:: animate 2-

   :color-red:`alarm.adb:2:24: error: non-external state "Input_State" cannot contain external constituents in refinement`

   :color-red:`alarm.adb:3:24: error: non-external state "Output_State" cannot contain external constituents in refinement`

   *The state references external data - the prover must be made aware*

.. container:: animate 3-

   - Add indications of which states are external, and how they are used

.. container:: animate 4-

   .. code:: Ada

      package Alarm
        with Abstract_State =>
          ((Input_State with External => Async_Writers),
           (Output_State with External => (Async_Readers,
                                           Effective_Writes)))
      is

-------------------
Software Boundary
-------------------

.. container:: animate 1-

   - Find and open the files :filename:`random_numbers.ads` and :filename:`random_numbers.adb` in
     :toolname:`GNAT Studio`

   - Run :menu:`SPARK` |rightarrow| :menu:`Prove File`. What's the problem?

.. container:: animate 2-

   :color-red:`random_numbers.adb:5:4: error: "Generator" is not allowed in SPARK (due to entity declared with SPARK_Mode Off)`

   :ada:`GNAT.Random` *is not in SPARK mode; we cannot call non-SPARK from SPARK*

   - Turn off SPARK mode for :ada:`Random_Numbers`

.. container:: animate 3-

   .. code:: Ada

      package body Random_Numbers
        with SPARK_Mode => Off
      is

   *We only want the implementation to be out of SPARK. We*
   *still want to be able to call* :ada:`Random_Numbers` *from SPARK*

--------------------
Integration with C
--------------------

.. container:: animate 1-

   - Find and open the file :filename:`main.adb` in :toolname:`GNAT Studio`

   - Run :menu:`SPARK` |rightarrow| :menu:`Prove File`. What's the problem?

.. container:: animate 2-

   :color-red:`main.adb:12:4: warning: no Global contract available for "Swap"`

   :color-red:`main.adb:12:4: warning: assuming "Swap" has no effect on global items`

   :color-red:`main.adb:12:4: warning: no Always_Terminates aspect available for "Swap"`

   :color-red:`main.adb:12:4: warning: assuming "Swap" always terminates`

   *Because the implementation of* :ada:`Swap` *is external, the prover*
   *can not examine the body, so it has to make assumptions*

  - Fix the warnings with suitable annotations on the declaration of :ada:`Swap`

.. container:: animate 3-

   .. code:: Ada

      procedure Swap (X, Y : in out Integer)
      with
        Import,
        Convention => C,
        Global => null,
        Always_Terminates;
