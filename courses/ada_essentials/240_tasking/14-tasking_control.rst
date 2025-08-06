=================
Tasking Control
=================

--------------------------
Synchronous Task Control
--------------------------

* Primitives synchronization mechanisms and two-stage suspend operation

   - No critical section
   - More lightweight than protected objects

* Package exports a `Suspension_Object` type

   - Values are :ada:`True` and :ada:`False`, initially :ada:`False`
   - Such objects are awaited by (at most) one task

      + But can be set by several tasks

.. code:: Ada

   package Ada.Synchronous_Task_Control is
      type Suspension_Object is limited private;
      procedure Set_True (S : in out Suspension_Object);
      procedure Set_False (S : in out Suspension_Object);
      procedure Suspend_Until_True (S : in out Suspension_Object);
      function Current_State (S : Suspension_Object) return Boolean;
   private
      ...
   end Ada.Synchronous_Task_Control;

---------------
Timing Events
---------------

* User-defined actions executed at a specified wall-clock time

   - Calls back an :ada:`access protected procedure`

* Do not require a :ada:`task` or a :ada:`delay` statement

 .. code:: Ada

    package Ada.Real_Time.Timing_Events is
       type Timing_Event is tagged limited private;
       type Timing_Event_Handler is
          access protected procedure
             (Event : in out Timing_Event);
       procedure Set_Handler
          (Event   : in out Timing_Event;
           At_Time : Time;
           Handler : Timing_Event_Handler);
       function Current_Handler
          (Event : Timing_Event)
           return Timing_Event_Handler;
       procedure Cancel_Handler
          (Event     : in out Timing_Event;
           Cancelled : out Boolean);
       function Time_Of_Event
          (Event : Timing_Event)
           return Time;
    private
       ...
    end Ada.Real_Time.Timing_Events;

-----------------------
Execution Time Clocks
-----------------------

* Not specific to Ravenscar / Jorvik
* Each task has an associated CPU time clock

   - Accessible via function call

* Clocks starts at creation time

    - **Before** activation

* Measures the task's total execution time
    
    - Including calls to libraries, O/S services...
    - But not including time in a blocked or suspended state

* System and runtime also execute code

   - As well as interrupt handlers
   - Their execution time clock assignment is implementation-defined

-------------------------------
Partition Elaboration Control
-------------------------------

* Library units are elaborated in a partially-defined order

   - They can declare tasks and interrupt handlers
   - Once elaborated, tasks start executing
   - Interrupts may occur as soon as hardware is enabled

      * May be during elaboration

* This can cause race conditions

   - Not acceptable for certification

* :ada:`pragma Partition_Elaboration_Policy`

------------------------------
Partition Elaboration Policy
------------------------------

* :ada:`pragma Partition_Elaboration_Policy`

   - Defined in RM Annex H "High Integrity Systems"

* Controls tasks' activation
* Controls interrupt attachment
* Always relative to library units' elaboration
* **Concurrent policy**

  - Activation at the end of declaration's scope elaboration
  - Ada default policy

* **Sequential policy**

  - Deferred activation and attachment until **all** library units are activated
  - Easier scheduling analysis

