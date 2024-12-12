========================
Some Advanced Concepts
========================

------------
Priorities
------------

.. container:: columns

 .. container:: column

  * Set by a :ada:`pragma Priority` or :ada:`Interrupt_Priority`

    - Can also use aspects
    - Tasks
    - Main subprogram (environment task)
    - :ada:`protected` definition

  * Lower values mean lower priority

    - :ada:`Priority`

      + At least 30 levels

    - :ada:`Interrupt_Priority`

      + At least 1 level
      + ``>`` :ada:`Priority`

 .. container:: column

    .. code:: Ada

       procedure Main is
         pragma Priority (2);

       task T is
         pragma Priority (4);

       protected Buffer is
          ...
       private
          pragma Priority (3);
       end Buffer;

-----------------
Ceiling Locking
-----------------

* Example of priority inversion:

.. code::

   L : Lock;

   T1 : Task (Priority => 1);
   T2 : Task (Priority => 2);
   T3 : Task (Priority => 3);

   T1 locks L
   T3 starts, get scheduled (T3 > T1)
   T3 tries to get L, blocks
   T2 starts, get scheduled (T2 > T1)

   Result: T2 running, T1 blocked, T3 blocked through L (but T3 > T2!)

* Solved with ceiling locking

    - Increase the priority of a task when it uses a protected

* Task priority is increased within a protected object

    - Condition: Task priority ``<=`` priorities of all protected objects it uses
    - Blocks other tasks without explicit locking

* :ada:`pragma Locking_Policy (Ceiling_Locking)`

    - Default on Ravenscar / Jorvik

-------------------------
Ceiling Locking Example
-------------------------

 .. code:: Ada

     protected P with Priority => 5 is
        procedure Set (V : Integer);

 .. code:: Ada

     task T with Priority => 4 is
       ...

     task body T is
       ...
       P.Set (1);

.. image:: ravenscar_ceiling_locking.png
   :width: 45%

------
Queue
------

* Protected :ada:`entry` are activated by **one** task at a time
* **Mutual exclusion** section
* Other tasks trying to enter

    - Are forbidden (Ravenscar)
    - Or are **queued** (Jorvik)

        + In **First-In First-Out** (FIFO) by default

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
       type Timing_Event_Handler is access protected procedure (
           Event : in out Timing_Event);
       procedure Set_Handler (Event   : in out Timing_Event;
                              At_Time : Time;
                              Handler : Timing_Event_Handler);
       function Current_Handler (Event : Timing_Event)
                                 return Timing_Event_Handler;
       procedure Cancel_Handler (Event     : in out Timing_Event;
                                 Cancelled : out Boolean);
       function Time_Of_Event (Event : Timing_Event)
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
    
    - Including calls to libraries, OS services...
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
* Controls interrupts attachment
* Always relative to library units' elaboration
* **Concurrent policy**

  - Activation at the end of declaration's scope elaboration
  - Ada default policy

* **Sequential policy**

  - Deferred activation and attachment until **all** library units are activated
  - Easier scheduling analysis

