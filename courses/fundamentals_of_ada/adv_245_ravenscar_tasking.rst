*******************
Ravenscar Tasking
*******************

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------------------------
What Is the Ravenscar Profile?
--------------------------------

* A **subset** of the Ada tasking model

  + Defined in the RM D.13

* Use concurrency in embedded real-time systems

   - Verifiable
   - Simple (Implemented reliably and efficiently)

* Scheduling theory for accurate analysis of real-time behavior
* Defined to help meeting **safety-critical real-time** requirements

   - Determinism
   - Schedulability analysis
   - Memory-boundedness
   - Execution efficiency and small footprint
   - Certification

* State-of-the-art concurrency constructs

   - Adequate for most types of real-time software

* :ada:`pragma Profile (Ravenscar)`

-----------------------------
What Is the Jorvik profile?
-----------------------------

* A **non-backwards compatible update** to Ravenscar

  + Defined in the RM D.13 (Ada 2022)

* Remove some constraints

  - Scheduling analysis may be harder to perform

* Keep the same requirements
* This class is about the more popular Ravenscar

  + But Jorvik's improvements are evoked

* :ada:`pragma Profile (Jorvik)`

-------------------------
What are GNAT runtimes?
-------------------------

* The :dfn:`runtime` is an embedded library

  - Executing at run-time
  - In charge of standard's library support...
  - ...including tasking

* Standard runtime

  - Full runtime support
  - "Full-fledged" OS target (Linux, WxWorks...)
  - Large memory footprint
  - Full tasking (not shown in this class)

* Embedded runtime

  - Baremetal and RTOS targets
  - Reduced memory footprint
  - Most of runtime, except I/O and networking
  - Ravenscar/Jorvik tasking

* Light runtime

  - Baremetal targets
  - Very small memory footprint
  - Selected, very limited, runtime
  - Optional Ravenscar/Jorvik tasking (Light-tasking)

===================================
Differences From Standard Tasking
===================================

-------------------------------
Ravenscar Tasking Limitations
-------------------------------

* Active synchronization not supported

   - Asymmetric rendezvous
   - Task entries

* Tasks declaration **must be** at library level

   - All **must** finish before the program terminates

* Protected object entries

   - Only one entry per protected object

      + Unlimited in Jorvik

   - Barriers can only be simple boolean values

      + Typically blocking until a flag clears
      + Jorvik allows for any :dfn:`pure barriers`

---------------------------
Task Types with Ravenscar
---------------------------

* It is possible to create :ada:`task` types

   - Only **static** ones in Ravenscar, no :ada:`new`
   - No "task hierarchy": declaration at library-level

* Tasks are activated at the end of their library unit's declarative part

   - Can be deferred to the end of **all** elaboration

--------------------------------
Protected Types With Ravenscar
--------------------------------

* Like tasks, protected objects can be defined through types
* Instantiation can then be done on library level
* Protected object types are :ada:`limited` types

==================
Tasking Behavior
==================

--------------------
Ravenscar Patterns
--------------------

* Periodic tasks (cyclic tasks / time triggered)

   - Sensor data acquisition
   - System monitoring
   - Control loops
   - Display update

* Event driven tasks

   - Alarm, Timeout
   - Interrupt
   - Data from another task

* Protected objects to synchronize tasks

-----------------------------------
What Tasks Look Like in Ravenscar
-----------------------------------

* Time-triggered task

   .. code:: Ada

      task body Cyclic is
        Period : constant Time_Span : Milliseconds (10);
        Activation : Time := Clock;
      begin
        loop
          delay until Activation;
          Do_Something;
          --  Compute next activation time
          Activation := Activation + Period;
         end loop;
      end Cyclic;

* Event-triggered task

   .. code:: Ada

      task body Sporadic is
      begin
         loop
           -- Protected entry
           Monitor.Wait_Event;
           Do_Something;
         end loop;
      end Sporadic;

-----------------
Ravenscar Tasks
-----------------

.. container:: columns

 .. container:: column

    * Fixed set of tasks

       - Only at library level
       - No dynamic allocation
       - No nested task declarations
       - Fixed priority
       - Statically created

          + Task descriptors, stacks, ...

 .. container:: column

    * Each task is an infinite loop

       - Single "triggering" action (delay or event)
       - Tasks never terminate
       - No entries, no rendezvous
       - No abort

    * Task creation and activation is very simple

       - All tasks are created at initialization
       - Then all are executed according to their priorities

------------------------------------------
Protected Objects and Interrupt Handling
------------------------------------------

* Simple protected operations

   - No queuing (except in Jorvik)
   - :dfn:`Ceiling locking` on monoprocessor (see later)
   - :dfn:`Proxy model` for protected entries

      + Entry body executed by the active task on behalf of the waiting tasks
      + Avoid unneeded context switches

* Simple, efficient, interrupt handling

    - :dfn:`Interrupt entries`
    - Protected procedures as low level interrupt handlers
    - Procedure is :dfn:`attached` to interrupt
    - Interrupt masking follows active priority

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

------------
Scheduling
------------

* Priority based
* No time slicing (quantum)
* A task executes until ...

   - The task is blocked (on delays or on protected object entry)
   - A higher priority task is woken up or unblocked (preemption)

-----------------
Ceiling Locking
-----------------

* Example of priority inversion

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
* Task priority is increased within a protected object

    - Condition: Task priority ``<=`` priorities of all protected objects it uses
    - Blocks other tasks without explicit locking

* :ada:`pragma Locking_Policy (Ceiling_Locking)`

    - Default on Ravenscar

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

=================
Tasking Control
=================

--------------------------
Synchronous Task Control
--------------------------

* Primitives synchronization mechanisms and two-stage suspend operations

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

* Library units are elaborated in an undefined order

   - They can declare tasks and interrupt handlers
   - Once elaborated, tasks start executing
   - Interrupts may occur as soon as hardware is enabled

      * May be during elaboration

* This can cause race conditions

   - Not acceptable for certification

* :ada:`pragma Partition_Elaboration_Policy`

   - Defined in RM Annex H "High Integrity Systems"
   - Controls tasks activation
   - Controls interrupts attachment
   - Always relative to library units' elaboration
   - **Concurrent policy**

      + Ada default policy

   - **Sequential policy**

      + Deferred activation and attachment until **all** library units are activated
      + Default policy for Ravenscar and Jorvik profiles

-------------------------------
Task Termination Notification
-------------------------------

* Tasks silently terminate

   - Without notification by default

* User-defined handlers for termination

   - Essentially a task's "last wishes"
   - Runtime calls back :ada:`access protected procedure`

* Termination cause provided as argument

   - Normal termination
   - Termination due to an unhandled exception
   - Termination due to task abort (forbidden in Ravenscar)

* Ravenscar Run-time Interface

   .. code:: Ada

      package Ada.Task_Termination is
         type Termination_Handler is access protected procedure
          (Cause : in Cause_Of_Termination;
           T     : in Ada.Task_Identification.Task_Id;
           X     : in Ada.Exceptions.Exception_Occurrence);
         procedure Set_Dependents_Fallback_Handler (Handler : Termination_Handler);
         function Current_Task_Fallback_Handler return Termination_Handler;
      end Ada.Task_Termination;

=========
Summary
=========

---------------
Light-Tasking
---------------

.. container:: columns

 .. container:: column

    * Everything is done by the Ada runtime

       - No OS underneath

    * Simple

       - Less than 2800 Logical SLOCs
       - Footprint for simple tasking program is 10KB

    * Static tasking model

       - Static tasks descriptors and stacks created at compile time
       - Task creation and activation is very simple
       - All tasks are created at initialization

 .. container:: column

    * Simple protected operations

       - No queuing
       - Locking/unlocking by increasing/decreasing priority

    * Complex features removed

       - Such as exception handling and propagation

    * ECSS (E-ST-40C and Q-ST-80C) qualification material
