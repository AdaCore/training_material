*******************
Ravenscar Tasking
*******************

.. include:: support_files/symbols.rst

==============
Introduction
==============

--------------------------------
What Is the Ravenscar Profile?
--------------------------------

* Use concurrency in embedded real-time systems

   - Verifiable
   - Simple (Implemented reliably and efficiently)

* Scheduling theory for accurate analysis of real-time behavior
* A subset of the Ada tasking model
* Defined to help meeting safety-critical real-time requirements

   - Determinism
   - Schedulability analysis
   - Memory-boundedness
   - Execution efficiency and small footprint
   - Suitability for certification

* State-of-the-art concurrency constructs

   - Adequate for most types of real-time software

===================================
Differences From Standard Tasking
===================================

-------------------------------
Ravenscar Tasking Limitations
-------------------------------

* Active synchronization not supported

   - Asymmetric rendezvous
   - Entries

* Tasks can only be declared at library level

   - All have to finish before the program terminates

* Protected object entries

   - Only one entry per protected object
   - Barriers can only be simple boolean values

---------------------------
Task Types with Ravenscar
---------------------------

* It is possible to create task types

   - Task objects can only be instantiated statically in Ravenscar
   - Can be instantiated on stack or heap in full Ada tasking model

* By default tasks are activated at the end of the elaboration of their library unit's declarative part

   - As if they were declared there
   - Activation can be deferred to the end of all elaboration

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

   - At most one entry
   - No queuing

      + Only one task can be blocked on the entry

   - Ceiling locking on monoprocessor

      + Bounded priority inversion
      + Efficient locking/unlocking by increasing/decreasing priority

   - "Proxy model" for protected entries

      + Avoid unneeded context switches

* Interrupt handling

   - Simple and efficient

      + Protected procedures as low level interrupt handlers
      + Masking hardware interrupts according to active priority

------------
Priorities
------------

.. container:: columns

 .. container:: column

    * Priorities are defined in package System

       - Lower values mean lower priority
       - Two non-overlapping ranges:

          + `Priority`
          + `Interrupt_Priority`

    * Priority is set by a :ada:`pragma Priority` or :ada:`pragma Interrupt_Priority`

       - Ignored for non-main subprograms

          + Set the priority of the environment task

    * `Interrupt_Priority` for priorities in the interrupt range

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
   - A higher priority task is woken up or unblocked

-----------------
Ceiling Locking
-----------------

.. container:: columns

 .. container:: column

   * Task priority is increased within a protected object

      - Priorities of task must be lower or equal than the priorities of protected objects used
      - Blocks other tasks
      - Performs locks without using locks

|

.. image:: ravenscar_ceiling_locking.png
   :width: 45%

 .. container:: column

   .. code:: Ada

      task T is
        pragma Priority(4);
        ...

      task body T is
        ...
        P.Set (1);
        ...

   .. code:: Ada

      protected P is
         pragma Priority(5);
         procedure Set
            V : Integer);

=================
Tasking Control
=================

--------------------------
Synchronous Task Control
--------------------------

* Provides primitives to construct synchronization mechanisms and two-stage suspend operations
* Package exports a `Suspension_Object` type

   - Values are "True" and "False", initially "False"
   - Such objects are awaited by one task but set by other tasks

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

   - Implemented as protected procedures

* Do not require a task or a delay statement
* Controlled via procedural interface

   - Links the protected procedure
   - Sets the time

* Ravenscar Run-time Interface

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

* Clocks start after creation but before activation
* Whenever the task executes the clock increments
* Total time includes run-time library and O.S. services executed on its behalf
* System and run-time library execution not specific to a given task may be assigned to some task(s)

   - Implementation-defined whether it does
   - Implementation-defined which task if it does

-------------------------------
Partition Elaboration Control
-------------------------------

* Library units are elaborated in an undefined order

   - They can declare tasks and interrupt handlers
   - Once elaborated tasks start executing
   - Interrupts occur as soon as hardware is enabled

* These are unacceptable race conditions

   - Especially for certification!

* :ada:`pragma Partition_Elaboration_Policy`

   - Controls when activation and attachment happens relative to library unit elaboration completion
   - Defined in High Integrity Systems Annex
   - **Concurrent policy**

      + Normal semantics: tasks and interrupts are concurrent with remaining library units' elaboration

   - **Sequential policy**

      + Task activation and interrupt handler attachment are deferred until library unit elaboration completes

-------------------------------
Task Termination Notification
-------------------------------

* Tasks silently terminate

   - Without notification by default

* User-defined handlers for termination

   - Essentially a task's "last wishes"
   - Handlers are protected procedures called by the run-time library

* States differentiated

   - Normal termination
   - Termination due to an unhandled exception
   - Termination due to task abort

* Ravenscar Run-time Interface

   .. code:: Ada

      package Ada.Task_Termination is
        type Termination_Handler is access protected procedure (
            T : Ada.Task_Identification.Task_Id);
        procedure Set_Dependents_Fallback_Handler (Handler : Termination_Handler);
        function Current_Task_Fallback_Handler return Termination_Handler;
      end Ada.Task_Termination;

=========
Summary
=========

---------------------------
Ravenscar Small FootPrint
---------------------------

.. container:: columns

 .. container:: column

    * Everything is done by the Ada run-time library

       - No OS underneath

    * Simple

       - Less than 2800 Logical SLOCs
       - Footprint for simple tasking program is 10KB

    * Static tasking model

       - Tasks descriptors and stacks are statically created at compile time
       - Task creation and activation is very simple
       - All tasks are created at initialization
       - Then all activated and executed according to their priority

 .. container:: column

    * Simple protected operations

       - No queuing
       - Locking/unlocking by increasing/decreasing priority

    * Complex features removed

       - Such as exception handling and propagation

    * ECSS (E-ST-40C and Q-ST-80C) qualification material
