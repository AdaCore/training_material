*******************
Ravenscar Tasking
*******************

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

================
Introduction
================

--------------------------------
What Is the Ravenscar Profile?
--------------------------------

* A **subset** of the Ada tasking model

  + Defined in the RM D.13

* Use concurrency in embedded real-time systems

   - Verifiable
   - Simple (Implemented reliably and efficiently)

* Scheduling theory for accurate analysis of real-time behavior
* Defined to help meet **safety-critical real-time** requirements

   - Determinism
   - Schedulability analysis
   - Memory-boundedness
   - Execution efficiency and small footprint
   - Certification

* :ada:`pragma Profile (Ravenscar)`

-----------------------------
What Is the Jorvik profile?
-----------------------------

* A **non-backwards compatible profile** based on Ravenscar

  + Defined in the RM D.13 (Ada 2022)

* Removes some constraints

  - Scheduling analysis may be harder to perform

* Subset of Ravenscar's requirements
* This class is about the more widespread Ravenscar

  + But some of Jorvik's differences are indicated

* :ada:`pragma Profile (Jorvik)`

-------------------------
What Are GNAT Runtimes?
-------------------------

* The :dfn:`runtime` is an embedded library

  - Executing at run-time
  - In charge of standard's library support...
  - ...including tasking

* Standard runtime

  - Full runtime support
  - "Full-fledged" OS target (Linux, VxWorks...)
  - Large memory footprint
  - Full tasking (not shown in this class)

* Embedded runtime

  - Baremetal and RTOS targets
  - Reduced memory footprint
  - Most of runtime, except I/O and networking
  - Ravenscar / Jorvik tasking

* Light runtime

  - Baremetal targets
  - Very small memory footprint
  - Selected, very limited, runtime
  - Optional Ravenscar tasking (*Light-tasking* runtime)

---------------
A Simple Task
---------------

* Concurrent code execution via **task**
* :ada:`limited` types (No copies allowed)

   .. code:: Ada

      package P is
         task type Put_T;

         T : Put_T;
      end P;

      package body P is
         task body Put_T is
         begin
            loop
               delay until Clock + Milliseconds (100);
               Put_Line ("T");
            end loop;
         end Put_T;
      end P;

--------------------------------
Two Ada Synchronization Models
--------------------------------

* Passive

   - **Protected objects** model
   - Concurrency-safe **semantics**

* Active

   - Rendezvous
   - **Client / Server** model

* In Ravenscar: only **passive**

=======
Tasks
=======

------------------
Task Declaration
------------------

* Each instance of a task type is executing **concurrently**
* The **whole** tasking setup must be **static**

    - Compiler "compiles-in" the scheduling

* Task instances must be declared at the **library level**

    - Reminder: :ada:`main` declarative part is **not** at library level

* Body of a task must **never stop**
* Tasks should probably **yield**

    - For example with :ada:`delay until`
    - Or also a **protected entry guard** (see later)
    - Because of **Ravenscar scheduling** (see later)

-------------------------------------
Ravenscar Tasks Declaration Example
-------------------------------------

:filename:`my_tasks.ads`

.. code:: Ada

    package My_Tasks is
        task type Printer;

        P1 : Printer;
        P2 : Printer;
    end My_Tasks;

:filename:`my_tasks.adb`

.. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    package body My_Tasks is
        P3 : Printer; --  correct

        task body Printer is
            Period : Time_Span := Milliseconds (100);
            Next : Time := Clock + Period;
            -- P : Printer -- /!\ Would be incorrect: not at library level
        begin
            loop
                Put_Line ("loops");

                --  Yielding
                delay until Next;
                Next := Next + Period;
            end loop;
        end Printer;
    end My_Tasks;

======
Delays
======

-------------
Delay Keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time
- Absolute: Blocks until a given :ada:`Ada.Real_Time.Time`
- Relative: exists, but forbidden in Ravenscar

.. code:: Ada

    with Ada.Real_Time; use Ada.Real_Time;

    procedure Main is
        Next : Time := Clock;
    begin
        loop
            Next := Next + Milliseconds (10);
            delay until Next;
        end loop;
    end Main;

===================
Protected Objects
===================

-------------------
Protected Objects
-------------------

* **Multitask-safe** accessors to get and set state
* **No** direct state manipulation
* **No** concurrent modifications
* :ada:`limited` types (No copies allowed)

.. container:: columns

 .. container:: column

  .. code:: Ada

   protected type
     Protected_Value is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Value : Integer;
   end Protected_Value;

 .. container:: column

  .. code:: Ada

   protected body Protected_Value is
      procedure Set (V : Integer) is
      begin
         Value := V;
      end Set;

      function Get return Integer is
      begin
         return Value;
      end Get;
   end Protected_Value;

.

--------------------------
Misc: Single Declaration
--------------------------

 * Instantiate an **anonymous** task (or protected) type
 * Declares an object of that type

    - Body declaration is then using the **object** name

 .. code:: Ada

   task Printer;

.. code:: Ada

   task body Printer is
   begin
      loop
        Put_Line ("loops");
      end loop;
   end Printer;

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **No** concurrent call to either :ada:`procedure` or :ada:`function`

* In case of concurrency, other callers get **blocked**

    - Until call finishes

-------------------
Protected Entries
-------------------

* A :ada:`entry` is equivalent to a procedure but

   - It can have a **guard condition**

       + Must be a **Boolean variable**
       + Declared as :ada:`private` member of the type

   - Calling task **blocks** on the guard until it is lifted

       + At most one task blocked (in Ravenscar)
   
   - At most one entry per protected type (in Ravenscar)

.. code:: Ada

    protected Blocker is
        entry Wait when Ready;
        procedure Mark_Ready; --  sets Ready to True
    private
        Ready : Boolean := False;
    end protected;

======================
Ravenscar Scheduling
======================

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

* Tasks can synchronize and communicate via protected objects

---------------
Task Activation
---------------

* Instantiated tasks start running when **activated**
* On the **stack**

   - When the **enclosing** package has finished **elaborating**

* Can be deferred to the end of **all** elaboration

:filename:`my_tasks.ads`

.. code:: Ada

   package My_Tasks is
      task type Foo_Task_T;

      T : Foo_Task_T;
      --  T is not running yet
   end My_Tasks;

:filename:`main.adb`

.. code:: Ada

   with My_Tasks;
   --  My_Tasks has finished elab, T runs

   procedure Main is
   [...]

------------
Scheduling
------------

* Priority-based
* No time slicing (quantum)
* A task executes until ...

   - The task is blocked

       + :ada:`delay until`
       + protected object :ada:`entry`

   - A higher priority task is woken up or unblocked (preemption)

------------------------------------------
Protected Objects and Interrupt Handling
------------------------------------------

* Simple protected operations

   - No queuing (except in Jorvik)
   - :dfn:`Ceiling locking` on monoprocessor (see later)
   - :dfn:`Proxy model` for protected entries

      + Entry body executed by the active task on behalf of the waiting tasks
      + Avoids unneeded context switches
      + Timing harder to analyze

* Simple, efficient, interrupt handling

    - Protected procedures as low level interrupt handlers
    - Procedure is :dfn:`attached` to interrupt
    - Interrupt masking follows active priority

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
