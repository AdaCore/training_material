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

* Tasks can synchronize and communicate via protected objects

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
      + Avoids unneeded context switches
      + Timing harder to analyze

* Simple, efficient, interrupt handling

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

       task Some_Task is
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

   The_Lock : Lock;

   T1 : Task (Priority => 1);
   T2 : Task (Priority => 2);
   T3 : Task (Priority => 3);

   T1 locks The_Lock
   T3 starts, get scheduled (T3 > T1)
   T3 tries to get The_Lock, blocks
   T2 starts, get scheduled (T2 > T1)

   Result: T2 running, T1 blocked, T3 blocked through The_Lock (but T3 > T2!)

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
        procedure Set (Val : Integer);

 .. code:: Ada

     task T with Priority => 4 is
       ...

     task body T is
       ...
       P.Set (1);

.. image:: ravenscar_ceiling_locking.png
   :width: 45%

