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

