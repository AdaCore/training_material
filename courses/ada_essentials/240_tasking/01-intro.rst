==============
Introduction
==============

--------------------------------------------------
Concurrency - One Program, Many Things Happening
--------------------------------------------------

* **Sequential programs** - one instruction at a time

* **Concurrent programs** - multiple activities *conceptually* happening at once

  * Even on one CPU
  * Think many cooks in one kitchen

* Why concurrency?

  * Respond to external events (real-time systems)
  * Improve performance or responsiveness

--------------------
Concurrency in Ada
--------------------

* Built-in language constructs

  * Not a library - part of the semantics

* :ada:`task` - concurrent process

  * Compiler/runtime coordinate all tasks (not programmer)

* :ada:`protected` - safe shared data access

-----------------------
Process Communication
-----------------------

* Tasks can rendezvous with other tasks

  * Via :ada:`entry` call
  * Data can be passed like in subprograms

* Protected objects control data access

  * Multiple simultaneous readers
  * One writer at a time

    * All other accesses blocked during write

* Tasks can

  * Wait for another task
  * Block other tasks
