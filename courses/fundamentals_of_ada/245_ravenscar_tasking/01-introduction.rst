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

