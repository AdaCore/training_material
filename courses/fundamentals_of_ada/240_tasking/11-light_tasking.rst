===============
Light-Tasking
===============

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

.. code:: Ada

   pragma Profile (Ravenscar);

-----------------------------
What Is the Jorvik profile?
-----------------------------

* A **non-backwards compatible profile** based on Ravenscar

  + Defined in the RM D.13 (Ada 2022)

* Removes some constraints

  - Scheduling analysis may be harder to perform

.. code:: Ada

   pragma Profile (Jorvik);

* Subset of Ravenscars' requirements

  * This class is about the more widespread Ravenscar
  * But some of Jorvik's differences are indicated

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
  - Ravenscar/Jorvik tasking

* Light runtime

  - Baremetal targets
  - Very small memory footprint
  - Selected, very limited, runtime
  - Optional Ravenscar tasking (*Light-tasking* runtime)

