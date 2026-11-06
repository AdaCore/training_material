==============
Introduction
==============

----------------
Topics Covered
----------------

* **Memory Budgets**

  - Distinguish storage location, capacity, and total memory use

* **Dynamic Allocation**

  - Identify allocation costs
  - Handle errors from fallible allocations

* **Caller-Provided Storage**

  - Reuse existing memory

* **Static Storage**

  - Control access to persistent data

* **Fixed-Capacity Collections**

  - Store bounded data without growing a heap allocation

* **Runtime Constraints**

  - Choose limits, failure policies, and checks that match requirements

---------------------------
Why Memory Budgets Matter
---------------------------

**Embedded systems must meet memory and timing requirements**

* Available memory is often limited

* Allocation and buffer growth can affect response time

* Designs must establish:

  * The maximum memory needed at once

  * When costly operations may occur

  * What happens when a memory limit is reached
