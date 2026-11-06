=========
Summary
=========

-----------------
What We Covered
-----------------

* **Memory Budgets**

  - Count storage, capacity, and simultaneously live data

* **Dynamic Allocation**

  - Make allocation points visible and handle reported failures explicitly

* **Caller-Provided Storage**

  - Reuse caller-owned buffers without allocating separate outputs

* **Static Storage**

  - Separate storage duration from ownership and access policy

* **Fixed-Capacity Collections**

  - Avoid growing allocations while handling capacity exhaustion

* **Runtime Constraints**

  - Bound the workload and check the complete execution path

.. tip::

  Choose storage and failure policies that match resource limits
