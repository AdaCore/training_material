=========
Summary
=========

-----------------
What We Covered
-----------------

* **Hardware Interrupts**

  * Hardware preemption without scheduler
  * Separating fast ISR from foreground work

* **Sharing States**

  * Necessity of :rust:`static` storage for global visibility
  * :rust:`unsafe` cannot fix the aliasing bugs of :rust:`static mut`
  * Satisfying exclusive-access contract behind :rust:`UnsafeCell`

* **Atomic Operations**

  * Preventing lost updates with indivisible read-modify-write
  * Critical Sections disables interrupts to avoid deadlocks

* **Cooperative Multitasking**

  * Asynchronous functions act as zero-cost state machines
  * Yielding via :rust:`.await` on shared execution stack
  * Asynchronous tasks via lightweight polling loops or runtimes