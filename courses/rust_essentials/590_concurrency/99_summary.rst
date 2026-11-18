=========
Summary
=========

-----------------
What We Covered
-----------------

* **Hardware Interrupts**

  * Hardware preemption without scheduler
  * Separating lightweight ISR from heavier foreground work

* **Sharing States**

  * Necessity of :rust:`static` storage for global visibility
  * :rust:`unsafe` cannot fix the aliasing bugs of :rust:`static mut`
  * Satisfying exclusive-access contract behind :rust:`UnsafeCell`

* **Atomic**

  * Preventing lost updates with indivisible read-modify-write
  * Critical Sections disables interrupts to protect shared data

* **Cooperative Multitasking**

  * Asynchronous functions act as zero-cost state machines
  * :rust:`.await` suspends the task only when the future is pending
  * Asynchronous tasks via lightweight polling loops or runtimes