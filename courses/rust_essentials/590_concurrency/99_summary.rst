=========
Summary
=========

-----------------
What We Covered
-----------------

* **Hardware Interrupts**
  * Hardware preemption without an OS scheduler
  * Separating fast ISR notifications from deferred foreground work

* **Sharing States**
  * The necessity of :rust:`static` storage for global visibility
  * Why :rust:`unsafe` cannot fix the aliasing bugs of :rust:`static mut`
  * Satisfying the exclusive-access contract behind :rust:`UnsafeCell`

* **Atomic Operations**
  * Preventing lost updates with indivisible read-modify-write operations
  * Selecting appropriate memory ordering for single-word publication
  * Critical Sections Disables interrupts to avoid single-core deadlocks

* **Cooperative Multitasking**
  * Converting asynchronous functions into zero-cost state machines
  * Voluntary CPU yielding via :rust:`.await` on a shared execution stack
  * Driving asynchronous tasks via lightweight polling loops or runtimes