=========
Summary
=========

----------------------------------------------
Hardware-Driven Real-Time Concurrency (RTIC)
----------------------------------------------

:rust:`rtic` **- Beyond cooperative async and manual interrupt masking**

  * Turns hardware into the scheduler
  * Tasks map directly to interrupts with preemptive priority 
  * Delivers true real-time preemption while sharing a single stack


-----------------
What We Covered
-----------------

* **Hardware Interrupts**
  * Direct hardware preemption without an OS scheduler
  * Stacking context frames across priority levels
  * Separating fast ISR notifications from deferred foreground work

* **Sharing States and Rust Rules**
  * The necessity of :rust:`static` storage for global visibility
  * Why :rust:`unsafe` cannot fix the aliasing bugs of :rust:`static mut`
  * Satisfying the exclusive-access contract behind :rust:`UnsafeCell`

* **Atomic Operations and Critical Sections**
  * Preventing lost updates with indivisible read-modify-write operations
  * Selecting appropriate memory ordering for single-word publication
  * Disabling interrupts for multi-field structs and avoiding single-core deadlocks

* **Cooperative Multitasking**
  * Converting asynchronous functions into zero-cost state machines
  * Voluntary CPU yielding via :rust:`.await` on a shared execution stack
  * Driving asynchronous tasks via lightweight polling loops or runtimes