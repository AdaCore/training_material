=========
Summary
=========

-----------------
What We Covered
-----------------

- **OS Threads**

  - Managed and preempted directly by the operating system

  - Ownership transfer using :rust:`move` closures and waiting via :rust:`.join()`

- **The** :rust:`Send` **and** :rust:`Sync` **Traits**

  - Zero-cost auto-traits governing boundary crossing

  - :rust:`Send` permits moving values; :rust:`Sync` permits sharing references

- **Hardware Interrupts (ISRs)**

  - Hardware-level preemption for sub-microsecond response

  - Prevent data race via :rust:`core::sync::atomic` and memory ordering

- **Cooperative Multitasking**

  - Zero-cost :rust:`Future` state machines yielding voluntarily via :rust:`.await`

  - Schedulers and executors like Embassy in :rust:`no_std` environments

- **State Sharing & Interior Mutability**

  - Single-threaded mutation using :rust:`Cell` and :rust:`RefCell`

  - :rust:`Arc<Mutex<T>>` vs. :rust:`critical_section` locks
