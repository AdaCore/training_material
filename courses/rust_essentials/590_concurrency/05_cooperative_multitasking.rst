==========================
Cooperative Multitasking
==========================

--------------------
The "std" Solution
--------------------

:rust:`std::thread` **relies on operating system**

  * OS handles scheduling, preempting, and context-switching

.. code:: rust

  use std::thread;

  fn worker() {
    println!("Hello from worker thread!");
  }

  fn main() {
    // Pass the function name directly
    let handle = thread::spawn(worker);

    handle.join().unwrap();
  }

.. note::

  Requires :rust:`std` crate and an underlying operating system

--------------------------
Why Not Just OS Threads?
--------------------------

- **Threads have overhead**

  * Each thread requires its own dedicated stack
  * Context switching requires OS kernel intervention
  * Running many threads drains memory quickly

- **The Cooperative Alternative**

  * Tasks yield the CPU voluntarily when waiting for hardware or timers
  * Tasks do not require a dedicated stack each
  * Tasks can be scheduled without an operating system

.. note::

  Cooperative multitasking works on any bare-metal target

-----------------
"async" Keyword
-----------------

**Marking a function as** :rust:`async` **changes what it returns**

  * Does not run immediately
  * Returns a :rust:`Future` (a state machine waiting to be polled)

.. code:: rust

  // Normal function: runs immediately when called
  fn meow() -> &'static str {
      "Meow!"
  }

  // Async function: does nothing until polled/awaited
  // This function will always be ready to be executed
  async fn meow_async() -> &'static str {
      "Meow!"
  }

-------------------
".await" Operator
-------------------

:rust:`.await` **evaluates a** :rust:`Future` **to completion**

  * *If ready immediately*

    * Execution continues with zero pause or yield

  * *If not ready (waiting on hardware/timer)*

    * Function pauses and yields control back up to caller or executor

.. code:: rust

  async fn prepare_snack() {
      // Ready immediately: executes instantly without yielding
      let greeting = meow_async().await;

      // Pending: yields control back to the executor
      let snack = fetch_treat_async().await;

      // Resumes here once the future completes executor polls this task
      serve(greeting, snack);
  }

---------------------
Awaiting the Future
---------------------

.. code:: rust

  fn regular_meow() {
    println!("Meow 1!");
  }

  async fn async_meow() {
    println!("Meow 2!");
  }

  async fn cat() {
    // Regular function: runs immediately
    regular_meow();

    // Async call WITHOUT '.await': only creates unexecuted 'Future'
    let future_obj = async_meow();

    regular_meow();

    // Awaiting the future: actually steps forward and runs!
    future_obj.await;
  }

* Output when the future returned by :rust:`cat()` is polled to completion

.. code:: output

  Meow 1!
  Meow 1!
  Meow 2!

.. note::

  If you don't :rust:`.await future_obj`, it simply gets dropped at the end

-----------------------------
The Danger: Hogging the CPU
-----------------------------

**Cooperative scheduling requires tasks to yield**

  * Short functions without :rust:`.await` finish quickly and return control
  * Tasks running continuously without yielding starves the executor
  * Other tasks cannot resume until active future yields or completes

.. code:: rust

  // BAD: Refuses to yield, freezes the entire runtime!
  async fn selfish_cat() {
      loop {
          // No '.await' here!
          // The executor never regains control; all other tasks starve
      }
  }

* Awaiting a pending future yields control to the executor

.. warning::

  .await will not yield if the future is ready

------------------------------
Concurrency Models Compared
------------------------------

.. list-table::
   :header-rows: 1
   :widths: 22 24 24 30
   :class: tight-table


   * - **Model**
     - **Preemption**
     - **Unit of Work**
     - **Best Used For**
   * - :rust:`std::thread`
     - OS Scheduler
     - OS Thread
     - Heavy CPU computation
   * - Interrupts
     - CPU/Hardware
     - ISR Function
     - Fast response
   * - :rust:`async`/:rust:`.await`
     - Programmed
     - Task
     - Heavy I/O & low RAM

-----------------------------
The Missing Piece: Executor
-----------------------------

* Rust provides the syntax (:rust:`async`/:rust:`.await`)

* An **Executor** is the engine that drives tasks when they yield

  * Bare-metal systems (:rust:`no_std`): :rust:`embassy`
  * Desktop/Server systems (:rust:`std`): :rust:`tokio`
  * Lightweight polling driver can be implemented manually

    * using :rust:`core::task`
