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

--------------------------
Why Not Just OS Threads?
--------------------------

- **Threads have overhead**

  * Each thread requires its own dedicated stack
  * Context switching requires OS kernel intervention
  * Running many threads drains memory quickly

- **The Cooperative Alternative**

  * Tasks yield the CPU voluntarily when waiting for hardware or timers
  * All tasks share the single main stack during execution
  * No OS scheduler needed—compiled into zero-cost state machines

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
  async fn meow_async() -> &'static str {
      "Meow!"
  }

-------------------
".await" Operator
-------------------

**.await evaluates a Future to completion**

  * *If ready immediately:* 
  
    * Execution continues with zero pause or yield
    
  * *If not ready (waiting on hardware/timer):* 
  
    * Function pauses and yields control back up to caller or executor

.. code:: rust

  async fn prepare_snack() {
      // 1. Ready immediately: executes instantly without yielding
      let greeting = meow_async().await;

      // 2. Not ready: pauses execution and yields CPU back to caller!
      Timer::after_secs(5).await;

      // 3. Resumes here once timer hardware wakes it up
      println!("Cat snack served: {}", greeting);
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
    // 1. Regular function: runs immediately
    regular_meow();

    // 2. Async call WITHOUT '.await'
    // Prints NOTHING! It only creates the unexecuted Future
    let future_obj = async_meow(); 
	
	// 3. Doing something else while awaiting
    regular_meow();

    // 4. Awaiting the future:
    // NOW the future actually steps forward and runs!
    future_obj.await; 
  }

.. code:: output

  Meow 1!
  Meow 1!
  Meow 2!
  
.. note::

  If you don't .await 'future_obj', it simply gets dropped at the end

-----------------------------
The Missing Piece: Executor
-----------------------------

* Rust provides the syntax (:rust:`async` / :rust:`.await`)

  * But upstream Rust has **no** built-in scheduler

* An **Executor** is the engine that drives tasks when they yield

  * In bare-metal :rust:`no_std` systems: :rust:`embassy`
  * In :rust:`std` desktop/server systems: :rust:`tokio`
  * Implement a lightweight polling driver using pure :rust:`core::task`

-----------------------------
The Danger: Hogging the CPU
-----------------------------

**Cooperative means tasks MUST cooperate**

  * Function without an :rust:`.await` blocks the executor

.. code:: rust

  // BAD: Refuses to yield, freezes the entire runtime!
  async fn selfish_cat() {
      loop {
          // No .await here!
          // The executor never regains control; all other tasks starve
      }
  }

  // GOOD: Cooperates by yielding periodically
  async fn polite_cat() {
      loop {
          Timer::after_millis(10).await; // Yields back to executor
      }
  }

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
   * - Hardware Interrupts
     - CPU/Hardware
     - ISR Function
     - Fast response
   * - :rust:`async` / :rust:`.await`
     - None
     - State Machine Task
     - Heavy I/O & bare-metal low RAM
