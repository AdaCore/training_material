==========================
Cooperative Multitasking
==========================

------------------------
Why Not Just OS Threads?
------------------------

- **Threads have overhead**

  * Each thread requires its own dedicated stack
  * Context switching requires OS kernel intervention
  * Running many threads drains memory quickly

- **The Cooperative Alternative**

  * Tasks yield the CPU voluntarily when waiting for hardware or timers
  * All tasks share the single main stack during execution
  * No OS scheduler needed—compiled into zero-cost state machines

---------------------
The "async" Keyword
---------------------

**Marking a function as** :rust:`async` **changes what it returns**

  * It does not run immediately
  * It returns a :rust:`Future` (a state machine waiting to be polled)

.. code:: rust

  // Normal function: runs immediately when called
  fn meow() -> &'static str {
      "Meow!"
  }

  // Async function: does nothing until polled/awaited
  async fn meow_async() -> &'static str {
      "Meow!"
  }

-----------------------
The ".await" Operator
-----------------------

**.await evaluates a Future to completion**

  * *If ready immediately:* 
  
    * Execution continues with zero pause or yield
	
  * *If not ready (waiting on hardware/timer):* 
  
    * The function pauses and yields control back up to caller or executor

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
Awaiting the future
---------------------
  
  fn regular_meow() {
    println!("Meow 1!");
  }

  async fn async_meow() {
    println!("Meow 2!");
  }

  async fn cat() {
    // 1. Regular function: runs immediately
    // Output: "Meow 1!"
    regular_meow();

    // 2. Async call WITHOUT .await:
    // Prints NOTHING! It only creates the unexecuted Future
    let future_obj = async_meow(); 

    // 3. Awaiting the future:
    // NOW the future actually steps forward and runs!
    // Output: "Meow 2!"
    future_obj.await; 
  }
  
.. note::

  If you don't .await 'future_obj', it simply gets dropped at the end

-----------------------------------
Waiting on Multiple Tasks: "join"
-----------------------------------

**Running futures concurrently without spawning new background tasks**

* :rust:`join` polls multiple futures cooperatively on the same execution context
* If Future A pauses at :rust:`.await`, the caller switches to polling Future B

.. code:: rust

  use embassy_futures::join::join;

  async fn cat_morning_routine() {
      // Runs both cooperatively: when one yields, the other progresses!
      let (snack, nap) = join(prepare_snack(), take_nap()).await;

      println!("Both finished!");
  }

-----------------------------
The Missing Piece: Executor
-----------------------------

- Rust provides the syntax (:rust:`async` / :rust:`.await`)

  * But upstream language has **no** built-in scheduler

- An **Executor** is the engine that drives tasks when they yield

  * In bare-metal :rust:`no_std` systems: :rust:`embassy`
  * In :rust:`std` desktop/server systems: :rust:`tokio`


--------------------------
Embassy Example (no_std)
--------------------------

**Spawning independent cooperative tasks on an embedded executor**

.. code:: rust

  #![no_std]
  #![no_main]

  use embassy_executor::Spawner;
  use embassy_time::Timer;

  #[embassy_executor::task]
  async fn blink_led() {
      loop {
          // Pauses and yields CPU back to the Embassy executor
          Timer::after_millis(500).await;
      }
  }

  #[embassy_executor::main]
  async fn main(spawner: Spawner) {
      // Hand task over to the executor
      spawner.spawn(blink_led()).unwrap();

      loop {
          // Main task also cooperates by yielding
          Timer::after_secs(1).await;
      }
  }

---------------------------
The Danger: Hogging the CPU
---------------------------

**Cooperative means tasks MUST cooperate**

* Because there is no hardware or OS preemption, a task without an :rust:`.await` blocks the executor

.. code:: rust

  // BAD: Refuses to yield, freezes the entire Embassy runtime!
  #[embassy_executor::task]
  async fn selfish_cat() {
      loop {
          // No .await here!
          // The executor never regains control; all other tasks starve
      }
  }

  // GOOD: Cooperates by yielding periodically
  #[embassy_executor::task]
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
   :stub-columns: 1

   * - **Model**
     - **Preemption By**
     - **Unit of Work**
     - **Best Used For**
   * - :rust:`std::thread`
     - OS Scheduler
     - OS Thread
     - Heavy CPU computation
   * - Hardware Interrupts
     - Physical CPU Hardware
     - ISR Function
     - Sub-microsecond hardware reaction
   * - :rust:`async` / :rust:`.await`
     - None (Tasks yield)
     - State Machine Task
     - High-volume I/O & bare-metal low RAM
