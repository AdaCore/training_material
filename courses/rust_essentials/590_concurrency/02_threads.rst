=========
Threads
=========

--------------------
The "std" Solution
--------------------

:rust:`std::thread` **relies on operating system**

  * OS handles scheduling, preempting, and context-switching across CPU

.. code:: rust

  use std::thread;

  // Spawns a background OS thread
  thread::spawn(|| {
    println!("Running concurrently on another thread!");
  });

------------------
Joining A Thread
------------------

**Pass a closure to** :rust:`std::thread::spawn` **to spawn a thread**

.. code:: rust

  use std::thread;
  use std::time::Duration;

  fn main() {
    // 1. Spawning a thread
    let handle = thread::spawn(|| {
      for i in 1..5 {
        println!("Thread printing: {}", i);
        thread::sleep(Duration::from_millis(1));
      }
      42 // Return value from thread
    });

    // 2. Joining the thread (waiting for completion)
    // '.join()' pauses 'main' until the thread finishes and returns a 'Result'
    match handle.join() {
        Ok(result) => println!("Thread finished with result: {}", result),
        Err(e) => println!("Thread panicked: {:?}", e),
    }
  }

--------------------------
Moving Data Into Threads
--------------------------

**Threads can outlive the calling context**

  * Use the move keyword to transfer ownership of local variables

.. code:: rust

  use std::thread;

  let data = vec![1, 2, 3];

  // 'move' forces ownership of 'data' to transfer into the thread closure
  let handle = thread::spawn(move || {
    println!("Data inside thread: {:?}", data);
  });

  // 'data' is no longer accessible here in main (caught at compile time!)
  