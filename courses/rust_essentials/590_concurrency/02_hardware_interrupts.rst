=====================
Hardware Interrupts
=====================

---------------------------
Hardware-Level Preemption
---------------------------

* Interrupts are triggered directly by hardware peripherals

  * CPU forcibly pauses the current execution
  * Transfers execution to an Interrupt Service Routine (ISR)

    * Not within an OS thread or asynchronous task
	
  * Can be paired with external architecture crates
  
    * To handle vector tables and handler registration

* Higher-priority interrupts preempt lower-priority ones

  * If architecture supports nested interrupts

--------------------
Execution Timeline
--------------------

**Single core alternates between two separate execution contexts**

  .. image:: rust_essentials/concurrency_interrupt.svg

  * Foreground loop does not know it was paused
  * Hardware handles switching without runtime or OS intervention

----------------------------------
Interrupt Priorities and Nesting
----------------------------------

* Microcontrollers support multiple interrupt sources 
  
  * With configurable priorities

* Higher-priority interrupts preempt lower-priority ones

  .. image:: rust_essentials/concurrency_interrupt_with_priorities.svg

* Foreground work only resumes once every pending handler exits

--------------------
Different Syntaxes
--------------------

**Hardware interrupts syntax varies depending on the solution**

  * **External libraries**
  
    * Example: :rust:`cortex-m-rt` uses :rust:`#[exception]`
	
  .. code:: rust

    #[exception]
    pub fn SysTick_Handler() {
    }
	
  * **Regular** :rust:`#![no_std]`
  
    * Functions need to be declared :rust:`extern "C"`
	* Add :rust:`#[no_mangle]` to keep the name intact for Linker

  .. code:: rust

    #[no_mangle]
    pub extern "C" fn SysTick_Handler() {
    }
	
.. note:: 

  :rust:`extern` ABI depends on the architecture, uses :rust:`"C"` on architecture using the C calling convention

------------------------------
ISRs are Top-Level Functions
------------------------------

**Can be mapped in the hardware interrupt vector table**


.. code:: rust

  #[no_mangle]
  // Hardware Interrupt Handler
  pub extern "C" fn on_cat_jumped() {
    deploy_emergency_catnip();
  }

  // Foreground Main Loop
  fn main() -> ! {
    loop {
      write_safe_rust_code();
    }
  }

----------------------------
Global Shared State Hazard
----------------------------

* Interrupt can trigger at any CPU instruction
* Unsynchronized access to global :rust:`static mut` causes data races and lost updates


.. code:: rust

  // DANGER: Global mutable state
  static mut COUNTER: u32 = 0; 

  fn main() -> ! {
    unsafe {
        COUNTER += 1; // Requires multi-step CPU Read-Modify-Write
    }
    loop {}
  }
  
  #[no_mangle]
  pub extern "C" fn TIMER0() {
    unsafe {
        // If 'main' is interrupted mid-write above, 'COUNTER' becomes corrupted!
        COUNTER = 0;
    }
  }

* ISR can interrupt a read/write
  
-------------------------------
Compound Operation Preemption
-------------------------------

  .. image:: rust_essentials/concurrency_lost_update.svg

* Code executes valid assembly instructions

  * But the application outcome is wrong!
  
* At the Rust language level, this data race is :dfn:`Undefined Behavior`

 * No specific outcome is guaranteed

-------------------------------
Two Distinct Classes of Races
-------------------------------

* **Data Races**

  * Multiple contexts access same memory location
  
    * Without synchronization
	
  * At least one access is a write and at least one is non-atomic
  * Causes undefined behavior

* **Logical Races**

  * No data races: individual operations are valid and well-defined
  * Program flow interleaves unexpectedly
  
    * Produces incorrect application results

  * Eliminating data races does not eliminate logical sequencing errors

-------------------- 
Logical Race Issue
--------------------

.. code:: rust
  :font-size: footnotesize

  #[no_mangle]
  pub extern "C" fn on_uart_rx_interrupt() {
    // ISR records that a packet is ready to process
    signal_packet_ready();
  }

  #[no_mangle]
  pub extern "C" fn recover_uart_errors() {
    unsafe {
      let status = UART_STATUS.read_volatile();
      if (status & RX_OVERRUN) != 0 {
        // PREEMPTION HAZARD
        // Hardware puts new packet into the FIFO and triggers interrupt
        // Clear the FIFO because we thought it was only full of bad bytes
        UART_CR.write_volatile(CLEAR_FIFO);
      }
    }
  }

* Individual memory accesses are valid, result is still wrong!
* Interrupt changed context between the two steps 

  * Made foreground's decision outdated and destructive

--------------------------------
Separation of Responsibilities
--------------------------------

* Interrupt handlers must execute with minimal latency

  * Avoid long loops, delays, or complex payload computations
  * Records event, clears flag, returns immediately

* Defer longer processing to foreground code

  * Foreground inspects notifications and performs heavy data work

* Combined pattern protects system responsiveness

  * Prevents high-priority hardware starvation and latency spikes
  * Keeps real-time interrupt response times short and bounded

.. code:: rust

  // BAD: Long processing inside an ISR starves other interrupts
  #[no_mangle]
  pub extern "C" fn BAD_TIMER_ISR() {
      for _ in 0..10_000 {
          expensive_checksum_computation();
      }
  }

  // GOOD: Signal quickly and defer work to foreground
  #[no_mangle]
  pub extern "C" fn GOOD_TIMER_ISR() {
      signal_work_pending(); // Exits immediately
  }
