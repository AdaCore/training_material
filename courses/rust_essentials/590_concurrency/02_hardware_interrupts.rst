=====================
Hardware Interrupts
=====================

---------------------------
Hardware-Level Preemption
---------------------------

* Hardware interrupts are fully supported in :rust:`#![no_std]` environments

  * Can be paired with external architecture crates

    * :rust:`cortex-m-rt`, :rust:`riscv-rt`, or :rust:`avr-device`

* Interrupts are triggered directly by hardware peripherals

  * CPU forcibly pauses the current execution
  * Transfers execution to an Interrupt Service Routine (ISR)

    * Not within an OS thread or asynchronous task

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

-------
Usage
-------

**ISRs are written as top-level functions**

  * Allows hardware interrupt vector table to call them
  

.. code:: rust

  #[no_mangle]
  // Hardware Interrupt Handler
  pub fn on_cat_jumped() {
    deploy_emergency_catnip();
  }

  // Foreground Main Loop
  fn main() {
    loop {
      write_safe_rust_code();
    }
  }
  
.. note::

  :rust:`#[no_mangle]` keeps the name intact for the linker  
  
----------------------------
Global Shared State Hazard
----------------------------

* Interrupt can trigger at any CPU instruction
* Sharing state between :rust:`main` and an ISR creates corruption


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
  fn TIMER0() {
    unsafe {
        // If 'main' is interrupted mid-write above, 'COUNTER' becomes corrupted!
        COUNTER = 0;
    }
  }

* Standard global variables require :rust:`static mut`

  * ISR can interrupt a read/write
  
-------------------------------
Compound Operation Preemption
-------------------------------

  .. image:: rust_essentials/concurrency_lost_update.svg

* Code executes valid assembly instructions

  * But the application outcome is wrong!

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

  // --- HARDWARE INTERRUPT (High-Priority Packet RX) ---
  #[no_mangle]
  pub fn on_uart_rx_interrupt() {
    // Peripheral automatically places packets in the FIFO.
    // ISR records that a packet is ready to process.
    signal_packet_ready();
  }

  // --- FOREGROUND ERROR HANDLER ---
  pub fn recover_uart_errors() {
    unsafe {
      let status = UART_STATUS.read_volatile();

      if (status & RX_OVERRUN) != 0 {
        // -------------------------------------------------------------
        // PREEMPTION HAZARD:
        // Hardware puts new packet into the FIFO and triggers interrupt
        // -------------------------------------------------------------

        // Clear the FIFO because we thought it was only full of bad bytes:
        UART_CR.write_volatile(CLEAR_FIFO);
      }
    }
  }

* Memory access is safe, result is still broken!
* Interrupt changed context between the two steps 

  * Made foreground's decision outdated and destructive

--------------------------------
Separation of Responsibilities
--------------------------------

* Interrupt handlers must execute with minimal latency

  * Avoid long loops, delays, or complex payload computations
  * Example: Handler records event, clears hardware flag, and returns immediately

* Foreground maintains computation ownership

  * Foreground inspects notifications and performs heavy data work

* Combined pattern protects system responsiveness

  * Prevents high-priority hardware starvation and latency spikes
  * Keeps real-time interrupt response times short and bounded

.. code:: rust

  // BAD: Long processing inside an ISR starves other interrupts
  fn BAD_TIMER_ISR() {
      for _ in 0..10_000 {
          expensive_checksum_computation();
      }
  }

  // GOOD: Signal quickly and defer work to foreground
  fn GOOD_TIMER_ISR() {
      signal_work_pending(); // Exits immediately
  }
