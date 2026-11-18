====================================
Memory and Runtime Constraints Lab
====================================

----------
Overview
----------

This lab will demonstrate your understanding of different types of memory budgets

  * Fixed capacity storage
  * Dynamically allocated storage
  * Caller-supplied storage

The example is a simplistic averaging routine - total the number of some
supplied values and determine the average of all the values.

Your first step is to implement a :rust:`static_data` module containing constants
for the maximum number of values (:rust:`MAX_SAMPLES`),
the list of values (:rust:`VALUES`), and the expected average (:rust:`AVERAGE`).

Then, each mechanism is defined by a :rust:`struct` in the appropriate module.
For each mechanism, you will need to implement the content of the structure,
a :rust:`new` method, and the :rust:`Logger` trait.

The main program will test that each module behaves correctly.

.. tip::

  There will be hints along the way so you don't get stuck forever!

-----
Lab
-----

* *Setup*

  Copy the :filename:`prompt` folder into your local working area. This is a
  complete Rust crate, but it will not build yet.

  Note that the :filename:`answer` folder contains a working solution. Feel
  free to use that as much or as little as you need.

* *Goals*

  * Fully implement the structures and the test function to prove everything

    * :rust:`Fixed` defines a fixed length holder for the maximum number of values
    * :rust:`Dynamic` defines a holder that can grow as large as it needs
    * :rust:`Caller` accepts a holder for storing values

  * Complete implementation of the :rust:`common_test` function to prove your code works

.. note::

  * :rust:`Caller` is trickier, so the structure and :rust:`new` function are already defined

**STOP HERE IF YOU DO NOT WANT HINTS!!**

  **The next pages show how to start with the fixed capacity module.**
  **If you need help/hints, proceed at your own pace.**

--------------------
Static Data Module
--------------------

:filename:`static_data.rs`

.. code:: rust
  :font-size: footnotesize

  pub const MAX_SAMPLES: usize = 9;
  pub const VALUES: [usize; MAX_SAMPLES] = [1, 2, 3, 5, 6, 7, 9, 10, 11];
  pub const AVERAGE: usize = 6;

-----------------------------------
Fixed Capacity Module - Structure
-----------------------------------

:filename:`fixed.rs`

.. code:: rust
  :font-size: footnotesize
  :number-lines: 1

  use crate::logger_trait::Logger;
  use crate::static_data::MAX_SAMPLES;

  pub struct Fixed {
      samples: [usize; MAX_SAMPLES],
      count: usize,
  }

  impl Fixed {
      pub fn new() -> Self {
          Self {
              samples: [0; MAX_SAMPLES],
              count: 0,
          }
      }
  }

--------------------------------
Fixed Capacity Module - Logger
--------------------------------

:filename:`fixed.rs`

.. code:: rust
  :font-size: tiny
  :number-lines: 18

  impl Logger for Fixed {
    // Return false if the logger is full.
    fn add_sample(&mut self, value: usize) -> bool {
        if self.count >= MAX_SAMPLES {
            return false;
        }

        self.samples[self.count] = value;
        self.count += 1;
        true
    }

    // Return None if no samples have been collected.
    fn average(&self) -> Option<usize> {
        if self.count == 0 {
            None
        } else {
            let mut total = 0;
            for index in 0..self.count {
                total = total + self.samples[index];
            }
            Some(total / self.count as usize)
        }
    }

    // Clear the logger so its storage can be reused.
    fn clear(&mut self) {
        self.count = 0;
    }

    // Number of items in use
    fn count(&self) -> usize {
        self.count
    }

    // Total capacity
    fn capacity(&self) -> usize {
        self.samples.len()
    }
  }

------------------------
Testing Fixed Capacity 
------------------------

:filename:`main.rs`

.. code:: rust
  :font-size: tiny
  :number-lines: 12

  fn common_test<T: logger_trait::Logger>(logger: &mut T) {
      for value in VALUES {
          if !logger.add_sample(value) {
              println!("Should not get here")
          }
      }
      if !logger.add_sample(1234) {
          println!("Exceeded capacity")
      }

      let count = logger.count();
      assert_eq!(count, MAX_SAMPLES);

      let average = logger.average().unwrap();
      assert_eq!(average, AVERAGE, "Actual average: {average}");

      println!("Average {average} over {count} values");

      logger.clear();
      assert_eq!(logger.count(), 0);
      let capacity = logger.capacity();
      assert_eq!(capacity, MAX_SAMPLES, "Actual capacity: {capacity}");
  }

  fn main() {
      println!("Testing a fixed buffer");
      let mut fixed = Fixed::new();
      common_test(&mut fixed);

----------
Solution
----------

* Remaining answers can be found in :filename:`answers` folder

* Output from running full test should look something like

.. code:: output

  Testing a fixed buffer
  Exceeded capacity
  Average 6 over 9 values

  Testing a caller-supplied buffer
  Exceeded capacity
  Average 6 over 9 values

  Testing a dynamic buffer
  Exceeded capacity
  Average 6 over 9 values

  thread 'main' panicked at src\main.rs:33:5:
  assertion `left == right` failed: Actual capacity: 16
    left: 16
   right: 9

.. note::

  Capacity for the dynamic storage may be different than the other mechanisms.
  This is not a bug, but it is why we run it last!
