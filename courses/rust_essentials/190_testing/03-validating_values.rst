===================
Validating Values
===================

-----------------------
How to Verify Success
-----------------------

* Generally, a test is validated by comparing results to expectations

  * Output matches an expected value
  * Output is in a range of values
  * Output scenario is consistent

* The :rust:`tests` module uses assertion macros for these validations

  * :rust:`asserteq!` (and :rust:`assert_ne!`)
  * :rust:`assert!`

---------------------------------
"assert_eq!" (and "assert_ne!")
---------------------------------

* Most common form of validation is equality

  * :rust:`2 + 2 == 4`
  * :rust:`sqrt(144) == 12`

* :rust:`assert_eq!` and :rust:`assert_ne!` simplify the equality check

  * Two parameters passed in
  * Successful if parameters are equal (or not equal)

* Checking for a match

  .. code:: rust

    assert_eq!(add(2, 2), 4);   // pass
    assert_eq!(sqrt(144), 12);  // pass
    assert_eq!(2i32.pow(3), 7); // panic!

* Checking for a non-match

  .. code:: rust

    assert_ne! (sqrt(144), -12)

-----------
"assert!"
-----------

* More complicated comparisons use :rust:`assert!`

  * One parameter - a boolean expression

.. code:: rust

  pub struct ClassStructure {
      name: String,
      start_time: u8,
      end_time: u8,
  }
  pub fn do_something(name: String,
                      start_time: u8,
                      end_time: u8) -> ClassStructure { ... }

  #[cfg(test)]
  mod tests {
      use super::*;

      #[test]
      fn test1() {
          let val = do_something("Fred".to_string(), 1, 2);
          assert!(val.end_time > val.start_time);
      }
  }
