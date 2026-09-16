=================
Enhancing Tests
=================

----------------------------
Assertion Failure Messages
----------------------------

.. code:: rust

  pub fn greeting(name: &str) -> String {
      format!("Hello {name}!")
  }

* Assertions with a single parameter just echo the assertion

  .. code:: rust
    :number-lines: 5

    #[test]
    fn test1() {
        let expected = "Barney";
        let result = greeting("Fred");
        assert!(result.contains(expected));
    }

  .. code:: error

    thread 'tests::test2' (54) panicked at src/lib.rs:20:9:
    assertion failed: result.contains(expected)

* But the macros can take multiple parameters

  .. code:: rust

    assert!(condition, "optional format string", arg1, arg2, ...);

-------------------------
Custom Failure Messages
-------------------------

* After the condition, parameters are treated as inputs to :rust:`format!` macro

  * Named format

    .. code:: rust
      :number-lines: 16

      #[test]
      fn test2() {
          let expected = "Barney";
          let result = greeting("Fred");
          assert!(result.contains(expected), "FAILURE: received {result}");
      }

    .. code:: error

      thread 'tests::test2' (54) panicked at src/lib.rs:20:9:
      FAILURE: received Hello Fred!

  * Positional format

    .. code:: rust
      :number-lines: 23

      #[test]
      fn test3() {
          let expected = "Barney";
          let result = greeting("Fred");
          assert!(
              result.contains(expected),
              "FAILURE: could not find {} in {}",
              expected,
              result
          );
      }

    .. code:: error

      thread 'tests::test3' (55) panicked at src/lib.rs:27:9:
      FAILURE: could not find Barney in Hello Fred!

--------------------
Dealing with Panic
--------------------

.. code:: rust
  :number-lines: 2
 
  // Return the index of 'value' in 'list'
  pub fn find_index(list: &[i32], value: i32) -> usize {
      for (index, item) in list.iter().enumerate() {
          if *item == value {
              return index;
          }
      }
      // panic if value not found
      panic!("value not found");
  }

* Successful test is easy

  .. code:: rust

    fn test1() {
        let values: [i32; 6] = [2, 3, 5, 8, 13, 21];
        let result = find_index(&values, 5);
        assert_eq!(result, 2);
    }

  .. code:: output

    test tests::test1 ... ok

* What happens when the value is not found?

-------------------
Expecting a Panic
-------------------

* Problem

  .. code:: rust

    #[test]
    fn test2() {
        let values: [i32; 6] = [2, 3, 5, 8, 13, 21];
        let result = find_index(&values, 6);
        assert_eq!(result, 2);
    }

  .. code:: error

    test tests::test2 ... FAILED

    thread 'tests::test2' (90) panicked at src/lib.rs:9:5:
    value not found

  * But the code behaved correctly!

* Need to set the expectation of panic 

  .. code:: rust

    #[test]
    #[should_panic]
    fn test3() {
        let values: [i32; 6] = [2, 3, 5, 8, 13, 21];
        let result = find_index(&values, 6);
        assert_eq!(result, 2);
    }

  .. code:: output

    test tests::test3 - should panic ... ok

-------------------------
Using "Result" in Tests
-------------------------

* Tests can return :rust:`Result` rather than just panicking

  * Allows use of :rust:`?` when tested function returns :rust:`Result`

.. code:: rust

  pub fn divide(top: f64, bottom: f64) -> Result<f64, String> {
      if bottom == 0.0 {
          Err("Cannot divide by zero!".to_string())
      } else {
          Ok(top / bottom)
      }
  }

.. code:: rust

    #[test]
    fn test1() -> Result<(), String> {
        let result = divide(14.0, 3.0)?;
        assert!(result > 4.6 && result < 4.7);
        Ok(())
    }

    #[test]
    fn test2() -> Result<(), String> {
        let result = divide(14.0, 0.0)?;
        assert!(result > 0.0);
        Ok(())
    }

.. code:: output

  test tests::test1 ... ok
  test tests::test2 ... FAILED

  failures:

  ---- tests::test2 stdout ----
  Error: "Cannot divide by zero!"
