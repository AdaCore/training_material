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

* By default, :rust:`assert!` displays failed condition

  .. code:: rust
    :number-lines: 5

    #[test]
    fn test1() {
        let expected = "Barney";
        let result = greeting("Fred");
        assert!(result.contains(expected));
    }

  .. code:: error

    thread 'tests::test1' (54) panicked at src/lib.rs:20:9:
    assertion failed: result.contains(expected)

* Assertion macros can take multiple arguments

  .. code:: rust

    assert!(condition, "optional format string", arg1, arg2, ...);

-------------------------
Custom Failure Messages
-------------------------

* After the condition, arguments are treated as inputs to :rust:`format!` macro

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
Dealing With Panic
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
      // panic if 'value' not found
      panic!("value not found");
  }

* What happens when the value is not found?

  .. code:: rust

    #[test]
    fn test_not_found() {
        let values: [i32; 6] = [2, 3, 5, 8, 13, 21];
        let result = find_index(&values, 6);
        assert_eq!(result, 2);
    }

-----------------
Expecting Panic
-----------------

* Running :rust:`test_not_found` causes test failure

  .. code:: error
    :font-size: footnotesize

    test tests::test2 ... FAILED

    thread 'tests::test_not_found' (90) panicked at src/lib.rs:9:5:
    value not found

  * But the code behaved correctly!

* Need to set the expectation of panic 

  .. code:: rust

    #[test]
    #[should_panic]
    fn test_panic() {
        let values: [i32; 6] = [2, 3, 5, 8, 13, 21];
        let result = find_index(&values, 6);
        assert_eq!(result, 2);
    }

  .. code:: output

    test tests::test_panic - should panic ... ok

---------------------------
Expecting the Right Panic
---------------------------

* What happens to our test if :rust:`find_index` doesn't work correctly?

  * For example, it returns a 0 on failure rather than panic

* Test will pass (because it expects a panic)

  * Even though the code is wrong, assertion will panic

* Need to specify expected text in panic message

  .. code:: rust
    :font-size: footnotesize

    #[test]
    #[should_panic(expected = "value not found")]
    fn test3() {
        let values: [i32; 6] = [2, 3, 5, 8, 13, 21];
        let result = find_index(&values, 6);
        assert_eq!(result, 2);
    }

  .. code:: output
    :font-size: tiny

    test tests::test3 - should panic ... FAILED

    failures:

    ---- tests::test3 stdout ----

    thread 'tests::test3' panicked at src\lib.rs:23:9:
    assertion `left == right` failed
      left: 0
     right: 2
    note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
    note: panic did not contain expected string
          panic message: `"assertion `left == right` failed\n  left: 0\n right: 2"`,
     expected substring: `"value not found"`

-------------------------------
Functions Returning "Result"
-------------------------------

* Code that returns :rust:`Result` can also be tested

  .. code:: rust
    :font-size: small

    pub fn divide(top: f64, bottom: f64) -> Result<f64, String> {
        if bottom == 0.0 {
            Err("Cannot divide by zero!".to_string())
        } else {
            Ok(top / bottom)
        }
    }

* Tests should detect :rust:`Result` rather than panicking

--------------------------
Testing "Result" Returns
--------------------------

* Use :rust:`?` when tested function returns :rust:`Result`

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

* Tests return :rust:`Ok` on success

  * But propagate :rust:`Err` on failure

.. code:: output
  :font-size: footnotesize

  test tests::test1 ... ok
  test tests::test2 ... FAILED

  failures:

  ---- tests::test2 stdout ----
  Error: "Cannot divide by zero!"
