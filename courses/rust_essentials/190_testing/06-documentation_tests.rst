=====================
Documentation Tests
=====================

----------------------
What Is a "doctest"?
----------------------

* Executable code snippets embedded directly inside documentation comments

  * Ensure code examples in documentation match actual code

* Example

  .. code:: rust

    /// Adds two numbers together.
    /// ```
    /// use adder::add;
    /// assert_eq!(add(2, 3), 5);
    /// ```
    pub fn add(left: i32, right: i32) -> i32 {
        left + right
    }

* Rust code blocks in :rust:`///` or :rust:`//!` comments are doctests by default

------------------------
Benefits of "doctest"?
------------------------

* Doctests serve two purposes

  * Public API documentation (via ``cargo doc``)
  * Automated integration tests

* Doctests act on external API of module

  * Tests focus on interaction with clients

    * Not low-level support functions

--------------------------
"doctest" Implementation
--------------------------

.. code:: rust
  :number-lines: 1

  /// Test add
  ///
  /// ```
  /// # use adder::add;
  /// assert_eq!(add(3, 4), 7);
  /// assert_ne!(add(2, 6), 7);
  /// ```

* :rust:`///` indicates documentation comments

  * Code blocks in document comments become doctests

    * Executed as part of test

  * Lines 3 and 7 delineate the actual test code
  * Line 4 indicates test code not included in documentation

* Document comments are included in documentation

------------------------------
Running "doctest" Standalone
------------------------------

:command:`cargo test --doc`

.. code:: output
  :font-size: tiny

     Doc-tests adder

  running 1 test
  test src\lib.rs - add (line 3) ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.04s

  all doctests ran in 1.05s; merged doctests compilation took 0.53s

--------------------------------
"doctest" as Part of Full Test
--------------------------------

:command:`cargo test`

.. code:: output
  :font-size: tiny

     Running unittests src\lib.rs (target\debug\deps\adder-f5d94484c9544f76.exe)

  running 1 test
  test tests::it_works ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running tests\integration_test.rs (target\debug\deps\integration_test-4ee73614a3903bd6.exe)

  running 1 test
  test add_positive ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Doc-tests adder

  running 1 test
  test src\lib.rs - add (line 3) ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.02s

  all doctests ran in 1.04s; merged doctests compilation took 0.58s

-------------------------------
Generating HTML Documentation
-------------------------------

* To generate documentation: :command:`cargo doc`

  * Generates HTML in :filename:`target/doc/adder/index.html`

**Adder module**

  .. image:: rust_essentials/doctest_index_html.png

**add function**

  .. image:: rust_essentials/doctest_fn_add_html.png
