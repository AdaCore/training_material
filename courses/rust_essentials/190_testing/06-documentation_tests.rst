=====================
Documentation Tests
=====================

----------------------
What is a "doctest"?
----------------------

* :dfn:`doctest` is a documentation test

  * Gets its test criteria from special comments

* Use :rust:`///` at the beginning of each test line

.. code:: rust

  /// Test positive/negative add
  ///
  /// ```
  /// # use adder::add;
  /// assert_eq!(add(3, 4), 7);
  /// assert_ne!(add(-3, -4), 7);
  /// ```

--------------------
Why Use "doctest"?
--------------------

* Comments describe actual behavior of code

  * Better / more recent than other documentation

* Doctests test external API

  * Rather than test modules that can test internal API

--------------------------
"doctest" Implementation
--------------------------

.. code:: rust
  :number-lines: 1

  /// Test positive/negative add
  ///
  /// ```
  /// # use adder::add;
  /// assert_eq!(add(3, 4), 7);
  /// assert_ne!(add(-3, -4), 7);
  /// ```

* "///" indicates a doctest
* Lines 3 and 7 delineate the actual test code
* Line 4 indicates test code not included in documentation

  * But does get compiled into code

* Remaining code is executed as part of test

  * And included in documentation

---------------------
Running a "doctest"
---------------------

* Just running "doctest" - :command:`cargo test --doc`

.. code:: output
  :font-size: tiny

     Doc-tests adder

  running 1 test
  test src\lib.rs - add (line 3) ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.04s
  
  all doctests ran in 1.05s; merged doctests compilation took 0.53s

* As part of full test run - :command:`cargo test`

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

-------------------
Generating Output
-------------------

* To generate documentation: :command:`rustdoc src/lib.rs`

  * Generates HTML in :filename:`doc/lib/index.html`

**Lib module**

  .. image:: rust_essentials/doctest_index_html.png

**add function**

  .. image:: rust_essentials/doctest_fn_add_html.png
