=====
Lab
=====

------------------
Lab Instructions
------------------

* A rover needs at least *20%* battery to launch
* **Task 1: Unit tests** in :filename:`src/lib.rs`

  * Check the 19% and 20% boundary
  * Check that a 101% reading panics with "invalid battery level"

* **Task 2: Doctest** in :filename:`src/lib.rs`

  * Document examples returning both "GO" and "WAIT"

* **Task 3: Integration test** in :filename:`tests/mission.rs`

  * Test :rust:`mission_status` at 0%, 19%, 20%, and 100%
  * Include the battery level in assertion failure messages

* Run :command:`cargo test`, find the bug, and fix it

.. tip::

  The goal is to test behavior, not to fix compilation errors
