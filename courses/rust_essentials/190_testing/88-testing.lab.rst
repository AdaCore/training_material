=============
Testing Lab
=============

------------------
Lab Instructions
------------------

* A rover needs at least *20%* battery to launch
* **Task 1: Unit tests** in :filename:`src/lib.rs`

  * Check the 19% and 20% boundary
  * Expect a panic for an invalid 101% reading

* **Task 2: Doctest** in :filename:`src/lib.rs`

  * Document examples returning both "GO" and "WAIT"

* **Task 3: Integration test** in :filename:`tests/mission.rs`

  * Test :rust:`mission_status` at 0%, 19%, 20%, and 100%

* Run :command:`cargo test`, find the bug, and fix it

.. tip::

  The goal is to test behavior, not to fix compilation errors
