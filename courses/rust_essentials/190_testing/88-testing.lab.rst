=============
Testing Lab
=============

------------------
Lab Instructions
------------------

* A rover needs at least *20%* battery to launch
* **Unit tests** in :filename:`src/lib.rs`

  * Check the 19% and 20% boundary
  * Expect a panic for an invalid 101% reading

* **Integration test** in :filename:`tests/mission.rs`

  * Check that :rust:`mission_status` reports "WAIT" or "GO"

* **Doctest** in :filename:`src/lib.rs`

  * Document one working example of :rust:`mission_status`

* Run :command:`cargo test`, find the bug, and fix it

.. tip::

  The goal is to test behavior, not to fix compilation errors
