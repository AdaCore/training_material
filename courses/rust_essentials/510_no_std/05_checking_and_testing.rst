=======================
Checking Configurations
=======================

------------------------------------
Check Each Supported Configuration
------------------------------------

**Each supported configuration needs a separate compilation check**

.. code:: sh

  cargo check --lib --no-default-features
  cargo check --lib --no-default-features --features alloc
  cargo check --lib

* These check the core-only, allocating, and default :rust:`std` configurations

* :command:`--all-features` alone would miss the smaller configurations

-------------------------------
What Host Checks Do Not Prove
-------------------------------

* Host build may succeed even if a dependency still uses :rust:`std`

* :command:`cargo check` checks code but does not produce a final linked program

* :rust:`no_std` alone does not prove portability to every platform

.. tip::

  Dependency feature inspection and target checks address different portability risks

------------------------------
Check a Target Without "std"
------------------------------

**Target-specific checks complement host feature checks**

.. code:: sh

  rustup target add thumbv7em-none-eabi
  cargo check --lib --no-default-features \
      --target thumbv7em-none-eabi

* Example target does not provide :rust:`std`

* :command:`cargo check` verifies compilation, not linking or execution
