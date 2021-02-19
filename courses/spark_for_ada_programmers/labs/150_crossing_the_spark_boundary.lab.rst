---------------------------------
Crossing the SPARK Boundary Lab
---------------------------------

- Find the :filename:`140_crossing_the_spark_boundary` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane
- This is a set of exercises demonstrating how to interface to subsystems not written in SPARK.

.. container:: speakernote


   Lab should take about 30 minutes

---------------
Task Overview
---------------

* Write a SPARK binding package to a C language library

   - Library implements a simple interface to a hardware cryptographic processor.
   - The crypto processor has its own persistent, internal RAM

      + Stores 4 keys, which are numbered 0 through 3.
      + Key storage RAM is initialized by hardware on power-up

   - The library provides

      + Function to load a key into a specified slot
      + Function to encrypt a block of data using the key in a specified slot.

---------------------------------------
Remember These Steps From the Lecture
---------------------------------------

- Read the API documentation!
- Does the C API have persistent (possibly hidden) state

   + If so, how is it initialized?

- Consider how the subprogram will be specified in SPARK

   - If the SPARK subprogram can be bound directly to the C API

      * Use Annex B of the Ada RM to help create the binding

   - If the SPARK subprogram is incompatible with the C API

      * Create a SPARK wrapper in which an Ada binding to the C API is used

-----------------------------------
Design a Wrapper Package In SPARK
-----------------------------------

* Design the specification (declaration) only of a wrapper package in SPARK

   - First decide if the package needs to have persistent state

      + Consider whether the C library has persistent state
      + If so, model this state as a state abstraction.
      + Is any state abstraction external, volatile and is it initialized?

   - A skeletal package declaration is in the file `crypto.ads`

      + Contains some useful declarations.

* Examine the skeletal SPARK package declaration

   - **Review it with a course instructor before you proceed**

-------------------------------------
Create an Interface To a C Function
-------------------------------------

* A C function exists for loading a key into the board:

   .. code:: C++

    typedef unsigned char BYTE;
    int loadkey (int keyslot, BYTE *key);

* Definitions

   - `keyslot` is in the range 0 through 3 (denotes which key from the keystore)
   - `key` points to an array of 128 bytes
   - `loadkey` returns an error code in the range 0 through 1

      + Return of 0 denotes success
      + Return of 1 denotes failure

* Construct and examine a declaration of the SPARK binding to `loadkey`

   - Review this with the course instructor.

-------------------------------------------
Create an Interface To Another C Function
-------------------------------------------

* A C function also exists for encrypting a block of data:

   .. code:: C++

    int encrypt (int keyslot, BYTE *data);

* Definitions

  - `keyslot` is in the range 0 through 3 (denotes which key from the keystore)
  - `data` points to a block of 1024 bytes of data which is encrypted and returned in-place
  - `encrypt` returns an error code

      + Return of 0 denotes success
      + Return of 1 denotes failure

* Construct and examine a specification of the SPARK binding to `encrypt`

   - Review this with the course instructor.

-------------------------------
Implement the Wrapper Package
-------------------------------

* Write a body for the package `Crypto`

   - Subprogram bodies that bind the SPARK interface to the C API may require `SPARK_Mode => Off`.
   - Examine and/or Prove your implementation of the `Crypto` SPARK wrapper.

      + There should be no warnings or errors and all checks should prove.

* Hints

   - Since the error code is bi-valued (0 for success, 1 for failure), we could use `Boolean`

      + Remember there are no numeric values attached to Ada `True` and `False`.

   - :toolname:`GNAT Studio` can generate a package body framework from a package spec

      + :menu:`Code` |rightarrow| :menu:`Generate Body`

   - Remember in Ada, as opposed to SPARK, a function can have mode `out` or `in out` parameters.
