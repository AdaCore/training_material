=========
Structs
=========

---------
Structs
---------

Like tuples, Struct can also be destructured by matching:

.. code:: rust

   {{#include ../../third_party/rust-by-example/destructuring-structs.rs}}

.. raw:: html

---------
Details
---------

-  Change the literal values in :rust:`foo` to match with the other
   patterns.
-  Add a new field to :rust:`Foo` and make changes to the pattern as needed.
-  The distinction between a capture and a constant expression can be
   hard to spot. Try changing the :rust:`2` in the second arm to a variable,
   and see that it subtly doesn't work. Change it to a :rust:`const` and see
   it working again.

.. raw:: html

