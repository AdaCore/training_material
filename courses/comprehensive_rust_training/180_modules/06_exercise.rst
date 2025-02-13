=====================================
Exercise: Modules for a GUI Library
=====================================

-------------------------------------
Exercise: Modules for a GUI Library
-------------------------------------

In this exercise, you will reorganize a small GUI Library
implementation. This library defines a :rust:`Widget` trait and a few
implementations of that trait, as well as a :rust:`main` function.

It is typical to put each type or set of closely-related types into its
own module, so each widget type should get its own module.

-------------
Cargo Setup
-------------

The Rust playground only supports one file, so you will need to make a
Cargo project on your local filesystem:

.. code:: shell

   cargo init gui-modules
   cd gui-modules
   cargo run

Edit the resulting :filename:`src/main.rs` to add :rust:`mod` statements, and add
additional files in the :filename:`src` directory.

--------
Source
--------

Here's the single-module implementation of the GUI library:

.. code:: rust

   {{#include exercise.rs:single-module}}

.. raw:: html

---------
Details
---------

Encourage students to divide the code in a way that feels natural for
them, and get accustomed to the required :rust:`mod`, :rust:`use`, and :rust:`pub`
declarations. Afterward, discuss what organizations are most idiomatic.

.. raw:: html

