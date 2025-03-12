=====================================
Exercise: Modules for a GUI Library
=====================================

-------------------------------------
Modules for a GUI Library Setup
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

-----------------
Source (1 of 2)
-----------------

Here's the single-module implementation of the GUI library:

.. container:: source_include 180_modules/src/180_modules.rs :start-after://ANCHOR-single_module_1 :end-before://ANCHOR-single_module_2 :code:rust

-----------------
Source (2 of 2)
-----------------

.. container:: source_include 180_modules/src/180_modules.rs :start-after://ANCHOR-single_module_2 :code:rust

----------
Solution
----------

Directory structure should look like:

::

   |-- main.rs
   |-- widgets
   |   |-- button.rs
   |   |-- label.rs
   |   |-- window.rs
   |-- widgets.rs

---------------------------
Solution - Module main.rs
---------------------------

.. container:: source_include 180_modules/src/180_modules-main.rs :code:rust

-------------------------------------
Solution - Module widgets/button.rs
-------------------------------------

.. container:: source_include 180_modules/src/180_modules-widgets-button.rs :code:rust

------------------------------------
Solution - Module widgets/label.rs
------------------------------------

.. container:: source_include 180_modules/src/180_modules-widgets-label.rs :code:rust

-------------------------------------
Solution - Module widgets/window.rs
-------------------------------------

.. container:: source_include 180_modules/src/180_modules-widgets-window.rs :code:rust

------------------------------
Solution - Module widgets.rs
------------------------------

.. container:: source_include 180_modules/src/180_modules-widgets.rs :code:rust

---------
Details
---------

Encourage students to divide the code in a way that feels natural for
them, and get accustomed to the required :rust:`mod`, :rust:`use`, and :rust:`pub`
declarations. Afterward, discuss what organizations are most idiomatic.
