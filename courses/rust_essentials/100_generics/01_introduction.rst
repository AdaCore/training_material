==============
Introduction
==============

----------------
Topics Covered
----------------

-  **Generic Data Types**

   -  Definition and instantiation

-  **Constraints and Properties** 
  
-  **Generic Traits and Constants**


-------------------------
The Notion of a Pattern
-------------------------

* Sometimes algorithms can be abstracted from types and subprograms

.. code:: Rust

    fn swap_int(l: i32, r: i32) -> (i32, i32) {
		(right, left) 
	}

    fn swap_float(l: f64, r: f64) -> (f64, f64) {
		(right, left) 
	}

*  A common pattern can be extracted, with only some parts to replace

.. code:: Rust

    fn Swap (l: a_type, r: a_type) -> (a_type, a_type) {
		(r, l)
	}
