==============
Introduction
==============

----------------
Topics Covered
----------------

-  **Generic data types**

   -  Definition and instantiation

-  **Genericity and traits**

   -  Adding constraints and properties
  
   -  Generic traits


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

* It would be nice to extract these properties in some common pattern, and then just replace the parts that need to be replaced

.. code:: Rust

    fn Swap (l: (a_type), r: (a_type)) -> ((a_type), (a_type)) {
		(r, l)
	}
