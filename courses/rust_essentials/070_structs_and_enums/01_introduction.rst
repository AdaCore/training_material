==============
Introduction
==============

----------------
Topics Covered
----------------

-  **Structs**: Grouping Related Data 

   -  Initialization & update syntax
   
   -  Named vs Tuple forms

-  **Enums**: Defining Distinct Options

   -  Variants holding Data
   
   -  State machines

-  **Type Aliases**

   - Renaming types for readability  


--------------
Introduction
--------------

-  :rust:`struct` and :rust:`enum` are user defined types 

   -  Create custom data structures 
   -  Bundle related pieces of information together
   -  Define their behavior

.. code:: rust

	struct Point {
		x_coordinate: f64,
		y_coordinate: f64,
		z_coordinate: f64,
	}
	enum Direction {
		North,
		East,
		South,
		West,
	}


