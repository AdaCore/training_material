==============
Introduction
==============

----------------
Topics Covered
----------------

-  **Structs** (Data Aggregation): 

   -  **Initialization**: The mandatory rule of full field initialization

   -  **Syntax**: Using the Struct Update Operator (..) for concise updates

   -  **Forms**: Understanding Named Structs, Tuple Structs, and the Newtype Idiom for robust type safety

-  **Enums** (Data Choices):

   -  **Associated Data**: How variants can optionally carry their own data (tuple or struct-like)

   -  **Usage**: Modeling real-world concepts like State Machines (e.g., connection status)

-  **Type Aliases**: Simple renaming of an existing complex type to improve code readability

--------------
Introduction
--------------

-  User-defined types are a core concept in Rust

   -  Create custom data structures 
   -  Bundle related pieces of information together
   -  Define their behavior

-  In Rust, user-defined types are created with :rust:`struct` and :rust:`enum`

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


