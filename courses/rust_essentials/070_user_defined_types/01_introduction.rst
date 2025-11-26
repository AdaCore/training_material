==============
Introduction
==============



----------------
Topics covered
----------------


This module will introduce how to create custom, reusable data types in Rust, serving as building blocks for complex applications.

-  **Structs** (Data Aggregation): 

   -  **Initialization**: The mandatory rule of full field initialization.

   -  **Syntax**: Using the Struct Update Operator (..) for concise updates.

   -  **Forms**: Understanding Named Structs, Tuple Structs, and the Newtype Idiom for robust type safety.

-  **Enums** (Data Choices):

   -  **Associated Data**: How variants can optionally carry their own data (tuple or struct-like).

   -  **Usage**: Modeling real-world concepts like State Machines (e.g., connection status).

-  **Type Aliases**: Simple renaming of an existing complex type to improve code readability.




--------------
Introduction
--------------

-  User-defined types are a core concept in Rust

   -  Create custom data structures that bundle related pieces of information together and define their behavior

-  In Rust, user-defined types are created with :rust:`struct` and :rust:`enum`

.. code:: rust

	struct Point {
		x: f64,
		y: f64,
		z: f64,
	}
	enum Direction {
		North,
		East,
		South,
		West,
	}


