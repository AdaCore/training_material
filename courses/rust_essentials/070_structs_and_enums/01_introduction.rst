==============
Introduction
==============

----------------
Topics Covered
----------------

-  **Structs**: Grouping Related Data 

   -  Initialization and update syntax
   
   -  Named-field vs Tuple forms

-  **Enums**: defining distinct options

   -  Variants holding data
   
   -  State machines



--------------
Introduction
--------------

-  :rust:`struct` and :rust:`enum` are user-defined types 

   -  Create custom data structures 
   -  Bundle related pieces of information together
   -  Define their behavior

.. code:: rust

	struct PlayerStats {
		level: u16,
		health: u32,
		is_online: bool,
		score: i64,
	}
	enum Direction {
		North,
		East,
		South,
		West,
	}


