==============
Introduction
==============

----------------
Topics Covered
----------------

-  **Structs**: 

   -  Group related data 

   -  Initialization and update syntax
   
   -  Named-field vs Tuple forms

-  **Enums**: 

   -  Define distinct options

   -  Variants holding data
   
   -  State machines



--------------
Introduction
--------------

-  :rust:`struct` and :rust:`enum` create custom data structures  

   -  User-defined types 
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


