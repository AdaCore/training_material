==============
Unit Structs
==============

--------------
Unit Structs
--------------

-  These structs have no fields and are mostly used when you don't need to hold any value like states or events

.. code:: rust

	// A struct that represents an event that has occurred
	struct Shutdown;

	// How you use it:
	let system_shutdown = Shutdown;
	// ... can be used later to check the type