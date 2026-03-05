==============================
Generic Traits and Constants
==============================

----------------
Generic Traits
----------------

-  Traits can be made generic

   -  Allows the same trait to behave differently with each type

.. code:: rust

	// 'T' is the "Target" type we want to turn into
	trait Transform<T> {
	  fn convert(&self) -> T;
	}
	struct Minutes(i32);
	// Rule for converting Minutes to Seconds
	impl Transform<i32> for Minutes {
		fn convert(&self) -> i32 { self.0 * 60 }
	}
	// Rule for converting Minutes to a String
	impl Transform<String> for Minutes {
	  fn convert(&self) -> String { format!("{} mins", self.0) }
	
----------------
Const Generics 
----------------

-  No generic constant declaration

-  *Const Generics* are generic over a **value** not a type

.. code:: rust

  struct Buffer<const N: usize> {
    data: [i32; N],
  }
  
  let small_buffer = Buffer::<10> { data: [0; 10] };
  let large_buffer = Buffer::<1024> { data: [0; 1024] };

-  :rust:`Buffer<10>` and :rust:`Buffer<1024>` are two different types
