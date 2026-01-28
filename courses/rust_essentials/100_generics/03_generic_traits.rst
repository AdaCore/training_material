====================
Generic and Traits
====================

--------------
Trait bounds
--------------

-  Generic data <T> is very broad 

-  Compiler will restrict what can be done with <T> 
   -  Doesn't know if number or string or else
   
-  Traits are **Fine Prints** on a generic contract

   -  Ensure the logic only executes on types that "fit" the requirements
   
   -  Added in the <> next to the type

.. code:: rust

	fn print_it<T: std::fmt::Display>(item: T) {
		println!("{}", item); // Only works if T implements Display
	}

   -  or with a :rust:`where` clause
   
.. code:: rust

	fn complex_function<T, U>(t: T, u: U) 
    where 
        T: Display, 
        U: Debug
    { ... }

-----------------
Multiple Traits
-----------------

-  Can have multiple trait bounds

   -  using the :rust`+` operator


.. code:: rust

	fn complex_function<T, U>(t: T, u: U) 
    where 
        T: Display + Clone, 
        U: Debug + PartialOrd 
    { ... }




----------------
Generic Traits
----------------






