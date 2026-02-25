==========
Generics
==========

-------------------
Generic Data Type
-------------------

-  Used to parameterize an object

   -  Declared using :rust:`<>`
   
-  Can take any identifier name

   -  Conventionally called :rust:`T`
   
.. code:: rust

   fn Swap<T> (l: T, r: T) -> (T, T) {
		(r, l)
}

   
-  :rust:`T` (the generic type parameter) means an :rust:`Swap` can wrap any type

   -  :rust:`Swap<i32>`, :rust:`Swap<f64>` etc



------------
Be Generic
------------

Constructs that can be made generic

.. container:: latex_environment scriptsize

    .. list-table:: 
       :header-rows: 1
       
       * - Constructs
         - Example Syntax
         - Purpose
         
       * - Functions
         - :rust:`fn logic<T>(arg: T)`
         - Logic that works on multiple types


       * - Structs
         - :rust:`struct Container<T>(T)`
         - Data structures that hold any type


       * - Enums
         - :rust:`enum Choice<T> { A(T), B }`
         - Variants that can wrap different data


       * - Traits
         - :rust:`trait Behavior<T>`
         - Defining interfaces with generic inputs

       * - Type Aliases
         - :rust:`type Res<T> = Result<GenT>`
         - Simplifying complex generic names

-- JBE : very all options are present in examples 
-- Add Examples ?


----------------
Type Inference 
----------------

-  Any **Sized**-type can specify the type argument    

.. code:: rust


    // Definition: 'T' is a placeholder for ANY type
    fn encourage<T>(item: T) -> T {
        println!("You're doing great, little value!");
        item 
    }

    // Usage with an integer
    let points = encourage(100); 

    // Usage with a string
    let name = encourage("Rustacean");

-  Type is **infered** at compile-time from the context
   

-----------------------
Multiple Generic Type
-----------------------

Constructs can have multiple generic data types 

.. code:: rust

    struct Point<T, U> {
        x: T,
        y: U,
    }

    let both_integer = Point { x: 5, y: 10 };
    let both_float = Point { x: 1.0, y: 4.0 };
    let integer_and_float = Point { x: 5, y: 4.0 };
    
	




