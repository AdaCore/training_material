==========
Generics
==========

-------------------
Generic Data Type
-------------------

-  Used to parameterize an object

   -  Declared using **<>**
   
-  Can take any identifier name

   -  Conventionnaly caled :rust:`T`
   
.. code:: rust

   enum Option<T> {
    None,
    Some(T),
   }
   
-  :rust:`T` generic type parameter, means an Option can wrap any type

   -  Option<i32>, Option<String>, or even Option<Option<u32>>

-  :rust:`None` represents the absence of a value

-  :rust:`Some(T)` tuple that holds a value of type :rust:`T`

------------
Be Generic
------------

-  Objects that can be made generic

.. container:: latex_environment scriptsize

    .. list-table:: 
       :header-rows: 1
       
       * - Feature
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

 

---------
Example 
---------

-  Any type with a known **size** can specify the type argument    

   -  Type argument can have additional constraints 

.. code:: rust


    // The Definition: T is a placeholder for ANY type
    fn encourage<T>(item: T) -> T {
        println!("You're doing great, little value!");
        item 
    }

    fn main() {
        // Usage with an integer
        let points = encourage(100); 

        // Usage with a string
        let name = encourage("Rustacean");
}

-----------------------
Multiple Generic Type
-----------------------

-  Objects can have multiple generic data types 

.. code:: rust

    struct Point<T, U> {
        x: T,
        y: U,
    }

    fn main() {
        let both_integer = Point { x: 5, y: 10 };
        let both_float = Point { x: 1.0, y: 4.0 };
        let integer_and_float = Point { x: 5, y: 4.0 };
    }
	

-----------
Turbofish
-----------

-------------------
Technical details
-------------------



-  This is similar to C++ templates, but Rust partially compiles the
   generic function immediately, so that function must be valid for all
   types matching the constraints. For example, try modifying :rust:`pick`
   to return :rust:`even + odd` if :rust:`n == 0`. Even if only the :rust:`pick`
   instantiation with integers is used, Rust still considers it invalid.
   C++ would let you do this.
