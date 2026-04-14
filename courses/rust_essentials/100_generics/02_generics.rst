==========
Generics
==========

-------------------
Generic Data Type
-------------------

- Used to parameterize an object

   - Declared using :rust:`<>`
   
- Can take any identifier name

   - Conventionally called :rust:`T`
   
.. code:: rust

  fn swap<T> (l: T, r: T) -> (T, T) {
    (r, l)
  }
   
- :rust:`T` (:dfn:`generic type parameter`) means :rust:`swap` can wrap any type

   - :rust:`swap<i32>`, :rust:`swap<MyOwnType>`, etc.

------------
Be Generic
------------

- Constructs that can be made generic

.. container:: latex_environment scriptsize

    .. list-table:: 
       :header-rows: 1
       :stub-columns: 1
       
       * - **Constructs**
         - **Example Syntax**
         - **Purpose**
         
       * - *Functions*
         - :rust:`fn logic<T>(arg: T)`
         - Logic that works on multiple types


       * - *Structs*
         - :rust:`struct Container<T>(T)`
         - Data structures that hold any type


       * - *Enums*
         - :rust:`enum Choice<T> { A(T), B }`
         - Variants that can wrap different data


       * - *Traits*
         - :rust:`trait Behavior<T>`
         - Defining interfaces with generic inputs

       * - *Type Aliases*
         - :rust:`type Res<T> = Result<GenT>`
         - Simplifying complex generic names

- Examples

.. code:: rust

  enum LaundryStatus<T> {
    SoakingInWater(T),
    SpinningViolently(T),
  }
  type Laundry<T> = LaundryStatus<T>;


----------------
Type Inference 
----------------

- Any **Sized** type can be used as the type argument    

.. code:: rust

    // Definition: 'T' is a placeholder for ANY type
    fn encourage<T>(item: T) -> T {
        println!("You're doing great, little value!");
        item 
    }
	
- Type is **inferred** at compile-time from the context

.. code:: rust  

    // With an integer
    let points = encourage(100); 

    // With a string
    let name = encourage("Rustacean");


   
-----------------------
Multiple Generic Types
-----------------------

**Constructs can have multiple generic data types** 

.. code:: rust

    struct Point<T, U> {
        x: T,
        y: U,
    }

    let both_integer = Point { x: 5, y: 10 };
    let both_float = Point { x: 1.0, y: 4.0 };
    let integer_and_float = Point { x: 5, y: 4.0 };
    
    
--------------
Type Aliases 
--------------

- Can be used to rename types and generic parameters

.. code:: rust

  // 'Item' and 'Label' are generic parameters 
  struct LargeShippingUnit<Item, Label>(Item, Label);
  type LargeCrate<T, U> = LargeShippingUnit<T, U>;
  
- Can *specify* the generic type

  - **Partially**

  .. code:: rust

    struct Animal;
    type AnimalCrate<U> = LargeCrate<Animal, U>;
  
  - **Totally**

  .. code:: rust

    struct Environment;
    type Biome = LargeCrate<Animal, Environment>;  
