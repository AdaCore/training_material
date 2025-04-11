========
Traits
========

--------
Traits
--------

Rust lets you abstract over types with traits. They're similar to
interfaces:

.. code:: rust

   trait Pet {
       /// Return a sentence from this pet.
       fn talk(&self) -> String;

       /// Print a string to the terminal greeting this pet.
       fn greet(&self);
   }

A trait defines a number of methods that types must have in order to implement the trait.

---------------------
Implementing Traits
---------------------

.. code:: rust

   trait Pet {
       fn talk(&self) -> String;

       fn greet(&self) {
           println!("Oh you're a cutie! What's your name? {}", self.talk());
       }
   }

   struct Dog {
       name: String,
       age: i8,
   }

   impl Pet for Dog {
       fn talk(&self) -> String {
           format!("Woof, my name is {}!", self.name)
       }
   }

   fn main() {
       let fido = Dog { name: String::from("Fido"), age: 5 };
       fido.greet();
   }

-----------------------------
More on Implementing Traits
-----------------------------

- To implement :rust:`Trait` for :rust:`Type`, you use an
  :rust:`impl Trait for Type { .. }` block.

- Having matching methods is not enough

  - :rust:`Cat` type with a :rust:`talk()` method would not automatically satisfy :rust:`Pet`

    - unless it is in an :rust:`impl Pet` block.

- Traits may provide default implementations of some methods. Default
  implementations can rely on all the methods of the trait.

  - In the example, :rust:`greet` is provided, and relies on :rust:`talk`.

-------------
Supertraits
-------------

A trait can require that types implementing it also implement other
traits, called *supertraits*. Here, any type implementing :rust:`Pet` must
implement :rust:`Animal`.

.. code:: rust

   trait Animal {
       fn leg_count(&self) -> u32;
   }

   trait Pet: Animal {
       fn name(&self) -> String;
   }

   struct Dog(String);

   impl Animal for Dog {
       fn leg_count(&self) -> u32 {
           4
       }
   }

   impl Pet for Dog {
       fn name(&self) -> String {
           self.0.clone()
       }
   }

   fn main() {
       let puppy = Dog(String::from("Rex"));
       println!("{} has {} legs", puppy.name(), puppy.leg_count());
   }

.. note::

   This is sometimes called *trait inheritance* but it does not behave like object-oriented
   inheritance. It just specifies an additional requirement on implementations of a trait.

------------------
Associated Types
------------------

Associated types are placeholder types which are supplied by the trait
implementation.

.. code:: rust

   #[derive(Debug)]
   struct Meters(i32);
   #[derive(Debug)]
   struct MetersSquared(i32);

   trait Multiply {
       type Output;
       fn multiply(&self, other: &Self) -> Self::Output;
   }

   impl Multiply for Meters {
       type Output = MetersSquared;
       fn multiply(&self, other: &Self) -> Self::Output {
           MetersSquared(self.0 * other.0)
       }
   }

   fn main() {
       println!("{:?}", Meters(10).multiply(&Meters(20)));
   }

-------------------------------
More Info on Associated Types
-------------------------------

-  Associated types are sometimes also called *output types*. The key
   observation is that the implementer, not the caller, chooses this
   type.

-  Many standard library traits have associated types, including
   arithmetic operators and :rust:`Iterator`.
