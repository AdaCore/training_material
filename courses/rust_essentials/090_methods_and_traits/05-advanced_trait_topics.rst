=======================
Advanced Trait Topics
=======================

-------------
Supertraits
-------------

* What is a :dfn:`supertrait`?

  * A trait that requires another trait
  * "If you implement this trait, you must also implement that one"

* Example: "Anything that is a 'party animal' must be able to dance"

  * Base trait

    .. code:: rust

      trait Dance {
          fn dance(&self);
      }

  * Supertrait

    .. code:: rust

      trait PartyAnimal: Dance {
          fn party(&self);
      }

* Explanation

  * "You cannot be a party animal unless you can already dance"

----------------------
Advanced Supertraits
----------------------

* A supertrait can depend on *multiple* traits

  * New base trait

    .. code:: rust

      trait Sing {
          fn Sing (&self);
      }

  * New supertrait

    .. code:: rust

      trait LifeOfParty: Dance + Sing {
          fn revel(&self);
      }

* Explanation

  * "You cannot be the life of the party unless you can sing *and* dance"

------------------
Associated Types
------------------

* What is an :dfn:`associated type`?

  * Type placeholder defined inside a trait
  * Chosen by the implementing type

.. code:: rust

  trait Animal {
    type Food; // associated type
    fn consume(&self, food: Self::Food);
  }

  struct Cat;
  struct Catnip;

  impl Animal for Cat {
      // We "associate" Catnip with Cat
      type Food = Catnip; 
      fn consume(&self, food: Catnip) {
          println!("The cat purrs intensely over the catnip.");
      }
  }

* Key Points

  * Implementer decides type once

    * Becomes "property" of implementation

  * Use when only one logical choice for helper type per implementation
