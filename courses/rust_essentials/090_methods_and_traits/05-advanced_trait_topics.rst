=======================
Advanced Trait Topics
=======================

-------------
Supertraits
-------------

* A :dfn:`supertrait` is a trait that requires *another* trait

  * "If you implement this trait, you must also implement that one"

*Anything that is a "party animal" must be able to dance**

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

**Explanation**

  * To be a :rust:`PartyAnimal` you must know how to :rust:`Dance`

----------------------
Advanced Supertraits
----------------------

* A supertrait can depend on *multiple* traits

**To be the "life of the party" you must be able to sing AND dance**

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

**Explanation**

  * To be a :rust:`LifeOfParty` you must know how to :rust:`Dance` and :rust:`Sing`

------------------
Associated Types
------------------

* An :dfn:`associated type` is a type placeholder defined inside a trait

  * Chosen by the implementing type

.. code:: rust

  trait Animal {
    type Food; // associated type
    fn consume(&self, food: Self::Food);
  }

  struct Cat;
  struct Catnip;

  impl Animal for Cat {
      // We associate Catnip with Cat
      type Food = Catnip; 
      fn consume(&self, food: Catnip) {
          println!("The cat purrs intensely over the catnip.");
      }
  }

* Key Points

  * Implementer decides type once

    * Becomes "property" of implementation

  * Use when only one logical choice for helper type per implementation
