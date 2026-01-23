=======================
Advanced Trait Topics
=======================

-------------
Supertraits
-------------

* What is a :dfn:`supertrait`?

  * A trait that requires another trait
  * "If you implement this trait, you must also implement that one"

.. container:: columns

  .. container:: column

    .. container:: latex_environment scriptsize

      .. code:: rust

        trait Animal {
          fn leg_count(&self) -> u32;
        }

        trait Pet: Animal {
          fn name(&self) -> String;
        }

        struct Dog(String);

  .. container:: column

    .. container:: latex_environment scriptsize

      .. code:: rust

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

* Key Points

  * :rust:`Pet` inherits requirements from :rust:`Animal`
  * Implementers must satisfy **all** supertraits
  * Enables trait APIs that *assume other capabilities*

------------------
Associated Types
------------------

* What is an :dfn:`associated type`?

  * Type placeholder defined inside a trait
  * Chosen by the implementing type

.. code:: rust

  trait Multiply {
      type Output;
      fn multiply(&self, other: &Self) -> Self::Output;
  }

  // when we implement Multiply, we need to specify
  // what the Output type should be
  impl Multiply for Meters {
      type Output = MetersSquared;
      fn multiply(&self, other: &Self) -> Self::Output {
          MetersSquared(self.0 * other.0)
      }
  }

* Key Points

  * Avoids extra generic parameters
  * Ties a type directly to a trait implementation
  * Improves readability and type inference
