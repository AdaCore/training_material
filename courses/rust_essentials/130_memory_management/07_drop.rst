========
"Drop"
========

---------------------
Destructor ("Drop")
---------------------

- Deterministic clean-up implemented with :rust:`Drop` Trait
  - Occurs *implicitly*, and usually at the closing brace :rust:`}`
  - Calling :rust:`.drop()` manually results in a compiler error
- Ideal for resource management, e.g, closing files or network sockets

.. code:: rust

  struct Mic {
      owner: String,
  }

  impl Drop for Mic {
      fn drop(&mut self) {
        println!("{} just dropped!", self.owner);
      }
  }

.. note:: 

   Saying a type has a *destructor* means it implements the :rust:`Drop` trait

-----------------------------
Variable Drop Order Example
-----------------------------

.. container:: latex_environment scriptsize

  .. container:: columns

    .. container:: column
        :width: 50%

          .. code:: rust

            struct Potato {
                id: String,
            }

            impl Drop for Potato {
                fn drop(&mut self) {
                    println!("Dropping {}", self.id);
                }
            }

    .. container:: column
        :width: 50%

          .. code:: rust

            fn main() {
              let tic = "tic".into();
              let tac = "tac".into();
              let toe = "toe".into();
              let _a = Potato { id: tic };
              let _b = Potato { id: tac };
              {
                  let _c = Potato { id: toe };
              } // Dropping 'toe'
            } // Dropping 'tac'
              // Dropping 'tic'

.. note::

  Variables are dropped in reverse order of their creation

-----------------------------------
Internal Field Drop Order Example
-----------------------------------

.. container:: latex_environment scriptsize

  .. container:: columns

    .. container:: column
        :width: 50%

          .. code:: rust

            struct Eggs;
            struct Bacon;

            impl Drop for Eggs {
                fn drop(&mut self) {
                    println!("Dropping eggs!");
                }
            }

            impl Drop for Bacon {
                fn drop(&mut self) {
                    println!("Dropping bacon!");
                }
            }

    .. container:: column
        :width: 50%

          .. code:: rust

            struct Breakfast {
                one: Eggs,
                two: Bacon,
            }

            fn main() {
                let _meal = Breakfast {
                    one: Eggs,
                    two: Bacon,
                };
            } // Dropping eggs! 
              // Dropping bacon!

.. note::

  Internal fields are dropped in the order they are declared

---------------
Explicit Drop
---------------

- Early clean-up is possible by calling :rust:`std::mem::drop`
- :rust:`std::mem::drop` (in *prelude*) is an empty generic function that
  - Captures ownership of the passed value
  - Triggers the :rust:`Drop` mechanism as the value goes out of scope

.. code:: rust

  let my_precious = String::from("The One Ring");
  drop(my_precious); // 'my_precious' is moved then dropped

  println!("{}", my_precious);

:error:`error[E0382]: borrow of moved value: 'my_precious'`

.. warning::

   :rust:`std::mem::drop` differs from :rust:`std::ops::Drop::drop`

----------------------------------
Exclusivity of "Copy" and "Drop"
----------------------------------

- Type cannot implement both the :rust:`Copy` and :rust:`Drop` traits
  - Implementing :rust:`Drop` guarantees destructor runs exactly once
- :rust:`Copy` implies a simple bitwise replication

.. code:: rust

  // This code will not compile
  #[derive(Copy, Clone)] 
  struct Highlander;

  impl Drop for Highlander {
    fn drop(&mut self) {
        println!("There can be only one!");
    }
  }

.. container:: latex_environment tiny

  :error:`error[E0184]: the trait 'Copy' cannot be implemented for this type; the type has a destructor`
