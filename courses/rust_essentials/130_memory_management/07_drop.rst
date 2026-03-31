========
"Drop"
========

---------------------
Destructor ("Drop")
---------------------

- Deterministic clean-up implemented with :rust:`Drop` trait
  - Occurs *implicitly*, and usually at the closing brace ":rust:`}`"
  - Calling :rust:`.drop()` manually results in a compile error
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
              let s1 = "tic".into();
              let s2 = "tac".into();
              let s3 = "toe".into();
              let _tic = Potato { id: s1 };
              {
                let _tac = Potato { id: s2 };
              }
              let _toe = Potato { id: s3 };
            }

:command:`Dropping tac`

:command:`Dropping toe`

:command:`Dropping tic`

.. note::

  Variables are dropped in *reverse order* of their creation

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
            } 

:command:`Dropping eggs!`

:command:`Dropping bacon!`

.. note::

  Internal fields are dropped in the *order* they are declared

---------------
Explicit Drop
---------------

- Early clean-up is possible by calling :rust:`std::mem::drop`
- :rust:`std::mem::drop` (in *prelude*) is an empty generic function that
  - Captures ownership of passed value
  - Triggers :rust:`Drop` mechanism as value goes out of scope

.. code:: rust

  let my_precious = String::from("The One Ring");
  drop(my_precious); // 'my_precious' is moved then dropped

  println!("{}", my_precious); // Error

:error:`error[E0382]: borrow of moved value: 'my_precious'`

.. note::

   :rust:`std::mem::drop` differs from :rust:`std::ops::Drop::drop`

----------------------------------
Exclusivity of "Copy" and "Drop"
----------------------------------

- Type cannot implement both :rust:`Copy` and :rust:`Drop` traits
  - Implementing :rust:`Drop` guarantees destructor runs *exactly once*
- :rust:`Copy` implies simple bitwise replication

.. code:: rust

  #[derive(Copy, Clone)] // This line will not compile
  struct Highlander;

  impl Drop for Highlander {
    fn drop(&mut self) {
        println!("There can be only one!");
    }
  }

.. container:: latex_environment tiny

  :error:`error[E0184]: the trait 'Copy' cannot be implemented for this type; the type has a destructor`
