===============
"Error" Trait
===============

-------------------
The "Error" Trait
-------------------

* Common interface for all errors

  * Defined in :rust:`std::error`
  * Implemented by many standard and custom error types

* All errors should use :rust:`Error` trait

  * Without standard trait, libraries would have their own errors

---------------------
Trait Prerequisites
---------------------

To implement :rust:`Error` type must also implement

  * :rust:`Display`

    * For user-facing error messages

  * :rust:`Debug`

    * For developer-facing details

------------------
Trait Definition
------------------

.. code:: rust

  use std::fmt::{Debug, Display};

  pub trait Error: Debug + Display {
      // Returns the lower-level cause of this error, if any
      fn source(&self) -> Option<&(dyn Error + 'static)> {
          None
      }
  }

:rust:`source()` **method**

  * Key to *Error Chaining*
  * Allows you to peel back layers of error to find root cause
  * E.g a "network error" caused by a "timeout"

.. note::

  :rust:`Source` returns an :rust:`Option` type where :rust:`Error` is

  * :rust:`dyn` - trait object (not a fixed-size type)
  * :rust:`'static` - has no temporary references 

------------------------
Implementing the Trait
------------------------

* Manual implementation can be heavy

.. container:: latex_environment tiny

  .. code:: rust

    #[derive(Debug)]
    enum MyError {
        Network(io::Error),
        BadInput,
    }

    impl Display for MyError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::Network(e) => write!(f, "Network issue: {e}"),
                Self::BadInput => write!(f, "Invalid user input"),
            }
        }
    }

    // Finally, implement the Error trait itself
    impl std::error::Error for MyError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match self {
                Self::Network(e) => Some(e),
                _ => None,
            }
        }
    }


.. tip::

  Instead, use crates like :rust:`thiserror`
  
---------------
Trait Objects
---------------

* Typically see :rust:`Box<dyn Error + 'static>`

* :rust:`Box<dyn Error>`

  * Use when function could fail with different errors
  * Hides specific type

    * Can use the ? operator everywhere

* :rust:`'static` bound

  * Ensures error can live for entire program duration

    * Safe to pass across boundaries

* You have a :rust:`dyn Error` but need to check if it's :rust:`Network` error

  * Can use :rust:`.downcast_ref::<MyError>()`

----------------------------------
Best Practices for Custom Errors
----------------------------------

* Implement methods for following traits

  * :rust:`Display`

    * Tell user what happened

  * :rust:`Debug`

    * Tell programmer what happened and where

  * :rust:`Error`

    * Allow everyone to know what happened
