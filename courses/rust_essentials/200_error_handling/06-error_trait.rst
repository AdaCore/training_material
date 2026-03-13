===============
"Error" Trait
===============

-------------------
The "Error" Trait
-------------------

* Provides common interface for all error types

* Without standard trait, libraries would have own way of describing errors

  * Makes it impossible to write generic error-handling code

* To implement :rust:`Error` your type must also implement

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


* Instead, can use crates like :rust:`thiserror`
  
  * Described in next chapter

---------------
Trait Objects
---------------

* When you don't know what specific error will happen

  * Use :rust:`Box<dyn Error>`
  * Or don't care!

* :rust:`'static` bound

  * Typically see :rust:`Box<dyn Error + 'static>`
  * Ensures error can live for entire program duration

    * Safe to pass across boundaries

* You have a :rust:`dyn Error` but need to check if it's :rust:`Network` error

  * Can use :rust:`.downcast_ref::<MyError>()`

--------------------------
Best Practice for Errors
--------------------------

* Implement :rust:`Display` - for the "what happened" summary

* Implement :rust:`Debug` - for the "where/how" technical details

* Implement :rust:`Error` - so your type is compatible with everybody else

* Use :rust:`source()` - to preserve the history of what went wrong
