===============
"Error" Trait
===============

-------------------
The "Error" Trait
-------------------

* **Purpose**

  * Provides common interface for all error types

* **Why it exists**

  * Without a standard trait, every library would have its own way of describing errors
  * Makes it impossible to write generic error-handling code

* **Requirements**

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

* :rst:`source()` method

  * Key to *Error Chaining*
  * Allows you to peel back layers of error to find root cause
  * E.g a "network error" caused by a "timeout"

------------------------
Implementing the Trait
------------------------

A Manual Example

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

.. note::

  Manual implementation can be heavy.

  Instead, we could use crates like :rust:`thiserror`
  (described in the next chapter).

---------------
Trait Objects
---------------

* When you don't know what specific error will happen, use :rust:`Box<dyn Error>`

  * Or don't care!

* :rust:`'static` bound

  * Typically see :rust:`Box<dyn Error + 'static>`
  * Ensures the error doesn't contain any temporary references

    * (Might disappear while the error is being passed around)

* You have a :rust:`dyn Error` but need to check if it's :rust:`Network` error

  * Can use :rust:`.downcast_ref::<MyError>()`

--------------------------
Best Practice for Errors
--------------------------

* Implement :rust:`Display` - for the "what happened" summary

* Implement :rust:`Debug` - for the "where/how" technical details

* Implement :rust:`Error` - so your type is compatible with everybody else

* Use :rust:`source()` - to preserve the history of what went wrong
