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
  
-----------------
Using the Trait
-----------------

* Return some kind of error

  .. container:: latex_environment footnotesize

    .. code:: rust

      fn do_something(fail: bool) -> Result<(), Box<dyn Error>> {
          if fail {
              let err = MyError {
                  details: String::from("Something went wrong!"),
              };
              // We box the error to erase its specific type
              return Err(Box::new(err));
          }
          Ok(())
      }

* Receive error and check for a specific problem

  .. container:: latex_environment footnotesize

    .. code:: rust
      :number-lines: 2

      match do_something(true) {
          Ok(_) => println!("Success!"),
          Err(e) => {
              // Attempt to downcast to our specific type
              if let Some(specific_err) = e.downcast_ref::<MyError>() {
                  println!("Caught a specific error: {}", specific_err.details);
              } else {
                  println!("Caught an unknown error type: {}", e);
              }
          }
      }

.. note::

  Line 6 :rust:`.downcast_ref::<MyError>()` "unboxes" error using :rust:`Option`

----------------------------------
Best Practices for Custom Errors
----------------------------------

**Implement methods for the following traits**

  * :rust:`Display`

    * Tell user what happened

  * :rust:`Debug`

    * Tell programmer what happened and where

  * :rust:`Error`

    * Allow everyone to know what happened
