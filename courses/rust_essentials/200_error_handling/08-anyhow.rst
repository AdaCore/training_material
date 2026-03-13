==========
"anyhow"
==========

-------------------------
Flexible Error Handling
-------------------------

* **The Problem**

  * Applications don't always want an enum for every possible error
  * Just want to catch *any* error, add context, and report it

* **The Solution**

  * :rust:`anyhow` crate provides a single type :rust:`anyhow::Result<T>`
  * Wraps any error type that implements :rust:`std::error::Error`

* **Primary Use Case**

  * Applications (not libraries)
  * Makes error propagation effortless

    * Without the boilerplate of custom enums

---------------------------
One Type to Rule Them All
---------------------------

* Written as :rust:`anyhow::Result<T>`

  * Instead of writing :rust:`Result<T, MyCustomError>`

* Type is compatible with any function that uses the :rust:`?` operator

* You can call functions from multiple libraries

  * Returning multiple error types
  * Use :rust:`?` on all of them in the same function

.. code:: rust

  use anyhow::Result;

  fn run_app() -> Result<()> {
      let config = read_config()?; // Could be io::Error
      let data = parse_data(config)?; // Could be ParseError
      Ok(())
  }

----------------
Adding Context
----------------

* Ability to attach "human" context to a technical error

  * Primary feature of :rust:`anyhow`

* Methods

  * :rust:`context`

    * Attaches message to error
    * If operation fails, the user sees *your* message *plus* the original error

  * :rust:`with_context`

    * The "lazy" version
    * Only evaluates the message if an error *actually* occurs
    * Better for performance with complex strings

.. code:: rust

  use anyhow::Context;

  fn main() -> Result<()> {
      let path = "config.json";
      let content = std::fs::read_to_string(path)
          .with_context(|| format!("Failed to read config file at {path}"))?;
    
      Ok(())
  }

-------------------------
Choosing the Right Tool
-------------------------

.. container:: latex_environment footnotesize

  .. list-table::
    :header-rows: 1

    * - **Feature**
      - :rust:`thiserror`
      - :rust:`anyhow`

    * - *User*
      - Library authors
      - Application authors

    * - *Error Type*
      - Strongly typed
      - Erased type

    * -
      - (enums)
      - (:rust:`anyhow:Error`)

    * - *Primary Goal*
      - Help *caller* handle specific cases
      - Help *user* understand what happened

    * - *Matching*
      - Easy to :rust:`match` on variants
      - Harder (requires "downcasting")

--------------------------------
Printing and Triggering Errors
--------------------------------

* :rust:`anyhow!` macro

  * Create an error on the fly from a string
  * Similar to :rust:`format!`

* :rust:`bail!` macro

  * Shorthand for :rust:`return Err(anyhow!(...))`
  * Great for early exits

* Printing

  * When you print an :rust:`anyhow::Error` with the "alternate" flag (:rust:`{:#}`)

    * Automatically prints the entire chain of causes
    * From high-level context down to the root cause

.. code:: rust

  if user.name.is_empty() {
      bail!("User name cannot be empty!");
  }
