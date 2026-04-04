==========
"anyhow"
==========

-------------------------
Flexible Error Handling
-------------------------

* Don't always want an enum for every error

  * Just want to propagate and report them

* :rust:`anyhow::Result<T>`

  * Available via **anyhow** crate
  * Wraps any error implementing :rust:`std::error::Error`

* Primarily used for applications (**not** libraries)

  * Effortless error propagation
  * But libraries should return specific errors

.. note::

  :rust:`anyhow` comes from a crate - needs to be installed

---------------------------
One Type to Rule Them All
---------------------------

* One :rust:`anyhow::Result` can handle many different errors

  * Instead of writing

    .. code:: rust

      fn run_app() -> Result<(), MyCustomError>

  * Simpler to write

    .. code:: rust

      fn run_app() -> anyhow::Result<()>

* Type is compatible with any function that uses :rust:`?` operator

.. container:: latex_environment small

  .. code:: rust

    use anyhow::Result;

    fn run_app() -> Result<()> {
        let config = read_config()?;    // Could be 'io::Error'
        let data = parse_data(config)?; // Could be 'ParseError'
        Ok(())
    }

-----------------------
Methods to Add Detail
-----------------------

* :rust:`.context()`

  * Attaches message to error
  * On failure, user sees *your* message *plus* original error

* :rust:`.with_context()`

  * Only evaluates message if an error *actually* occurs
  * Better for performance with complex messages

.. code:: rust

  use anyhow::Context;

  fn main() -> Result<()> {
      let path = "config.json";
      let content = std::fs::read_to_string(path)
          .with_context(|| format!("Failed config file {path}"))?;
    
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

    * - *Best For*
      - Libraries
      - Applications

    * - *Error Type*
      - Strongly typed
      - General error

    * -
      - (enums)
      - (:rust:`anyhow::Error`)

    * - *Goal*
      - Custom errors with no boilerplate
      - Simplifies propagation

    * - *Matching*
      - Easy to :rust:`match`
      - Harder (requires *downcasting*)

-------------------------
Common Error Operations
-------------------------

* :rust:`anyhow!`

  * Create an error on the fly from a string
  * Similar to :rust:`format!`

  .. code:: rust

    let my_error = anyhow!("Something bad happened");

* :rust:`bail!`

  * Shorthand for :rust:`return Err(anyhow!(...))`
  * Great for early exits

  .. code:: rust

    if user.name.is_empty() {
        bail!("User name cannot be empty!");
    }

* Printing errors

  * Top-level (outermost) error message

    .. code:: rust

      println!("{}", report);

  * Error and every "source" (cause) underneath

    .. code:: rust

      println!("{:#}", report);

    * Also called :dfn:`developer view`
