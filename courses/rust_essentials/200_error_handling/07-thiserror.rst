=============
"thiserror"
=============

---------------------------------
"Macro Magic" for Custom Errors
---------------------------------

* Manually implementing :rust:`Display`, :rust:`Debug`, and :rust:`Error::source`

  * Tedius and error-prone to do for every enum

* :rust:`thiserror` crate provides a convenient **derive macro**

  * Generates all that code for you using simple attributes

---------------------
"thiserror" Example
---------------------

.. code:: rust
  :number-lines: 1

  #[derive(Debug, Error)]
  enum MyError {
      #[error("I/O error: {0}")]
      IoError(#[from] io::Error),
      #[error("No text in {0}")]
      EmptyText(String),
}

* Line 1: generate :rust:`std::error::Error` implementation for :rust:`MyError`
* Line 3: Replace :rust:`fmt::Display` implementation

  * Uses this string with :rust:`IoError` value when printing error

* Line 5: generate :rust:`impl From<io::Error> for :rust:`MyError`

  * :rust:`?` will convert error into :rust:`MyError::IoError`

-----------------
More Attributes
-----------------

* :rust:`#[source]`

  * Describes where the original error came from
  * :rust:`#[from]` implies :rust:`#[source]`

* :rust:`transparent` errors - :rust:`#[error(transparent)]`

  * Uses :rust:`Display` :rust:`from std::io::Error`

    * No need to write custom string

  * Automatically implements :rust:`source()` method

    * Returns underlying error

  * Does not add a new message

----------------------
Why Use "thiserror"?
----------------------

* Structured data

  * Enums allow :rust:`match` to handle specific errors

    * Rather than checking an error message string

* Type safety

  * Compiler ensures that error messages match enum fields

* Ecosystem compatibility

  * Generates a standard :rust:`std::error::Error` implementation
  * So errors work are compatible with other tools

    * Like standard library traits

-----------------------
"thiserror" in Action
-----------------------

.. container:: latex_environment scriptsize

  .. code:: rust

    fn get_config(path: &str) -> Result<Config, DataStoreError> {
        // If 'File::open' fails, it returns 'io::Error'
        // "?" converts the error via "from"
        let content = fs::read_to_string(path)?; 

        if content.is_empty() {
            return Err(DataStoreError::NotFound(path.to_string()));
        }

        Ok(parse_config(content))
    }
