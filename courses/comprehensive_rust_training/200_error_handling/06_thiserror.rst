===============
"thiserror"
===============

---------------
"thiserror"
---------------

The `thiserror <https://docs.rs/thiserror/>`__ crate provides macros
to help avoid boilerplate when defining error types. It provides derive
macros that assist in implementing :rust:`From<T>`, :rust:`Display`, and the
:rust:`Error` trait.

.. code:: rust

   use std::io::Read;
   use std::{fs, io};
   use thiserror::Error;

   #[derive(Debug, Error)]
   enum ReadUsernameError {
       #[error("I/O error: {0}")]
       IoError(#[from] io::Error),
       #[error("Found no username in {0}")]
       EmptyUsername(String),
   }

   fn read_username(path: &str) -> Result<String, ReadUsernameError> {
       let mut username = String::with_capacity(100);
       fs::File::open(path)?.read_to_string(&mut username)?;
       if username.is_empty() {
           return Err(ReadUsernameError::EmptyUsername(String::from(path)));
       }
       Ok(username)
   }

   fn main() {
       //fs::write("config.dat", "").unwrap();
       match read_username("config.dat") {
           Ok(username) => println!("Username: {username}"),
           Err(err) => println!("Error: {err:?}"),
       }
   }

---------
Details
---------

-  The :rust:`Error` derive macro is provided by :rust:`thiserror`, and has lots
   of useful attributes to help define error types in a compact way.
-  The message from :rust:`#[error]` is used to derive the :rust:`Display`
   trait.
-  Note that the (:rust:`thiserror::`)\ :rust:`Error` derive macro, while it has
   the effect of implementing the (:rust:`std::error::`)\ :rust:`Error` trait,
   is not the same this; traits and macros do not share a namespace.
