============
"anyhow"
============

------------
"anyhow"
------------

The `anyhow <https://docs.rs/anyhow/>`__ crate provides a rich error
type with support for carrying additional contextual information, which
can be used to provide a semantic trace of what the program was doing
leading up to the error.

This can be combined with the convenience macros from
`thiserror <https://docs.rs/thiserror/>`__ to avoid writing out
trait impls explicitly for custom error types.

.. code:: rust

   use anyhow::{bail, Context, Result};
   use std::fs;
   use std::io::Read;
   use thiserror::Error;

   #[derive(Clone, Debug, Eq, Error, PartialEq)]
   #[error("Found no username in {0}")]
   struct EmptyUsernameError(String);

   fn read_username(path: &str) -> Result<String> {
       let mut username = String::with_capacity(100);
       fs::File::open(path)
           .with_context(|| format!("Failed to open {path}"))?
           .read_to_string(&mut username)
           .context("Failed to read")?;
       if username.is_empty() {
           bail!(EmptyUsernameError(path.to_string()));
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

-  :rust:`anyhow::Error` is essentially a wrapper around :rust:`Box<dyn Error>`.
   As such it's again generally not a good choice for the public API of
   a library, but is widely used in applications.
-  :rust:`anyhow::Result<V>` is a type alias for
   :rust:`Result<V, anyhow::Error>`.
-  Functionality provided by :rust:`anyhow::Error` may be familiar to Go
   developers, as it provides similar behavior to the Go :rust:`error` type
   and :rust:`Result<T, anyhow::Error>` is much like a Go :rust:`(T, error)`
   (with the convention that only one element of the pair is
   meaningful).
-  :rust:`anyhow::Context` is a trait implemented for the standard
   :rust:`Result` and :rust:`Option` types. :rust:`use anyhow::Context` is necessary
   to enable :rust:`.context()` and :rust:`.with_context()` on those types.

-----------------
More to Explore
-----------------

-  :rust:`anyhow::Error` has support for downcasting, much like
   :rust:`std::any::Any`; the specific error type stored inside can be
   extracted for examination if desired with
   `Error::downcast <https://docs.rs/anyhow/latest/anyhow/struct.Error.html#method.downcast>`__.
