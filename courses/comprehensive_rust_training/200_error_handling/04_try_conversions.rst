=================
Try Conversions
=================

-----------------
Try Conversions
-----------------

The effective expansion of :rust:`?` is a little more complicated than
previously indicated:

.. code:: rust

   expression?

works the same as

.. code:: rust

   match expression {
       Ok(value) => value,
       Err(err)  => return Err(From::from(err)),
   }

The :rust:`From::from` call here means we attempt to convert the error type
to the type returned by the function. This makes it easy to encapsulate
errors into higher-level errors.

---------
Example
---------

.. code:: rust

   use std::error::Error;
   use std::io::Read;
   use std::{fmt, fs, io};

   #[derive(Debug)]
   enum ReadUsernameError {
       IoError(io::Error),
       EmptyUsername(String),
   }

   impl Error for ReadUsernameError {}

   impl fmt::Display for ReadUsernameError {
       fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
           match self {
               Self::IoError(e) => write!(f, "I/O error: {e}"),
               Self::EmptyUsername(path) => write!(f, "Found no username in {path}"),
           }
       }
   }

   impl From<io::Error> for ReadUsernameError {
       fn from(err: io::Error) -> Self {
           Self::IoError(err)
       }
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
       //std::fs::write("config.dat", "").unwrap();
       let username = read_username("config.dat");
       println!("username or error: {username:?}");
   }

---------
Details
---------

The :rust:`?` operator must return a value compatible with the return type
of the function. For :rust:`Result`, it means that the error types have to
be compatible. A function that returns :rust:`Result<T, ErrorOuter>` can
only use :rust:`?` on a value of type :rust:`Result<U, ErrorInner>` if
:rust:`ErrorOuter` and :rust:`ErrorInner` are the same type or if :rust:`ErrorOuter`
implements :rust:`From<ErrorInner>`.

A common alternative to a :rust:`From` implementation is
:rust:`Result::map_err`, especially when the conversion only happens in one
place.

There is no compatibility requirement for :rust:`Option`. A function
returning :rust:`Option<T>` can use the :rust:`?` operator on :rust:`Option<U>` for
arbitrary :rust:`T` and :rust:`U` types.

A function that returns :rust:`Result` cannot use :rust:`?` on :rust:`Option` and
vice versa. However, :rust:`Option::ok_or` converts :rust:`Option` to :rust:`Result`
whereas :rust:`Result::ok` turns :rust:`Result` into :rust:`Option`.
