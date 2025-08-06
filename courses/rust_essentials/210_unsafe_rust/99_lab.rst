============================
Exercise: Safe FFI Wrapper
============================

-----------------------------
Safe FFI Wrapper Background
-----------------------------

Rust has great support for calling functions through a *foreign function
interface* (FFI). We will use this to build a safe wrapper for the
:rust:`libc` functions you would use from C to read the names of files in a
directory.

You will want to consult the manual pages:

-  :url:`opendir(3) <https://man7.org/linux/man-pages/man3/opendir.3.html>`
-  :url:`readdir(3) <https://man7.org/linux/man-pages/man3/readdir.3.html>`
-  :url:`closedir(3) <https://man7.org/linux/man-pages/man3/closedir.3.html>`

You will also want to browse the
:url:`std::ffi <https://doc.rust-lang.org/std/ffi/>` module. There you
find a number of string types which you need for the exercise:

.. list-table::
   :header-rows: 1

   * - Types
     - Encoding
     - Use

   * - :url:`str <https://doc.rust-lang.org/std/primitive.str.html>` and :url:`String <https://doc.rust-lang.org/std/string/struct.String.html>`
     - UTF-8
     - Text processing in Rust

   * - :url:`CStr <https://doc.rust-lang.org/std/ffi/struct.CStr.html>` and :url:`CString <https://doc.rust-lang.org/std/ffi/struct.CString.html>`
     - NUL-terminated
     - Communicating with C functions

   * - :url:`OsStr <https://doc.rust-lang.org/std/ffi/struct.OsStr.html>` and :url:`OsString <https://doc. rust-lang.org/std/ffi/struct.OsString.html>`
     - OS-specific
     - Communicating with the OS

You will convert between all these types:

-  :rust:`&str` to :rust:`CString`: you need to allocate space for a trailing
   :rust:`\0` character,
-  :rust:`CString` to :rust:`*const i8`: you need a pointer to call C functions,
-  :rust:`*const i8` to :rust:`&CStr`: you need something which can find the
   trailing :rust:`\0` character,
-  :rust:`&CStr` to :rust:`&[u8]`: a slice of bytes is the universal interface
   for "some unknown data",
-  :rust:`&[u8]` to :rust:`&OsStr`: :rust:`&OsStr` is a step towards :rust:`OsString`,
   use
   :url:`OsStrExt <https://doc.rust-lang.org/std/os/unix/ffi/trait.OsStrExt.html>`
   to create it,
-  :rust:`&OsStr` to :rust:`OsString`: you need to clone the data in :rust:`&OsStr`
   to be able to return it and call :rust:`readdir` again.

The :url:`Nomicon <https://doc.rust-lang.org/nomicon/ffi.html>` also has a
very useful chapter about FFI.

-----------------------------
Safe FFI Wrapper Setup
-----------------------------

Copy the code on the next two pages to https://play.rust-lang.org/.

.. container:: source_include 210_unsafe_rust/src/210_unsafe_rust.rs :start-after://ANCHOR-ffi :end-before://ANCHOR-solution :code:rust

-----------------------------
Safe FFI Wrapper Main
-----------------------------

.. container:: source_include 210_unsafe_rust/src/210_unsafe_rust.rs :start-after://ANCHOR-main :code:rust

-----------------------------
Safe FFI Wrapper Problem
-----------------------------

Fill in the missing functions and methods in place of **TBD** in the code

.. code:: rust

   impl DirectoryIterator {
       fn new(path: &str) -> Result<DirectoryIterator, String> {
           // Call opendir and return a Ok value if that worked,
           // otherwise return Err with a message.
           todo!()
       }
   }

   impl Iterator for DirectoryIterator {
       type Item = OsString;
       fn next(&mut self) -> Option<OsString> {
           // Keep calling readdir until we get a NULL pointer back.
           todo!()
       }
   }

   impl Drop for DirectoryIterator {
       fn drop(&mut self) {
           // Call closedir as needed.
           todo!()
       }
   }

-----------------------------
Safe FFI Wrapper Solution
-----------------------------

.. container:: source_include 210_unsafe_rust/src/210_unsafe_rust.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust
