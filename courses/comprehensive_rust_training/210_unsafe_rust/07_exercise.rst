==================
Safe FFI Wrapper
==================

------------------
Safe FFI Wrapper
------------------

Rust has great support for calling functions through a *foreign function
interface* (FFI). We will use this to build a safe wrapper for the
:rust:`libc` functions you would use from C to read the names of files in a
directory.

You will want to consult the manual pages:

-  `opendir(3) <https://man7.org/linux/man-pages/man3/opendir.3.html>`__
-  `readdir(3) <https://man7.org/linux/man-pages/man3/readdir.3.html>`__
-  `closedir(3) <https://man7.org/linux/man-pages/man3/closedir.3.html>`__

You will also want to browse the
`std::ffi <https://doc.rust-lang.org/std/ffi/>`__ module. There you
find a number of string types which you need for the exercise:

.. list-table::
   :header-rows: 1

   * - Types
     - Encoding
     - Use

   * - `str <https://doc.rust-lang.org/std/primitive.str.html>`__ and `String <https://doc.rust-lang.org/std/string/struct.String.html>`__
     - UTF-8
     - Text processing in Rust

   * - `CStr <https://doc.rust-lang.org/std/ffi/struct.CStr.html>`__ and `CString <https://doc.rust-lang.org/std/ffi/struct.CString.html>`__
     - NUL-terminated
     - Communicating with C functions

   * - `OsStr <https://doc.rust-lang.org/std/ffi/struct.OsStr.html>`__ and `OsString <https://doc. rust-lang.org/std/ffi/struct.OsString.html>`__
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
   `OsStrExt <https://doc.rust-lang.org/std/os/unix/ffi/trait.OsStrExt.html>`__
   to create it,
-  :rust:`&OsStr` to :rust:`OsString`: you need to clone the data in :rust:`&OsStr`
   to be able to return it and call :rust:`readdir` again.

The `Nomicon <https://doc.rust-lang.org/nomicon/ffi.html>`__ also has a
very useful chapter about FFI.

Copy the code below to https://play.rust-lang.org/ and fill in the
missing functions and methods:

::

   // TODO: remove this when you're done with your implementation.
   #![allow(unused_imports, unused_variables, dead_code)]

   #include exercise.rs:ffi}}

   #include exercise.rs:DirectoryIterator}}
           unimplemented!()

   #include exercise.rs:Iterator
           unimplemented!()
       }
   }

   #include exercise.rs:Drop
           unimplemented!()
       }
   }

   #include exercise.rs:main
