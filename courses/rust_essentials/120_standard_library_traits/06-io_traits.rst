============
I/O Traits
============

-------------------------------
Reading and Writing via Trait
-------------------------------

* :rust:`std::io::Read`

  * Implemented by types from which bytes can be sourced

    * Such as :rust:`File` or :rust:`&[u8]`

  * Key methods: :rust:`read()`, :rust:`read_to_string()`

* :rust:`std::io::Write`

  * Implemented by types to which bytes can be sent

    * Such as :rust:`File`, :rust:`TcpStream`, or :rust:`Vec<u8>`

  * Key methods: :rust:`write()`, :rust:`flush()`

* Why It Matters

  * Central to Rust’s I/O ecosystem (files, network, buffers)
  * Trait-based

    * Many types can be read from / written to
    * Regardless of what they're made of

-----------------------------
"Write" then "Read" Example
-----------------------------

.. code:: rust

  use std::fs::File;
  use std::io::{Read, Write};

  // ----- Write -----
  let mut file = File::create("example.txt")?;
  file.write_all(b"Hello, Rust!\n")?;

  // ----- Read -----
  let mut file = File::open("example.txt")?;
  let mut contents = String::new();
  file.read_to_string(&mut contents)?;

.. note::

  No error handling needed - the try operator (:rust:`?`) will
  return an error immediately, otherwise things progress normally.
  (More on :rust:`?` later)

--------------------
Practical Patterns
--------------------

* Combine with :rust:`BufRead` for buffered input
* Writers often implement :rust:`flush()` to ensure output is sent
* You can implement these on custom types (e.g., in-memory buffers)

