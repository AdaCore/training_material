============
I/O Traits
============

-------------------------------
Reading and Writing via Trait
-------------------------------

* :rust:`std::io::Read`

  * Implemented by types from which bytes can be sourced

    * E.g., :rust:`File` or :rust:`&[u8]`

  * **Key methods:** :rust:`read()`, :rust:`read_to_string()`

* :rust:`std::io::Write`

  * Implemented by types to which bytes can be sent

    * Such as :rust:`File`, :rust:`TcpStream`, or :rust:`Vec<u8>`

  * **Key methods:** :rust:`write()`, :rust:`flush()`

* Central to I/O ecosystem

  * Many types can be read from / written to
  * Allows function to accept any readable/writable source

    * I.e., files, network, memory, etc.

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

  More on *try operator* (:rust:`?`) later

--------------------
Practical Patterns
--------------------

* Writers often implement :rust:`flush()` to ensure output is sent

.. code:: rust
  :font-size: scriptsize

  use std::fs::File;
  use std::io::Write;

  fn main() -> std::io::Result<()> {
      let mut file = File::create("log.txt")?;

      // The OS receives this data, but might hold it in a 'page cache'
      file.write_all(b"Critical system event occurred.")?;

      // Ensures the OS moves data from its internal cache to the storage device
      file.flush()?; 

      println!("Data is physically committed to disk.");
      Ok(())
  }
