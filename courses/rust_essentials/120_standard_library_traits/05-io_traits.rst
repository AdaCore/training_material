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

* Central to Rust’s I/O ecosystem (files, network, buffers)

* Trait-based

  * Many types can be read from / written to
  * Allows function to accept any readable/writable source

    * E.g., files, network, memory, etc.

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

* Combine with :rust:`BufReader` for buffered input

  * Struct that implements trait :rust:`BufRead`
  * Allows reading line-by-line (rather than as a byte array)

* Writers often implement :rust:`flush()` to ensure output is sent

.. code:: rust
  :font-size: scriptsize

  use std::io::{BufRead, BufReader, Write}; // Traits (Abilities)
  use std::fs::File;

  fn main() -> std::io::Result<()> {
      // 1. SETUP: Open the file and wrap it in a buffer
      let file = File::open("input.txt")?;
      let mut reader = BufReader::new(file); 
      let mut output = Vec::new(); // Vec implements Write

      // 2. READ: Use BufRead trait to grab a line easily
      let mut line = String::new();
      reader.read_line(&mut line)?; 

      // 3. WRITE: Use Write trait to save the data
      output.write_all(line.as_bytes())?;
      output.flush()?; // Ensure all bytes are pushed out

      Ok(())
  }
