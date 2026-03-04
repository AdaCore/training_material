============
I/O Traits
============

-------------------------------
Reading and Writing via Trait
-------------------------------

* I/O Traits

  * :rust:`std::io::Read`

    * For getting data **from** source
    * Key method: :rust:`read(&mut self, buf: &mut [u8]) -> Result<usize>`

  * :rust:`std::io::Write`

    * For pushing data **to** a destination
    * Key methods: :rust:`write(&mut self, buf: &[u8])` and :rust:`flush(&self)`

* Why It Matters

  * Central to Rust’s I/O ecosystem (files, network, buffers)
  * Trait-based

    * Many types can be read from / written to generically

-----------------------------
"Write" then "Read" Example
-----------------------------

.. code:: rust

  use std::fs::File;
  use std::io::{Read, Write};

  fn main() -> std::io::Result<()> {
      // ----- Write -----
      let mut file = File::create("example.txt")?;
      file.write_all(b"Hello, Rust!\n")?;

      // ----- Read -----
      let mut file = File::open("example.txt")?;
      let mut contents = String::new();
      file.read_to_string(&mut contents)?;

      assert_eq!(contents, "Hello, Rust!\n");
      Ok(())
  }

.. note::

  Reminder: no error handling needed - the :rust:`?` will return
  an error immediately, otherwise things progress normally

--------------------
Practical Patterns
--------------------

* Combine with :rust:`BufRead` for buffered input
* Writers often implement :rust:`flush()` to ensure output is sent
* You can implement these on custom types (e.g., in-memory buffers)

