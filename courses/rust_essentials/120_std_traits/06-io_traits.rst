============
I/O Traits
============

-------------------------------
Reading and Writing via Trait
-------------------------------

* I/O Traits

  * Standard traits for I/O are :rust:`Read` and :rust:`Write` in :rust:`std::io`
  * Define methods like read(&mut self, buf: &mut [u8]) and write(&mut self, buf: &[u8])

* Why It Matters

  * Central to Rust’s I/O ecosystem (files, network, buffers)
  * Trait-based so many types can be read from / written to generically

--------------------
Practical Patterns
--------------------

* Combine with :rust:`BufRead` for buffered input
* Writers often implement :rust:`flush()` to ensure output is sent
* You can implement these on custom types (e.g., in-memory buffers)

---------
Example
---------

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
