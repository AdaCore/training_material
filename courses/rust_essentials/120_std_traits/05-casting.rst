=========
Casting
=========

-----------------------------------------
Conversion Between Numbers and Pointers
-----------------------------------------

* Casting traits help interpet/transform values

  * Convert between numeric and pointer types

* Use cases

  * Conversions that aren’t just value-to-value (e.g., pointer casts)
  * Support in low-level code and FFI scenarios

.. note::

  Rust has no *implicit* conversion - explicit casts use :rust:`as`

------------------
Casting Examples
------------------

.. code:: rust

  fn main() {
      let value: i64 = 1000;
      println!("as u16: {}", value as u16);
      println!("as i16: {}", value as i16);
      println!("as u8: {}", value as u8);
  }

**produces**

.. code:: output

  as u16: 1000
  as i16: 1000
  as u8: 232

-------------------------
Be Careful with Casting
-------------------------

* Casting may truncate in ways you don't expect

  * But it is consistent
  * Check documentation for clarity

* :rust:`From` and :rust:`Into` are generally safer

  * Also use :rust:`TryFrom` and :rust:`TryInto` when you're not sure about the result
