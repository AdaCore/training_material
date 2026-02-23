====================
Reference Validity
====================

------------------
Reference Safety
------------------

- References are always safe to use
  - Can **never** be null
  - Cannot outlive the data they point to
  - Dangling references cannot occur

.. code:: rust

   let rose = {
      let jack = String::from("Jack");
      &jack
   };
   println!("Jack screams: {rose}");

* Compiler output

:command:`error[E0597]: 'jack' does not live long enough`

.. note::

    In *Unsafe Rust*, raw pointers can be null or dangling, leaving memory integrity entirely in your hands (more on this later)
