====================
Reference Validity
====================

------------------
Reference Safety
------------------

- References are always safe to use
  - Can **never** be :rust:`null`
  - Null checking is not necessary
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
