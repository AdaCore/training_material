====================
Reference Validity
====================


----------------------------------
References End at Their Last Use
----------------------------------

.. code:: rust

    let mut ego = 10;
    let ref_1 = &ego;
    println!("ref_1: {ref_1}");  // Last use of 'ref_1'
    let ref_2 = &mut ego;        // Allowed

- :rust:`ref_1` is no longer needed after :rust:`println!`
- :rust:`ref_2` creation is allowed
- :rust:`ref_1` and :rust:`ref_2` do not overlap

.. note::

   References end at their **last use**, not necessarily at the end of the scope :rust:`{ }`

-----------------------------------
References are always safe to use
-----------------------------------

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
