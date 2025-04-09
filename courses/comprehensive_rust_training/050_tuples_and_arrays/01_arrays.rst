========
Arrays
========

--------
Arrays
--------

.. code:: rust
   :number-lines: 2

   fn main() {
       let mut a: [i8; 10] = [42; 10];
       a[5] = 0;
       println!("a: {a:?}");
   }

-------------------
Array Description
-------------------

- Value of array type :rust:`[T; N]` holds :rust:`N` elements of type :rust:`T`.

  - :rust:`N` is a compile-time constant

- Length of array is **part of its type**

  - :rust:`[u8; 3]` and :rust:`[u8; 4]` are two different types
  - Slices, whose size is determined at runtime, are covered later.

- Out of bounds elements are compile errors (if possible) otherwise *panic*

  .. code:: rust
    :number-lines: 4

    a[99] = 0;

  ::

    index out of bounds: the length is 10 but the index is 99

  .. code:: rust
    :number-lines: 4

    a[b] = 0;

  ::

    thread 'main' panicked at src/main.rs:4:4:

-------------------
Printing an Array
-------------------
    
- :rust:`println!` macro asks for the debug implementation via :rust:`?` format parameter

  - :rust:`{}` gives the default output
  - :rust:`{:?}` gives the debug output.
  - Types such as integers and strings implement the default output, but arrays only implement tdebug output.
    means that we must use debug output here.

- Adding :rust:`#`, eg :rust:`{a:#?}`, invokes a "pretty printing" format, which can be easier to read.
