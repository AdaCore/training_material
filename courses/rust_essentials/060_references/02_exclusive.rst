======================
Exclusive References
======================

--------------------
Mutable References
--------------------

- Allow changing the value they refer to
  - Also known as exclusive references
- No other references (shared or exclusive) can exist simultaneously
- You cannot create a :rust:`&mut` reference to an immutable variable

.. code:: rust

    let mut two_plus_two = 4;
    let big_brother = &mut two_plus_two;
    *big_brother = 5; 
    println!("Truth: {two_plus_two}");

* Generates the following output

:command:`Truth: 5`

.. note::

  A mutable reference to a type :rust:`T` has type :rust:`&mut T`

----------------------
Binding vs Reference
----------------------

The four quadrants of Rust's reference system

.. container:: latex_environment scriptsize

.. list-table::
  :header-rows: 1

  * - Syntax
    - Binding
    - Reference
    - Capabilities

  * - :rust:`let r = &x`
    - Immutable
    - Shared
    - 

  * - :rust:`let mut r = &x`
    - **Mutable**
    - Shared
    - Rebind

  * - :rust:`let r = &mut x`
    - Immutable
    - **Mutable**
    - Modify

  * - :rust:`let mut r = &mut x`
    - **Mutable**
    - **Mutable**
    - Rebind + Modify

