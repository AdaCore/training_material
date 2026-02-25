======================
Exclusive References
======================

--------------------
Exclusive References
--------------------

- Created with the :rust:`&mut` operator
- Allow changing the value they refer to
- Also known as **mutable** references
- No other references (shared or exclusive) can exist simultaneously
- An exclusive reference to a type :rust:`T` has type :rust:`&mut T`

.. code:: rust

    let mut two_plus_two = 4;
    let big_brother = &mut two_plus_two;
    *big_brother = 5; 
    println!("Truth: {two_plus_two}");

* Generates the following output

:command:`Truth: 5`

.. note::

    You cannot create a :rust:`&mut` reference to an immutable variable

------------------------------------------
How References Use Non-Lexical Lifetimes
------------------------------------------

- Case A: :rust:`ref_1` is still "active", the compiler won't let :rust:`ref_2` exist

.. code:: rust

    let mut ego = 10;
    let ref_1 = &ego;
    let ref_2 = &mut ego;

    println!("ref_1: {ref_1}");

- Generates the following output

:command:`error[E0502]: cannot borrow 'ego' as mutable because it is also borrowed as immutable`

- Case B: :rust:`ref_1` is never actually used, the compiler lets :rust:`ref_2` exist

.. code:: rust

    let mut ego = 10;
    let ref_1 = &ego;
    let ref_2 = &mut ego;

    println!("ref_2: {ref_2}");

- Generates the following output

:command:`ref_2: 10`

.. note::

    References live until their last use, not to the end of the scope :rust:`{ }`

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

  * - :rust:`let r = &x`
    - Immutable
    - Shared

  * - :rust:`let mut r = &x`
    - Mutable
    - Shared

  * - :rust:`let r = &mut x`
    - Immutable
    - Mutable

  * - :rust:`let mut r = &mut x`
    - Mutable
    - Mutable

---------------------------------
The "Observer" (let r = &x)
---------------------------------

- *Cannot* point to something else
- *Cannot* change the value

.. code:: rust

    let past = 1984;
    let future = 2048;
    let r = &past; 

    *r = 1776; // Error

* Generates the following ouput

:error:`error[E0594]: cannot assign to '*r', which is behind a '&'' reference`

.. code:: rust

    r = &future; // Error

* Generates the following output

:error:`error[E0384]: cannot assign twice to immutable variable 'r'`

.. note::

  "I am stuck with you, and I can only look"

---------------------------------
The "Rebinder" (let mut r = &x)
---------------------------------

- *Can* point to something else
- *Cannot* change the value

.. code:: rust

    let news_a = "War with the North";
    let news_b = "War with the South";
    let mut r = &news_a;

    r = &news_b;
    *r = "Peace"; // Error

* Generates the following output

:error:`error[E0594]: cannot assign to '*r', which is behind a '&'' reference`

.. note::

  "I can't touch the contents, but I can look at someone else"

---------------------------------
The "Modifier" (let r = &mut x) 
---------------------------------

- *Cannot* point to something else
- *Can* change the value

.. code:: rust

    let mut room_101 = 0;
    let mut room_102 = 0;
    let r = &mut room_101;

    *r = 1;
    r = &mut room_102; // Error

* Generates the following output

:error:`error[E0384]: cannot assign twice to immutable variable 'r'`

.. note::

  "I am stuck with you, but I can change who you are"

---------------------------------------
The "Free Agent" (let mut r = &mut x) 
---------------------------------------

- *Can* point to something else
- *Can* change the value

.. code:: rust

    let mut focus = 100;
    let mut shame = 0;
    let mut r = &mut focus;

    *r = 0;
    r = &shame;
    *r = 999;

.. note::

  "I can change the value, and I can move on to something else"
