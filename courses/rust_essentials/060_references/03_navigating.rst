======================
Binding vs Reference
======================

-------------------------
Rust's Reference System
-------------------------

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


- **Binding**: labeled slot that holds the address
- **Reference**: access contract for the data at that address

-----------------------------
The "Observer" (let r = &x)
-----------------------------

- *Cannot* point to something else
- *Cannot* change the value

.. code:: rust

    let past = 1984;
    let future = 2048;
    let rf = &past; 

    *rf = 1776; // Error

:error:`error[E0594]: cannot assign to '*rf', which is behind a '&'' reference`

.. code:: rust

    rf = &future; // Error

:error:`error[E0384]: cannot assign twice to immutable variable 'rf'`

.. note::

  The reference cannot be redirected, nor can the data be modified

---------------------------------
The "Rebinder" (let mut r = &x)
---------------------------------

- *Can* point to something else
- *Cannot* change value

.. code:: rust

    let news_a = "War with the North";
    let news_b = "War with the South";
    let mut rf = &news_a;

    rf = &news_b;
    *rf = "Peace"; // Error

:error:`error[E0594]: cannot assign to '*rf', which is behind a '&'' reference`

.. note::

  The reference can be redirected, but the data cannot be modified

---------------------------------
The "Modifier" (let r = &mut x) 
---------------------------------

- *Cannot* point to something else
- *Can* change value

.. code:: rust

    let mut room_101 = 0;
    let mut room_102 = 0;
    let rf = &mut room_101;

    *rf = 1;
    rf = &mut room_102; // Error

:error:`error[E0384]: cannot assign twice to immutable variable 'rf'`

.. note::

  The reference cannot be redirected, but the data can be modified

---------------------------------------
The "Free Agent" (let mut r = &mut x) 
---------------------------------------

- *Can* point to something else
- *Can* change value

.. code:: rust

    let mut focus = 100;
    let mut shame = 0;
    let mut rf = &mut focus;

    *rf = 0;
    rf = &shame;
    *rf = 999;

.. note::

  The reference can be redirected, and the data can be modified
