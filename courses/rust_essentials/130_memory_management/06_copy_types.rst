================
Copy Semantics
================

------------
Copy Types
------------

- Utilize automatic *bitwise* duplication
- Like integers, floats, and booleans
- Exist entirely on the stack
  - Cost of copying is negligible
  - Implement :rust:`Copy` trait

.. code:: rust

   let x = 42;
   let y = x;

   println!("x: {x}"); // Valid: 'x' was not moved
   println!("y: {y}");

-------------------
Custom Copy Types
-------------------

- User-defined types can opt-in for this behavior
  - Provided they consist strictly of other :rust:`Copy` types

.. code:: rust

   #[derive(Copy, Clone)]
   struct Point(i32, i32);

   let p1 = Point(3, 4);
   let p2 = p1;
   
- Both :rust:`p1` and :rust:`p2` own their own data
- Using :rust:`let p2 = p1.clone();` would do the same explicitly

---------------------------
No "Copy" Without "Clone"
---------------------------

- :rust:`Copy` is a *subtrait* of :rust:`Clone`
  - Defined as :rust:`trait Copy: Clone`
- Using :rust:`#derive[Copy]` alone triggers a compile error

.. code:: rust

   #[derive(Copy)]
   struct Point(i32, i32);

   let p1 = Point(3, 4);
   let p2 = p1;

.. container:: latex_environment footnotesize

   :error:`error[E0277]: the trait bound 'Point: Clone' is not satisfied`

---------------------------------
"Copy" requires All-Copy fields
---------------------------------

- User-defined types can only be :rust:`Copy` if all fields are also :rust:`Copy`

.. code:: rust

   #[derive(Copy, Clone)] 
   struct User(i32, String); 

   let user_a = User(42, String::from("Alice"));    
   let user_b = user_a;

.. container:: latex_environment footnotesize

   :error:`error[E0204]: the trait 'Copy' cannot be implemented for this type`

.. note::

   Types that own heap memory cannot be :rust:`Copy` to prevent memory issues
