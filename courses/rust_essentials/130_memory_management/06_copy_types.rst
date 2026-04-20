==================
"Copy" Semantics
==================

--------------
"Copy" Types
--------------

- Usually live on the stack
- Utilize automatic *bitwise* duplication
- Cost of copying is negligible
- Implement :rust:`Copy` trait
  - Scalar types are :rust:`Copy`

.. note:: 

   Saying a type is :rust:`Copy` means it implements the :rust:`Copy` trait

.. code:: rust

   let gizmo = 1984;
   let gremlin = gizmo;

   println!("gizmo: {gizmo}"); // Valid: 'gizmo' was not moved
   println!("gremlin: {gremlin}");

---------------------
Custom "Copy" Types
---------------------

- Programmer-defined types can opt-in to :rust:`Copy`
  - Via :rust:`#[derive]` macro or manually with :rust:`impl`

.. code:: rust

   #[derive(Copy, Clone)]
   struct Point(i32, i32);

   let p1 = Point(3, 4);
   let p2 = p1;
   
- :rust:`p1` and :rust:`p2` hold independent copies of data
- :rust:`let p2 = p1.clone();` produces the same result
  - But :rust:`Copy` makes it implicit

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

------------------------------------
"Copy" Types and Field Constraints
------------------------------------

- Programmer-defined types can only be :rust:`Copy` if all fields are also :rust:`Copy`
- Types that own heap memory cannot be :rust:`Copy`
  - Prevents memory issues

.. code:: rust

   #[derive(Copy, Clone)] 
   struct User(i32, String); 

   let user_a = User(42, String::from("Alice"));    
   let user_b = user_a;

.. container:: latex_environment footnotesize

   :error:`error[E0204]: the trait 'Copy' cannot be implemented for this type`

-----------------------
"Copy" vs. Non-"Copy"
-----------------------

.. list-table::
  :header-rows: 1
  :stub-columns: 1

  * - **Property**
    - :rust:`Copy` **types**
    - **Non-**:rust:`Copy` **types**

  * - *Assignment logic*
    - Still usable
    - **Invalid** (compile error if used)
   
  * - *Trait required*
    - :rust:`impl Copy` 
    - None (default behavior)

  * - *Examples*
    - :rust:`i32`, :rust:`bool`, :rust:`[f64, 4]`
    - :rust:`String`, :rust:`Vec<T>`

