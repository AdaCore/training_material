=========================
Structuring Expressions
=========================

---------------------
Declare Expressions
---------------------

.. admonition:: Language Variant

   Ada 2022

* Convenient shorthand for **repeated** subexpression

  - Only constants and renamings allowed
  - Typically used in **postconditions**

  .. code:: Ada

     function Find (T : Table; R : Integer) return Integer
       with Post =>
         (declare
            Res : constant Integer := Find'Result;
          begin
            Res >= 0 and then
            (if Res /= 0 then T (Res) = R));

----------------------
Expression Functions
----------------------

* Convenient shorthand for **repeated** subexpression

  - Somewhat similar goal as declare expressions
  - But visible in a **larger** scope

* Simple query functions used in contracts

  .. code:: Ada

     function Is_Sorted (T : Table) return Boolean is
       (for all J in T'Range =>
          (for all K in T'Range => (if J < K then T(J) <= T(K))));

* Above is equivalent to having a **postcondition**

  - But no subprogram body to add in the body unit

  .. code:: Ada

     function Is_Sorted (T : Table) return Boolean
       with Post => Is_Sorted'Result = (for all J in T'Range => ...);

* Pre and postconditions can be specified **after** the expression

  .. code:: Ada

     function Is_Sorted (T : Table) return Boolean is (...)
       with Pre => T'Length > 0;

-----------------------------
Use of Expression Functions
-----------------------------

* Expression functions can be declared in a package spec and used in **contracts**

  - It can even be declared **after** its use in contracts!

* For queries over objects of a :ada:`private` type

  - Function **spec** is declared in the **public** part
  - **Expression function** is declared in the **private** part

  .. code:: Ada

     package P is
       type T is private;
       function Value (X : T) return Integer;
     private
       type T is new Integer;
       function Value (X : T) return Integer is (Integer (X));
     end;

  - :toolname:`GNATprove` uses the **implicit postcondition** to prove client units

