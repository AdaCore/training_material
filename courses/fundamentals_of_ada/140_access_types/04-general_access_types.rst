==========================
General Access Types
==========================

----------------------
General Access Types
----------------------

* Can point to any pool (including stack)

   .. code:: Ada

      type T is [...]
      type T_Access is access all T;
      V : T_Access := new T;

* Still distinct type
* Conversions are possible

   .. code:: Ada

      type T_Access_2 is access all T;
      V2 : T_Access_2 := T_Access_2 (V); -- legal

-----------------------
Referencing the Stack
-----------------------

* By default, stack-allocated objects cannot be referenced - and can even be optimized into a register by the compiler
* :ada:`aliased` declares an object to be referenceable through an access value

   .. code:: Ada

      V : aliased Integer;

* :ada:`'Access` attribute gives a reference to the object

   .. code:: Ada

      A : Int_Access := V'Access;

   - :ada:`'Unchecked_Access` does it **without checks**

----------------------------
`Aliased` Objects Examples
----------------------------

.. code:: Ada

   type Acc is access all Integer;
   V, G : Acc;
   I : aliased Integer;
   ...
   V := I'Access;
   V.all := 5; -- Same a I := 5
   ...
   procedure P1 is
      I : aliased Integer;
   begin
      G := I'Unchecked_Access;
      P2;
      --  Necessary to avoid corruption
      --  Watch out for any of G's copies!
      G := null;
   end P1;

   procedure P2 is
   begin
      G.all := 5;
   end P2;

----------------------
`Aliased` Parameters
----------------------

* To ensure a subprogram parameter always has a valid memory address, define it as :ada:`aliased`

   * Ensures :ada:`'Access` and :ada:`'Address` are valid for the parameter

.. code:: Ada

   procedure Example (Param : aliased Integer);

   Object1 : aliased Integer;
   Object2 : Integer;

.. code:: Ada

   -- This is OK
   Example (Object1);

   -- Compile error: Object2 could be optimized away
   -- or stored in a register
   Example (Object2);

   -- Compile error: No address available for parameter
   Example (123);

------
Quiz
------

.. code:: Ada

   type General_T is access all Integer;
   type Pool_T is access Integer;

   Aliased_Object : aliased Integer;
   Random_Object  : Integer;

   General_Ptr       : General_T;
   Pool_Specific_Ptr : Pool_T;

Which assignment(s) is (are) legal?

A. ``General_Ptr := Random_Object'Access;``
B. :answermono:`General_Ptr := Aliased_Object'Access;`
C. ``Pool_Specific_Ptr := Random_Object'Access;``
D. ``Pool_Specific_Ptr := Aliased_Object'Access;``

.. container:: animate

   :ada:`'Access` is only allowed for general access types
   (:ada:`General_T`). To use :ada:`'Access` on an object, the
   object **must** be :ada:`aliased`.
