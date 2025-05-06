=================
Tagged Derivation
=================

---------------------------------
Difference with Simple Derivation
---------------------------------

* Tagged derivation **can** change the structure of a type

    - Keywords :ada:`tagged record` and :ada:`with record`

   .. code:: Ada

      type Root is tagged record
         F1 : Integer;
      end record;

      type Child is new Root with record
         F2 : Integer;
      end record;

--------------
Type Extension
--------------

* A tagged derivation **has** to be a type extension

    - Use :ada:`with null record` if there are no additional components

   .. code:: Ada

      type Child is new Root with null record;
      type Child is new Root; -- illegal

* Conversion is only allowed from **child to parent**

   .. code:: Ada

      V1 : Root;
      V2 : Child;
      ...
      V1 := Root (V2);
      V2 := Child (V1); -- illegal

*Information on extending private types appears at the end of this module*

------------
Primitives
------------

* Child **cannot remove** a primitive
* Child **can add** new primitives
* :dfn:`Controlling parameter`

    - Parameters the subprogram is a primitive of
    - For :ada:`tagged` types, all should have the **same type**

   .. code:: Ada

      type Root1 is tagged null record;
      type Root2 is tagged null record;

      procedure P1 (V1 : Root1;
                    V2 : Root1);
      procedure P2 (V1 : Root1;
                    V2 : Root2); -- illegal

-------------------------------
Freeze Point for Tagged Types
-------------------------------

* Freeze point definition does not change

   - A variable of the type is declared
   - The type is derived
   - The end of the scope is reached

* Declaring tagged type primitives past freeze point is **forbidden**

.. code:: Ada

   type Root is tagged null record;

   procedure Prim (V : Root);

   type Child is new Root with null record; -- freeze root

   procedure Prim2 (V : Root); -- illegal

   V : Child; --  freeze child

   procedure Prim3 (V : Child); -- illegal

------------------
Tagged Aggregate
------------------

* At initialization, all components (including **inherited**) must have a **value**

   .. code:: Ada

       type Root is tagged record
           F1 : Integer;
       end record;

       type Child is new Root with record
           F2 : Integer;
       end record;

       V : Child := (F1 => 0, F2 => 0);

* For **private types** use :dfn:`aggregate extension`

    - Copy of a parent instance
    - Use :ada:`with null record` absent new components

   .. code:: Ada

      V2 : Child := (Parent_Instance with F2 => 0);
      V3 : Empty_Child := (Parent_Instance with null record);

*Information on aggregates of private extensions appears at the end of this module*

---------------------
Overriding Indicators
---------------------

* Optional :ada:`overriding` and :ada:`not overriding` indicators

   .. code:: Ada

      type Shape_T is tagged record
         Name : String (1..10);
      end record;

      -- primitives of "Shape_T"
      function Get_Name (S : Shape_T) return String;
      procedure Set_Name (S : in out Shape_T);

      -- Derive "Point_T" from Shape_T
      type Point_T is new Shape_T with record
         Origin : Coord_T;
      end record;

      -- We want to _change_ the behavior of Set_Name
      overriding procedure Set_Name (P : in out Point_T);
      -- We want to _add_ a new primitive
      not overriding procedure Set_Origin (P : in out Point_T);
      -- We get "Get_Name" for free

..
  language_version 2005

-----------------
Prefix Notation
-----------------

* Tagged types primitives can be called as usual
* The call can use prefixed notation

    - **If** the first argument is a controlling parameter
    - No need for :ada:`use` or :ada:`use type` for visibility

   .. code:: Ada

      -- Prim1 visible even without *use Pkg*
      X.Prim1;

      declare
         use Pkg;
      begin
         Prim1 (X);
      end;

..
  language_version 2005

------
Quiz
------

.. include:: ../quiz/tagged_primitives/quiz.rst

------
Quiz
------

.. include:: ../quiz/tagged_dot_and_with/quiz.rst

------
Quiz
------

Which code block(s) is (are) legal?

.. container:: columns

  .. container:: column

    A. | ``type A1 is record``
       |    ``Component1 : Integer;``
       | ``end record;``
       | ``type A2 is new A1 with null record;``
    B. | :answermono:`type B1 is tagged record`
       |    :answermono:`Component2 : Integer;`
       | :answermono:`end record;`
       | :answermono:`type B2 is new B1 with record`
       |    :answermono:`Component2b : Integer;`
       | :answermono:`end record;`

  .. container:: column

    C. | ``type C1 is tagged record``
       |    ``Component3 : Integer;``
       | ``end record;``
       | ``type C2 is new C1 with record``
       |    ``Component3 : Integer;``
       | ``end record;``
    D. | ``type D1 is tagged record``
       |    ``Component1 : Integer;``
       | ``end record;``
       | ``type D2 is new D1;``

.. container:: animate

   Explanations

   A. Cannot extend a non-tagged type
   B. Correct
   C. Components must have distinct names
   D. Types derived from a tagged type must have an extension

