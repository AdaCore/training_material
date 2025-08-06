==============
Generic Data
==============

--------------------------------
Generic Types Parameters (1/3)
--------------------------------

* A generic parameter is a template
* It specifies the properties the generic body can rely on

   .. code:: Ada

      generic
         type T1 is private;
         type T2 (<>) is private;
         type T3 is limited private;
      package Parent is

* The actual parameter must be no more restrictive then the :dfn:`generic contract`

---------------------------------------
Generic Types Parameters (2/3)
---------------------------------------

* Generic formal parameter tells generic what it is allowed to do with the type

.. container:: latex_environment tiny

  .. list-table::

    * - :ada:`type T1 is (<>);`

      - Discrete type; :ada:`'First`, :ada:`'Succ`, etc available

    * - :ada:`type T2 is range <>;`

      - Signed Integer type; appropriate mathematic operations allowed

    * - :ada:`type T3 is digits <>;`

      - Floating point type; appropriate mathematic operations allowed

    * - :ada:`type T4;`

      - Incomplete type; can only be used as target of :ada:`access`

    * - :ada:`type T5 is tagged private;`

      - :ada:`tagged` type; can extend the type

    * - :ada:`type T6 is private;`

      - No knowledge about the type other than assignment, comparison, object creation allowed

    * - :ada:`type T7 (<>) is private;`

      - :ada:`(<>)` indicates type can be unconstrained, so any object has to be initialized

--------------------------------
Generic Types Parameters (3/3)
--------------------------------

* The usage in the generic has to follow the contract

  * Generic Subprogram

    .. code:: Ada

       generic
          type T (<>) is private;
       procedure P (V : T);
       procedure P (V : T) is
          X1 : T := V; -- OK, can constrain by initialization
          X2 : T;      -- Compilation error, no constraint to this
       begin

  * Instantiations

    .. code:: Ada

       type Limited_T is limited null record;

       -- unconstrained types are accepted
       procedure P1 is new P (String);

       -- type is already constrained
       -- (but generic will still always initialize objects)
       procedure P2 is new P (Integer);

       -- Illegal: the type can't be limited because the generic
       -- thinks it can make copies
       procedure P3 is new P (Limited_T);

------------------------------------
Generic Parameters Can Be Combined
------------------------------------

* Consistency is checked at compile-time

.. code:: Ada

   generic
      type T (<>) is private;
      type Acc is access all T;
      type Index is (<>);
      type Arr is array (Index range <>) of Acc;
   function Component (Source   : Arr;
                       Position : Index)
                       return T;

   type String_Ptr is access all String;
   type String_Array is array (Integer range <>)
       of String_Ptr;

   function String_Component is new Component
      (T     => String,
       Acc   => String_Ptr,
       Index => Integer,
       Arr   => String_Array);

------
Quiz
------

.. code:: Ada

   generic
      type T1 is (<>);
      type T2 (<>) is private;
   procedure G
     (A : T1;
      B : T2);

Which is (are) legal instantiation(s)?

   A. ``procedure A is new G (String, Character);``
   B. :answermono:`procedure B is new G (Character, Integer);`
   C. :answermono:`procedure C is new G (Integer, Boolean);`
   D. :answermono:`procedure D is new G (Boolean, String);`

.. container:: animate

   :ada:`T1` must be discrete - so an integer or an enumeration. :ada:`T2` can be any type

