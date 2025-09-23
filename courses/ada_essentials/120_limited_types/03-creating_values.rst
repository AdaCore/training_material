=================
Creating Values
=================

-----------------
Creating Values
-----------------

* Initialization is not assignment (but looks like it)!
* Via **limited constructor functions**

   - Functions returning values of limited types

* Via an **aggregate**

    - :dfn:`limited aggregate` when used for a :ada:`limited` type

.. code:: Ada

   type Spin_Lock is limited record
     Flag : Interfaces.Unsigned_8;
   end record;
   ...
   Mutex : Spin_Lock := (Flag => 0); -- limited aggregate

-------------------------------
Limited Constructor Functions
-------------------------------

.. container:: columns

 .. container:: column

    * Allowed wherever limited aggregates are allowed
    * More capable (can perform arbitrary computations)
    * Necessary when limited type is also private

       - Users won't have visibility required to express aggregate contents

 .. container:: column

   .. code:: Ada

      function F return Spin_Lock
      is
      begin
        ...
        return (Flag => 0);
      end F;

---------------------------------------
Writing Limited Constructor Functions
---------------------------------------

* Remember - copying is not allowed

.. code:: Ada

   function F return Spin_Lock is
     Local_X : Spin_Lock;
   begin
     ...
     return Local_X; -- this is a copy - not legal
      -- (also illegal because of pass-by-reference)
   end F;

.. code:: Ada

   Global_X : Spin_Lock;
   function F return Spin_Lock is
   begin
     ...
     -- This is not legal starting with Ada2005
     return Global_X; -- this is a copy
   end F;

-------------------
 "Built In-Place"
-------------------

* Limited aggregates and functions, specifically
* No copying done by implementation

   - Values are constructed in situ

.. code:: Ada

   Mutex : Spin_Lock := (Flag => 0);

.. code:: Ada

   function F return Spin_Lock is
   begin
     return (Flag => 0);
   end F;

------
Quiz
------

.. code:: Ada

   type Limited_T is limited record
      Field : Integer;
   end record;

Which piece(s) of code is (are) a legal constructor for :ada:`Limited_T`?

.. container:: latex_environment tiny

 .. container:: columns

  .. container:: column

    A. | :answermono:`function Create return Limited_T is`
       | :answermono:`begin`
       |    :answermono:`return Limited_T'(Field => 0);`
       | :answermono:`end Create;`
    B. | :answermono:`function Create return Limited_T is`
       |    :answermono:`Val : Integer := 0;`
       | :answermono:`begin`
       |    :answermono:`return (Field => Val);`
       | :answermono:`end Create;`

  .. container:: column

    C. | ``function Create return Limited_T is``
       |    ``Ret : Limited_T := (Field => 0);``
       | ``begin``
       |    ``return Ret;``
       | ``end Create;``
    D. | ``function Create return Limited_T is``
       | ``begin``
       |    ``return (0);``
       | ``end Create;``

.. container:: animate

   A. Create an object using a qualifier
   B. Create an object inline
   C. Cannot copy an object
   D. Single component record needs named notation

------
Quiz
------

.. code:: Ada

   package Example is
      type Limited_T is limited record
         Field : Integer;
         Flag  : Character;
      end record;
      Zero : Limited_T := (0, ' ');
      One : constant Limited_T := (1, 'a');
      Two : Limited_T;
      function Create return Limited_T;
   end Example;

Which is a correct completion of Create?

A. :answermono:`return (3, 'c');`
B. | ``Two := (2, 'b');``
   | ``return Two;``
C. ``return One;``
D. ``return Zero;``

.. container:: animate

   :ada:`A` contains an "in-place" return. The rest all rely on
   other objects, which would require an (illegal) copy.

