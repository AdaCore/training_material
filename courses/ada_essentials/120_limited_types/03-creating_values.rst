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

.. include:: ../quiz/limited_constructor_syntax/quiz.rst

------
Quiz
------

.. code:: Ada

   package P is
      type T is limited record
         F1 : Integer;
         F2 : Character;
      end record;
      Zero : T := (0, ' ');
      One : constant T := (1, 'a');
      Two : T;
      function F return T;
   end P;

Which is a correct completion of F?

A. :answermono:`return (3, 'c');`
B. | ``Two := (2, 'b');``
   | ``return Two;``
C. ``return One;``
D. ``return Zero;``

.. container:: animate

   :ada:`A` contains an "in-place" return. The rest all rely on
   other objects, which would require an (illegal) copy.

