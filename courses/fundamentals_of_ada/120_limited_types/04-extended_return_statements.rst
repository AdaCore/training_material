============================
Extended Return Statements
============================

-------------------------------------
Function Extended Return Statements
-------------------------------------

* :dfn:`Extended return`
* Result is expressed as an object
* More expressive than aggregates
* Handling of unconstrained types
* Syntax (simplified):

   .. code:: Ada

      return identifier : subtype [:= expression];

      return identifier : subtype
      [do
         sequence_of_statements ...
       end return];

..
  language_version 2005

----------------------------------
Extended Return Statements Example
----------------------------------

.. code:: Ada

       --  Implicitly limited array
       type Spin_Lock_Array (Positive range <>) of Spin_Lock;

       function F return Spin_Lock_Array is
       begin
         return Result : Spin_Lock_Array (1 .. 10) do
           ...
         end return;
       end F;

..
  language_version 2005

------------------------------------
Expression / Statements Are Optional
------------------------------------

* Without sequence (returns default if any)

   .. code:: Ada

      function F return Spin_Lock is
      begin
        return Result : Spin_Lock;
      end F;

* With sequence

   .. code:: Ada

      function F return Spin_Lock is
        X : Interfaces.Unsigned_8;
      begin
        --  compute X ...
        return Result : Spin_Lock := (Flag => X);
      end F;

..
  language_version 2005

-----------------------
Statements Restrictions
-----------------------

* **No** nested extended return
* **Simple** return statement **allowed**

   - **Without** expression
   - Returns the value of the **declared object** immediately

.. code:: Ada

   function F return Spin_Lock is
   begin
     return Result : Spin_Lock do
       if Set_Flag then
         Result.Flag := 1;
         return;  --  returns 'Result'
       end if;
       Result.Flag := 0;
     end return; --  Implicit return
   end F;

..
  language_version 2005

------
Quiz
------

.. include:: ../quiz/limited_extended_return/quiz.rst
