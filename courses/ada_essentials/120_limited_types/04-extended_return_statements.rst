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

**Syntax**

.. container:: source_include 120_limited_types/syntax.bnf :start-after:function_extended_return_statements_begin :end-before:function_extended_return_statements_end :code:bnf

..
  language_version 2005

------------------------------------
Extended Return Statements Example
------------------------------------

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

--------------------------------------
Expression / Statements Are Optional
--------------------------------------

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

-------------------------
Statements Restrictions
-------------------------

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

.. code:: Ada

  type T is limited record
     I : Integer;
  end record;

  function F return T is
  begin
     -- F body...
  end F;

  O : T := F;

Which declaration(s) of ``F`` is (are) valid?

A. ``return Return : T := (I => 1);``
B. :answermono:`return Result : T;`
C. ``return Value := (others => 1);``
D. | :answermono:`return R : T do`
   |    :answermono:`R.I := 1;`
   | :answermono:`end return;`

.. container:: animate

  A. Using :ada:`return` reserved keyword
  B. OK, default value
  C. Extended return must specify type
  D. Assign a value to a component of the extended return object
