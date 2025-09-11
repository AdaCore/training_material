=============================
Language-Defined Exceptions
=============================

--------------------
`Constraint_Error`
--------------------

* Caused by violations of constraints on range, index, etc.
* The most common exceptions encountered

   .. code:: Ada

      K : Integer range 1 .. 10;
      ...
      K := -1;

   .. code:: Ada

      L : array (1 .. 100) of Some_Type;
      ...
      L (400) := SomeValue;

-----------------
`Program_Error`
-----------------

* When runtime control structure is violated

   - Elaboration order errors and function bodies

* When implementation detects bounded errors

   - Discussed momentarily

.. code:: Ada

   function F return Some_Type is
   begin
     if something then
       return Some_Value;
     end if; -- program error - no return statement
   end F;

-----------------
`Storage_Error`
-----------------

* When insufficient storage is available
* Potential causes

   - Declarations
   - Explicit allocations
   - Implicit allocations

.. code:: Ada

   Data : array (1..1e20) of Big_Type;

------------------------------
Explicitly-Raised Exceptions
------------------------------

**Syntax**

.. container:: source_include 190_exceptions/syntax.bnf :start-after:explicitly_raised_exceptions_begin :end-before:explicitly_raised_exceptions_end :code:bnf

Raised by application via :ada:`raise` statements

   * Named exception becomes active
   * A :ada:`raise` by itself is only allowed in handlers

**Example**

.. code:: Ada

   if Unknown (User_ID) then
     raise Invalid_User;
   end if;

   if Unknown (User_ID) then
     raise Invalid_User
        with "Attempt by " & Image (User_ID);
   end if;
