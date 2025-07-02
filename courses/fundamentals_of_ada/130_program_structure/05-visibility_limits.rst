===================
Visibility Limits
===================

-------------------------------------
Parents Do Not Know Their Children!
-------------------------------------

* Children grant themselves access to ancestors' private parts

   - May be created well after parent
   - Parent doesn't know if/when child packages will exist

* Alternatively, language *could have* been designed to grant access when declared

   - Like ``friend`` units in C++
   - But would have to be prescient!

      * Or else adding children requires modifying parent

   - Hence too restrictive

* Note: Parent body can reference children

   - Typical method of parsing out complex processes

----------------------------------------------
Correlation to C++ Class Visibility Controls
----------------------------------------------

.. container:: columns

 .. container:: column

   * Ada private part is visible to child units

      .. code:: Ada

         package P is
           A ...
         private
           B ...
         end P;
         package body P is
           C ...
         end P;

 .. container:: column

   * Thus private part is like the protected part in C++

      .. code:: C++

         class C {
         public:
           A ...
         protected:
           B ...
         private:
           C ...
         };

-------------------
Visibility Limits
-------------------

* Visibility to parent's private part is not open-ended

   - Only visible to private parts and bodies of children
   - As if only private part of child package is nested in parent

* Recall users can only reference exported declarations

   - Child public spec only has access to parent public spec

.. code:: Ada

   package Parent is
      ...
   private
      type Parent_T is ...
   end Parent;

   package Parent.Child is
     -- Parent_T is not visible here!
   private
     -- Parent_T is visible here
   end Parent.Child;

   package body Parent.Child is
    -- Parent_T is visible here
   end Parent.Child;

--------------------------------
Children Can Break Abstraction
--------------------------------

* Could **break** a parent's abstraction

  - Alter a parent package state
  - Alters an ADT object state

* Useful for reset, testing: fault injections...

.. code:: Ada

   package Stack is
      ...
   private
      Values : array (1 .. N) of Foo;
      Top : Natural range 0 .. N := 0;
   end Stack;

   package body Stack.Reset is
      procedure Reset is
      begin
        Top := 0;
      end Reset;
   end Stack.Reset;

--------------------------
Using Children for Debug
--------------------------

* Provide **accessors** to parent's private information
* eg internal metrics...

.. code:: Ada

   package P is
      ...
   private
     Internal_Counter : Integer := 0;
   end P;

.. code:: Ada

   package P.Child is
     function Count return Integer;
   end P.Child;

.. code:: Ada

   package body P.Child is
     function Count return Integer is
     begin
       return Internal_Counter;
     end Count;
   end P.Child;

------
Quiz
------

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      package P is
         Object_A : Integer;
      private
         Object_B : Integer;
         procedure Dummy_For_Body;
      end P;

      package body P is
         Object_C : Integer;
         procedure Dummy_For_Body is null;
      end P;

      package P.Child is
         function X return Integer;
      end P.Child;

  .. container:: column

   Which return statement(s) would be legal in ``P.Child.X?``

      A.  :answermono:`return Object_A;`
      B.  :answermono:`return Object_B;`
      C.  ``return Object_C;``
      D.  None of the above

   .. container:: animate

      Explanations

      A. :ada:`Object_A` is in the public part of :ada:`P` - visible to any unit that :ada:`with`'s :ada:`P`
      B. :ada:`Object_B` is in the private part of :ada:`P` - visible in the private part or body of any descendant of :ada:`P`
      C. :ada:`Object_C` is in the body of :ada:`P`, so it is only visible in the body of :ada:`P`
      D. A and B are both valid completions

