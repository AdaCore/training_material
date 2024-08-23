=========================
User-Defined Exceptions
=========================

-------------------------
User-Defined Exceptions
-------------------------

* Syntax

   .. code:: Ada

      defining_identifier_list : exception;

* Behave like predefined exceptions

   - Scope and visibility rules apply
   - Referencing as usual
   - Some minor differences

* Exception identifiers' use is restricted

   - :ada:`raise` statements
   - Handlers
   - Renaming declarations

---------------------------------
User-Defined Exceptions Example
---------------------------------

* An important part of the abstraction
* Designer specifies how component can be used

.. code:: Ada

   package Stack is
     Underflow, Overflow : exception;
     procedure Push (Item : in Integer);
     ...
   end Stack;

   package body Stack is
     procedure Push (Item : in Integer) is
     begin
       if Top = Index'Last then
         raise Overflow;
       end if;
       Top := Top + 1;
       Values (Top) := Item;
     end Push;
   ...

