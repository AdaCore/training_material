==============
Introduction
==============

----------------------
"Design-By-Contract"
----------------------

* Source code acting in roles of **client** and **supplier** under a binding **contract**

   - :dfn:`Contract` specifies *requirements* or *guarantees*

      - *"A specification of a software element that affects its use by potential clients."* (Bertrand Meyer)

   - :dfn:`Supplier` provides services

       - Guarantees specific functional behavior
       - Has requirements for guarantees to hold

   - :dfn:`Client` utilizes services

       - Guarantees supplier's conditions are met
       - Requires result to follow the subprogram's guarantees

-------------
"Assertion"
-------------

* Boolean expression expected to be :ada:`True`
* Said *to hold* when :ada:`True`
* Language-defined :ada:`pragma`

    - The :ada:`Ada.Assertions.Assert` subprogram can wrap it

.. code:: Ada

   pragma Assert (not Full (Stack));
   -- stack is not full
   pragma Assert (Stack_Length = 0,
                  Message => "stack was not empty");
   -- stack is empty

* Raises language-defined :ada:`Assertion_Error` exception if expression does not hold

.. code:: Ada

   package Ada.Assertions is
     Assertion_Error : exception;
     procedure Assert (Check : in Boolean);
     procedure Assert (Check : in Boolean; Message : in String);
   end Ada.Assertions;

-----------------------
Defensive Programming
-----------------------

* Should be replaced by subprogram contracts when possible

.. code:: Ada

   procedure Push (Stack : Stack_Type) is
      Entry_Length : constant Positive := Length (Stack);
   begin
      pragma Assert (not Is_Full (Stack)); -- entry condition
      [...]
      pragma Assert (Length (Stack) = Entry_Length + 1); -- exit condition
   end Push;

* Subprogram contracts are an **assertion** mechanism

   - **Not** a drop-in replacement for all defensive code

.. code:: Ada

   procedure Force_Acquire (Resource : Shared_Resource) is
   begin
      if not Available (Resource) then
         -- Corrective action
         Force_Release (Resource);
         pragma Assert (Available (Resource));
      end if;

      Acquire (Resource);
   end;
