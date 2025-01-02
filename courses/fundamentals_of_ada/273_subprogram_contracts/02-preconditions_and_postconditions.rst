===================================
Preconditions and Postconditions
===================================

-----------------------------
Subprogram-based Assertions
-----------------------------

* **Explicit** part of a subprogram's **specification**

    - Unlike defensive code

* :dfn:`Precondition`

   - Assertion expected to hold **prior to** subprogram call

* :dfn:`Postcondition`

   - Assertion expected to hold **after** subprogram return

* Requirements and guarantees on both supplier and client
* Syntax uses **aspects**

   .. code:: Ada

      procedure Push (This : in out Stack_T;
                      Value : Content_T)
        with Pre  => not Full (This),
             Post => not Empty (This)
                     and Top (This) = Value;

---------------------------------
Requirements / Guarantees: Quiz
---------------------------------

* Given the following piece of code

    .. code:: Ada

       procedure Start is
       begin
           ...
           Turn_On;
           ...

       procedure Turn_On
        with Pre => Has_Power,
             Post => Is_On;

* Complete the table in terms of requirements and guarantees

.. list-table::

   * - 
     - Client (:ada:`Start`)
     - Supplier (:ada:`Turn_On`)

   * - Pre (:ada:`Has_Power`)
     - :animate:`Requirement`
     - :animate:`Guarantee`

   * - Post (:ada:`Is_On`)
     - :animate:`Guarantee`
     - :animate:`Requirement`

-----------------------
Defensive Programming
-----------------------

* Should be replaced by subprogram contracts when possible

.. code:: Ada

   procedure Push (The_Stack : Stack) is
      Entry_Length : constant Positive := Length (The_Stack);
   begin
      pragma Assert (not Is_Full (The_Stack)); -- entry condition
      [...]
      pragma Assert (Length (The_Stack) = Entry_Length + 1); -- exit condition
   end Push;

* Subprogram contracts are an **assertion** mechanism

   - **Not** a drop-in replacement for all defensive code

.. code:: Ada

   procedure Force_Acquire (Resource : Peripheral) is
   begin
      if not Available (Resource) then
         -- Corrective action
         Force_Release (Resource);
         pragma Assert (Available (Resource));
      end if;

      Acquire (Resource);
   end;

-----------------------------
Pre/Postcondition Semantics
-----------------------------

* Calls inserted automatically by compiler

|

.. image:: pre_and_post_insertion_flow.svg
   :width: 90%

-------------------------------------
Contract with Quantified Expression
-------------------------------------

* Pre- and post-conditions can be **arbitrary** :ada:`Boolean` expressions 

.. code:: Ada

   type Status_Flag is (Power, Locked, Running);

   procedure Clear_All_Status (
       Unit : in out Controller)
     -- guarantees no flags remain set after call
     with Post => (for all Flag in Status_Flag =>
       not Status_Indicated (Unit, Flag));

   function Status_Indicated (
       Unit : Controller;
       Flag : Status_Flag)
       return Boolean;

-------------------------------------
Visibility for Subprogram Contracts
-------------------------------------

* **Any** visible name
    
   - All of the subprogram's **parameters**
   - Can refer to functions **not yet specified**

      - Must be declared in same scope
      - Different elaboration rules for expression functions

  .. code:: Ada

     function Top (This : Stack) return Content
       with Pre => not Empty (This);
     function Empty (This : Stack) return Boolean;

* :ada:`Post` has access to special attributes
    
    - See later

------------------------------------------
Preconditions and Postconditions Example
------------------------------------------

* Multiple aspects separated by commas

.. code:: Ada

     procedure Push (This : in out Stack;
                     Value : Content)
       with Pre  => not Full (This),
            Post => not Empty (This) and Top (This) = Value;

------------------------------------
(Sub)Types Allow Simpler Contracts
------------------------------------

* Pre-condition

   .. code:: Ada

      procedure Compute_Square_Root (Input : Integer;
                                     Result : out Natural)
        with Pre  => Input >= 0,
             Post => (Result * Result) <= Input and
                     (Result + 1) * (Result + 1) > Input;

* Subtype

   .. code:: Ada

      procedure Compute_Square_Root (Input  : Natural;
                                     Result : out Natural)
         with
             -- "Pre => Input >= 0" not needed
             -- (Input can't be < 0)
             Post => (Result * Result) <= Input and
                     (Result + 1) * (Result + 1) > Input;

------
Quiz
------

.. code:: Ada

   --  Convert string to Integer
   function To_Integer ( S : String ) return Integer
      with Pre => S'Length > 0;

   procedure Print_Something is
      I : Integer := To_Integer ("");
   begin
      Put_Line (I'Image);
   end Print_Something;

Assuming :ada:`To_Integer` is defined somewhere, what happens
when :ada:`Print_Something` is run?

   A. "0" is printed
   B. Constraint Error exception
   C. :answer:`Assertion Error exception`
   D. Undefined behavior

.. container:: animate

   Explanations

   The call to :ada:`To_Integer` will fail its precondition, which is considered
   an :ada:`Assertion_Error` exception.

------
Quiz
------

.. code:: Ada

   function Area (Length : Positive; Height : Positive) return Positive is
      (Length * Height)
   with Pre => ?

Which pre-condition is necessary for :ada:`Area` to calculate the correct result for
all values :ada:`L` and :ada:`H`

   A. ``Length > 0 and Height > 0``
   B. ``Length < Positive'Last and Height < Positive'Last``
   C. ``Length * Height in Positive``
   D. :answer:`None of the above`

.. container:: animate

   Explanations

   A. Parameters are :ada:`Positive`, so this is unnecessary
   B. :ada:`Length = Positive'Last-1 and Height = Positive'Last-1` will still cause an overflow
   C. Classic trap: the check itself may cause an overflow!

   Preventing an overflow requires using the expression :ada:`Integer'Last / Length <= Height`

