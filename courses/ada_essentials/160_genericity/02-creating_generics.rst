===================
Creating Generics
===================

-------------
Declaration
-------------

* Subprograms

   .. code:: Ada

      generic
         type T is private;
      procedure Swap (L, R : in out T);

* Packages

  .. code:: Ada

      generic
         type T is private;
      package Stack is
         procedure Push (Item : T);
      end Stack;

* Body is required

    - Will be specialized and compiled for **each instance**

* Children of generic units have to be generic themselves

   .. code:: Ada

      generic
      package Stack.Utilities is
         procedure Print (S : Stack_T);

-------
Usage
-------

* Instantiated with the :ada:`new` keyword

.. code:: Ada

   --  Standard library
   function Convert is new Ada.Unchecked_Conversion
     (Integer, Array_Of_4_Bytes);
   --  Callbacks
   procedure Parse_Tree is new Tree_Parser
     (Visitor_Procedure);
   --  Containers, generic data-structures
   package Integer_Stack is new Stack (Integer);

* Advanced usages for testing, proof, meta-programming

------
Quiz
------

Which one(s) of the following can be made generic?

.. code:: Ada

    generic
       type T is private;
    <code goes here>

A. :answermono:`package`
B. ``record``
D. :answermono:`function`
C. ``array``

.. container:: animate

   Only packages, functions, and procedures, can be made generic.
