==============
Introduction
==============

----------
Packages
----------

* Enforce separation of user from implementation

   - In terms of compile-time visibility
   - For data
   - For type representation, when combined with :ada:`private` types

      + Abstract Data Types

* Provide basic namespace control
* Directly support software engineering principles

   - Especially in combination with :ada:`private` types
   - Modularity
   - Information Hiding (Encapsulation)
   - Abstraction
   - Separation of Concerns

-------------------------------
Basic Syntax and Nomenclature
-------------------------------

* Spec

   - Basic declarative items **only**
   - e.g. no subprogram bodies

     .. code:: Ada

           package name is
              {basic_declarative_item}
           end [name];

* Body

  .. code:: Ada

           package body name is
              declarative_part
           end [name];

-----------------------------------------
Separating Interface and Implementation
-----------------------------------------

* :dfn:`Implementation` and :dfn:`specification` are textually distinct from each other

   - Typically in separate files

* Users can compile their code before body exists

   - All they need is the package specification
   - Users have **no** visibility over the body
   - Full user/designer consistency is guaranteed

.. code:: Ada

   package Float_Stack is
     Max : constant := 100;
     procedure Push (X : in Float);
     procedure Pop (X : out Float);
   end Float_Stack;

---------------------------------
Uncontrolled Visibility Problem
---------------------------------

* Users have too much access to representation

   - Data
   - Type representation

* Changes force users to recode and retest
* Manual enforcement is not sufficient
* Why fixing bugs introduces new bugs!

