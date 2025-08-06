====================
Generic Completion
====================

------------------------------
Implications at Compile-Time
------------------------------

* The body needs to be visible when compiling the user code
* Therefore, when distributing a component with generics to be instantiated, the code of the generic must come along

-----------------------------
Generic and Freezing Points
-----------------------------

* A generic type **freezes** the type and needs the **full view**
* May force separation between its declaration (in spec) and instantiations (in private or body)

.. code:: Ada

   generic
      type X is private;
   package Base is
      V : access X;
   end Base;

   package P is
      type X is private;
      -- illegal
      package B is new Base (X);
   private
      type X is null record;
   end P;

-------------------------------
Generic Incomplete Parameters
-------------------------------

* A generic type can be incomplete
* Allows generic instantiations before full type definition
* Restricts the possible usages (only :ada:`access`)

.. code:: Ada

   generic
      type X; -- incomplete
   package Base is
      V : access X;
   end Base;

   package P is
      type X is private;
      -- legal
      package B is new Base (X);
   private
      type X is null record;
   end P;

------
Quiz
------

.. include:: ../quiz/genericity_private_type/quiz.rst
