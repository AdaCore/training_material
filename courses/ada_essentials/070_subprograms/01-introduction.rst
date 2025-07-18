==============
Introduction
==============

--------------
Introduction
--------------

* Are syntactically distinguished as :ada:`function` and :ada:`procedure`

   - Functions represent *values*
   - Procedures represent *actions*

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean
      procedure Split (T : in out Tree;
                       Left : out Tree;
                       Right : out Tree)

* Provide direct syntactic support for separation of specification from implementation

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean;
      function Is_Leaf (T : Tree) return Boolean is
      begin
      ...
      end Is_Leaf;

--------------------------------------
Recognizing Procedures and Functions
--------------------------------------

* Functions' results must be treated as values

   - And cannot be ignored

* Procedures cannot be treated as values
* You can always distinguish them via the call context

   .. code:: Ada
      :number-lines: 10

      Open (Source, "SomeFile.txt");
      while not End_of_File (Source) loop
        Get (Next_Char, From => Source);
        if Found (Next_Char, Within => Buffer) then
          Display (Next_Char);
          Increment;
        end if;
      end loop;

  * Note that a subprogram without parameters (:ada:`Increment` on line 15) does not allow an empty set of parentheses

----------------------------------
A Little "Preaching" About Names
----------------------------------

* Procedures are abstractions for actions
* Functions are abstractions for values
* Use identifiers that reflect those facts!

   - Imperative verbs for procedure identifiers
   - Nouns for function identifiers, as for mathematical functions

      + Questions work for boolean functions

.. code:: Ada

   procedure Open (V : in out Valve);
   procedure Close (V : in out Valve);
   function Square_Root (V: Float) return Float;
   function Is_Open (V: Valve) return Boolean;

