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
