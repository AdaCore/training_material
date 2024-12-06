==============
Introduction
==============

------------------------------------------
Multiple Inheritance Is Forbidden in Ada
------------------------------------------

* There are potential conflicts with multiple inheritance
* Some languages allow it: ambiguities have to be resolved when entities are referenced
* Ada forbids it to improve integration

.. code:: Ada

    type Graphic is tagged record
       X, Y : Float;
    end record;
    function Get_X (V : Graphic) return Float;

    type Shape is tagged record
       X, Y : Float;
    end record;
    function Get_X (V : Shape) return Float;

    type Displayable_Shape is new Shape and Graphic with ...

----------------------------------
Multiple Inheritance - Safe Case
----------------------------------

* If only one type has concrete operations and fields, this is fine

   .. code:: Ada

      type Graphic is abstract tagged null record;
      function Get_X (V : Graphic) return Float is abstract;

      type Shape is tagged record
         X, Y : Float;
      end record;
      function Get_X (V : Shape) return Float;

      type Displayable_Shape is new Shape and Graphic with ...

* This is the definition of an interface (as in Java)

   .. code:: Ada

      type Graphic is interface;
      function Get_X (V : Graphic) return Float is abstract;

      type Shape is tagged record
         X, Y : Float;
      end record;
      function Get_X (V : Shape) return Float;

      type Displayable_Shape is new Shape and Graphic with ...

