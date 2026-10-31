==============
Introduction
==============

-------------------------
The Notion of a Pattern
-------------------------

* Sometimes algorithms can be abstracted from types and subprograms

  .. code:: Ada

     procedure Swap_Int (Left, Right : in out Integer) is
       V : Integer := Left;
     begin
        Left := Right;
        Right := V;
     end Swap_Int;

     procedure Swap_Float (Left, Right : in out Float) is
        V : Float := Left;
     begin
        Left := Right;
        Right := V;
     end Swap_Float;

* It would be nice to extract these properties in some common pattern, and then just replace the parts that need to be replaced

  .. code:: Ada

     procedure Swap (Left, Right : in out (Integer | Float)) is
       V : (Integer | Float) := Left;
     begin
        Left := Right;
        Right := V;
     end Swap;

--------------------------------------
Ada Generic Compared to C++ Template
--------------------------------------

**Ada Generic**

  .. code:: Ada

    -- specification
    generic
      type T is private;
    procedure Swap (L, R : in out T);

    -- implementation
    procedure Swap (L, R : in out T) is
       Tmp : T := L;
    begin
       L := R;
       R := Tmp;
    end Swap;

    -- instance
    procedure Swap_F is new Swap (Float);

**C++ Template**

  .. code:: C++

    // prototype
    template <class T>
    void Swap (T & L, T & R);

    // implementation
    template <class T>
    void Swap (T & L, T & R) {
       T Tmp = L;
       L = R;
       R = Tmp;
    }

    // instance
    int x, y;
    Swap<int>(x,y);
