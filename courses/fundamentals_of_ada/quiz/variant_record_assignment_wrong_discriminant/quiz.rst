.. code:: Ada

    procedure Main is
       type Shape_Kind is (Circle, Line);
    
       type Shape (Kind : Shape_Kind) is record
          case Kind is
             when Line =>
                X, Y : Float;
                X2, Y2 : Float;
             when Circle =>
                Radius : Float;
          end case;
       end record;
    begin
       V := V2;

Which declaration(s) is(are) legal for this piece of code?

A. | ``V : Shape := (Circle, others => 0.0)``
   | ``V2 : Shape (Line);``
B. | :answermono:`V : Shape := (Kind => Circle, Radius => 0.0);`
   | :answermono:`V2 : Shape (Circle);`
C. | ``V : Shape (Line) := (Kind => Circle, Radius => 0.0);``
   | ``V2 : Shape (Circle);``
D. | ``V : Shape;``
   | ``V2 : Shape (Circle);``

.. container:: animate

    A. Cannot assign with different discriminant
    B. OK
    C. ``V`` initial value has a different discriminant
    D. ``Shape`` cannot be mutable: ``V`` must have a discriminant
