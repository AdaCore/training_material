..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       generic
          type Component_T is (<>);
          Last : in out Component_T;
       procedure Write (P : Component_T);
    
       Numeric        : Integer;
       Enumerated     : Boolean;
       Floating_Point : Float;

Which of the following piece(s) of code is (are) legal?

A. :answermono:`procedure Write_A is new Write (Integer, Numeric)`
B. :answermono:`procedure Write_B is new Write (Boolean, Enumerated)`
C. ``procedure Write_C is new Write (Integer, 1234)``
D. ``procedure Write_D is new Write (Float, Floating_Point)``

.. container:: animate

    A. :ada:`Integer` matches restrictions of :ada:`Component_T` and :ada:`Numeric` is the appropriate type
    B. :ada:`Boolean` matches restrictions of :ada:`Component_T` and :ada:`Enumerated` is the appropriate type
    C. The second generic parameter has to be a variable
    D. The first generic parameter has to be discrete
