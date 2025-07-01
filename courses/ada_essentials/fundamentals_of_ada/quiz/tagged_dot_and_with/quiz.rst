..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    with Shapes; -- Defines tagged type Shape, with primitive P
    with Colors; use Colors; -- Defines tagged type Color, with primitive P
    with Weights; -- Defines tagged type Weight, with primitive P
    use type Weights.Weight;
    
    procedure Main is
       The_Shape : Shapes.Shape;
       The_Color : Colors.Color;
       The_Weight : Weights.Weight;

Which statement(s) is (are) valid?

A. :answermono:`The_Shape.P`
B. ``P (The_Shape)``
C. :answermono:`P (The_Color)`
D. ``P (The_Weight)``

.. container:: animate

    D. :ada:`use type` only gives visibility to operators; needs to be :ada:`use all type`
