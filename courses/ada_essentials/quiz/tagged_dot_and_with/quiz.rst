..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    with Shapes;  -- Defines tagged type Shape, primitive Set_Shape
    with Colors;  -- Defines tagged type Color, primitive Set_Color
    with Weights; -- Defines tagged type Weight,primitive Set_Weight
    use Colors;
    use type Weights.Weight;
    
    procedure Main is
       The_Shape  : Shapes.Shape;
       The_Color  : Colors.Color;
       The_Weight : Weights.Weight;

Which statement(s) is (are) valid?

A. :answermono:`The_Shape.Set_Shape`
B. ``Set_Shape (The_Shape)``
C. :answermono:`Set_Color (The_Color)`
D. ``Set_Weight (The_Weight)``

.. container:: animate

    A. "Distinguished Receiver" always allowed
    B. No :ada:`use` of :ada:`Colors` or :ada:`use all type` for :ada:`Color`
    C. :ada:`Set_Color` made visible by :ada:`use Colors`
    D. :ada:`use type Weights.Weight` only gives visibility to operators; needs to be :ada:`use all type`
