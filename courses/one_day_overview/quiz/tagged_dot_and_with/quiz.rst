..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    with Pkg1; -- Defines tagged type Tag1, with primitive P
    with Pkg2; use Pkg2; -- Defines tagged type Tag2, with primitive P
    with Pkg3; -- Defines tagged type Tag3, with primitive P
    use type Pkg3.Tag3;
    
    procedure Main is
       O1 : Pkg1.Tag1;
       O2 : Pkg2.Tag2;
       O3 : Pkg3.Tag3;

Which statement(s) is(are) valid?

A. :answermono:`O1.P`
B. ``P (O1)``
C. :answermono:`P (O2)`
D. ``P (O3)``

.. container:: animate

    D. Only operators are :ada:`use`d, should have been :ada:`use all`
