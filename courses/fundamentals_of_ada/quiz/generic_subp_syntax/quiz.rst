..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

   generic
      type T1 is (<>);
      type T2 (<>) is private;
   procedure G
     (A : T1;
      B : T2);

Which instantiation is not legal?

   A. :answermono:`procedure A is new G (String, Character);`
   B. ``procedure B is new G (Character, Integer);``
   C. ``procedure C is new G (Integer, Boolean);``
   D. ``procedure D is new G (Boolean, String);``

.. container:: animate

   * :ada:`T2` can be almost anything, so it's not the issue
   * :ada:`T1` must be discrete, so it cannot be :ada:`String`

