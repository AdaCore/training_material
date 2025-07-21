..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

Which declaration(s) is (are) legal?

A. | :answermono:`function F (S : String) return Boolean is`
   |   :answermono:`(for all C of S => C /= ' ');`
B. | ``function F (S : String) return Boolean is``
   |   ``(not for some C of S => C = ' ');``
C. | ``function F (S : String) return String is``
   |   ``(for all C of S => C);``
D. | :answermono:`function F (S : String) return String is`
   |   :answermono:`(if (for all C of S => C /= ' ') then "OK"`
   |    :answermono:`else "NOK");`

.. container:: animate

    B. Parentheses required around the quantified expression
    C. Must return a :ada:`Boolean`
