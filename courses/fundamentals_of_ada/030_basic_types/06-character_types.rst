=================
Character Types
=================

----------------------------------
Language-Defined Character Types
----------------------------------

* :ada:`Character`

   - 8-bit Latin-1
   - Base element of :ada:`String`
   - Uses attributes :ada:`'Image` / :ada:`'Value`

* :ada:`Wide_Character`

   - 16-bit Unicode
   - Base element of :ada:`Wide_Strings`
   - Uses attributes :ada:`'Wide_Image` / :ada:`'Wide_Value`

* :ada:`Wide_Wide_Character`

   - 32-bit Unicode
   - Base element of :ada:`Wide_Wide_Strings`
   - Uses attributes :ada:`'Wide_Wide_Image` / :ada:`'Wide_Wide_Value`

-----------------------------
Character Oriented Packages
-----------------------------

* Language-defined
* :ada:`Ada.Characters.Handling`

   - Classification
   - Conversion

* :ada:`Ada.Characters.Latin_1`

   - Characters as constants

* See RM Annex A for details

-----------------------------------------
`Ada.Characters.Latin_1` Sample Content
-----------------------------------------

.. code:: Ada

   package Ada.Characters.Latin_1 is
     NUL : constant Character := Character'Val (0);
     ...
     LF  : constant Character := Character'Val (10);
     VT  : constant Character := Character'Val (11);
     FF  : constant Character := Character'Val (12);
     CR  : constant Character := Character'Val (13);
     ...
     Commercial_At  : constant Character := '@';  -- Character'Val (64)
     ...
     LC_A : constant Character := 'a';  -- Character'Val (97)
     LC_B : constant Character := 'b';  -- Character'Val (98)
     ...
     Inverted_Exclamation : constant Character := Character'Val (161);
     Cent_Sign            : constant Character := Character'Val (162);
   ...
     LC_Y_Diaeresis       : constant Character := Character'Val (255);
   end Ada.Characters.Latin_1;

----------------------------------------
Ada.Characters.Handling Sample Content
----------------------------------------

.. code:: Ada

   package Ada.Characters.Handling is
     function Is_Control           (Item : Character) return Boolean;
     function Is_Graphic           (Item : Character) return Boolean;
     function Is_Letter            (Item : Character) return Boolean;
     function Is_Lower             (Item : Character) return Boolean;
     function Is_Upper             (Item : Character) return Boolean;
     function Is_Basic             (Item : Character) return Boolean;
     function Is_Digit             (Item : Character) return Boolean;
     function Is_Decimal_Digit     (Item : Character) return Boolean renames Is_Digit;
     function Is_Hexadecimal_Digit (Item : Character) return Boolean;
     function Is_Alphanumeric      (Item : Character) return Boolean;
     function Is_Special           (Item : Character) return Boolean;
     function To_Lower (Item : Character) return Character;
     function To_Upper (Item : Character) return Character;
     function To_Basic (Item : Character) return Character;
     function To_Lower (Item : String) return String;
     function To_Upper (Item : String) return String;
     function To_Basic (Item : String) return String;
   ...
   end Ada.Characters.Handling;

------
Quiz
------

.. include:: ../quiz/user_defined_character/quiz.rst

------
Quiz
------

.. code:: Ada

    with Ada.Characters.Latin_1;
    use Ada.Characters.Latin_1;
    with Ada.Characters.Handling;
    use Ada.Characters.Handling;

Which of the following proposition(s) are true?

A. ``NUL = 0``
B. ``NUL = '\0'``
C. :answermono:`Character'Pos (NUL) = 0`
D. :answermono:`Is_Control (NUL)`
