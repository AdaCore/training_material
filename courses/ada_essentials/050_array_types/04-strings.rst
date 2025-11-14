=========
Strings
=========

----------------
"String" Types
----------------

* Language-defined unconstrained array types

  - Allow double-quoted literals as well as aggregates
  - Always have a character component type
  - Always one-dimensional

* Language defines various types

  - `String`, with `Character` as component

    .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last;
      type String is array (Positive range <>) of Character;

  - `Wide_String`, with `Wide_Character` as component
  - `Wide_Wide_String`, with `Wide_Wide_Character` as component

    - Ada 2005 and later

* Can be defined by applications too

----------------------------------
Application-Defined String Types
----------------------------------

* Like language-defined string types

  - Always have a character component type
  - Always one-dimensional

* Recall character types are enumeration types with at least one character literal value

.. code:: Ada

   type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
   type Roman_Number is array (Positive range <>)
       of Roman_Digit;
   Orwellian : constant Roman_Number := "MCMLXXXIV";

------------------------------------------
Specifying Constraints Via Initial Value
------------------------------------------

* Lower bound is :ada:`Index_subtype'First`
* Upper bound is taken from number of items in value

.. code:: Ada

   subtype Positive is Integer range 1 .. Integer'Last;
   type String is array (Positive range <>)
       of Character;

   Prompt1 : String := "Hello World!";
   -- Prompt1'First is Positive'First (1)

   type Another_String is array (Integer range <>)
       of Character;

   Prompt2 : Another_String := "Hello World!";
   -- Prompt2'First is Integer'First

-----------------
String Literals
-----------------

* A :dfn:`literal` is a *textual* representation of a value in the code

.. code:: Ada
   
   -- two double quotes with nothing inside
   A_Null_String : constant String := "";

   String_Of_Length_One : constant String := "A";

   Embedded_Single_Quotes : constant String
                          := "Embedded 'single' quotes";
                          
   Embedded_Double_Quotes : constant String
                          := "Embedded ""double"" quotes";

.. container:: speakernote

  Note that the last example literal (that has embedded double quotes) is not an example of concatenation!
