********************************
Using Strings in Ada with GNAT
********************************

.. include:: support_files/symbols.rst

==============
Introduction
==============

-----------------------------
GNAT vs Ada String packages
-----------------------------

* Ada provides several standard :ada:`String` manipulation packages

    -

* Ada has 3 standard "character" sets

    - :ada:`Character` is 8-bits wide
    - :ada:`Wide_Character` is 16-bits wide
    - :ada:`Wide_Wide_Character` is 32-bits wide

-------------------------------
Ada :ada:`String` and Unicode
-------------------------------

* Unicode defines **code-points**

    - Each **unicode character** has one
    - Possible values :ada:`16#000000# .. 16#10ffff#`
    - ~150,000 characters are defined

* Unicode has a large number of encodings

    - Each encoding needs a different data-stream width
    - UTF-8: 8-bits
    - UCS-2, UTF-16, UTF-16LE, UTF-16BE: 16-bits
    - UTF-32, UTF-32LE, UTF-32BE: 32-bits 

* Character sets are **unrelated** to unicode encoding

    - Several :ada:`Character` or :ada:`Wide_Character` may represent a **single** code-point
    - eg. :ada:`#16#a2#` in UTF-8 is :ada:`(Character'Val (#2#11000010#), Character'Val (#2#10100010#))`

* See https://unicode.org/faq/

------------------------------------
Use Standard libraries for Unicode
------------------------------------

* Encoding turns a sequence of code-points to a string-type object

    - It cannot fail

* Transcoding turns a string-type onto another string-type

    - Input may be malformed

* Decoding turns a string-type object to a sequence of code-points

    - Input may be malformed
    - It is a **very-hard** problem

* Get help from the libraries
* :ada:`Ada.Strings.UTF_Encoding` for encoding (RM A.4.11)
* :ada:`GNAT.UTF_32` for code-point informations

    + Uses type :ada:`Wide_Wide_String`
    + Name is confusing because UTF-32 is an encoding
    + Common mix-up between code-point, character set and encoding

=========================
Ada :ada:`String` types
=========================

----------------
Standard Types
----------------

* Ada defines 3 standard **string models**

    - Fixed-length
    - Bounded
    - Unbounded

* Each of those for 3 different **character sets**

    - Mapped to :ada:`Character` 8-bits components

        + :ada:`package <pkg>` and :ada:`type <pkg>_String`

    - Mapped to :ada:`Wide_Character` 16-bits components

        + :ada:`package Wide_<pkg>` and :ada:`type <pkg>_Wide_String`

    - Mapped to :ada:`Wide_Wide_Character` 32-bits components

        + :ada:`package Wide_Wide_<pkg>` and :ada:`type <pkg>_Wide_Wide_String`

* Thus **9** standard :ada:`String` types eg. :ada:`Strings.Wide_Bounded.Bounded_Wide_String`
* :ada:`String` version showed

---------------------
Fixed-Length Strings
---------------------

:ada:`package Ada.Strings.Fixed`

* Standard :ada:`String` type
* Defined as an :ada:`array of Character`
* Memory and size

    - Size is **constant** :ada:`String'Length`
    - Memory size is **static**

* **Best** performances
* RM A.4.3

----------------
Bounded Strings
----------------

:ada:`generic package Ada.Strings.Bounded.Generic_Bounded_Length`

* Type :ada:`<Pkg_Instance>.Bounded_String`
* Implementation-defined **opaque pointer**
* Memory and size

    - Size is a **variable** :ada:`Length (S : Bounded_String) return Natural`
    - Max size is **constant** :ada:`<Pkg_Instance>.Max : Natural`
    - Memory size is **static**

* **Very good** performances
* Conversions with String

    - :ada:`Ada.Strings.<Pkg_Instance>.To_String`
    - :ada:`Ada.Strings.<Pkg_Instance>.To_Bounded_String`

* RM A.4.4

------------------
Unbounded Strings
------------------

:ada:`package Ada.Strings.Unbounded`

* Type :ada:`Strings.Bounded.Unbounded_String`
* Implementation-defined **opaque pointer**
* Memory and size

    - Size is a **variable** :ada:`Length (S : Unbounded_String) return Natural`
    - Memory size is **dynamic** (using Finalization)

* **Bad** performances

    - Especially if lots of **hidden** reallocations
    - eg. concatenations with :ada:`String & String`

* Conversions with String

    - :ada:`Ada.Strings.Unbounded.To_String`
    - :ada:`Ada.Strings.Unbounded.To_Unbounded_String`

* RM A.4.5

============================
Standard String Operations
============================

-------------------------
Linear Search of String
-------------------------

.. code:: Ada

    function Index (Source  : in <String_Type>;
                    Pattern : in String;
                    From    : in Positive;
                    Going   : in Direction)
      return Natural;

* Linear search
* Most efficient for arbitrary strings
* Lots of overloads
* Common arguments

    - :ada:`Pattern` Pattern to search
    - :ada:`From` Index to start from
    - :ada:`From` [Optional] Index to start from, must be in range
    - :ada:`Going` [Optional] Direction to search from

* Returns index or 0 if Pattern is not found

--------------------
Counting of String
--------------------

.. code:: Ada

    function Count (Source  : in <String_Type>;
                    Pattern : in String)
      return Natural;

* Number of occurent of Pattern
* Linear search
* Most efficient for arbitrary strings

-------------
Replacement
-------------

.. code:: Ada

    procedure Insert (Source  : in <String_Type>;
                      Before  : in Positive;
                      Pattern : in String);

    function Overwrite (Source   : in <String_Type>;
                        Positiom : in Positive;
                        Pattern  : in String)
      return String;

* Linear replacement
* No change of size
* Performant
* Limited...

-------------------------
Chaining Concatenations
-------------------------

.. code:: Ada

    

===============================
Match with :ada:`GNAT.Regexp`
===============================

---------------------
Regular Expressions
---------------------

* Mechanism for matching :ada:`Strings`
* Optimized for matching **multiple choices**
* Syntax

    - :code:`abc` The literal string "abc" 
    - :code:`(abc)` Grouping operator
    - :code:`.` Any character
    - :code:`\.` Literal :code:`.` (all special char can be escaped)
    - :code:`*` Previous group any number of times
    - :code:`+` Previous group at least once
    - :code:`?` Previous group at most once
    - :code:`[abc]` Any character between a, b, or c
    - :code:`[a-c]` Any character in the range between a and c
    - :code:`[^abc]` Any character that is **not** a, b, or c
    - :code:`A|B` The group A or the group B

------------------------------
Regular Expressions Examples
------------------------------

* :code:`.` will match :code:`A` and :code:`$` but not :code:`AB`
* :code:`\.` will match :code:`.` but not :ada:`A`
* :code:`.?` will match :code:`A` and the empty string but not :code:`AB`
* :code:`.+` will match :code:`A` and :code:`A$B` but not the empty string
* :code:`...[A-Z]*` will match :code:`AAA` and :code:`$%^AAAAA`
* :code:`..*` is **strictly** equivalent to :code:`.+`

------
Quiz
------

Which of the following will the pattern :code:`abcd?` match?

* :answer:`abcd`
* :answer:`abc`
* a
* ad
* acd

------
Quiz
------

Which of the following will the pattern :code:`a[bc]d?` match?

* abcd
* :answer:`abc`
* a
* ad
* :answer:`acd`

------
Quiz
------

Which of the following will the pattern :code:`a[bc]*d?` match?

* :answer:`abcd`
* :answer:`abc`
* :answer:`a`
* :answer:`ad`
* :answer:`acd`

------
Quiz
------

Which of the following will the pattern :code:`[a-zA-Z0-9]+@[a-zA-Z0-9]+\.(com|edu)` match?

* @adacore.com
* :answer:`foo@adacore.edu`
* john.silver@adacore.com
* :answer:`SARAHCONNOR@ADACORE.EDU`

--------------------
Defining a Pattern
--------------------

.. code:: Ada

    GNAT.Regexp.Compile (Pattern        : String;
                         Glob           : Boolean;
                         Case_Sensitive : Boolean)
      return Regexp;

* :code:`Pattern`: Regex pattern
* :code:`Glob`: Use globbing syntax
* :code:`Case_Sensitive`: Use a case-sensitive pattern
* Match with

.. code:: Ada

    function Match (S : String;
                    R : Regexp) return Boolean;

-------------------------
Updating Code for Regex
-------------------------

.. code:: Ada

    if Index (S1, S2) > 0 and Index (S1, S3) > 0 then
    ...

* Inefficient: iterates over S2 and S3 for **each** character of S1

    :math:`O(n) = S1'Length \times (S2'Length + S3'Length)`

* Replace by the regexp :code:`S2|S3`
* Following code is more efficient

.. code:: Ada

    declare
       R : constant Regexp := GNAT.Regexp.Compile (S1 & "|" & S3);
    begin
       if Match (S1, R) then
       ...
    end;

* Beware of :code:`S2` and :ada:`S3` containing regexp characters!

    - Escape them with "\" beforehand

------
Quiz
------

.. code:: Ada

    R : constant Regexp := GNAT.Regexp.Compile ("a*d");
    B : Boolean := GNAT.Regexp.Match (R, "ad");

What is the result?

* B is True
* B is False
* :answer:`Compilation Error`
* Runtime Error

------
Quiz
------

.. code:: Ada

    R : constant Regexp := GNAT.Regexp.Compile ("ab?c*d+");
    B : Boolean := GNAT.Regexp.Match ("ad", R);

What is the result?

* :answer:`B is True`
* B is False
* Compilation Error
* Runtime Error

------
Quiz
------

.. code:: Ada

    R : constant Regexp := GNAT.Regexp.Compile ("*");
    B : Boolean := GNAT.Regexp.Match ("ad", R);

What is the result?

* B is True
* B is False
* Compilation Error
* :answer:`Runtime Error`

=================================
Performance-oriented operations
=================================

--------------------------
:ada:`GNAT.String_Split`
--------------------------

TBD

--------------------------
:ada:`GNAT.Rewrite_Data`
--------------------------

TBD
