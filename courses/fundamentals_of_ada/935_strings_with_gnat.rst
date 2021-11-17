*******************************
Using :ada:`String` with GNAT
*******************************

==============
Introduction
==============

-----------------------------
GNAT vs Ada String packages
-----------------------------

* Ada provides several standard :ada:`String` manipulation packages

    -

-------------------------------
Ada :ada:`String` and Unicode
-------------------------------

* Ada has 3 standard "character" sets

    - :ada:`Character` is 8 bits wide
    - :ada:`Wide_Character` is 16 bits wide
    - :ada:`Wide_Wide_Character` is 32 bits wide

* This is **unrelated** to unicode encoding
* Unicode defines **code-points**

    - Each **unicode character** has one
    - Possible values :ada:`16#000000# .. 16#10ffff#`
    - ~150,000 character exist

* Unicode has a large number of encodings

    - UTF-8-BE, UTF-8-LE : 8 bits
    - UCS-2, UTF-16 : 16 bits
    - UTF-32: 32 bits

* Encoding, decoding match code-points to "characters"

    - A single codepoint may map to several :ada:`Character`

---------------------------
Unicode
---------------------------

* Encoding turns a sequence of code-points to a string-type object

    - It cannot fail
    - It is relatively easy

* Decoding turns a string-type object to a sequence of code-points

    - Input may be malformed
    - It is a **very-hard** problem
    - Use a standard library

* Transcoding

=========================
Ada :ada:`String` types
=========================

---------------------
Fixed-Length String
---------------------

----------------
Bounded String
----------------

------------------
Unbounded String
------------------

===============================
Match with :ada:`GNAT.Regexp`
===============================

---------------------
Regular Expressions
---------------------

--------------------
Defining a pattern
--------------------

--------------------
Matching a pattern
--------------------

---------------------------------
Using with :ada:`Wide_*_String`
---------------------------------

------
Quiz
------

======================
Modify :ada:`String`
======================

=================================
Performance-oriented operations
=================================



--------------------------
:ada:`GNAT.String_Split`
--------------------------

--------------------------
:ada:`GNAT.Rewrite_Data`
--------------------------
