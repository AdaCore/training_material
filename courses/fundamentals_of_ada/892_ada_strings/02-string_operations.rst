=====================
String Operations
=====================

----------------------------
Primitive String Functions
----------------------------

* Operations like concatenation (:ada:`"&"`) and comparison (:ada:`">="`, etc)

  * Built in for **fixed-length** strings
  * Defined in appropriate package for **bounded** and **unbounded**

    * Require :ada:`use` or :ada:`use type` for simple visibility

--------------------
Common Subprograms
--------------------

.. container:: latex_environment scriptsize

  .. list-table::

    * - :ada:`"*"`

      - Return the character or string duplicated N times

    * - :ada:`Count`

      - Number of occurrences of specified string/character set

    * - :ada:`Delete`

      - Remove slice

    * - :ada:`Find_Token`

      - Location of token that matches/doesn't match character set

    * - :ada:`Head`

      - Front N characters (padded as necessary)

    * - :ada:`Index`

      - Index of character/string, given starting location/direction

    * - :ada:`Index_Non_Blank`

      - Index of first/last character/string, given starting location/direction

    * - :ada:`Insert`

      - Insert substring into source before the specified position

    * - :ada:`Overwrite`

      - Overwrite source with new substring starting at the specified position

    * - :ada:`Replace_Slice`

      - Replace specified slice with new string

    * - :ada:`Tail`

      - Last N characters (padded as necessary)

    * - :ada:`Translate`

      - Translate string using specified character mapping

    * - :ada:`Trim`

      - Remove leading/trailing characters from source

-------------------------------
Bounded/Unbounded Subprograms
-------------------------------

.. container:: latex_environment scriptsize

  .. list-table::

    * - :ada:`Append`

      - Concatenate bounded strings and/or standard strings

    * - 

      - to create an unbounded string

    * - :ada:`Element`

      - Character at specified position

    * - :ada:`Length`

      - Length of string

    * - :ada:`Replace_Element`

      - Put input character specified position

    * - :ada:`Slice`

      - Standard string slice from specified positions

    * - :ada:`To_String`

      - Convert unbounded string to standard string

-------------------------------
Unique Subprograms
-------------------------------

* :ada:`Ada.Strings.Fixed`

  .. container:: latex_environment scriptsize

    .. list-table::

      * - :ada:`Move`

        -  Copy source to target with truncation/padding

* :ada:`Ada.Strings.Bounded`

  .. container:: latex_environment scriptsize

    .. list-table::

      * - :ada:`Bounded_Slice`

        - Bounded string slice from specified positions

      * - :ada:`Replicate`

        - Return the bounded string duplicated N times

      * - :ada:`Set_Bounded_String`

        - Procedural copy standard string to bounded string

      * - :ada:`To_Bounded_String`

        - Copy standard string to bounded string

* :ada:`Ada.Strings.Unbounded`

  .. container:: latex_environment scriptsize

    .. list-table::

      * - :ada:`Set_Unbounded_String`

        - Procedural copy standard string to unbounded string

      * - :ada:`To_Unbounded_String`

        - Copy standard string to unbounded string

      * - :ada:`Unbounded_Slice`

        - Unbounded string slice from specified positions

