========================
Ada.Characters.Latin_1
========================

----------------------------
Package Contents (Partial)
----------------------------

.. code::

  package Ada.Characters.Latin_1 is
    NUL          : constant Character := Character'Val (0);
    SOH          : constant Character := Character'Val (1);
    STX          : constant Character := Character'Val (2);
    ETX          : constant Character := Character'Val (3);
    EOT          : constant Character := Character'Val (4);
    ENQ          : constant Character := Character'Val (5);
    -- ...
    Space        : constant Character := ' ';  -- Character'Val (32)
    Exclamation  : constant Character := '!';  -- Character'Val (33)
    Quotation    : constant Character := '"';  -- Character'Val (34)
    Number_Sign  : constant Character := '#';  -- Character'Val (35)
    Dollar_Sign  : constant Character := '$';  -- Character'Val (36)
    -- ...
    LC_A         : constant Character := 'a';  -- Character'Val (97)
    LC_B         : constant Character := 'b';  -- Character'Val (98)
    LC_C         : constant Character := 'c';  -- Character'Val (99)
    LC_D         : constant Character := 'd';  -- Character'Val (100)
    LC_E         : constant Character := 'e';  -- Character'Val (101)
    -- ...
  end Ada.Characters.Latin_1;

--------
Idioms
--------

* Obvious - giving names to unprintable characters

* Good coding practice to use names instead of literals

  * Easier searching for non-alphanumeric characters

* Some symbols have multiple names, such as:

  * :ada:`Minus_Sign` |rightarrow| :ada:`Hyphen`
  * :ada:`NBSP` |rightarrow| :ada:`No_Break_Space`
  * :ada:`Ring_Above` |rightarrow| :ada:`Degree_Sign`

