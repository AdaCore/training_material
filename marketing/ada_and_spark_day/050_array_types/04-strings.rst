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

  - :ada:`String`, with :ada:`Character` as component

    .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last;
      type String is array (Positive range <>) of Character;

  - :ada:`Wide_String`, with :ada:`Wide_Character` as component
  - :ada:`Wide_Wide_String`, with :ada:`Wide_Wide_Character` as component

    - Ada 2005 and later

* Can be defined by applications too
