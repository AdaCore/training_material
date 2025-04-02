=========================
Conditional Expressions
=========================

----------------
If Expressions
----------------

* :ada:`(if Cond then A else B)` evaluates :ada:`A` or :ada:`B` depending on
  the value of :ada:`Cond`

  - Note: always in **parentheses**!
  - :ada:`A` and :ada:`B` must have the same type
  - ...not always :ada:`Boolean`!

    .. code:: Ada

       A := (if Cond then 2 else 3);

* Frequent use with :ada:`Boolean` type in specifications

  - :ada:`(if Cond then Property)` is shortcut for :ada:`(if Cond then Property else True)`
  - This expresses a **logical implication** :ada:`Cond` |rightarrow| :ada:`Property`
  - Also equivalent to :ada:`not Cond or else Property`

* Complete form has :ada:`elsif` parts

------------------
Case Expressions
------------------

* Extension of *if expressions* to non-Boolean discrete types

  .. code:: ada

     (case Day is
        when Monday
           | Friday
           | Sunday    => 6,
        when Tuesday   => 7,
        when Thursday
           | Saturday  => 8,
        when Wednesday => 9)

* Same **choice expressions** as in *case statements*

  - Can also use :ada:`others` as last alternative
  - Note: always in parentheses!
  - Note: cases are separated by commas

--------------
Set Notation
--------------

* Usable in both *case expressions* / *case statements* and in membership tests
* Without set notation:

  .. code:: Ada

     if X = 'A' or else X = 'B' or else X = 'C' then

* With set notation:

  .. code:: Ada

     if X in 'A' | 'B' | 'C' then

* Also allowed for opposite membership test: :ada:`if X not in ...`

