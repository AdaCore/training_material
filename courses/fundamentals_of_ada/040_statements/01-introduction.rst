==============
Introduction
==============

-----------------
Statement Kinds
-----------------

* Simple

  - :ada:`null`
  - :ada:`A := B` (assignments)
  - :ada:`exit`
  - :ada:`goto`
  - :ada:`delay`
  - :ada:`raise`
  - :ada:`P (A, B)` (procedure calls)
  - :ada:`return`
  - Tasking-related: :ada:`requeue`, entry call :ada:`T.E (A, B)`, :ada:`abort`

* Compound

  - :ada:`if`
  - :ada:`case`
  - :ada:`loop` (and variants)
  - :ada:`declare`
  - Tasking-related: :ada:`accept`, :ada:`select`

*Tasking-related are seen in the tasking chapter*

----------------------------
Procedure Calls (Overview)
----------------------------

* Procedures must be defined before they are called

  .. code:: Ada

     procedure Activate (This : in out Foo;
                         Flag :        Boolean);  

* Procedure calls are statements

  * Traditional call notation

    .. code:: Ada

      Activate (Idle, True);

  * "Distinguished Receiver" notation

    .. code:: Ada

      Idle.Activate (True);

* More details in "Subprograms" section

