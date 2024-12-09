==============
Introduction
==============

-----------------------
Improving Readability
-----------------------

* Descriptive names plus hierarchical packages makes for very long statements

   .. code::

      Messages.Queue.Diagnostics.Inject_Fault (
         Fault    => Messages.Queue.Diagnostics.CRC_Failure,
         Position => Messages.Queue.Front);

* Operators treated as functions defeat the purpose of overloading

   .. code::

      Complex1 := Complex_Types."+" (Complex2, Complex3);

* Ada has mechanisms to simplify hierarchies

--------------------------
Operators and Primitives
--------------------------

* :dfn:`Operators`

   - Constructs which behave generally like functions but which differ syntactically or semantically
   - Typically arithmetic, comparison, and logical

* **Primitive operation**

   - Predefined operations such as ``=`` and ``+``  etc.
   - Subprograms declared in the same package as the type and which operate on the type
   - Inherited or overridden subprograms
   - For :ada:`tagged` types, class-wide subprograms
   - Enumeration literals

