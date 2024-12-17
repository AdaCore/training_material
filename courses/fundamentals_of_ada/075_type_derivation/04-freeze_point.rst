==============
Freeze Point
==============

-----------------------------
What is the "Freeze Point"?
-----------------------------

* Ada doesn't explicitly identify the end of the "scope" of a type

   * The compiler needs to know it for determining primitive operations
   * Also needed for other situations (described elsewhere)

* This end is the implicit **freeze point** occurring whenever:

   - A **variable** of the type is **declared**
   - The type is **derived**
   - The **end of the scope** is reached

* Subprograms past this "freeze point" are not primitive operations

.. code:: Ada

   type Parent is Integer;
   procedure Prim (V : Parent);

   type Child is new Parent;

   -- Parent has been derived, so it is frozen.
   -- Prim2 is not a primitive
   procedure Prim2 (V : Parent);

   V : Child;

   -- Child used in an object declaration, so it is frozen
   -- Prim3 is not a primitive
   procedure Prim3 (V : Child);

-----------------------
Debugging Type Freeze
-----------------------

* Freeze |rightarrow| Type **completely** defined
* Compiler does **need** to determine the freeze point

    - To instantiate, derive, get info on the type (:ada:`'Size`)...
    - Freeze rules are a guide to place it
    - Actual choice is more technical

        + May contradict the standard

* :command:`-gnatDG` to get **expanded** source

    - **Pseudo-Ada** debug information

:filename:`pkg.ads`

   .. code:: Ada

       type Up_To_Eleven is range 0 .. 11;

:filename:`<obj>/pkg.ads.dg`

.. container:: latex_environment tiny
        
   ::

      type example__up_to_eleven_t is range 0 .. 11;              -- type declaration
      [type example__Tup_to_eleven_tB is new short_short_integer] -- representation
      freeze example__Tup_to_eleven_tB []                         -- freeze representation
      freeze example__up_to_eleven_t []                           -- freeze representation

------
Quiz
------

.. container:: latex_environment tiny

   .. code:: Ada

      type Parent is range 1 .. 100;
      procedure Proc_A (X : in out Parent);

      type Child is new Parent range 2 .. 99;
      procedure Proc_B (X : in out Parent);
      procedure Proc_B (X : in out Child);

      -- Other scope
      procedure Proc_C (X : in out Child);

      type Grandchild is new Child range 3 .. 98;

      procedure Proc_C (X : in out Grandchild);

.. container:: columns

 .. container:: column

  Which are :ada:`Parent`'s primitives?

     A. :answermono:`Proc_A`
     B. ``Proc_B``
     C. ``Proc_C``
     D. No primitives of :ada:`Parent`

 .. container:: column

  .. container:: animate

   Explanations

   A. Correct
   B. Freeze: :ada:`Parent` has been derived
   C. Freeze: scope change
   D. Incorrect


