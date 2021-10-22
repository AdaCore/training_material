***********************
Advanced Polymorphism
***********************

================
Abstract Types
================

----------------
Abstract Types
----------------

* A tagged type can be declared :ada:`abstract`
* Then, :ada:`abstract tagged` types:

   - cannot be instantiated
   - can have abstract subprograms (with no implementation)
   - Non-abstract derivation of an abstract type must override and implement abstract subprograms

---------------------------
Abstract Types Ada vs C++
---------------------------

* Ada

    .. code:: Ada

       type Root is abstract tagged record
          F : Integer;
       end record;
       procedure P1 (V : Root) is abstract;
       procedure P2 (V : Root);
       type Child is abstract new Root with null record;
       type Grand_Child is new Child with null record;

       overriding  -- Ada 2005 and later
       procedure P1 (V : Grand_Child);

* C++

    .. code:: Ada

       class Root {
          public:
             int F;
             virtual void P1 (void) = 0;
             virtual void P2 (void);
       };
       class Child : public Root {
       };
       class Grand_Child {
          public:
             virtual void P1 (void);
       };

.. container:: speakernote

   "overriding" keyword is optional


======================
Advanced Dispatching
======================

----------------------------------
Static Calls on class-wide types
----------------------------------

* It is still possible to force a call to be static using a conversion of view

.. container:: columns

 .. container:: column

   *Ada*

   .. code:: Ada

      declare
        V1 : Root'Class :=
             Root'(others => <>);
        V2 : Root'Class :=
             Child'(others => <>);
      begin
        Root (V1).P; -- calls P of Root
        Root (V2).P; -- calls P of Root

 .. container:: column

   *C++*

   .. code:: C++

      Root * V1 = new Root ();
      Root * V2 = new Child ();
      ((Root) *V1).P ();
      ((Root) *V2).P ();

-------------------------------
Definite and class wide views
-------------------------------

* In C++, dispatching occurs only on pointers
* In Ada, dispatching occurs only on class wide views

.. code:: Ada

   type Root is tagged null record;
   procedure P1 (V : Root);
   procedure P2 (V : Root);
   type Child is new Root with null record;
   overriding procedure P2 (V : Child);
   procedure P1 (V : Root) is
