
*************
Elaboration
*************

==============
Introduction
==============

---------------------------
Why Elaboration Is Needed
---------------------------

* Ada has some powerful features that require initialization:

   .. code:: Ada

      with Dep1;
      package P1 is
         -- value not known at compile time
         Val : constant Integer := Dep1.Call;
      end P1;
 
* May also involve dynamic allocation:

   .. code:: Ada

      with P1;
      package P2 is
         -- size not known at compile time
         Buffer : String (1 .. P1.Val);
      end P1;
 
* Or explicit user code to initialize a package

   .. code:: Ada

      package body P3 is
         ...
      begin
         Put_Line ("Starting P3");
      end P3;
 
* Requires initialization code at startup
* Implies ordering

=============
Elaboration
=============

----------
Examples
----------

.. include:: examples/200_elaboration/elaboration.rst

-------------
Elaboration
-------------

* Process where entities are created
* The Rule: "an entity has to be elaborated before use"

   - Subprograms have to be elaborated before being called
   - Variables have to be elaborated before being referenced

* Such elaboration issues typically arise due to:

   - Global variable initialization
   - Package sequence of statements

.. code:: Ada
    
   with Dep1;
   package P1 is
      -- Dep1 body has to be elaborated before this point
      V_Spec : Integer := Dep1.Call;
   end P1;
       
   with Dep2;
   package body P1 is
      V_Body : Integer;
   begin
      -- Dep2 body has to be elaborated before this point
      V_Body := Dep2.Call;
   end P1;
     
-------------------
Elaboration Order
-------------------

* The elaboration order is the order in which the packages are created
* It may or may not be deterministic

   .. code:: Ada

      package P1 is
         V_Spec : Integer := Call(1);
      end P1;
      package body P1 is
         V_Body : Integer := Call(2);
      end P1;
      package P2 is
         V_Spec : Integer := Call('A');
      end P1;
      package body P2 is
         V_Body : Integer := Call('B');
      end P1;
 
* The binder (GNAT: gnatbind) is responsible for finding an elaboration order

   - Computes the possible order
   - Reports an error when no order is possible

-----------------------------------
Circular Elaboration Dependencies
-----------------------------------

* Although not explicitly specified by the `with` clauses, elaboration dependencies may exhibit circularities
* Sometimes, they are static
    
   .. code:: Ada
    
      package body P1 is
         V_Body : Integer := P2.Call;
      end P1;
      package body P2 is
         V_Body : Integer := P1.Call;
      end P2;
     
* Sometimes they are dynamic
    
   .. code:: Ada
    
      package body P1 is
         V_Body : Integer;
      begin
         if Something then
            V_Body := P1.Call;
         end if;
      end P1;
      package body P2 is
         V_Body : Integer;
      begin
         if Something then
            V_Body := P2.Call;
         end if;
      end P2;
     
-------------------------------
GNAT Static Elaboration Model
-------------------------------

* By default, GNAT ensures elaboration safety

   - It adds elaboration control pragma to statically ensure that elaboration is possible
   - Very safe, but...
   - Not fully Ada compliant (may reject some valid programs)
   - Highly recommended however (least surprising effect)

* Performed by :command:`gnatbind`

   - Automatically called by a builder (:command:`gprbuild`)
   - Reads ALI files from the closure
   - Generates :filename:`b_xxx.ad[sb]` or :filename:`b__xxx.ad[sb]` files
   - Contains elaboration and finalization procedures
   - Defines the entry point procedure, `main()`.

=====================
Elaboration Control
=====================

----------
Examples
----------

.. include:: examples/200_elaboration/elaboration_control.rst

-----------------------
`Pragma Preelaborate`
-----------------------

* Adds restrictions on a unit to ease elaboration
* Elaboration without explicit execution of code

   - No user initialization code
   - No calls to subprograms
   - Static values
   - Dependencies only on `Preelaborate` packages

   .. code:: Ada

      package P1 is
         pragma Preelaborate;
         Var : Integer := 7;
      end P1;
 
* But compiler may generate elaboration code

   .. code:: Ada

      package P1 is
        pragma Preelaborate;
        type Ptr is access String;
        v : Ptr := new String'("hello");
      end P1;
 
---------------
`Pragma Pure`
---------------

* Adds more restrictions on a unit to ease elaboration
* `Preelaborate` restrictions plus ...

   - No variable declaration
   - No allocators
   - No access type declaration
   - Dependencies only on `Pure` packages

   .. code:: Ada

      package Ada.Numerics is
         pragma Pure;
         Argument_Error : exception;
         Pi : constant := 3.14...;
      end Ada.Numerics;
 
* But compiler may generate elaboration code

   .. code:: Ada

      package P2 is
         pragma Pure;
         Var : constant Array (1 .. 10 * 1024) of Integer :=
               (others => 118);
      end P2;
 
-------------------------
`Pragma Elaborate_Body`
-------------------------

* Forces the elaboration of a body just after a specification
* Forces a body to be present even if none is required
* Problem: it may introduce extra circularities

   .. code:: Ada

      package P1 is
         pragma Elaborate_Body;
         function Call return Integer;
      end P1;
      with P2;
      package body P1 is
      ..
      end P1;
      package P2 is
         pragma Elaborate_Body;
         function Call return Integer;
      end P2;
      with P1;
      package body P2 is
      ...
      end P2;
 
* Useful in the case where a variable declared in the specification is initialized in the body

.. container:: speakernote

   If global value is set in body elaboration, without elaborate body it's possible to reference global value before it has been set

--------------------
`Pragma Elaborate`
--------------------

* `Pragma Elaborate` forces the elaboration of a dependency body
* It does not force the elaboration of transitive dependencies

.. code:: Ada
    
   package P1 is
      function Call return Integer;
   end P1;
   package P2 is
      function Call return Integer;
   end P1;
   with P1;
   package body P2 is
      function Call return Integer is ( P1.Call );
   end P2;
   with P2;
   pragma Elaborate (P2);
   -- P2 must be elaborated before we get here
   -- but nobody forces P1 to be elaborated!
   package body P3 is
      V : Integer;
   begin
      V := P2.Call;
   end P3;
     
------------------------
`Pragma Elaborate_All`
------------------------

* `Pragma Elaborate_All` forces the elaboration of a dependency body and all transitive dependencies
* May introduce unwanted cycles
* Safer than `Elaborate`

.. code:: Ada
    
   package P1 is
      function Call return Integer;
   end P1;
   package P2 is
      function Call return Integer;
   end P1;
   with P1;
   package body P2 is
      function Call return Integer is ( P1.Call );
   end P2;
   with P2;
   pragma Elaborate_All (P2);
   -- P2 must be elaborated before we get here.
   -- Elaborate_All enforces P1 being elaborated as well
   package body P3 is
      V : Integer;
   begin
      V := P2.Call;
   end P3;
     
========
Lab
========

.. include:: labs/200_elaboration.lab.rst

=========
Summary
=========

---------
Summary
---------

* Elaboration is a difficult problem to deal with
* The binder tries to resolve it in a "safe way"
* If it can't, it's possible to manually place elaboration pragmas
* Better to avoid elaboration constraints as much as possible
* Use dynamic elaboration (gnat binder switch -E) as last resort
* See *Elaboration Order Handling in GNAT* annex in GNAT Pro User's Guide.
