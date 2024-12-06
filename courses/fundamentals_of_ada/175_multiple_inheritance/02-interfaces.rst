============
Interfaces
============

--------------------
Interfaces - Rules
--------------------

* An interface is a tagged type marked interface, containing

   - Abstract primitives
   - Null primitives
   - No fields

* Null subprograms provide default empty bodies to primitives that can be overridden

   .. code:: Ada

      type I is interface;
      procedure P1 (V : I) is abstract;
      procedure P2 (V : access I) is abstract
      function F return I is abstract;
      procedure P3 (V : I) is null;

* Note: null can be applied to any procedure (not only used for interfaces)

----------------------
Interface Derivation
----------------------

* An interface can be derived from another interface, adding primitives

   .. code:: Ada

      type I1 is interface;
      procedure P1 (V : I) is abstract;
      type I2 is interface and I1;
      Procedure P2 (V : I) is abstract;

* A tagged type can derive from several interfaces and can derive from one interface several times

   .. code:: Ada

      type I1 is interface;
      type I2 is interface and I1;
      type I3 is interface;

      type R is new I1 and I2 and I3 ...

* A tagged type can derive from a single tagged type and several interfaces

   .. code:: Ada

      type I1 is interface;
      type I2 is interface and I1;
      type R1 is tagged null record;

      type R2 is new R1 and I1 and I2 ...

------------------------
Interfaces and Privacy
------------------------

* If the partial view of the type is tagged, then both the partial and the full view must expose the same interfaces

   .. code:: Ada

      package Types is

         type I1 is interface;
         type R is new I1 with private;

      private

         type R is new I1 with record ...

-------------------------------------
Limited Tagged Types and Interfaces
-------------------------------------

* When a tagged type is limited in the hierarchy, the whole hierarchy has to be limited
* Conversions to interfaces are "just conversions to a view"

   - A view may have more constraints than the actual object

* :ada:`limited` interfaces can be implemented by BOTH limited types and non-limited types
* Non-limited interfaces have to be implemented by non-limited types

