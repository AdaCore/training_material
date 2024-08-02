========================
Anonymous Access Types
========================

-----------------------------
Anonymous Access Parameters
-----------------------------

* Parameter modes are of 4 types: :ada:`in`, :ada:`out`, :ada:`in out`, :ada:`access`
* The access mode is called :dfn:`anonymous access type`

   - Anonymous access is implicitly general (no need for :ada:`all`)

* When used:

   - Any named access can be passed as parameter
   - Any anonymous access can be passed as parameter

.. code:: Ada

   type Acc is access all Integer;
   Aliased_Integer : aliased Integer;
   Access_Object   : Acc := Aliased_Integer'Access;
   procedure P1 (Anon_Access : access Integer) is null;
   procedure P2 (Access_Parameter : access Integer) is
   begin
      P1 (Aliased_Integer'Access);
      P1 (Access_Object);
      P1 (Access_Parameter);
   end P2;

------------------------
Anonymous Access Types
------------------------

* Other places can declare an anonymous access

   .. code:: Ada

      function F return access Integer;
      V : access Integer;
      type T (V : access Integer) is record
        C : access Integer;
      end record;
      type A is array (Integer range <>) of access Integer;

* Do not use them without a clear understanding of accessibility check rules

----------------------------------
Anonymous Access Constants
----------------------------------

* :ada:`constant` (instead of :ada:`all`) denotes an access type through which the referenced object cannot be modified

   .. code:: Ada

      type CAcc is access constant Integer;
      G1 : aliased Integer;
      G2 : aliased constant Integer := 123;
      V1 : CAcc := G1'Access;
      V2 : CAcc := G2'Access;
      V1.all := 0; -- illegal

* :ada:`not null` denotes an access type for which null value cannot be accepted

   - Available in Ada 2005 and later

   .. code:: Ada

      type NAcc is not null access Integer;
      V : NAcc := null; -- illegal

* Also works for subprogram parameters

   .. code:: Ada

      procedure Bar (V1 : access constant Integer);
      procedure Foo (V1 : not null access Integer); -- Ada 2005

