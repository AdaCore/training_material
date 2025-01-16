=========================================
Reference Counting Using Controlled Types
=========================================

---------------
Global Overview
---------------

* Idiom for counting object references

    - Safe deallocation
    - No memory leak
    - Efficient
    - All :ada:`access` must then be using it

* A refcounted type derives from :ada:`Refcounted`

    - Tagged
    - Get a :ada:`Ref` through :ada:`Set`
    - Turn a :ada:`Ref` into an :ada:`access` through :ada:`Get`

.. code:: Ada
    
    package Ref_Counter is
       type Refcounted is abstract tagged private;
       procedure Free (Self : in out Refcounted) is null;

       type Refcounted_Access is access all Refcounted'Class;
       type Ref is tagged private;

       procedure Set (Self : in out Ref; Data : Refcounted'Class);
       function Get (Self : Ref) return Refcounted_Access;
       procedure Finalize (P : in out Ref);
       procedure Adjust   (P : in out Ref);
    private
       type Refcounted is abstract tagged record
          Refcount : Integer := 0;
       end record;

       type Ref is new Ada.Finalization.Controlled with record
           Data : Refcounted_Access;
       end record;

----------------------
Implementation Details
----------------------

* :ada:`Set` is safe
    
    - :ada:`Ref` default value is :ada:`null`
    - Clears up any previously used :ada:`Ref`

.. code:: Ada

    procedure Set (Self : in out Ref; Data : Refcounted'Class) is
      D : constant Refcounted_Access := new Refcounted'Class'(Data);
    begin
      if Self.Data /= null then
          Finalize (Self); -- decrement old reference count
      end if;

      Self.Data := D;
      Adjust (Self);  -- increment reference count (set to 1)
    end Set;

* :ada:`Adjust` called for all new references

.. code:: Ada

    overriding procedure Adjust (P : in out Ref) is
    begin
       if P.Data /= null then
          P.Data.Refcount := P.Data.Refcount + 1;
       end if;
    end Adjust;
