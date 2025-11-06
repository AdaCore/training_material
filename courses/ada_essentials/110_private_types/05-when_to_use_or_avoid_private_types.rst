====================================
When to Use or Avoid Private Types
====================================

---------------------------
When to Use Private Types
---------------------------

* Implementation may change

   - Allows **clients** to be unaffected by changes in representation

* Normally available operations do not "make sense"

   - Normally available based upon type's representation
   - Determined by intent of ADT

.. container:: latex_environment footnotesize

  .. code:: Ada

    package Valves is
       type Valve_Id_T is private;
       procedure Set (Valve : Valve_Id_T;
                      Value : Integer);
    private
       type Valve_Id_T is new Integer;
    end Valves;

    with Valves; use Valves;
    procedure Initialize is
       Hot, Cold : Valve_Id_T;
    begin
       Set (Hot, Hot + Cold);
    end Initialize;

* If :ada:`Valve_Id_T` was not private, call to **Set** would be valid

   - But doesn't make sense

-----------------------------
When to Avoid Private Types
-----------------------------

* If the abstraction is too simple to justify the effort

   - But that's the thinking that led to Y2K rework

* If normal **client** interface requires representation-specific operations that cannot be provided

   - Those that cannot be redefined by programmers
   - Would otherwise be hidden by a private type
   - If `Vector` is private, indexing of components is annoying

     .. code:: Ada

       type Vector is array (Positive range <>) of Float;
       V : Vector (1 .. 3);
       ...
       V (1) := Alpha; -- Illegal since Vector is private

