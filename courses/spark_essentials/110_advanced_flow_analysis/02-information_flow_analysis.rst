===========================
Information Flow Analysis
===========================

---------------------------
Direct and Indirect Flows
---------------------------

* A direct flow occurs when assigning :ada:`A` to :ada:`B`

  .. code:: ada

     B := A;

* An indirect flow occurs when assigning :ada:`B` conditioned on :ada:`A`

  .. code:: ada

     if A then
        B := ...
     end if;

* A direct flow can be masquerading as indirect flow

  .. code:: ada

     if A then
        B := True;
     else
        B := False;
     end if;

* :toolname:`GNATprove` handle both flows together in flow analysis

-------------------------------------
Self-Dependency on Array Assignment
-------------------------------------

* Flow analysis is not value-dependent

* Assigning an array component or slice preserves part of the original value

  .. code:: ada

     type T is array (1 .. 2) of Boolean;
     A : T := ...

     A (1) := True;
     -- intermediate value of A seen as dependent on
     -- original value
     A (2) := False;
     -- final value of A seen as dependent on original value

* This holds also for slices

  .. code:: ada

     A (1 .. 2) := (True, False);
     -- final value of A seen as dependent on original value

