==============================
Limitations of Flow Analysis
==============================

-----------------------------------
Analysis of Value-Dependent Flows
-----------------------------------

* Flow analysis depends only on control flow, not on values
* Flow analysis is imprecise on value-dependent flows

  .. code:: Ada

     procedure Absolute_Value
       (X : Integer;
        R : out Natural) -- Initialization check fails
     is
     begin
       if X < 0 then
         R := -X;
       end if;
       if X >= 0 then
         R := X;
       end if;
     end Absolute_Value;

* Use control flow instead: use :ada:`if-then-else` above

----------------------------------------
Analysis of Array Initialization (1/2)
----------------------------------------

* Array indexes are values
* Flow analysis does not depend on values
* Flow analysis treats array assignment as a partial write

  - When assigning to an array index
  - When assigning to an array slice

  .. code:: Ada

     type T is array (1 .. 10) of Boolean;

     -- Initialization check fails
     procedure Init_Array (A : out T) is
     begin
        A (1) := True;
        A (2 .. 10) := (others => False);
     end Init_Array;

* No such imprecision for record components

----------------------------------------
Analysis of Array Initialization (2/2)
----------------------------------------

* Use array aggregates when possible

  .. code:: Ada

     type T is array (1 .. 10) of Boolean;

     procedure Init_Array (A : out T) is -- Initialization check proved
     begin
        A := (1 => True, 2 .. 10 => False);
     end Init_Array;

* Do not please the tool! :ada:`A` is not :ada:`in out` here!

  - Otherwise, caller is forced to initialize :ada:`A`

* Some built-in heuristics recognize an initializing loop

  .. code:: Ada

     procedure Init_Array (A : out T) is -- Initialization check proved
     begin
        for J in A'Range loop
           A (J) := False;
        end loop;
     end Init_Array;

---------------------------
Dealing with False Alarms
---------------------------

* Check messages can be justified with pragma :ada:`Annotate`

  .. code:: Ada

     procedure Init_Array
       (A : out T) -- Initialization check justified
     is
        pragma Annotate (GNATprove, False_Positive,
                         """A"" might not be initialized",
                         "value-dependent init");

* Justification inserted immediately after the check message location
* Relaxed initialization will be seen in course on Advanced Proof

