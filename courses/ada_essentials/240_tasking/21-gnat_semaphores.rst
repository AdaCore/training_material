===============
GNAT Semaphores
===============

----------
Semaphores
----------

* Shared counters
* Multitask-safe

    - Support priorities from "Real-time Systems" LRM Annex D

* :ada:`Counting_Semaphore` and :ada:`Binary_Semaphore`

    - :ada:`protected` types
    - Counting holds an Integer
    - Binary holds a Boolean

* Priority ceiling (LRM D.3)

    - For :ada:`pragma Locking_Policy (Ceiling_Locking)`
    - Protects against priority inversions


---------
Interface
---------

.. code:: Ada

   protected type Counting_Semaphore
      (Initial_Value : Natural;
      [...]

      entry Seize;
      --  Blocks caller until/unless the semaphore's internal counter is
      --  greater than zero. Decrements the semaphore's internal counter when
      --  executed.

      procedure Release;
      --  Increments the semaphore's internal counter

.. code:: Ada

   protected type Binary_Semaphore
     (Initially_Available : Boolean;

.. code:: Ada

   subtype Mutual_Exclusion is Binary_Semaphore
     (Initially_Available => True,
      Ceiling             => Default_Ceiling);

------------------
Idiom: Scope Locks
------------------

* Automatic release

.. code:: Ada

    type Scope_Lock (Lock : access Mutual_Exclusion) is
       new Ada.Finalization.Limited_Controlled with null record;

    procedure Initialize (This : in out Scope_Lock) is
    begin
      This.Lock.Seize;
    end Initialize;

    procedure Finalize (This : in out Scope_Lock) is
    begin
      This.Lock.Release;
    end Finalize;
    
    Mutex : aliased Mutual_Exclusion;

    State : Integer := 0;

    procedure Operation_1 is
       S : Scope_Lock (Mutex’Access);
    begin
       State := State + 1;  -- for example...
       Put_Line ("State is now" & State'Img);
    end Operation_1;
