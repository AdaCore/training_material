==========================
Task and Protected Types
==========================

------------------------------------------------
Beyond One-Off Tasks: Task and Protected Types
------------------------------------------------

* Creating templates for tasks and protected objects

  * When you need multiple tasks or protected objects that behave similarly

* Task (and protected) types rather than objects

  * Can be parameterized to cause different behavior

------------------------
Reusable Task Patterns
------------------------

.. code:: Ada

   task type Worker is
      entry Initialize (Cyle : Duration);
   end Worker;

   task body Worker is
      Delay_Time : Duration;
   begin
      accept Initialize (Cyle : Duration) do
          Delay_Time := Cycle;
      end Initialize;
      loop
         delay Delay_Time;
         Do_Something;
      end loop;
   end Worker;

   W1, W2 : Worker;

   procedure Main is
   begin
      W1.Initialize (1.0);
      W2.Initialize (2.0);
   end Main;

* Each Worker runs its own independent thread of control

-------------------------------
Reusable Protected Components
-------------------------------

* Protected type

  * Defines synchronized access to shared data

* Protected object - an instance of that type

* Procedures and functions inside the type control access rules

.. code:: Ada

  protected type Counter is
     procedure Increment;
     function Value return Integer;
  private
     Count : Integer := 0;
  end Counter;

  protected body Counter is
     procedure Increment is
     begin
        Count := Count + 1;
     end Increment;

     function Value return Integer is (Count);
  end Counter;

  C1, C2 : Counter;
