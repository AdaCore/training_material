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

   -- Simple task that, upon startup, loops forever
   -- calling some procedure and pausing
   task type Worker is
      entry Initialize (Cyle : Duration);
   end Worker;

   task body Worker is
      Delay_Time : Duration;
   begin
      -- Wait until initialized with a delay time
      accept Initialize (Cyle : Duration) do
          Delay_Time := Cycle;
      end Initialize;
      -- Once task has started, just wait a certain
      -- amount of time and then call a procedure
      loop
         delay Delay_Time;
         Do_Something;
      end loop;
   end Worker;

   --  Two tasks that start at elaboration and wait for initialization
   Worker_1, Worker_2 : Worker;

   procedure Main is
   begin
      -- Start the tasks at different frequencies
      Worker_1.Initialize (1.0);
      Worker_2.Initialize (2.0);
   end Main;

* Each Worker runs its own independent thread of control

-------------------------------
Reusable Protected Components
-------------------------------

* :dfn:`Protected type`

  * Defines synchronized access to shared data

* :dfn:`Protected object`

  * An instance of a protected type

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
