=======
Tasks
=======

------------
Basic Task
------------

* **Specification** (:ada:`task`) 

  * What other parts of the program see

* **Body** (:ada:`task body`) 

  * Code the task actually runs

.. code:: Ada

  procedure Main is
    task My_Task; -- declare the task

    task body My_Task is -- implement the task
    begin
       Put_Line ("Entered My_Task");
    end My_Task;
 begin
    Put_Line ("In Main");
 end Main;

* :ada:`My_Task` starts automatically when :ada:`Main` starts

.. note::

  The application’s main program is itself a task

-----------------------
Basic Synchronization
-----------------------

* Enclosing scope cannot exit until all of its tasks have completed

.. code:: Ada

  procedure Show_Simple_Sync is
    task Hello_Task;
    task body Hello_Task is
    begin
      for Counter in 1 .. 10 loop
        Put_Line ("hello");
      end loop;
    end Hello_Task;
  begin
    null;
    --  Will wait here until Hello_Task is finished
  end Show_Simple_Sync;

------------
Rendezvous
------------

* Tasks synchronize actions using mechanism called :dfn:`rendezvous`

  * Follows a client/server model

  * **Server** task declares an :ada:`entry`

    * Public point of synchronization other tasks call

  * **Client** task calls that entry same as a procedure

  * **Server** must then accept call

* Both tasks are blocked until rendezvous is complete

  * **Server** must perform entry processing
  * **Client** is waiting for **Server** to finish

.. code:: Ada

  task Server_Task is
    entry Receive_Message (S : in String);
  end Server_Task;

  task body Server_Task is
  begin
    accept Receive_Message (S : in String) do -- waiting for client
      Put_Line ("Received: " & S);
    end Receive_Message; -- release to client
  end Server_Task;

  procedure Client is
  begin
    -- The client calls the entry and waits
    Server_Task.Receive_Message ("Hello!");
  end Client;

-----------------------
Sequential Rendezvous
-----------------------

* Task can have multiple entry points that need to be called in sequence
* Each entry call is blocking

.. code:: Ada

  task body Worker is
    Job_Data : Some_Data_Type;
    Result   : Some_Result_Type;
  begin
    loop
      -- Step 1: Wait for a client to provide a new job
      accept Get_Work (Data : in Some_Data_Type) do
        Job_Data := Data;
      end Get_Work;

      -- Step 2: Do the work (details omitted)
      Result := Process (Job_Data);

      -- Step 3: Wait for the client to request the result
      accept Report_Result (Final_Result : out Some_Result_Type) do
        Final_Result := Result;
      end Report_Result;
    end loop;
  end Worker;

  Worker.Get_Work (My_Job);          -- Give the worker a job
  Worker.Report_Result (My_Result);  -- Get the result

* :ada:`Worker` cannot generate report until after :ada:`Get_Work` has completed
