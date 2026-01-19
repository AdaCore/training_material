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

.. container:: latex_environment small

  .. code:: Ada

    task Server_Task is
      entry Receive_Message (S : in String);
    end Server_Task;

    task body Server_Task is
    begin
      accept Receive_Message (S : in String) do -- wait for client
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

.. container:: latex_environment footnotesize

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

----------------------
Selective Rendezvous
----------------------

* Task isn't limited to waiting for just one entry

  * Typically, **server** task needs to be able to accept several kinds of requests

* To wait for multiple entries at the same time use :ada:`select` statement

  * Task waits until **client** calls an :ada:`entry` included in :ada:`select`, then executes that block

  * If multiple calls waiting, the runtime chooses which **client** to handle

    * Selection order is not guaranteed

------------------------
Select Example in Code
------------------------

* **Server** task waits for either a message to process or a signal to stop

.. container:: latex_environment small

  .. code:: Ada

    task body Controller is
    begin
      loop
        -- Wait for EITHER Receive_Message OR Stop to be called
        select
          accept Receive_Message (V : in String) do
            Put_Line ("Processing: " & V);
          end Receive_Message;
        or
          accept Stop;
            Put_Line ("Stopping task...");
            exit; -- Exit the loop to terminate the task
        end select;
      end loop;
    end Controller;

* How a **client** would use it:

  .. code:: Ada

    -- Client_X
    Controller.Receive_Message ("Run diagnostic");

    -- Client_Y
    Controller.Stop;

  * :ada:`Client_X` and :ada:`Client_Y` can be the same task, different tasks, or the main program

------
Quiz
------

.. code:: Ada

  task Simple_Task is
     entry Go;
  end Simple_Task;

  task body Simple_Task is
  begin
      accept Go do
          loop
              null;
          end loop;
      end Go;
  end Simple_Task;

What happens when :ada:`Simple_Task.Go` is called?

A. Compilation error
B. Run-time error
C. The calling task completes successfully
D. :answer:`Simple_Task hangs`

.. container:: animate

    A. Syntax is correct
    B. Code is doing what it is supposed to
    C. Caller must wait for :ada:`Go` block to finish
    D. :ada:`Go` block is entered, but never completes
