========================
Some Advanced Concepts
========================

----------------------
Waiting with a Delay
----------------------

* A :ada:`select` statement may **time-out** using :ada:`delay` or :ada:`delay until`

    - Resume execution at next statement

* Multiple :ada:`delay` allowed

   - Useful when the value is not hard-coded

.. code:: Ada

  loop
    select
      accept Receive_Message (V : String) do
        Put_Line ("Message : " & V);
      end Receive_Message;
    or
      delay 50.0;
      Put_Line ("Don't wait any longer");
      exit;
    end select;
  end loop;

*Task will wait up to 50 seconds for* :ada:`Receive_Message`. *If no message is received, it will write to the console, and then restart the loop. (If the* :ada:`exit` *wasn't there, the loop would exit the first time no message was received.)*

------------------------------------------
Calling an Entry with a Delay Protection
------------------------------------------

* A call to :ada:`entry` **blocks** the task until the entry is :ada:`accept` 'ed
* Wait for a **given amount of time** with :ada:`select ... delay`
* Only **one** entry call is allowed
* No :ada:`accept` statement is allowed

.. code:: Ada

   task Msg_Box is
      entry Receive_Message (V : String);
   end Msg_Box;

   procedure Main is
   begin
      select
         Msg_Box.Receive_Message ("A");
      or
         delay 50.0;
      end select;
   end Main;

*Procedure will wait up to 50 seconds for* :ada:`Receive_Message` *to be accepted before it gives up*

----------------------------
The Delay Is Not a Timeout
----------------------------

* The time spent by the client is actually **not bounded**

    - Delay's timer **stops** on :ada:`accept`
    - The call blocks **until** :ada:`end` of server-side statements

* In this example, the total delay is up to **1010 s**

.. code:: Ada

    task body Msg_Box is
       accept Receive_Message (S : String) do
          delay 1000.0;
       end Receive_Message;
    ...
    procedure Client is
    begin
       select
          Msg_Box.Receive_Message ("My_Message")
       or
          delay 10.0;
       end select;

----------------------------
Non-blocking Accept or Entry
----------------------------

* Using :ada:`else`

    - Task **skips** the :ada:`accept` or :ada:`entry` call if they are **not ready** to be entered

* On an :ada:`accept`

.. include:: ../examples/select_non_blocking_entry_and_call/extracts/tasks.adb
   :code: Ada

* As caller on an :ada:`entry`

.. include:: ../examples/select_non_blocking_entry_and_call/extracts/main_no_stop.adb
   :code: Ada

* :ada:`delay` is **not** allowed in this case

-----------------------------------
Issues with "Double Non-Blocking"
-----------------------------------

* For :ada:`accept ... else` the server **peeks** into the queue

   - Server **does not** wait

* For :ada:`<entry-call> ... else` the caller looks for a **waiting** server
* If both use it, the entry will **never** be called
* Server

.. include:: ../examples/select_non_blocking_entry_and_call/extracts/tasks.adb
   :code: Ada

* Caller

.. include:: ../examples/select_non_blocking_entry_and_call/extracts/main_no_call.adb
   :code: Ada

-----------------------
Terminate Alternative
-----------------------

* An entry can't be called anymore if all tasks calling it are over
* Handled through :ada:`or terminate` alternative

   - Terminates the task if **all others** are terminated
   - Or are **blocked** on :ada:`or terminate` themselves

* Task is terminated **immediately**

    - No additional code executed

.. code:: Ada

   select
      accept Entry_Point
   or
      terminate;
   end select;

-------------------------------------
Select on Protected Objects Entries
-------------------------------------

* Same as :ada:`select` but on task entries

   - With a :ada:`delay` part

   .. code:: Ada

      select
         O.Push (5);
      or
         delay 10.0;
         Put_Line ("Delayed overflow");
      end select;

  - or with an :ada:`else` part

   .. code:: Ada

      select
         O.Push (5);
      else
         Put_Line ("Overflow");
      end select;

------
Queue
------

* Protected :ada:`entry`, :ada:`procedure`, and tasks :ada:`entry` are activated by **one** task at a time
* **Mutual exclusion** section
* Other tasks trying to enter are **queued**

    - In **First-In First-Out** (FIFO) by default

* When the server task **terminates**, tasks still queued receive :ada:`Tasking_Error`

-----------------
Queuing Policy
-----------------

* Queuing policy can be set using

.. code:: Ada

   pragma Queuing_Policy (<policy_identifier>);

* The following are available for *policy_identifier*

   - FIFO_Queuing (default)
   - Priority_Queuing

* FIFO_Queuing

   - First-in First-out, classical queue

* Priority_Queuing

   - Takes into account priority
   - Priority of the calling task **at time of call**

-----------------------
Setting Task Priority
-----------------------

* GNAT available priorities are :ada:`0 .. 30`, see :file:`gnat/system.ads`
* Tasks with the highest priority are prioritized more
* Priority can be set **statically**

.. code:: Ada

  task type T
     with Priority => <priority_level>
     is ...

* Priority can be set **dynamically**

.. code:: Ada

   with Ada.Dynamic_Priorities;

   task body T is
   begin
      Ada.Dynamic_Priorities.Set_Priority (10);
   end T;

--------------------------
:ada:`requeue` Instruction
--------------------------

* :ada:`requeue` can be called in any :ada:`entry` (task or protected)
* Puts the requesting task back into the queue

   - May be handled by another :ada:`entry`
   - Or the same one...

* Reschedule the processing for later

   .. code:: Ada

      entry Extract (Qty : Integer) when True is
      begin
         if not Try_Extract (Qty) then
            requeue Extract;
         end if;
      end Extract;

* Same parameter values will be used on the queue

-----------------------
:ada:`requeue` Tricks
-----------------------

* Only an accepted call can be requeued
* Accepted entries are waiting for :ada:`end`

    - Not in a :ada:`select ... or delay ... else` anymore

* So the following means the client blocks for **2 seconds**

.. include:: ../examples/select_requeue_issue/extracts/tasks.bodies.adb
   :code: Ada

------------------
Abort Statements
------------------

* :ada:`abort` stops the tasks **immediately**

    - From an external caller
    - No cleanup possible
    - Highly unsafe - should be used only as **last resort**

   .. code:: Ada

      procedure Main is
         task type T;

         task body T is
         begin
            loop
               delay 1.0;
               Put_Line ("A");
            end loop;
         end T;

         Task_Instance : T;
      begin
         delay 10.0;
         abort Task_Instance;
      end;

-----------------------------------
:ada:`select` ... :ada:`then abort`
-----------------------------------

* :ada:`select` can call :ada:`abort`
* Can abort anywhere in the processing
* **Highly** unsafe

-------------------------
Multiple Select Example
-------------------------

.. include:: ../examples/task_select_multiple_or/extracts/task_select.select_or.adb
    :code: Ada

------------------
Example: Main
------------------

.. include:: ../examples/task_select_multiple_or/src/main.adb
    :code: Ada

------
Quiz
------

.. code:: Ada

    task T is
       entry E1;
       entry E2;
    end T;
    ...
    task body Other_Task is
    begin
       select
          T.E1;
       or
          T.E2;
       end select;
    end Other_Task;

What is the result of compiling and running this code?

A. :ada:`T.E1` is called
B. Nothing
C. :answer:`Compilation error`
D. Run-time error

.. container:: animate

    A :ada:`select` entry call can only call one :ada:`entry` at a time.

------
Quiz
------

.. container:: columns

 .. container:: column

  .. code:: Ada

      procedure Main is
         task T is
            entry A;
         end T;

         task body T is
         begin
            select
               accept A;
               Put ("A");
            else
               delay 1.0;
            end select;
         end T;
      begin
         select
            T.A;
         else
            delay 1.0;
         end select;
      end Main;

 .. container:: column

  What is the output of this code?

  A. "AAAAA..."
  B. :answer:`Nothing`
  C. Compilation error
  D. Run-time error

  .. container:: animate

      Common mistake: :ada:`Main` and :ada:`T` won't wait on each other and
      will both execute their :ada:`delay` statement only.

.

------
Quiz
------

.. code:: Ada

    procedure Main is
       task type T is
          entry A;
       end T;

       task body T is
       begin
          select
             accept A;
          or
             terminate;
          end select;

          Put_Line ("Terminated");
       end T;

       My_Task : T;
    begin
       null;
    end Main;

What is the output of this code?

A. "Terminated"
B. :answer:`Nothing`
C. Compilation error
D. Run-time error

.. container:: animate

    :ada:`T` is terminated at the end of :ada:`Main`

------
Quiz
------

.. code:: Ada

    procedure Main is
    begin
       select
          delay 2.0;
       then abort
          loop
             delay 1.5;
             Put ("A");
          end loop;
       end select;

       Put ("B");
    end Main;

What is the output of this code?

A. "A"
B. "AAAA..."
C. :answer:`"AB"`
D. Compilation error
E. Run-time error

.. container:: animate

    :ada:`then abort` aborts the select only, not :ada:`Main`.

------
Quiz
------

.. code:: Ada

    procedure Main is
        Ok : Boolean := False

        protected type O is
           entry P;
        end O;

        protected body O is
        begin
           entry P when Ok is
              Put_Line ("OK");
           end P;
        end O;

        Protected_Instance : O;
         
    begin
        Protected_Instance.P;
    end Main;

What is the result of compiling and running this code?

A. :ada:`OK = True`
B. Nothing
C. Compilation error
D. Run-time error

.. container:: animate

    Stuck on waiting for :ada:`Ok` to be set, :ada:`Main` will never terminate.

--------------------------------------
Standard "Embedded" Tasking Profiles
--------------------------------------

* Better performances but more constrained
* Ravenscar profile

  - Ada 2005
  - No :ada:`select`
  - No :ada:`entry` for tasks
  - Single :ada:`entry` for :ada:`protected` types
  - No entry queues

* Jorvik profile

  - Ada 2022
  - Less constrained, still performant
  - Any number of :ada:`entry` for :ada:`protected` types
  - Entry queues

* See RM D.13
