
*********
Tasking
*********

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

---------------
A Simple Task
---------------

* Parallel code execution via **task**
* :ada:`limited` types (No copies allowed)

   .. code:: Ada

      procedure Main is
         task T;
         task body T is
         begin
            loop
               delay 1.0;
               Put_Line ("T");
            end loop;
         end T;
      begin
         loop
            delay 1.0;
            Put_Line ("Main");
         end loop;
      end;

* A task is started when its declaration scope is **elaborated**
* Its enclosing scope exits when **all tasks** have finished

--------------------------
Two Synchronization Models
--------------------------

* Active

   - Rendezvous
   - **Client / Server** model
   - Server **entries**
   - Client **entry calls**

* Passive

   - **Protected objects** model
   - Concurrency-safe **semantics**

=======
Tasks
=======

------------------------
Rendezvous Definitions
------------------------

* **Server** declares several :ada:`entry`
* Client calls entries like subprograms
* Server :ada:`accept` the client calls
* At each standalone :ada:`accept`, server task **blocks**

    - **Until** a client calls the related :ada:`entry`

   .. code:: Ada

      task type Msg_Box_T is
         entry Start;
         entry Receive_Message (S : String);
      end Msg_Box_T;

      task body Msg_Box_T is
      begin
         loop
            accept Start;
            Put_Line ("start");

            accept Receive_Message (S : String) do
               Put_Line (S);
            end Receive_Message;
         end loop;
      end Msg_Box_T;

------------------------
Rendezvous Entry Calls
------------------------

* Upon calling an :ada:`entry`, client **blocks**

     - **Until** server reaches :ada:`end` of its :ada:`accept` block

   .. code:: Ada

      Put_Line ("calling start");
      T.Start;
      Put_Line ("calling receive 1");
      T.Receive_Message ("1");
      Put_Line ("calling receive 2");
      T.Receive_Message ("2");

* May be executed as follows:

   .. code:: Ada

      calling start
      start             -- May switch place with line below
      calling receive 1 -- May switch place with line above
      Receive 1
      calling receive 2
      -- Blocked until another task calls Start

------------------------
Accepting a Rendezvous
------------------------

* :ada:`accept` statement

   - Wait on single entry
   - If entry call waiting: Server handles it
   - Else: Server **waits** for an entry call

* :ada:`select` statement

   - **Several** entries accepted at the **same time**
   - Can **time-out** on the wait
   - Can be **not blocking** if no entry call waiting
   - Can **terminate** if no clients can **possibly** make entry call
   - Can **conditionally** accept a rendezvous based on a **guard expression**

------------------------
Rendezvous Calls
------------------------

* When calling an **entry**, the caller waits until the task is ready to be called

   .. code:: Ada

      Put_Line ("calling start");
      T.Start;
      Put_Line ("calling receive 1");
      T.Receive_Message ("1");
      Put_Line ("calling receive 2");
      --  Locks until somebody calls Start
      T.Receive_Message ("2");

* Results in an output like:

   .. code:: Ada

      calling start
      start
      calling receive 1
      Receive 1
      calling receive 2

.. container:: speakernote

   The loop requires Accept and ReceiveMessage to be called one followed by other

------------------------
Accepting a Rendezvous
------------------------

* Simple :ada:`accept` statement

   - Used by a server task to indicate a willingness to provide the service at a given point

* Selective :ada:`accept` statement (later in these slides)

   - Wait for more than one rendezvous at any time
   - Time-out if no rendezvous within a period of time
   - Withdraw its offer if no rendezvous is immediately available
   - Terminate if no clients can possibly call its entries
   - Conditionally accept a rendezvous based on a guard expression

---------------------------
Example: Task - Declaration
---------------------------

.. include:: examples/task_very_simple/src/tasks.ads
    :code: Ada

--------------------
Example: Task - Body
--------------------

.. include:: examples/task_very_simple/src/tasks.adb
    :code: Ada

---------------
Example: Main
---------------

.. include:: examples/task_very_simple/src/main.adb
    :code: Ada

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      with Ada.Text_IO; use Ada.Text_IO;
      procedure Main is
         task T is
            entry Hello;
            entry Goodbye;
         end T;
         task body T is
         begin
            loop
               accept Hello do
                  Put_Line ("Hello");
               end Hello;
               accept Goodbye do
                  Put_Line ("Goodbye");
               end Goodbye;
            end loop;
            Put_Line ("Finished");
         end T;
      begin
         T.Hello;
         T.Goodbye;
         Put_Line ("Done");
      end Main;

 .. container:: column

   What is the output of this program?

      A. Hello, Goodbye, Finished, Done
      B. Hello, Goodbye, Finished
      C. :answer:`Hello, Goodbye, Done`
      D. Hello, Goodbye

   .. container:: animate

      |

      - Entries :ada:`Hello` and :ada:`Goodbye` are reached (so "Hello" and
      "Goodbye" are printed).

      - After :ada:`Goodbye`, task returns to :ada:`Main`
      (so "Done" is printed) but the loop in the task never finishes (so
      "Finished" is never printed).

===================
Protected Objects
===================

-------------------
Protected Objects
-------------------

* **Multitask-safe** accessors to get and set state
* **No** direct state manipulation
* **No** concurrent modifications
* :ada:`limited` types (No copies allowed)

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - **Multiple-Readers**
   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **Single-Writer**
   - **No** concurrent call to either :ada:`procedure` or :ada:`function`

* In case of concurrency, other callers get **blocked**

    - Until call finishes

* Support for read-only locks **depends on OS**

    - Windows has **no** support for those
    - In that case, :ada:`function` are **blocking** as well

------------------------------------------
Example: Protected Objects - Declaration
------------------------------------------

.. include:: examples/protected_objects/src/protected_objects.ads
    :code: Ada

-----------------------------------
Example: Protected Objects - Body
-----------------------------------

.. include:: examples/protected_objects/src/protected_objects.adb
    :code: Ada

------
Quiz
------

.. container:: latex_environment footnotesize

 .. code:: Ada

   protected P is
      procedure Initialize (V : Integer);
      procedure Increment;
      function Decrement return Integer;
      function Query return Integer;
   private
      Object : Integer := 0;
   end P;

What of the following completions for :ada:`P`'s members is illegal?

 .. container:: latex_environment footnotesize

   A. |  ``procedure Initialize (V : Integer) is``
      |  ``begin``
      |     ``Object := V;``
      |  ``end Initialize;``
   B. |  ``procedure Increment is``
      |  ``begin``
      |     ``Object := Object + 1;``
      |  ``end Increment;``
   C. |  :answermono:`function Decrement return Integer is`
      |  :answermono:`begin`
      |     :answermono:`Object := Object - 1;`
      |     :answermono:`return Object;`
      |  :answermono:`end Decrement;`
   D. |  ``function Query return Integer is begin``
      |     ``return Object;``
      |  ``end Query;``

.. container:: animate

   A. Legal
   B. Legal - subprograms do not need parameters
   C. Functions in a protected object cannot modify global objects
   D. Legal

======
Delays
======

-------------
Delay keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time
- Relative: Blocks for at least :ada:`Duration`
- Absolute: Blocks until a given :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. include:: examples/delays/src/main.adb
   :code: Ada

==========================
Task and Protected Types
==========================

---------------
Task Activation
---------------

* Instantiated tasks start running when **activated**
* On the **stack**

   - When **enclosing** declarative part finishes **elaborating**

* On the **heap**

   - **Immediately** at instantiation

.. code:: Ada

   task type First_T is ...
   type First_T_A is access all First_T;

   task body First_T is ...
   ...
   declare
      V1 : First_T;
      V2 : First_T_A;
   begin  -- V1 is activated
      V2 := new First_T;  -- V2 is activated immediately

--------------------
Single Declaration
--------------------

 * Instanciate an **anonymous** task (or protected) type
 * Declares an object of that type

    - Body declaration is then using the **object** name

 .. code:: Ada

   task Msg_Box is
       -- Msg_Box task is declared *and* instanciated
      entry Receive_Message (S : String);
   end Msg_Box_T;

   task body Msg_Box is
   begin
      loop
         accept Receive_Message (S : String) do
            Put_Line (S);
         end Receive_Message;
      end loop;
   end Msg_Box;

-----------
Task Scope
-----------

* Nesting is possible in **any** declarative block
* Scope has to **wait** for tasks to finish before ending
* At library level: program ends only when **all tasks** finish

   .. code:: Ada

      package P is
         task T;
      end P;

      package body P is
         task body T is
            loop
               delay 1.0;
               Put_Line ("tick");
            end loop;
         end T;
      end P;

------------------------------------------
Example: Protected Objects - Declaration
------------------------------------------

.. include:: examples/protected_objects_2/src/protected_objects.ads
    :code: Ada

-----------------------------------
Example: Protected Objects - Body
-----------------------------------

.. include:: examples/protected_objects_2/src/protected_objects.adb
    :code: Ada

------------------------------
Example: Tasks - Declaration
------------------------------

.. include:: examples/protected_objects_2/src/tasks.ads
    :code: Ada

-----------------------
Example: Tasks - Body
-----------------------

.. include:: examples/protected_objects_2/extracts/tasks.body.adb
    :code: Ada

------------------
Example: Main
------------------

.. include:: examples/protected_objects_2/src/test_protected_objects.adb
    :code: Ada

========================
Some Advanced Concepts
========================

------------------------------
Waiting On Different Entries
------------------------------

* It is convenient to be able to accept several entries
* The :ada:`select` statements can wait simultaneously on a list of entries

    - It accepts the **first** one that is requested

.. code:: Ada

   select
     accept Receive_Message (V : String)
     do
       Put_Line ("Message : " & String);
     end Receive_Message;
   or
     accept Stop;
       exit;
     end select;

----------------------
Waiting With a Delay
----------------------

* A :ada:`select` statement can wait with a :ada:`delay`

    - If that delay is exceeded with no entry call, block is executed

* The :ada:`delay until` statement can be used as well
* There can be multiple :ada:`delay` statements

   - (useful when the value is not hard-coded)

.. code:: Ada

   select
     accept Receive_Message (V:String) do
       Put_Line ("Message : " & String);
     end Receive_Message;
   or
     delay 50.0;
     Put_Line ("Don't wait any longer");
     exit;
   end select;

----------------------------
The Delay Is Not A Timeout
----------------------------

* The :ada:`or delay` is not *really* a timeout
* The server time to process messages is **not included**
* In the following, the total delay can be up to **110 seconds**

.. code:: Ada

   procedure Update_Thingy is
   begin
      if not Thingy_Ready then
         delay 100.0; --  Wait for the thingy
      end if;
   ...
   select
     accept ....
   or
     delay 10.0;
     Update_Thingy;
   end select;

------------------------------------------
Calling an Entry With a Delay Protection
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

----------------------------
The Delay Is Not A Timeout
----------------------------

* Similar to :ada:`select ... or delay`
* The time spent by the client is actually **not bounded**

    - Delay stops on :ada:`accept`
    - **But** the call blocks until :ada:`end` of server-side statements

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

.. include:: examples/select_non_blocking_entry_and_call/extracts/tasks.adb
   :code: Ada

* As caller on an :ada:`entry`

.. include:: examples/select_non_blocking_entry_and_call/extracts/main_no_stop.adb
   :code: Ada

* :ada:`delay` is **not** allowed in this case

-----------------------------------
Issues With "Double Non-Blocking"
-----------------------------------

* For :ada:`accept ... else` the server **peeks** into the queue

   - Server **does not** wait

* For :ada:`<entry-call> ... else` the caller look for a **waiting** server
* If both use it, the entry will **never** be called
* Server

.. include:: examples/select_non_blocking_entry_and_call/extracts/tasks.adb
   :code: Ada

* Caller

.. include:: examples/select_non_blocking_entry_and_call/extracts/main_no_call.adb
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

-------------------
Guard Expressions
-------------------

* :ada:`accept` may depend on a **guard condition** with :ada:`when`

    - Evaluated when entering :ada:`select`

.. code:: Ada

   task body T is
      Val : Integer;
      Initialized : Boolean := False;
   begin
      loop
         select
            accept Put (V : Integer) do
               Val := V;
               Initialized := True;
            end Put;
         or
            when Initialized =>
               accept Get (V : out Integer) do
                  V := Val;
               end Get;
         end select;
      end loop;
   end T;

------------------------
Protected Object Entries
------------------------

* **Special** kind of protected :ada:`procedure`
* May use a **barrier**, that **only** allows call on a **boolean** condition
* Barrier is **evaluated** and may be **relieved** when

   - A task calls :ada:`entry`
   - A protected :ada:`entry` or :ada:`procedure` is **exited**

* Several tasks can be waiting on the same :ada:`entry`

    - Only **one** will be re-activated when the barrier is relieved

.. code:: Ada

   protected body Stack is
      entry Push (V : Integer) when Size < Buffer'Length is
      ...
      entry Pop  (V : out Integer) when Size > 0 is
      ...
   end Object;

-------------------------------------
Select On Protected Objects Entries
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

.. include:: examples/select_requeue_issue/extracts/tasks.bodies.adb
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
         task T;

         task T is
         begin
            loop
               delay 1.0;
               Put_Line ("A");
            end loop;
         end T;
      begin
         delay 10.0;
         abort T;
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

.. include:: examples/task_select_multiple_or/extracts/task_select.select_or.adb
    :code: Ada

------------------
Example: Main
------------------

.. include:: examples/task_select_multiple_or/src/main.adb
    :code: Ada

========
Lab
========

.. include:: labs/adv_240_tasking.lab.rst

=========
Summary
=========

---------
Summary
---------

* Tasks are language-based multiprocessing mechanisms

   - Not necessarily designed to be operated in parallel
   - Original design assumed task-switching / time-slicing

* Multiple mechanisms to synchronize tasks

   - Delay
   - Rendezvous
   - Protected Objects
