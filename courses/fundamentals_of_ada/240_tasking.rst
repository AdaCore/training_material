
*********
Tasking
*********

.. role:: ada(code)
   :language: ada

.. role:: C(code)
   :language: C

.. role:: cpp(code)
   :language: C++

================
Introduction
================

---------------
A Simple Task
---------------

* Parallel code execution via **task**
* Tasks are :ada:`limited` types (No copies allowed)

   .. code:: Ada

      procedure Main is
         task type Put_T;
         task body Put_T is
         begin
            loop
               delay 1.0;
               Put_Line ("T");
            end loop;
         end Put_T;

         T : Put_T;
      begin -- Main task body
         loop
            delay 1.0;
            Put_Line ("Main");
         end loop;
      end;

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
* Client calls entries
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

===================
Protected Objects
===================

-------------------
Protected Objects
-------------------

* **Passive** objects state

   - **Multitask-safe** accessors to get and set state
   - **No** direct state manipulation
   - **No** concurrent modifications

* Protected objects are :ada:`limited` types

.. code:: Ada

   protected type Protected_Value is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Value : Integer;
   end Protected_Value;

   protected body Protected_Value is
      procedure Set (V : Integer) is
      begin
         Value := V;
      end Set;

      function Get return Integer is
      begin
         return Value;
      end Get;
   end Protected_Value;

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **No** concurrent call to either :ada:`procedure` or :ada:`function`

* In case of concurrency, other callers get **blocked**

    - Until call finishes

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

.. code:: Ada

    Relative : Duration := Seconds(5.0);
    delay Relative;

    Absolute : Time := Time_Of (2030, 10, 30);
    delay until Absolute;

==========================
Task and Protected Types
==========================

---------------
Task Activation
---------------

* An instantiated task starts running when **activated**
* On the stack

    - Activated when **enclosing** declarative part finishes its **elaboration**

* On the heap

    - Activated **immediately** at instanciation

.. code:: Ada

   task type First_T is [...]

   type First_T_A is access all First_T;

   task body First_T is
   begin
      accept First;
   end First_T;

   [...]

      V1 : First_T;
      V2 : First_T_A;
   begin -- Task V1 is activated
      V2 := new First_T; -- Task V2 is activated

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

---------------
Scope Of a Task
---------------

* Tasks can be nested in **any** declarative block
* A **subprogram** finishes **only** when all its **nested task** bodies are over
* The **program** terminates when all **library-level tasks** finish

.. code:: Ada

   package P is
      task type Tick_T;
   end P;

   -- Programs using the package may never terminate
   package body P is
      task body Tick_T is
         loop
            delay 1.0;
            Put_Line ("tick");
         end loop;
      end Tick_T;
   end P;

========================
Some Advanced Concepts
========================

---------------------------
Waiting On Multiple Entries
---------------------------

* :ada:`select` can wait on multiple entries

    - With **equal** priority, regardless of declaration order

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

  [...]

  T.Receive_Message ("A");
  T.Receive_Message ("B");
  T.Stop;

----------------------
Waiting With a Delay
----------------------

* A :ada:`select` statement may **time-out** using :ada:`delay` or :ada:`delay until`

    - Resume execution at next statement

* Multiple :ada:`delay` allowed

   - Useful when the value is not hard-coded

.. code:: Ada

   task body Msg_Box_T is
   begin
     loop
       select
         accept Receive_Message (V : String) do
           Put_Line ("Message : " & String);
         end Receive_Message;
       or
         delay 50.0;
         Put_Line ("Don't wait any longer");
         exit;
       end select;
     end loop;
   end Msg_Box_T;

.. container:: speakernote

   Task will wait up to 50 seconds for "Receive_Message", print a message, and then enter the loop
   Without the "exit" it will print the message and wait another 50 seconds, and so on

------------------------------------------
Calling an Entry With a Delay Protection
------------------------------------------

* A call to :ada:`entry` **blocks** the task until the entry is :ada:`accept` 'ed
* Wait for a **given amount of time** with :ada:`select ... delay`
* Only **one** entry call is allowed
* No :ada:`accept` statement is allowed

.. code:: Ada

   task type Msg_Box_T is
      entry Receive_Message (V : String);
   end Msg_Box_T;

   procedure Main is
   begin
      select
         T.Receive_Message ("A");
      or
         delay 50.0;
      end select;
   end Main;

.. container:: speakernote

   Procedure will wait up to 50 seconds for "Receive_Message" to be accepted before it gives up

----------------------------
Non-blocking Accept or Entry
----------------------------

* Using :ada:`else`

    - Task **skips** the :ada:`accept` or :ada:`entry` call if they are **not ready** to be entered

* :ada:`delay` is **not** allowed in this case

.. code:: Ada

   select
      accept Receive_Message (V : String) do
         Put_Line ("Received : " & V);
      end Receive_Message;
   else
      Put_Line ("Nothing to receive");
   end select;

   [...]

   select
      T.Receive_Message ("A");
   else
      Put_Line ("Receive message not called");
   end select;

------------------------
Protected Object Entries
------------------------

* **Special** kind of protected :ada:`procedure`
* Several tasks can be waiting on the same :ada:`entry`
* Only **one** will be re-activated when the barrier is **relieved**
* May use a **barrier**, that is **evaluated** when

   - A task calls :ada:`entry`
   - A protected :ada:`entry` or :ada:`procedure` is **exited**

* Barriers **only** allow call on a boolean condition
* When condition is fulfilled, barrier is **relieved**

.. code:: Ada

   protected body Stack is
      entry Push (V : Integer) when Size < Buffer'Length is
      [...]

      entry Pop  (V : out Integer) when Size > 0 is
      [...]
   end Object;

-------------------------------------
Select On Protected Objects Entries
-------------------------------------

* Works the same way as :ada:`select` on task entries

   - With a :ada:`delay` part

   .. code:: Ada

      select
         O.Push (5);
      or
         delay 10.0;
         Put_Line ("Delayed overflow");
      end select;

   - With an :ada:`else` part

   .. code:: Ada

      select
         O.Push (5);
      else
         Put_Line ("Overflow");
      end select;

------
Queue
------

* Protected :ada:`entry` or :ada:`procedure` and tasks :ada:`entry` are activated by **one** task at a time
* **Mutual exclusion** section
* Other tasks trying to enter are **queued**

    - In **First-In First-Out** (FIFO) by default

* When the server task **terminates**, tasks still queued receive :ada:`Tasking_Error`

----------------
Advanced Tasking
----------------

Other constructions are available

* **Guard condition** on :ada:`accept`
* :ada:`requeue` to **defer** handling of an :ada:`entry` call
* :ada:`terminate` the task when no :ada:`entry` call can happen anymore
* :ada:`abort` to stop a task immediately
* :ada:`select ... then abort` some other task

=========
Summary
=========

---------
Summary
---------

* Tasks are **language-based** multiprocessing mechanisms

   - Not necessarily for **truly** parallel operations
   - Originally for task-switching / time-slicing

* Multiple mechanisms to **synchronize** tasks

   - Delay
   - Rendezvous
   - Queues
   - Protected Objects
