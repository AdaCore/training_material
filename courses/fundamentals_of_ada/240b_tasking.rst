
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
         task T;
         task body T is
         begin
            loop
               delay 1.0;
               Put_Line ("T");
            end loop;
         end T;
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

------------------------
Rendezvous Definitions
------------------------

* **Server** declares entries
* Client makes :ada:`entry` call
* Server :ada:`accept` the client calls
* At each :ada:`accept`, server task **blocks**

    - Until a client calls :ada:`entry`

   .. code:: Ada
    
      task T is
         entry Start;
         entry Receive_Message (S : String);
      end T;
          
      task body T is
      begin
         loop
            accept Start;
            Put_Line ("start");

            accept Receive_Message (S : String) do
               Put_Line (S);
            end Start;
         end loop;
      end T;

* Execution order of this example

    - :ada:`Start`
    - :ada:`Receive_Message`
    - :ada:`Start`
    - etc.

------------------------
Rendezvous Calls
------------------------

* Upon calling :ada:`entry`, client **blocks**

     - Until server reaches :ada:`end` of its :ada:`accept` block
    
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
      start             -- May switch place with line vvv below vvv
      calling receive 1 -- May switch place with line ^^^ above ^^^
      Receive 1
      calling receive 2
      -- Blocked until another task calls Start
     
------------------------
Accepting a Rendezvous
------------------------

* **Simple** :ada:`accept` statement

   - Straightforward
   - Entry call waiting: Server handles it
   - **No** entry call waiting: Server **waits** for one

* **Selective** :ada:`accept` statement: Richer

   - **Several** entries accepted at the **same time**
   - **Time-out** on the wait
   - **Not blocking** if no entry call waiting
   - **Terminate** if no clients can **possibly** make entry call
   - **Conditionally** accept a rendezvous based on a **guard expression**

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
    
   protected Object is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Value : Integer;
   end Object;
       
   protected body Object is
      procedure Set (V : Integer) is
      begin
         Value := V;
      end Set;
       
      function Get return Integer is
      begin
         return Value;
      end Get;
   end Object;
     
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
- Relative: Blocks for a :ada:`Duration`
- Absolute: Blocks until a given :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. code:: Ada

    Relative : Duration := Seconds(5.0);
    delay Relative;

    Absolute : Time := Time_Of (2030, 10, 30);
    delay until Absolute;


==========================
Task and Protected Types
==========================

------------
Task Types
------------

* Instanciating task types
* On the stack

    - Starts when **enclosing** declarative part finishes its **elaboration**

* On the heap

    - Activated **immediately**

* *NB:* Tasks are :ada:`limited` types
    
    - No copies allowed

.. code:: Ada
    
   task type T is
      entry First;
   end T;
       
   type T_A is access all T;
       
   task body T is
   begin
      accept First;
   end T;
   ...
      V1 : T;
      V2 : A_T;
   begin -- V1 task is started
      V1.First;
      V2 := new T; -- V2 task is started
      V2.all.First;
     
------------------------
Protected Object Types
------------------------

* Protected objects are :ada:`limited` types

.. code:: Ada
    
   protected type Object is
      entry Push (V : Integer);
      entry Pop  (V : out Integer);
   private
      Buffer : Integer_Array (1 .. 10);
      Size : Integer := 0;
   end Object;
       
   protected body Object is
      entry Push (V : Integer) when Size < Buffer'Length is
      begin
         Buffer (Size + 1) := V;
         Size := Size + 1;
      end Push;
          
      entry Pop  (V : out Integer) when Size > 0 is
      begin
         V := Buffer (Size);
         Size := Size - 1;
      end Pop;
   end Object;
     
-----------------
Scope Of a Task
-----------------

* Tasks can be nested in **any** declarative block
* When **nested** in a **subprogram**, the subprograms ends when **both** the task and the subprogram body are over
* Tasks declared at **library** level **all** have to finish before the **program** terminates


.. code:: Ada

   package P is
      task T; 
   end P;
   
   -- Programs using the package *won't* terminate
   package body P is
      task body T is
         loop
            delay 1.0;
            Put_Line ("tick");
         end loop;
      end T;
   end P;
     
========================
Some Advanced Concepts
========================

------------------------------
Waiting On Different Entries
------------------------------

* :ada:`select` can wait on multiple entries

    - All are **equal**, regardless of order
    - **FIFO** entry call priority

* Convenient for eg. message queues

.. code:: Ada
    
   task T is
     entry Start;
     entry Receive_Message (V : String);
     entry Stop;
   end T;
       
   task body T is
   begin
     accept Start;
     loop
       select
         accept Receive_Message (V : String) 
         do
           Put_Line ("Message : " & String);
         end Receive_Message;
       or
         accept Stop;
           exit;
         end select;
     end loop;
   end T;
     
----------------------
Waiting With a Delay
----------------------

* A :ada:`select` statement may **time-out** using :ada:`delay` or :ada:`delay until`

    - Resume execution at next statement

* Multiple :ada:`delay` allowed

   - Useful when the value is not hard-coded

.. code:: Ada

   task body T is
   begin
     loop
       select
         accept Receive_Message (V:String) do
           Put_Line ("Message : " & String);
         end Receive_Message;
       or
         delay 50.0;
         Put_Line ("Don't wait any longer");
         exit;
       end select;
     end loop;
   end T;
     
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
    
   task T is
      entry Receive_Message (V:String);
   end T;
       
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

-------------------------------------------
Avoid Waiting If No Entry Or Accept Ready
-------------------------------------------

* Using :ada:`else`

    - Task **skips** the :ada:`accept` or :ada:`entry` if they are **not ready** to be entered

* **No** :ada:`delay` is allowed in this case

.. code:: Ada
    
   task body T is
   begin
      select
         accept Receive_Message (V : String) do
            Put_Line ("Received : " & V);
         end Receive_Message;
      else
         Put_Line ("Nothing to receive");
      end select;
   end T;
       
   procedure Main is
   begin
      select
         T.Receive_Message ("A");
      else
         Put_Line ("Receive message not called");
      end select;
   end Main;

--------------------------------
Protected Object Entries (1/2)
--------------------------------

* Protected :ada:`entry` are a **special** kind of protected :ada:`procedure`
* Can be defined using a barrier, allowing the entry to be called on a boolean condition
* Barriers are evaluated when

   - A task calls :ada:`entry`
   - A protected :ada:`entry` or :ada:`procedure` is **exited**

.. code:: Ada
    
   protected Object is
      entry Push (V : Integer);
      entry Pop  (V : out Integer);
   private
      Buffer : Integer_Array (1 .. 10);
      Size : Integer := 0;
   end Object;
       
   protected body Object is
      entry Push (V : Integer) when Size < Buffer'Length is
      begin
         Buffer (Size + 1) := V;
         Size := Size + 1;
      end Push;
          
      entry Pop  (V : out Integer) when Size > 0 is
      begin
         V := Buffer (Size);
         Size := Size - 1;
      end Pop;
   end Object;
     
--------------------------------
Protected Object Entries (2/2)
--------------------------------

* Several tasks can be waiting on the same :ada:`entry`
* Only one will be reactivated when the barrier is **relieved**

    - Depends on the **activation policy**

.. code:: Ada
    
   task body T1 is
      V : Integer;
   begin
      Object.Pop (V);
   end T1;
       
   task body T2 is
      V : Integer;
   begin
      Object.Pop (V);
   end T2;
       
   task body T3 is
   begin
      delay 1.0;
      Object.Push (42);
   end T3;
     
-------------------------------------
Select On Protected Objects Entries
-------------------------------------

* Works the same way as select on task entries

   - With a `delay` part

   .. code:: Ada

      select
         O.Push (5);
      or
         delay 10.0;
         Put_Line ("Delayed overflow");
      end select;
      
   - With an `else` part

   .. code:: Ada

      select
         O.Push (5);
      else
         Put_Line ("Overflow");
      end select;
 
------
Queue
------

* Protected :ada:`entry`, :ada:`procedure` and tasks :ada:`entry` are activated by **one** task at a time
* Other tasks trying to enter the **mutual exclusion** section are **queued**
* **By default**, tasks are entering the queue in **FIFO**
* When the server task **terminates**, tasks still queued receive :ada:`Tasking_Error`

----------------
Advanced Tasking
----------------

* Other constructions are available
* A task may :ada:`terminate` if no :ada:`entry` call can happen anymore
* The :ada:`accept` statement may **depend** on a guard condition to be True
* :ada:`requeue` to put back an :ada:`entry` call
* :ada:`abort` to stop a task immediately
* :ada:`select ... then abort` some other task
 
========
Lab
========

.. include:: labs/240_tasking.lab.rst

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
