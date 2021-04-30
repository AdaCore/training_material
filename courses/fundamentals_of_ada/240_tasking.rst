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

   - Wait for a client call on **single** entry

* :ada:`select` statement

   - Wait for a client call on **several** entries at the **same time**
   - Can **time-out** on the wait
   - Can be **not blocking** if no entry call waiting
   - Can **terminate** if no clients can **possibly** make entry call
   - Can **conditionally** accept a rendezvous based on a **guard expression**

* Clients entry calls may be **blocking**

    - Until an :ada:`accept` or :ada:`select` accepts it

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
         end T;
         task body T is
         begin
            loop
               accept Hello do
                  Put_Line ("Hello");
               end Hello;
            end loop;
            Put_Line ("Finished");
         end T;
      begin
         T.Hello;
         Put_Line ("Done");
      end Main;

 .. container:: column

   What is the output of this program?

      A. Hello, Finished, Done
      B. Hello, Finished
      C. :answer:`Hello, Done`
      D. Hello

   .. container:: animate

      Entry :ada:`Hello` is reached, then task
      returns to :ada:`Main` (so "Done" is printed)
      and continues looping (so "Finished" is never
      printed).

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

--------------
Relative Delay 
--------------

- Use :ada:`delay`
- Blocks for a time
- Relative: Blocks for at least :ada:`Duration`

.. code:: Ada

    Relative : Duration := Seconds(5.0);
    delay Relative;

- May be **restricted** on some runtimes or coding standards

    * Real-time constraints

--------------
Absolute Delay 
--------------

- Using :ada:`delay until`
- Blocks until a given :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. code:: Ada

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
   end Msg_Box;
       
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
* Scope has to **wait** for task to finish before ending
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
      accept Entry_Point;
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
* May use a **barrier**, that **only** allows call on a boolean condition
* Barrier is **evaluated** and may be **relieved** when

   - A task calls :ada:`entry`
   - A protected :ada:`entry` or :ada:`procedure` is **exited**

* Several tasks can be waiting on the same :ada:`entry`

    - Only **one** will be re-activated when the barrier is relieved

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
* Can abort anywhere in the processing, highly unsafe

   .. code:: Ada

      select
         delay 10.0;
      then abort
         Some_Long_Processing;
      end select;
      -- statements to execute after select
 
   .. code:: Ada

      select
         T.Wait_For_Interruption;
      then abort
         Some_Long_Processing;
      end select;
      -- statements to execute after select
 
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
